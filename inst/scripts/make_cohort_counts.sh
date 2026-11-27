#!/usr/bin/env bash
# Build a cohort carrier-count table for puzzleapp's paths$snvs_cohort_counts.
#
# For every allele in the cohort VCF, counts the samples carrying it: at least
# one non-reference allele in GT. Het, hom-alt, haploid and half-calls (./1)
# each count as one carrier, the same definition puzzleapp has always used for
# N_Cohort.
#
# Multi-allelic records are split first (bcftools norm -m -any), so the table has
# one row per allele and joins onto puzzleapp's split SNV records. Only GT is
# kept, so this streams in constant memory whatever the cohort size.
#
# The same allele can sit in more than one record (e.g. a merged cohort VCF where
# left-alignment moved one record onto another's allele). Such alleles get a
# single row, counting each sample once across those records. Only the
# duplicated sites are re-read for this (by index when the VCF has one).
#
# Output: tab-separated, bgzip-compressed, with header
#   CHROM  POS  REF  ALT  N_Cohort
# Only alleles with at least one carrier are written.
#
# Usage: make_cohort_counts.sh <cohort.vcf.gz> <out.tsv.gz> [threads]
# Requires bcftools (+fill-tags plugin) and bgzip on PATH.
set -euo pipefail

if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "Usage: $(basename "$0") <cohort.vcf.gz> <out.tsv.gz> [threads]" >&2
  exit 1
fi
in_vcf=$1
out=$2
threads=${3:-4}

for tool in bcftools bgzip; do
  command -v "$tool" >/dev/null 2>&1 || { echo "Error: $tool not found on PATH" >&2; exit 1; }
done
[[ -f "$in_vcf" ]] || { echo "Error: cohort VCF not found: $in_vcf" >&2; exit 1; }

# Intermediate files sit next to the output and are removed on exit; the table
# is renamed into place only at the end, so a failed run leaves no partial table
tmp="$out.tmp.$$"
raw="$tmp.raw.gz"        # per-record counts, may hold duplicated alleles
dups="$tmp.dups"         # duplicated alleles: CHROM POS REF ALT
regions="$tmp.regions"   # their positions: CHROM POS
merged="$tmp.merged"     # duplicated alleles with distinct-carrier counts
trap 'rm -f "$tmp" "$raw" "$dups" "$regions" "$merged"' EXIT

# Split to one allele per record, keeping only GT
split_gt() {
  bcftools annotate --threads "$threads" -x INFO,^FORMAT/GT -Ou "$@" \
    | bcftools norm -m -any -Ou
}

# --- Pass 1: per-record counts ------------------------------------------------
# N_Cohort = AC_Het + AC_Hom/2 + AC_Hemi: a diploid hom-alt sample adds 2 to
# AC_Hom, and fill-tags counts haploid calls and half-calls under AC_Hemi
split_gt "$in_vcf" \
  | bcftools +fill-tags -Ou -- -t AC_Het,AC_Hom,AC_Hemi \
  | bcftools query -i 'AC_Het+AC_Hom+AC_Hemi>0' \
      -f '%CHROM\t%POS\t%REF\t%ALT\t%AC_Het\t%AC_Hom\t%AC_Hemi\n' \
  | awk -F'\t' -v OFS='\t' '{ print $1, $2, $3, $4, $5 + $6 / 2 + $7 }' \
  | bgzip -@ "$threads" > "$raw"

# --- Find alleles that appear in more than one record --------------------------
# Rows come in VCF order, so only keys from nearby positions are kept in memory
# (flushed every 100 kb), whatever the table size
bgzip -dc "$raw" | awk -F'\t' -v OFS='\t' '
  $1 != chrom { delete seen; chrom = $1; flushed = $2 }
  $2 - flushed > 100000 {
    for (k in seen) if (seen[k] < $2 - 100000) delete seen[k]
    flushed = $2
  }
  {
    k = $1 FS $2 FS $3 FS $4
    if ((k in seen) && !(k in dup)) { dup[k] = 1; print k }
    seen[k] = $2
  }' > "$dups"

n_dup=$(wc -l < "$dups")
if (( n_dup == 0 )); then
  { printf 'CHROM\tPOS\tREF\tALT\tN_Cohort\n'; bgzip -dc "$raw"; } | bgzip -@ "$threads" > "$tmp"
else
  # --- Pass 2: count distinct carriers at the duplicated alleles only ----------
  cut -f1,2 "$dups" | sort -u -k1,1 -k2,2n > "$regions"
  # By index when there is one; otherwise stream the VCF, matching records that
  # overlap the positions
  if [[ -f "$in_vcf.tbi" || -f "$in_vcf.csi" ]]; then
    select=(-R "$regions")
  else
    select=(-T "$regions" --targets-overlap 1)
  fi
  bcftools view "${select[@]}" -Ou "$in_vcf" \
    | split_gt - \
    | bcftools query -f '%CHROM\t%POS\t%REF\t%ALT[\t%GT]\n' \
    | awk -F'\t' -v OFS='\t' '
        NR == FNR { want[$0] = 1; next }
        {
          k = $1 FS $2 FS $3 FS $4
          if (!(k in want)) next
          for (i = 5; i <= NF; i++) {
            # carrier: any allele of GT is this ALT (1 once split), so het,
            # hom-alt, haploid and half-calls each count
            n = split($i, a, /[\/|]/)
            for (j = 1; j <= n; j++) if (a[j] == "1") { carrier[k, i] = 1; break }
          }
        }
        END {
          for (k in want) {
            c = 0
            for (ki in carrier) { split(ki, p, SUBSEP); if (p[1] == k) c++ }
            print k, c
          }
        }' "$dups" - > "$merged"

  # --- Write one row per allele, the merged count replacing the per-record ones
  bgzip -dc "$raw" | awk -F'\t' -v OFS='\t' '
      NR == FNR { n[$1 FS $2 FS $3 FS $4] = $5; next }
      FNR == 1 { print "CHROM", "POS", "REF", "ALT", "N_Cohort" }
      {
        k = $1 FS $2 FS $3 FS $4
        if (k in n) { if (k in done) next; done[k] = 1; $5 = n[k] }
        print
      }' "$merged" - \
    | bgzip -@ "$threads" > "$tmp"
fi

mv "$tmp" "$out"
echo "Wrote $out: $(( $(bgzip -dc "$out" | wc -l) - 1 )) alleles with at least one carrier"
if (( n_dup > 0 )); then
  echo "  $n_dup allele(s) were in more than one record; each sample counted once across them"
fi
