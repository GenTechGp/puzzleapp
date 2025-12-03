#!/usr/bin/env bash
# dump_vep.sh
# Usage: ./dump_vep.sh <ensembl_vep_ref> [output_dir]
#
# Behavior:
#  - Clone ensembl-io, ensembl, ensembl-variation (shallow clones, skip if already present)
#  - Clone ensembl-vep using the exact ref (branch or tag) you provide if possible;
#    if clone by ref fails, fall back to downloading the release tarball for that ref
#  - Build PERL5LIB from the cloned repos' modules/ dirs (and the extracted ensembl-vep)
#  - Run a small Perl extractor that dumps OVERLAP_CONSEQUENCES into:
#       vep_consequences_<sanitized_ref>.tsv
#
# Examples:
#   ./dump_vep.sh release/115.2
#   ./dump_vep.sh release-115 /tmp/vep_115
#
set -euo pipefail

usage() {
  echo "Usage: $0 <ensembl_vep_ref> [output_dir]"
  echo "Example: $0 release/115.2 /tmp/ensembl_vep_115"
  exit 1
}

if [ "$#" -lt 1 ]; then
  usage
fi

VEP_REF="$1"
OUTDIR="${2:-$(pwd)/ensembl_vep_${VEP_REF}}"
mkdir -p "$OUTDIR"
cd "$OUTDIR"

GIT="${GIT:-git}"
CURL="${CURL:-curl}"
TAR="${TAR:-tar}"
PERL="${PERL:-perl}"
REPOS=(ensembl-io ensembl ensembl-variation)

echo "Working in: $OUTDIR"
echo "Requested ensembl-vep ref: $VEP_REF"

# 1) Clone ensembl-io, ensembl, ensembl-variation (shallow)
for repo in "${REPOS[@]}"; do
  if [ -d "${OUTDIR}/${repo}" ]; then
    echo "Directory ${repo} already exists; skipping clone."
  else
    echo "Cloning ${repo} (shallow, depth=1) ..."
    if ! $GIT clone --depth 1 "https://github.com/Ensembl/${repo}.git" "${OUTDIR}/${repo}"; then
      echo "ERROR: git clone failed for ${repo}"
      exit 2
    fi
  fi
done

# 2) Try to clone ensembl-vep using the exact ref supplied (works for branches and tags)
VEP_DIR="${OUTDIR}/ensembl-vep-${VEP_REF//\//_}"   # sanitized directory name (slashes -> underscores)
if [ -d "$VEP_DIR" ]; then
  echo "ensembl-vep directory $VEP_DIR already exists; skipping clone/download."
else
  echo "Attempting git clone of ensembl-vep and check out ref '${VEP_REF}' ..."
  # Try to shallow clone the repository and check out the desired ref
  if $GIT clone --depth 1 --branch "$VEP_REF" "https://github.com/Ensembl/ensembl-vep.git" "$VEP_DIR" 2>/dev/null; then
    echo "Cloned ensembl-vep and checked out '${VEP_REF}' -> $VEP_DIR"
  else
    echo "Git clone by branch/tag '${VEP_REF}' failed. Falling back to downloading archive for the ref."
    # Try downloading archive directly (works for tags like release/115.2 or release-115)
    TARURL="https://github.com/Ensembl/ensembl-vep/archive/refs/tags/${VEP_REF}.tar.gz"
    TARFILE="${OUTDIR}/ensembl-vep-${VEP_REF//\//_}.tar.gz"
    echo "Downloading ${TARURL} ..."
    if $CURL -fSL -o "$TARFILE" "$TARURL"; then
      echo "Extracting ${TARFILE} ..."
      $TAR -xzf "$TARFILE"
      rm -f "$TARFILE"
      # find extracted dir (common pattern: ensembl-vep-<ref-with-slashes-replaced>)
      # we will pick the single new directory whose name starts with ensembl-vep-
      extracted="$(ls -1d ensembl-vep-* 2>/dev/null | head -n1 || true)"
      if [ -z "$extracted" ]; then
        echo "ERROR: could not find extracted ensembl-vep directory after extracting $TARFILE"
        exit 4
      fi
      # rename to sanitized target dir
      mv "$extracted" "$VEP_DIR"
      echo "Extracted ensembl-vep to $VEP_DIR"
    else
      echo "ERROR: failed to download ensembl-vep archive for ref '${VEP_REF}'."
      echo "Please verify the ref exists on GitHub (tags and branch names are case-sensitive)."
      exit 5
    fi
  fi
fi

# 3) Collect module directories for PERL5LIB
MODULE_DIRS=()
for d in "${OUTDIR}/ensembl/modules" "${OUTDIR}/ensembl-io/modules" "${OUTDIR}/ensembl-variation/modules" "${VEP_DIR}/modules"; do
  if [ -d "$d" ]; then
    MODULE_DIRS+=("$d")
  else
    echo "WARNING: modules/ not found at $d"
  fi
done

if [ "${#MODULE_DIRS[@]}" -eq 0 ]; then
  echo "ERROR: no modules/ directories found in any repo. Cannot set PERL5LIB."
  exit 6
fi

# Build PERL5LIB preserving any existing PERL5LIB
PERL5LIB_VAL=""
for p in "${MODULE_DIRS[@]}"; do
  if [ -z "$PERL5LIB_VAL" ]; then
    PERL5LIB_VAL="${p}"
  else
    PERL5LIB_VAL="${PERL5LIB_VAL}:${p}"
  fi
done
PERL5LIB_VAL="${PERL5LIB_VAL}:${PERL5LIB:-}"
export PERL5LIB="${PERL5LIB_VAL}"
echo "PERL5LIB set to: ${PERL5LIB}"

# 4) Quick sanity check that the constants module can be loaded
echo "Checking that Bio::EnsEMBL::Variation::Utils::Constants can be loaded..."
if ! $PERL -MBio::EnsEMBL::Variation::Utils::Constants -e 'print "OK\n"' >/dev/null 2>&1; then
  echo "ERROR: Perl could not load Bio::EnsEMBL::Variation::Utils::Constants with PERL5LIB set."
  echo "Inspect the earlier WARNING lines to see which modules/ dirs were missing."
  exit 7
fi

# 5) Write and run the Perl extractor
PERL_SCRIPT="${OUTDIR}/dump_vep_consequences.pl"
cat > "$PERL_SCRIPT" <<'PERL_END'
#!/usr/bin/env perl
use strict;
use warnings;
use Bio::EnsEMBL::Variation::Utils::Constants;

my %cons = %Bio::EnsEMBL::Variation::Utils::Constants::OVERLAP_CONSEQUENCES;

print join("\t", qw(term accession display_term impact rank is_coding category description)), "\n";

my @terms = sort {
  my $ra = (defined $cons{$a}->rank) ? $cons{$a}->rank : 99999;
  my $rb = (defined $cons{$b}->rank) ? $cons{$b}->rank : 99999;
  $ra <=> $rb || $a cmp $b;
} keys %cons;

for my $term (@terms) {
  my $c = $cons{$term} or next;
  my $so_term     = $c->SO_term // $term;
  my $so_acc      = $c->SO_accession // '';
  my $label       = $c->label // $so_term;
  my $impact      = $c->impact // '';
  my $rank        = defined $c->rank ? $c->rank : '';
  my $is_coding   = ($c->{include} && $c->{include}->{coding}) ? 1 : 0;
  my $category    = $c->{category} // '';
  my $description = $c->description // '';
  $description =~ s/[\r\n]+/ /g;
  $description =~ s/\t/ /g;
  print join("\t", $so_term, $so_acc, $label, $impact, $rank, $is_coding, $category, $description), "\n";
}
PERL_END

chmod +x "$PERL_SCRIPT"

SANITIZED_REF="${VEP_REF//[^A-Za-z0-9_.-]/_}"
OUTFILE="${OUTDIR}/vep_consequences_${SANITIZED_REF}.tsv"

echo "Running extractor and writing ${OUTFILE} ..."
$PERL "$PERL_SCRIPT" > "$OUTFILE"

echo "Done. Output: ${OUTFILE}"
