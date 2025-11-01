#!/usr/bin/env bash
set -o pipefail

# PanelApp AU end-to-end:
# 1) Create a manifest (panel_id, version, name)
# 2) Download per-panel TSVs for those id+version pairs
# 3) Concatenate into a single TSV with header spaces converted to underscores
#
# Usage:
#   ./panelapp_to_tsv2.sh OUTPUT_DIR
#
# Notes:
# - Requires: curl, jq
# - API base can be overridden with env var PANELAPP_API_BASE
#   e.g., PANELAPP_API_BASE="https://panelapp.agha.umccr.org/api/v1" ./panelapp_to_tsv2.sh outdir

API_BASE_DEFAULT="https://panelapp-aus-staging.org/api/v1"

EXPECTED_HEADER=$'Entity_Name\tEntity_type\tGene_Symbol\tSources(;_separated)\tLevel4\tLevel3\tLevel2\tModel_Of_Inheritance\tPhenotypes\tOmim\tOrphanet\tHPO\tPublications\tDescription\tFlagged\tGEL_Status\tUserRatings_Green_amber_red\tversion\tready\tMode_of_pathogenicity\tEnsemblId(GRch37)\tEnsemblId(GRch38)\tHGNC\tPosition_Chromosome\tPosition_GRCh37_Start\tPosition_GRCh37_End\tPosition_GRCh38_Start\tPosition_GRCh38_End\tSTR_Repeated_Sequence\tSTR_Normal_Repeats\tSTR_Pathogenic_Repeats\tRegion_Haploinsufficiency_Score\tRegion_Triplosensitivity_Score\tRegion_Required_Overlap_Percentage\tRegion_Variant_Type\tRegion_Verbose_Name'

USED_HEADER=$'Entity_Name\tEntity_type\tGene_Symbol\tSources\tLevel4\tLevel3\tLevel2\tModel_Of_Inheritance\tPhenotypes\tOmim\tOrphanet\tHPO\tPublications\tDescription\tFlagged\tGEL_Status\tUserRatings_Green_amber_red\tversion\tready\tMode_of_pathogenicity\tEnsemblId_GRch37\tEnsemblId_GRch38\tHGNC\tPosition_Chromosome\tPosition_GRCh37_Start\tPosition_GRCh37_End\tPosition_GRCh38_Start\tPosition_GRCh38_End\tSTR_Repeated_Sequence\tSTR_Normal_Repeats\tSTR_Pathogenic_Repeats\tRegion_Haploinsufficiency_Score\tRegion_Triplosensitivity_Score\tRegion_Required_Overlap_Percentage\tRegion_Variant_Type\tRegion_Verbose_Name\tPanel_ID\tPanel_Version'

PAGE_SUFFIX="01234"

die() {
  echo "ERROR: $*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

panelapp_panels_to_tsv() {
  # Args:
  #   $1 = out_path (manifest TSV)
  #   $2 = api_base
  local out_path="$1"
  local api_base="$2"

  echo "Writing panel manifest to: $out_path"
  printf 'panel_id\tversion\tname\n' >"$out_path" || die "Cannot write to $out_path"

  local url="${api_base%/}/panels/?page=1&page_size=100"
  while [ -n "$url" ]; do
    echo "Fetching page: $url"
    local json
    if ! json="$(curl -fsSL "$url")"; then
      die "curl failed for $url"
    fi

    # Append rows: id, version, cleaned name
    jq -r '
      .results[]
      | {id: (.id // .panel_id // .pk), version: (.version|tostring), name: (.name|tostring)}
      | select(.id and .version and .name)
      | [(.id|tostring), .version, (.name|gsub("\t";" ")|gsub("\r|\n";" "))]
      | @tsv
    ' <<<"$json" >>"$out_path" || die "jq failed to parse page results"

    url="$(jq -r '.next // ""' <<<"$json")"
  done
}

download_all() {
  # Args:
  #   $1 = out_dir (directory containing manifest and for storing TSVs)
  #   $2 = api_base
  local out_dir="$1"
  local api_base="$2"
  local manifest="$out_dir/panel_manifest.tsv"
  local panels_dir="$out_dir/panels"

  [ -s "$manifest" ] || die "Manifest not found or empty: $manifest"
  mkdir -p "$panels_dir" || die "Failed to create panels dir: $panels_dir"

  echo "Downloading panel TSVs into: $panels_dir"
  local total downloaded=0
  total="$(($(wc -l <"$manifest") - 1))" || total=0

  # Skip header line by starting from line 2
  tail -n +2 "$manifest" | while IFS=$'\t' read -r panel_id version name; do
    [ -n "$panel_id" ] && [ -n "$version" ] || continue
    local url="https://panelapp-aus.org/panels/${panel_id}/download/${PAGE_SUFFIX}/"
    # sanitize name for filename (remove spaces and slashes)
    name="${name// /_}"
    name="${name//\//_}"
    local out_file="${panels_dir}/${name}_${panel_id}_${version}.tsv"

    echo " - [$((++downloaded))/$total] ${panel_id} v${version} ${name}"
    if ! curl -fsSL -o "$out_file" "$url"; then
      die "Failed to download panel ${panel_id} (v${version}) from ${url}"
    fi
    sleep 0.1  # uncomment to be gentle on the server
  done
}

concat_tsvs() {
  # Args:
  #   $1 = out_dir
  local out_dir="$1"
  local panels_dir="$out_dir/panels"
  local combined="$out_dir/all_panels.tsv"

  [ -d "$panels_dir" ] || die "Panels directory not found: $panels_dir"

  # Find first TSV to capture header
  local first_tsv
  first_tsv="$(ls -1 "$panels_dir"/*.tsv 2>/dev/null | head -n 1)"
  [ -n "$first_tsv" ] || die "No TSV files found in $panels_dir"

  local header
  header="$(head -n 1 "$first_tsv")"
  [ -n "$header" ] || die "First TSV appears empty: $first_tsv"

  local header_us
  header_us="$(head -n 1 "$first_tsv" | tr -d '\r' | sed 's/ /_/g')"

  if [[ "$header_us" != "$EXPECTED_HEADER" ]]; then
    echo "Expected:$EXPECTED_HEADER"
    echo "Found:$header_us"
    # Helpful debug: show differences in a visible/escaped form
    printf 'Expected (repr): %q\n' "$EXPECTED_HEADER"
    printf 'Found    (repr): %q\n' "$header_us"
    die "Header in first TSV does not match expected format: $first_tsv"
  fi

  echo "Writing combined TSV: $combined"
  printf '%s\n' "$USED_HEADER" >"$combined" || die "Cannot write $combined"

  # Append body (skip header) of each panel TSV to combined + add Panel_ID and Panel_Version columns the file has name {id}_{version}.tsv
  for f in "$panels_dir"/*.tsv; do
    base=${f##*/}              # e.g. "Additional findings_Paediatric_3302_0.278.tsv"
    stem=${base%.tsv}          # "Additional findings_Paediatric_3302_0.278"

    panel_version=${stem##*_}        # "0.278"        (after last underscore)
    rest=${stem%_*}            # "Additional findings_Paediatric_3302"
    panel_id=${rest##*_}       # "3302"         (after second-to-last underscore)
    name=${rest%_*}            # "Additional findings_Paediatric" (optional)

    tail -n +2 "$f" | awk -F'\t' -v OFS='\t' -v id="$panel_id" -v ver="$panel_version" '
      NF==0 { next }                              # skip empty lines
      {
        gsub(/\r+$/, "", $0)                      # strip CR (Windows line endings)
        $1 = $1                                   # rebuild $0 from fields -> removes trailing tabs
        print $0, id, ver
      }
    ' >>"$combined" || die "Failed to append $f"
  done

  echo "Concatenation complete: $combined"
}

main() {
  require_cmd curl
  require_cmd jq

  local out_dir="$1"
  local api_base="${PANELAPP_API_BASE:-$API_BASE_DEFAULT}"

  if [ -z "$out_dir" ]; then
    echo "Usage: $0 OUTPUT_DIR" >&2
    exit 2
  fi
  if [ -e "$out_dir" ]; then
    die "Output directory already exists: $out_dir"
  fi

  mkdir -p "$out_dir" || die "Failed to create output dir: $out_dir"

  local manifest="$out_dir/panel_manifest.tsv"

  panelapp_panels_to_tsv "$manifest" "$api_base" || die "Failed to create panel manifest"
  download_all "$out_dir" "$api_base" || die "Failed to download panel TSVs"
  concat_tsvs "$out_dir" || die "Failed to concatenate panel TSVs"

  echo
  echo "Done."
  echo " - Manifest: $manifest"
  echo " - Panels dir: $out_dir/panels"
  echo " - Combined TSV: $out_dir/all_panels.tsv"
}

main "$@"
