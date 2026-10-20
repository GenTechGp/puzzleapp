# VCF Field Mapping Strategy

## Overview

The preprocessing pipeline (`run_preprocess` → `process_snv_data` / `process_sv_data`) converts raw VCF files into the tab-delimited TSV format consumed by the app. Different VCF callers and reference genomes use different field names for the same data (e.g. `gnomAD_AF_joint` in GRCh38 vs `gnomAD_AF` in CHM13/hs1). Rather than hardcoding field names, the pipeline uses **canonical field mapping TSVs** that declare which VCF fields map to which output columns.

A two-tier approach is used:

1. **Canonical mapping** — bundled with the package; covers standard WGS/WES VCFs annotated with VEP.
2. **User override mapping** — an optional TSV supplied per-run in the YAML config; merged on top of the canonical mapping (user wins on `output_column` key).

---

## Canonical mapping files

| Variant type | File |
|---|---|
| SNV / indel | `inst/extdata/preprocess/snv_field_mapping.tsv` |
| SV | `inst/extdata/preprocess/sv_field_mapping.tsv` |

### TSV schema

| Column | Description |
|---|---|
| `vcf_field` | VCF field name(s). A single name for direct renames; a **comma-separated list** when the output depends on multiple input fields. Field order matters — the code interprets it positionally (e.g. first field is the numerator, second is the denominator for a VAF). For `INFO_SUB` it is written `TAG\|SUBFIELD` (e.g. `SpliceAI\|DS_AG`). |
| `output_column` | Column name written to the output TSV. May appear on more than one row to declare alternative sources — see [Multiple sources for one output column](#multiple-sources-for-one-output-column). |
| `field_type` | One of `FORMAT`, `FORMAT_SV`, `CSQ`, `INFO`, `INFO_SUB` — tells the pipeline where in the VCF to look |
| `required` | `TRUE` / `FALSE` — if `TRUE` and the field is absent, preprocessing stops with an error |
| `note` | Human-readable description of the field and, for derived outputs, the formula |

### Field types

| `field_type` | Source in the VCF |
|---|---|
| `FORMAT` | Per-sample FORMAT tags (SNV/indel) |
| `FORMAT_SV` | Per-sample FORMAT tags (SV) |
| `CSQ` | VEP `CSQ` INFO subfields (parsed by `extract_annotation_columns`) |
| `INFO` | Top-level INFO keys (SV only) |
| `INFO_SUB` | Subfields of a pipe-delimited INFO tag whose header declares `Format: A\|B\|...`, such as `INFO/SpliceAI` added by `bcftools annotate` (parsed by `add_info_sub_columns`) |

### `INFO_SUB` fields

Some annotations are added outside VEP as a single INFO tag that packs several values into one pipe-delimited string. `bcftools annotate -c INFO/SpliceAI` with the precomputed SpliceAI score files writes:

```
##INFO=<ID=SpliceAI,...,Description="... Format: ALLELE|SYMBOL|DS_AG|DS_AL|DS_DG|DS_DL|DP_AG|DP_AL|DP_DG|DP_DL">
... SpliceAI=A|OR4F5|0.00|0.00|0.00|0.00|2|7|-41|-19 ...
```

An `INFO_SUB` row names the tag and the subfield, e.g. `SpliceAI|DS_DL → Donor_Loss`. Subfields are located by name using the header's `Format:`, so their order in the header does not matter.

- Records without the tag, or with `SpliceAI=.`, give `NA`.
- A tag can hold several comma-separated entries, e.g. one per overlapping gene (`C|PLEKHN1|...,C|PERM1|...`). One **whole** entry is kept per record so that all its subfields describe the same gene. If the tag has `DS_*` subfields, the entry with the highest delta score is kept (first entry on a tie); otherwise the first entry is kept.

---

## Multiple sources for one output column

An output column may be declared on several rows, one per possible source. The canonical SNV mapping does this for SpliceAI: each SpliceAI column has a `CSQ` row (VEP SpliceAI plugin, `SpliceAI_pred_*`) and an `INFO_SUB` row (`bcftools annotate`, `SpliceAI|*`).

The source is chosen **once per file**, from the VCF header:

1. Sources are tried by `field_type`: `INFO_SUB` before `CSQ`. Mapping row order only breaks ties between rows of the same type. The order of the VCF header lines has no effect.
2. The first source the header declares is used for every record. A source the header does not declare never overwrites one it does.
3. If no source is declared, the column is `NA` and a single warning names all of its sources (or preprocessing stops if any row is `required = TRUE`).

For SpliceAI this means:

| VCF annotated with | SpliceAI columns come from |
|---|---|
| VEP SpliceAI plugin only | `CSQ` (`SpliceAI_pred_*`) — unchanged behaviour |
| `bcftools annotate` only | `INFO/SpliceAI` |
| both | `INFO/SpliceAI`, for every record — a record with `SpliceAI=.` is `NA` even if CSQ has a score |
| neither | `NA`, with a warning |

---

## Schema limitations

This schema covers the common case well: a named VCF field (or a small set of named fields) that maps to a named output column. It deliberately does not encode the computation formula.

The reason is practical: different callers produce fields that require different formulas for the same output (e.g. SNV VAF = `AD / DP` where DP is pre-computed total depth, but SV VAF = `DV / (DV + DR)` where DR is reference reads). Encoding every such variant in the TSV would require a mini formula language — complexity with diminishing returns.

**What this means in practice:**

- A comma in `vcf_field` signals to the code that the output is derived from multiple input fields. The exact formula is hardcoded in `process_snv_data` / `process_sv_data`, not in the TSV.
- Some INFO fields require non-trivial transforms (e.g. `abs(SVLEN)`). These are handled by field name in the code, not by schema metadata.
- The `note` column is the human-readable description of the formula. It is not parsed by the code.

**If you need to add a field with a non-trivial computation:** declare the field mapping in the TSV (so the field name is configurable) and add the formula to the relevant processing function in the code.

---

## Graceful degradation

If a non-required field (`required = FALSE`) is absent from the VCF the pipeline:

- emits a `warning()` naming the missing field, and
- fills the output column with `NA`.

Required fields (`required = TRUE`) cause an immediate `stop()`.

---

## User override mappings

An override TSV uses the **same schema** as the canonical files. It only needs to include rows for fields that differ from the canonical mapping. Rows are merged on the `output_column` key — a user row with the same `output_column` as a canonical row replaces it entirely. Where the canonical mapping has several rows (sources) for that `output_column`, all of them are replaced, so list every source you want to keep.

### Example — CHM13 / hs1 reference genome

CHM13-annotated VCFs carry `gnomAD_AF` instead of `gnomAD_AF_joint` in the CSQ field. A one-row override file handles this:

```
vcf_field   output_column  field_type  required  note
gnomAD_AF   AF             CSQ         FALSE     hs1: gnomAD_AF replaces gnomAD_AF_joint
```

The canonical mapping maps `gnomAD_AF_joint → AF`; the override replaces that row with `gnomAD_AF → AF`.

---

## YAML configuration

Provide override paths in the `paths` block of the YAML config:

```yaml
paths:
  snvs_vcf:          /path/to/sample.snvs.vcf.gz
  snvs_tsv:          /path/to/output_snv.tsv
  svs_vcf:           /path/to/sample.svs.vcf.gz
  svs_tsv:           /path/to/output_sv.tsv

  # Optional — omit to use canonical defaults
  snv_field_mapping: /path/to/hs1_snv_field_mapping.tsv
  sv_field_mapping:  /path/to/hs1_sv_field_mapping.tsv
```

`snv_field_mapping` and `sv_field_mapping` are independent; you can override one without the other.

---

## Adding support for a new VCF caller

1. Identify which output columns the new caller names differently (compare your VCF header against the canonical TSV).
2. Create a small override TSV containing only the differing rows.
3. Point `snv_field_mapping` and/or `sv_field_mapping` in your YAML to that file.

No code changes are required — unless the new caller uses a formula that is not already handled, in which case the formula must be added to the processing function alongside the TSV entry.
