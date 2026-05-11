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
| `vcf_field` | Primary VCF field name (FORMAT tag, CSQ subfield, or INFO key) |
| `vcf_field_2` | Secondary field used in two-field computations (ratio / sum); empty otherwise |
| `output_column` | Column name written to the output TSV |
| `field_type` | One of `FORMAT`, `FORMAT_SV`, `CSQ`, `INFO` |
| `required` | `TRUE` / `FALSE` — if `TRUE` and the field is absent, preprocessing stops with an error |
| `computation` | How the output value is derived (see below) |
| `note` | Human-readable explanation |

### Computation types

| Value | Meaning |
|---|---|
| `direct` | Copy / rename `vcf_field` → `output_column` |
| `ratio` | `output_column = vcf_field / vcf_field_2` (e.g. VAF = AD / DP) |
| `sum` | `output_column = vcf_field + vcf_field_2` (e.g. DP = DV + DR) |
| `abs` | `output_column = abs(vcf_field)` (e.g. VAR_LENGTH = |SVLEN|) |

> **Note:** For `field_type = CSQ` the `computation` column is informational only — the pipeline always performs a direct rename for CSQ subfields.

### Field types

| `field_type` | Source in the VCF |
|---|---|
| `FORMAT` | Per-sample FORMAT tags (SNV/indel) |
| `FORMAT_SV` | Per-sample FORMAT tags (SV) |
| `CSQ` | VEP `CSQ` INFO subfields (parsed by `extract_csq_columns`) |
| `INFO` | Top-level INFO keys |

---

## Graceful degradation

If a non-required field (`required = FALSE`) is absent from the VCF the pipeline:

- emits a `warning()` naming the missing field, and
- fills the output column with `NA`.

Required fields (`required = TRUE`) cause an immediate `stop()`.

---

## User override mappings

An override TSV uses the **same schema** as the canonical files. It only needs to include rows for fields that differ from the canonical mapping. Rows are merged on the `output_column` key — a user row with the same `output_column` as a canonical row replaces it entirely.

### Example — CHM13 / hs1 reference genome

CHM13-annotated VCFs carry `gnomAD_AF` instead of `gnomAD_AF_joint` in the CSQ field. A one-row override file handles this:

```
vcf_field    vcf_field_2  output_column  field_type  required  computation  note
gnomAD_AF                 AF             CSQ         FALSE     direct       hs1: gnomAD_AF replaces gnomAD_AF_joint
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

No code changes are required.
