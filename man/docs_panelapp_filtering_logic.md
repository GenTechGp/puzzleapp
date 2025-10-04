# Variant Filtering Logic: PanelApp, Positive, and Negative Gene Lists

This document specifies the end-to-end filtering and annotation behavior for variants based on:
- Selected PanelApp panels (Level4 set),
- A checkbox to invert PanelApp selection (“treat as negative”),
- A free-form Positive genes list,
- A free-form Negative genes list,
- And PanelApp-derived annotation.

It is intended to guide both implementation and maintenance, and to serve as a reference for expected outcomes across scenarios.

---

## Overview

We compute the final set of genes to retain using three layers with strict precedence:

1) Negative list (highest precedence): If a gene is in the Negative list, it is excluded.
2) Positive list (middle precedence): If a gene is in the Positive list (and not in Negative), it is included.
3) PanelApp base logic (lowest precedence): Includes or excludes genes according to selected PanelApp panels and the checkbox mode.

Special rule:
- If no PanelApp panels are selected AND the Positive list is non-empty, keep ONLY the Positive list genes (minus any in the Negative list). Do not pass through all other genes.

Annotation:
- Always merge PanelApp annotation when available, regardless of whether panels were selected.
- Annotation sources include:
  - Genes from the selected panels (if any),
  - Genes in Positive list that exist in the PanelApp master table,
  - Genes in Negative list that exist in the PanelApp master table.
- Aggregation per gene:
  - PANEL_APP = semicolon-joined unique Level4 values,
  - INHERITANCE = semicolon-joined unique Model_Of_Inheritance values.
- Genes without annotation retain PANEL_APP = NA and INHERITANCE = NA.

---

## Inputs

- Dataset (variants) with a character column `GENE_SYMBOL`.
- PanelApp master table `panel_app_genes` with columns:
  - `Entity_Name` (gene symbol),
  - `Level4` (panel name/class),
  - `Model_Of_Inheritance` (inheritance label).

- Filters:
  - `filters$panelapp_filter`: character vector of selected PanelApp Level4 names (can be empty).
  - `filters$panelapp_negative_genes`: logical
    - FALSE = Inclusion mode (base set is “in selected panels”),
    - TRUE = Exclusion mode (base set is “not in selected panels”).
  - `filters$positive_genes`: free text list of genes (comma/semicolon/tab/space/newline separated).
  - `filters$negative_genes`: free text list of genes (same separators).

Normalization:
- Parse Positive/Negative lists by splitting on `[ , ; \\t \\n ]+`, trimming whitespace, dropping empty tokens.
- Normalize gene symbol case consistently with your dataset (commonly uppercase).
- Deduplicate lists.

---

## Sets and Notation

- U = all unique `GENE_SYMBOL` in the dataset.
- P = genes from `panel_app_genes` whose `Level4` is in `filters$panelapp_filter` (can be empty).
- Pos = parsed Positive list (normalized).
- Neg = parsed Negative list (normalized).

Precedence: Neg > Pos > PanelApp base.

---

## Final Selection Logic

Let F be the final set of genes to retain.

1) If NO panels are selected (`length(filters$panelapp_filter) == 0`):
   - If `length(Pos) > 0`:
     - F = Pos \ Neg
   - Else if `length(Neg) > 0` (and Pos is empty):
     - F = U \ Neg
   - Else (Pos and Neg both empty):
     - F = U

2) If panels ARE selected:
   - Compute P from `panel_app_genes` using the selected `Level4` terms.
   - Base by mode:
     - Inclusion mode (checkbox FALSE):
       - If `length(P) > 0`: B0 = P
       - Else (selected panels yielded no genes): B0 = ∅ (empty set)
     - Exclusion mode (checkbox TRUE):
       - If `length(P) > 0`: B0 = U \ P
       - Else (selected panels yielded no genes): B0 = U
   - Apply overrides:
     - Positive override: B1 = B0 ∪ Pos
     - Negative override: F = B1 \ Neg

Behavioral highlights:
- Negative overrides Positive and PanelApp membership.
- Positive overrides the PanelApp base (e.g., rescues a gene excluded by Exclusion mode).
- If a gene is mentioned in Pos/Neg but not present in the dataset, it has no effect on rows (it still may appear in the annotation table but will not merge into the dataset without matching rows).

Per-gene decision (conceptual):
- If gene ∈ Neg: exclude.
- Else if gene ∈ Pos: include.
- Else:
  - If no panels selected: include (unless Neg-only case applies above).
  - If panels selected:
    - Inclusion mode: include iff gene ∈ P.
    - Exclusion mode: include iff gene ∉ P.

---

## Annotation Logic (Always On)

Scope:
- Always produce and merge annotation regardless of panel selection.

Annotation sources (union):
- Selected panel genes P (if any),
- Pos ∩ genes that exist in `panel_app_genes`,
- Neg ∩ genes that exist in `panel_app_genes`.

Aggregation per gene:
- PANEL_APP = `paste(unique(Level4), collapse = ";")`
- INHERITANCE = `paste(unique(Model_Of_Inheritance), collapse = ";")`

Merge:
- Left-merge the aggregated annotation onto the filtered dataset by `GENE_SYMBOL`.
- Ensure the columns always exist in the result:
  - If absent, create `PANEL_APP := NA_character_` and `INHERITANCE := NA_character_`.

Notes:
- In Exclusion mode, a gene in P can be excluded by base logic but re-included by Pos; it will still annotate because it exists in the annotation sources.
- Genes in Pos/Neg that are not found in `panel_app_genes` receive NA annotation after merge.

---

## Edge Cases and Clarifications

- Panels selected but P is empty:
  - Inclusion mode (no Pos): final set is empty.
  - Exclusion mode: base is U; Pos/Neg overrides apply normally.
- Panels not selected and Pos non-empty:
  - Keep ONLY Pos \ Neg (do not pass-through all genes).
- Empty Pos and Neg:
  - Behavior driven exclusively by PanelApp mode (or pass-through if no panels selected).
- Case differences:
  - Normalize gene symbols for stable matching across dataset, PanelApp, and Pos/Neg inputs.
- Duplicates:
  - Deduplicate parsed Pos/Neg lists; aggregation removes duplicate annotation rows.
- Out-of-universe inputs:
  - Genes in Pos/Neg not present in U have no effect on rows; they can only contribute annotation if present in `panel_app_genes`, which then won’t merge if rows are absent.

---

## Implementation Sketch (Pseudocode)

```r
# Parse lists
Pos <- parse_gene_list(filters$positive_genes) # normalized case
Neg <- parse_gene_list(filters$negative_genes)

# Build sets
U <- unique(dataset$GENE_SYMBOL)
P <- unique(panel_app_genes[Level4 %in% filters$panelapp_filter, Entity_Name])

# Final set F
if (length(filters$panelapp_filter) == 0) {
  if (length(Pos) > 0) {
    F <- setdiff(Pos, Neg)
  } else if (length(Neg) > 0) {
    F <- setdiff(U, Neg)
  } else {
    F <- U
  }
} else {
  if (!isTRUE(filters$panelapp_negative_genes)) {
    B0 <- if (length(P) > 0) P else character(0)                  # inclusion
  } else {
    B0 <- if (length(P) > 0) setdiff(U, P) else U                 # exclusion
  }
  B1 <- union(B0, Pos)
  F  <- setdiff(B1, Neg)
}

# Filter rows
filtered <- dataset[GENE_SYMBOL %in% F]

# Build annotation sources (always)
ann_rows <- panel_app_genes[
  Entity_Name %in% union(P, union(
    intersect(Pos, toupper(panel_app_genes$Entity_Name)), 
    intersect(Neg, toupper(panel_app_genes$Entity_Name))
  ))
]

annotation <- ann_rows[, .(
  PANEL_APP   = paste(unique(Level4), collapse = ";"),
  INHERITANCE = paste(unique(Model_Of_Inheritance), collapse = ";")
), by = .(GENE_SYMBOL = Entity_Name)]

# Merge annotation
filtered[annotation, on = "GENE_SYMBOL", `:=`(
  PANEL_APP   = i.PANEL_APP,
  INHERITANCE = i.INHERITANCE
)]
if (!"PANEL_APP" %in% names(filtered))    filtered[, PANEL_APP := NA_character_]
if (!"INHERITANCE" %in% names(filtered))  filtered[, INHERITANCE := NA_character_]
```

---

## Validation Tips

- Log counts before/after each phase (PanelApp base, Pos addition, Neg subtraction).
- Log sizes of P, Pos, Neg sets and their overlaps.
- Add temporary provenance flags for debugging (e.g., `FROM_PANELAPP`, `IN_POS`, `IN_NEG`) to confirm precedence-driven outcomes.
- Include tests for:
  - No panels, Pos only (keeps Pos \ Neg).
  - Inclusion mode with empty P (empty result unless Pos present).
  - Exclusion mode re-include via Pos.
  - Neg overriding Pos and/or PanelApp.
  - Annotation presence with and without panels selected.

---

## Summary

- Final inclusion is determined by a strict precedence: Neg > Pos > PanelApp base.
- “Panels empty + Pos non-empty” forces a Pos-only result (minus Neg).
- Annotation is always attempted and merged, sourced from selected panels and from Pos/Neg genes present in the PanelApp master, aggregated across all their panel entries.
- Genes without PanelApp reference retain NA annotation.