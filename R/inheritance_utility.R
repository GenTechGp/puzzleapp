#' Inheritance Utility
#'
library(data.table)

# Inheritance model selected?
#   │
#   ├── No/""  ──► (Skip inheritance constraints)
#   │
#   └── Yes ──────────────────────────────────────────────────────────────────────┐
#                                                                                │
#   Model ∈ {"Homozygous Recessive","Dominant/De Novo","Compound Heterozygous",  │
#            "X-Linked Recessive","Custom"}                                      │
#                                                                                │
#   For each sample in the pedigree (has sample_id, status, sex, code index):    │
#     • Compute expected alt-allele-count **string** with                        │
#       alleleCount(model, status, sex):                                         │
#         - Homozygous Recessive:   affected → "2";     unaffected → "0-1"       │
#         - Dominant / De Novo:     affected → "1-2";   unaffected → "0"         │
#         - Compound Heterozygous:  affected → "1";     unaffected → "0-1"       │
#         - X-Linked Recessive:     if male: affected→"1"; unaffected→"0"        │
#                                   if female: affected→"2"; unaffected→"0-1"    │
#         - Custom:                 take user radio value: "", "0","0-1","1",    │
#                                   "1-2","2" ("" means no constraint)           │
#                                                                                │
#     • If value == "": (Custom only) → skip this sample (no constraint)         │
#       Else build condition string:                                             │
#         compare_allele_count(get(paste0("alt_allele_count_", code)), value)    │
#                                                                                │
#   Combine all per-sample conditions with "&" (all must hold).                  │
#                                                                                │
#   Add this combined predicate to:                                              │
#     - filter_expression (row filter)                                           │
#     - global_filters_expression (for overrides to reuse)                       │
#                                                                                │
#   Special case for X-Linked Recessive:                                         │
#     - Wrap the final expression with "… & CHROM == 'chrX'" so only chrX rows   │
#       are retained (the code already applies this at the end).                 │
#                                                                                │
#   Result: Only rows consistent with the pedigree's expected alt-allele counts  │
#   under the selected model pass the initial filter.                            │
#                                                                                └────────────────────────
#
# SpliceAI / ClinVar "override" gates (when set) are OR'ed with the main filter:
#   combined_expression = (filter_expression)
#                         OR (global_filters_expression & spliceai_override)
#                         OR (global_filters_expression & clinvar_override)
#
# If model == "X-Linked Recessive":
#   → each OR arm is additionally constrained to CHROM == 'chrX'.
#
#
# If input$inher == "Compound Heterozygous":
#   Is this a trio? (parents present: kinship includes both "mother" and "father")
#     │
#     ├── No (non-trio) ──►
#     │      - Count per gene **separately by proband haplotype**:
#     │          • comp_hets_1: alt_allele_count_1 == 1 & GT_1 == "1|0"  → VAR_COUNT_1 by gene
#     │          • comp_hets_2: alt_allele_count_1 == 1 & GT_1 == "0|1"  → VAR_COUNT_2 by gene
#     │      - Keep genes where VAR_COUNT_1 > 0 **and** VAR_COUNT_2 > 0
#     │      - Filter dataset to variants whose GENE_SYMBOL ∈ kept genes
#     │
#     └── Yes (trio) ──►
#            - Identify genes with ≥2 proband hets **in trans from different parents**:
#                 alt_allele_count_1 == 1  AND
#                 ((GT_2 == "1|0" & GT_3 == "0|1") OR (GT_2 == "0|1" & GT_3 == "1|0"))
#               → summarise VAR_COUNT by GENE_SYMBOL
#            - Keep genes with VAR_COUNT > 1
#            - Filter dataset to variants whose GENE_SYMBOL ∈ kept genes

convert_samples_to_pedigree1 <- function(samples_list) {
  cat("Converting samples list to pedigree table with", length(samples_list), "samples.\n")

  # Pre-allocate empty vectors
  sample_ids <- character()
  statuses   <- character()
  sexes      <- character()
  codes      <- integer()

  for (i in seq_along(samples_list)) {
    s <- samples_list[[i]]
    # Debug print
    # cat("Processing sample", i, "\n")
    # print(s)

    # Extract values safely with defaults
    sid  <- if (!is.null(s$sample_id)) as.character(s$sample_id) else NA_character_
    st   <- if (!is.null(s$status))    as.character(s$status)    else NA_character_
    sx   <- if (!is.null(s$sex))       as.character(s$sex)       else NA_character_
    cd   <- if (!is.null(s$code))      as.integer(s$code)        else NA_integer_

    # cat("  sample_id:", sid, " status:", st, " sex:", sx, " code:", cd, "\n")

    # Append
    sample_ids <- c(sample_ids, sid)
    statuses   <- c(statuses, st)
    sexes      <- c(sexes, sx)
    codes      <- c(codes, cd)
  }

  pedigree <- data.table::data.table(
    sample_id = sample_ids,
    status    = statuses,
    sex       = sexes,
    code      = codes
  )
  # Debug print
  # cat("pedigree table:\n")
  # print(pedigree)

  pedigree
}


convert_samples_to_pedigree <- function(samples_list) {
  cat("Converting", length(samples_list), "samples to pedigree list.\n")
  
  pedigree <- lapply(samples_list, function(s) {
    list(
      sample_id = s$sample_id,
      status    = s$status,
      sex       = s$sex,
      code      = s$code
    )
  })
  print(pedigree)
  pedigree
}

# --- Helper: compute allele count per sample ---
alleleCount <- function(inher, status, sex) {
  if (status == "NA") return("")  # special case

  if (inher == "Homozygous Recessive") {
    if (status == "affected") "2" else "0-1"
  } else if (inher == "Dominant/De Novo") {
    if (status == "affected") "1-2" else "0"
  } else if (inher == "Compound Heterozygous") {
    if (status == "affected") "1" else "0-1"
  } else if (inher == "X-Linked Recessive") {
    if (sex == "male") {
      if (status == "affected") "1" else "0"
    } else {
      if (status == "affected") "2" else "0-1"
    }
  } else {
    ""
  }
}

# --- Compute allele counts for standard inheritance models as a named list ---
compute_allele_table <- function(pedigree, inher) {
  if (length(pedigree) == 0 || inher == "") return(list())
  res <- list()
  for (sample in pedigree) {
    sid <- sample$sample_id
    res[[sid]] <- alleleCount(inher, sample$status, sample$sex)
  }
  res
}

