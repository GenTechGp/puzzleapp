#' Filter Utility
#'#' Utility functions for filtering datasets based on user-defined criteria.
#'

check_snvs_data <- function(dt, expected_cols) {
  # Check if data is NULL or empty
  if (is.null(dt) || nrow(dt) == 0) {
    warning("SNVs dataset is NULL or empty. Skipping filtering.")
    return(FALSE)
  }
  expected_cols <- c("AF", "QUAL", "VAF_1", "SIFT", "PolyPhen", "CLINVAR", "Donor_Loss",
                   "Donor_Gain", "Acceptor_Loss", "Acceptor_Gain")
  # Check if all expected columns exist
  missing_cols <- setdiff(expected_cols, colnames(dt))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "SNVs dataset is missing expected columns: %s. Skipping filtering.",
      paste(missing_cols, collapse = ", ")
    ))
    return(FALSE)
  }
  TRUE
}

apply_snv_filters_debug <- function(dt, filters) {
  cat("AF filter value:", filters$af, "\n")
  af_vec <- dt[["AF"]]  # vector of numeric values
  filtered <- dt[is.na(af_vec) | af_vec <= filters$af, ]
  cat("nrow after filter:", nrow(filtered), "\n")
  filtered
}

clinvar_filtering <- function(dt, filters) {
  # Start
  #   │
  #   ▼
  # Is filters$clinvar_filter set and non-empty?
  #   │
  #   ├── No ──► (Skip ClinVar filtering)
  #   │
  #   └── Yes
  #         │
  #         ▼
  #   Replace "VUS" → "uncertain"
  #         │
  #         ▼
  #   Build regex pattern from terms
  #         │
  #         ▼
  #   Does filter include "Not available"?
  #         │
  #         ├── Yes ──► clinvar_condition =
  #         │           (grepl(pattern, CLINVAR, ignore.case=TRUE) OR is.na(CLINVAR))
  #         │
  #         └── No ───► clinvar_condition =
  #                     grepl(pattern, CLINVAR, ignore.case=TRUE)
  #         │
  #         ▼
  #   Add clinvar_condition to filter_expression
  #         │
  #         ▼
  #   Collect override terms:
  #     - Pathogenic?
  #     - Likely pathogenic?
  #     - uncertain?
  #         │
  #         ▼
  #   Any override terms collected?
  #         │
  #         ├── No ──► (No override condition defined)
  #         │
  #         └── Yes ─► clinvar_override_condition =
  #                     grepl(override_pattern, CLINVAR, ignore.case=TRUE)
  #                     AND (is.na(AF) OR AF < 0.05)

  filter_expr <- quote(TRUE)
  # ClinVar pathogenicity filter (checkboxes)
  if (!is.null(filters$clinvar_checkboxes) && length(filters$clinvar_checkboxes) > 0) {
    cat("[filtServer][filter_dataset] Applying CLINVAR filter for:",
        paste(filters$clinvar_checkboxes, collapse = ", "), "\n")

    # 1. Normalize UI labels -> dataset format
    # Example:
    #   "Likely pathogenic" -> "likely_pathogenic"
    #   "Benign"            -> "benign"
    #   "Uncertain significance" -> "uncertain_significance"
    checkbox_map <- tolower(gsub(" ", "_", filters$clinvar_checkboxes))

    # 2. Build regex pattern for all terms except "Not available"
    valid_terms <- setdiff(checkbox_map, "not_available")
    clinvar_pattern <- NULL
    if (length(valid_terms) > 0) {
      clinvar_pattern <- paste(
        sapply(valid_terms, function(x) paste0("\\b", x, "\\b")),
        collapse = "|"
      )
    }

    # 3. Build condition
    if ("not_available" %in% checkbox_map && !is.null(clinvar_pattern)) {
      # Include NA values AND matching pattern
      clinvar_condition <- bquote(
        (grepl(.(clinvar_pattern), dt[["CLINVAR"]], ignore.case = TRUE) | is.na(dt[["CLINVAR"]]))
      )
    } else if ("not_available" %in% checkbox_map && is.null(clinvar_pattern)) {
      # Only "Not available" selected
      clinvar_condition <- bquote(is.na(dt[["CLINVAR"]]))
    } else {
      # Only pattern-based terms selected
      clinvar_condition <- bquote(grepl(.(clinvar_pattern), dt[["CLINVAR"]], ignore.case = TRUE))
    }

    # 4. Add to overall filter expression
    filter_expr <- bquote(.(filter_expr) & .(clinvar_condition))

    # 5. Optional override: keep rare variants (<5%) if flagged as Pathogenic / Likely pathogenic / Uncertain significance
    # not used for now
    override_patterns <- c()
    if ("pathogenic" %in% checkbox_map) override_patterns <- c(override_patterns, "\\bpathogenic\\b")
    if ("likely_pathogenic" %in% checkbox_map) override_patterns <- c(override_patterns, "\\blikely_pathogenic\\b")
    if ("uncertain_significance" %in% checkbox_map) override_patterns <- c(override_patterns, "\\buncertain_significance\\b")

    if (length(override_patterns) > 0) {
      override_pattern <- paste(override_patterns, collapse = "|")
      clinvar_override_condition <- bquote(
        (grepl(.(override_pattern), dt[["CLINVAR"]], ignore.case = TRUE) & (is.na(dt[["AF"]]) | dt[["AF"]] < 0.05))
      )
    }
  }
  filter_expr
}

# Compare column values to allele counts (vectorized)
compare_allele_count <- function(col, values) {
  if (is.null(values) || values == "") return(rep(TRUE, length(col)))
  rng <- as.numeric(unlist(strsplit(values, "-")))
  if (length(rng) == 1) return(col == rng)
  state <- col >= rng[1] & col <= rng[2]
  state
}

build_inheritance_filter_vec <- function(dt, pedigree, allele_counts) {
  if (length(pedigree) == 0) return(rep(TRUE, nrow(dt)))
  mask <- rep(TRUE, nrow(dt))
  for (i in seq_along(pedigree)) {
    sid <- pedigree[[i]]$sample_id
    val <- allele_counts[[sid]] %||% ""
    if (val == "") next
    col_name <- paste0("alt_allele_count_", pedigree[[i]]$code)
    mask <- mask & compare_allele_count(dt[[col_name]], val)
  }
  mask
}


build_inheritance_filter_expr <- function(pedigree, allele_counts) {
  filter_expr <- quote(TRUE)
  if (length(pedigree) == 0) return(filter_expr)
  for (i in seq_along(pedigree)) {
    sid <- pedigree[[i]]$sample_id
    val <- allele_counts[[sid]] %||% ""
    if (val == "") next
    col_name <- paste0("alt_allele_count_", pedigree[[i]]$code)
    # filter_expr <- bquote(.(filter_expr) & compare_allele_count(get(.(col_name)), .(val)))
    # Use dt[[col_name]] instead of get()
    filter_expr <- bquote(
      .(filter_expr) & compare_allele_count(dt[[.(col_name)]], .(val))
    )
  }
  filter_expr
}

apply_filters <- function(pedigree, allele_counts, dt, filters, type, vep_consequences) {
  if (!check_snvs_data(dt)) {
    return(dt)  # return original data if checks fail
  }

  # Start with a filter expression that always passes
  filter_expr <- quote(TRUE)

  # inheritance filter
  inheritance_filter_expr <- build_inheritance_filter_expr(pedigree, allele_counts)
  filter_expr <- bquote(.(filter_expr) & .(inheritance_filter_expr))
  # dt <- dt[eval(filter_expr)]
  # mask <- buildInheritanceFilter_vec(dt, pedigree, allele_counts)
  # dt <- dt[mask]

  # SpliceAI filter; each column is checked separately, and a variant only survives if all four columns are NA or above threshold.
  if (!is.null(filters$spliceai_score)) {
    spliceai_cols <- c("Donor_Gain", "Donor_Loss", "Acceptor_Gain", "Acceptor_Loss")
    spliceai_cols <- spliceai_cols[spliceai_cols %in% names(dt)]
    if (length(spliceai_cols) > 0) {
      spliceai_exprs <- lapply(spliceai_cols, function(col) {
        bquote(is.na(dt[[.(col)]]) | dt[[.(col)]] >= .(filters$spliceai_score))
      })
      filter_expr <- Reduce(function(x, y) bquote(.(x) & .(y)), spliceai_exprs, init = filter_expr)
    }
  }

  # Consequence filter (checkboxes)
  if (!is.null(filters$conseq_checkboxes) && length(filters$conseq_checkboxes) > 0) {
    # Expand user-friendly groups into underlying terms
    selected_terms <- unlist(vep_consequences[filters$conseq_checkboxes], use.names = FALSE)
    cat("Applying CONSEQUENCE filter for groups:", paste(filters$conseq_checkboxes, collapse = ", "), "\nExpanded terms:", paste(unique(selected_terms), collapse = ", "), "\n")
    filter_expr <- bquote(.(filter_expr) & (dt[["CONSEQUENCE"]] %in% .(selected_terms)))
  }

  # Clinvar pathogenicity filter (checkboxes) todo
  clinvar_filter_expr <- clinvar_filtering(dt, filters)
  filter_expr <- bquote(.(filter_expr) & .(clinvar_filter_expr))

  # Revel score filter
  if (!is.null(filters$revel) && filters$revel > 0) {
    filter_expr <- bquote(.(filter_expr) & (is.na(dt[["REVEL"]]) | dt[["REVEL"]] >= .(filters$revel)))
  }

  # AlphaMissense score filter # todo: check
  if (!is.null(filters$alpha_missense) && filters$alpha_missense > 0) {
    filter_expr <- bquote(.(filter_expr) & (is.na(dt[["am_pathogenicity"]]) | dt[["am_pathogenicity"]] >= .(filters$alpha_missense)))
  }

  # SIFT filter
  if (!is.null(filters$sift_filter) && nzchar(filters$sift_filter)) {
    filter_expr <- bquote(.(filter_expr) & grepl(.(filters$sift_filter), dt[["SIFT"]], ignore.case = TRUE))
  }

  # PolyPhen filter
  if (!is.null(filters$polyphen_filter) && nzchar(filters$polyphen_filter)) {
    filter_expr <- bquote(.(filter_expr) & grepl(.(filters$polyphen_filter), dt[["PolyPhen"]], ignore.case = TRUE))
  }

  # PASS-only filter
  if (isTRUE(filters$pass_only)) {
    # "PASS" indicates variants that passed all filters; "." typically means missing or unfiltered values.
    # filter_expr <- bquote(.(filter_expr) & (dt[["FILTER"]] %in% c("PASS", ".")))
    filter_expr <- bquote(.(filter_expr) & (dt[["FILTER"]] %in% c("PASS")))
  }

  # Genotype quality filter (QUAL column)
  if (!is.null(filters$genotype_quality) && filters$genotype_quality > 0) {
    filter_expr <- bquote(.(filter_expr) & (dt[["QUAL"]] >= .(filters$genotype_quality)))
  }

  # Allele balance / VAF filter
  if (!is.null(filters$allele_balance)) {
    vaf_cols <- grep("^VAF_", names(dt), value = TRUE)
    cat("VAF columns found for allele balance filter:", paste(vaf_cols, collapse = ", "), "\n")
    if (length(vaf_cols) > 0) {
      vaf_exprs <- lapply(vaf_cols, function(col) {
        bquote(is.na(dt[[.(col)]]) | dt[[.(col)]] >= .(filters$allele_balance))
      })
      filter_expr <- Reduce(function(x, y) bquote(.(x) & .(y)), vaf_exprs, init = filter_expr)
    }
  }

  # Affected-only filter (assumes per-sample GT columns exist)
  if (isTRUE(filters$affected_only)) {
    gt_cols <- grep("^GT_", names(dt), value = TRUE)
    # cat("GT columns found for affected-only filter:", paste(gt_cols, collapse = ", "), "\n")
    if (length(gt_cols) > 0) {
      gt_exprs <- lapply(gt_cols, function(col) {
        bquote(grepl("0/1|1/1", dt[[.(col)]]))
      })
      filter_expr <- Reduce(function(x, y) bquote(.(x) & .(y)), gt_exprs, init = filter_expr)
    }
  }

  # AF filter
  af_val <- as.numeric(filters$af)
  if (!is.null(af_val) && !is.na(af_val)) {
    filter_expr <- bquote(.(filter_expr) & (is.na(dt[["AF"]]) | dt[["AF"]] <= .(af_val)))
  }

  # SV features filter (checkboxes)
  if (type == "sv" && !is.null(filters$sv_features_checkboxes) && length(filters$sv_features_checkboxes) > 0) {
    sv_type_map <- list("Insertion" = "INS", "Deletion" = "DEL", "Duplication" = "DUP", "Inversion" = "INV", "Translocation" = "TRA|BND")
    cat("Applying SV FEATURES filter for:", paste(filters$sv_features_checkboxes, collapse = ", "), "\n")
    sv_types <- unlist(sv_type_map[filters$sv_features_checkboxes])
    sv_pattern <- paste(sv_types, collapse = "|")
    filter_expr <- bquote(.(filter_expr) & grepl(.(sv_pattern), dt[["VAR_TYPE"]], ignore.case = TRUE))
  }

  # SV length filters
  if (type == "sv") {
    if (!is.null(filters$sv_min_len) && !is.na(filters$sv_min_len) && filters$sv_min_len > 0) {
      filter_expr <- bquote(.(filter_expr) & (is.na(dt[["VAR_LENGTH"]]) | dt[["VAR_LENGTH"]] >= .(filters$sv_min_len)))
    }
    if (!is.null(filters$sv_max_len) && !is.na(filters$sv_max_len) && filters$sv_max_len > 0) {
      filter_expr <- bquote(.(filter_expr) & (is.na(dt[["VAR_LENGTH"]]) | dt[["VAR_LENGTH"]] <= .(filters$sv_max_len)))
    }
  }

  # Apply filter and return filtered data.table
  filtered_dt <- dt[eval(filter_expr), ]
  cat("nrow after filtering:", nrow(filtered_dt), "\n")
  filtered_dt
}
