format_time <- function(time) {
  paste(round(time["elapsed"], 3), "seconds")
}

# Helper function: Generate condition for allele count filtering
compare_allele_count <- function(col, values) {
  values <- as.numeric(unlist(strsplit(values, "-")))
  if (length(values) == 1) return(col == values)
  if (length(values) == 2) return(col >= values[1] & col <= values[2])
  return(rep(TRUE, length(col)))
}

# Helper function: Construct filter expression
add_filter_condition <- function(filter_expression, condition) {
  if (!is.null(condition) && condition != "") {
    log_info(sprintf("Adding filter condition: %s", condition))
    return(paste(filter_expression, condition, sep = " & "))
  }
  return(filter_expression)
}

# Helper function: Handle inheritance filtering
inheritance_filter <- function(filters, pedigree, allele_tab) {
  #browser()
  if (!is.null(filters$inheritance_filter) && filters$inheritance_filter != "") {
    allele_count <- allele_tab
    names(allele_count) <- c("sample_id", "allele_count")
    print(pedigree)
    # browser()
    # pedigree[, code := seq_len(.N)]
    pedigree$code <- seq_len(nrow(pedigree))
    allele_count <- merge(pedigree, allele_count, by = "sample_id")
    # allele_count[, col_name := paste0("alt_allele_count_", code), by = sample_id]
    allele_count$col_name <- paste0("alt_allele_count_", allele_count$code)


    inheritance_conditions <- vector("character")

    for (i in seq_len(nrow(allele_count))) {
      col_name <- allele_count[i, "col_name"]
      val <- allele_count[i, "allele_count"]
      # condition <- sprintf("compare_allele_count(get('%s'), '%s')",col_name,val)
      # inheritance_conditions <- c(inheritance_conditions, condition)
      if (val != "") {
        condition <- sprintf("compare_allele_count(get('%s'), '%s')", col_name, val)
        inheritance_conditions <- c(inheritance_conditions, condition)
      }
    }

    # Combine inheritance conditions into a single OR expression #AND or OR?
    if (length(inheritance_conditions) > 0) {
      inheritance_expression <- paste(inheritance_conditions, collapse = " & ")
      return(inheritance_expression)
    } else {
      return(NULL)
    }
  }
  return(NULL)
}

# Helper function: Apply text-based filters
text_filter <- function(column, values) {
  if (length(values) == 0) return(NULL)
  return(sprintf("grepl('%s', %s, ignore.case = TRUE)", paste(values, collapse = "|"), column))
}

# Helper function: Handle frequency and quality filters
quality_filters <- function(filters, data, pedigree) {
  conditions <- list()
  # let's add filter pass_variants to look at FILTER column
  if (!is.null(filters$pass_variants) && filters$pass_variants == 'PASS only variants') {
    conditions <- c(conditions, "FILTER == 'PASS'")
  }

  if (!is.null(filters$af_value) && filters$af_value < 1) 
    conditions <- c(conditions, sprintf("(is.na(AF) | AF <= %f)", filters$af_value))

  # check if pedigree samples have status "affected" if so get their codes
  gq_vars <- grep("^GQ_", colnames(data), value = TRUE)
  vaf_vars <- grep("^VAF_", colnames(data), value = TRUE)
  if (isTRUE(filters$affected_only)) {
    affected_codes <- pedigree$code[pedigree$status == "affected"]
    log_info(sprintf("Affected codes: %s", affected_codes))
    gq_vars <- paste0("GQ_", affected_codes)
    vaf_vars <- paste0("VAF_", affected_codes)
  }

  if (!is.null(filters$genotype_quality_value) && filters$genotype_quality_value > 0 && length(gq_vars) > 0) {
    conditions <- c(conditions, paste(sprintf("as.numeric(get('%s')) >= %f", gq_vars, filters$genotype_quality_value), collapse = " & "))
  }

  if (!is.null(filters$allele_balance_value) && filters$allele_balance_value > 0 && length(vaf_vars) > 0) {
    conditions <- c(conditions, paste(sprintf("as.numeric(get('%s')) >= %f", vaf_vars, filters$allele_balance_value), collapse = " & "))
  }

  return(paste(conditions, collapse = " & "))
}

# Helper: parse and normalize user-entered gene lists
parse_gene_list <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character(0))
  y <- unlist(strsplit(x, "[,;[:space:]]+"))
  y <- trimws(y)
  y <- y[nzchar(y)]
  unique(toupper(y))
}

# Helper function: Apply PanelApp and custom gene filters
panelapp_filter <- function(filters, panel_app_genes) {
  # custom_genes is already parsed (character vector). May be NULL/empty.
  custom_vec <- filters$custom_genes
  if (is.null(custom_vec)) custom_vec <- character(0)

  # New subtract inputs (already parsed). May be NULL/empty.
  sub_panel_levels <- filters$substract_panelapp_gene_lists_filter
  if (is.null(sub_panel_levels)) sub_panel_levels <- character(0)
  sub_gene_vec <- filters$substract_panelapp_genes_filter
  if (is.null(sub_gene_vec)) sub_gene_vec <- character(0)

  # Minimal behavior: only act if there is a positive selection (PanelApp or custom).
  have_filters <- (length(filters$panelapp_filter) > 0) || (length(custom_vec) > 0)
  if (!have_filters) return(NULL)

  pa <- data.table::as.data.table(panel_app_genes)

  # 1) Build PanelApp-derived set (PanelApp membership) from selected Level4 lists
  pa_membership <- pa[Level4 %in% filters$panelapp_filter]

  # 2) Apply subtraction ONLY to PanelApp membership, BEFORE adding custom genes.
  #    This means subtraction controls only the PanelApp membership and does not affect custom genes.
  #    If a gene is subtracted here but present in custom_genes, it will still be kept due to custom.
  if (length(sub_panel_levels) > 0 || length(sub_gene_vec) > 0) {
    remove_from_panels <- character(0)
    if (length(sub_panel_levels) > 0) {
      remove_from_panels <- unique(toupper(pa[Level4 %in% sub_panel_levels, Entity_Name]))
    }
    remove_from_genes <- if (length(sub_gene_vec) > 0) unique(toupper(sub_gene_vec)) else character(0)
    remove_set <- unique(c(remove_from_panels, remove_from_genes))

    if (length(remove_set) > 0) {
      pa_membership <- pa_membership[!(toupper(Entity_Name) %in% remove_set)]
    }
  }

  # Map PanelApp membership rows to a unified schema
  genes_raw <- pa_membership[
    ,
    .(
      PANEL_APP = Level4,                                 # PanelApp membership (Level4)
      GENE_SYMBOL = toupper(Entity_Name),                 # normalized for matching
      INHERITANCE = ifelse(is.na(Model_Of_Inheritance), "", Model_Of_Inheritance)
    )
  ]

  # 3) Add custom genes AFTER subtraction, as independent membership "CUSTOM"
  #    This ensures custom genes can re-introduce a gene even if its PanelApp membership was subtracted.
  if (length(custom_vec) > 0) {
    custom_raw <- data.table::data.table(
      PANEL_APP = rep("CUSTOM", length(custom_vec)),
      GENE_SYMBOL = toupper(custom_vec),
      INHERITANCE = ""
    )
    genes_raw <- data.table::rbindlist(list(genes_raw, custom_raw), use.names = TRUE, fill = TRUE)
  }

  # Collapse values ignoring empty strings; return empty string if nothing to show
  collapse_empty <- function(x) {
    z <- unique(x[!is.na(x) & nzchar(x)])
    if (length(z) == 0) "" else paste(z, collapse = ";")
  }

  # 4) Collapse by gene symbol to produce final table with combined PanelApp membership and inheritance
  genes <- genes_raw[
    ,
    .(
      PANEL_APP = collapse_empty(PANEL_APP),
      INHERITANCE = collapse_empty(INHERITANCE)
    ),
    by = GENE_SYMBOL
  ]

  genes_search <- unique(genes$GENE_SYMBOL)

  # 5) Build condition; treat_negative toggles the final membership result
  treat_neg <- isTRUE(filters$treat_negative)
  panel_app_condition <- if (treat_neg) {
    "!(GENE_SYMBOL %in% genes_search)"
  } else {
    "GENE_SYMBOL %in% genes_search"
  }

  # Return:
  # - condition string that refers to 'genes_search' symbol (to be evaluated in filter context)
  # - the vector of symbols
  # - the per-gene table with aggregated PanelApp membership and inheritance
  list(panel_app_condition, genes_search, genes)
}

# Generic filter function for SNVs and SVs
filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE) {
  # Return NULL immediately if input is NULL
  if (is.null(data)) return(NULL)
  if (nrow(data) == 0) return(data)
  if (!is.character(data$GENE_SYMBOL)) {
    stop("Error: data$GENE_SYMBOL must be of type character, but is ", class(data$GENE_SYMBOL))
  }
  log_info(sprintf("[filtServer][filter_dataset] Filtering dataset (SNV: %s)", is_snv))

  filter_expression <- "TRUE"
  global_filters_expression <- "TRUE"

  # Apply common filters
  inheritance_filter_condition <- inheritance_filter(filters, pedigree, allele_tab)
  if (!is.null(inheritance_filter_condition)) {
    log_info("[filtServer][filter_dataset] Applying inheritance filter")
    filter_expression <- add_filter_condition(filter_expression, inheritance_filter_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, inheritance_filter_condition)
  }
  panelapp_filter_results <- panelapp_filter(filters, panel_app_genes)
  if (!is.null(panelapp_filter_results)) {
    log_info("[filtServer][filter_dataset] Applying PanelApp filter")
    panelapp_filter_condition <- panelapp_filter_results[[1]]
    genes_search <- panelapp_filter_results[[2]]
    genes <- panelapp_filter_results[[3]]
    print(class(genes))
    filter_expression <- add_filter_condition(filter_expression, panelapp_filter_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, panelapp_filter_condition)
  }
  
  log_info("[filtServer][filter_dataset] Applying quality filters")
  filter_expression <- add_filter_condition(filter_expression, quality_filters(filters, data, pedigree))

  # Apply VEP Annotation filter (for both SNVs and SVs)
  log_info("[filtServer][filter_dataset] Applying VEP annotation filter")
  filter_expression <- add_filter_condition(filter_expression, text_filter("CONSEQUENCE", vep_consequences[consequence %in% filters$annotation_filter, term]))

  spliceai_override_condition <- NULL
  clinvar_override_condition <- NULL

  if (is_snv) {
    log_info("[filtServer][filter_dataset] Applying SNV-specific filters")
    # SNV-specific filters
    if (!is.null(filters$sift_filter) && nzchar(filters$sift_filter)) {
      filter_expression <- add_filter_condition(filter_expression, text_filter("SIFT", filters$sift_filter))
    }
    if (!is.null(filters$polyphen_filter) && nzchar(filters$polyphen_filter)) {
      filter_expression <- add_filter_condition(filter_expression, text_filter("PolyPhen", gsub(" ", "_", filters$polyphen_filter)))
    }
    # REVEL threshold (allow NA to pass)
    if (!is.null(filters$revel_value) && filters$revel_value > 0) {
      filter_expression <- add_filter_condition(filter_expression, sprintf("(is.na(REVEL) | REVEL >= %f)", filters$revel_value))
    }
    # ClinVar filter and override
    if (!is.null(filters$clinvar_filter) && length(filters$clinvar_filter) > 0) {
      log_info("[filtServer][filter_dataset] Applying ClinVar filter")
      # Main ClinVar condition
      clinvar_filter_updated <- gsub("VUS", "uncertain", filters$clinvar_filter)
      clinvar_pattern <- paste(sapply(clinvar_filter_updated, function(x) paste0("\\\\b", x, "\\\\b")), collapse = "|")

      # Add condition for "Not available" (i.e., NA values in CLINVAR)
      if ("Not available" %in% filters$clinvar_filter) {
        clinvar_condition <- sprintf("(%s | is.na(CLINVAR))",
                                     paste0("grepl('", clinvar_pattern, "', CLINVAR, ignore.case = TRUE)"))
      } else {
        clinvar_condition <- sprintf("grepl('%s', CLINVAR, ignore.case = TRUE)", clinvar_pattern)
      }
      #clinvar_condition <- sprintf("grepl('%s', CLINVAR, ignore.case = TRUE)", clinvar_pattern)
      filter_expression <- paste(filter_expression, clinvar_condition, sep = " & ")

      # ClinVar override for specific terms
      override_patterns <- c()
      if ("Pathogenic" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\bPathogenic\\\\b")
      if ("Likely pathogenic" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\bLikely pathogenic\\\\b")
      if ("uncertain" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\buncertain\\\\b")

      if (length(override_patterns) > 0) {
        override_pattern <- paste(override_patterns, collapse = "|")
        clinvar_override_condition <- sprintf(
          "(grepl('%s', CLINVAR, ignore.case = TRUE) & (is.na(AF) | AF < 0.05))",
          override_pattern
        )
      }
    }

    # SpliceAI override
    if (!is.null(filters$spliceai_filter) && filters$spliceai_filter > 0) {
      log_info("[filtServer][filter_dataset] Applying SpliceAI override filter")
      spliceai_override_condition <- sprintf(
        "(Donor_Loss > %f | Donor_Gain > %f | Acceptor_Loss > %f | Acceptor_Gain > %f)",
        filters$spliceai_filter,
        filters$spliceai_filter,
        filters$spliceai_filter,
        filters$spliceai_filter
      )
    }

    # Ensure SpliceAI and ClinVar overrides are applied
    override_conditions <- list()

    if (filters$inheritance_filter == "X-Linked Recessive") {
      if (!is.null(spliceai_override_condition)) spliceai_override_condition <- sprintf("(%s & CHROM == 'chrX')", spliceai_override_condition)
      if (!is.null(clinvar_override_condition)) clinvar_override_condition <- sprintf("(%s & CHROM == 'chrX')", clinvar_override_condition)
    }

    # override_conditions <- c(spliceai_override_condition, clinvar_override_condition)
    # override_conditions <- Filter(Negate(is.null), override_conditions)  # Remove NULLs
    # 
    # if (length(override_conditions) > 0) {
    #   filter_expression <- paste(filter_expression, paste(override_conditions, collapse = " | "), sep = " | ")
    # }

  } else {
    # SV-specific filters
    log_info("[filtServer][filter_dataset] Filtering SVs based on type and length")
    sv_type_map <- list("Insertion" = "INS", "Deletion" = "DEL", "Duplication" = "DUP", "Inversion" = "INV", "Translocation" = "TRA|BND")
    sv_types <- unlist(sv_type_map[filters$sv_features])
    if (!is.null(sv_types) && length(sv_types) > 0) {
      filter_expression <- add_filter_condition(filter_expression, sprintf("VAR_TYPE %%in%% c('%s')", paste(sv_types, collapse = "', '")))
    }
    if (!is.null(filters$min_svlen) && filters$min_svlen > 0) {
      filter_expression <- add_filter_condition(filter_expression, sprintf("VAR_LENGTH >= %d", filters$min_svlen))
    }
    if (!is.null(filters$max_svlen) && filters$max_svlen > 0) {
      filter_expression <- add_filter_condition(filter_expression, sprintf("VAR_LENGTH <= %d", filters$max_svlen))
    }
  }

  all_conditions <- list(
    if (!is.null(filters$inheritance_filter) && filters$inheritance_filter == "X-Linked Recessive") {
      sprintf("(%s & CHROM == 'chrX')", filter_expression)
    } else {
      sprintf("(%s)", filter_expression)
    },
    # Add SpliceAI override with conditional CHROM filter
    if (!is.null(spliceai_override_condition)) {
      if (!is.null(filters$inheritance_filter) && filters$inheritance_filter == "X-Linked Recessive") {
        sprintf("(%s & %s & CHROM == 'chrX')", global_filters_expression, spliceai_override_condition)
      } else {
        sprintf("(%s & %s)", global_filters_expression, spliceai_override_condition)
      }
    } else {
      NULL
    },
    # Add ClinVar override with conditional CHROM filter
    if (!is.null(clinvar_override_condition)) {
      if (!is.null(filters$inheritance_filter) && filters$inheritance_filter == "X-Linked Recessive") {
        sprintf("(%s & %s & CHROM == 'chrX')", global_filters_expression, clinvar_override_condition)
      } else {
        sprintf("(%s & %s)", global_filters_expression, clinvar_override_condition)
      }
    } else {
      NULL
    }
  )

  combined_expression <- paste(Filter(Negate(is.null), all_conditions), collapse = " | ")

  # Special case for X-Linked Recessive
  if (filters$inheritance_filter == "X-Linked Recessive") {
    combined_expression <- sprintf("(%s & CHROM == 'chrX')",  combined_expression)
  }

  # Apply filtering
  #print(combined_expression)
  log_info(sprintf("[filtServer][filter_dataset] Filter expression: %s", combined_expression))
  filtered_data <- data[eval(parse(text = combined_expression))]

  if (!is.character(filtered_data$GENE_SYMBOL)) {
    stop("Error: filtered_data$GENE_SYMBOL must be of type character, but is ", class(filtered_data$GENE_SYMBOL))
  }

  # Add panel app and HPO information
  # if (length(filters$panelapp_filter) > 0) {
  #   setkey(filtered_data, GENE_SYMBOL)
  #   setkey(genes, GENE_SYMBOL)
  #   filtered_data <- merge(filtered_data, genes, by = "GENE_SYMBOL", all.x = TRUE)
  # } else {
  #   filtered_data[, PANEL_APP := NA]
  #   filtered_data[, INHERITANCE := NA]
  # }
  if (length(filters$panelapp_filter) > 0 || length(filters$custom_genes) > 0) {
    filtered_data[genes, on = "GENE_SYMBOL", `:=`(
      PANEL_APP   = i.PANEL_APP,
      INHERITANCE = i.INHERITANCE
    )]
  }

  # if (!is.null(filters$hpo_terms_list) && length(filters$hpo_terms_list) > 0) {
  #   split_hpo_terms_list <- unlist(strsplit(filters$hpo_terms_list, "; "))
  #   hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list & gene_symbol %in% filtered_data$GENE_SYMBOL, .(HPO_ID = hpo_id, GENE_SYMBOL = gene_symbol)]
  #   hpo_terms_summary <- hpo_terms_data[, .(HPO_ID = paste(HPO_ID, collapse = ";"), HPO_COUNT = .N), by = GENE_SYMBOL]
  #   setkey(hpo_terms_summary, GENE_SYMBOL)
  #   filtered_data <- merge(filtered_data, hpo_terms_summary, by = "GENE_SYMBOL", all.x = TRUE)
  #   filtered_data[is.na(HPO_COUNT), HPO_COUNT := 0]
  # } else {
  #   filtered_data[, HPO_ID := NA]
  #   filtered_data[, HPO_COUNT := 0]
  # }
  if (!is.null(filters$hpo_terms_list) && length(filters$hpo_terms_list) > 0) {
    split_hpo_terms_list <- unlist(strsplit(filters$hpo_terms_list, "; "))
    hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list & gene_symbol %in% filtered_data$GENE_SYMBOL, .(HPO_ID = hpo_id, GENE_SYMBOL = gene_symbol)]
    hpo_terms_summary <- hpo_terms_data[, .(HPO_ID = paste(HPO_ID, collapse=";"), HPO_COUNT = .N), by = GENE_SYMBOL]
    filtered_data[hpo_terms_summary, on = "GENE_SYMBOL", `:=`(HPO_ID = i.HPO_ID, HPO_COUNT = i.HPO_COUNT)][is.na(HPO_COUNT), HPO_COUNT := 0]
  }

  # filtered_data[, clinvar_override := FALSE]
  # filtered_data[, spliceai_override := FALSE]
  # if (!is.null(clinvar_override_condition)) {
  #   filtered_data[eval(parse(text = clinvar_override_condition)),
  #                 clinvar_override := TRUE]
  # }
  # if (!is.null(spliceai_override_condition)) {
  #   filtered_data[eval(parse(text = spliceai_override_condition)),
  #                 spliceai_override := TRUE]
  # }
  # Initialize existing columns
  filtered_data[, `:=`(clinvar_override = FALSE, spliceai_override = FALSE)]

  # Apply ClinVar override if condition exists
  if (!is.null(clinvar_override_condition)) filtered_data[eval(parse(text = clinvar_override_condition)), clinvar_override := TRUE]

  # Apply SpliceAI override if condition exists
  if (!is.null(spliceai_override_condition)) filtered_data[eval(parse(text = spliceai_override_condition)), spliceai_override := TRUE]

  log_info(sprintf("[filtServer][filter_dataset] Filtering complete: %d variants retained", nrow(filtered_data)))
  return(filtered_data)
}

# Function to apply compound heterozygous filtering to a filtered dataset
apply_compound_het <- function(all_filtered_data, pedigree, inheritance_type) {
  if (inheritance_type != "Compound Heterozygous" || nrow(all_filtered_data) == 0) return(all_filtered_data)
  is_trio <- sum(pedigree$kinship %in% c("mother", "father")) == 2
  if (!is_trio) {
    comp_hets_1 <- all_filtered_data[alt_allele_count_1 == 1 & GT_1 == "1|0", .(VAR_COUNT_1 = .N), by = GENE_SYMBOL]
    comp_hets_2 <- all_filtered_data[alt_allele_count_1 == 1 & GT_1 == "0|1", .(VAR_COUNT_2 = .N), by = GENE_SYMBOL]
    comp_hets <- merge(comp_hets_1, comp_hets_2, by = "GENE_SYMBOL", all = TRUE)[VAR_COUNT_1 > 0 & VAR_COUNT_2 > 0]
    all_filtered_data <- all_filtered_data[GENE_SYMBOL %in% comp_hets$GENE_SYMBOL]
  } else {
    comp_hets <- all_filtered_data[
      alt_allele_count_1 == 1 &  # Proband is heterozygous
        ((GT_2 == "1|0" & GT_3 == "0|1") | (GT_2 == "0|1" & GT_3 == "1|0")),  # Opposite inheritance from parents
      .(VAR_COUNT = .N), by = GENE_SYMBOL]
    # Keep genes where at least 2 variants exist and are inherited from different parents
    valid_genes <- comp_hets[VAR_COUNT > 1, GENE_SYMBOL]
    all_filtered_data <- all_filtered_data[GENE_SYMBOL %in% valid_genes]
  }
  return(all_filtered_data)
}

# export this function in roxygen
#' Variant filtering function for SNVs and SVs
#' @param data Data table of variants (SNVs or SVs)
#' @param filters List of filter parameters
#' @param pedigree Data table representing the pedigree information
#' @param allele_tab Data table of allele counts
#' @param panel_app_genes Data table of PanelApp genes
#' @param vep_consequences Data table of VEP consequences
#' @param phenotype_data Data table of phenotype information
#' @param is_snv Logical indicating if the data is SNVs (TRUE) or SVs (FALSE)
#' @return Filtered data table of variants after applying filters and compound heterozygous filtering
#' @export
puzzlecore_variant_filter <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE) {
    variant_type <- if (is_snv) "SNV" else "SV"
    log_info(sprintf("[filtServer][variant_filter] Starting variant filtering for %s", variant_type))
    filter_time <- system.time({
      filtered_data <- filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv)
    })
    log_info(sprintf("nrow(filtered_data): %d", nrow(filtered_data)))
    log_info(sprintf("Total filter_dataset execution time: %s", format_time(filter_time)))
    filtered_data_comphet <- apply_compound_het(filtered_data, pedigree, filters$inheritance_filter)
    log_info(sprintf("nrow(filtered_data_comphet): %d", nrow(filtered_data_comphet)))
    return(filtered_data_comphet)
}