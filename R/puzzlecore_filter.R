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
    # print(pedigree)
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
  if (!is.null(filters$af_value) && filters$af_value < 1) 
    #conditions <- c(conditions, sprintf("(is.na(AF) | AF <= %f)", filters$af_value))

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

# Helper: Apply PanelApp inheritance + allele count gating
apply_inheritance_panelapp_gene <- function(filters, genes, pedigree) {
  # Bare-minimum logic for Dominant/De Novo
  if (!isTRUE(filters$inheritance_panelapp_gene)) return(NULL)
  if (is.null(filters$inheritance_filter)) return(NULL)
  
  if (filters$inheritance_filter == "Dominant/De Novo") {
    # using GENE_SYMBOL because INHERITANCE is not yet merged into data at this point
    dom_genes <- genes[grepl("MONOALLELIC|BOTH", INHERITANCE, ignore.case = TRUE), GENE_SYMBOL]
    if (length(dom_genes) == 0) return(NULL)
    return(sprintf("(GENE_SYMBOL %%in%% c('%s') & alt_allele_count_1 >= 1)", paste(dom_genes, collapse = "','")))
  }
  
  if (filters$inheritance_filter == "Homozygous Recessive") {
    rec_genes <- genes[grepl("BIALLELIC|BOTH", INHERITANCE, ignore.case = TRUE), GENE_SYMBOL]
    if (length(rec_genes) == 0) return(NULL)
    return(sprintf("(GENE_SYMBOL %%in%% c('%s') & alt_allele_count_1 == 2)", paste(rec_genes, collapse = "','")))
  }

  if (filters$inheritance_filter == "X-Linked Recessive") {
    x_genes <- genes[grepl("X[[:space:]-]?linked", INHERITANCE, ignore.case = TRUE), GENE_SYMBOL]
    if (length(x_genes) == 0) return(NULL)
    # check if proband is male or female
    proband <- pedigree[pedigree$kinship == "proband", ]
    if (proband$sex == "male") {
      return(sprintf("(GENE_SYMBOL %%in%% c('%s') & alt_allele_count_1 >= 1)", paste(x_genes, collapse = "','")))
    }
    return(sprintf("(GENE_SYMBOL %%in%% c('%s') & alt_allele_count_1 == 2)", paste(x_genes, collapse = "','")))
  }
  return(NULL)
}

# Generic filter function for SNVs and SVs
filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE, svlog_db = NULL) {
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

    # Apply PanelApp inheritance + proband allele-count gating (Dominant/De Novo)
    inheritance_pa_condition <- apply_inheritance_panelapp_gene(filters, genes, pedigree)
    filter_expression <- add_filter_condition(filter_expression, inheritance_pa_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, inheritance_pa_condition)
  }
  
  log_info("[filtServer][filter_dataset] Applying quality filters")
  filter_expression <- add_filter_condition(filter_expression, quality_filters(filters, data, pedigree))

  # Apply VEP Annotation filter (for both SNVs and SVs)
  log_info("[filtServer][filter_dataset] Applying VEP annotation filter")
  if ("Other" %in% filters$annotation_filter) {
    specific_list <- vep_consequences[consequence != "Other", term]
    # remove other explicitly selected filters (besides "Other")
    other_terms <- setdiff(filters$annotation_filter, "Other")
    if (length(other_terms) > 0) {
      mapped_other_terms <- vep_consequences[consequence %in% other_terms, term]
      specific_list <- setdiff(specific_list, mapped_other_terms)
    }
    if (length(specific_list) > 0) {
      negation_expr <- sprintf("!grepl('%s', VEP_CONSEQUENCE, ignore.case = TRUE)", paste(specific_list, collapse = "|"))
      filter_expression <- add_filter_condition(filter_expression, negation_expr)
    }
  } else {
    filter_expression <- add_filter_condition(
      filter_expression,
      text_filter("VEP_CONSEQUENCE", vep_consequences[consequence %in% filters$annotation_filter, term])
    )
  }

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

    override_threshold <- if (isTRUE(filters$use_af)) filters$af_value else 0.05

    # ClinVar filter and override
    if (!is.null(filters$clinvar_filter) && length(filters$clinvar_filter) > 0) {
      # Normalize input
      filters$clinvar_filter <- gsub(" ", "_", filters$clinvar_filter)
      log_info("[filtServer][filter_dataset] Applying ClinVar filter")

      # Mapping for special cases
      special_map <- list(
        "VUS" = "uncertain",
        "Conflicting" = "conflicting"
      )

      if ("Other" %in% filters$clinvar_filter) {
        # Exclude all explicit categories except those explicitly selected alongside "Other"
        explicit_terms <- c("Pathogenic", "Likely_pathogenic", "VUS", "Conflicting", "Benign", "Likely_benign", "Not_available")
        selected_explicit <- intersect(setdiff(filters$clinvar_filter, "Other"), explicit_terms)
        exclude_explicit <- setdiff(explicit_terms, selected_explicit)

        if (length(exclude_explicit) > 0) {
          word_boundary_terms <- c()
          substring_terms <- c()
          for (term in exclude_explicit) {
            if (term %in% names(special_map)) {
              substring_terms <- c(substring_terms, special_map[[term]])
            } else {
              word_boundary_terms <- c(word_boundary_terms, paste0("\\\\b", term, "\\\\b"))
            }
          }
          clinvar_pattern <- paste(c(word_boundary_terms, substring_terms), collapse = "|")
          negation_expr <- sprintf("!grepl('%s', CLINVAR, ignore.case = TRUE)", clinvar_pattern)
          filter_expression <- add_filter_condition(filter_expression, negation_expr)
        }
      } else {
        word_boundary_terms <- c()
        substring_terms <- c()
        for (term in filters$clinvar_filter) {
          if (term %in% names(special_map)) {
            # special case → substring match (no word boundaries)
            substring_terms <- c(substring_terms, special_map[[term]])
          } else {
            # normal case → strict word-boundary match
            word_boundary_terms <- c(word_boundary_terms, paste0("\\\\b", term, "\\\\b"))
          }
        }
        # Combine all patterns into one regex
        clinvar_pattern <- paste(c(word_boundary_terms, substring_terms), collapse = "|")
        clinvar_condition <- sprintf("grepl('%s', CLINVAR, ignore.case = TRUE)", clinvar_pattern)
        filter_expression <- paste(filter_expression, clinvar_condition, sep = " & ")
      }

      # ClinVar override for specific terms
      override_patterns <- c()
      if ("Pathogenic" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\bPathogenic\\\\b")
      if ("Likely_pathogenic" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\bLikely_pathogenic\\\\b")
      if ("VUS" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "uncertain")

      if (length(override_patterns) > 0) {
        override_pattern <- paste(override_patterns, collapse = "|")
        clinvar_override_condition <- sprintf("(grepl('%s', CLINVAR, ignore.case = TRUE) & (is.na(AF) | AF < %f))", override_pattern, override_threshold)
      }
    }

    # SpliceAI override
    if (!is.null(filters$spliceai_filter) && filters$spliceai_filter > 0) {
      log_info("[filtServer][filter_dataset] Applying SpliceAI override filter")
      spliceai_override_condition <- sprintf(
        "(Donor_Loss > %f | Donor_Gain > %f | Acceptor_Loss > %f | Acceptor_Gain > %f) & (is.na(AF) | AF < %f)",
        filters$spliceai_filter,
        filters$spliceai_filter,
        filters$spliceai_filter,
        filters$spliceai_filter,
        override_threshold
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
    
    # --------------------------------------------------------------------------
    # SV-specific filters
    # --------------------------------------------------------------------------
    
    # ---- 1) Basic SV properties: type + length ----
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
    
    # ---- 2) SV classification labels ----
    if (!is.null(filters$classification_filter) &&
        length(filters$classification_filter) > 0) {
      
      # exact match on FINAL_CLASSIFICATION
      # (assumes TSV values == FINAL_CLASSIFICATION values)
      cls_vals <- paste(sprintf("'%s'", filters$classification_filter),
                        collapse = ", ")
      class_expr <- sprintf(
        "FINAL_CLASSIFICATION %%in%% c(%s)",
        cls_vals
      )
      filter_expression <- add_filter_condition(filter_expression, class_expr)
      log_info("[filtServer][filter_dataset] Applying FINAL_CLASSIFICATION filter")
    }
    
    # --------------------------------------------------------------------------
    # Genomic context filters
    # --------------------------------------------------------------------------
    
    # 1) Max distance to splice site (bp) — intronic
    if (!is.null(filters$intronic_splice_max_dist)) {
      expr <- sprintf(
        "(is.na(INTRON_MIN_DIST) | INTRON_MIN_DIST <= %d)",
        as.integer(filters$intronic_splice_max_dist)
      )
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info("[filtServer][filter_dataset] Applying intronic splice distance filter")
    }
    
    # 2) Min ratio SV length / intron length — intronic
    #    Treat non-intronic (INTRON_LENGTH NA or <= 0) as passing.
    if (!is.null(filters$intronic_min_len_intron_ratio)) {
      expr <- sprintf(
        "(is.na(INTRON_LENGTH) | INTRON_LENGTH <= 0 | VAR_LENGTH / INTRON_LENGTH >= %f)",
        filters$intronic_min_len_intron_ratio
      )
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info("[filtServer][filter_dataset] Applying intronic length/intron ratio filter")
    }
    
    # 3) Max distance to nearest TAD boundary (bp)
    #    Use min(upstream, downstream); if both NA, pass.
    if (!is.null(filters$tad_max_dist)) {
      expr <- sprintf(
        "((is.na(INTERGENIC_NEAREST_UPSTREAM_TAD_DIST) & is.na(INTERGENIC_NEAREST_DOWNSTREAM_TAD_DIST)) | " %+%
          " pmin(INTERGENIC_NEAREST_UPSTREAM_TAD_DIST, INTERGENIC_NEAREST_DOWNSTREAM_TAD_DIST, na.rm = TRUE) <= %d)",
        as.integer(filters$tad_max_dist)
      )
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info("[filtServer][filter_dataset] Applying TAD boundary distance filter")
    }
    
    # 4) Max distance to nearest enhancer (bp)
    #    Use min(upstream, downstream); if both NA, pass.
    if (!is.null(filters$enhancer_max_dist)) {
      expr <- sprintf(
        "((is.na(INTERGENIC_NEAREST_UPSTREAM_ENH_DIST) & is.na(INTERGENIC_NEAREST_DOWNSTREAM_ENH_DIST)) | " %+%
          " pmin(INTERGENIC_NEAREST_UPSTREAM_ENH_DIST, INTERGENIC_NEAREST_DOWNSTREAM_ENH_DIST, na.rm = TRUE) <= %d)",
        as.integer(filters$enhancer_max_dist)
      )
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info("[filtServer][filter_dataset] Applying enhancer distance filter")
    }
    
    # 5) Intra / inter TAD boundary
    #    If both FALSE → no filter.
    #    If only intra TRUE → type == 'intra'
    #    If only inter TRUE → type == 'inter'
    #    If both TRUE → restrict to intra or inter (exclude NA/other).
    if (isTRUE(filters$intra_tad_only) || isTRUE(filters$inter_tad_only)) {
      wanted <- character()
      if (isTRUE(filters$intra_tad_only)) wanted <- c(wanted, "intra")
      if (isTRUE(filters$inter_tad_only)) wanted <- c(wanted, "inter")
      
      vals <- paste(sprintf("'%s'", wanted), collapse = ", ")
      expr <- sprintf(
        "INTERGENIC_BOUNDARY_SPAN_TYPE %%in%% c(%s)",
        vals
      )
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info(sprintf(
        "[filtServer][filter_dataset] Applying TAD span filter: %s",
        paste(wanted, collapse = ",")
      ))
    }
    
    # --------------------------------------------------------------------------
    # SVlog Consequence
    # --------------------------------------------------------------------------
    
    if (!is.null(filters$svlog_annotation_filter) &&
        length(filters$svlog_annotation_filter) > 0) {
      
      pat <- paste(filters$svlog_annotation_filter, collapse = "|")
      
      expr <- sprintf(
        "(is.na(SVLOG_CONSEQUENCE) | grepl('%s', SVLOG_CONSEQUENCE, ignore.case = TRUE))",
        pat
      )
      
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info("[filtServer][filter_dataset] Applying SVlog annotation filter")
    }
    
    # --------------------------------------------------------------------------
    # Tier prioritisation: Keeping / Filtering out
    # --------------------------------------------------------------------------
    K <- filters$keeping_tiers
    F <- filters$filtering_out_tiers
    
    # Normalise: drop empty strings if they slip through
    if (!is.null(K)) K <- K[nzchar(K)] else K <- character(0)
    if (!is.null(F)) F <- F[nzchar(F)] else F <- character(0)
    
    if (length(K) > 0 || length(F) > 0) {
      
      # Helper: build regex that matches a tier as a whole token in
      # a comma-separated string (e.g. "1" matches "1,2" but not "10")
      mk_tier_pattern <- function(x) {
        paste(sprintf("(^|,)%s(,|$)", x), collapse = "|")
      }
      
      # Part 1: KEEPING has at least one of the selected Keeping tiers
      if (length(K) > 0) {
        pat_keep <- mk_tier_pattern(K)
        keep_part <- sprintf(
          "(!is.na(KEEPING) & grepl('%s', KEEPING))",
          pat_keep
        )
      } else {
        keep_part <- "TRUE"
      }
      
      # Part 2: FILTERING_OUT has none of the selected Filtering out tiers
      if (length(F) > 0) {
        pat_out <- mk_tier_pattern(F)
        out_part <- sprintf(
          "(is.na(FILTERING_OUT) | !grepl('%s', FILTERING_OUT))",
          pat_out
        )
      } else {
        out_part <- "TRUE"
      }
      
      tier_expr <- sprintf("(%s & %s)", keep_part, out_part)
      
      filter_expression <- add_filter_condition(filter_expression, tier_expr)
      log_info(sprintf(
        "[filtServer][filter_dataset] Applying tier filter (Keeping=%s, Filtering out=%s)",
        paste(K, collapse = ","),
        paste(F, collapse = ",")
      ))
    }
    
    # --------------------------------------------------------------------------
    # Advanced: Keeping / Filtering out predicate selection
    # --------------------------------------------------------------------------
    
    K <- filters$svlog_advanced_keeping
    F <- filters$svlog_advanced_filtering_out
    
    if (!is.null(K)) K <- K[nzchar(K)] else K <- character(0)
    if (!is.null(F)) F <- F[nzchar(F)] else F <- character(0)
    
    if (length(K) > 0 || length(F) > 0) {
      
      mk_pat <- function(x) paste(sprintf("(^|,)%s(,|$)", x), collapse = "|")
      
      # KEEPING_EVIDENCE has at least one selected Keeping predicate
      keep_part <- if (length(K) > 0) {
        sprintf("(!is.na(KEEPING_EVIDENCE) & grepl('%s', KEEPING_EVIDENCE))", mk_pat(K))
      } else "TRUE"
      
      # FILTERING_OUT_EVIDENCE has none of the selected Filtering out predicates
      out_part <- if (length(F) > 0) {
        sprintf("(is.na(FILTERING_OUT_EVIDENCE) | !grepl('%s', FILTERING_OUT_EVIDENCE))", mk_pat(F))
      } else "TRUE"
      
      expr <- sprintf("(%s & %s)", keep_part, out_part)
      
      filter_expression <- add_filter_condition(filter_expression, expr)
      log_info(sprintf(
        "[filtServer][filter_dataset] Applying SVlog advanced evidence filter (Keeping=%s, Filtering out=%s)",
        paste(K, collapse = ","), paste(F, collapse = ",")
      ))
    }
    
    # --------------------------------------------------------------------------
    # SVlog database filters (this mutates `data`, so keep it last)
    # --------------------------------------------------------------------------
    if (!is.null(svlog_db)) {
      
      log_info("[filtServer][filter_dataset] Applying SVlog database filters (gnomAD, ClinVar, ONT1000G, Internal Cohort)")
      
      # keep a copy of original data before SVlog merge
      data_before_svlog <- data
      
      # IDs that have *no* entry in svlog_db at all
      if (!"ID" %in% names(data)) {
        stop("SV data does not contain 'ID' column, required for SVlog merge.")
      }
      if (!"ID" %in% names(svlog_db)) {
        stop("svlog_db does not contain 'ID' column, required for SVlog merge.")
      }
      
      ids_no_svlog <- setdiff(
        unique(data$ID),
        unique(svlog_db$ID)
      )
      
      svlog_summary <- filter_svlog_to_wide(
        svlog_db  = svlog_db,
        sv_filters = filters
      )
      
      # inner join: only IDs with passing SVlog evidence survive here
      data <- merge(
        data,
        svlog_summary,
        by = "ID"
      )
      log_info(sprintf("[filtServer][filter_dataset] SVlog filter: %d variants with passing SVlog database evidence", nrow(data)))
      # rescue variants that had no SVlog record at all
      if (length(ids_no_svlog) > 0) {
        rescued <- data_before_svlog[ID %in% ids_no_svlog]
        log_info(sprintf("[filtServer][filter_dataset] SVlog rescue: %d variants had no SVlog record and were kept unfiltered", nrow(rescued)))
        # bind them back; they will have NA in the SVlog columns
        data <- data.table::rbindlist(
          list(data, rescued),
          use.names = TRUE,
          fill = TRUE
        )
      }
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
    hpo_terms_data <- unique(hpo_terms_data, by = c("GENE_SYMBOL", "HPO_ID"))
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
apply_compound_het <- function(all_filtered_data, pedigree, filters) {
  inheritance_type <- filters$inheritance_filter
  if (inheritance_type != "Compound Heterozygous" || nrow(all_filtered_data) == 0) return(all_filtered_data)

  # If PanelApp inheritance gating is ON, restrict to Biallelic genes and proband hets
  inheritance_panelapp_gene <- filters$inheritance_panelapp_gene
  if (isTRUE(inheritance_panelapp_gene)) {
    all_filtered_data <- all_filtered_data[grepl("Biallelic", INHERITANCE, ignore.case = TRUE) & alt_allele_count_1 == 1]
  }

  is_trio <- sum(pedigree$kinship %in% c("mother", "father")) == 2
  if (!is_trio) {
    comp_hets_1 <- all_filtered_data[alt_allele_count_1 == 1 & GT_1 == "1|0", .(VAR_COUNT_1 = .N), by = GENE_SYMBOL]
    comp_hets_2 <- all_filtered_data[alt_allele_count_1 == 1 & GT_1 == "0|1", .(VAR_COUNT_2 = .N), by = GENE_SYMBOL]
    comp_hets <- merge(comp_hets_1, comp_hets_2, by = "GENE_SYMBOL", all = TRUE)[VAR_COUNT_1 > 0 & VAR_COUNT_2 > 0]
    all_filtered_data <- all_filtered_data[GENE_SYMBOL %in% comp_hets$GENE_SYMBOL]
  } else {
    mom_code <- pedigree$code[pedigree$kinship == "mother"]
    dad_code <- pedigree$code[pedigree$kinship == "father"]
    mom_gt_col <- paste0("GT_", mom_code)
    dad_gt_col <- paste0("GT_", dad_code)
    mom <- all_filtered_data[[mom_gt_col]]
    dad <- all_filtered_data[[dad_gt_col]]
    het <- all_filtered_data[["alt_allele_count_1"]] == 1 # Proband is heterozygous
    cond1 <- het & mom == "1|0" & dad == "0|1" # Inherited from different parents
    cond2 <- het & mom == "0|1" & dad == "1|0" # Inherited from different parents
    comp_hets <- all_filtered_data[cond1 | cond2, .(VAR_COUNT = .N), by = GENE_SYMBOL]
    # Keep genes where at least 2 variants exist and are inherited from different parents
    valid_genes <- comp_hets[VAR_COUNT > 1, GENE_SYMBOL]
    all_filtered_data <- all_filtered_data[GENE_SYMBOL %in% valid_genes]
  }
  return(all_filtered_data)
}

# Function to filter and summarise SVlog database entries per SV
filter_svlog_to_wide <- function(svlog_db, sv_filters) {
  if (is.null(svlog_db) || !nrow(svlog_db)) {
    return(data.table::data.table())
  }
  
  dt <- data.table::as.data.table(data.table::copy(svlog_db))
  
  # ---------- helpers ----------
  safe_max_num <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    max(x)
  }
  safe_max_int <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_integer_)
    as.integer(max(x))
  }
  
  # carriers = HET + HOM (NA-aware)
  dt[, carriers := {
    h <- ifelse(is.na(HET), 0L, as.integer(HET))
    m <- ifelse(is.na(HOM), 0L, as.integer(HOM))
    out <- h + m
    both_na <- is.na(HET) & is.na(HOM)
    out[both_na] <- NA_integer_
    out
  }]
  
  # ---------- 1) similarity / matching filters (row-level) ----------
  keep <- rep(TRUE, nrow(dt))
  
  # Non-INS: reciprocal overlap
  if (!is.null(sv_filters$svlog_min_recip_overlap)) {
    thr <- sv_filters$svlog_min_recip_overlap
    has_ov <- !is.na(dt$OVL_Q) & !is.na(dt$OVL_T)
    keep[has_ov] <- keep[has_ov] & (pmin(dt$OVL_Q[has_ov], dt$OVL_T[has_ov]) >= thr)
    # rows with no overlap info (INS) are unaffected
  }
  
  # INS: breakpoint distance
  if (!is.null(sv_filters$svlog_max_break_distance)) {
    thr <- sv_filters$svlog_max_break_distance
    has_dist <- !is.na(dt$DIST)
    keep[has_dist] <- keep[has_dist] & (dt$DIST[has_dist] <= thr)
    # non-INS (DIST NA) unaffected
  }
  
  # INS: |Δlen|
  if (!is.null(sv_filters$svlog_max_abs_dlen)) {
    thr <- sv_filters$svlog_max_abs_dlen
    has_dlen <- !is.na(dt$DLEN)
    keep[has_dlen] <- keep[has_dlen] & (abs(dt$DLEN[has_dlen]) <= thr)
    # non-INS (DLEN NA) unaffected
  }
  
  dt <- dt[keep]
  if (!nrow(dt)) return(data.table::data.table())
  
  # ---------- 2) per-svlog_id DB summaries & pass/fail ----------
  agg <- unique(dt[, .(svlog_id)])
  
  # gnomAD
  gnom <- dt[SRC == "gnomAD",
             .(gnomAD_AF_max = safe_max_num(AF)),
             by = svlog_id]
  agg <- merge(agg, gnom, by = "svlog_id", all.x = TRUE)
  has_gnomAD <- !is.na(agg$gnomAD_AF_max)
  if (!is.null(sv_filters$svlog_gnomad_af)) {
    thr <- sv_filters$svlog_gnomad_af
    agg[, gnomAD_pass := is.na(gnomAD_AF_max) | gnomAD_AF_max <= thr]
  } else {
    agg[, gnomAD_pass := NA]
  }
  
  # ONT 1000G
  kg <- dt[SRC %in% c("ONT1000G", "1000g"),
           .(ont1000g_max_carriers = safe_max_int(carriers)),
           by = svlog_id]
  agg <- merge(agg, kg, by = "svlog_id", all.x = TRUE)
  has_kg <- !is.na(agg$ont1000g_max_carriers)
  if (!is.null(sv_filters$svlog_1000g_max_carriers)) {
    thr <- as.integer(sv_filters$svlog_1000g_max_carriers)
    agg[, ont1000g_pass :=
          is.na(ont1000g_max_carriers) | ont1000g_max_carriers <= thr]
  } else {
    agg[, ont1000g_pass := NA]
  }
  
  # Internal cohort
  internal <- dt[SRC %in% c("InternalCohort", "internal"),
                 .(
                   internal_max_carriers = safe_max_int(carriers),
                   internal_max_families = safe_max_int(NUM_FAMS)
                 ),
                 by = svlog_id]
  agg <- merge(agg, internal, by = "svlog_id", all.x = TRUE)
  has_internal <- !is.na(agg$internal_max_carriers) | !is.na(agg$internal_max_families)
  
  if (!is.null(sv_filters$svlog_internal_max_carriers)) {
    thr <- as.integer(sv_filters$svlog_internal_max_carriers)
    carriers_ok <- is.na(agg$internal_max_carriers) | agg$internal_max_carriers <= thr
  } else {
    carriers_ok <- rep(TRUE, nrow(agg))
  }
  if (!is.null(sv_filters$svlog_internal_max_families)) {
    thr <- as.integer(sv_filters$svlog_internal_max_families)
    fam_ok <- is.na(agg$internal_max_families) | agg$internal_max_families <= thr
  } else {
    fam_ok <- rep(TRUE, nrow(agg))
  }
  agg[, internal_pass := carriers_ok & fam_ok]
  
  # ClinVar
  clin <- dt[SRC == "ClinVar" & !is.na(CLNSIG) & nzchar(CLNSIG),
             .(clinvar_labels = paste(unique(CLNSIG), collapse = ";")),
             by = svlog_id]
  agg <- merge(agg, clin, by = "svlog_id", all.x = TRUE)
  has_clinvar <- !is.na(agg$clinvar_labels) & nzchar(agg$clinvar_labels)
  agg[, clinvar_pass := TRUE]
  
  if (!is.null(sv_filters$clinvar_filter) &&
      length(sv_filters$clinvar_filter) > 0) {
    
    cf <- sv_filters$clinvar_filter
    cf <- gsub(" ", "_", cf)
    
    special_map <- list(
      "VUS"        = "uncertain",
      "Conflicting" = "conflicting"
    )
    
    word_boundary_terms <- character()
    substring_terms     <- character()
    for (term in cf) {
      if (term %in% names(special_map)) {
        substring_terms <- c(substring_terms, special_map[[term]])
      } else {
        word_boundary_terms <- c(word_boundary_terms,
                                 paste0("\\b", term, "\\b"))
      }
    }
    clinvar_pattern <- paste(c(word_boundary_terms, substring_terms),
                             collapse = "|")
    want_na <- "Not available" %in% sv_filters$clinvar_filter
    
    agg[, clinvar_pass := {
      lab <- clinvar_labels
      has_lab <- !is.na(lab) & nzchar(lab)
      hit <- rep(FALSE, .N)
      if (any(has_lab)) {
        hit[has_lab] <- grepl(clinvar_pattern, lab[has_lab], ignore.case = TRUE)
      }
      if (want_na) {
        hit | !has_lab
      } else {
        hit
      }
    }]
  }
  
  # which DBs actually contribute evidence per svlog_id
  used_gnomAD   <- has_gnomAD   & !is.na(agg$gnomAD_pass)
  used_kg       <- has_kg       & !is.na(agg$ont1000g_pass)
  used_internal <- has_internal
  used_clinvar  <- has_clinvar   # only constrains if filter actually applied above
  
  # agg[, pass_svlog :=
  #       (used_gnomAD   & gnomAD_pass) |
  #       (used_kg       & ont1000g_pass) |
  #       (used_internal & internal_pass) |
  #       (used_clinvar  & clinvar_pass)]
  
  agg[, pass_svlog :=
        (used_gnomAD   & gnomAD_pass) |
        (used_kg & ont1000g_pass & used_internal & internal_pass) |
        (used_clinvar  & clinvar_pass)]
  
  # No evidence from any DB → do not filter on SVlog
  no_evidence <- !(used_gnomAD | used_kg | used_internal | used_clinvar)
  agg[no_evidence, pass_svlog := TRUE]
  
  # ---------- 3) keep only passing svlog_id ----------
  keep_ids <- agg[pass_svlog == TRUE, svlog_id]
  dt_pass  <- dt[svlog_id %in% keep_ids]
  if (!nrow(dt_pass)) return(data.table::data.table())
  
  # ---------- 4) long → wide: id = (svlog_id, ID), columns = SRC, value = STRING ----------
  # collapse multiple STRINGs per (svlog_id, ID, SRC)
  collapsed <- dt_pass[
    ,
    .(STRING = paste(unique(STRING), collapse = ",")),
    by = .(ID, SRC)
  ]
  
  wide <- data.table::dcast(
    collapsed,
    ID ~ SRC,
    value.var = "STRING",
    fill = NA_character_
  )
  
  # ---------- 5) add per-ID summary columns ----------
  # map svlog_id -> ID for passing rows
  id_map <- unique(dt_pass[, .(svlog_id, ID)])
  id_agg <- merge(id_map, agg, by = "svlog_id", all.x = TRUE)
  
  id_summary <- id_agg[
    ,
    .(
      gnomAD_AF_max            = safe_max_num(gnomAD_AF_max),
      ONT1000G_carriers_max    = safe_max_int(ont1000g_max_carriers),
      Internal_carriers_max    = safe_max_int(internal_max_carriers),
      Internal_families_max    = safe_max_int(internal_max_families),
      ClinVar_CLASS = {
        labs <- clinvar_labels
        labs <- labs[!is.na(labs) & nzchar(labs)]
        if (!length(labs)) NA_character_ else paste(unique(labs), collapse = ",")
      }
    ),
    by = ID
  ]
  
  wide <- merge(
    wide,
    id_summary,
    by = "ID",
    all.x = TRUE
  )
  
  wide[]
}

# export this function in roxygen
#' Variant filtering function for SNVs and SVs
#' @param snv_data Data table of SNV variants
#' @param sv_data Data table of SV variants
#' @param snv_filters List of SNV filter parameters
#' @param sv_filters List of SV filter parameters
#' @param pedigree Data table representing the pedigree information
#' @param allele_tab Data table of allele counts
#' @param panel_app_genes Data table of PanelApp genes
#' @param vep_consequences Data table of VEP consequences
#' @param phenotype_data Data table of phenotype information
#' @return Filtered data table of variants after applying filters and compound heterozygous filtering
#' @export
puzzlecore_variant_filter <- function(snv_data, sv_data, snv_filters, sv_filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  log_info(sprintf("[filtServer][variant_filter] Starting variant filtering for %s", "SNV"))
  filter_time <- system.time({
    snv_filtered_data <- filter_dataset(snv_data, snv_filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, TRUE)
  })
  log_info(sprintf("nrow(filtered_data): %d", nrow(snv_filtered_data)))
  log_info(sprintf("Total filter_dataset execution time: %s", format_time(filter_time)))

  log_info(sprintf("[filtServer][variant_filter] Starting variant filtering for %s", "SV"))
  filter_time <- system.time({
    sv_filtered_data <- filter_dataset(sv_data, sv_filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, FALSE)
  })
  log_info(sprintf("nrow(filtered_data): %d", nrow(sv_filtered_data)))
  log_info(sprintf("Total filter_dataset execution time: %s", format_time(filter_time)))
  
  # Only combine and apply compound het if requested
  if (!is.null(snv_filters$inheritance_filter) && snv_filters$inheritance_filter == "Compound Heterozygous") {
    # Combine, allowing empty inputs
    filtered_data <- data.table::rbindlist(list(snv_filtered_data, sv_filtered_data), use.names = TRUE, fill = TRUE)
    log_info(sprintf("nrow(combined filtered_data): %d", nrow(filtered_data)))

    filtered_data_comphet <- apply_compound_het(filtered_data, pedigree, snv_filters)
    log_info(sprintf("nrow(filtered_data_comphet): %d", nrow(filtered_data_comphet)))

    # Separate back purely by CATEGORY (no ID or gene logic)
    snv_out <- filtered_data_comphet[CATEGORY == "SNV & Indel"]
    sv_out  <- filtered_data_comphet[CATEGORY == "SV"]
    return(list(snv = snv_out, sv = sv_out))
  }
  # Passthrough when not Compound Heterozygous (no combine)
  return(list(snv = snv_filtered_data, sv = sv_filtered_data))
}
