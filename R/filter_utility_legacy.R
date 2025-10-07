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
quality_filters <- function(filters, data) {
  conditions <- list()

  if (!is.null(filters$af_value) && filters$af_value < 1) 
    conditions <- c(conditions, sprintf("(is.na(AF) | AF <= %f)", filters$af_value))

  if (!is.null(filters$genotype_quality_value) && filters$genotype_quality_value > 0) 
    conditions <- c(conditions, sprintf("QUAL >= %f", filters$genotype_quality_value))

  if (!is.null(filters$allele_balance_value) && filters$allele_balance_value > 0) {
    vaf_vars <- grep("^VAF_", colnames(data), value = TRUE)
    conditions <- c(conditions, paste(sprintf("get('%s') >= %f", vaf_vars, filters$allele_balance_value), collapse = " & "))
  }

  return(paste(conditions, collapse = " & "))
}

# Helper: parse and normalize user-entered gene lists
parse_gene_list <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character(0))
  y <- unlist(strsplit(x, "[,;\\s]+"))
  y <- trimws(y)
  y <- y[nzchar(y)]
  # Normalize case (uppercase commonly used for gene symbols)
  unique(toupper(y))
}

# # Helper function: Handle panel app gene filtering
# panelapp_filter <- function(filters, panel_app_genes) {
#   if (length(filters$panelapp_filter) > 0) {
#     genes <- panel_app_genes[Level4 %in% filters$panelapp_filter,.(PANEL_APP=Level4,GENE_SYMBOL=Entity_Name,INHERITANCE=Model_Of_Inheritance)]
#     genes <- genes[,.(PANEL_APP=paste(PANEL_APP,collapse=";"),INHERITANCE=paste(INHERITANCE,collapse=";")),by=GENE_SYMBOL]
#     genes_search <- genes[,GENE_SYMBOL]
#     panel_app_condition <- "GENE_SYMBOL %in% genes_search"
#     return(list(panel_app_condition,genes_search,genes))
#   } else {
#     return(NULL)
#   }
# }

panelapp_filter <- function(filters, panel_app_genes) {
  # custom_genes is already parsed (character vector). May be NULL/empty.
  custom_vec <- filters$custom_genes
  if (is.null(custom_vec)) custom_vec <- character(0)

  have_filters <- (length(filters$panelapp_filter) > 0) || (length(custom_vec) > 0)
  if (!have_filters) return(NULL)

  pa_aug <- data.table::copy(panel_app_genes)

  # Append custom genes as Level4 = "CUSTOM" with empty strings for other fields
  if (length(custom_vec) > 0) {
    custom_dt <- data.table::data.table(
      Level4 = rep("CUSTOM", length(custom_vec)),
      Entity_Name = custom_vec,
      Model_Of_Inheritance = "",
      Sources = ""
    )
    pa_aug <- data.table::rbindlist(list(pa_aug, custom_dt), use.names = TRUE, fill = TRUE)
  }

  # Build filter levels (include CUSTOM only if custom genes provided)
  filt_levels <- unique(c(filters$panelapp_filter, if (length(custom_vec) > 0) "CUSTOM"))

  genes <- pa_aug[
    Level4 %in% filt_levels,
    .(
      PANEL_APP = Level4,
      GENE_SYMBOL = toupper(Entity_Name),
      INHERITANCE = ifelse(is.na(Model_Of_Inheritance), "", Model_Of_Inheritance)
    )
  ]

  # Collapse values ignoring empty strings; return empty string if nothing to show
  collapse_empty <- function(x) {
    z <- unique(x[!is.na(x) & nzchar(x)])
    if (length(z) == 0) "" else paste(z, collapse = ";")
  }

  genes <- genes[
    ,
    .(
      PANEL_APP = collapse_empty(PANEL_APP),
      INHERITANCE = collapse_empty(INHERITANCE)
    ),
    by = GENE_SYMBOL
  ]

  genes_search <- unique(genes$GENE_SYMBOL)
  # panel_app_condition <- "GENE_SYMBOL %in% genes_search"
  # Toggle negation based on filters$treat_negative
  treat_neg <- isTRUE(filters$treat_negative)
  panel_app_condition <- if (treat_neg) {
    "!(GENE_SYMBOL %in% genes_search)"
  } else {
    "GENE_SYMBOL %in% genes_search"
  }

  # print(panel_app_condition)
  # print(genes_search)
  # print(genes)

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
  filter_expression <- add_filter_condition(filter_expression, quality_filters(filters, data))

  # Apply VEP Annotation filter (for both SNVs and SVs)
  log_info("[filtServer][filter_dataset] Applying VEP annotation filter")
  filter_expression <- add_filter_condition(filter_expression, text_filter("CONSEQUENCE", vep_consequences[consequence %in% filters$annotation_filter, term]))

  spliceai_override_condition <- NULL
  clinvar_override_condition <- NULL

  if (is_snv) {
    log_info("[filtServer][filter_dataset] Applying SNV-specific filters")
    # SNV-specific filters
    if (!is.null(filters$sift_filter) && length(filters$sift_filter) > 0) {
      filter_expression <- add_filter_condition(filter_expression, text_filter("SIFT", filters$sift_filter))
    }
    if (!is.null(filters$polyphen_filter) && length(filters$polyphen_filter) > 0) {
      filter_expression <- add_filter_condition(filter_expression, text_filter("PolyPhen", gsub(" ", "_", filters$polyphen_filter)))
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

# Wrapper functions for SNVs and SVs
snv_filter_dataset <- function(data, filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE)
}

sv_filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = FALSE)
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

apply_filter_legacy_mode2 <- function(input, snvs_data, svs_data, snv_filters, sv_filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  snv_total_time <- system.time({
    snv_filtered_data <- snv_filter_dataset(data=snvs_data,filters=snv_filters,pedigree=pedigree, allele_tab=allele_tab, panel_app_genes=panel_app_genes, vep_consequences=vep_consequences, phenotype_data=phenotype_data)
  })
  log_info(sprintf("nrow(snv_filtered_data): %d", nrow(snv_filtered_data)))
  log_info(sprintf("Total snv_filtered_data execution time: %s", format_time(snv_total_time)))

  sv_total_time <- system.time({
    sv_filtered_data <- sv_filter_dataset(data=svs_data,filters=sv_filters,pedigree=pedigree, allele_tab=allele_tab, panel_app_genes=panel_app_genes, vep_consequences=vep_consequences, phenotype_data=phenotype_data)
  })
  log_info(sprintf("nrow(sv_filtered_data): %d", nrow(sv_filtered_data)))
  log_info(sprintf("Total sv_filter_dataset execution time: %s", format_time(sv_total_time)))

  # convert to data.table if not already
  if (!is.data.table(snv_filtered_data)) snv_filtered_data <- data.table(snv_filtered_data)
  if (!is.data.table(sv_filtered_data)) sv_filtered_data <- data.table(sv_filtered_data)

  log_info(sprintf("col orders in snv_filtered_data: %s", paste(colnames(snv_filtered_data), collapse = ", ")))
  snv_filtered_comphet <- apply_compound_het(snv_filtered_data, pedigree, snv_filters$inheritance_filter)

  log_info(sprintf("nrow(snv_filtered_comphet): %d", nrow(snv_filtered_comphet)))
  sv_filtered_comphet <- apply_compound_het(sv_filtered_data, pedigree, sv_filters$inheritance_filter)

  return(list(snv_filtered_data=snv_filtered_comphet, sv_filtered_data=sv_filtered_comphet))

}

read_search_files <- function(directory, flag_all=TRUE) {
  file_pattern <- ".*\\.tsv$"
  files_defaults <- character(0)
  files_work_dir <- character(0)
  if (flag_all) {
    directory_default <- system.file("extdata", "pre_saved_filters", package = "puzzleapp")
    files_defaults <- list.files(directory_default, pattern = file_pattern, full.names = TRUE)
  }
  if (!is.null(directory)) {
    files_work_dir <- list.files(directory, pattern = file_pattern, full.names = TRUE)
  }
  files <- unique(c(files_work_dir, files_defaults))  # Combine and remove duplicates
  # Initialize an empty list to store results
  search_data <- list()
  # Iterate over each file
  for (file in files) {
    # Read the file into a dataframe
    df <- read.delim(file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
    # Convert the data to a named list
    file_data <- setNames(as.list(df$Value), df$Key)
    # Extract the name of the file without extension to use as label
    label <- tools::file_path_sans_ext(basename(file))
    search_data[[label]] <- file_data
  }
  return(search_data)
}

get_snv_filters <- function(input, phenos) {
  snv_filters <- list(
    clinvar_filter = input$clinvar_checkboxes,
    af_value = as.numeric(input$af),
    annotation_filter = input$conseq_checkboxes,
    revel_value = input$revel,
    sift_filter = input$sift,
    polyphen_filter = input$polyphen,
    spliceai_filter = input$spliceai_score,
    inheritance_filter = input$inher,
    panelapp_filter = input$panelapp,
    custom_genes = parse_gene_list(input$custom_genes),
    treat_negative = input$treat_negative,
    genotype_quality_value = input$genotype_quality,
    allele_balance_value = input$allele_balance,
    hpo_terms_list = phenos
  )
  return(snv_filters)
}

get_sv_filters <- function(input, phenos) {
  sv_filters <- list(
    inheritance_filter = input$inher,
    panelapp_filter = input$panelapp,
    custom_genes = parse_gene_list(input$custom_genes),
    treat_negative = input$treat_negative,
    annotation_filter = input$sv_conseq_checkboxes,
    sv_features = input$sv_features_checkboxes,
    min_svlen = input$min_svlen,
    max_svlen = input$max_svlen,
    genotype_quality_value = input$sv_genotype_quality,
    allele_balance_value = input$sv_allele_balance,
    af_value = as.numeric(input$sv_af),
    hpo_terms_list = phenos
  )
  return(sv_filters)
}

list_files <- function(dir) {
  if(is.null(dir)) return(character(0))
  log_info(sprintf("[filtServer] Listing files in directory: %s", dir))
  if (dir.exists(dir)) {
    list.files(dir, full.names = FALSE)
  } else {
    character(0)
  }
}

capture_filters <- function(input, phenos) {
  # List of shared keys
  shared_keys <- c("Inheritance", "PanelApp Genes", "HPO Terms")

  # SNV filters (excluding shared)
  snv_filters <- list(
    "Annotation" = if (!is.null(input$conseq_checkboxes)) paste(input$conseq_checkboxes, collapse = ";") else "",
    "Pathogenicity" = if (!is.null(input$clinvar_checkboxes)) paste(input$clinvar_checkboxes, collapse = ";") else "",
    "SpliceAI score" = input$spliceai_score,
    "REVEL" = input$revel,
    "AlphaMissense" = input$alpha_missense,
    "SIFT" = input$sift,
    "PolyPhen" = input$polyphen,
    "gnomADv4 AF" = input$af,
    "Affected only" = input$affected_switch,
    "Allele balance" = input$allele_balance,
    "Genotype quality" = input$genotype_quality,
    "Filter value" = input$pass_variants
  )

  # SV filters (excluding shared)
  sv_filters <- list(
    "Annotation" = if (!is.null(input$sv_conseq_checkboxes)) paste(input$sv_conseq_checkboxes, collapse = ";") else "",
    "SV type" = if (!is.null(input$sv_features_checkboxes)) paste(input$sv_features_checkboxes, collapse = ";") else "",
    "Min SV Length" = input$min_svlen,
    "Max SV Length" = input$max_svlen,
    "gnomADv4 AF" = input$sv_af,
    "Affected only" = input$sv_affected_switch,
    "Allele balance" = input$sv_allele_balance,
    "Genotype quality" = input$sv_genotype_quality,
    "Filter value" = input$sv_pass_variants
  )

  # Shared filters: always use unprefixed keys
  shared_filters <- list(
    "Inheritance" = input$inher,
    "PanelApp Genes" = if (!is.null(input$panelapp)) paste(input$panelapp, collapse = ";") else "",
    "HPO Terms" = paste(phenos, collapse = "; "),
    "Custom Genes" = input$custom_genes,
    "Treat Negative" = input$treat_negative
  )

  # Prefix SNV and SV keys
  snv_prefixed <- setNames(snv_filters, paste0("SNV_", names(snv_filters)))
  sv_prefixed <- setNames(sv_filters, paste0("SV_", names(sv_filters)))

  # Combine all into one named list (order: SNV, SV, shared)
  all_filters <- c(snv_prefixed, sv_prefixed, shared_filters)

  # Return as a data.table
  data.table(
    Variable = names(all_filters),
    Value = vapply(all_filters, function(x) if (is.null(x)) "" else as.character(x), FUN.VALUE = character(1))
  )
}

update_filters_params <- function(search_params, session) {
  # Mapping table: for each base param, SNV and SV update info, and shared update info for 3 params
  param_mapping <- list(
    "Annotation" = list(
      snv = list(func = updateCheckboxGroupInput, id = "conseq_checkboxes", selected = TRUE, split = TRUE),
      sv  = list(func = updateCheckboxGroupInput, id = "sv_conseq_checkboxes", selected = TRUE, split = TRUE)
    ),
    "Pathogenicity" = list(
      snv = list(func = updateCheckboxGroupInput, id = "clinvar_checkboxes", selected = TRUE, split = TRUE)
    ),
    "SpliceAI score" = list(
      snv = list(func = updateNumericInput, id = "spliceai_score", value = TRUE, as_numeric = TRUE)
    ),
    "REVEL" = list(
      snv = list(func = updateNumericInput, id = "revel", value = TRUE, as_numeric = TRUE)
    ),
    "AlphaMissense" = list(
      snv = list(func = updateNumericInput, id = "alpha_missense", value = TRUE, as_numeric = TRUE)
    ),
    "SIFT" = list(
      snv = list(func = updateSelectInput, id = "sift", selected = TRUE)
    ),
    "PolyPhen" = list(
      snv = list(func = updateSelectInput, id = "polyphen", selected = TRUE)
    ),
    "gnomADv4 AF" = list(
      snv = list(func = updateSelectInput, id = "af", selected = TRUE, as_numeric = FALSE),
      sv  = list(func = updateSelectInput, id = "sv_af", selected = TRUE, as_numeric = FALSE)
    ),
    "SV type" = list(
      sv  = list(func = updateCheckboxGroupInput, id = "sv_features_checkboxes", selected = TRUE, split = TRUE)
    ),
    "Min SV Length" = list(
      sv  = list(func = updateNumericInput, id = "min_svlen", value = TRUE, as_numeric = TRUE)
    ),
    "Max SV Length" = list(
      sv  = list(func = updateNumericInput, id = "max_svlen", value = TRUE, as_numeric = TRUE)
    ),
    "Affected only" = list(
      snv = list(func = updateCheckboxInput, id = "affected_switch", value = TRUE, as_logical = TRUE),
      sv  = list(func = updateCheckboxInput, id = "sv_affected_switch", value = TRUE, as_logical = TRUE)
    ),
    "Allele balance" = list(
      snv = list(func = updateSliderInput, id = "allele_balance", value = TRUE, as_numeric = TRUE),
      sv  = list(func = updateSliderInput, id = "sv_allele_balance", value = TRUE, as_numeric = TRUE)
    ),
    "Genotype quality" = list(
      snv = list(func = updateSliderInput, id = "genotype_quality", value = TRUE, as_numeric = TRUE),
      sv  = list(func = updateSliderInput, id = "sv_genotype_quality", value = TRUE, as_numeric = TRUE)
    ),
    "Filter value" = list(
      snv = list(func = updateSelectInput, id = "pass_variants", selected = TRUE),
      sv  = list(func = updateSelectInput, id = "sv_pass_variants", selected = TRUE)
    ),
    "Inheritance" = list(
      shared = list(func = updateRadioButtons, id = "inher", selected = TRUE)
    ),
    "PanelApp Genes" = list(
      shared = list(func = updateSelectInput, id = "panelapp", selected = TRUE, split = TRUE)
    ),
    "HPO Terms" = list(
      shared = list(func = updateCheckboxGroupInput, id = "phenotype", selected = TRUE, split = TRUE)
    ),
    "Custom Genes" = list(
      shared = list(func = updateTextInput, id = "custom_genes", value = TRUE)
    ),
    "Treat Negative" = list(
      shared = list(func = updateCheckboxInput, id = "treat_negative", value = TRUE, as_logical = TRUE)
    )
  )

  # If search_params is NULL or empty, do nothing
  if (is.null(search_params) || length(search_params) == 0) return(invisible())

  for (param in names(search_params)) {
    value <- search_params[[param]]
    # Determine type by prefix
    if (startsWith(param, "SNV_")) {
      base_param <- substring(param, 5)
      mapping_entry <- param_mapping[[base_param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$snv)) next
      update_info <- mapping_entry$snv
    } else if (startsWith(param, "SV_")) {
      base_param <- substring(param, 4)
      mapping_entry <- param_mapping[[base_param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$sv)) next
      update_info <- mapping_entry$sv
    } else {
      # Shared params: extend allowlist to include the new three
      if (!param %in% c("Inheritance", "PanelApp Genes", "HPO Terms",
                        "Custom Genes", "Treat Negative")) next
      mapping_entry <- param_mapping[[param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$shared)) next
      update_info <- mapping_entry$shared
    }

    # Conversion logic
    if (!is.null(update_info$as_numeric)) value <- as.numeric(value)
    if (!is.null(update_info$as_logical)) value <- as.logical(value)
    if (!is.null(update_info$split)) value <- unlist(strsplit(value, ";"))

    # Apply updates explicitly based on argument type
    if (!is.null(update_info$selected)) {
      update_info$func(session, update_info$id, selected = value)
    } else if (!is.null(update_info$value)) {
      update_info$func(session, update_info$id, value = value)
    }
  }
}

save_session_data <- function(input, session_name, sessions_dir, snvs_data, svs_data, phenos) {
  session_dir <- sprintf("%s/%s", sessions_dir, session_name)
  log_info(sprintf("[filtServer] Saving session: %s", session_name))

  if (!dir.exists(session_dir)) {
    if (!dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)) {
      showNotification("Failed to create session directory", type = "error")
      return(FALSE)
    }
    message(sprintf("Created session directory: %s", session_dir))
  } else {
    message(sprintf("Session directory already exists: %s", session_dir))
  }

  # Capture current filter states
  filters_dt <- capture_filters(input, phenos)

  # Save filter tables
  fwrite(filters_dt, file = file.path(session_dir, "filters.tsv"), sep = "\t", quote = FALSE, col.names = FALSE)

  # Save flagged rows if any
  save_flagged_rows <- function(data, data_type) {
    if (!is.null(data) && all(c("PRIORITY", "NOTES") %in% colnames(data))) {
      fwrite(data[PRIORITY != 0 | NOTES != "", .(ID, PRIORITY, NOTES)],
        file = file.path(session_dir, sprintf("flagged_rows_%s.tsv", data_type)),
        sep = "\t", quote = FALSE, col.names = TRUE)
      message("Flagged rows saved successfully.")
    } else {
      message("No flagged rows to save.")
    }
  }
  save_flagged_rows(snvs_data, "snvs_data")
  save_flagged_rows(svs_data, "svs_data")

  return(TRUE)
}

save_filters <- function(input, phenos, file_path) {
  filter_save_name <- input$filters_save_name
  filters_dt <- capture_filters(input, phenos)

  if (!dir.exists(file_path)) {
    dir.create(file_path, recursive = TRUE)
  }

  # file_name = file_path + filter_save_name + .tsv
  file_name <- file.path(file_path, paste0(filter_save_name, ".tsv"))
  # add Label as the first row
  fwrite(filters_dt, file = file_name, sep = "\t", quote = FALSE, col.names = FALSE)
  
}

delete_filters <- function(filter_name, file_path) {
  file_name <- file.path(file_path, paste0(filter_name, ".tsv"))
  if (file.exists(file_name)) {
    file.remove(file_name)
    message(sprintf("Deleted filter file: %s", file_name))
    return(TRUE)
  } else {
    message(sprintf("Filter file does not exist: %s", file_name))
    return(FALSE)
  }
}

update_flagged_rows <- function(original_dt, flagged_rows_file) {
  stopifnot(is.data.table(original_dt))  # must be data.table

  # Reset PRIORITY and NOTES in the original data.table
  original_dt[, PRIORITY := 0L]
  original_dt[, NOTES := NA_character_]
  original_dt[, PRIORITYFlag := as.logical(NA)]
  
  # Check required columns
  required_cols <- c("ID", "PRIORITY", "NOTES")
  missing_cols <- setdiff(required_cols, names(original_dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("Original data is missing required columns: %s",
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Return early if no file
  if (!file.exists(flagged_rows_file)) {
    message("Flagged rows file does not exist. Returning original data.")
    return(copy(original_dt))
  }
  
  # Read flagged rows as data.table
  flagged_rows_dt <- fread(flagged_rows_file, sep = "\t", header = TRUE, na.strings = NULL, nThread = 8)
  flagged_rows_dt <- setDT(flagged_rows_dt)
  if (nrow(flagged_rows_dt) == 0) {
    message("Flagged rows file is empty. Returning original data.")
    return(copy(original_dt))
  }
  log_info(sprintf("Read %d flagged rows from file: %s", nrow(flagged_rows_dt), flagged_rows_file))
  
  # Type consistency
  if (!is.numeric(flagged_rows_dt$PRIORITY)) {
    flagged_rows_dt[, PRIORITY := as.numeric(PRIORITY)]
  }
  if (!is.character(flagged_rows_dt$NOTES)) {
    flagged_rows_dt[, NOTES := as.character(NOTES)]
    flagged_rows_dt[is.na(NOTES), NOTES := ""]
  }
  
  # Update only overlapping IDs and merge to retain only the original_dts rows
  # original_dt <- merge(
  #   original_dt[, !c("PRIORITY", "NOTES"), with = FALSE],
  #   flagged_rows_dt[, .(ID, PRIORITY, NOTES)],
  #   by = "ID",
  #   all.x = TRUE
  # )
  # Update only overlapping IDs and keep PRIORITY/NOTES for others
  original_dt[flagged_rows_dt, 
              `:=`(PRIORITY = i.PRIORITY, NOTES = i.NOTES), 
              on = "ID"]
  
  original_dt[, PRIORITYFlag := fifelse(
    is.na(PRIORITY) | PRIORITY == 0, NA,
    fifelse(PRIORITY > 0, TRUE, FALSE)
  )]

  log_info(sprintf("Updated flagged rows from file: %s", flagged_rows_file))
  return(original_dt)
}

load_session_data <- function(input, session_name, sessions_dir, snvs_data, svs_data) {
  session_to_load <- sprintf("%s/%s", sessions_dir, session_name)
  log_info(sprintf("[filtServer] Loading session: %s", session_to_load))
  if (! dir.exists(session_to_load)) {
    showNotification("Session directory does not exist", type = "error")
    return(NULL)
  }
  filters_file <- file.path(session_to_load, "filters.tsv")
  if (!file.exists(filters_file)) {
    showNotification("filters.tsv file does not exist in session directory", type = "error")
    return(NULL)
  }
  snv_flagged_rows_file <- file.path(session_to_load, "flagged_rows_snvs_data.tsv")
  sv_flagged_rows_file <- file.path(session_to_load, "flagged_rows_svs_data.tsv")
  snv_flagged_rows_exists <- file.exists(snv_flagged_rows_file)
  sv_flagged_rows_exists <- file.exists(sv_flagged_rows_file)

  filters_df <- read.delim(filters_file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
  filters_df <- setNames(as.list(filters_df$Value), filters_df$Key)

  log_info("[filtServer][filter_dataset] Updating flagged rows for SNV data")
  snvs_data <- update_flagged_rows(snvs_data, snv_flagged_rows_file)
  log_info("[filtServer][filter_dataset] Updating flagged rows for SV data")
  svs_data <- update_flagged_rows(svs_data, sv_flagged_rows_file)
  return(list(filters=filters_df, snvs_data=snvs_data, svs_data=svs_data))

}

delete_session_data <- function(session_name, sessions_dir) {
  session_to_delete <- sprintf("%s/%s", sessions_dir, session_name)
  log_info(sprintf("[filtServer] Deleting session: %s", session_to_delete))
  if (dir.exists(session_to_delete)) {
    unlink(session_to_delete, recursive = TRUE)
    log_info(sprintf("Deleted session directory: %s", session_to_delete))
    return(TRUE)
  } else {
    log_info(sprintf("Session directory does not exist: %s", session_to_delete))
    return(FALSE)
  }
}
