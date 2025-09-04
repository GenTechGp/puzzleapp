# Helper function: Generate condition for allele count filtering
compare_allele_count <- function(col, values) {
  values <- as.numeric(unlist(strsplit(values, "-")))
  if (length(values) == 1) return(col == values)
  if (length(values) == 2) return(col >= values[1] & col <= values[2])
  return(rep(TRUE, length(col)))
}

# Helper function: Construct filter expression
add_filter_condition <- function(filter_expression, condition) {
  cat("Adding filter condition:", condition, "\n")
  if (!is.null(condition) && condition != "") {
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

# Helper function: Handle panel app gene filtering
panelapp_filter <- function(filters, panel_app_genes) {
  if (length(filters$panelapp_filter) > 0) {
    genes <- panel_app_genes[Level4 %in% filters$panelapp_filter,.(PANEL_APP=Level4,GENE_SYMBOL=Entity_Name,INHERITANCE=Model_Of_Inheritance)]
    genes <- genes[,.(PANEL_APP=paste(PANEL_APP,collapse=";"),INHERITANCE=paste(INHERITANCE,collapse=";")),by=GENE_SYMBOL]
    genes_search <- genes[,GENE_SYMBOL]
    panel_app_condition <- "GENE_SYMBOL %in% genes_search"
    return(list(panel_app_condition,genes_search,genes))
  } else {
    return(NULL)
  }
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


text_filter <- function(column, values) {
  if (is.null(values) || length(values) == 0 || nzchar(values) == 0) return(NULL)
  return(sprintf("grepl('%s', %s, ignore.case = TRUE)", paste(values, collapse = "|"), column))
}

# Generic filter function for SNVs and SVs
filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE) {
  # browser()
  cat(sprintf("[filtServer][filter_dataset] Filtering dataset (SNV: %s)\n", is_snv))

  filter_expression <- "TRUE"
  global_filters_expression <- "TRUE"

  # Apply common filters
  inheritance_filter_condition <- inheritance_filter(filters, pedigree, allele_tab)
  if (!is.null(inheritance_filter_condition)) {
    cat("[filtServer][filter_dataset] Applying inheritance filter\n")
    filter_expression <- add_filter_condition(filter_expression, inheritance_filter_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, inheritance_filter_condition)
  }
  panelapp_filter_results <- panelapp_filter(filters, panel_app_genes)
  if (!is.null(panelapp_filter_results)) {
    cat("[filtServer][filter_dataset] Applying PanelApp filter\n")
    panelapp_filter_condition <- panelapp_filter_results[[1]]
    genes_search <- panelapp_filter_results[[2]]
    genes <- panelapp_filter_results[[3]]
    print(class(genes))
    filter_expression <- add_filter_condition(filter_expression, panelapp_filter_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, panelapp_filter_condition)
  }
  
  cat("[filtServer][filter_dataset] Applying quality filters\n")
  filter_expression <- add_filter_condition(filter_expression, quality_filters(filters, data))

  # Apply VEP Annotation filter (for both SNVs and SVs)
  if (!is.null(filters$annotation_filter) && length(filters$annotation_filter) > 0) {
    cat("[filtServer][filter_dataset] Applying VEP annotation filter\n")

    # filter_expression <- add_filter_condition(filter_expression, text_filter("CONSEQUENCE", vep_consequences[consequence %in% filters$annotation_filter, term]))
    conseq_col <- "consequence"  # name of the column to filter on
    term_col <- "term"            # name of the column to extract

    terms <- vep_consequences[
      vep_consequences[[conseq_col]] %in% filters$annotation_filter,
      term_col,
      with = FALSE
    ][[1]]

    filter_expression <- add_filter_condition(
      filter_expression,
      text_filter("CONSEQUENCE", terms)
    )
  }

  spliceai_override_condition <- NULL
  clinvar_override_condition <- NULL

  if (is_snv) {
    cat("[filtServer][filter_dataset] Applying SNV-specific filters\n")
    # SNV-specific filters
    # browser()
    filter_expression <- add_filter_condition(filter_expression, text_filter("SIFT", filters$sift_filter))
    filter_expression <- add_filter_condition(filter_expression, text_filter("PolyPhen", gsub(" ", "_", filters$polyphen_filter)))

    # ClinVar filter and override
    if (!is.null(filters$clinvar_filter) && length(filters$clinvar_filter) > 0) {
      cat("[filtServer][filter_dataset] Applying ClinVar filter\n")
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
      cat("[filtServer][filter_dataset] Applying SpliceAI override filter\n")
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
    cat("[filtServer][filter_dataset] Filtering SVs based on type and length\n")
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
  cat(sprintf("[filtServer][filter_dataset] Filter expression: %s\n", combined_expression))
  
  #added to debug
  # combined_expression <- prefix_data_cols(combined_expression, data)
  # cat("Rewritten expression:", combined_expression, "\n")
  cat("Class of data:", class(data), "\n")
  cat("Number of rows in data:", nrow(data), "\n")
  # filtered_data <- data[eval(parse(text = combined_expression))]
  filtered_data <- with(data, data[eval(parse(text = combined_expression)), ])
  cat("Class of filtered_data:", class(filtered_data), "\n")
  cat("Number of rows after filtering:", nrow(filtered_data), "\n")
  # Compute filtered out rows (the complement)
  # filtered_out <- with(data, data[!eval(parse(text = combined_expression)), ])
  # cat("Number of rows filtered out:", nrow(filtered_out), "\n")
  # cat("Preview of filtered_out rows:\n")
  # print(head(filtered_out, 10))
  cat("first eval done\n")
  # browser()
  # Add panel app and HPO information
  if (length(filters$panelapp_filter) > 0) {
    setkey(filtered_data, GENE_SYMBOL)
    setkey(genes, GENE_SYMBOL)
    filtered_data <- merge(filtered_data, genes, by = "GENE_SYMBOL", all.x = TRUE)
  } else {
    # filtered_data[, PANEL_APP := NA]
    # filtered_data[, INHERITANCE := NA]
    filtered_data$PANEL_APP <- if(nrow(filtered_data) == 0) character(0) else NA_character_
    filtered_data$INHERITANCE <- if(nrow(filtered_data) == 0) character(0) else NA_character_
  }

  if (!is.null(filters$hpo_terms_list) && length(filters$hpo_terms_list) > 0) {
    split_hpo_terms_list <- unlist(strsplit(filters$hpo_terms_list, "; "))
    hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list & gene_symbol %in% filtered_data$GENE_SYMBOL, .(HPO_ID = hpo_id, GENE_SYMBOL = gene_symbol)]
    hpo_terms_summary <- hpo_terms_data[, .(HPO_ID = paste(HPO_ID, collapse = ";"), HPO_COUNT = .N), by = GENE_SYMBOL]
    setkey(hpo_terms_summary, GENE_SYMBOL)
    filtered_data <- merge(filtered_data, hpo_terms_summary, by = "GENE_SYMBOL", all.x = TRUE)
    filtered_data[is.na(HPO_COUNT), HPO_COUNT := 0]
  } else {
    # filtered_data[, HPO_ID := NA]
    # filtered_data[, HPO_COUNT := 0]
    filtered_data$HPO_ID <- if(nrow(filtered_data) == 0) character(0) else NA_character_
    filtered_data$HPO_COUNT <- if(nrow(filtered_data) == 0) integer(0) else 0

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
  # Initialize columns
  # filtered_data$clinvar_override <- FALSE
  # filtered_data$spliceai_override <- FALSE
  filtered_data$clinvar_override <- if(nrow(filtered_data) == 0) logical(0) else FALSE
  filtered_data$spliceai_override <- if(nrow(filtered_data) == 0) logical(0) else FALSE

  # Apply conditions if provided
  if (!is.null(clinvar_override_condition) && nrow(filtered_data) > 0) {
    # Create a logical vector for the condition
    idx <- with(filtered_data, eval(parse(text = clinvar_override_condition)))
    filtered_data$clinvar_override[idx] <- TRUE
  }

  if (!is.null(spliceai_override_condition) && nrow(filtered_data) > 0) {
    idx <- with(filtered_data, eval(parse(text = spliceai_override_condition)))
    filtered_data$spliceai_override[idx] <- TRUE
  }

  cat(sprintf("[filtServer][filter_dataset] Filtering complete: %d variants retained\n", nrow(filtered_data)))
  return(filtered_data)
}

# Wrapper functions for SNVs and SVs
snv_filter_dataset <- function(data, filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE)
}

sv_filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = FALSE)
}

apply_filter_legacy <- function(data, snv_filters, sv_filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  snv_filtered_data <- snv_filter_dataset(data=data[data$CATEGORY=="SNV & Indel",],filters=snv_filters,pedigree=pedigree_dt, allele_tab=allele_counts_dt, panel_app_genes=NULL, vep_consequences=vep_consequences, phenotype_data=NULL)
  sv_filtered_data <- sv_filter_dataset(data=data[data$CATEGORY=="SV",],filters=sv_filters,pedigree=pedigree_dt, allele_tab=allele_counts_dt, panel_app_genes=NULL, vep_consequences=vep_consequences, phenotype_data=NULL)
  return(list(snv_filtered_data=snv_filtered_data, sv_filtered_data=sv_filtered_data))
}

# observeEvent(input$apply_filter, {

#       showNotification("Filtering...", duration = NULL, id = ns("notify_filter"),
#                        type = "message")
      
#       # If Custom inheritance, update allele_tab from custom_allele_tab
#       if (input$inher == "Custom") {
#         allele_tab(custom_allele_tab())
#       }

#       # Define filters
#       snv_filters <- list(
#         clinvar_filter = input$clinvar_checkboxes,
#         af_value = as.numeric(input$af),
#         annotation_filter = input$conseq_checkboxes,
#         revel_value = input$revel,
#         sift_filter = input$sift,
#         polyphen_filter = input$polyphen,
#         spliceai_filter = input$spliceai_score,
#         inheritance_filter = input$inher,
#         panelapp_filter = input$panelapp,
#         genotype_quality_value = input$genotype_quality,
#         allele_balance_value = input$allele_balance,
#         hpo_terms_list = phenos()
#       )
      
      
#       if (nrow(dataset[CATEGORY == "SNV & Indel"]) > 0) {
#         print("")
#         snv_total_time <- system.time({
#           snv_filtered_data <- snv_filter_dataset(dataset[CATEGORY=="SNV & Indel"],snv_filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data)
#         })
#         #cat(paste("Total execution time:", format_time(snv_total_time), "\n"))
#         message("[filtServer] Time for SNV filtering: ", format_time(snv_total_time))
#       } else {
#         message("[filtServer] No SNV & Indel data found — skipping SNV filtering")
#       }

#       # Define filters for SV
#       sv_filters <- list(
#         inheritance_filter = input$inher,
#         panelapp_filter = input$panelapp,
#         annotation_filter = input$sv_conseq_checkboxes,
#         sv_features = input$sv_features_checkboxes,
#         min_svlen = input$min_svlen,
#         max_svlen = input$max_svlen,
#         genotype_quality_value = input$sv_genotype_quality,
#         allele_balance_value = input$sv_allele_balance,
#         af_value = as.numeric(input$sv_af),
#         hpo_terms_list = phenos()
#       )

#       # Filter SVs
#       if (nrow(dataset[CATEGORY == "SV"]) > 0) {
#         sv_total_time <- system.time({
#           sv_filtered_data <- sv_filter_dataset(dataset[CATEGORY == "SV"],sv_filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data)
#         })
#         #cat(paste("Total execution time:", format_time(sv_total_time), "\n"))
#         message("[filtServer] Time for SV filtering: ", format_time(sv_total_time))
#       } else {
#         message("[filtServer] No SV data found — skipping SV filtering")
#       }
      
#       filtered_list <- list()
      
#       if (exists("snv_filtered_data") && is.data.table(snv_filtered_data) && nrow(snv_filtered_data) > 0) {
#         filtered_list[[length(filtered_list) + 1]] <- snv_filtered_data
#       }
      
#       if (exists("sv_filtered_data") && is.data.table(sv_filtered_data) && nrow(sv_filtered_data) > 0) {
#         filtered_list[[length(filtered_list) + 1]] <- sv_filtered_data
#       }
      
#       if (length(filtered_list) > 0) {
#         all_filtered_data <- rbindlist(filtered_list, use.names = TRUE, fill = TRUE)
#       } else {
#         removeNotification(ns("notify_filter"))
#         showNotification("No variants passed filtering", type = "warning")
#         showNotification("Showing all variants", type = "warning")
#         filtered_table_output(dataset)
#         return()  # Exit the observeEvent early
#       }
      
#       #all_filtered_data <- rbind(snv_filtered_data,sv_filtered_data)
      
#       #print("past filtering")

#       if (input$inher=="Compound Heterozygous") {
#         cat("[filtServer] Applying compound heterozygous filtering...\n")
#         is_trio <- sum(pedigree$kinship %in% c("mother", "father")) == 2
#         if (!is_trio) {
#           comp_hets_1 <- all_filtered_data[alt_allele_count_1 == 1 & GT_1 == "1|0", .(VAR_COUNT_1 = .N), by = GENE_SYMBOL]
#           comp_hets_2 <- all_filtered_data[alt_allele_count_1 == 1 & GT_1 == "0|1", .(VAR_COUNT_2 = .N), by = GENE_SYMBOL]
#           comp_hets <- merge(comp_hets_1, comp_hets_2, by = "GENE_SYMBOL", all = TRUE)[VAR_COUNT_1 > 0 & VAR_COUNT_2 > 0]
#           all_filtered_data <- all_filtered_data[GENE_SYMBOL %in% comp_hets$GENE_SYMBOL]
#         } else {
#           comp_hets <- all_filtered_data[
#             alt_allele_count_1 == 1 &  # Proband is heterozygous
#               ((GT_2 == "1|0" & GT_3 == "0|1") | (GT_2 == "0|1" & GT_3 == "1|0")),  # Opposite inheritance from parents
#             .(VAR_COUNT = .N), by = GENE_SYMBOL]
#           # Keep genes where at least 2 variants exist and are inherited from different parents
#           valid_genes <- comp_hets[VAR_COUNT > 1, GENE_SYMBOL]
#           all_filtered_data <- all_filtered_data[GENE_SYMBOL %in% valid_genes]
#         }
#       }

#       # Handle PRIORITY and NOTES
#       filtered_data <- data.table(filtered_table_output())
#       #print(filtered_data)

#       if (!is.null(filtered_data) && all(c("PRIORITY", "NOTES") %in% colnames(filtered_data))) {
#         # Extract flagged rows
#         current_flagged_rows <- filtered_data[,
#           .(ID, CURRENT_PRIORITY=PRIORITY,CURENT_NOTES=NOTES)
#         ]
#         # current_flagged_rows[, CURENT_NOTES := as.character(CURENT_NOTES)]
#         # current_flagged_rows[is.na(CURENT_NOTES),CURENT_NOTES:=""]
#         #str(current_flagged_rows)

#         # Check for changes in flagged rows
#         previous_flagged_rows <- flagged_rows_reactive()
#         #str(previous_flagged_rows)
#         if (!is.null(previous_flagged_rows)) {
#           # Identify new or updated rows
#           merged_flagged_rows <- merge(
#             previous_flagged_rows, current_flagged_rows,
#             by = "ID", all = TRUE)
#         }
#         current_flagged_rows <- merged_flagged_rows[,.(ID,
#             PRIORITY = fifelse(is.na(PRIORITY) | (PRIORITY != CURRENT_PRIORITY & !is.na(CURRENT_PRIORITY)), CURRENT_PRIORITY, PRIORITY),
#             NOTES = fifelse(is.na(NOTES) | (NOTES != CURENT_NOTES & !is.na(CURENT_NOTES)), CURENT_NOTES, NOTES))]

#         # Update flagged rows reactive value
#         flagged_rows_reactive(copy(current_flagged_rows))

#         # Merge flagged rows back into the filtered data
#         all_filtered_data <- merge(current_flagged_rows, all_filtered_data, by = "ID", all.y = TRUE)
#         all_filtered_data[is.na(PRIORITY), PRIORITY := 0]
#         all_filtered_data[PRIORITY > 0, PRIORITYFlag := TRUE]
#         all_filtered_data[PRIORITY < 0, PRIORITYFlag := FALSE]
#       } else {
#         # Fallback if no PRIORITY or NOTES columns exist
#         all_filtered_data <- data.table(PRIORITY = 0, NOTES = "", all_filtered_data)
#       }

#       filtered_table_output(copy(all_filtered_data))
#       #print("after")

#       removeNotification(ns("notify_filter"))
#       showNotification("Data filtered", type = "message")
#     }) 

#     return(filtered_table_output)
#   })