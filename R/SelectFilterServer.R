# Metadata

metaServer <- function(output, ns, pedigree) {
  peditab <- pedigree[, 1:4]
  opts <- c("affected", "unaffected")

  for (i in seq_len(dim(pedigree)[1])) {
    id = paste0("status_", pedigree$sample_id[i])
    selected = pedigree$status[i]
    peditab$status[i] <- as.character(selectInput(ns(id), NULL, opts, selected))
  }
  names(peditab) <- c("Sample ID", "Kinship", "Status", "Sex")
  output$meta <- renderTable(peditab, sanitize.text.function = function(x) x)
}

############################################

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
    return(paste(filter_expression, condition, sep = " & "))
  }
  return(filter_expression)
}

# Helper function: Handle inheritance filtering
inheritance_filter <- function(filters, pedigree, allele_tab) {
  if (!is.null(filters$inheritance_filter) && filters$inheritance_filter != "") {
    allele_count <- allele_tab()
    names(allele_count) <- c("sample_id", "allele_count")
    allele_count <- merge(pedigree, allele_count, by = "sample_id")
    allele_count[, col_name := paste0("alt_allele_count_", code), by = sample_id]
    
    inheritance_conditions <- vector("character")
    
    for (i in seq_len(nrow(allele_count))) {
      col_name <- allele_count[i, col_name]
      val <- allele_count[i, allele_count]
      condition <- sprintf("compare_allele_count(get('%s'), '%s')",col_name,val)
      inheritance_conditions <- c(inheritance_conditions, condition)
    }
    
    # Combine inheritance conditions into a single OR expression
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

# Helper function: Apply text-based filters
text_filter <- function(column, values) {
  if (length(values) == 0) return(NULL)
  return(sprintf("grepl('%s', %s, ignore.case = TRUE)", paste(values, collapse = "|"), column))
}

# Generic filter function for SNVs and SVs
filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE) {
  
  filter_expression <- "TRUE"
  global_filters_expression <- "TRUE"
  
  # Apply common filters
  inheritance_filter_condition <- inheritance_filter(filters, pedigree, allele_tab)
  if (!is.null(inheritance_filter_condition)) {
    filter_expression <- add_filter_condition(filter_expression, inheritance_filter_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, inheritance_filter_condition)
  }
  panelapp_filter_results <- panelapp_filter(filters, panel_app_genes)
  if (!is.null(panelapp_filter_results)) {
    panelapp_filter_condition <- panelapp_filter_results[[1]]
    genes_search <- panelapp_filter_results[[2]]
    genes <- panelapp_filter_results[[3]]
    print(class(genes))
    filter_expression <- add_filter_condition(filter_expression, panelapp_filter_condition)
    global_filters_expression <- add_filter_condition(global_filters_expression, panelapp_filter_condition)
  }
  filter_expression <- add_filter_condition(filter_expression, quality_filters(filters, data))
  
  # Apply VEP Annotation filter (for both SNVs and SVs)
  filter_expression <- add_filter_condition(filter_expression, text_filter("CONSEQUENCE", vep_consequences[consequence %in% filters$annotation_filter, term]))
  
  spliceai_override_condition <- NULL
  clinvar_override_condition <- NULL
  
  if (is_snv) {
    # SNV-specific filters
    filter_expression <- add_filter_condition(filter_expression, text_filter("SIFT", filters$sift_filter))
    filter_expression <- add_filter_condition(filter_expression, text_filter("PolyPhen", gsub(" ", "_", filters$polyphen_filter)))
    
    # ClinVar filter and override
    if (!is.null(filters$clinvar_filter) && length(filters$clinvar_filter) > 0) {
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
    
    override_conditions <- c(spliceai_override_condition, clinvar_override_condition)
    override_conditions <- Filter(Negate(is.null), override_conditions)  # Remove NULLs
    
    if (length(override_conditions) > 0) {
      filter_expression <- paste(filter_expression, paste(override_conditions, collapse = " | "), sep = " | ")
    }
    
  } else {
    # SV-specific filters
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
  print(combined_expression)
  filtered_data <- data[eval(parse(text = combined_expression))]
  
  # Add panel app and HPO information
  if (length(filters$panelapp_filter) > 0) {
    setkey(filtered_data, GENE_SYMBOL)
    setkey(genes, GENE_SYMBOL)
    filtered_data <- merge(filtered_data, genes, by = "GENE_SYMBOL", all.x = TRUE)
  } else {
    filtered_data[, PANEL_APP := NA]
    filtered_data[, INHERITANCE := NA]
  }
  
  if (!is.null(filters$hpo_terms_list) && length(filters$hpo_terms_list) > 0) {
    split_hpo_terms_list <- unlist(strsplit(filters$hpo_terms_list, "; "))
    hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list & gene_symbol %in% filtered_data$GENE_SYMBOL, .(HPO_ID = hpo_id, GENE_SYMBOL = gene_symbol)]
    hpo_terms_summary <- hpo_terms_data[, .(HPO_ID = paste(HPO_ID, collapse = ";"), HPO_COUNT = .N), by = GENE_SYMBOL]
    setkey(hpo_terms_summary, GENE_SYMBOL)
    filtered_data <- merge(filtered_data, hpo_terms_summary, by = "GENE_SYMBOL", all.x = TRUE)
    filtered_data[is.na(HPO_COUNT), HPO_COUNT := 0]
  } else {
    filtered_data[, HPO_ID := NA]
    filtered_data[, HPO_COUNT := 0]
  }
  
  filtered_data[, clinvar_override := FALSE]
  filtered_data[, spliceai_override := FALSE]
  
  if (!is.null(clinvar_override_condition)) {
    filtered_data[eval(parse(text = clinvar_override_condition)),
                  clinvar_override := TRUE]
  }
  
  if (!is.null(spliceai_override_condition)) {
    filtered_data[eval(parse(text = spliceai_override_condition)),
                  spliceai_override := TRUE]
  }
  
  return(filtered_data)
}

# Wrapper functions for SNVs and SVs
snv_filter_dataset <- function(data, filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = TRUE)
}

sv_filter_dataset <- function(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data) {
  filter_dataset(data, filters, pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data, is_snv = FALSE)
}

############################################

read_search_files <- function(directory, type=NULL) {
  
  file_pattern <- if (!is.null(type)) {
    paste0(".*\\.", type, "_search.tsv$")
  } else {
    ".*_search\\.tsv$"
  }
  
  # List all TSV files in the directory
  files <- list.files(directory, pattern = file_pattern, full.names = TRUE)
  
  # Initialize an empty list to store results
  search_data <- list()
  
  # Iterate over each file
  for (file in files) {
    # Read the file into a dataframe
    df <- read.delim(file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
    
    # Convert the data to a named list
    file_data <- setNames(as.list(df$Value), df$Key)
    
    # Extract the Label field as the key
    label <- file_data[["Label"]]
    
    if (!is.null(label)) {
      # Store the data inside the main list, using label as key
      search_data[[label]] <- file_data
    } else {
      warning(sprintf("Skipping file %s as it has no 'Label' field", file))
    }
  }
  
  return(search_data)
}

alleleCount <- function(inher, status, sex) {
  if (inher == "Homozygous Recessive") {
    if (status == "affected")
      "2"
    else
      "0-1"
  } else if (inher == "Dominant/De Novo") {
    if (status == "affected")
      "1-2"
    else
      "0"
  } else if (inher == "Compound Heterozygous") {
    if (status == "affected")
      "1"
    else
      "0-1"
  } else if (inher == "X-Linked Recessive") {
    if (sex == "male") {
      if (status == "affected")
        "1"
      else
        "0"
    } else {
      if (status == "affected")
        "2"
      else
        "0-1"
    }
  } else {
    ""
  }
}

alleleTable <- function(pedigree, alleles_FUN) {
  tab <- data.frame(id = pedigree$sample_id, alleles = NA)
  tab$alleles <- sapply(pedigree$sample_id, alleles_FUN)
  names(tab) <- c("Sample ID", "Allele Count")
  tab
}

alleleCustomTable <- function(ns, pedigree) {
  FUN <- function(x) {
    boxId <- paste0("allele_", x)
    names <- c("None", "0", "0-1", "1", "1-2", "2")
    vals <- c("", "0", "0-1", "1", "1-2", "2")
    as.character(radioButtons(ns(boxId), NULL, selected = "", inline = TRUE,
                              choiceNames = names, choiceValues = vals))
  }
  alleleTable(pedigree, FUN)
}

alleleServer <- function(input, output, ns, pedigree, allele_tab) {
  # Init allele table and custom checkbox table
  allele_tab(alleleTable(pedigree, function(x) ""))
  allele_custom_tab <- alleleCustomTable(ns, pedigree)

  n <- dim(pedigree)[1]

  alleles <- reactiveValues()
  for (i in seq_len(n)) {
    id <- pedigree$sample_id[i]
    sid <- paste0("status_", id)
    alleles[[id]] <- reactive(
      if (is.null(input[[sid]])) {
        alleleCount(input$inher, pedigree$status[i], pedigree$sex[i])
      } else {
        alleleCount(input$inher, input[[sid]], pedigree$sex[i])
      }
    )
  }

  tab <- reactive({
    if (input$inher == "") {
      NULL
    } else if (input$inher == "Custom") {
      allele_custom_tab
    } else {
      tmp <- isolate(allele_tab())
      for (i in seq_len(n)) {
        id <- tmp[i, "Sample ID"]
        tmp[i, "Allele Count"] <- alleles[[id]]()
      }
      allele_tab(tmp)
      tmp
    }
  })
  output$allele <- renderTable(tab(), sanitize.text.function = function(x) x)

  # Update allele table on custom checkbox event
  for (id in pedigree$sample_id) {
    boxId <- paste0("allele_", id)
    observeEvent(input[[boxId]], {
      tab <- allele_tab()
      cnt <- c(input[[boxId]])
      tab[tab["Sample ID"] == id, "Allele Count"] <- cnt
      allele_tab(tab)
    })
  }
}

# Main

selectFiltersServer <- function(id, dataset, pedigree, panel_app_genes, vep_consequences, phenotype_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_spinner()

    number_of_individuals <- reactiveVal(dim(pedigree)[1])

    allele_tab <- reactiveVal()

    green_genes <- reactiveVal()
    red_genes <- reactiveVal()
    amber_genes <- reactiveVal()
    unclassified_genes <- reactiveVal()

    phenos <- reactiveVal()

    filtered_table_output <- reactiveVal(dataset)

    igv_coord_box <- reactiveVal(NULL)

    initialisation <- reactiveVal(TRUE)

    metaServer(output, ns, pedigree)

    flagged_rows_reactive <- reactiveVal(data.table(ID = character(0), PRIORITY = numeric(0), NOTES = character(0)))

    observe({
      shinyjs::enable(ns("inher"))
      shinyjs::enable(ns("pathogenicity"))
      shinyjs::enable(ns("pre_saved_search"))
      shinyjs::enable(ns("apply_filter"))
    })

    alleleServer(input, output, ns, pedigree, allele_tab)
    
    app_dir <- getwd()
    data_dir <- paste0(app_dir, "/data")

    # Load available searches on startup
    available_searches <- reactive({
      read_search_files(data_dir, "snv")
    })
    
    sv_available_searches <- reactive({
      read_search_files(data_dir, "sv")
    })
    
    # Sessions
    sessions_dir <- reactive({
      outdir <- Sys.getenv("OUTDIR")
      sample <- pedigree[kinship=="proband",sample_id]
      session_dir <- sprintf("%s/%s",outdir,sample)
      session_dir
    })

    # List all session directories
    all_sessions <- reactive({
      dir_path <- sessions_dir()
      
      if (dir.exists(dir_path)) {
        sessions <- list.files(dir_path, full.names = FALSE)
        print(sessions)
        sessions
      } else {
        character(0)
      }
    })
    
    observeEvent(input$save_session, {
      
      session_dir <- sessions_dir()
      session_dir <- sprintf("%s/%s",session_dir,input$session_name)
      
      if (!dir.exists(session_dir)) {
        dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
        message(sprintf("Created session directory: %s", session_dir))
      } else {
        message(sprintf("Session directory already exists: %s", session_dir))
      }
      
      # Capture SNV filter states
      snv_filters <- list(
        "Inheritance" = input$inher,
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
        "Filter value" = input$pass_variants,
        "PanelApp Genes" = if (!is.null(input$panelapp)) paste(input$panelapp, collapse = ";") else "",
        "HPO Terms" = if (!is.null(input$phenotype)) paste(input$phenotype, collapse = ";") else ""
      )
      
      # Capture SV filter states
      sv_filters <- list(
        "Inheritance" = input$inher,
        "Annotation" = if (!is.null(input$sv_conseq_checkboxes)) paste(input$sv_conseq_checkboxes, collapse = ";") else "",
        "SV type" = if (!is.null(input$sv_features_checkboxes)) paste(input$sv_features_checkboxes, collapse = ";") else "",
        "Min SV Length" = input$min_svlen,
        "Max SV Length" = input$max_svlen,
        "gnomADv4 AF" = input$sv_af,
        "Affected only" = input$sv_affected_switch,
        "Allele balance" = input$sv_allele_balance,
        "Genotype quality" = input$sv_genotype_quality,
        "Filter value" = input$sv_pass_variants,
        "PanelApp Genes" = if (!is.null(input$panelapp)) paste(input$panelapp, collapse = ";") else "",
        "HPO Terms" = if (!is.null(input$phenotype)) paste(input$phenotype, collapse = ";") else ""
      )
      
      # Convert SNV filter list to data.table
      snv_filters_dt <- data.table(
        Variable = names(snv_filters),
        Value = vapply(snv_filters, function(x) {
          if (is.null(x)) "" else paste(x, collapse = ";")
        }, FUN.VALUE = character(1))
      )
      
      # Convert SV filter list to data.table
      sv_filters_dt <- data.table(
        Variable = names(sv_filters),
        Value = vapply(sv_filters, function(x) {
          if (is.null(x)) "" else paste(x, collapse = ";")
        }, FUN.VALUE = character(1))
      )
      
      filtered_data <- data.table(filtered_table_output())
      
      snv_filters_file <- sprintf("%s/snv_filters.tsv",session_dir)
      sv_filters_file <- sprintf("%s/sv_filters.tsv",session_dir)
      flagged_rows_file <- sprintf("%s/flagged_rows.tsv", session_dir)
      
      # Save tables without column names
      fwrite(snv_filters_dt, file = snv_filters_file, sep = "\t", quote = FALSE, col.names = FALSE)
      fwrite(sv_filters_dt, file = sv_filters_file, sep = "\t", quote = FALSE, col.names = FALSE)
      
      # Save flagged rows only if it contains data
      if (!is.null(filtered_data) && all(c("PRIORITY", "NOTES") %in% colnames(filtered_data))) {
        fwrite(filtered_data[PRIORITY !=0 | NOTES != "",.(ID, PRIORITY,NOTES)], file = flagged_rows_file, sep = "\t", quote = FALSE, col.names = TRUE)
        message("Flagged rows saved successfully.")
      } else {
        message("No flagged rows to save.")
      }
      
    })
    
    
    observeEvent(input$load_session, {
      session_name <- input$available_sessions
      session_dir <- sessions_dir()
      session_to_load <- sprintf("%s/%s",session_dir,session_name)
      
      if (dir.exists(session_to_load)) {
        snv_file <- file.path(session_to_load, "snv_filters.tsv")
        sv_file <- file.path(session_to_load, "sv_filters.tsv")
        flagged_rows_file <- file.path(session_to_load, "flagged_rows.tsv")
        
        snv_exists <- file.exists(snv_file)
        sv_exists <- file.exists(sv_file)
        flagged_rows_exists <- file.exists(flagged_rows_file)
        
        if (snv_exists) {
          snv_df <- read.delim(snv_file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
          snv_df <- setNames(as.list(snv_df$Value), snv_df$Key)
          update_search_params(snv_df, session, type="snv")
        }
        
        if (sv_exists) {
          sv_df <- read.delim(sv_file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
          sv_df <- setNames(as.list(sv_df$Value), sv_df$Key)
          update_search_params(sv_df, session, type="sv")
        }
        
        if (flagged_rows_exists) {
          flagged_rows_dt <- fread(flagged_rows_file, sep = "\t", header = TRUE, na.strings = "", nThread = 8)
          filtered_data <- data.table(filtered_table_output())
          str(flagged_rows_dt)
          cols_to_keep <- c("ID",setdiff(names(filtered_data), names(flagged_rows_dt)))
          merged_data <- merge(filtered_data[, ..cols_to_keep], flagged_rows_dt, by = "ID", all = TRUE)
          filtered_table_output(copy(merged_data))
        }
        shinyjs::delay(100, shinyjs::click("apply_filter"))
      }
    })
    
    # Update UI dropdown with available searches
    observe({
      choices <- c("", names(available_searches()))
      updateSelectizeInput(session, "pre_saved_search",
                        choices = choices, selected = "")
    })
    
    observe({
      choices <- c("", names(sv_available_searches()))
      updateSelectizeInput(session, "sv_pre_saved_search",
                           choices = choices, selected = "")
    })
    
    # Update UI dropdown with available sessions
    observe({
      choices <- c("", all_sessions())
      print(choices)
      updateSelectizeInput(session, "available_sessions",
                           choices = choices, selected = "")
    })
    
    # Generalized function to update UI elements
    update_search_params <- function(search_params, session, type="snv") {
      
      # Define default values for clearing selections
      default_values <- list(
        "Inheritance" = "",
        "Annotation" = character(0),
        "Pathogenicity" = character(0),
        "SpliceAI score" = 0,
        "gnomADv4 AF" = "",
        "Affected only" = FALSE,
        "Allele balance" = 0,
        "Genotype quality" = 0,
        "Filter value" = ""
      )
      
      if (type == "snv") {
        update_mapping <- list(
          "Inheritance" = list(func = updateSelectInput, id = "inher", selected = TRUE),
          "Annotation" = list(func = updatePrettyCheckboxGroup, id = "conseq_checkboxes", selected = TRUE, split = TRUE),
          "Pathogenicity" = list(func = updatePrettyCheckboxGroup, id = "clinvar_checkboxes", selected = TRUE, split = TRUE),
          "SpliceAI score" = list(func = updateNumericInput, id = "spliceai_score", value = TRUE, as_numeric = TRUE),
          "REVEL" = list(func = updateNumericInput, id = "revel", value = TRUE, as_numeric = TRUE),
          "AlphaMissense" = list(func = updateNumericInput, id = "alpha_missense", value = TRUE, as_numeric = TRUE),
          "SIFT" = list(func = updateSelectInput, id = "sift", selected = TRUE),
          "PolyPhen" = list(func = updateSelectInput, id = "polyphen", selected = TRUE),
          "gnomADv4 AF" = list(func = updateSelectInput, id = "af", selected = TRUE, as_numeric = TRUE),
          "Affected only" = list(func = updateMaterialSwitch, id = "affected_switch", value = TRUE, as_logical = TRUE),
          "Allele balance" = list(func = updateSliderInput, id = "allele_balance", value = TRUE, as_numeric = TRUE),
          "Genotype quality" = list(func = updateSliderInput, id = "genotype_quality", value = TRUE, as_numeric = TRUE),
          "Filter value" = list(func = updateSelectInput, id = "pass_variants", selected = TRUE),
          "PanelApp Genes" = list(func = updateSelectInput, id = "panelapp", selected = TRUE, split = TRUE),
          "HPO Terms" = list(func = updatePrettyCheckboxGroup, id = "phenotype", selected = TRUE, split = TRUE)
        )
      } else {
        update_mapping <- list(
          "Inheritance" = list(func = updateSelectInput, id = "inher", selected = TRUE),
          "Annotation" = list(func = updatePrettyCheckboxGroup, id = "sv_conseq_checkboxes", selected = TRUE, split = TRUE),
          "gnomADv4 AF" = list(func = updateSelectInput, id = "sv_af", selected = TRUE, as_numeric = TRUE),
          "SV type" = list(func = updatePrettyCheckboxGroup, id = "sv_features_checkboxes", selected = TRUE, split = TRUE),
          "Min SV Length" = list(func = updateNumericInput, id = "min_svlen", value = TRUE, as_numeric = TRUE),
          "Max SV Length" = list(func = updateNumericInput, id = "max_svlen", value = TRUE, as_numeric = TRUE),
          "Affected only" = list(func = updateMaterialSwitch, id = "sv_affected_switch", value = TRUE, as_logical = TRUE),
          "Allele balance" = list(func = updateSliderInput, id = "sv_allele_balance", value = TRUE, as_numeric = TRUE),
          "Genotype quality" = list(func = updateSliderInput, id = "sv_genotype_quality", value = TRUE, as_numeric = TRUE),
          "Filter value" = list(func = updateSelectInput, id = "sv_pass_variants", selected = TRUE),
          "PanelApp Genes" = list(func = updateSelectInput, id = "panelapp", selected = TRUE, split = TRUE),
          "HPO Terms" = list(func = updatePrettyCheckboxGroup, id = "phenotype", selected = TRUE, split = TRUE)
        )
      }
      
      # If search_params is NULL or empty string, clear selections
      if (is.null(search_params) || length(search_params) == 0) {
        search_params <- default_values
      }
      
      for (param in names(update_mapping)) {
        if (!is.null(search_params[[param]])) {
          update_info <- update_mapping[[param]]
          value <- search_params[[param]]
          
          # Convert value if necessary
          if (!is.null(update_info$as_numeric)) value <- as.numeric(value)
          if (!is.null(update_info$as_logical)) value <- as.logical(value)
          if (!is.null(update_info$split)) value <- unlist(strsplit(value, ";"))
          
          if (param %in% c("Annotation", "Pathogenicity") && is.null(value)) {
            value <- character(0)
          }
          
          # Apply updates explicitly based on argument type
          if (!is.null(update_info$selected)) {
            update_info$func(session, update_info$id, selected = value)
          } else if (!is.null(update_info$value)) {
            update_info$func(session, update_info$id, value = value)
          }
        } 
      }
    }
    
    
    # Handle search selection
    observeEvent(input$pre_saved_search, {
      selected_search <- input$pre_saved_search
      #if (is.null(selected_search) || selected_search == "") return()
      
      print(paste("Loading pre-saved search:", selected_search))  # Debugging print
      search_params <- available_searches()[[selected_search]]
      update_search_params(search_params, session)
    })
    
    observeEvent(input$sv_pre_saved_search, {
      selected_search <- input$sv_pre_saved_search
      #if (is.null(selected_search) || selected_search == "") return()
      
      print(paste("Loading pre-saved search:", selected_search))  # Debugging print
      search_params <- sv_available_searches()[[selected_search]]
      update_search_params(search_params, session, type="sv")
    })
    
    observeEvent(input$pathogenicity, {
      if (input$pathogenicity == "Pathogenic/Likely pathogenic") {
        select <- c("Pathogenic", "Likely pathogenic")
      } else if (input$pathogenicity == "Not benign") {
        select <- c("Pathogenic", "Likely pathogenic", "VUS", "Conflicting")
      } else if (input$pathogenicity == "") {
        select <- NULL
      } else {
        return
      }
      updatePrettyCheckboxGroup(session, "clinvar_checkboxes", selected = select)
    })
    
    updateAnnotationSelection <- function(annotation_value,checkboxes_id,session) {
      selected_values <- switch(
        annotation_value,
        "High impact" = c("Stop gained", "Start lost", "Stop lost", "Splice variant", "Frameshift variant"),
        "Moderate to high impact" = c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                                      "Frameshift variant", "Missense variant", "In-frame variant"),
        NULL  # Default case
      )
      updatePrettyCheckboxGroup(session, checkboxes_id, selected = selected_values)
      #return(selected_values)
    }
    
    observeEvent(input$annotation, {
      updateAnnotationSelection(input$annotation, "conseq_checkboxes", session)
    })
    
    observeEvent(input$sv_annotation, {
      updateAnnotationSelection(input$sv_annotation, "sv_conseq_checkboxes", session)
    })

    observeEvent(input$panelapp, {
      tmp <- panel_app_genes[Level4 %in% input$panelapp]
      green_genes(sort(unique(tmp[Sources == "Green", Entity_Name])))
      red_genes(sort(unique(tmp[Sources == "Red", Entity_Name])))
      amber_genes(sort(unique(tmp[Sources == "Amber", Entity_Name])))
      unclassified_genes(sort(unique(tmp[!(Sources %in% c("Green", "Red", "Amber")),
                                         Entity_Name])))
    }, ignoreNULL = FALSE)

    # Render UI outputs for each gene category
    output$green_genes <- renderText({ paste(green_genes(), collapse = "; ") })
    output$red_genes <- renderText({ paste(red_genes(), collapse = "; ") })
    output$amber_genes <- renderText({ paste(amber_genes(), collapse = "; ") })
    output$unclassified_genes <- renderText({
      paste(unclassified_genes(), collapse = "; ")
    })

    ##################### Phenotype

    # Add multiple IDs to the phenotype list
    observeEvent(input$phenotype_add, {
      new_ids <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))
      new_ids <- trimws(new_ids)
      print(new_ids)

      if (length(new_ids) > 0) {
        current_ids <- phenos()
        valid_new_ids <- new_ids[new_ids %in% phenotype_data$hpo_id & !(new_ids %in% current_ids)]

        if (length(valid_new_ids) > 0) {
          phenos(c(current_ids, valid_new_ids))
          updateTextInput(session, "phenotype_var", value = "")
        }
      }
    })


    observeEvent(input$phenotype_remove, {
      current_ids <- phenos()
      ids_to_remove <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))  # Split by spaces, commas, or semicolons
      ids_to_remove <- ids_to_remove[ids_to_remove %in% current_ids]

      if (length(ids_to_remove) > 0) {
        ids_to_remove <- trimws(ids_to_remove)  # Remove leading/trailing spaces
        ids_to_remove <- ids_to_remove[ids_to_remove != ""]  # Remove empty strings

        # Remove the terms that are in the current list
        updated_ids <- setdiff(current_ids, ids_to_remove)

        # Update the current phenotype terms list
        phenos(updated_ids)

        # Clear the input field
        updateTextInput(session, "phenotype_var", value = "")
      }
    })

    # Display the list of HPO terms
    output$phenotype <- renderText({
      paste(phenos(), collapse="; ")
    })

    hide_spinner()

    ##################### Filtering

    observeEvent(input$apply_filter, {

      showNotification("Filtering...", duration = NULL, id = ns("notify_filter"),
                       type = "message")

      # Define filters
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
        genotype_quality_value = input$genotype_quality,
        allele_balance_value = input$allele_balance,
        hpo_terms_list = phenos()
      )

      snv_total_time <- system.time({
        snv_filtered_data <- snv_filter_dataset(dataset[CATEGORY=="SNV & Indel"],snv_filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data)
      })
      cat(paste("Total execution time:", format_time(snv_total_time), "\n"))
      
      # Define filters for SV
      sv_filters <- list(
        inheritance_filter = input$inher,
        panelapp_filter = input$panelapp,
        annotation_filter = input$sv_conseq_checkboxes,
        sv_features = input$sv_features_checkboxes,
        min_svlen = input$min_svlen,
        max_svlen = input$max_svlen,
        genotype_quality_value = input$sv_genotype_quality,
        allele_balance_value = input$sv_allele_balance,
        af_value = as.numeric(input$sv_af),
        hpo_terms_list = phenos()
      )
      
      # Filter SVs
      sv_total_time <- system.time({
        sv_filtered_data <- sv_filter_dataset(dataset[CATEGORY == "SV"],sv_filters,pedigree, allele_tab, panel_app_genes, vep_consequences, phenotype_data)
      })
      cat(paste("Total execution time:", format_time(sv_total_time), "\n"))
      
      all_filtered_data <- rbind(snv_filtered_data,sv_filtered_data)
      
      if (input$inher=="Compound Heterozygous") {
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
      }

      # Handle PRIORITY and NOTES
      filtered_data <- data.table(filtered_table_output())

      if (!is.null(filtered_data) && all(c("PRIORITY", "NOTES") %in% colnames(filtered_data))) {
        # Extract flagged rows
        current_flagged_rows <- filtered_data[,
          .(ID, CURRENT_PRIORITY=PRIORITY,CURENT_NOTES=NOTES)
        ]

        # Check for changes in flagged rows
        previous_flagged_rows <- flagged_rows_reactive()
        if (!is.null(previous_flagged_rows)) {
          # Identify new or updated rows
          merged_flagged_rows <- merge(
            previous_flagged_rows, current_flagged_rows,
            by = "ID", all = TRUE)
        }
        current_flagged_rows <- merged_flagged_rows[,.(ID,
            PRIORITY = fifelse(is.na(PRIORITY) | (PRIORITY != CURRENT_PRIORITY & !is.na(CURRENT_PRIORITY)), CURRENT_PRIORITY, PRIORITY),
            NOTES = fifelse(is.na(NOTES) | (NOTES != CURENT_NOTES & !is.na(CURENT_NOTES)), CURENT_NOTES, NOTES))]

        # Update flagged rows reactive value
        flagged_rows_reactive(copy(current_flagged_rows))

        # Merge flagged rows back into the filtered data
        all_filtered_data <- merge(current_flagged_rows, all_filtered_data, by = "ID", all.y = TRUE)
        all_filtered_data[is.na(PRIORITY), PRIORITY := 0]
        all_filtered_data[PRIORITY > 0, PRIORITYFlag := TRUE]
        all_filtered_data[PRIORITY < 0, PRIORITYFlag := FALSE]
      } else {
        # Fallback if no PRIORITY or NOTES columns exist
        all_filtered_data <- data.table(PRIORITY = 0, NOTES = "", all_filtered_data)
      }

      filtered_table_output(copy(all_filtered_data))

      removeNotification(ns("notify_filter"))
    }) 

    return(filtered_table_output)
  })
}
