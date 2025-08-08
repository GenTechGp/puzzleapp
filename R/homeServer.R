initializeSampleObjects <- function() {
  objects_to_check <- c(
    "sample", "processed_data", "pedigree_data", "panel_app_genes", "coverage_data",
    "somalier", "vep_consequences", "panel_app", "panel_app_vars",
    "snvs_vcf", "svs_vcf", "bam_files", "phenotype_data"
  )
  for (obj in objects_to_check) {
    if (!exists(obj, envir = .GlobalEnv)) {
      assign(obj, NULL, envir = .GlobalEnv)
    }
  }
}

parse_config <- function(config_path) {
  # Check if the YAML configuration file exists
  if (!file.exists(config_path)) {
    stop(paste("Configuration file not found at:", config_path))
  }
  
  # Read YAML configuration
  config <- tryCatch({
    yaml::read_yaml(config_path)
  }, error = function(e) {
    stop(paste("Error reading YAML file:", e$message))
  })
  
  # Validate samples exist
  if (is.null(config$samples) || length(config$samples) == 0) {
    stop("Error: No samples found in the configuration file.")
  }
  
  # Convert the list of samples into a data.table
  pedigree_data <- tryCatch({
    rbindlist(lapply(config$samples, as.data.table), fill = TRUE)
  }, error = function(e) {
    stop("Error processing pedigree data:", e$message)
  })
  
  # Validate coverage field exists before accessing it
  if (!"coverage" %in% names(pedigree_data)) {
    stop("Error: 'coverage' field is missing in pedigree data.")
  }
  
  # BAM files list
  bam_files <- pedigree_data$bam
  
  # Error handling for coverage paths
  valid_coverage_paths <- sapply(pedigree_data$coverage, function(path) {
    if (!is.null(path)) file.exists(path) else FALSE
  })
  
  if (any(!valid_coverage_paths)) {
    warning("Some coverage file paths are missing or invalid. Setting coverage_data to NULL.")
    coverage_data <- NULL
  } else {
    # Coverage data with added sample ID column
    coverage_data <- rbindlist(lapply(1:nrow(pedigree_data), function(i) {
      sample_id <- pedigree_data$sample_id[i]
      coverage_path <- pedigree_data$coverage[i]
      
      if (!file.exists(coverage_path)) return(NULL)  # Skip if file is missing
      
      # Read the coverage data and add a new column for the sample ID
      coverage_dt <- fread(coverage_path, header = TRUE)
      coverage_dt[, sample_id := sample_id]  # Add sample ID column
      
      # Filter region-based coverage for RF samples
      if (grepl("-RF-", sample_id)) {
        coverage_dt <- coverage_dt[grepl("_region$", chrom)]
        coverage_dt[, chrom := sub("_region$", "", chrom)]
      }
      
      return(coverage_dt)
    }), fill = TRUE)
    
    # Standardize coverage data
    if (!is.null(coverage_data)) {
      coverage_data <- coverage_data[, .(CHROM = chrom, AVERAGE_COVERAGE = mean, SAMPLE = sample_id)]
      coverage_data <- coverage_data[!str_detect(CHROM, "_")]  # Remove unwanted chromosomes
    }
  }
  
  # Extract required paths with NULL checks
  extract_path <- function(path) if (!is.null(path)) path else stop(paste("Error: Missing path for", deparse(substitute(path))))
  
  snvs_vcf <- extract_path(config$paths$snvs_vcf)
  snvs_tsv <- extract_path(config$paths$snvs_tsv)
  svs_vcf <- extract_path(config$paths$svs_vcf)
  svs_tsv <- extract_path(config$paths$svs_tsv)
  panel_app <- extract_path(config$dependencies$panel_app)
  vep_consequences <- extract_path(config$dependencies$vep_consequences)
  phenotype_data <- extract_path(config$dependencies$phenotype_data)
  
  # Validate required file dependencies (excluding SNV/SV files for now)
  required_paths <- list(
    panel_app = panel_app,
    vep_consequences = vep_consequences,
    phenotype_data = phenotype_data
  )
  
  # Ensure at least one pair (SNVs or SVs) exists and is valid
  snv_files_exist <- !is.null(snvs_vcf) && file.exists(snvs_vcf) &&
    !is.null(snvs_tsv) && file.exists(snvs_tsv)
  
  sv_files_exist <- !is.null(svs_vcf) && file.exists(svs_vcf) &&
    !is.null(svs_tsv) && file.exists(svs_tsv)
  
  if (!(snv_files_exist || sv_files_exist)) {
    stop("Error: At least one set of SNV (snvs_vcf & snvs_tsv) or SV (svs_vcf & svs_tsv) files must be provided.")
  }
  
  # Add only the existing files to the required paths for further validation
  if (snv_files_exist) {
    required_paths$snvs_vcf <- snvs_vcf
    required_paths$snvs_tsv <- snvs_tsv
  }
  if (sv_files_exist) {
    required_paths$svs_vcf <- svs_vcf
    required_paths$svs_tsv <- svs_tsv
  }
  
  # Check for missing required files
  missing_files <- names(required_paths)[!sapply(required_paths, file.exists)]
  if (length(missing_files) > 0) {
    stop(paste("Required files not found:", paste(missing_files, collapse = ", ")))
  }
  
  # Return a list containing all parsed information
  return(list(
    config_path = config_path,
    pedigree_data = pedigree_data,
    bam_files = bam_files,
    coverage_data = coverage_data,
    snvs_vcf = snvs_vcf,
    snvs_tsv = snvs_tsv,
    svs_vcf = svs_vcf,
    svs_tsv = svs_tsv,
    panel_app = panel_app,
    vep_consequences = vep_consequences,
    phenotype_data = phenotype_data
  ))
}

metaServer <- function(output, ns, pedigree, num_individuals) {
  
  cat(sprintf("[HomeServer][metaSever] Update pedigree table\n"))
  
  # Ensure that pedigree has at least num_individuals rows
  if (nrow(pedigree) < num_individuals) {
    additional_rows <- num_individuals - nrow(pedigree)
    empty_rows <- data.frame(
      sample_id = rep("", additional_rows),
      kinship = NA_character_,
      status = NA_character_,
      sex = NA_character_,
      bam = NA_character_,
      coverage = NA_character_,
      stringsAsFactors = FALSE
    )
    pedigree <- rbind(pedigree, empty_rows)
  }
  

  
  peditab <- pedigree[, 1:4]
  status_opts <- c("affected", "unaffected","NA")
  sex_opts <- c("male", "female", "NA")
  kinship_opts <- c("proband", "mother", "father", "sibling","NA")
  
  # Ensure peditab has all required columns
  required_cols <- c("sample_id", "kinship", "status", "sex", "bam", "coverage")
  missing_cols <- setdiff(required_cols, names(pedigree))
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      pedigree[[col]] <- NA_character_
    }
  }

  for (i in seq_len(dim(pedigree)[1])) {
    
    # Sample ID text input
    sample_id <- paste0("sample_id_", pedigree$sample_id[i])
    sample_selected <- pedigree$sample_id[i]
    peditab$sample_id[i] <- as.character(textInput(ns(sample_id), NULL, value = sample_selected,width="100%"))
    
    # Status dropdown
    status_id = paste0("status_", pedigree$sample_id[i])
    #status_selected = pedigree$status[i]
    status_selected = ifelse(is.na(pedigree$status[i]), "NA", pedigree$status[i])
    peditab$status[i] <- as.character(selectInput(ns(status_id), NULL, status_opts, status_selected,width="100%"))
    
    # Sex dropdown
    sex_id <- paste0("sex_", pedigree$sample_id[i])
    sex_selected <- ifelse(is.na(pedigree$sex[i]), "NA", pedigree$sex[i])
    peditab$sex[i] <- as.character(selectInput(ns(sex_id), NULL, sex_opts, selected = sex_selected,width="100%"))
    
    # Kinship dropdown
    kinship_id <- paste0("kinship_", pedigree$sample_id[i])
    kinship_selected <- pedigree$kinship[i]
    kinship_selected <- ifelse(is.na(pedigree$kinship[i]), "NA", pedigree$kinship[i])
    peditab$kinship[i] <- as.character(selectInput(ns(kinship_id), NULL, kinship_opts, selected = kinship_selected,width="100%"))
    
    # BAM path text input
    bam_id <- paste0("bam_", pedigree$sample_id[i])
    bam_selected <- pedigree$bam[i]
    peditab$bam[i] <- as.character(textInput(ns(bam_id), NULL, value = ifelse(is.na(bam_selected), "", bam_selected),width = "100%"))
    
    # Coverage text input
    coverage_id <- paste0("coverage_", pedigree$sample_id[i])
    coverage_selected <- pedigree$coverage[i]
    peditab$coverage[i] <- as.character(textInput(ns(coverage_id), NULL, value = ifelse(is.na(coverage_selected), "", coverage_selected),width="100%"))
    
  }
  names(peditab) <- c("Sample ID", "Kinship", "Status", "Sex", "BAM", "Coverage")
  output$meta <- renderTable(peditab, sanitize.text.function = function(x) x, width = "100%")

}


homeServer <- function(id,reload_trigger,processed_colnames,pref) {
  moduleServer(id, function(input, output, session) {
    
    cat(sprintf("[HomeServer] Module initialized\n"))

    ns <- session$ns
    
    app_dir <- getwd()
    data_dir <- paste0(app_dir, "/data")
    outdir <- Sys.getenv("OUTDIR")
    if (outdir == "")
      outdir <- "."
    
    colnames_options <- fread(sprintf("%s/colnames_options.tsv",data_dir),header=TRUE)
    
    config_data <- reactiveVal(NULL)
    
    if (exists("config_path", envir = .GlobalEnv)) {
      updateTextInput(session, "yaml_path", value = config_path)
    }
    
    config_initialised <- reactiveVal(FALSE)
    observe({
      if (!config_initialised() && is.null(config_data()) &&
          exists("config_path", envir = .GlobalEnv) &&
          file.exists(config_path)) {
        cat("[HomeServer] Initialising config_data from existing config_path\n")
        parsed_data <- parse_config(config_path)
        config_data(parsed_data)
        
        # Also initialise pedigree + metaServer if needed
        if (!exists("pedigree", envir = .GlobalEnv)) {
          pedigree <<- parsed_data$pedigree_data
        }
        updateNumericInput(session, "num_individuals", value = nrow(pedigree))
        metaServer(output, ns, pedigree, num_individuals = nrow(pedigree))
        config_initialised(TRUE)
      }
    })
    
    rebuild_pedigree <- reactive({
      req(exists("pedigree", envir = .GlobalEnv))
      req(pedigree$sample_id)
      
      data.table(
        sample_id = sapply(pedigree$sample_id, function(id) input[[paste0("sample_id_", id)]]),
        kinship   = sapply(pedigree$sample_id, function(id) input[[paste0("kinship_", id)]]),
        status    = sapply(pedigree$sample_id, function(id) input[[paste0("status_", id)]]),
        sex       = sapply(pedigree$sample_id, function(id) input[[paste0("sex_", id)]]),
        bam       = sapply(pedigree$sample_id, function(id) input[[paste0("bam_", id)]]),
        coverage  = sapply(pedigree$sample_id, function(id) input[[paste0("coverage_", id)]])
      )
    })
    
    observe({
      cat("[HomeServer] Checking existing environment variables to populate paths\n")
      
      paths_mapping <- list(
        snvs_vcf = "snvs_vcf_path",
        svs_vcf = "sv_vcf_path",
        snvs_tsv = "snvs_tsv_path",
        svs_tsv = "svs_tsv_path",
        panel_app_tsv = "panelapp_path",
        phenotype_data_tsv = "hpo_phenotype_path"
      )
      
      lapply(names(paths_mapping), function(var) {
        if (exists(var, envir = .GlobalEnv)) {
          val <- get(var, envir = .GlobalEnv)
          if (!is.null(val)) {
            updateTextInput(session, inputId = paths_mapping[[var]], value = val)
          }
        }
      })
    })
    
    observeEvent(input$load_yaml, {
      
      cat(sprintf("[HomeServer] Observing changes in input$yaml_path\n"))
      
      config_path <- input$yaml_path
      
      if (is.null(config_path) || !nzchar(config_path) || !file.exists(config_path)) {
        showNotification(paste("YAML file does not exist at path:", config_path), type = "error")
        return()
      }
      
      assign("config_path", config_path, envir = .GlobalEnv)
      
      # Define a mapping of config_data keys to UI input IDs
      paths_mapping <- list(
        snvs_vcf = "snvs_vcf_path",
        svs_vcf = "sv_vcf_path",
        snvs_tsv = "snvs_tsv_path",
        svs_tsv = "svs_tsv_path",
        panel_app = "panelapp_path",
        phenotype_data = "hpo_phenotype_path"
      )
      
      parsed_data <- tryCatch({
        parse_config(config_path)
      }, error = function(e) {
        showNotification(paste("Invalid YAML file:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(parsed_data)) return()
      
      config_data(parsed_data)
    
      # Iterate over the mapping and update inputs dynamically
      lapply(names(paths_mapping), function(key) {
        if (!is.null(parsed_data[[key]])) {
          updateTextInput(session, paths_mapping[[key]], value = parsed_data[[key]])
        }
      })
        
      # Update num_individuals if pedigree_data is available
      if (!is.null(parsed_data$pedigree_data)) {
        pedigree <<- parsed_data$pedigree_data
        updateNumericInput(session, "num_individuals", value = nrow(pedigree))
        metaServer(output, ns, pedigree, num_individuals = nrow(pedigree))
      }
      # if (!is.null(parsed_data$pedigree_data)) {
      #   if (!is.null(parsed_data$pedigree_data)) {
      #     pedigree <<- parsed_data$pedigree_data
      #     updateNumericInput(session, "num_individuals", value = nrow(pedigree))
      #     metaServer(output, ns, pedigree, num_individuals = nrow(pedigree))
      #   }
      # }
    }, ignoreInit = TRUE)
    
    observeEvent(input$clear_inputs, {
      cat("[HomeServer] Clear Inputs button pressed\n")
      
      paths_mapping <- list(
        snvs_vcf_path = "", sv_vcf_path = "",
        snvs_tsv_path = "", svs_tsv_path = "",
        panelapp_path = "", hpo_phenotype_path = ""
      )
      lapply(names(paths_mapping), function(input_id) updateTextInput(session, input_id, value = paths_mapping[[input_id]]))
      updateTextInput(session, "yaml_path", value = "")
      updateNumericInput(session, "num_individuals", value = 1)
      
      clearSampleObjects()
      
      pedigree <<- data.frame(
        sample_id = character(),
        kinship = character(),
        status = character(),
        sex = character(),
        bam = character(),
        coverage = character(),
        stringsAsFactors = FALSE
      )
      
      initializeSampleObjects()
      
      config_data(NULL)

      metaServer(output, ns, pedigree, num_individuals = 1)
      showNotification("Inputs cleared.", type = "message")
    })
    
    observe({
      req(config_data())
      updated <- rebuild_pedigree()
      
      config <- config_data()
      if (!identical(config$pedigree_data, updated)) {
        cat(sprintf("[HomeServer] Updating the pedigree table\n"))
        #print(updated)
        pedigree_data <<- updated
        config$pedigree_data <- updated
        isolate(config_data(config))
      }
    })
    
    
    observe({
      req(input$num_individuals)
      if (!exists("pedigree", envir = .GlobalEnv)) {
        cat(sprintf("[HomeServer] Initialising the pedigree table\n"))
        pedigree <<- data.frame(
          sample_id = character(),
          kinship = character(),
          status = character(),
          sex = character(),
          bam = character(),
          coverage = character(),
          stringsAsFactors = FALSE
        )
      }
      num_individuals <- max(nrow(pedigree), input$num_individuals)
      #print("observer")
      #print(pedigree)
      metaServer(output, ns, pedigree, num_individuals = num_individuals)
    })

    
    observe({
      
      #req(processed_colnames())
      cat(sprintf("[HomeServer] Updating pref reactive variable\n"))
      
      resolve_colnames <- function(selected, available) {
        resolved <- unlist(sapply(selected, function(col) {
          grep(paste0("^", col, "$|^", col, "_[0-9]+$"), available, value = TRUE)
        }))
        return(resolved)
      }
      additional_column_names <- c(
        "PRIORITY", "NOTES", "INHERITANCE", "PANEL_APP",
        "HPO_ID", "HPO_COUNT", "spliceai_override",
        "clinvar_override", "PRIORITYFlag"
      )
      selected_pref <- resolve_colnames(input$variants_preferences, c(processed_colnames(),additional_column_names))
      pref$variants <- isolate(selected_pref)
      pref$panelapp <- isolate(input$panelapp_preferences)
      pref$phenotype <- isolate(input$phenotype_preferences)
    })
    
    observe({
      cat(sprintf("[HomeServer] Checking preferences\n"))
      pref_file <- file.path(outdir, "preferences", "colnames_preferences.tsv")
      
      # Default values
      variants_default <- c("ID", "PRIORITY", "NOTES", "GT", "CONSEQUENCE", "GENE_SYMBOL", "AF", "N_HOM_ALT", 
                            "SpliceAI_pred", "CLINVAR", "REVEL", "SIFT", "PolyPhen", "am_class", "am_pathogenicity", 
                            "CADD_PHRED", "CADD_RAW", "PANEL_APP", "INHERITANCE")
      
      panelapp_default <- c("Entity_Name", "Mode_Of_Inheritance", "Level4", "Sources")
      
      phenotype_default <- c("disease_id", "hpo_id", "gene_symbol", "hpo_name", "ncbi_gene_id")
      
      # Read preferences if the file exists, otherwise use defaults
      if (file.exists(pref_file)) {
        colnames_preferences <- fread(pref_file, header = TRUE)
        
        get_preferences <- function(table_name, default_values) {
          row <- colnames_preferences[Table == table_name, colNames]
          if (length(row) > 0) unlist(strsplit(row, ";")) else character(0)
        }
        
      } else {
        get_preferences <- function(table_name, default_values) default_values
      }
      
      # Update dropdowns with either stored preferences or default choices
      update_dropdown <- function(table_name, input_id, default_values) {
        colnames_string <- colnames_options[Table == table_name, colNames]
        colnames_vector <- unlist(strsplit(colnames_string, ";"))
        
        selected_values <- get_preferences(table_name, default_values)
        
        updateSelectizeInput(session, input_id, choices = c("", colnames_vector), selected = selected_values)
      }
      
      update_dropdown("Variants", "variants_preferences", variants_default)
      update_dropdown("PanelApp", "panelapp_preferences", panelapp_default)
      update_dropdown("Phenotype", "phenotype_preferences", phenotype_default)
    })
    
    observeEvent(input$update_preferences, {
      cat(sprintf("[HomeServer] Updating preferences\n"))
      pref_dir <- file.path(outdir, "preferences")
      
      # Ensure the preferences directory exists
      if (!dir.exists(pref_dir)) {
        dir.create(pref_dir, recursive = TRUE)
      }
      
      # Create a data.table to store preferences
      colnames_preferences <- data.table(
        Table = c("Variants", "PanelApp", "Phenotype"),
        colNames = c(
          paste0(input$variants_preferences, collapse = ";"),
          paste0(input$panelapp_preferences, collapse = ";"),
          paste0(input$phenotype_preferences, collapse = ";")
        )
      )
      # Save to file
      fwrite(colnames_preferences, file.path(pref_dir, "colnames_preferences.tsv"), sep = "\t", quote = FALSE)
      showNotification("Preferences saved successfully!", type = "message")
    })
    
    # Helper function to clear existing objects
    clearSampleObjects <- function() {
      objects_to_clear <- c(
        "sample","config_path","processed_data", "pedigree_data", "pedigree", "panel_app_genes", "coverage_data",
        "somalier", "vep_consequences", "preselected_vars", "panel_app", "panel_app_vars",
        "snvs_vcf", "svs_vcf", "bam_files", "phenotype_data","snvs_tsv","svs_tsv","phenotype_data_tsv","panel_app_tsv"
      )
      for (obj in objects_to_clear) {
        if (exists(obj, envir = .GlobalEnv)) {
          rm(list = obj, envir = .GlobalEnv)
        }
      }
    }

    # Observe sample path submission
    observeEvent(input$load_sample, {
      req(config_data())
      cat(sprintf("[HomeServer] Loading sample\n"))
      clearSampleObjects()
      numeberThreads <- 32

      # Load the new sample
      showNotification("Loading sample...", id = ns("notify_load"), type = "message",duration=NULL)
      #load(sample_path, envir = .GlobalEnv) # Adjust file name/path as needed

      safe_extract <- function(data, key) {
        if (!is.null(data[[key]])) return(data[[key]]) else return(NULL)
      }
      
      #print(config_data())

      vep_consequences <- safe_extract(config_data(), "vep_consequences")
      vep_consequences <- fread(vep_consequences,header=TRUE)
      assign("vep_consequences",vep_consequences, envir = .GlobalEnv)

      phenotype_data <- safe_extract(config_data(), "phenotype_data")
      phenotype_data <- fread(phenotype_data,nThread=numeberThreads,header=TRUE)
      assign("phenotype_data",phenotype_data, envir = .GlobalEnv)


      assign("config_path", safe_extract(config_data(), "config_path"), envir = .GlobalEnv)
      assign("pedigree_data", safe_extract(config_data(), "pedigree_data"), envir = .GlobalEnv)
      assign("coverage_data", safe_extract(config_data(), "coverage_data"), envir = .GlobalEnv)
      assign("snvs_vcf", safe_extract(config_data(), "snvs_vcf"), envir = .GlobalEnv)
      assign("svs_vcf", safe_extract(config_data(), "svs_vcf"), envir = .GlobalEnv)
      assign("snvs_tsv", safe_extract(config_data(), "snvs_tsv"), envir = .GlobalEnv)
      assign("svs_tsv", safe_extract(config_data(), "svs_tsv"), envir = .GlobalEnv)
      assign("panel_app_tsv", safe_extract(config_data(), "panel_app"), envir = .GlobalEnv)
      assign("phenotype_data_tsv", safe_extract(config_data(), "phenotype_data"), envir = .GlobalEnv)
      assign("pedigree", pedigree_data, envir = .GlobalEnv)
      assign("bam_files", pedigree_data$bam, envir = .GlobalEnv)
      print(pedigree_data)


      # Process PanelApp genes
      panel_app_path <- safe_extract(config_data(), "panel_app")
      if (!is.null(panel_app_path) && file.exists(panel_app_path)) {
        panel_app <- fread(panel_app_path, nThread=numeberThreads, header = TRUE)

        if (ncol(panel_app) >= 8) {  # Ensure there are enough columns
          panel_app_genes <- panel_app[,c(1,4,5,8)]
          panel_app_genes[, Sources := str_extract(Sources, "Expert Review ([[:alnum:].]+)")]
          panel_app_genes[, Sources := str_remove(Sources, "Expert Review ")]
          panel_app_genes[, Model_Of_Inheritance := tstrsplit(panel_app$Model_Of_Inheritance, ",")[[1]]]
          panel_app_genes <- as.data.table(panel_app_genes)

          assign("panel_app",panel_app, envir = .GlobalEnv)
          assign("panel_app_genes", panel_app_genes, envir = .GlobalEnv)
        } else {
          warning("PanelApp file does not have the expected number of columns. Skipping processing.")
          assign("panel_app",NULL, envir = .GlobalEnv)
          assign("panel_app_genes", NULL, envir = .GlobalEnv)
        }
      } else {
        warning("PanelApp file is missing or does not exist. Skipping processing.")
        assign("panel_app",NULL, envir = .GlobalEnv)
        assign("panel_app_genes", NULL, envir = .GlobalEnv)
      }

      # Initialize processed_data list
      processed_list <- list()

      # Read SNVs TSV
      snvs_tsv <- safe_extract(config_data(), "snvs_tsv")
      if (!is.null(snvs_tsv) && file.exists(snvs_tsv)) {
        dt_snvs <- fread(snvs_tsv,nThread=numeberThreads)
        processed_list[["snvs"]] <- dt_snvs
      } else {
        warning("SNVs TSV file missing. SNVs data will not be included.")
      }

      # Read SVs TSV
      svs_tsv <- safe_extract(config_data(), "svs_tsv")
      if (!is.null(svs_tsv) && file.exists(svs_tsv)) {
        dt_svs <- fread(svs_tsv,nThread=16)
        processed_list[["svs"]] <- dt_svs
      } else {
        warning("SVs TSV file missing. SVs data will not be included.")
      }

      # Combine SNVs and SVs if both exist
      if (length(processed_list) > 0) {
        processed_data <- rbindlist(processed_list, use.names = TRUE, fill = TRUE)
        assign("processed_data", processed_data, envir = .GlobalEnv)
      } else {
        warning("No valid SNVs or SVs data found. Processed data will be empty.")
        assign("processed_data", NULL, envir = .GlobalEnv)
      }

      if (exists("processed_data", envir = .GlobalEnv) && !is.null(processed_data)) {
        processed_colnames(colnames(processed_data))
      }

      # Clear DataTables cache in the browser
      session$sendCustomMessage(type = "clearDataTablesCache", list())

      # Trigger a full app reload
      #reload_trigger(runif(1))
      initializeSampleObjects()
      session$sendCustomMessage("reloadPage", list())
      removeNotification(ns("notify_load"))
      showNotification("Sample loaded successfully!", type = "message")
    })

    return(pref)
  })
}
