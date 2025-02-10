homeServer <- function(id,reload_trigger,processed_colnames,pref) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    app_dir <- getwd()
    data_dir <- paste0(app_dir, "/data")
    outdir <- Sys.getenv("OUTDIR")
    if (outdir == "")
      outdir <- "."
    
    colnames_options <- fread(sprintf("%s/colnames_options.tsv",data_dir),header=TRUE)
    
    observe({
      
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
      pref$variants <- resolve_colnames(input$variants_preferences, c(processed_colnames(),additional_column_names))
      pref$panelapp <- input$panelapp_preferences
      pref$phenotype <- input$phenotype_preferences
    })
    
    observe({
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
        "sample", "processed_data", "pedigree_data", "panel_app_genes", "coverage_data",
        "somalier", "vep_consequences", "preselected_vars", "panel_app", "panel_app_vars",
        "snvs_vcf", "svs_vcf", "bam_files", "phenotype_data"
      )
      for (obj in objects_to_clear) {
        if (exists(obj, envir = .GlobalEnv)) {
          rm(list = obj, envir = .GlobalEnv)
        }
      }
    }

    # Observe sample path submission
    observeEvent(input$load_sample, {
      sample_path <- input$sample_path
      print("here")

      if (!file.exists(sample_path)) {
        showNotification("Invalid path. Please check the directory and try again.", type = "error")
      } else {
        # Clear existing objects
        clearSampleObjects()

        # Load the new sample
        showNotification("Loading sample...", id = ns("notify_load"), type = "message")
        load(sample_path, envir = .GlobalEnv) # Adjust file name/path as needed

        # Additional logic for processing the sample can be added here
        print(paste("Sample loaded from:", sample_path))

        # Trigger a full app reload
        reload_trigger(runif(1))
        removeNotification(ns("notify_load"))
        showNotification("Sample loaded successfully!", type = "message")
      }
    })

    return(pref)
  })
}
