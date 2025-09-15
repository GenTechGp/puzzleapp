#' Home Tab Server
#'
#' @param id Module ID
#' @export
#' @import shiny

source("R/db_utility.R")

home_server <- function(id, shared_data, shared_store, shared_rx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe({
    #   cat("[DEBUG] All input names:", names(reactiveValuesToList(input)), "\n")
    # })

    clear_shared_data <- function(shared_data) {
      # Clear shared_data reactiveValues
      fields <- c("samples", "dependencies", "snvs_data", "svs_data", "snvs_data_filtered", "svs_data_filtered", "panel_app_data", "vep_map", "phenotype_data", "vep_consequences", "legacy_snvs_data_filtered", "legacy_svs_data_filtered", "work_dir")
      for (f in fields) shared_data[[f]] <- NULL
      shared_data$paths <- list()
      shared_data$pref <- list(variants = NULL, panelapp = NULL, phenotype = NULL, outdir = "")
    }

    # Clear inputs
    shiny::observeEvent(input$clear_inputs, {
      # Reset numeric input for Individuals
      shiny::updateNumericInput(session, "num_individuals", value = 1)
      shiny::updateTextInput(session, "yml_path", value = "")
      shiny::updateTextInput(session, "snvs_vcf", value = "")
      shiny::updateTextInput(session, "snvs_tsv", value = "")
      shiny::updateTextInput(session, "svs_vcf", value = "")
      shiny::updateTextInput(session, "svs_tsv", value = "")
      shiny::updateTextInput(session, "panel_app", value = "")
      shiny::updateTextInput(session, "vep_consequences", value = "")
      shiny::updateTextInput(session, "phenotype_data", value = "")
      shiny::updateTextInput(session, "outdir", value = "")
      # Clear shared_data
      clear_shared_data(shared_data)
      shared_data$config_samples <- NULL
      shared_data$config_samples <- list(list(sample_id="",kinship="unknown",status="unknown",sex="unknown",code=1,bam="",coverage=""))
      cat("Inputs cleared and shared_data reset.\n")

      shared_store$A <- NULL
      shared_store$B <- NULL
      bump_version(shared_rx)
      cat("[Home] Deleted datasets A and B\n")

    })

    # Feedback to user
    output$status <- shiny::renderText({
      msgs <- c()

      if (!is.null(shared_data$snvs_data)) {
        msgs <- c(msgs,                   paste("SNVs & Indels TSV loaded with",                         nrow(shared_data$snvs_data), "rows and",                         ncol(shared_data$snvs_data), "columns."))
      }
      if (!is.null(shared_data$svs_data)) {
        msgs <- c(msgs,                   paste("SVs TSV loaded with",                         nrow(shared_data$svs_data), "rows and",                         ncol(shared_data$svs_data), "columns."))
      }

      if (length(msgs) == 0) {
        "No data loaded yet."
      } else {
        paste(msgs, collapse = "\n")
      }
    })

    # Load YAML: populate shared_data with all variables
    shiny::observeEvent(input$load_yml, {
      yml_path <- NULL

      # Determine path from upload or typed input
      if (!is.null(input$yml_path) && nzchar(input$yml_path)) {
        yml_path <- input$yml_path
      }

      # Check that we have a valid path before reading
      if (!is.null(yml_path) && length(yml_path) == 1 && file.exists(yml_path)) {
        config <- yaml::read_yaml(yml_path)
        shared_data$config_samples <- config$samples

        # pre-fill TSV input
        if (!is.null(config$paths$snvs_vcf)) {
          shiny::updateTextInput(session, "snvs_vcf", value = config$paths$snvs_vcf)
        }
        if (!is.null(config$paths$snvs_tsv)) {
          shiny::updateTextInput(session, "snvs_tsv", value = config$paths$snvs_tsv)
        }
        if (!is.null(config$paths$svs_vcf)) {
          shiny::updateTextInput(session, "svs_vcf", value = config$paths$svs_vcf)
        }
        if (!is.null(config$paths$svs_tsv)) {
          shiny::updateTextInput(session, "svs_tsv", value = config$paths$svs_tsv)
        }
        if (!is.null(config$dependencies$panel_app)) {
          shiny::updateTextInput(session, "panel_app", value = config$dependencies$panel_app)
        }
        if (!is.null(config$dependencies$vep_consequences)) {
          shiny::updateTextInput(session, "vep_consequences", value = config$dependencies$vep_consequences)
        }
        if (!is.null(config$dependencies$phenotype_data)) {
          shiny::updateTextInput(session, "phenotype_data", value = config$dependencies$phenotype_data)
        }
        # Set number of Individuals based on YAML
        shiny::updateNumericInput(session, "num_individuals", value = length(config$samples))
      } else {
        shiny::showNotification("No valid YAML path provided.", type = "error")
      }
    })

    shiny::observeEvent(input$load_data, {
      if (nzchar(input$snvs_tsv) == 0 && nzchar(input$svs_tsv) == 0) {
        shiny::showNotification("No data files specified to load.", type = "error")
        return()
      }
      if (nzchar(input$work_dir) == 0) {
        shiny::showNotification("Please specify a working directory.", type = "error")
        return()
      }
      clear_shared_data(shared_data)
      # Store in shared_data
      collected <- collect_inputs(input)

      for (msg in collected$messages) {
        shiny::showNotification(msg, type = "error")
      }

      # Fill shared_data reactives
      shared_data$samples <- collected$samples
      shared_data$pedigree <- collected$pedigree
      shared_data$snvs_data <- collected$snvs_data
      shared_data$snvs_data_filtered <- collected$snvs_data
      shared_data$svs_data <- collected$svs_data
      shared_data$svs_data_filtered <- collected$svs_data
      shared_data$panel_app_genes <- collected$panel_app_data
      shared_data$vep_consequences <- collected$vep_consequences
      shared_data$phenotype_data <- collected$phenotype_data

      shared_data$work_dir <- input$work_dir

      #to support legacy start
      processed_list <- list()
      processed_list[["snvs"]] <- shared_data$snvs_data
      processed_list[["svs"]]  <- shared_data$svs_data
      processed_data <- rbindlist(processed_list, use.names = TRUE, fill = TRUE)
      assign("processed_data", processed_data, envir = .GlobalEnv)

      resolve_colnames <- function(selected, available) {
        unlist(sapply(selected, function(col) {
          grep(paste0("^", col, "$|^", col, "_[0-9]+$"), available, value = TRUE)
        }))
      }
      snvs_cols <- if (!is.null(shared_data$snvs_data)) colnames(shared_data$snvs_data) else character(0)
      svs_cols  <- if (!is.null(shared_data$svs_data)) colnames(shared_data$svs_data) else character(0)
      available_cols <- sort(unique(c(snvs_cols, svs_cols)))
      selected_pref_variant_cols <- resolve_colnames(input$variants_preferences, available_cols)
      cat("Selected variant columns based on preferences:", selected_pref_variant_cols, "\n")
      shared_data$pref$variants <- selected_pref_variant_cols
      shared_data$pref$panelapp <- input$panelapp_preferences
      shared_data$pref$phenotype <- input$phenotype_preferences
      shared_data$pref$working_dir <- shared_data$work_dir

      # shared_data$legacy_snvs_data_filtered <- shared_data$snvs_data_filtered
      # to support legacy end
      # add_row_id <- function(df) {
      #   df$.row_id <- seq_len(nrow(df))
      #   df
      # }
      # sample_base <- function(n) {
      #   n <- max(1, min(n, nrow(iris)))
      #   iris[sample.int(nrow(iris), n, replace = FALSE), , drop = FALSE]
      # }
      # base0 <- add_row_id(sample_base(30))
      # shared_store$A <- base0
      # shared_store$B <- base0


      shared_store$A <- collected$snvs_data
      shared_store$B <- collected$svs_data
      shared_store$preferred_cols <- selected_pref_variant_cols
      bump_version(shared_rx)

      cat("Data loaded into shared_data.\n")
    })

    output$samples_panel <- shiny::renderUI({
      n <- input$num_individuals
      if (is.null(n) || n < 1) n <- 1
      # Header row
      header_row <- shiny::fluidRow(
        shiny::column(1, shiny::strong("Sample ID")),
        shiny::column(1, shiny::strong("Kinship")),
        shiny::column(1, shiny::strong("Status")),
        shiny::column(1, shiny::strong("Sex")),
        shiny::column(1, shiny::strong("Code")),
        shiny::column(3, shiny::strong("BAM Path")),
        shiny::column(4, shiny::strong("Coverage Path"))
      )
      # Pull samples from shared_data if available
      samples <- shared_data$config_samples %||% list()
      sample_rows <- lapply(seq_len(n), function(i) {
        s <- if (!is.null(samples) && length(samples) >= i) samples[[i]] else list()
        shiny::fluidRow(
          shiny::column(1, shiny::textInput(ns(paste0("sample_id_", i)), label = NULL, value = s$sample_id %||% "")),
          shiny::column(1, shiny::selectInput(ns(paste0("kinship_", i)), label = NULL, choices = c("proband", "mother", "father", "sibling", "unknown"), selected = s$kinship %||% "unknown")),
          shiny::column(1, shiny::selectInput(ns(paste0("status_", i)), label = NULL, choices = c("affected", "unaffected", "unknown"), selected = s$status %||% "unknown")),
          shiny::column(1, shiny::selectInput(ns(paste0("sex_", i)), label = NULL, choices = c("female", "male", "unknown"), selected = s$sex %||% "unknown")),
          shiny::column(1, shiny::numericInput(ns(paste0("code_", i)), label = NULL, value = s$code %||% 1, min = 0)),
          shiny::column(3, shiny::textInput(ns(paste0("bam_", i)), label = NULL, value = s$bam %||% "", width = "100%")),
          shiny::column(4, shiny::textInput(ns(paste0("coverage_", i)), label = NULL, value = s$coverage %||% "", width = "100%"))
        )
      })
      shiny::tagList(header_row, sample_rows)
    })

    # Preferences management with cookies
    # Using JavaScript to read/write cookies and communicate with Shiny
    # If/when the browser sends input$cookie_prefs → dropdowns update with cookie values.
    # --- Default preferences ---
    variants_default   <- c("ID", "PRIORITY", "NOTES", "GT", "CONSEQUENCE", "GENE_SYMBOL", "AF",
                            "N_HOM_ALT", "SpliceAI_pred", "CLINVAR", "REVEL", "SIFT", "PolyPhen",
                            "am_class", "am_pathogenicity", "CADD_PHRED", "CADD_RAW",
                            "PANEL_APP", "INHERITANCE")
    panelapp_default   <- c("Entity_Name", "Mode_Of_Inheritance", "Level4", "Sources")
    phenotype_default  <- c("disease_id", "hpo_id", "gene_symbol", "hpo_name", "ncbi_gene_id")
    # --- All available columns (user can choose any of these) ---
    colnames_options <- data.table(
      Table = c("Variants", "PanelApp", "Phenotype"),
      colNames = c(
        "AD;AF;ALT;Acceptor_Gain;Acceptor_Loss;CADD_PHRED;CADD_RAW;CATEGORY;CHROM;CLINVAR;CONSEQUENCE;DP;Donor_Gain;Donor_Loss;FILTER;GENE_ID;GENE_SYMBOL;GT;HGVSc;HGVSg;HGVSp;HPO_COUNT;HPO_ID;ID;INHERITANCE;NOTES;N_HOM_ALT;PANEL_APP;POS;PRIORITY;PRIORITYFlag;PolyPhen;QUAL;REF;REVEL;SIFT;SpliceAI_pred;TRANSCRIPT;VAF;VAR_LENGTH;VAR_TYPE;alt_allele_count;am_class;am_pathogenicity;clinvar_override;spliceai_override;gnomAD_ID;CLINVAR_ID;N_Cohort",
        "Entity_Name;Level4;Model_Of_Inheritance;Sources",
        "disease_id;gene_symbol;hpo_id;hpo_name;ncbi_gene_id"
      )
    )

    # --- Utility to update one dropdown ---
    update_pref_dropdown <- function(session, table_name, input_id, defaults, cookie = NULL) {
      # all available columns for this table
      colnames_string <- colnames_options$colNames[colnames_options$Table == table_name]
      colnames_vector <- unlist(strsplit(colnames_string, ";"))

      # pick either cookie values or defaults
      selected_values <- if (!is.null(cookie) && !is.null(cookie[[table_name]])) {
        cat("[HomeServer] Loaded preferences from cookie for", table_name, ":", cookie[[table_name]], "\n")
        cookie[[table_name]]
      } else {
        cat("[HomeServer] Using defaults for", table_name, "\n")
        defaults
      }
      # update UI
      cat("[HomeServer] Updating", input_id, "with selected values:", selected_values, "\n")
      updateSelectizeInput(session, input_id, choices = c("", colnames_vector), selected = selected_values)
    }

    # --- Always show defaults on startup ---
    observe({
      update_pref_dropdown(session, "Variants",  "variants_preferences",  variants_default)
      update_pref_dropdown(session, "PanelApp",  "panelapp_preferences",  panelapp_default)
      update_pref_dropdown(session, "Phenotype", "phenotype_preferences", phenotype_default)
    })

    # --- If cookie arrives, override defaults ---
    observeEvent(input$cookie_prefs, {
      cat("[HomeServer] cookie_prefs received:", "\n")
      # print(input$cookie_prefs)   # prints the full list in readable format
      # Flatten the list of lists into list of character vectors
      prefs <- lapply(input$cookie_prefs, function(x) unlist(x))
      cat("[HomeServer] Flattened cookie_prefs keys and lengths:\n")
      for (nm in names(prefs)) {
        cat(nm, ":", length(prefs[[nm]]), "items\n")
      }
      update_pref_dropdown(session, "Variants",  "variants_preferences",  variants_default, prefs)
      update_pref_dropdown(session, "PanelApp",  "panelapp_preferences",  panelapp_default, prefs)
      update_pref_dropdown(session, "Phenotype", "phenotype_preferences", phenotype_default, prefs)
    })

    # --- Save to cookie when user clicks "Save preferences" ---
    observeEvent(input$update_preferences, {
      prefs <- list(
        Variants  = input$variants_preferences,
        PanelApp  = input$panelapp_preferences,
        Phenotype = input$phenotype_preferences
      )
      cat("[HomeServer] Saving preferences to cookie\n")
      session$sendCustomMessage("set_cookie", list(
        name = "user_prefs",
        value = prefs
      ))
      showNotification("Saved as a browser cookie!", type = "message")
    })

  })
}
