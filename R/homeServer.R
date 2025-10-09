#' Home Tab Server
#'
#' @param id Module ID
#' @param shared_store A reactiveValues object to share data across modules
#' @param shared_rx A list of reactive values for cross-module communication
#' @export
#' @import shiny

home_server <- function(id, shared_store, shared_rx) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe({
    #   cat("[DEBUG] All input names:", names(reactiveValuesToList(input)), "\n")
    # })
    # a new reactive to store config samples from YAML or user input
    config_samples <- shiny::reactiveVal(list())

    clear_shared_store <- function() {
      default_dt <- shared_store$data_for_data[["[Synthetic] Boundary"]]
      shared_store$data_for_data  <- list()
      shared_store$data_for_data[["[Synthetic] Boundary"]] <- default_dt
      shared_store$original_data  <- list()
      shared_store$preferred_cols <- character(0)
      shared_store$samples <- NULL
      shared_store$pedigree <- NULL
      shared_store$panel_app_data <- NULL
      shared_store$vep_map <- NULL
      shared_store$phenotype_data <- NULL
      shared_store$vep_consequences <- NULL
      shared_store$verbose_level <- 0L
      
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
      # Clear shared_store
      clear_shared_store()
      config_samples(NULL)
      config_samples(list(list(sample_id="",kinship="unknown",status="unknown",sex="unknown",code=1,bam="",coverage="")))

      bump_version(version_type = "data", shared_rx = shared_rx)
      bump_version(version_type = "panelapp", shared_rx = shared_rx)

    })

    # Feedback to user
    # listen to shared_rx$data_version to update status
    shiny::observeEvent(shared_rx$data_version(), {
      output$status <- shiny::renderText({
        msgs <- c()
        # check if shared_store$original_data is empty list
        if (length(shared_store$original_data) == 0) {
          return("No data loaded yet.")
        }
        msgs <- c(msgs, paste("SNVs & Indels TSV loaded with", nrow(shared_store$original_data[["SNV"]]), "rows and", ncol(shared_store$original_data[["SNV"]]), "columns."))
        msgs <- c(msgs, paste("SVs TSV loaded with", nrow(shared_store$original_data[["SV"]]), "rows and", ncol(shared_store$original_data[["SV"]]), "columns."))
        paste(msgs, collapse = "\n")
      })
    })

    shiny::observeEvent(input$load_yml, {
      yml_path <- NULL

      # Determine path from upload or typed input
      if (!is.null(input$yml_path) && nzchar(input$yml_path)) {
        yml_path <- input$yml_path
      }

      # Check that we have a valid path before reading
      if (!is.null(yml_path) && length(yml_path) == 1 && file.exists(yml_path)) {
        config <- yaml::read_yaml(yml_path)
        config_samples(config$samples)

        # pre-fill TSV input if in single line
        if (!is.null(config$paths$snvs_vcf) && nzchar(config$paths$snvs_vcf)) {
          shiny::updateTextInput(session, "snvs_vcf", value = config$paths$snvs_vcf)
        }
        if (!is.null(config$paths$snvs_tsv) && nzchar(config$paths$snvs_tsv)) {
          shiny::updateTextInput(session, "snvs_tsv", value = config$paths$snvs_tsv)
        }
        if (!is.null(config$paths$svs_vcf) && nzchar(config$paths$svs_vcf)) {
          shiny::updateTextInput(session, "svs_vcf", value = config$paths$svs_vcf)
        }
        if (!is.null(config$paths$svs_tsv) && nzchar(config$paths$svs_tsv)) {
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
      clear_shared_store()
      if (nzchar(input$work_dir) == 0) {
        # shiny::showNotification("Please specify a working directory.", type = "error")
        # return()
        # use $HOME if unset
        shiny::updateTextInput(session, "work_dir", value = Sys.getenv("HOME"))
        shared_store$work_dir <- Sys.getenv("HOME")
      } else {
        shared_store$work_dir <- input$work_dir
      }
      # Store in shared_store
      collected <- collect_inputs(input)

      shared_store$data_for_data[["[Synthetic] Boundary"]] <- collected$default_dt
      shared_store$data_for_data[["SNV"]] <- collected$snvs_data
      shared_store$data_for_data[["SV"]] <- collected$svs_data
      shared_store$original_data[["SNV"]] <- data.table::copy(collected$snvs_data)
      shared_store$original_data[["SV"]] <- data.table::copy(collected$svs_data)

      shared_store$samples <- collected$samples
      shared_store$pedigree <- collected$pedigree
      shared_store$panel_app_genes <- collected$panel_app_data
      shared_store$vep_consequences <- collected$vep_consequences
      shared_store$phenotype_data <- collected$phenotype_data

      stopifnot(
        !identical(
          lobstr::obj_addr(shared_store$data_for_data[["SNV"]]),
          lobstr::obj_addr(shared_store$original_data[["SNV"]])
        )
      )

      for (msg in collected$messages) {
        shiny::showNotification(msg, type = "error")
      }

      resolve_colnames <- function(selected, available) {
        unlist(sapply(selected, function(col) {
          grep(paste0("^", col, "$|^", col, "_[0-9]+$"), available, value = TRUE)
        }))
      }
      snvs_cols <- if (!is.null(shared_store$original_data[["SNV"]])) colnames(shared_store$original_data[["SNV"]]) else character(0)
      svs_cols  <- if (!is.null(shared_store$original_data[["SV"]])) colnames(shared_store$original_data[["SV"]]) else character(0)
      available_cols <- sort(unique(c(snvs_cols, svs_cols)))
      selected_pref_variant_cols <- resolve_colnames(input$variants_preferences, available_cols)
      cat("Selected variant columns based on preferences:", selected_pref_variant_cols, "\n")
      # shared_data$pref$variants <- selected_pref_variant_cols
      # shared_data$pref$panelapp <- input$panelapp_preferences
      # shared_data$pref$phenotype <- input$phenotype_preferences

      shared_store$preferred_cols <- selected_pref_variant_cols

      bump_version(version_type = "data", shared_rx = shared_rx)
      bump_version(version_type = "panelapp", shared_rx = shared_rx)

      cat("Data loaded into shared_store.\n")
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
      # Pull samples from config_samples if available
      samples <- config_samples() %||% list()
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
