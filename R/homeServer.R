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
    status_text <- shiny::reactiveVal("No data loaded yet.")
    # observe({
    #   cat("[DEBUG] All input names:", names(reactiveValuesToList(input)), "\n")
    # })
    # a new reactive to store config samples from YAML or user input
    config_samples <- shiny::reactiveVal(list())
    paths <- shiny::reactiveValues(
      svlog_db = NULL,
      coverage_vaf_html = NULL,
      somalier_html = NULL
    )
    clear_shared_store <- function() {
      shared_store$data_for_data  <- list()
      shared_store$original_data  <- list()
      shared_store$preferred_cols <- list()
      shared_store$samples <- NULL
      shared_store$pedigree <- NULL
      shared_store$panel_app_data <- NULL
      shared_store$vep_map <- NULL
      shared_store$phenotype_data <- NULL
      shared_store$vep_consequences <- NULL
      shared_store$verbose_level <- 0L
    }

    # Clear inputs
    observeEvent(input$clear_inputs, ignoreInit = TRUE, {
      showModal(modalDialog(
        title = "Clear loaded data?",
        HTML("App will restart and all loaded data will be lost.<br>
            Consider saving your session from the Filter tab.<br>
            Are you sure you want to proceed?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_restart"), "Restart", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_restart, ignoreInit = TRUE, {
      removeModal()
      session$reload()
    })

    output$status <- shiny::renderText({
      status_text()
    })

    shiny::observeEvent(input$load_yml, {
      yml_path <- NULL

      # Determine path from upload or typed input
      if (!is.null(input$yml_path) && nzchar(input$yml_path)) {
        yml_path <- input$yml_path
      }

      # Check that we have a valid path before reading
      if (!is.null(yml_path) && length(yml_path) == 1 && file.exists(yml_path)) {
        ext <- tools::file_ext(yml_path)
        #if not .yml or .yaml, show error
        if (!ext %in% c("yml", "yaml")) {
          shiny::showNotification("Uploaded file is not a YAML file (.yml or .yaml).", type = "error")
          return()
        }
        config <- yaml::read_yaml(yml_path)

        samples <- sanitise_samples(config$samples)
        # Update reactiveVal to pre-fill sample inputs
        config_samples(samples)

        if (input$load_local_db) {
          panelapp <- load_local_db("panelapp", "all_panels.tsv")
          phenotype <- load_local_db("phenotype", "phenotype_to_genes.txt")
          config$dependencies <- list(
            panel_app = panelapp,
            phenotype_data = phenotype
          )
        }

        paths$svlog_db <- config$paths$svlog_db %||% NULL
        paths$coverage_vaf_html <- config$paths$coverage_vaf_html %||% NULL
        paths$somalier_html <- config$paths$somalier_html %||% NULL
        output$other_params_text <- renderUI({
          li_list <- Filter(
            Negate(is.null),
            list(
              if (!is.null(paths$svlog_db))
                tags$li(paste("SVLog DB:", paths$svlog_db)),

              if (!is.null(paths$coverage_vaf_html))
                tags$li(paste("Coverage html Path:", paths$coverage_vaf_html)),

              if (!is.null(paths$somalier_html))
                tags$li(paste("Somalier html Path:", paths$somalier_html))
            )
          )

          if (length(li_list) > 0) {
            tags$div(
              tags$strong("Other params loaded from yaml: "),
              tags$ul(li_list)
            )
          }
        })

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
      # disable load button to prevent multiple clicks
      status_text("Loading data...")
      shinyjs::disable("load_data")
      if (nzchar(input$snvs_tsv) == 0 && nzchar(input$svs_tsv) == 0) {
        shiny::showNotification("No data files specified to load.", type = "error")
        shinyjs::enable("load_data")
        status_text("No data loaded yet.")
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
      shared_store$sticky_work_dir <- input$sticky_work_dir
      create_work_dir(shared_store$work_dir, shared_store$sticky_work_dir)
      # Store in shared_store
      add_svlog_columns <- !is.null(paths$svlog_db)
      collected <- collect_inputs(input, add_svlog_columns = add_svlog_columns)
      if (length(collected$messages) > 0) {
        for (msg in collected$messages) {
          shiny::showNotification(msg, type = "error")
        }
        shinyjs::enable("load_data")
        status_text("No data loaded yet.")
        return()
      }
      vep_consequences_file <- load_local_db("vep_consequences", "vep_annotations.tsv")
      vep_consequences <- puzzlecore_load_vep_consequences(file = vep_consequences_file)
      vep_consequences$.row_id <- seq_len(nrow(vep_consequences))

      shared_store$data_for_data[["[SNV]_Boundary"]] <- collected$snv_default_dt
      shared_store$data_for_data[["[SV]_Boundary"]] <- collected$sv_default_dt
      shared_store$data_for_data[["SNV"]] <- collected$snvs_data
      shared_store$data_for_data[["SV"]] <- collected$svs_data
      shared_store$original_data[["SNV"]] <- data.table::copy(collected$snvs_data)
      shared_store$original_data[["SV"]] <- data.table::copy(collected$svs_data)

      shared_store$samples <- collected$samples
      shared_store$pedigree <- collected$pedigree
      shared_store$panel_app_genes <- collected$panel_app_data
      shared_store$vep_consequences <- vep_consequences
      shared_store$phenotype_data <- collected$phenotype_data
      custom_genome <- NULL
      igv_genome_name <- input$igv_genome
      use_custom_genome <- FALSE
      if (input$igv_genome == "custom (as configured in app.conf)") {
        custom_genome <- get_igv_custom_genome()
        if (is.null(custom_genome)) {
          shiny::showNotification("Custom genome is not properly configured in app.conf.", type = "error")
          shinyjs::enable("load_data")
          status_text("No data loaded yet.")
          return()
        }
        igv_genome_name <- custom_genome$name
        use_custom_genome <- TRUE
      }
      shared_store$igv_data <- list(
        snvs_vcf = collected$snvs_vcf,
        svs_vcf = collected$svs_vcf,
        igv_genome = igv_genome_name,
        custom_genome = custom_genome,
        use_custom_genome = use_custom_genome
      )

      shared_store$data_for_data[["[panel_app]_Boundary"]] <- collected$panel_app_default_dt
      shared_store$data_for_data[["panel_app"]] <- collected$panel_app_data

      shared_store$data_for_data[["[phenotype]_Boundary"]] <- collected$phenotype_default_dt
      shared_store$data_for_data[["phenotype"]] <- collected$phenotype_data

      shared_store$data_for_data[["[vep_consequences]_Boundary"]] <- vep_consequences
      shared_store$data_for_data[["vep_consequences"]] <- vep_consequences

      # todo: do a proper table schema validation
      if (!is.null(paths$svlog_db)) {
        shared_store$svlog_db <- fread(paths$svlog_db)
      }

      stopifnot(
        !identical(
          lobstr::obj_addr(shared_store$data_for_data[["SNV"]]),
          lobstr::obj_addr(shared_store$original_data[["SNV"]])
        )
      )

      resolve_colnames <- function(selected, available) {
        unlist(sapply(selected, function(col) {
          grep(paste0("^", col, "$|^", col, "_[0-9]+$"), available, value = TRUE)
        }))
      }
      snvs_cols <- if (!is.null(shared_store$original_data[["SNV"]])) colnames(shared_store$original_data[["SNV"]]) else character(0)
      svs_cols  <- if (!is.null(shared_store$original_data[["SV"]])) colnames(shared_store$original_data[["SV"]]) else character(0)
      available_cols <- sort(unique(c(snvs_cols, svs_cols)))
      selected_pref_snv_cols <- resolve_colnames(input$snv_preferences, snvs_cols)
      selected_pref_sv_cols <- resolve_colnames(input$sv_preferences, svs_cols)
      cat("Selected SNV columns based on preferences:", selected_pref_snv_cols, "\n")
      cat("Selected SV columns based on preferences:", selected_pref_sv_cols, "\n")
      # shared_data$pref$variants <- selected_pref_variant_cols
      # shared_data$pref$panelapp <- input$panelapp_preferences
      # shared_data$pref$phenotype <- input$phenotype_preferences

      shared_store$preferred_cols[["SNV"]] <- selected_pref_snv_cols
      shared_store$preferred_cols[["SV"]] <- selected_pref_sv_cols
      shared_store$preferred_cols[["panel_app"]] <- input$panelapp_preferences
      shared_store$preferred_cols[["Phenotype"]] <- input$phenotype_preferences
      
      if (paths$coverage_vaf_html %||% "" != ""){
        coverage_vaf_html_path <- paths$coverage_vaf_html
        coverage_html_dir <- dirname(coverage_vaf_html_path %||% "")
        addResourcePath("coverage_html", coverage_html_dir)
        coverage_html_name <- basename(coverage_vaf_html_path %||% "")
        shared_store$html$coverage_path <- file.path("/coverage_html", coverage_html_name)
      }
      if (paths$somalier_html %||% "" != ""){
        somalier_html_path <- paths$somalier_html
        somalier_html_dir <- dirname(somalier_html_path %||% "")
        addResourcePath("somalier_html", somalier_html_dir)
        somalier_html_name <- basename(somalier_html_path %||% "")
        shared_store$html$somalier_path <- file.path("/somalier_html", somalier_html_name)
      }

      bump_version(version_type = "data", shared_rx = shared_rx)
      bump_version(version_type = "panelapp", shared_rx = shared_rx)

      msgs <- c()
      msgs <- c(msgs, paste("SNVs & Indels TSV loaded with", nrow(shared_store$original_data[["SNV"]]), "rows and", ncol(shared_store$original_data[["SNV"]]), "columns."))
      msgs <- c(msgs, paste("SVs TSV loaded with", nrow(shared_store$original_data[["SV"]]), "rows and", ncol(shared_store$original_data[["SV"]]), "columns."))

      status_text(paste(msgs, collapse = "\n"))
      cat("Data loaded into shared_store.\n")

      shinyjs::disable("load_yml")

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
        shiny::column(5, shiny::strong("BAM Path")),
        shiny::column(2, shiny::strong("Coverage Path"))
      )
      # Pull samples from config_samples if available
      samples <- config_samples() %||% list()
      sample_rows <- lapply(seq_len(n), function(i) {
        s <- if (!is.null(samples) && length(samples) >= i) samples[[i]] else list()
        shiny::fluidRow(
          shiny::column(1, shiny::textInput(ns(paste0("sample_id_", i)), label = NULL, value = s$sample_id %||% "")),
          shiny::column(1, shiny::selectInput(ns(paste0("kinship_", i)), label = NULL, choices = v_kinships, selected = s$kinship %||% "unknown")),
          shiny::column(1, shiny::selectInput(ns(paste0("status_", i)), label = NULL, choices = v_statuses, selected = s$status %||% "unknown")),
          shiny::column(1, shiny::selectInput(ns(paste0("sex_", i)), label = NULL, choices = v_sexes, selected = s$sex %||% "unknown")),
          shiny::column(1, shiny::numericInput(ns(paste0("code_", i)), label = NULL, value = s$code %||% 1, min = 0)),
          shiny::column(5, shiny::textInput(ns(paste0("bam_", i)), label = NULL, value = s$bam %||% "", width = "100%")),
          shiny::column(2, shiny::textInput(ns(paste0("coverage_", i)), label = NULL, value = s$coverage %||% "", width = "100%"))
        )
      })
      shiny::tagList(header_row, sample_rows)
    })

    # Preferences management with cookies
    # Using JavaScript to read/write cookies and communicate with Shiny
    # If/when the browser sends input$cookie_prefs → dropdowns update with cookie values.
    # --- Default preferences ---
    # read from file
    colnames_dt <- fread(system.file("extdata", "db", "table_schema", "colnames.tsv", package = "puzzleapp"))

    # Helpers
    get_raw <- function(key) {
      v <- colnames_dt[name == key, value][1]
      if (length(v) == 0 || is.na(v)) return(NA_character_)
      trimws(gsub('"', "", v))
    }
    get_vec <- function(key) {
      v <- get_raw(key)
      if (is.na(v)) character(0) else trimws(strsplit(v, ";", fixed = TRUE)[[1]])
    }

    # Defaults (vectors)
    snv_default        <- get_vec("snv_default")
    sv_default         <- get_vec("sv_default")
    panelapp_default   <- get_vec("panelapp_default")
    phenotype_default  <- get_vec("phenotype_default")

    # All available columns (keep as semicolon strings to match your original structure)
    colnames_options <- data.table(
      Table    = c("SNV", "SV", "PanelApp", "Phenotype"),
      colNames = c(get_raw("SNV"), get_raw("SV"), get_raw("PanelApp"), get_raw("Phenotype"))
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
      update_pref_dropdown(session, "SNV",  "snv_preferences",  snv_default)
      update_pref_dropdown(session, "SV",       "sv_preferences",       sv_default)
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
      update_pref_dropdown(session, "SNV",  "snv_preferences",  snv_default, prefs)
      update_pref_dropdown(session, "SV",       "sv_preferences",       sv_default, prefs)
      update_pref_dropdown(session, "PanelApp",  "panelapp_preferences",  panelapp_default, prefs)
      update_pref_dropdown(session, "Phenotype", "phenotype_preferences", phenotype_default, prefs)
    })

    # --- Save to cookie when user clicks "Save preferences" ---
    observeEvent(input$update_preferences, {
      prefs <- list(
        SNV  = input$snv_preferences,
        SV        = input$sv_preferences,
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

    observeEvent(input$save_yml, {
      # Show save dialog
      shiny::showModal(shiny::modalDialog(
        title = "Save Configuration YAML",
        shiny::textInput(ns("yml_save_path"), "YAML file path:", placeholder = "e.g., config.yml", width = "100%"),
        footer = tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_save_yml"), "Save", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_save_yml, {
      removeModal()
      n <- input$num_individuals
      samples <- lapply(seq_len(n), function(i) {
        list(
          sample_id = input[[paste0("sample_id_", i)]],
          kinship   = input[[paste0("kinship_", i)]],
          status    = input[[paste0("status_", i)]],
          sex       = input[[paste0("sex_", i)]],
          code      = input[[paste0("code_", i)]],
          bam       = input[[paste0("bam_", i)]],
          coverage  = input[[paste0("coverage_", i)]]
        )
      })
      config <- list(
        samples = samples,
        paths = list(
          snvs_vcf = input$snvs_vcf,
          snvs_tsv = input$snvs_tsv,
          svs_vcf  = input$svs_vcf,
          svs_tsv  = input$svs_tsv
        ),
        dependencies = list(
          panel_app = input$panel_app,
          phenotype_data = input$phenotype_data
        )
      )
      yml_path <- input$yml_save_path
      if (is.null(yml_path) || !nzchar(yml_path)) {
        shiny::showNotification(
          "Please provide a valid file path to save the YAML.",
          type = "error"
        )
        return()
      }
      yml_lines <- character()
      yaml_quote <- function(x) {
        x <- as.character(x)
        x <- gsub('"', '\\"', x, fixed = TRUE)
        paste0('"', x, '"')
      }
      ## samples
      yml_lines <- c(yml_lines, "samples:")
      for (s in config$samples) {
        yml_lines <- c(
          yml_lines,
          paste0("  - sample_id: ", yaml_quote(s$sample_id)),
          paste0("    kinship: ",   yaml_quote(s$kinship)),
          paste0("    status: ",    yaml_quote(s$status)),
          paste0("    sex: ",       yaml_quote(s$sex)),
          paste0("    code: ",      s$code),
          paste0("    bam: ",       yaml_quote(s$bam)),
          paste0("    coverage: ",  yaml_quote(s$coverage))
        )
      }
      ## paths
      yml_lines <- c(
        yml_lines,
        "paths:",
        paste0("  snvs_vcf: ", yaml_quote(config$paths$snvs_vcf)),
        paste0("  snvs_tsv: ", yaml_quote(config$paths$snvs_tsv)),
        paste0("  svs_vcf: ",  yaml_quote(config$paths$svs_vcf)),
        paste0("  svs_tsv: ",  yaml_quote(config$paths$svs_tsv))
      )
      ## dependencies
      yml_lines <- c(
        yml_lines,
        "dependencies:",
        paste0("  panel_app: ",       yaml_quote(config$dependencies$panel_app)),
        paste0("  phenotype_data: ",  yaml_quote(config$dependencies$phenotype_data))
      )
      writeLines(yml_lines, yml_path)
      shiny::showNotification(
        paste("Configuration saved to", yml_path),
        type = "message"
      )
    })

  }) # end moduleServer
} # end home_server
