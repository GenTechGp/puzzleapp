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
      shiny::updateActionButton(session, "load_data", disabled = TRUE)
      if (nzchar(input$snvs_tsv) == 0 && nzchar(input$svs_tsv) == 0) {
        shiny::showNotification("No data files specified to load.", type = "error")
        shiny::updateActionButton(session, "load_data", disabled = FALSE)
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
      collected <- collect_inputs(input)
      if (length(collected$messages) > 0) {
        for (msg in collected$messages) {
          shiny::showNotification(msg, type = "error")
        }
        shiny::updateActionButton(session, "load_data", disabled = FALSE)
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
      shared_store$igv_data <- list(
        snvs_vcf = collected$snvs_vcf,
        svs_vcf = collected$svs_vcf,
        igv_genome = input$igv_genome
      )

      shared_store$data_for_data[["[panel_app]_Boundary"]] <- collected$panel_app_default_dt
      shared_store$data_for_data[["panel_app"]] <- collected$panel_app_data

      shared_store$data_for_data[["[phenotype]_Boundary"]] <- collected$phenotype_default_dt
      shared_store$data_for_data[["phenotype"]] <- collected$phenotype_data

      shared_store$data_for_data[["[vep_consequences]_Boundary"]] <- vep_consequences
      shared_store$data_for_data[["vep_consequences"]] <- vep_consequences

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
          shiny::column(1, shiny::selectInput(ns(paste0("kinship_", i)), label = NULL, choices = v_kinships, selected = s$kinship %||% "unknown")),
          shiny::column(1, shiny::selectInput(ns(paste0("status_", i)), label = NULL, choices = v_statuses, selected = s$status %||% "unknown")),
          shiny::column(1, shiny::selectInput(ns(paste0("sex_", i)), label = NULL, choices = v_sexes, selected = s$sex %||% "unknown")),
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
    snv_default   <- c("ID", "PRIORITY", "NOTES", "GT", "CONSEQUENCE", "GENE_SYMBOL", "AF",
                            "N_HOM_ALT", "SpliceAI_pred", "CLINVAR", "REVEL", "SIFT", "PolyPhen",
                            "am_class", "am_pathogenicity", "CADD_PHRED", "CADD_RAW",
                            "PANEL_APP", "INHERITANCE")
    sv_default   <- c("ID", "PRIORITY", "NOTES", "GT", "CONSEQUENCE", "GENE_SYMBOL", "AF",
                            "N_HOM_ALT", "SpliceAI_pred", "CLINVAR", "REVEL", "SIFT", "PolyPhen",
                            "am_class", "am_pathogenicity", "CADD_PHRED", "CADD_RAW",
                            "PANEL_APP", "INHERITANCE")
    panelapp_default   <- c("Entity_Name", "Mode_Of_Inheritance", "Level4", "Sources")
    phenotype_default  <- c("disease_id", "hpo_id", "gene_symbol", "hpo_name", "ncbi_gene_id")
    # --- All available columns (user can choose any of these) ---
    colnames_options <- data.table(
      Table = c("SNV", "SV", "PanelApp", "Phenotype"),
      colNames = c(
        "AD;AF;ALT;Acceptor_Gain;Acceptor_Loss;CADD_PHRED;CADD_RAW;CATEGORY;CHROM;CLINVAR;CONSEQUENCE;DP;Donor_Gain;Donor_Loss;FILTER;GENE_ID;GENE_SYMBOL;GT;HGVSc;HGVSg;HGVSp;HPO_COUNT;HPO_ID;ID;INHERITANCE;NOTES;N_HOM_ALT;PANEL_APP;POS;PRIORITY;PRIORITYFlag;PolyPhen;QUAL;REF;REVEL;SIFT;SpliceAI_pred;TRANSCRIPT;VAF;VAR_LENGTH;VAR_TYPE;alt_allele_count;am_class;am_pathogenicity;clinvar_override;spliceai_override;gnomAD_ID;CLINVAR_ID;N_Cohort;GQ",
        "AD;AF;ALT;Acceptor_Gain;Acceptor_Loss;CADD_PHRED;CADD_RAW;CATEGORY;CHROM;CLINVAR;CONSEQUENCE;DP;Donor_Gain;Donor_Loss;FILTER;GENE_ID;GENE_SYMBOL;GT;HGVSc;HGVSg;HGVSp;HPO_COUNT;HPO_ID;ID;INHERITANCE;NOTES;N_HOM_ALT;PANEL_APP;POS;PRIORITY;PRIORITYFlag;PolyPhen;QUAL;REF;REVEL;SIFT;SpliceAI_pred;TRANSCRIPT;VAF;VAR_LENGTH;VAR_TYPE;alt_allele_count;am_class;am_pathogenicity;clinvar_override;spliceai_override;gnomAD_ID;CLINVAR_ID;N_Cohort;GQ",
        "Entity_Name;Entity_type;Gene_Symbol;Sources;Level4;Level3;Level2;Model_Of_Inheritance;Phenotypes;Omim;Orphanet;HPO;Publications;Description;Flagged;GEL_Status;UserRatings_Green_amber_red;version;ready;Mode_of_pathogenicity;EnsemblId_GRch37;EnsemblId_GRch38;HGNC;Position_Chromosome;Position_GRCh37_Start;Position_GRCh37_End;Position_GRCh38_Start;Position_GRCh38_End;STR_Repeated_Sequence;STR_Normal_Repeats;STR_Pathogenic_Repeats;Region_Haploinsufficiency_Score;Region_Triplosensitivity_Score;Region_Required_Overlap_Percentage;Region_Variant_Type;Region_Verbose_Name;Panel_ID;Panel_Version",
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

  }) # end moduleServer
} # end home_server
