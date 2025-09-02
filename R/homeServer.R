#' Home Tab Server
#'
#' @param id Module ID
#' @export
#' @import shiny

source("R/db_utility.R")

home_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    clear_shared_data <- function(shared_data) {
      # Clear shared_data reactiveValues
      fields <- c("samples", "dependencies", "snvs_data", "svs_data", "snvs_data_filtered", "svs_data_filtered", "panel_app_data", "vep_map", "phenotype_data")
      for (f in fields) shared_data[[f]] <- NULL
      shared_data$paths <- list()
    }

    # Clear inputs
    observeEvent(input$clear_inputs, {
      # Reset numeric input for Individuals
      updateNumericInput(session, "num_individuals", value = 1)
      updateTextInput(session, "yml_path", value = "")
      updateTextInput(session, "snvs_vcf", value = "")
      updateTextInput(session, "snvs_tsv", value = "")
      updateTextInput(session, "svs_vcf", value = "")
      updateTextInput(session, "svs_tsv", value = "")
      updateTextInput(session, "panel_app", value = "")
      updateTextInput(session, "vep_consequences", value = "")
      updateTextInput(session, "phenotype_data", value = "")
      
      # Clear shared_data
      clear_shared_data(shared_data)
      shared_data$config_samples <- NULL
      shared_data$config_samples <- list(list(sample_id="",kinship="unknown",status="unknown",sex="unknown",code=1,bam="",coverage=""))
      cat("Inputs cleared and shared_data reset.\n")
    })

    # Feedback to user
    output$status <- renderText({
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
    observeEvent(input$load_yml, {
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
          updateTextInput(session, "snvs_vcf", value = config$paths$snvs_vcf)
        }
        if (!is.null(config$paths$snvs_tsv)) {
          updateTextInput(session, "snvs_tsv", value = config$paths$snvs_tsv)
        }
        if (!is.null(config$paths$svs_vcf)) {
          updateTextInput(session, "svs_vcf", value = config$paths$svs_vcf)
        }
        if (!is.null(config$paths$svs_tsv)) {
          updateTextInput(session, "svs_tsv", value = config$paths$svs_tsv)
        }
        if (!is.null(config$dependencies$panel_app)) {
          updateTextInput(session, "panel_app", value = config$dependencies$panel_app)
        }
        if (!is.null(config$dependencies$vep_consequences)) {
          updateTextInput(session, "vep_consequences", value = config$dependencies$vep_consequences)
        }
        if (!is.null(config$dependencies$phenotype_data)) {
          updateTextInput(session, "phenotype_data", value = config$dependencies$phenotype_data)
        }
        # Set number of Individuals based on YAML
        updateNumericInput(session, "num_individuals", value = length(config$samples))
      } else {
        showNotification("No valid YAML path provided.", type = "error")
      }
    })

    shiny::observeEvent(input$load_data, {
      if (nzchar(input$snvs_tsv) == 0 && nzchar(input$svs_tsv) == 0) {
        showNotification("No data files specified to load.", type = "error")
        return()
      }
      clear_shared_data(shared_data)
      # Store in shared_data
      # Collect sample info from UI inputs
      n <- input$num_individuals
      shared_data$samples <- lapply(seq_len(n), function(i) {
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
      cat("Collected sample info for", length(shared_data$samples), "individuals.\n")
      cat("Samples:", str(shared_data$samples), "\n")

      if (!is.null(input$snvs_tsv) && nzchar(input$snvs_tsv)){
        if (file.exists(input$snvs_tsv)) {
          shared_data$snvs_data <- fread(input$snvs_tsv, stringsAsFactors = FALSE, nrows = 1000)
          shared_data$snvs_data_filtered <- shared_data$snvs_data # acts like a shallow reference initially, but behaves as a deep copy once you modify it
        } else {
          shiny::showNotification("SNVs & Indels TSV file not found.", type = "error")
        }
      }
      if (!is.null(input$svs_tsv) && nzchar(input$svs_tsv)){
        if( file.exists(input$svs_tsv)) {
          shared_data$svs_data <- fread(input$svs_tsv, stringsAsFactors = FALSE, nrows = 1000)
          shared_data$svs_data_filtered <- shared_data$svs_data # acts like a shallow reference initially, but behaves as a deep copy once you modify it
        } else {
          showNotification("SVs TSV file not found.", type = "error")
        }
      }
      if (!is.null(input$panel_app) && nzchar(input$panel_app)){
        cat("input$panel_app:", input$panel_app, "\n")
        if (file.exists(input$panel_app)) {
          shared_data$panel_app_data <- NULL#load_panel_app_data(file = input$panel_app)
        } else {
          showNotification("PanelApp TSV file not found.", type = "error")
        }
      } else {
        cat("Loading from PanelApp database.\n")
        shared_data$panel_app_data <- NULL#load_panel_app_data()
      }
      if (!is.null(input$vep_consequences) && nzchar(input$vep_consequences)){
        if (file.exists(input$vep_consequences)) {
          shared_data$vep_map <- NULL#load_vep_map(file = input$vep_consequences)
        } else {
          showNotification("VEP consequence annotations file not found.", type = "error")
        }
      } else {
        cat("Loading from VEP consequences database.\n")
        shared_data$vep_map <- NULL#load_vep_map()
      }
      if (!is.null(input$phenotype_data) && nzchar(input$phenotype_data)){
        if (file.exists(input$phenotype_data)) {
          shared_data$phenotype_data <- NULL#load_phenotype_data(file = input$phenotype_data)
        } else {
          showNotification("Human Phenotype Ontology TSV file not found.", type = "error")
        }
      } else {
        cat("Loading from HPO database.\n")
        shared_data$phenotype_data <- NULL#load_phenotype_data()
      }
      cat("Data loaded into shared_data.\n")
    })

    output$samples_panel <- renderUI({
      n <- input$num_individuals
      if (is.null(n) || n < 1) n <- 1
      # Header row
      header_row <- fluidRow(
        column(1, strong("Sample ID")),
        column(1, strong("Kinship")),
        column(1, strong("Status")),
        column(1, strong("Sex")),
        column(1, strong("Code")),
        column(3, strong("BAM Path")),
        column(4, strong("Coverage Path"))
      )
      # Pull samples from shared_data if available
      samples <- shared_data$config_samples %||% list()
      sample_rows <- lapply(seq_len(n), function(i) {
        s <- if (!is.null(samples) && length(samples) >= i) samples[[i]] else list()
        fluidRow(
          column(1, textInput(ns(paste0("sample_id_", i)), label = NULL, value = s$sample_id %||% "")),
          column(1, selectInput(ns(paste0("kinship_", i)), label = NULL, choices = c("proband", "mother", "father", "sibling", "unknown"), selected = s$kinship %||% "unknown")),
          column(1, selectInput(ns(paste0("status_", i)), label = NULL, choices = c("affected", "unaffected", "unknown"), selected = s$status %||% "unknown")),
          column(1, selectInput(ns(paste0("sex_", i)), label = NULL, choices = c("female", "male", "unknown"), selected = s$sex %||% "unknown")),
          column(1, numericInput(ns(paste0("code_", i)), label = NULL, value = s$code %||% 1, min = 0)),
          column(3, textInput(ns(paste0("bam_", i)), label = NULL, value = s$bam %||% "", width = "100%")),
          column(4, textInput(ns(paste0("coverage_", i)), label = NULL, value = s$coverage %||% "", width = "100%"))
        )
      })
      tagList(header_row, sample_rows)
    })


  })
}
