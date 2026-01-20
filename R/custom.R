#' Raw Filter Tab UI
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
customUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::br(),
    fluidRow(
      column(6, style = "padding: 1;", textInput(ns("load_from_file"), label = NULL, placeholder = "Filter file path:", width = "100%")),
      column(1, actionButton(ns("btn_load_custom"), "Load file", class = "btn-primary")),
      column(5)
    ),
    # Dynamic container for a single child UI (replaced on each load)
    fluidRow(
      column(12, uiOutput(ns("custom_table_container")))
    )
  )
}

#' Raw Filter Tab Server
#' @param id Module ID
#' @return Shiny server module
#' @export
customServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Holds the current child module id
      current_child_id <- reactiveVal(NULL)

      # Render the current child UI; old UI is removed when id changes
      output$custom_table_container <- renderUI({
        req(current_child_id())
        dataUI(ns(current_child_id()))
      })

      observeEvent(input$btn_load_custom, {
        req(input$load_from_file)

        # Create fresh storage and reactive state for the new module instance. Todo: this many children are not needed.
        shared_store2 <- new.env(parent = emptyenv())
        shared_store2$value_for_data  <- list()
        shared_store2$data_for_data   <- list()
        shared_store2$original_data   <- list()
        shared_store2$preferred_cols  <- list()
        shared_store2$samples         <- NULL
        shared_store2$pedigree        <- NULL
        shared_store2$panel_app_data  <- NULL
        shared_store2$vep_map         <- NULL
        shared_store2$phenotype_data  <- NULL
        shared_store2$vep_consequences<- NULL
        shared_store2$svlog_db        <- NULL
        shared_store2$igv_data        <- NULL
        shared_store2$gene_symbol_data<- NULL
        shared_store2$hpo_id_data     <- NULL
        shared_store2$work_dir        <- NULL
        shared_store2$sticky_work_dir <- FALSE
        shared_store2$verbose_level   <- 0L
        shared_rx2 <- list(
          data_version       = reactiveVal(0L),
          panelapp_version   = reactiveVal(0L),
          igv_version        = reactiveVal(0L),
          genesymbol_version = reactiveVal(0L),
          hpoid_version      = reactiveVal(0L)
        )

        tryCatch({
          custom_df <- data.table::fread(input$load_from_file, sep = "\t", data.table = FALSE)
          custom_df$.row_id <- seq_len(nrow(custom_df))
          shared_store2$data_for_data[["[custom]_Boundary"]] <- custom_df
          shared_store2$data_for_data[["custom"]]            <- custom_df

          # New child id for each load ensures old module is removed/stopped
          child_id <- sprintf("custom_table_%s", as.integer(Sys.time()))

          # Re-render UI with the new child id (this removes the previous UI)
          current_child_id(child_id)

          # Start a fresh dataServer bound to the new storage/reactives
          dataServer(child_id, shared_store2, shared_rx2, "custom", "custom")
        }, error = function(e) {
          showNotification(paste("Error loading file:", e$message), type = "error")
        })
      })
    }
  )
}