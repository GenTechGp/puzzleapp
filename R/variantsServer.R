#' Variants Tab Server
#' @param id Module ID
#' @export
#' @import shiny
variants_server <- function(id, shared_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exported_cols <- reactiveVal(NULL)

    reset_trigger <- reactiveVal(0)
    observeEvent(input$reset_table, {
      reset_trigger(reset_trigger() + 1)
    })

    output$variants_table <- DT::renderDataTable({
      reset_trigger()  # dependency for reset

      df <- shared_data()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      DT::datatable(
        df,
        extensions = c("ColReorder", "Buttons"),
        options = list(
          dom = "lBfrtip",  # Buttons + Filter + Length + Table + Info + Pagination
          buttons = list(
            list(extend = "colvis", text = "Select Columns"),
            list(
              extend = "collection",   # can be 'collection' or 'text'
              text = "Deselect All",
              action = DT::JS("
                function ( e, dt, node, config ) {
                  dt.columns().visible(false);
                }
              ")
            ),
            list(
              extend = "collection",
              text = "Select All",
              action = DT::JS("
                function ( e, dt, node, config ) {
                  dt.columns().visible(true);
                }
              ")
            ),
            list(
              extend = "collection", text = "Export", dropIcon = TRUE,
              action = DT::JS(
                sprintf("
                  function(e, dt, node, config) {
                    // Get the column indexes in current *display* order
                    var colOrder = dt.colReorder.order();

                    // Keep only visible ones
                    var exportCols = colOrder.filter(function(idx){
                      return dt.column(idx).visible();
                    });

                    // Send to Shiny
                    Shiny.setInputValue('%s', exportCols, {priority: 'event'});
                  }", ns("export_signal")
                )
              )
            )
          ),
          colReorder = TRUE,
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(list(width = "150px", targets = "_all")),
          drawCallback = DT::JS("
            function(settings) {
              var table = this.api();
              // ensure headers/body align after column visibility change
              table.on('column-visibility.dt', function(e, settings, column, state) {
                table.columns.adjust();
              });
            }
          ")
        )
      )
    })

    observeEvent(input$export_signal, {
      exported_cols(input$export_signal)

      showModal(modalDialog(
        title = "Save Table",
        textInput(ns("out_path"), "Save Path", value = ""),
        selectInput(ns("out_ext"), "Format", choices = c("tsv","csv","xlsx"), selected = "tsv"),
        checkboxInput(ns("overwrite"), "Overwrite if output file exists", value = TRUE),
        checkboxInput(ns("save_selected"), "Save only selected columns", value = TRUE),  # NEW
        footer = tagList(
          actionButton(ns("confirm_save"), "Save"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_save, {
      req(input$out_path)

      # check overwrite setting
      if (!isTRUE(input$overwrite) && file.exists(input$out_path)) {
        showNotification("File already exists. Overwrite is disabled.", type = "error")
        return(NULL)
      }
      cat("Saving data to", input$out_path, "as", input$out_ext, "\n")

      df <- shared_data()

      # Decide whether to save only selected columns or full dataset
      if (isTRUE(input$save_selected)) {
        req(exported_cols())
        cat("Save selected columns only\n")
        cat("Exported columns in user order:", paste(exported_cols(), collapse = ", "), "\n")
        data_to_save <- df[, exported_cols(), drop = FALSE]
      } else {
        data_to_save <- df
      }

      tryCatch({
        switch(input$out_ext,
              tsv  = data.table::fwrite(data_to_save, input$out_path, sep = "\t"),
              csv  = data.table::fwrite(data_to_save, input$out_path, sep = ","),
              xlsx = openxlsx::write.xlsx(data_to_save, input$out_path)
        )
        showNotification("Data saved successfully", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error saving data:", e$message), type = "error")
      })
    })
  })
}