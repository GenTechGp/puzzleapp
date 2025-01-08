# Define a server module for the "Tab Label"
tabServer <- function(id, filtered_data, selected) {
  moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    selected <- setdiff(selected, "Color")

    data <- as.data.frame(isolate(filtered_data()))
    if (!("PRIORITY" %in% names(data))) {
      data$PRIORITY <- 0
    }
    if (!("NOTES" %in% names(data))) {
      data$NOTES <- ""
    }
    if (!("Color" %in% names(data))) {
      data$Color <- "#FFFFFF"
    }
    # Order selected columns first
    data <- data[, c(selected, setdiff(names(data), selected))]

    opts <- list(
      stateSave = FALSE,
      lengthMenu = list(c(25, 50, -1), c("25", "50", "All")),
      columnDefs = list(
        list(targets = which(names(data) %in% selected == FALSE),
             visible = FALSE),
        list(targets = '_all', className = 'dt-body-nowrap')
      ),
      colReorder = TRUE
    )
    js <- paste0(
      "table.on('column-reorder', function(e, settings, details) {",
      "  Shiny.setInputValue('", ns("colOrder"), "', details.mapping, {priority: 'event'});",
      "});"
    )

    output$table <- DT::renderDT({
      # Check if data is valid and has columns
      if (!is.null(data) && ncol(data) > 0) {
        DT::datatable(
          data,
          filter = list(position = "top", clear = TRUE),
          selection = "none",
          escape = FALSE,
          extensions = "ColReorder",
          callback = JS(js),
          options = opts,
          editable = list(target = "cell", columns = 1)
        ) %>%
          # Apply row background color based on "Color" column values
          formatStyle(columns = c(selected, "Color"), valueColumns = "Color",
                      backgroundColor = JS("value"), target = 'row')
      } else {
        # Display message if data is null or empty
        DT::datatable(
          data.frame(Message = "No data available"),
          selection = "none",
          escape = FALSE,
          options = list(
            dom = 't',
            paging = FALSE,
            searching = FALSE
          )
        )
      }
    })

    # Save table to output file
    observeEvent(input$save_file, {
      show_spinner()
      output_dir <- input$output_dir
      filename <- input$out_filename
      filetype <- input$filetype
      scope <- input$out_scope

      # Check if output directory is specified
      if (output_dir == "") {
        shinyalert("Error", "Output directory is empty. Please provide a valid directory.", type = "error")
        hide_spinner()
        return(NULL)
      }

      # Check if output directory exists
      if (!dir.exists(output_dir)) {
        shinyalert("Error", "Output directory does not exist. Please provide a valid directory.", type = "error")
        hide_spinner()
        return(NULL)
      }

      # Add the filetype extension to the filename
      file_extension <- switch(filetype,
                               "excel" = "xlsx",
                               "tsv" = "tsv",
                               "csv" = "csv")
      full_filename <- paste0(filename, ".", file_extension)
      output_path <- file.path(output_dir, full_filename)

      if (scope == "all") {
        dataset <- data.table(filtered_data())
        dataset[, c("ClinVar", "GNOMADv4", "Color") := NULL]
      } else {
        # TODO: handle
        dataset <- data.table(fileData()[,names(fileData())])
        dataset[, c("ClinVar", "GNOMADv4", "Color") := NULL]
      }

      tryCatch({
        if (filetype == "excel") {
          write.xlsx(dataset, output_path)
        } else if (filetype == "tsv") {
          fwrite(dataset, output_path, sep = "\t")
        } else if (filetype == "csv") {
          fwrite(dataset, output_path, sep = ",")
        }
        shinyalert("Success", paste("File saved successfully:", full_filename), type = "success")
      }, error = function(e) {
        shinyalert("Error", paste("Error saving file:", e$message), type = "error")
      })
      hide_spinner()
    })

    proxy <- dataTableProxy("table")

    # 0 represents the row index column
    colOrder <- reactiveVal(0:ncol(data))
    observeEvent(input$colOrder, colOrder(colOrder()[input$colOrder + 1]))

    observeEvent(input$selected_vars, {
      cols <- c(NA, names(data))[colOrder() + 1]
      sel <- c(setdiff(input$selected_vars, "Color"), NA)
      pos <- which(cols %in% sel) - 1
      showCols(proxy, pos, reset = TRUE)
    })
  })
}
