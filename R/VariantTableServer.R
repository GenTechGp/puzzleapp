coladd <- function(df, id, val) {
  if (!(id %in% names(df))) {
    df[, id] <- val
  }
  df
}

setup_data <- function(data, selected) {
  data <- data %>%
    as.data.frame %>%
    coladd("PRIORITY", 0) %>%
    coladd("NOTES", "") %>%
    coladd("INHERITANCE", NA) %>%
    coladd("PANEL_APP", NA) %>%
    coladd("HPO_ID", NA) %>%
    coladd("HPO_COUNT", 0) %>%
    coladd("Color", "#FFFFFF")
  # Order selected columns first
  data[, c(selected, setdiff(sort(names(data)), selected))]
}

# Swap column re-order direction
# Case 1) i+1, i+2, ..., i+j-1, i+j, i
# Case 2) i+j, i  , i+1, i+2  , ..., i+j-1
swap_order <- function(x) {
  l <- length(x)
  if (l < 3)
    return(x)

  if (x[1] < x[2]) # Case 1
    return(x[c(l-1, l, 1:(l-2))])

  return(x[c(3:l, 1, 2)]) # Case 2
}

# Define a server module for the "Tab Label"
tabServer <- function(id, filtered_data, selected) {
  moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    data <- setup_data(isolate(filtered_data()), selected)

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
          # TODO: fix
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
    }, server = TRUE)

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

    # 0 and NA represent the row index column
    colOrder <- reactiveVal(0:ncol(data))
    cols <- reactive(c(NA, names(data))[colOrder() + 1])
    sel <- reactive(c(NA, input$selected_vars))
    pos <- reactive(which(cols() %in% sel()) - 1)

    observeEvent(input$colOrder, {
      order <- input$colOrder
      focus <- order[order != 0:ncol(data)]
      order[order != 0:ncol(data)] <- swap_order(focus)
      colOrder(colOrder()[order + 1])
    })

    observeEvent(input$selected_vars, # TODO: react to table rendering
      showCols(proxy, pos(), reset = TRUE)
    )

    observeEvent(filtered_data(), {
      data <- setup_data(filtered_data(), selected)
      replaceData(proxy, data)
    }, ignoreInit = TRUE)
  })
}
