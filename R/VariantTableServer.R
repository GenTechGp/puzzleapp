coladd <- function(df, id, val) {
  if (!(id %in% names(df))) {
    df[, id] <- val
  }
  df
}

setupData <- function(data, selected) {
  data <- data %>%
    as.data.frame %>%
    coladd("PRIORITY", 0) %>%
    coladd("NOTES", "") %>%
    coladd("INHERITANCE", NA) %>%
    coladd("PANEL_APP", NA) %>%
    coladd("HPO_ID", NA) %>%
    coladd("HPO_COUNT", 0) %>%
    coladd("spliceai_override", FALSE) %>%
    coladd("clinvar_override", FALSE) %>%
    coladd("PRIORITYFlag", NA)
  # Order selected columns first, then the rest with the tail at the end
  tail <- c("spliceai_override", "clinvar_override", "PRIORITYFlag")
  cols <- c(selected, setdiff(sort(names(data)), c(selected, tail)), tail)
  data[, cols]
}

resetData <- function(proxy, data, selected) {
  data <- setupData(data, selected)
  replaceData(proxy, data)
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

    data <- setupData(isolate(filtered_data()), selected)
    filtered_data(data)

    initComplete <- paste0(
      "function(settings, json) {",
      "  Shiny.setInputValue('", ns("tableInitComplete"), "', json);",
      "}"
    )
    opts <- list(
      stateSave = FALSE,
      lengthMenu = list(c(25, 50, -1), c("25", "50", "All")),
      columnDefs = list(
        list(targets = which(names(data) %in% selected == FALSE),
             visible = FALSE),
        list(targets = '_all', className = 'dt-body-nowrap')
      ),
      colReorder = TRUE,
      initComplete = JS(initComplete),
      dom = 'Bfrtip',
      buttons = list(list(extend = 'colvis', text = 'Select Variables'))
    )
    callback <- paste0(
      "table.on('column-reorder', function(e, settings, details) {",
      "  Shiny.setInputValue('", ns("colOrder"), "', details.mapping, {priority: 'event'});",
      "});"
    )

    output$table <- DT::renderDT({
      # Check if data is valid and has columns
      data <- setupData(isolate(filtered_data()), selected)
      if (!is.null(data) && ncol(data) > 0) {
        DT::datatable(
          data,
          filter = list(position = "top", clear = TRUE),
          selection = "none",
          escape = FALSE,
          extensions = c("ColReorder", "Buttons"),
          callback = JS(callback),
          options = opts,
          editable = list(target = "cell", numeric = "none")
        ) %>%
          # Apply row background color
          formatStyle("spliceai_override",
                      backgroundColor = styleEqual(TRUE, "#FFFF0099"),
                      target = 'row') %>%
          formatStyle("clinvar_override",
                      backgroundColor = styleEqual(TRUE, "#FFA50099"),
                      target = 'row') %>%
          formatStyle("PRIORITYFlag",
                      backgroundColor = styleEqual(c(TRUE, FALSE),
                                                   c("#90EE90", "#FFCCCC")),
                      target = 'row')
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
        dataset[, c("ClinVar", "GNOMADv4", "PRIORITYFlag") := NULL]
      } else {
        # TODO: use colvis
        cols_sub <- setdiff(sel(), c("ClinVar", "GNOMADv4", "PRIORITYFlag", NA))
        dataset <- data.table(filtered_data()[, cols_sub])
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

    observeEvent(input$colOrder, {
      order <- input$colOrder
      focus <- order[order != 0:ncol(data)]
      order[order != 0:ncol(data)] <- swap_order(focus)
      colOrder(colOrder()[order + 1])
    })

    observeEvent(filtered_data(), {
      resetData(proxy, filtered_data(), selected)
    }, ignoreInit = TRUE)

    observeEvent(input$table_cell_edit, {
      edit <- input$table_cell_edit
      req(edit)

      data <- isolate(filtered_data())
      clear <- FALSE

      col <- cols()[edit$col+1]
      if (is.na(col)) {
        clear <- TRUE
      } else if (col == "PRIORITY") {
        v <- as.integer(edit$value)
        if (is.na(v)) { # Not an integer
          p <- 0
          c <- NA
          if (data[edit$row, "PRIORITY"] == 0) {
            clear <- TRUE
          }
        } else {
          p <- v
          if (v > 0) {
            c <- TRUE
          } else if (v < 0) {
            c <- FALSE
          } else {
            c <- NA
          }
        }
        data[edit$row, "PRIORITY"] <- p
        data[edit$row, "PRIORITYFlag"] <- c
      } else if (col == "NOTES") {
        data[edit$row, "NOTES"] <- edit$value
      } else {
        clear <- TRUE
      }

      if (clear) {
        resetData(proxy, data, selected) # Undo edit
      } else {
        filtered_data(data)
      }
    })
  })
}
