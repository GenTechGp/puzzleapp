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

get_out_path <- function(ns, dir, ext) {
  sample_id <- unlist(strsplit(ns("sample"), "-"))[1]
  path <- paste0(dir, "/", sample_id, ".", ext)
}

tabFileSavingUI <- function(ns, dir) {
  default_ext <- "tsv"
  default_path <- get_out_path(ns, dir, default_ext)
  fmts <- c("tsv", "csv", "xlsx")
  scopes <- c("All variables" = "all",
              "Selected variables" = "selected")

  tagList(
    selectInput(ns("out_ext"), "Format", fmts, default_ext),
    selectInput(ns("out_scope"), "Scope", scopes, "all"),
    checkboxInput(ns("out_filter"), "Use search filters"),
    textInput(ns("out_path"), "Path", value = default_path)
  )
}

save_file <- function(ns, input, filtered_data, sel) {
  save_exclude_vars <- c("ClinVar", "GNOMADv4", "PRIORITYFlag", NA)

  if (input$out_scope == "all") {
    cols_sub <- setdiff(names(filtered_data()), save_exclude_vars)
  } else if (input$out_scope == "selected") {
    cols_sub <- setdiff(sel(), save_exclude_vars)
  } else {
    showNotification(paste("Unknown scope:", input$out_scope), type = "error")
  }

  # TODO: one line solution
  if (nrow(filtered_data()) == 0) {
    dataset <- filtered_data()[, ..cols_sub]
  } else {
    dataset <- filtered_data()[, cols_sub]
  }

  if (input$out_filter == TRUE) {
    dataset <- dataset[input$table_rows_all, ]
  }

  showNotification("Saving...", duration = NULL, id = ns("notify_save"),
                   type = "message")
  tryCatch({
    if (input$out_ext == "xlsx") {
      write.xlsx(dataset, input$out_path)
    } else if (input$out_ext == "tsv") {
      fwrite(dataset, input$out_path, sep = "\t")
    } else if (input$out_ext == "csv") {
      fwrite(dataset, input$out_path, sep = ",")
    }
    removeNotification(ns("notify_save"))
    showNotification("Data saved", type = "message")
  }, error = function(e) {
    removeNotification(ns("notify_save"))
    showNotification(paste("Error saving data:", e$message), type = "error")
  })
}


# Define a server module for the "Tab Label"
tabServer <- function(id, filtered_data, selected, pref, exclude = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    data <- setupData(isolate(filtered_data()), selected)
    filtered_data(data)

    opts <- list(
      stateSave = TRUE,
      lengthMenu = list(c(25, 50, -1), c("25", "50", "All")),
      columnDefs = list(
        list(targets = which(names(data) %in% selected == FALSE),
             visible = FALSE),
        list(targets = '_all', className = 'dt-body-nowrap')
      ),
      colReorder = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(extend = 'colvis', text = 'Select Variables'),
        list(extend = 'collection', text = 'Export', dropIcon = TRUE,
             action = JS(paste0(
               "function(e, dt, node, config) {",
                 "Shiny.setInputValue('", ns("save"), "', true, {priority: 'event'});",
               "}"
             )))
      )
    )
    callback <- paste0(
      "table.on('column-reorder', function(e, settings, details) {",
      "  Shiny.setInputValue('", ns("colOrder"), "', details.mapping, {priority: 'event'});",
      "});"
    )

    output$table <- DT::renderDT({
      # Check if data is valid and has columns
      data <- setupData(isolate(filtered_data()), selected)
      if (!is.null(exclude)) {
        opts$buttons[[1]]$columns <- c(0, which(!(names(data) %in% exclude)))
      }
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

    observeEvent(input$save, {
      showModal(modalDialog(
        title = 'Export Options',
        tabFileSavingUI(ns, pref$outdir),
        footer = tagList(
          actionButton(ns("save_file"), 'Save'),
          modalButton('Cancel')
        ),
        easyClose = TRUE)
      )
    })

    observeEvent(input$out_ext, {
      path <- get_out_path(ns, pref$outdir, input$out_ext)
      updateTextInput(inputId = "out_path", value = path)
    })

    # Save table to output file
    observeEvent(input$save_file, {
      if (file.exists(input$out_path)) {
        showModal(modalDialog(
          title = "Path already exists! Overwrite?",
          tagList(
            modalButton('No'),
            actionButton(ns("save_overwrite"), 'Yes')
          ),
          footer = NULL
        ))
      } else {
        removeModal()
        save_file(ns, input, filtered_data, sel)
      }
    })

    observeEvent(input$save_overwrite, {
      removeModal()
      save_file(ns, input, filtered_data, sel)
    })

    proxy <- dataTableProxy("table")

    # 0 and NA represent the row index column
    colOrder <- reactiveVal(0:ncol(data))
    cols <- reactive(c(NA, names(data))[colOrder() + 1])
    # Requires stateSave = TRUE in datatable options
    table_st_cols <- reactive(data.frame(
      cbind(name = names(input$table_state$columns),
            do.call(rbind, c(input$table_state$columns, use.names = FALSE)))
    ))
    vis <- reactive(unlist(table_st_cols()$visible))
    sel <- reactive(c(NA, names(data))[vis()])

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
