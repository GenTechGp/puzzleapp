coladd <- function(df, id, val) {
  if (!(id %in% names(df))) {
    if (nrow(df) == 0) {
      # Create an empty column of the correct type
      if (is.numeric(val)) df[[id]] <- numeric(0)
      else if (is.character(val)) df[[id]] <- character(0)
      else if (is.logical(val)) df[[id]] <- logical(0)
      else df[[id]] <- vector(mode = typeof(val), length = 0)
    } else {
      df[[id]] <- val
    }
  }
  df
}

# coladd <- function(df, id, val) {
#   if (!(id %in% names(df))) {
#     df[, id] <- val
#   }
#   df
# }

setupData <- function(data, selected) {
  # browser()
  cat("colnames(data):", paste(colnames(data), collapse=", "), "\n")
  cat("selected:", paste(selected, collapse=", "), "\n")
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
  # browser()
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
    cat("Unknown scope:", input$out_scope, "\n")
    showNotification(paste("Unknown scope:", input$out_scope), type = "error")
  }
  # browser()
  dt <- as.data.table(filtered_data())
  dataset <- dt[, cols_sub, with = FALSE]
  cat("nrow(dataset):", nrow(dataset), "\n")
  cat("cls(dataset):", paste(class(dataset), collapse=", "), "\n")
  # dataset <- data.table(filtered_data())[, ..cols_sub]

  if (input$out_filter == TRUE) {
    dataset <- dataset[input$table_rows_all, ]
  }
  dataset <- as.data.table(dataset) 
  showNotification("Saving...", duration = NULL, id = ns("notify_save"), type = "message")
  
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
tabServer <- function(id, filtered_data, selected, pref, selected_igv_id, exclude = NULL) {
  moduleServer(id, function(input, output, session) {
    
    cat(sprintf("[tabServer] Module initialized (%s)\n",id))
    
    ns <- shiny::NS(id)

    data <- setupData(isolate(filtered_data()), selected)
    filtered_data(data)

    targets_hidden <- which(!(names(data) %in% selected))
    
    total_rows <- reactive({
      nrow(isolate(filtered_data()))  # Get total rows dynamically
    })
    
    display_rows <- reactiveVal(min(100000, total_rows()))  # Start with max 100K or total
    
    display_percentage <- reactive({
      round((display_rows() / total_rows()) * 100, 1)  # Convert rows to percentage
    })
    
    # Dynamically find the index of the "ID" column (zero-based for JavaScript)
    id_column_index <- if ("ID" %in% colnames(data)) {
      which(colnames(data) == "ID")  # Adjust for JS (zero-based)
    } else {
      NULL
    }
    
    opts <- list(
      stateSave = TRUE,
      server = TRUE,         # Enable server-side processing
      processing = TRUE,     # Show spinner while loading
      deferRender = TRUE,    # Render only when needed
      pageLength = 25,       # Show fewer rows by default
      lengthMenu = list(c(25, 50, -1), c("25", "50", "All")),
      columnDefs = list(
        #list(targets = which(names(data) %in% selected == FALSE),
         list(targets = targets_hidden,
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
               "}")
             )),
        list(extend = 'collection', text = '% Data Shown',
             action = JS(paste0(
               "function (e, dt, node, config) {",
               "  Shiny.setInputValue('", ns("open_data_modal"), "', true, {priority: 'event'});",
               "}"
             )))
        
      )
    )
    
    # Add clickable ID logic only if "ID" column exists
# if (!is.null(id_column_index)) {
#   opts$columnDefs <- append(opts$columnDefs, list(
#     list(
#       targets = id_column_index,
#       render = JS(
#         "function(data, type, row, meta) {",
#         "  return '<a class=\"id-link\" data-id=\"' + data + '\" style=\"cursor: pointer; text-decoration: underline; color: blue;\">' + data + '</a>';",
#         "}"
#       )
#     )
#   ))
# }
    if (!is.null(id_column_index)) {
      opts$columnDefs <- append(opts$columnDefs, list(
        list(
          targets = id_column_index,
          render = JS(
            "function(data, type, row, meta) {",
            "  return '<a class=\"id-link\" data-id=\"' + data + '\" style=\"cursor: pointer; text-decoration: underline; color: blue;\">' + data + '</a>';",
            "}"
          )
        )
      ))
    }
    
    callback <- paste0(
      "table.on('column-reorder', function(e, settings, details) {",
      "  Shiny.setInputValue('", ns("colOrder"), "', details.mapping, {priority: 'event'});",
      "});"
    )
    
    if (!is.null(id_column_index)) {
      callback <- paste0(callback, sprintf(
        "table.on('click', '.id-link', function() { 
           event.preventDefault();
           var id = $(this).data('id'); 
           Shiny.setInputValue('%s', id, {priority: 'event'}); 
         });", ns("selected_igv")
      ))
    }

    output$table <- DT::renderDT({
      cat(sprintf("[tabServer] Rendering table (%s)\n",id))
      # Show loading notification
      showNotification("Rendering table...", id = ns("notify_table"), type = "message", duration = NULL)

      time_setup_start <- Sys.time()
      # Check if data is valid and has columns
      data <- setupData(isolate(filtered_data()), selected)
      #data <- data[1:isolate(display_rows()), ]
      data <- data[1:min(nrow(data), isolate(display_rows())), ]
      #data <- data[1:100000,]
      time_setup_end <- Sys.time()
      message("[tabServer] Time for setupData: ", time_setup_end - time_setup_start)
      
      # Timing exclude processing
      time_exclude_start <- Sys.time()
      if (!is.null(exclude)) {
        opts$buttons[[1]]$columns <- c(0, which(!(names(data) %in% exclude)))
      }
      time_exclude_end <- Sys.time()
      message("[tabServer] Time for exclude processing: ", time_exclude_end - time_exclude_start)
      
      # Timing datatable rendering
      time_dt_start <- Sys.time()
      result <- if (!is.null(data) && ncol(data) > 0) {
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
      # Remove loading notification
      removeNotification(ns("notify_table"))
      time_dt_end <- Sys.time()
      message("[tabServer] Time for DT::datatable rendering: ", time_dt_end - time_dt_start)
      return(result)
    }, server = TRUE)
    
    observeEvent(input$selected_igv, {
      cat(sprintf("[tabServer] ID clicked: %s\n",input$selected_igv))
      selected_igv_id(input$selected_igv)
    },ignoreInit = TRUE)

    observeEvent(input$save, {
      cat(sprintf("[tabServer] Save pop-up box (%s)\n",id))
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
    
    observeEvent(display_rows(), {
      cat(sprintf("[tabServer] Display rows updated (%s)\n",id))
    })
    
    observeEvent(input$open_data_modal, {
      cat(sprintf("[tabServer] Percent data shown pop-up box (%s)\n",id))
      showModal(modalDialog(
        title = "Adjust Data Display",
        sliderInput(ns("data_slider"), "Data Shown (%)", 
                    min = 0, max = 100, value = display_percentage(), step = 5, post = "%"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_data_percentage"), "Apply")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_data_percentage, {
      cat(sprintf("[tabServer] Update percent data shown (%s)\n",id))
      removeModal()
      
      # Convert percentage to row count
      new_row_count <- max(1, round((input$data_slider / 100) * total_rows()))
      
      # Update both reactives
      display_rows(new_row_count)  # Update row count
      replaceData(dataTableProxy("table"), filtered_data()[1:new_row_count, ], resetPaging = FALSE)
    })

    observeEvent(input$out_ext, {
      path <- get_out_path(ns, pref$outdir, input$out_ext)
      updateTextInput(inputId = "out_path", value = path)
    })

    # Save table to output file
    observeEvent(input$save_file, {
      cat(sprintf("[tabServer] Save file (%s)\n",id))
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
      cat(sprintf("[tabServer] Overwriting existing file (%s) | Path: %s\n", id, input$out_path))
      removeModal()
      save_file(ns, input, filtered_data, sel)
    })

    proxy <- dataTableProxy("table")

    # 0 and NA represent the row index column
    colOrder <- reactiveVal(0:ncol(data))
    cols <- reactive(c(NA, names(data))[colOrder() + 1])
    # Requires stateSave = TRUE in datatable options
    table_st_cols <- reactive({
      state_df <- data.frame(cbind(name = names(input$table_state$columns),
            do.call(rbind, c(input$table_state$columns, use.names = FALSE))))
      return(state_df)
    })
    vis <- reactive(unlist(table_st_cols()$visible))
    sel <- reactive(c(NA, names(data))[vis()])

    observeEvent(input$colOrder, {
      cat(sprintf("[tabServer] Column order changed (%s)\n", id))
      order <- input$colOrder
      focus <- order[order != 0:ncol(data)]
      order[order != 0:ncol(data)] <- swap_order(focus)
      colOrder(colOrder()[order + 1])
    })

    observeEvent(filtered_data(), {
      data <- filtered_data()
      data <- data[1:min(nrow(data), isolate(display_rows())), ]
      cat(sprintf("[tabServer] Filtered data updated (%s) | Rows: %d\n", id, nrow(data)))
      resetData(proxy, data, selected)
    }, ignoreInit = TRUE)

    observeEvent(input$table_cell_edit, {
      cat(sprintf("[tabServer] Table cell edited (%s)\n", id))
      edit <- input$table_cell_edit
      req(edit)

      data <- isolate(filtered_data())
      #data <- data[1:isolate(display_rows()), ]
      data <- data[1:min(nrow(data), isolate(display_rows())), ]
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
