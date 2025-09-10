# Single-file modular Shiny app (v30 + helper moved inside Data module, no demo dataset)
# - Home: Load TSV and optional semicolon-separated preferred column order
# - Filter: slider filters rows by VALUE (live)
# - Data:
#    - Checkbox grid to pick visible columns (max ~10 rows layout)
#    - selectizeInput used for ordering (drag to reorder)
#    - Buttons: Select all, Select none, Reset preferred, Reset file order,
#      Save as preferred, Update table, Download table, Save table
#    - Draft percent slider (1-100) is committed on Update; Update sets shared_data$display_row_count
#    - Render, download and save use the same helper to ensure WYSIWYG behavior
#
# Run:
# shiny::runApp("app.R", port = 8888, host = "0.0.0.0")

library(shiny)
library(DT)

# small helper: fallback for NULL
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------
# Home module (UI + Server)
# ---------------------------
homeModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(7, textInput(ns("path"), "Path to TSV file (remote filesystem)",
                          placeholder = "/path/to/file.tsv")),
      column(5, textInput(ns("col_order"), "Preferred column order (semicolon-separated)",
                          placeholder = "VALUE;ID"))
    ),
    fluidRow(
      column(12, actionButton(ns("load"), "Load TSV & Columns", class = "btn-primary"))
    ),
    br(),
    tags$div(style = "margin-top: 10px;",
             verbatimTextOutput(ns("status")))
  )
}

homeModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    parse_cols <- function(txt) {
      if (is.null(txt)) return(character(0))
      parts <- unlist(strsplit(txt, ";", fixed = TRUE))
      parts <- trimws(parts)
      parts[parts != ""]
    }

    observeEvent(input$load, {
      path <- trimws(isolate(input$path))
      pref_cols <- parse_cols(isolate(input$col_order))

      if (identical(path, "") || is.null(path)) {
        showNotification("Please provide a file path before clicking Load TSV & Columns.", type = "error")
        return()
      }
      if (!file.exists(path)) {
        showNotification("File not found at the provided path.", type = "error")
        shared_data$data <- NULL
        shared_data$table_filtered <- NULL
        shared_data$preferred_cols <- NULL
        shared_data$display_order <- NULL
        shared_data$file_order <- NULL
        shared_data$committed_visible <- NULL
        shared_data$display_row_count <- NULL
        return()
      }

      tryCatch({
        df <- read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
        shared_data$data <- df
        shared_data$table_filtered <- df

        # store preferred columns (as typed)
        shared_data$preferred_cols <- pref_cols
        file_cols <- colnames(df)
        pref_valid <- intersect(pref_cols, file_cols)          # preserves user's preferred order

        # display_order always stores a full covering order (preferred first if present)
        initial_order <- if (length(pref_valid) > 0) c(pref_valid, setdiff(file_cols, pref_valid)) else file_cols
        shared_data$display_order <- initial_order
        shared_data$file_order <- file_cols  # remember original file order

        # On initial load we commit the initial visible set so that the table shows something without requiring Update.
        if (length(pref_valid) > 0) {
          shared_data$committed_visible <- pref_valid
        } else {
          shared_data$committed_visible <- file_cols
        }

        # initialize display_row_count to full rows of the filtered table
        shared_data$display_row_count <- nrow(df)

        missing <- setdiff(pref_cols, file_cols)
        if (length(missing) > 0 && length(pref_cols) > 0) {
          showNotification(
            sprintf("Some preferred columns not found and ignored: %s", paste(missing, collapse = ", ")),
            type = "warning", duration = 6
          )
        } else {
          showNotification("File and preferred columns loaded successfully.", type = "message")
        }
      }, error = function(e) {
        shared_data$data <- NULL
        shared_data$table_filtered <- NULL
        shared_data$preferred_cols <- NULL
        shared_data$display_order <- NULL
        shared_data$file_order <- NULL
        shared_data$committed_visible <- NULL
        shared_data$display_row_count <- NULL
        showModal(modalDialog(
          title = "Error reading TSV",
          paste("Failed to read TSV:", e$message),
          easyClose = TRUE
        ))
      })
    })

    output$status <- renderText({
      parts <- c()
      if (is.null(shared_data$data)) {
        parts <- c(parts, "No file loaded.")
      } else {
        df <- shared_data$data
        parts <- c(parts, sprintf("Loaded %d rows x %d cols.", nrow(df), ncol(df)))
        parts <- c(parts, sprintf("Available columns: %s", paste(colnames(df), collapse = ", ")))
      }
      pref <- shared_data$preferred_cols
      if (is.null(pref) || length(pref) == 0) {
        parts <- c(parts, "Preferred column order: (none)")
      } else {
        parts <- c(parts, sprintf("Preferred order: %s", paste(pref, collapse = " ; ")))
      }
      paste(parts, collapse = "\n")
    })
  })
}

# ---------------------------
# Filter module (UI + Server)
# ---------------------------
filterModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("threshold"), "Minimum VALUE threshold", min = 0, max = 10, value = 0, step = 0.1),
    tags$div(style = "font-size:90%; color:#666; margin-top:6px;",
             "Adjust the slider to update the filtered table used by the Data tab.")
  )
}

filterModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$threshold, {
      df <- shared_data$data
      if (is.null(df)) {
        shared_data$table_filtered <- NULL
        return()
      }

      if (!("VALUE" %in% colnames(df))) {
        shared_data$table_filtered <- df
        showNotification("Column 'VALUE' not found — no filtering applied.", type = "warning", duration = 4)
        return()
      }

      suppressWarnings(val <- as.numeric(df$VALUE))
      keep <- !is.na(val) & (val >= input$threshold)
      shared_data$table_filtered <- df[keep, , drop = FALSE]
    }, ignoreNULL = FALSE)
  })
}

# ---------------------------
# Data module (UI + Server)
# ---------------------------
dataModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Hide the selectize remove 'x' so selectize acts only as an ordering widget
    tags$head(tags$style(HTML("
      /* Hide the selectize item remove 'x' so users don't try to delete items here */
      .selectize-control .item .remove { display: none !important; }
      .selectize-control .item .remove { cursor: default !important; }

      .selectize-control.multi .selectize-input {
        white-space: normal;
        overflow-x: visible;
        overflow-y: auto;
        max-height: 420px;
      }
      .selectize-control.multi .selectize-input .item {
        display: inline-block;
        white-space: normal;
        margin-right: 6px;
        margin-bottom: 6px;
      }

      .btn-row { display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-top:8px; margin-bottom:8px; }
      .checkbox-grid-label { margin-bottom:6px; display:block; font-weight:600; }
      .checkbox-group .shiny-options-group label { font-size: 13px; }
    "))),
    uiOutput(ns("col_controls")),
    DTOutput(ns("data_table"))
  )
}

dataModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local helper: apply ordering/visibility and row-limiting
    get_committed_view <- function(tbl,
                                   display_order = NULL,
                                   committed_visible = NULL,
                                   display_row_count = NULL,
                                   selected_only = TRUE) {
      if (is.null(tbl)) return(NULL)

      existing <- colnames(tbl)

      effective_order <- if (!is.null(display_order) && length(display_order) > 0L) {
        intersect(display_order, existing)
      } else {
        existing
      }

      if (isTRUE(selected_only)) {
        if (is.null(committed_visible) || length(committed_visible) == 0L) {
          return(tbl[0L, FALSE, drop = FALSE])
        }
        final_cols <- intersect(effective_order, committed_visible)
      } else {
        final_cols <- effective_order
      }

      if (length(final_cols) == 0L) return(tbl[0L, FALSE, drop = FALSE])

      if (is.null(display_row_count)) {
        n_keep <- nrow(tbl)
      } else {
        n_keep <- as.integer(display_row_count)
        if (is.na(n_keep) || n_keep < 0L) n_keep <- 0L
        if (n_keep > nrow(tbl)) n_keep <- nrow(tbl)
      }

      if (n_keep == 0L) {
        return(tbl[0L, final_cols, drop = FALSE])
      }

      rows_idx <- seq_len(n_keep)
      tbl[rows_idx, final_cols, drop = FALSE]
    }

    # Build the layout; choices/selected values are computed in renderUI so the UI is correct whenever the tab is shown.
    output$col_controls <- renderUI({
      df <- shared_data$data
      file_cols <- if (!is.null(df)) colnames(df) else character(0)
      pref <- shared_data$preferred_cols
      pref_valid <- if (!is.null(pref) && length(pref) > 0) intersect(pref, file_cols) else character(0)

      # Checkbox choices always show all file columns (in file order).
      # If preferred present, only those are initially checked; else all checked.
      checkbox_choices <- file_cols
      checkbox_selected <- if (length(pref_valid) > 0) pref_valid else file_cols

      # compute number of columns for grid so there are at most 10 rows
      ncols <- if (length(checkbox_choices) == 0) 1 else ceiling(length(checkbox_choices) / 10)
      if (ncols < 1) ncols <- 1

      tagList(
        # Visible columns (checkbox grid) - kept on top
        tags$div(
          tags$span("Visible columns", class = "checkbox-grid-label"),
          div(style = sprintf("column-count: %d; -webkit-column-count: %d; column-gap: 20px;", ncols, ncols),
              checkboxGroupInput(ns("cols"), NULL, choices = checkbox_choices, selected = checkbox_selected)
          )
        ),
        br(),

        # selectize area - full width row under checkboxes
        tags$div(
          tags$label("Column order (drag to reorder)"),
          div(style = "width:100%;",
              selectizeInput(ns("order_selectize"),
                             NULL,
                             choices = checkbox_selected,
                             selected = checkbox_selected,
                             multiple = TRUE,
                             options = list(plugins = list('drag_drop'),
                                            placeholder = 'Drag to reorder selected items'),
                             width = "100%")
          )
        ),

        # Percent slider (draft) - placed above the action buttons
        div(style = "margin-top:8px;",
            sliderInput(ns("percent_slider"),
                        "Display rows (%) (draft)",
                        min = 1, max = 100, value = 100, step = 1)
        ),

        # buttons in a single horizontal row in requested order
        div(class = "btn-row",
            actionButton(ns("select_all"), "Select all"),
            actionButton(ns("select_none"), "Select none"),
            actionButton(ns("reset_pref"), "Reset to preferred"),
            actionButton(ns("reset_file"), "Reset to file order"),
            actionButton(ns("save_pref"), "Save as preferred"),
            actionButton(ns("update_table"), "Update table", class = "btn-primary"),
            downloadButton(ns("download_table"), "Download table"),
            actionButton(ns("save_table"), "Save table")
        )
      )
    })

    # Helper: compute preference-valid columns in file order
    pref_valid_in_file <- function() {
      df <- shared_data$data
      if (is.null(df)) return(character(0))
      file_cols <- colnames(df)
      pref <- shared_data$preferred_cols
      if (is.null(pref) || length(pref) == 0) return(character(0))
      intersect(pref, file_cols)
    }

    # build_final_table delegates to get_committed_view
    build_final_table <- function(selected_only = TRUE) {
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) return(NULL)
      get_committed_view(
        tbl = tbl,
        display_order = shared_data$display_order,
        committed_visible = shared_data$committed_visible,
        display_row_count = shared_data$display_row_count,
        selected_only = selected_only
      )
    }

    # On file load: ensure server state (display_order, committed_visible, display_row_count) is initialized.
    observeEvent(shared_data$data, {
      df <- shared_data$data
      if (is.null(df)) {
        shared_data$committed_visible <- NULL
        shared_data$display_row_count <- NULL
        return()
      }
      file_cols <- colnames(df)
      pref_valid <- pref_valid_in_file()

      # Ensure display_order and committed_visible are set for initial table rendering
      if (length(pref_valid) > 0) {
        shared_data$display_order <- c(pref_valid, setdiff(file_cols, pref_valid))
        # commit the preferred subset for initial display
        shared_data$committed_visible <- pref_valid
      } else {
        shared_data$display_order <- file_cols
        shared_data$committed_visible <- file_cols
      }

      # initialize display_row_count to full rows of the filtered table
      shared_data$display_row_count <- nrow(shared_data$table_filtered %||% df)
    }, ignoreNULL = FALSE)

    # When checkboxes change, update the selectize selected set to match (but do NOT re-render the table).
    observeEvent(input$cols, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- colnames(df)
      checked <- input$cols
      if (is.null(checked)) checked <- character(0)

      # Keep ordering of checked items consistent with current display_order (so reordering is preserved)
      base_order <- shared_data$display_order
      if (is.null(base_order) || length(base_order) == 0) base_order <- file_cols
      selected_ordered <- intersect(base_order, checked)
      remaining <- setdiff(checked, selected_ordered)
      if (length(remaining) > 0) selected_ordered <- c(selected_ordered, intersect(file_cols, remaining))

      # LIMIT selectize choices to the checked set; selected in the preserved order
      updateSelectizeInput(session, "order_selectize", choices = checked, selected = selected_ordered, server = FALSE)
    }, ignoreNULL = FALSE)

    # Select all / none buttons (operate on the file columns)
    observeEvent(input$select_all, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- colnames(df)
      updateCheckboxGroupInput(session, "cols", selected = file_cols)
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
    })
    observeEvent(input$select_none, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- colnames(df)
      updateCheckboxGroupInput(session, "cols", selected = character(0))
      updateSelectizeInput(session, "order_selectize", choices = character(0), selected = character(0), server = FALSE)
    })

    # Update table: commit the selectize order + checked set and percent slider as display_row_count
    observeEvent(input$update_table, {
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      file_cols <- colnames(df)
      sel_order <- input$order_selectize
      checked <- input$cols

      # If selectize empty but checkboxes have values, use checkboxes to determine order (file order)
      if ((is.null(sel_order) || length(sel_order) == 0) && !is.null(checked) && length(checked) > 0) {
        sel_order <- intersect(file_cols, checked)
      }
      if (is.null(sel_order) || length(sel_order) == 0) {
        showNotification("No columns selected to show in the table. Use the checkboxes to select columns then press Update.", type = "error")
        return()
      }

      # validate against current file columns
      valid <- intersect(sel_order, file_cols)
      if (length(valid) == 0) {
        showNotification("None of the selected columns are present in the current file.", type = "error")
        return()
      }

      # Build server-authoritative display order: selected ordered first, then remaining file columns appended
      new_order <- c(valid, setdiff(file_cols, valid))
      shared_data$display_order <- new_order

      # Commit visible set (what the table will show) <- intersection of checked & valid (in selected order)
      committed_visible <- intersect(valid, if (is.null(checked)) character(0) else checked)
      if (length(committed_visible) == 0) {
        # if user checked nothing but had a selectize selection, use the selectize subset
        committed_visible <- valid
      }
      shared_data$committed_visible <- committed_visible

      # compute display_row_count from percent slider (draft); rounding to nearest integer of current filtered rows
      if (is.null(shared_data$table_filtered)) {
        src_nrow <- 0L
      } else {
        if (length(committed_visible) == 0L) {
          full_committed_tbl <- shared_data$table_filtered[0L, FALSE, drop = FALSE]
        } else {
          full_committed_tbl <- shared_data$table_filtered[, intersect(committed_visible, colnames(shared_data$table_filtered)), drop = FALSE]
        }
        src_nrow <- nrow(full_committed_tbl)
      }

      pct <- as.numeric(input$percent_slider %||% 100)
      if (is.na(pct) || pct < 0) pct <- 100

      if (src_nrow == 0L) {
        n_keep <- 0L
      } else {
        n_keep <- as.integer(round(src_nrow * pct / 100))
        if (n_keep < 1L && pct > 0) n_keep <- 1L
        if (n_keep > src_nrow) n_keep <- src_nrow
      }

      # Commit the integer count into shared_data
      shared_data$display_row_count <- n_keep

      # Ensure selectize reflects committed subset and limit choices to committed_visible
      updateSelectizeInput(session, "order_selectize", choices = committed_visible, selected = committed_visible, server = FALSE)
      showNotification(sprintf("Table updated. Columns will appear in this order: %s. Committed %d rows (%.0f%%).",
                               paste(committed_visible, collapse = ", "), n_keep, pct), type = "message")
    })

    # Save as preferred (in-session only) - UI update and saved to shared_data$preferred_cols
    observeEvent(input$save_pref, {
      sel <- input$order_selectize
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      file_cols <- colnames(df)

      if (is.null(sel) || length(sel) == 0) {
        showNotification("No selection in the selectize to save as preferred.", type = "error")
        return()
      }

      # Save the selected order as preferred (in-session)
      shared_data$preferred_cols <- sel

      # Update UI so the saved order is visible in the draft area (selectize & checkboxes).
      updateCheckboxGroupInput(session, "cols", choices = file_cols, selected = intersect(file_cols, sel))
      updateSelectizeInput(session, "order_selectize", choices = sel, selected = sel, server = FALSE)

      showNotification(sprintf("Saved preferred columns: %s", paste(sel, collapse = ", ")), type = "message", duration = 6)
    })

    # Reset to preferred: update checkboxes/selectize to preferred (UI-only; no table render)
    observeEvent(input$reset_pref, {
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      pref_valid <- pref_valid_in_file()
      if (length(pref_valid) == 0) {
        showNotification("No preferred columns set or none present in this file.", type = "warning")
        return()
      }
      updateCheckboxGroupInput(session, "cols", choices = colnames(df), selected = pref_valid)
      updateSelectizeInput(session, "order_selectize", choices = pref_valid, selected = pref_valid, server = FALSE)
      showNotification("UI reset to preferred columns (table unchanged until Update pressed).", type = "message")
    })

    # Reset to file order: update checkboxes/selectize to file order (UI-only; no table render)
    observeEvent(input$reset_file, {
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      file_cols <- colnames(df)
      updateCheckboxGroupInput(session, "cols", choices = file_cols, selected = file_cols)
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
      showNotification("UI reset to original file order (table unchanged until Update pressed).", type = "message")
    })

    # Download currently displayed (committed) table as TSV using the helper
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("table-", Sys.Date(), ".tsv")
      },
      content = function(file) {
        tbl_src <- shared_data$table_filtered
        if (is.null(tbl_src)) {
          write.table(data.frame(Message = "No filtered table available."), file, sep = "\t", row.names = FALSE, quote = FALSE)
          return()
        }

        committed_view <- get_committed_view(
          tbl = tbl_src,
          display_order = shared_data$display_order,
          committed_visible = shared_data$committed_visible,
          display_row_count = shared_data$display_row_count,
          selected_only = TRUE
        )

        if (is.null(committed_view) || ncol(committed_view) == 0L) {
          write.table(data.frame(Message = "No committed columns to download. Select columns and press Update."), file, sep = "\t", row.names = FALSE, quote = FALSE)
          return()
        }

        write.table(committed_view, file, sep = "\t", row.names = FALSE, quote = FALSE)
      },
      contentType = "text/tab-separated-values"
    )

    # Render the DT using centralized helper (applies committed columns and row-limiting)
    output$data_table <- renderDT({
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        return(datatable(data.frame(Message = "No filtered table available. Load a TSV & Columns on Home and set a threshold on Filter."),
                         options = list(dom = "t")))
      }

      committed_view <- get_committed_view(
        tbl = tbl,
        display_order = shared_data$display_order,
        committed_visible = shared_data$committed_visible,
        display_row_count = shared_data$display_row_count,
        selected_only = TRUE
      )

      if (is.null(committed_view) || ncol(committed_view) == 0L || nrow(committed_view) == 0L) {
        # Show helpful messages depending on reason
        if (is.null(shared_data$committed_visible) || length(shared_data$committed_visible) == 0) {
          return(datatable(data.frame(Message = "No columns have been committed to show. Select columns and press Update."),
                           options = list(dom = "t")))
        }
        if (nrow(committed_view) == 0L) {
          return(datatable(data.frame(Message = "Committed view contains 0 rows (result of filtering/percent)."),
                           options = list(dom = "t")))
        }
        return(datatable(data.frame(Message = "No committed columns available to show. Use the column controls above and press Update."),
                         options = list(dom = "t")))
      }

      datatable(committed_view,
                options = list(dom = 'frtip', pageLength = 15, scrollX = TRUE),
                rownames = FALSE)
    }, server = TRUE)

    # ---- Save table to server path logic ----
    observeEvent(input$save_table, {
      default_name <- paste0("table-", Sys.Date(), ".tsv")
      showModal(modalDialog(
        title = "Save table to server path",
        textInput(ns("save_path"), "Server file path (including filename)", value = default_name),
        checkboxInput(ns("save_allow_overwrite"), "Allow overwrite", value = FALSE),
        checkboxInput(ns("save_selected_only"), "Selected columns only", value = TRUE),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_table_confirm"), "Save")
        ),
        easyClose = FALSE
      ))
    })

    # Helper to actually write table to disk using our centralized helper
    perform_save <- function(path, selected_only) {
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) stop("No filtered table available to save.")

      final_tbl <- get_committed_view(
        tbl = tbl,
        display_order = shared_data$display_order,
        committed_visible = shared_data$committed_visible,
        display_row_count = shared_data$display_row_count,
        selected_only = selected_only
      )

      if (is.null(final_tbl)) stop("No data to save. Ensure the filtered table exists and (if selected-only) that committed columns are set.")

      dirpath <- dirname(path)
      if (!dir.exists(dirpath)) {
        stop(sprintf("Directory '%s' does not exist. The server will not create directories automatically.", dirpath))
      }

      write.table(final_tbl, file = path, sep = "\t", row.names = FALSE, quote = FALSE)
      TRUE
    }

    observeEvent(input$save_table_confirm, {
      req(input$save_path)
      path <- trimws(input$save_path)
      allow_ovr <- isTRUE(input$save_allow_overwrite)
      selected_only <- isTRUE(input$save_selected_only)

      if (identical(path, "") || is.null(path)) {
        showNotification("Please provide a valid server path.", type = "error")
        return()
      }

      if (file.exists(path) && !allow_ovr) {
        removeModal()
        showModal(modalDialog(
          title = "Confirm overwrite",
          sprintf("The file already exists at: %s", path),
          p("Do you want to overwrite it?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("overwrite_yes"), "Yes"),
            actionButton(ns("overwrite_no"), "No")
          ),
          easyClose = FALSE
        ))
        session$userData$pending_save <- list(path = path, selected_only = selected_only)
        return()
      }

      tryCatch({
        perform_save(path, selected_only)
        removeModal()
        showNotification(sprintf("Table saved to %s", path), type = "message", duration = 6)
      }, error = function(e) {
        showNotification(sprintf("Failed to save table: %s", e$message), type = "error", duration = 8)
      })
    })

    observeEvent(input$overwrite_yes, {
      pending <- session$userData$pending_save
      if (is.null(pending) || is.null(pending$path)) {
        removeModal()
        showNotification("No pending save found.", type = "error")
        return()
      }
      path <- pending$path
      selected_only <- pending$selected_only %||% TRUE
      tryCatch({
        perform_save(path, selected_only)
        removeModal()
        showNotification(sprintf("File overwritten: %s", path), type = "message", duration = 6)
      }, error = function(e) {
        removeModal()
        showNotification(sprintf("Failed to overwrite file: %s", e$message), type = "error", duration = 8)
      })
      session$userData$pending_save <- NULL
    })

    observeEvent(input$overwrite_no, {
      pending <- session$userData$pending_save
      prev_path <- if (!is.null(pending$path)) pending$path else paste0("table-", Sys.Date(), ".tsv")
      prev_selected <- if (!is.null(pending$selected_only)) pending$selected_only else TRUE
      session$userData$pending_save <- NULL
      removeModal()
      showModal(modalDialog(
        title = "Save table to server path",
        textInput(ns("save_path"), "Server file path (including filename)", value = prev_path),
        checkboxInput(ns("save_allow_overwrite"), "Allow overwrite", value = FALSE),
        checkboxInput(ns("save_selected_only"), "Selected columns only", value = prev_selected),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_table_confirm"), "Save")
        ),
        easyClose = FALSE
      ))
    })

  })
}

# ---------------------------
# App UI and Server wiring
# ---------------------------
ui <- fluidPage(
  titlePanel("3-Tab Shiny App (checkbox → selectize → Update commit)"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home", homeModuleUI("home")),
    tabPanel("Filter", filterModuleUI("filter")),
    tabPanel("Data", dataModuleUI("data"))
  )
)

server <- function(input, output, session) {
  shared_data <- reactiveValues(
    data = NULL,            # original data from TSV
    table_filtered = NULL,  # final table after applying slider filter
    preferred_cols = NULL,  # user-specified column order (character vector, in-session)
    display_order = NULL,   # current UI display order (server authoritative, updated on Update)
    file_order = NULL,      # original file column order
    committed_visible = NULL, # visible columns committed by user via Update (controls table visibility)
    display_row_count = NULL  # committed integer number of rows to display
  )

  homeModuleServer("home", shared_data)
  filterModuleServer("filter", shared_data)
  dataModuleServer("data", shared_data)
}

# Return app object
shinyApp(ui = ui, server = server)