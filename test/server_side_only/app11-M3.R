# Single-file modular Shiny app (v30 adjustments; bugfix for Reset to preferred ordering)
# - Two percent variables:
#     * shared_data$display_row_pct <- committed by Update table (1..100) and used for rendering/download/save
#     * shared_data$preferred_pct <- saved only by "Save as preferred" and applied only by "Reset to preferred"
# - get_committed_view is local to dataModuleServer and expects display_row_pct
# - No demo data; app requires loading a TSV in Home
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
        shared_data$committed_visible <- NULL
        shared_data$display_row_pct <- NULL
        shared_data$preferred_pct <- NULL
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

        # On initial load we commit the initial visible set so that the table shows something without requiring Update.
        if (length(pref_valid) > 0) {
          shared_data$committed_visible <- pref_valid
        } else {
          shared_data$committed_visible <- file_cols
        }

        # initialize display_row_pct to 100% by default (can be changed on Update)
        shared_data$display_row_pct <- 100L

        # preferred_pct is not used to initialize the slider; initialize to 100 for completeness
        shared_data$preferred_pct <- 100L

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
        shared_data$committed_visible <- NULL
        shared_data$display_row_pct <- NULL
        shared_data$preferred_pct <- NULL
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
    sliderInput(ns("threshold"), "Minimum QUAL threshold", min = 1, max = 100, value = 1, step = 1),
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

      if (!("QUAL" %in% colnames(df))) {
        shared_data$table_filtered <- df
        showNotification("Column 'QUAL' not found — no filtering applied.", type = "warning", duration = 4)
        return()
      }

      suppressWarnings(val <- as.numeric(df$QUAL))
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

    # Local helper: apply ordering/visibility and row-limiting using display_row_pct
    get_committed_view <- function(tbl,
                                   display_order = NULL,
                                   committed_visible = NULL,
                                   display_row_pct = NULL,
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

      # determine number of rows to keep from percent
      if (is.null(display_row_pct)) {
        n_keep <- nrow(tbl)
      } else {
        pct <- as.integer(display_row_pct)
        if (is.na(pct) || pct < 0L) pct <- 0L
        if (pct > 100L) pct <- 100L
        if (pct == 0L) {
          n_keep <- 0L
        } else {
          n_keep <- as.integer(round(nrow(tbl) * pct / 100))
          if (n_keep < 1L && pct > 0L) n_keep <- 1L
          if (n_keep > nrow(tbl)) n_keep <- nrow(tbl)
        }
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

      # Determine slider startup value:
      #  - preserve existing draft input if present (isolate(input$percent_slider))
      #  - else default to 100
      slider_start <- isolate(input$percent_slider) %||% 100

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
                        min = 1, max = 100, value = slider_start, step = 1)
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

    # On file load: ensure server state (display_order, committed_visible, display_row_pct, preferred_pct) is initialized.
    observeEvent(shared_data$data, {
      df <- shared_data$data
      if (is.null(df)) {
        shared_data$committed_visible <- NULL
        shared_data$display_row_pct <- NULL
        shared_data$preferred_pct <- NULL
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

      # initialize display_row_pct to 100 if missing
      if (is.null(shared_data$display_row_pct)) shared_data$display_row_pct <- 100L
      if (is.null(shared_data$preferred_pct)) shared_data$preferred_pct <- 100L
    }, ignoreNULL = FALSE)

    # When checkboxes change, update the selectize selected set to match (but do NOT re-render the table).
    # Improved logic: prefer preserving the current selectize order if it already contains checked items;
    # otherwise fall back to shared_data$display_order. This avoids race conditions with Reset/Save flows.
    observeEvent(input$cols, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- colnames(df)
      checked <- input$cols
      if (is.null(checked)) checked <- character(0)

      # Prefer to preserve existing selectize ordering if it contains checked items.
      current_sel <- isolate(input$order_selectize)
      selected_ordered <- character(0)

      if (!is.null(current_sel) && length(intersect(current_sel, checked)) > 0L) {
        # preserve the order from the current selectize value (only include checked items)
        selected_ordered <- intersect(current_sel, checked)
        remaining <- setdiff(checked, selected_ordered)
        if (length(remaining) > 0) selected_ordered <- c(selected_ordered, intersect(file_cols, remaining))
      } else {
        # fallback: preserve shared_data$display_order ordering
        base_order <- shared_data$display_order
        if (is.null(base_order) || length(base_order) == 0) base_order <- file_cols
        selected_ordered <- intersect(base_order, checked)
        remaining <- setdiff(checked, selected_ordered)
        if (length(remaining) > 0) selected_ordered <- c(selected_ordered, intersect(file_cols, remaining))
      }

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

    # Update table: commit the selectize order + checked set and percent slider as shared_data$display_row_pct
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

      # compute display_row_pct from percent slider (draft); sanitize to 1..100
      pct <- as.numeric(input$percent_slider %||% 100)
      if (is.na(pct) || pct < 0) pct <- 100
      pct_int <- as.integer(round(pct))
      if (pct_int < 1L) pct_int <- 1L
      if (pct_int > 100L) pct_int <- 100L

      # Commit the percent into shared_data (this is the committed display percent)
      shared_data$display_row_pct <- pct_int

      # Compute n_keep for messaging (based on filtered rows)
      src_nrow <- if (is.null(shared_data$table_filtered)) 0L else nrow(shared_data$table_filtered)
      if (src_nrow == 0L) {
        n_keep <- 0L
      } else {
        n_keep <- as.integer(round(src_nrow * pct_int / 100))
        if (n_keep < 1L && pct_int > 0) n_keep <- 1L
        if (n_keep > src_nrow) n_keep <- src_nrow
      }

      # Ensure selectize reflects committed subset and limit choices to committed_visible
      updateSelectizeInput(session, "order_selectize", choices = committed_visible, selected = committed_visible, server = FALSE)
      showNotification(sprintf("Table updated. Columns will appear in this order: %s. Committed %d rows (%d%%).",
                               paste(committed_visible, collapse = ", "), n_keep, pct_int), type = "message")
    })

    # Save as preferred (in-session only) - UI update and saved to shared_data$preferred_cols and preferred_pct
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

      # Capture current draft percent (before renderUI is triggered)
      current_pct <- as.integer(isolate(input$percent_slider) %||% 100)
      if (is.na(current_pct) || current_pct < 1) current_pct <- 1L
      if (current_pct > 100) current_pct <- 100L

      # Save the selected order as preferred (in-session)
      shared_data$preferred_cols <- sel
      # Save the preferred percent so Reset to preferred can use it
      shared_data$preferred_pct <- current_pct

      # Update UI so the saved order is visible in the draft area (selectize & checkboxes).
      updateCheckboxGroupInput(session, "cols", choices = file_cols, selected = intersect(file_cols, sel))
      updateSelectizeInput(session, "order_selectize", choices = sel, selected = sel, server = FALSE)

      showNotification(sprintf("Saved preferred columns: %s and preferred percent: %d%%", paste(sel, collapse = ", "), current_pct),
                   type = "message", duration = 6)
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
      # Update selectize first (so checkbox observer can preserve this ordering if it runs)
      updateSelectizeInput(session, "order_selectize", choices = pref_valid, selected = pref_valid, server = FALSE)
      updateCheckboxGroupInput(session, "cols", choices = colnames(df), selected = pref_valid)
      # apply preferred percent to the draft slider (preferred_pct is only used here)
      if (!is.null(shared_data$preferred_pct)) {
        updateSliderInput(session, "percent_slider", value = shared_data$preferred_pct)
      }
      showNotification("UI reset to preferred columns and preferred percent (table unchanged until Update pressed).", type = "message")
    })

    # Reset to file order: update checkboxes/selectize to file order (UI-only; no table render)
    observeEvent(input$reset_file, {
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      file_cols <- colnames(df)
      # update selectize first to avoid checkbox observer overriding preferred ordering
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
      updateCheckboxGroupInput(session, "cols", choices = file_cols, selected = file_cols)
      showNotification("UI reset to original file order (table unchanged until Update pressed).", type = "message")
    })

    # Download currently displayed (committed) table as TSV using the helper (uses shared_data$display_row_pct)
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
          display_row_pct = shared_data$display_row_pct,
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

    # Render the DT using centralized helper (applies committed columns and percent-based row-limiting)
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
        display_row_pct = shared_data$display_row_pct,
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
        display_row_pct = shared_data$display_row_pct,
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
    data = NULL,             # original data from TSV
    table_filtered = NULL,   # final table after applying slider filter
    preferred_cols = NULL,   # user-specified column order (character vector, in-session)
    display_order = NULL,    # current UI display order (server authoritative, updated on Update)
    committed_visible = NULL,# visible columns committed by user via Update (controls table visibility)
    display_row_pct = NULL,  # committed percent (1..100) set by Update and used for slicing
    preferred_pct = NULL     # persisted preferred percent (1..100) set only by Save as preferred
  )

  homeModuleServer("home", shared_data)
  filterModuleServer("filter", shared_data)
  dataModuleServer("data", shared_data)
}

# Return app object
shinyApp(ui = ui, server = server)