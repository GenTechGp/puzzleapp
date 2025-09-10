# Single-file modular Shiny app with inline editable cells (extensible)
# - Uses shared_data$editable_schema as the canonical source of editable columns and validators.
# - Uses a monotonic shared_data$load_counter to signal genuine file loads (robust vs boolean toggle).
# - Client-side: only columns present in shared_data$editable_schema (and visible in the committed view)
#   are editable in the browser (DT editable disable list computed dynamically).
# - Server-side: edits are accepted only for columns present in shared_data$editable_schema (defence-in-depth).
# - apply_to_source checkbox moved to static UI so its value persists across control re-renders.
# - Uses an internal .rowid to map rows reliably.
# - Uses dataTableProxy + replaceData for in-place updates and includes debug counters to detect re-renders.
#
# Run:
# shiny::runApp("app.R")

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
        # clear state
        shared_data$data <- NULL
        shared_data$table_filtered <- NULL
        shared_data$preferred_cols <- NULL
        shared_data$display_order <- NULL
        shared_data$committed_visible <- NULL
        shared_data$display_row_pct <- NULL
        shared_data$preferred_pct <- NULL
        shared_data$editable_schema <- list()
        return()
      }

      # read and initialize inside tryCatch; only increment load_counter on successful load
      tryCatch({
        df <- read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

        # add stable internal row id so edits can be mapped back to original rows
        df$.rowid <- seq_len(nrow(df))

        # assign data and filtered copy
        shared_data$data <- df
        shared_data$table_filtered <- df

        # store preferred columns (as typed)
        shared_data$preferred_cols <- pref_cols
        file_cols <- colnames(df)
        # exclude internal column when building preferred list handling elsewhere
        pref_valid <- intersect(pref_cols, setdiff(file_cols, ".rowid"))

        # display_order always stores a full covering order (preferred first if present).
        # Note: display_order refers to visible column names (not including ".rowid")
        initial_order <- if (length(pref_valid) > 0) c(pref_valid, setdiff(setdiff(file_cols, ".rowid"), pref_valid)) else setdiff(file_cols, ".rowid")
        shared_data$display_order <- initial_order

        # On initial load we commit the initial visible set so that the table shows something without requiring Update.
        if (length(pref_valid) > 0) {
          shared_data$committed_visible <- pref_valid
        } else {
          shared_data$committed_visible <- setdiff(file_cols, ".rowid")
        }

        # initialize display_row_pct to 100% by default (can be changed on Update)
        shared_data$display_row_pct <- 100L

        # preferred_pct is not used to initialize the slider; initialize to 100 for completeness
        shared_data$preferred_pct <- 100L

        # initialize editable schema (extendable). For now validate QUAL as numeric 0..100.
        # Validator contract: function(raw_value) -> list(ok=TRUE/FALSE, coerced=<value if ok>, message=<string if not ok>)
        shared_data$editable_schema <- list(
          QUAL = function(value) {
            v <- suppressWarnings(as.numeric(value))
            if (is.na(v)) return(list(ok = FALSE, message = "Value must be numeric"))
            if (v < 0 || v > 100) return(list(ok = FALSE, message = "Value must be between 0 and 100"))
            return(list(ok = TRUE, coerced = v))
          }
        )

        # successful load: bump the load counter so data module runs initialization for a true load
        shared_data$load_counter <- shared_data$load_counter + 1L

        missing <- setdiff(pref_cols, setdiff(file_cols, ".rowid"))
        if (length(missing) > 0 && length(pref_cols) > 0) {
          showNotification(
            sprintf("Some preferred columns not found and ignored: %s", paste(missing, collapse = ", ")),
            type = "warning", duration = 6
          )
        } else {
          showNotification("File and preferred columns loaded successfully.", type = "message")
        }
      }, error = function(e) {
        # on error, clear partial state and do not increment the load counter
        shared_data$data <- NULL
        shared_data$table_filtered <- NULL
        shared_data$preferred_cols <- NULL
        shared_data$display_order <- NULL
        shared_data$committed_visible <- NULL
        shared_data$display_row_pct <- NULL
        shared_data$preferred_pct <- NULL
        shared_data$editable_schema <- list()
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
        # exclude internal .rowid from displayed column count
        ncols_display <- max(0, ncol(df) - (".rowid" %in% colnames(df)))
        parts <- c(parts, sprintf("Loaded %d rows x %d cols.", nrow(df), ncols_display))
        parts <- c(parts, sprintf("Available columns: %s", paste(setdiff(colnames(df), ".rowid"), collapse = ", ")))
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
    # Static area: controls that should persist (apply_to_source moved here)
    div(style = "margin-bottom:8px;",
        checkboxInput(ns("apply_to_source"), "Apply edits to original data", value = FALSE)
    ),
    uiOutput(ns("col_controls")),
    DTOutput(ns("data_table"))
  )
}

dataModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local helper: apply ordering/visibility and row-limiting using display_row_pct
    # Ensure .rowid is preserved (and placed first) to map edits reliably.
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
        # default to all non-.rowid columns in file order
        setdiff(existing, ".rowid")
      }

      if (isTRUE(selected_only)) {
        if (is.null(committed_visible) || length(committed_visible) == 0L) {
          # still include .rowid so mapping is always possible; but no visible columns
          if (".rowid" %in% existing) {
            return(tbl[0L, ".rowid", drop = FALSE])
          } else {
            return(tbl[0L, FALSE, drop = FALSE])
          }
        }
        final_cols <- intersect(effective_order, committed_visible)
      } else {
        final_cols <- effective_order
      }

      if (length(final_cols) == 0L) {
        # include .rowid if present (first)
        if (".rowid" %in% existing) {
          return(tbl[0L, ".rowid", drop = FALSE])
        }
        return(tbl[0L, FALSE, drop = FALSE])
      }

      # Ensure .rowid present and placed first
      if (".rowid" %in% existing) {
        cols_with_rowid <- unique(c(".rowid", final_cols))
      } else {
        cols_with_rowid <- final_cols
      }

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
        return(tbl[0L, cols_with_rowid, drop = FALSE])
      }

      rows_idx <- seq_len(n_keep)
      tbl[rows_idx, cols_with_rowid, drop = FALSE]
    }

    # Build the controls UI; choices/selected values are computed in renderUI so UI reflects state.
    output$col_controls <- renderUI({
      df <- shared_data$data
      file_cols <- if (!is.null(df)) setdiff(colnames(df), ".rowid") else character(0)

      # committed and preferred values (may be NULL)
      committed_visible <- shared_data$committed_visible
      committed_order <- shared_data$display_order
      committed_pct <- shared_data$display_row_pct
      preferred_cols <- shared_data$preferred_cols
      preferred_pct <- shared_data$preferred_pct

      # current draft inputs (may be NULL on first render)
      current_checked <- isolate(input$cols)
      current_selectize <- isolate(input$order_selectize)
      current_slider <- isolate(input$percent_slider)

      # --- Slider value: draft -> committed -> preferred -> 100 ---
      if (!is.null(current_slider)) {
        slider_start <- as.numeric(current_slider)
        if (is.na(slider_start)) {
          slider_start <- if (!is.null(committed_pct)) as.numeric(committed_pct) %||% NA else NA
          if (is.na(slider_start) && !is.null(preferred_pct)) slider_start <- as.numeric(preferred_pct)
        }
      } else if (!is.null(committed_pct)) {
        slider_start <- as.numeric(committed_pct)
      } else if (!is.null(preferred_pct)) {
        slider_start <- as.numeric(preferred_pct)
      } else {
        slider_start <- 100
      }
      if (is.na(slider_start)) slider_start <- 100
      slider_start <- as.integer(round(slider_start))
      if (is.na(slider_start) || slider_start < 1L) slider_start <- 1L
      if (slider_start > 100L) slider_start <- 100L

      # --- Checkbox selected: draft -> committed -> preferred -> file_cols ---
      if (!is.null(current_checked) && length(current_checked) > 0) {
        checkbox_selected <- intersect(current_checked, file_cols)
        # if draft becomes empty after filtering, fall back to committed/preferred/file
        if (length(checkbox_selected) == 0L) {
          if (!is.null(committed_visible) && length(committed_visible) > 0) {
            checkbox_selected <- intersect(committed_visible, file_cols)
          } else if (!is.null(preferred_cols) && length(preferred_cols) > 0) {
            checkbox_selected <- intersect(preferred_cols, file_cols)
          } else {
            checkbox_selected <- file_cols
          }
        }
      } else if (!is.null(committed_visible) && length(committed_visible) > 0) {
        checkbox_selected <- intersect(committed_visible, file_cols)
        if (length(checkbox_selected) == 0L && !is.null(preferred_cols) && length(preferred_cols) > 0) {
          checkbox_selected <- intersect(preferred_cols, file_cols)
        }
        if (length(checkbox_selected) == 0L) checkbox_selected <- file_cols
      } else if (!is.null(preferred_cols) && length(preferred_cols) > 0) {
        checkbox_selected <- intersect(preferred_cols, file_cols)
        if (length(checkbox_selected) == 0L) checkbox_selected <- file_cols
      } else {
        checkbox_selected <- file_cols
      }

      # ensure checkbox_choices is file_cols (always)
      checkbox_choices <- file_cols

      # --- Selectize selected (ordering): draft -> committed order restricted -> preferred restricted -> file order ---
      if (!is.null(current_selectize) && length(current_selectize) > 0) {
        sel_order_preserved <- intersect(current_selectize, checkbox_selected)
        remaining <- setdiff(checkbox_selected, sel_order_preserved)
        if (length(remaining) > 0) sel_order_preserved <- c(sel_order_preserved, intersect(file_cols, remaining))
        selectize_selected <- sel_order_preserved
      } else if (!is.null(committed_order) && length(committed_order) > 0) {
        selectize_selected <- intersect(committed_order, checkbox_selected)
        remaining <- setdiff(checkbox_selected, selectize_selected)
        if (length(remaining) > 0) selectize_selected <- c(selectize_selected, intersect(file_cols, remaining))
        if (length(selectize_selected) == 0L && !is.null(preferred_cols) && length(preferred_cols) > 0) {
          selectize_selected <- intersect(preferred_cols, checkbox_selected)
          if (length(selectize_selected) == 0L) selectize_selected <- intersect(file_cols, checkbox_selected)
        }
      } else if (!is.null(preferred_cols) && length(preferred_cols) > 0) {
        selectize_selected <- intersect(preferred_cols, checkbox_selected)
        remaining <- setdiff(checkbox_selected, selectize_selected)
        if (length(remaining) > 0) selectize_selected <- c(selectize_selected, intersect(file_cols, remaining))
        if (length(selectize_selected) == 0L) selectize_selected <- intersect(file_cols, checkbox_selected)
      } else {
        selectize_selected <- intersect(file_cols, checkbox_selected)
      }

      # compute number of columns for grid so there are at most 10 rows
      ncols <- if (length(checkbox_choices) == 0) 1 else ceiling(length(checkbox_choices) / 10)
      if (ncols < 1) ncols <- 1

      tagList(
        # Visible columns (checkbox grid)
        tags$div(
          tags$span("Visible columns", class = "checkbox-grid-label"),
          div(style = sprintf("column-count: %d; -webkit-column-count: %d; column-gap: 20px;", ncols, ncols),
              checkboxGroupInput(ns("cols"), NULL, choices = checkbox_choices, selected = checkbox_selected)
          )
        ),
        br(),

        # selectize area - ordering widget
        tags$div(
          tags$label("Column order (drag to reorder)"),
          div(style = "width:100%;",
              selectizeInput(ns("order_selectize"),
                            NULL,
                            choices = checkbox_selected,
                            selected = selectize_selected,
                            multiple = TRUE,
                            options = list(plugins = list('drag_drop'),
                                            placeholder = 'Drag to reorder selected items'),
                            width = "100%")
          )
        ),

        # Percent slider (draft)
        div(style = "margin-top:8px;",
            sliderInput(ns("percent_slider"),
                        "Display rows (%) (draft)",
                        min = 1, max = 100, value = slider_start, step = 1)
        ),

        # buttons
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
      file_cols <- setdiff(colnames(df), ".rowid")
      pref <- shared_data$preferred_cols
      if (is.null(pref) || length(pref) == 0) return(character(0))
      intersect(pref, file_cols)
    }

    # On true file load: initialize server state for the new file
    observeEvent(shared_data$load_counter, {
      # Only initialize if shared_data$data exists (defensive)
      req(shared_data$data)

      df <- shared_data$data
      file_cols <- setdiff(colnames(df), ".rowid")
      pref_valid <- pref_valid_in_file()

      if (length(pref_valid) > 0) {
        shared_data$display_order <- c(pref_valid, setdiff(file_cols, pref_valid))
        shared_data$committed_visible <- pref_valid
      } else {
        shared_data$display_order <- file_cols
        shared_data$committed_visible <- file_cols
      }

      if (is.null(shared_data$display_row_pct)) shared_data$display_row_pct <- 100L
      if (is.null(shared_data$preferred_pct)) shared_data$preferred_pct <- 100L
      if (is.null(shared_data$editable_schema)) shared_data$editable_schema <- list()

      # Note: do not modify input$apply_to_source here; persistence chosen by user
    }, ignoreNULL = TRUE)

    # When checkboxes change, update selectize selected set to match (but do NOT re-render the table).
    observeEvent(input$cols, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- setdiff(colnames(df), ".rowid")
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

      updateSelectizeInput(session, "order_selectize", choices = checked, selected = selected_ordered, server = FALSE)
    }, ignoreNULL = FALSE)

    # Select all / none buttons
    observeEvent(input$select_all, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- setdiff(colnames(df), ".rowid")
      updateCheckboxGroupInput(session, "cols", selected = file_cols)
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
    })
    observeEvent(input$select_none, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- setdiff(colnames(df), ".rowid")
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
      file_cols <- setdiff(colnames(df), ".rowid")
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

    # Save as preferred (in-session only)
    observeEvent(input$save_pref, {
      sel <- input$order_selectize
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      file_cols <- setdiff(colnames(df), ".rowid")

      if (is.null(sel) || length(sel) == 0) {
        showNotification("No selection in the selectize to save as preferred.", type = "error")
        return()
      }

      # Capture current draft percent
      current_pct <- as.integer(isolate(input$percent_slider) %||% 100)
      if (is.na(current_pct) || current_pct < 1) current_pct <- 1L
      if (current_pct > 100) current_pct <- 100L

      shared_data$preferred_cols <- sel
      shared_data$preferred_pct <- current_pct

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
      updateCheckboxGroupInput(session, "cols", choices = colnames(df)[colnames(df) != ".rowid"], selected = pref_valid)
      # apply preferred percent to the draft slider
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
      file_cols <- setdiff(colnames(df), ".rowid")
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
      updateCheckboxGroupInput(session, "cols", choices = file_cols, selected = file_cols)
      showNotification("UI reset to original file order (table unchanged until Update pressed).", type = "message")
    })

    # ---- datatable rendering (editable) ----
    # render counter to detect re-renders for debugging
    render_counter <- reactiveVal(0)

    output$data_table <- renderDT({
      # Increment render counter so we can observe whether renderDT is re-run
      render_counter(isolate(render_counter()) + 1)
      message(sprintf("[DEBUG] renderDT executed; count = %d", render_counter()))

      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        return(datatable(
          data.frame(Message = "No filtered table available. Load a TSV & Columns on Home and set a threshold on Filter."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      # committed view includes .rowid so edits can be mapped. get_committed_view places .rowid first.
      committed_view <- get_committed_view(
        tbl = tbl,
        display_order = shared_data$display_order,
        committed_visible = shared_data$committed_visible,
        display_row_pct = shared_data$display_row_pct,
        selected_only = TRUE
      )

      if (is.null(committed_view) || ncol(committed_view) == 0L || nrow(committed_view) == 0L) {
        if (is.null(shared_data$committed_visible) || length(shared_data$committed_visible) == 0) {
          return(datatable(
            data.frame(Message = "No columns have been committed to show. Select columns and press Update."),
            options = list(dom = "t"),
            rownames = FALSE
          ))
        }
        if (nrow(committed_view) == 0L) {
          return(datatable(
            data.frame(Message = "Committed view contains 0 rows (result of filtering/percent)."),
            options = list(dom = "t"),
            rownames = FALSE
          ))
        }
        return(datatable(
          data.frame(Message = "No committed columns available to show. Use the column controls above and press Update."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      # Determine editable columns from shared_data$editable_schema (canonical source).
      # Compute their positions in the committed_view and convert to DT's 0-based indices.
      editable_names <- names(shared_data$editable_schema)
      if (is.null(editable_names)) editable_names <- character(0)
      # Keep only those editable names that are visible in the committed view
      editable_visible <- intersect(editable_names, colnames(committed_view))
      # R 1-based positions of editable_visible in committed_view
      editable_pos1 <- which(colnames(committed_view) %in% editable_visible)
      # Convert to DT 0-based indices
      editable_pos0 <- if (length(editable_pos1) > 0L) editable_pos1 - 1L else integer(0)

      # All column indices in DT (0-based)
      ncols <- ncol(committed_view)
      all_idx0 <- if (ncols > 0L) seq(0, ncols - 1) else integer(0)

      # editable_disable should be the set of columns not editable (0-based).
      # If there are no editable columns visible, disable all columns.
      if (length(editable_pos0) == 0L) {
        editable_disable <- all_idx0
      } else {
        editable_disable <- setdiff(all_idx0, editable_pos0)
      }

      # Render with .rowid hidden, editable cells enabled only for columns declared editable in shared_data$editable_schema.
      # Disable ordering to keep row indices stable for mapping edits.
      datatable(
        committed_view,
        # filter = list(position = "top", clear = TRUE),
        # filter = "top",
        editable = list(target = "cell", disable = list(columns = editable_disable)),
        options = list(dom = 'lfrtip', pageLength = 15, scrollX = TRUE, ordering = TRUE,
        # options = list(dom = 'rtip', scrollX = FALSE, ordering = TRUE,
                       columnDefs = list(list(visible = FALSE, targets = 0))), # hide .rowid (first column)
        rownames = FALSE
      )
    }, server = TRUE)

    # Create a proxy for efficient updates after edits
    proxy <- dataTableProxy("data_table", session = session)

    # ---- Handle in-place edits from DT ----
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      if (is.null(info)) return()
      # DT provides 1-based row and 0-based column indices
      row_idx <- info$row
      col_idx <- info$col
      new_val_raw <- info$value

      message(sprintf("[DEBUG] cell_edit received: row=%s (1-based), col=%s (0-based), value=%s", row_idx, col_idx, as.character(new_val_raw)))

      # Capture render count before replaceData to detect re-renders
      before_render_count <- render_counter()
      message(sprintf("[DEBUG] before replaceData, render_count = %d", before_render_count))

      # Reconstruct current committed view (must match what's shown)
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        showNotification("No data to edit.", type = "error")
        return()
      }
      committed_view <- get_committed_view(
        tbl = tbl,
        display_order = shared_data$display_order,
        committed_visible = shared_data$committed_visible,
        display_row_pct = shared_data$display_row_pct,
        selected_only = TRUE
      )

      # Bounds check: row_idx is 1-based; col_idx is 0-based, so valid columns are 0..(ncol-1)
      if (is.null(committed_view) ||
          nrow(committed_view) < row_idx ||
          !is.numeric(col_idx) ||
          col_idx < 0 ||
          (col_idx + 1) > ncol(committed_view)) {
        # Revert to current view
        replaceData(proxy, committed_view, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      # Map 0-based DT column index to R 1-based column index
      col_name <- colnames(committed_view)[col_idx + 1]

      # Server-side guard: only columns listed in shared_data$editable_schema are accepted.
      # This prevents client-side tampering or accidental edits to non-editable columns.
      if (!(col_name %in% names(shared_data$editable_schema))) {
        showNotification(sprintf("Column '%s' is not editable.", col_name), type = "error")
        replaceData(proxy, committed_view, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      # Do not allow editing of .rowid column (internal)
      if (identical(col_name, ".rowid")) {
        showNotification("Cannot edit internal row id.", type = "error")
        replaceData(proxy, committed_view, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      # If there is a validator for this column in the editable_schema, run it (should exist because of guard above).
      validator <- shared_data$editable_schema[[col_name]]
      coerced_value <- NULL
      if (!is.null(validator) && is.function(validator)) {
        res <- tryCatch(validator(new_val_raw), error = function(e) list(ok = FALSE, message = e$message))
        if (!is.list(res) || is.null(res$ok)) {
          # Unexpected validator result - reject edit conservatively
          showNotification(sprintf("Validation failed for %s", col_name), type = "error")
          replaceData(proxy, committed_view, resetPaging = FALSE, rownames = FALSE)
          return()
        }
        if (!isTRUE(res$ok)) {
          msg <- if (!is.null(res$message)) res$message else "Invalid value"
          showNotification(sprintf("Invalid edit for '%s': %s", col_name, msg), type = "error")
          replaceData(proxy, committed_view, resetPaging = FALSE, rownames = FALSE)
          return()
        }
        coerced_value <- res$coerced %||% new_val_raw
      } else {
        # As a safety fallback (shouldn't happen because of guard), reject edits
        showNotification(sprintf("Column '%s' is not editable.", col_name), type = "error")
        replaceData(proxy, committed_view, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      # Find the .rowid for the edited row in the committed_view
      rowid_val <- committed_view[row_idx, ".rowid", drop = TRUE]

      # Update shared_data$table_filtered (always) by matching .rowid
      idx_filtered <- which(shared_data$table_filtered$.rowid == rowid_val)
      if (length(idx_filtered) > 0L) {
        # preserve column type by coercing appropriately if coerced_value is numeric
        shared_data$table_filtered[idx_filtered, col_name] <- coerced_value
      } else {
        message(sprintf("[DEBUG] rowid %s not found in table_filtered", as.character(rowid_val)))
      }

      # Only write back to original shared_data$data if the checkbox is checked
      apply_to_source <- isTRUE(input$apply_to_source)
      if (apply_to_source && !is.null(shared_data$data)) {
        idx_data <- which(shared_data$data$.rowid == rowid_val)
        if (length(idx_data) > 0L) {
          shared_data$data[idx_data, col_name] <- coerced_value
          showNotification(sprintf("Updated %s for row %s in original data", col_name, rowid_val), type = "message", duration = 2)
        } else {
          message(sprintf("[DEBUG] rowid %s not found in shared_data$data", as.character(rowid_val)))
          showNotification(sprintf("Edit applied to view but could not find original row (%s) to update.", as.character(rowid_val)), type = "warning")
        }
      } else {
        showNotification(sprintf("Updated %s for row %s (filtered view only)", col_name, rowid_val), type = "message", duration = 2)
      }

      # Rebuild authoritative committed_view and push to client to reflect authoritative value
      committed_view2 <- get_committed_view(
        tbl = shared_data$table_filtered,
        display_order = shared_data$display_order,
        committed_visible = shared_data$committed_visible,
        display_row_pct = shared_data$display_row_pct,
        selected_only = TRUE
      )

      # Use replaceData to update in-place (avoid re-rendering the widget)
      replaceData(proxy, committed_view2, resetPaging = FALSE, rownames = FALSE)
      message("[DEBUG] replaceData called to sync authoritative data to client")

      # Check whether renderDT re-ran as a result (it should not be immediate because reactive invalidation happens after)
      after_render_count <- render_counter()
      message(sprintf("[DEBUG] after replaceData, render_count = %d", after_render_count))
      if (after_render_count == before_render_count) {
        message("[DEBUG] replaceData did NOT trigger renderDT to re-run (expected).")
      } else {
        message("[DEBUG] replaceData appears to have triggered renderDT to re-run (unexpected).")
      }
    }, ignoreInit = TRUE)

    # ---- Download currently displayed (committed) table as TSV using the helper ----
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

        # remove internal .rowid column before writing
        if (".rowid" %in% colnames(committed_view)) {
          out_tbl <- committed_view[, setdiff(colnames(committed_view), ".rowid"), drop = FALSE]
        } else {
          out_tbl <- committed_view
        }

        write.table(out_tbl, file, sep = "\t", row.names = FALSE, quote = FALSE)
      },
      contentType = "text/tab-separated-values"
    )

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

      # remove internal .rowid column before writing
      if (".rowid" %in% colnames(final_tbl)) {
        final_tbl <- final_tbl[, setdiff(colnames(final_tbl), ".rowid"), drop = FALSE]
      }

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
    data = NULL,             # original data from TSV (will include .rowid)
    table_filtered = NULL,   # final table after applying slider filter (includes .rowid)
    preferred_cols = NULL,   # user-specified column order (character vector, in-session)
    display_order = NULL,    # current UI display order (server authoritative, updated on Update)
    committed_visible = NULL,# visible columns committed by user via Update (controls table visibility)
    display_row_pct = NULL,  # committed percent (1..100) set by Update and used for slicing
    preferred_pct = NULL,    # persisted preferred percent (1..100) set only by Save as preferred
    editable_schema = list(),# per-column validators/coercers; filled on load
    load_counter = 0L        # monotonic counter incremented on true file load
  )

  homeModuleServer("home", shared_data)
  filterModuleServer("filter", shared_data)
  dataModuleServer("data", shared_data)
}

# Return app object
shinyApp(ui = ui, server = server)