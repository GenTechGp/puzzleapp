# Single-file Shiny app (modular)
# - Home: Load TSV from path and parse semicolon-separated preferred column order in one button
# - Filter: slider filters rows by VALUE
# - Data:
#    - Checkbox grid (max 10 rows) controls which columns are available in the selectize ordering widget.
#    - selectizeInput only contains checked columns and is used to reorder them (no remove 'x' visible).
#    - Buttons (Select all, Select none, Reset preferred, Reset file order, Save as preferred, Update table, Download table, Save table)
#      appear in a single horizontal line below the selectize.
#    - Update table commits the checked+ordered set to the server (shared_data$display_order and shared_data$committed_visible).
#    - Download table downloads the currently displayed (committed) table as TSV.
#    - Save table saves the currently committed view (or full filtered table) to a server path provided by user as TSV.
#
# Run:
# shiny::runApp("app.R", port = 8888, host = "0.0.0.0")

library(shiny)
library(DT)

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
        # If user provided preferred -> commit only those; else commit all.
        if (length(pref_valid) > 0) {
          shared_data$committed_visible <- pref_valid
        } else {
          shared_data$committed_visible <- file_cols
        }

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
      /* Also prevent cursor change on the small area */
      .selectize-control .item .remove { cursor: default !important; }

      /* Allow selectize items to wrap to multiple rows and limit vertical growth */
      .selectize-control.multi .selectize-input {
        white-space: normal;        /* allow wrapping */
        overflow-x: visible;
        overflow-y: auto;
        max-height: 420px;         /* allow vertical growth up to this limit */
      }
      .selectize-control.multi .selectize-input .item {
        display: inline-block;
        white-space: normal;
        margin-right: 6px;
        margin-bottom: 6px;
      }

      /* Basic spacing for button row */
      .btn-row { display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-top:8px; margin-bottom:8px; }
      /* Slight spacing under checkbox grid title */
      .checkbox-grid-label { margin-bottom:6px; display:block; font-weight:600; }
      /* Make the checkbox grid inputs use slightly smaller font to fit more */
      .checkbox-group .shiny-options-group label { font-size: 13px; }
    "))),
    uiOutput(ns("col_controls")),
    DTOutput(ns("data_table"))
  )
}

dataModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

        # buttons in a single horizontal row in requested order
        div(class = "btn-row",
            actionButton(ns("select_all"), "Select all"),
            actionButton(ns("select_none"), "Select none"),
            actionButton(ns("reset_pref"), "Reset to preferred"),
            actionButton(ns("reset_file"), "Reset to file order"),
            actionButton(ns("save_pref"), "Save as preferred"), # colorless
            actionButton(ns("update_table"), "Update table", class = "btn-primary"),
            downloadButton(ns("download_table"), "Download table"),
            actionButton(ns("save_table"), "Save table") # server-side save
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
      # preserve order of pref when intersecting with file columns
      intersect(pref, file_cols)
    }

    # Helper: compute the final table to be used for download/save/preview.
    # If selected_only == TRUE -> use committed_visible (must be non-empty to download/save)
    # If selected_only == FALSE -> return full filtered table (ordered by display_order if available)
    build_final_table <- function(selected_only = TRUE) {
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) return(NULL)
      existing <- colnames(tbl)

      disp_order <- shared_data$display_order
      if (is.null(disp_order) || length(disp_order) == 0) disp_order <- existing
      disp_order_valid <- intersect(disp_order, existing)
      remainder <- setdiff(existing, disp_order_valid)
      final_order <- c(disp_order_valid, remainder)

      if (selected_only) {
        committed_visible <- shared_data$committed_visible
        if (is.null(committed_visible) || length(committed_visible) == 0) return(NULL)
        final_visible <- intersect(final_order, committed_visible)
        if (length(final_visible) == 0) return(NULL)
        return(tbl[, final_visible, drop = FALSE])
      } else {
        # return full table in display_order
        final_visible <- intersect(final_order, existing)
        return(tbl[, final_visible, drop = FALSE])
      }
    }

    # On file load: ensure server state (display_order, committed_visible) is initialized.
    # We avoid forcing UI updates here because renderUI already populates inputs when shown.
    observeEvent(shared_data$data, {
      df <- shared_data$data
      if (is.null(df)) {
        shared_data$committed_visible <- NULL
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
    }, ignoreNULL = FALSE)

    # When checkboxes change, update the selectize selected set to match (but do NOT re-render the table).
    # Now the selectize choices are restricted to the checked columns only.
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
      # selectize choices = checked (now all)
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
    })
    observeEvent(input$select_none, {
      df <- shared_data$data
      if (is.null(df)) return()
      file_cols <- colnames(df)
      updateCheckboxGroupInput(session, "cols", selected = character(0))
      # selectize choices empty
      updateSelectizeInput(session, "order_selectize", choices = character(0), selected = character(0), server = FALSE)
    })

    # Update table: commit the selectize order + checked set as the authoritative display_order and re-render
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

      # Ensure selectize reflects committed subset and limit choices to committed_visible
      updateSelectizeInput(session, "order_selectize", choices = committed_visible, selected = committed_visible, server = FALSE)
      showNotification(sprintf("Table updated. Columns will appear in this order: %s", paste(committed_visible, collapse = ", ")), type = "message")
    })

    # Save as preferred (in-session only) - colorless button, UI update only, show notification listing saved columns
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

      # Show notification listing the newly saved preferred columns
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
      # Update checkboxes and selectize selection ONLY (do not change committed_visible or display_order)
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
      # Update UI only; do not modify committed_visible or display_order
      updateCheckboxGroupInput(session, "cols", choices = file_cols, selected = file_cols)
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = file_cols, server = FALSE)
      showNotification("UI reset to original file order (table unchanged until Update pressed).", type = "message")
    })

    # Download currently displayed (committed) table as TSV
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("table-", Sys.Date(), ".tsv")
      },
      content = function(file) {
        tbl <- shared_data$table_filtered
        if (is.null(tbl)) {
          # no data: write a small message tsv
          write.table(data.frame(Message = "No filtered table available."), file, sep = "\t", row.names = FALSE, quote = FALSE)
          return()
        }

        disp_order <- shared_data$display_order
        if (is.null(disp_order) || length(disp_order) == 0) disp_order <- colnames(tbl)

        committed_visible <- shared_data$committed_visible
        if (is.null(committed_visible) || length(committed_visible) == 0) {
          # no committed columns: write a small message tsv
          write.table(data.frame(Message = "No committed columns to download. Select columns and press Update."), file, sep = "\t", row.names = FALSE, quote = FALSE)
          return()
        }

        existing <- colnames(tbl)
        disp_order_valid <- intersect(disp_order, existing)
        remainder <- setdiff(existing, disp_order_valid)
        final_order <- c(disp_order_valid, remainder)
        final_visible <- intersect(final_order, committed_visible)

        if (length(final_visible) == 0) {
          write.table(data.frame(Message = "No committed columns to download. Select columns and press Update."), file, sep = "\t", row.names = FALSE, quote = FALSE)
          return()
        }

        final_tbl <- tbl[, final_visible, drop = FALSE]
        write.table(final_tbl, file, sep = "\t", row.names = FALSE, quote = FALSE)
      },
      contentType = "text/tab-separated-values"
    )

    # Render the DT using server-side authoritative visibility + ordering (only on Update or initial commit)
    output$data_table <- renderDT({
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        return(datatable(data.frame(Message = "No filtered table available. Load a TSV & Columns on Home and set a threshold on Filter."),
                         options = list(dom = "t")))
      }

      disp_order <- shared_data$display_order
      if (is.null(disp_order) || length(disp_order) == 0) disp_order <- colnames(tbl)

      # Use committed_visible (what was last committed via Update or initial load) to control visible columns
      committed_visible <- shared_data$committed_visible
      if (is.null(committed_visible) || length(committed_visible) == 0) {
        return(datatable(data.frame(Message = "No columns have been committed to show. Select columns and press Update."),
                         options = list(dom = "t")))
      }

      # compute final order: use disp_order intersect with current table columns, then intersect with committed_visible
      existing <- colnames(tbl)
      disp_order_valid <- intersect(disp_order, existing)
      remainder <- setdiff(existing, disp_order_valid)
      final_order <- c(disp_order_valid, remainder)
      final_visible <- intersect(final_order, committed_visible)

      if (length(final_visible) == 0) {
        return(datatable(data.frame(Message = "No committed columns available to show. Use the column controls above and press Update."),
                         options = list(dom = "t")))
      }

      final_tbl <- tbl[, final_visible, drop = FALSE]

      datatable(final_tbl,
                options = list(dom = 'frtip', pageLength = 15, scrollX = TRUE),
                rownames = FALSE)
    }, server = TRUE)

    # ---- Save table to server path logic ----
    # Open modal to ask for path and options
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

    # Helper to actually write table to disk
    perform_save <- function(path, selected_only) {
      final_tbl <- build_final_table(selected_only = selected_only)
      if (is.null(final_tbl)) {
        stop("No data to save. Ensure the filtered table exists and (if selected-only) that committed columns are set.")
      }

      dirpath <- dirname(path)
      if (!dir.exists(dirpath)) {
        stop(sprintf("Directory '%s' does not exist. The server will not create directories automatically.", dirpath))
      }

      # write as TSV, no rownames, no quotes
      write.table(final_tbl, file = path, sep = "\t", row.names = FALSE, quote = FALSE)
      return(TRUE)
    }

    # Handler when user confirms Save in the save modal
    observeEvent(input$save_table_confirm, {
      req(input$save_path)
      path <- trimws(input$save_path)
      allow_ovr <- isTRUE(input$save_allow_overwrite)
      selected_only <- isTRUE(input$save_selected_only)

      if (identical(path, "") || is.null(path)) {
        showNotification("Please provide a valid server path.", type = "error")
        return()
      }

      # if file exists and overwrite not allowed -> show confirmation modal
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
        # store the necessary information in reactiveValues in session for handling overwrite responses
        session$userData$pending_save <- list(path = path, selected_only = selected_only)
        return()
      }

      # otherwise attempt to save immediately
      tryCatch({
        perform_save(path, selected_only)
        removeModal()
        showNotification(sprintf("Table saved to %s", path), type = "message", duration = 6)
      }, error = function(e) {
        showNotification(sprintf("Failed to save table: %s", e$message), type = "error", duration = 8)
      })
    })

    # Handle overwrite confirmation "Yes"
    observeEvent(input$overwrite_yes, {
      pending <- session$userData$pending_save
      if (is.null(pending) || is.null(pending$path)) {
        removeModal()
        showNotification("No pending save found.", type = "error")
        return()
      }
      path <- pending$path
      selected_only <- pending$selected_only %||% TRUE
      # attempt save with overwrite
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

    # Handle overwrite confirmation "No"
    observeEvent(input$overwrite_no, {
      # cancel pending save, reopen initial save modal with prior values if available
      pending <- session$userData$pending_save
      prev_path <- if (!is.null(pending$path)) pending$path else paste0("table-", Sys.Date(), ".tsv")
      prev_selected <- if (!is.null(pending$selected_only)) pending$selected_only else TRUE
      session$userData$pending_save <- NULL
      removeModal()
      # reopen initial save modal populated
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
# App UI and Server
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
    committed_visible = NULL # visible columns committed by user via Update (controls table visibility)
  )

  homeModuleServer("home", shared_data)
  filterModuleServer("filter", shared_data)
  dataModuleServer("data", shared_data)
}

# Return shiny.appobj so runApp("app.R") works.
shinyApp(ui = ui, server = server)