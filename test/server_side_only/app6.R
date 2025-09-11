# Single-file Shiny app (modular)
# - Home: Load TSV from path and parse semicolon-separated preferred column order in one button
# - Filter: slider filters rows by VALUE (no preview)
# - Data:
#    - server-authoritative visibility (checkboxes)
#    - selectizeInput for ordering (all columns available as choices)
#    - explicit "Update order" button to apply the selectize order to the server
#    - "Select all" / "Select none" for checkboxes (server-side authoritative)
#    - No Move up/Move down fallback widget (removed as requested)
#
# Behavior changes applied per user request:
# 1) Removed fallback move up/down UI and related server observers.
# 2) Ensure selectize initial selection respects user's preferred list:
#    - If user provided preferred columns -> selectize selected = preferred_valid (only those),
#      and checkboxes choices are the preferred_valid and only those are ticked.
#    - Else (no preferred) -> selectize selected = full file order (all items selected),
#      and checkboxes show all columns ticked.
# 3) Update button applies the selected order (the ordered vector from selectize).
#    The server sets shared_data$display_order <- c(valid_selected, setdiff(file_cols, valid_selected))
#    so the selected columns are placed first in the requested order, remaining columns appended.
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
        return()
      }

      tryCatch({
        df <- read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
        shared_data$data <- df
        shared_data$table_filtered <- df

        # store preferred columns (as typed) and compute initial display order
        shared_data$preferred_cols <- pref_cols
        file_cols <- colnames(df)
        pref_valid <- intersect(pref_cols, file_cols)          # preserves user's preferred order
        # display_order always stores a full covering order (preferred first if present)
        initial_order <- if (length(pref_valid) > 0) c(pref_valid, setdiff(file_cols, pref_valid)) else file_cols
        shared_data$display_order <- initial_order
        shared_data$file_order <- file_cols  # remember original file order

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
    uiOutput(ns("col_controls")),
    DTOutput(ns("data_table"))
  )
}

dataModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Column controls: visibility checkboxes, selectize ordering with Update,
    # select all / select none for checkboxes
    output$col_controls <- renderUI({
      df <- shared_data$data
      if (is.null(df)) {
        tags$div(style = "color:#666;", "No data loaded. Load TSV & Columns on the Home tab to get column controls.")
      } else {
        cols <- colnames(df)
        display_order <- shared_data$display_order
        if (is.null(display_order) || length(display_order) == 0) display_order <- cols

        tagList(
          fluidRow(
            column(6,
                   tags$div(style = "margin-bottom:6px;",
                            actionButton(ns("select_all"), "Select all"),
                            actionButton(ns("select_none"), "Select none"),
                            span("|"),
                            actionButton(ns("update_order"), "Update order", class = "btn-primary"),
                            span(style="margin-left:10px; font-size:90%; color:#666;", "Reorder using the Selectize box and press Update.")
                   ),
                   checkboxGroupInput(ns("cols"), "Visible columns", choices = display_order, selected = display_order, inline = FALSE)
            ),
            column(6,
                   tags$div(style = "font-size:90%; color:#666;",
                            "Select columns to show with the checkboxes. Use the Selectize box to arrange the order, then press Update."
                   ),
                   # Save / Reset buttons for preferred and file order
                   tags$div(style = "margin-top:10px;",
                            actionButton(ns("save_pref"), "Save as preferred", class = "btn-success"),
                            actionButton(ns("reset_pref"), "Reset to preferred"),
                            actionButton(ns("reset_file"), "Reset to file order")
                   )
            )
          ),
          fluidRow(
            column(12,
                   # selectizeInput: choices contain all columns, selected set based on preferred presence
                   # enable drag_drop plugin where available
                   selectizeInput(ns("order_selectize"),
                                  "Column order (drag selected items to reorder, then press Update)",
                                  choices = cols,
                                  selected = display_order,
                                  multiple = TRUE,
                                  options = list(plugins = list('remove_button', 'drag_drop'),
                                                 placeholder = 'Drag to reorder selected items'))
            )
          )
        )
      }
    })

    # When a new file is loaded, set initial checkbox selection and selectize selection in the UI
    observeEvent(shared_data$data, {
      df <- shared_data$data
      if (is.null(df)) {
        updateSelectizeInput(session, "order_selectize", choices = character(0), selected = character(0))
        updateCheckboxGroupInput(session, "cols", choices = character(0), selected = character(0))
        return()
      }
      cols <- colnames(df)
      disp <- shared_data$display_order
      if (is.null(disp) || length(disp) == 0) disp <- cols

      # If user provided a preferred list, select only those in the selectize and tick only them.
      pref <- shared_data$preferred_cols
      pref_valid <- if (!is.null(pref) && length(pref) > 0) intersect(pref, cols) else character(0)

      if (length(pref_valid) > 0) {
        # preferred exists -> select only those in selectize (in user's preferred order),
        # and check only those checkboxes
        updateSelectizeInput(session, "order_selectize", choices = cols, selected = pref_valid, server = FALSE,
                             options = list(plugins = list('remove_button', 'drag_drop')))
        updateCheckboxGroupInput(session, "cols", choices = disp, selected = pref_valid)
      } else {
        # no preferred -> select all (in display order) and tick all
        updateSelectizeInput(session, "order_selectize", choices = cols, selected = disp, server = FALSE,
                             options = list(plugins = list('remove_button', 'drag_drop')))
        updateCheckboxGroupInput(session, "cols", choices = disp, selected = cols)
      }
    }, ignoreNULL = FALSE)

    # When display_order changes (e.g., after Update or Reset), keep checkbox choices in sync.
    observeEvent(shared_data$display_order, {
      df <- shared_data$data
      if (is.null(df)) return()
      cols <- colnames(df)
      disp <- intersect(shared_data$display_order, cols)
      if (length(disp) == 0) disp <- cols

      # Keep previous checkbox selections if possible
      prev_sel <- isolate(input$cols)
      sel <- if (!is.null(prev_sel) && length(prev_sel) > 0) intersect(prev_sel, cols) else disp

      # Update the checkbox choices (ordering in the UI follows display order)
      updateCheckboxGroupInput(session, "cols", choices = disp, selected = sel)
      # Do not forcibly change selectize selection here: selectize represents the user's last-edit order until they Update.
      # But keep choices updated so user can reorder any column.
      updateSelectizeInput(session, "order_selectize", choices = cols, server = FALSE,
                           options = list(plugins = list('remove_button', 'drag_drop')))
    }, ignoreNULL = FALSE)

    # Select all / none for checkboxes
    observeEvent(input$select_all, {
      df <- shared_data$data
      if (is.null(df)) return()
      updateCheckboxGroupInput(session, "cols", selected = colnames(df))
    })
    observeEvent(input$select_none, {
      updateCheckboxGroupInput(session, "cols", selected = character(0))
    })

    # Update order: apply selectize ordering to server-side display_order
    observeEvent(input$update_order, {
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      sel_order <- input$order_selectize
      if (is.null(sel_order) || length(sel_order) == 0) {
        showNotification("No order provided. Make sure you have selected and arranged the columns in the selectize box.", type = "error")
        return()
      }
      # validate against current file columns
      file_cols <- colnames(df)
      valid <- intersect(sel_order, file_cols)
      if (length(valid) == 0) {
        showNotification("None of the selected columns are present in the current file.", type = "error")
        return()
      }
      # selected columns come first in requested order; append remaining columns
      new_order <- c(valid, setdiff(file_cols, valid))
      shared_data$display_order <- new_order
      # ensure selectize still shows the selected subset (so user's selection remains visible)
      updateSelectizeInput(session, "order_selectize", choices = file_cols, selected = valid, server = FALSE,
                           options = list(plugins = list('remove_button', 'drag_drop')))
      showNotification(sprintf("Order applied: %s", paste(valid, collapse = ", ")), type = "message")
    })

    # Save as preferred
    observeEvent(input$save_pref, {
      if (is.null(shared_data$display_order)) {
        showNotification("No display order available to save.", type = "error")
        return()
      }
      shared_data$preferred_cols <- shared_data$display_order
      showNotification("Current display order saved as preferred.", type = "message")
    })

    # Reset to preferred
    observeEvent(input$reset_pref, {
      pref <- shared_data$preferred_cols
      df <- shared_data$data
      if (is.null(df)) {
        showNotification("No data loaded.", type = "error")
        return()
      }
      if (is.null(pref) || length(pref) == 0) {
        showNotification("No preferred order set.", type = "warning")
        return()
      }
      valid <- intersect(pref, colnames(df))
      if (length(valid) == 0) {
        showNotification("Preferred columns not present in current file.", type = "warning")
        return()
      }
      new_order <- c(valid, setdiff(colnames(df), valid))
      shared_data$display_order <- new_order
      # selectize should select only the preferred subset (so user sees them)
      updateSelectizeInput(session, "order_selectize", choices = colnames(df), selected = valid, server = FALSE,
                           options = list(plugins = list('remove_button', 'drag_drop')))
      updateCheckboxGroupInput(session, "cols", choices = new_order, selected = valid)
      showNotification("Reset display order to preferred (where present).", type = "message")
    })

    # Reset to file original order
    observeEvent(input$reset_file, {
      file_order <- shared_data$file_order
      if (is.null(file_order)) {
        showNotification("No file order available (no data loaded).", type = "error")
        return()
      }
      shared_data$display_order <- file_order
      # if there is a preferred set, we don't change checkbox selection here; keep visibility as-is
      # selectize select all (so user can reorder any/all)
      updateSelectizeInput(session, "order_selectize", choices = file_order, selected = file_order, server = FALSE,
                           options = list(plugins = list('remove_button', 'drag_drop')))
      updateCheckboxGroupInput(session, "cols", choices = file_order)
      showNotification("Reset display order to original file order.", type = "message")
    })

    # Render the DT using server-side authoritative visibility + ordering
    output$data_table <- renderDT({
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        return(datatable(data.frame(Message = "No filtered table available. Load a TSV & Columns on Home and set a threshold on Filter."),
                         options = list(dom = "t")))
      }

      disp_order <- shared_data$display_order
      if (is.null(disp_order) || length(disp_order) == 0) disp_order <- colnames(tbl)

      selected_cols <- input$cols
      if (is.null(selected_cols)) selected_cols <- colnames(tbl)

      # compute final order: use disp_order intersect with current table columns, then intersect with selected_cols
      existing <- colnames(tbl)
      disp_order_valid <- intersect(disp_order, existing)
      remainder <- setdiff(existing, disp_order_valid)
      final_order <- c(disp_order_valid, remainder)
      final_visible <- intersect(final_order, selected_cols)

      if (length(final_visible) == 0) {
        return(datatable(data.frame(Message = "No columns selected. Use the column controls above to show columns."),
                         options = list(dom = "t")))
      }

      final_tbl <- tbl[, final_visible, drop = FALSE]

      datatable(final_tbl,
                options = list(dom = 'frtip', pageLength = 15, scrollX = TRUE),
                rownames = FALSE)
    }, server = TRUE)
  })
}

# ---------------------------
# App UI and Server
# ---------------------------
ui <- fluidPage(
  titlePanel("3-Tab Shiny App (selectize ordering + server-side visibility)"),
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
    preferred_cols = NULL,  # user-specified column order (character vector, persisted only on Save)
    display_order = NULL,   # current UI display order (server authoritative)
    file_order = NULL       # original file column order
  )

  homeModuleServer("home", shared_data)
  filterModuleServer("filter", shared_data)
  dataModuleServer("data", shared_data)
}

# Return shiny.appobj so runApp("app.R") works.
shinyApp(ui = ui, server = server)