# Single-file Shiny app (modular)
# - Home: Load TSV from path and parse semicolon-separated preferred column order in one button
# - Filter: slider filters rows by VALUE (no preview)
# - Data: server-authoritative column visibility + server-side reordering (up/down buttons),
#         Save-as-preferred / Reset-to-preferred / Reset-to-file-order actions.
#
# Notes:
# - Reordering is server-side using a selectInput + Move Up / Move Down buttons for simplicity and reliability.
# - Filtering always uses column names (so reordering never breaks filtering).
# - The "preferred" order is only overwritten when user clicks "Save preferred".
# - The app avoids extra JS packages so it should run on typical cluster R installs.
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
        return()
      }

      tryCatch({
        df <- read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
        shared_data$data <- df
        shared_data$table_filtered <- df

        # store preferred columns (as typed) and compute initial display order
        shared_data$preferred_cols <- pref_cols
        file_cols <- colnames(df)
        pref_valid <- intersect(pref_cols, file_cols)
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

    # Column controls: visibility checkboxes, order select + move up/down, save/reset buttons
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
                   checkboxGroupInput(ns("cols"), "Visible columns", choices = display_order, selected = display_order, inline = FALSE)
            ),
            column(6,
                   tags$div(style = "margin-bottom:6px;",
                            actionButton(ns("move_up"), "Move up"),
                            actionButton(ns("move_down"), "Move down"),
                            actionButton(ns("save_pref"), "Save as preferred", class = "btn-success"),
                            actionButton(ns("reset_pref"), "Reset to preferred"),
                            actionButton(ns("reset_file"), "Reset to file order")
                   ),
                   tags$div(style = "font-size:90%; color:#666;",
                            "Use the list below to select a single column, then Move up / Move down to reorder."
                   )
            )
          ),
          fluidRow(
            column(12,
                   # IMPORTANT: set selectize = FALSE when using 'size' so we don't get the error:
                   # "Warning: Error in selectInput: 'size' argument is incompatible with 'selectize=TRUE'."
                   selectInput(ns("order_list"), "Column order (select one to move)", choices = display_order,
                               selected = display_order[[1]], multiple = FALSE,
                               selectize = FALSE, size = min(max(6, length(display_order)), 16))
            )
          )
        )
      }
    })

    # When data is loaded or display_order changes, update order_list and cols choices/selected appropriately
    observeEvent(shared_data$display_order, {
      df <- shared_data$data
      if (is.null(df)) return()
      cols <- colnames(df)
      disp <- shared_data$display_order
      # Ensure display order only contains existing columns
      disp <- intersect(disp, cols)
      if (length(disp) == 0) disp <- cols
      # Update order list and checkbox choices
      prev_sel <- isolate(input$cols)
      sel <- if (!is.null(prev_sel) && length(prev_sel) > 0) intersect(prev_sel, cols) else disp
      updateSelectInput(session, "order_list", choices = disp, selected = if (length(sel) > 0) sel[[1]] else disp[[1]])
      updateCheckboxGroupInput(session, "cols", choices = disp, selected = sel)
    }, ignoreNULL = FALSE)

    # Move up / down logic for selectInput based ordering
    observeEvent(input$move_up, {
      req(shared_data$display_order)
      sel <- isolate(input$order_list)
      if (is.null(sel) || sel == "") return()
      order_vec <- shared_data$display_order
      # ensure current display order includes the selected item
      order_vec <- intersect(order_vec, colnames(shared_data$data))
      idx <- match(sel, order_vec)
      if (is.na(idx) || idx <= 1) return()
      new_order <- order_vec
      new_order[c(idx-1, idx)] <- new_order[c(idx, idx-1)]
      shared_data$display_order <- new_order
      updateSelectInput(session, "order_list", choices = new_order, selected = sel)
    })

    observeEvent(input$move_down, {
      req(shared_data$display_order)
      sel <- isolate(input$order_list)
      if (is.null(sel) || sel == "") return()
      order_vec <- shared_data$display_order
      order_vec <- intersect(order_vec, colnames(shared_data$data))
      idx <- match(sel, order_vec)
      if (is.na(idx) || idx >= length(order_vec)) return()
      new_order <- order_vec
      new_order[c(idx, idx+1)] <- new_order[c(idx+1, idx)]
      shared_data$display_order <- new_order
      updateSelectInput(session, "order_list", choices = new_order, selected = sel)
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
      updateSelectInput(session, "order_list", choices = new_order, selected = valid[[1]])
      updateCheckboxGroupInput(session, "cols", choices = new_order, selected = isolate(input$cols))
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
      updateSelectInput(session, "order_list", choices = file_order, selected = file_order[[1]])
      updateCheckboxGroupInput(session, "cols", choices = file_order, selected = isolate(input$cols))
      showNotification("Reset display order to original file order.", type = "message")
    })

    # When a new file is loaded, set initial checkbox selection and order_list in the UI
    observeEvent(shared_data$data, {
      df <- shared_data$data
      if (is.null(df)) {
        updateSelectInput(session, "order_list", choices = character(0), selected = character(0))
        updateCheckboxGroupInput(session, "cols", choices = character(0), selected = character(0))
        return()
      }
      cols <- colnames(df)
      disp <- shared_data$display_order
      if (is.null(disp) || length(disp) == 0) disp <- cols
      # Initialize checkbox selection: use preferred valid columns if present, else all
      pref <- shared_data$preferred_cols
      if (!is.null(pref) && length(pref) > 0) {
        pref_valid <- intersect(pref, cols)
        initial_sel <- if (length(pref_valid) > 0) pref_valid else cols
      } else {
        initial_sel <- cols
      }
      # Ensure order_list uses display order
      updateSelectInput(session, "order_list", choices = disp, selected = disp[[1]])
      updateCheckboxGroupInput(session, "cols", choices = disp, selected = initial_sel)
    }, ignoreNULL = FALSE)

    # Render the DT using server-side authoritative visibility + ordering
    output$data_table <- renderDT({
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        return(datatable(data.frame(Message = "No filtered table available. Load a TSV & Columns on Home and set a threshold on Filter."),
                         options = list(dom = "t")))
      }

      pref <- shared_data$preferred_cols
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

      # Use server = TRUE in renderDT above; datatable() itself should not receive a server argument.
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
  titlePanel("3-Tab Shiny App (server-side column ordering & visibility)"),
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