# Single-file Shiny app (modular) with three server modules: Home, Filter, Data
# Changes requested:
# - Home: single button ("Load TSV & Columns") that reads TSV from a path AND parses/stores the user's
#         preferred column order (semicolon-separated) — both actions handled by the same button.
# - Removed data_filtered (it was unused).
# - Filter: same slider behavior (no DTOutput).
# - Data: the only DTOutput; shows shared_data$table_filtered in the user's preferred column order.
#
# Returns a shiny.appobj so shiny::runApp("app.R") works.

library(shiny)
library(DT)

# ---------------------------
# Home module (UI + Server)
# ---------------------------
homeModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(7, textInput(ns("path"), "Path to TSV file (remote filesystem)", placeholder = "/path/to/file.tsv")),
      column(5, textInput(ns("col_order"), "Preferred column order (semicolon-separated)", placeholder = "ID;VALUE;Name"))
    ),
    fluidRow(
      column(12, actionButton(ns("load"), "Load TSV & Columns", class = "btn-primary"))
    ),
    br(),
    tags$div(style = "margin-top: 10px;",
             verbatimTextOutput(ns("status"))
    )
  )
}

homeModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    # parse semicolon-separated list into vector
    parse_cols <- function(txt) {
      if (is.null(txt)) return(character(0))
      parts <- unlist(strsplit(txt, ";", fixed = TRUE))
      parts <- trimws(parts)
      parts[parts != ""]
    }

    observeEvent(input$load, {
      path <- trimws(isolate(input$path))
      # parse preferred columns from the text field
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
        return()
      }

      # Try reading TSV
      tryCatch({
        df <- read.delim(path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
        shared_data$data <- df
        # initialize table_filtered to full dataset (filter module will update it on slider change)
        shared_data$table_filtered <- df
        # store preferred columns (as typed). We'll warn if some are missing.
        shared_data$preferred_cols <- pref_cols

        # warn if preferred columns include names not present in the loaded data
        if (length(pref_cols) > 0) {
          missing <- setdiff(pref_cols, colnames(df))
          if (length(missing) > 0) {
            showNotification(
              sprintf("Some preferred columns were not found and will be ignored in display: %s", paste(missing, collapse = ", ")),
              type = "warning", duration = 6
            )
          } else {
            showNotification("File and preferred columns loaded successfully.", type = "message")
          }
        } else {
          showNotification("File loaded. No preferred column order provided.", type = "message")
        }
      }, error = function(e) {
        shared_data$data <- NULL
        shared_data$table_filtered <- NULL
        shared_data$preferred_cols <- NULL
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
             "Adjust the slider to update the filtered table used by the Data tab."
    )
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
        # If there's no VALUE column, keep original data and warn the user
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
    DTOutput(ns("data_table"))
  )
}

dataModuleServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    output$data_table <- renderDT({
      tbl <- shared_data$table_filtered
      if (is.null(tbl)) {
        datatable(data.frame(Message = "No filtered table available. Load a TSV & Columns on Home and set a threshold on Filter."),
                  options = list(dom = "t"))
      } else {
        pref <- shared_data$preferred_cols
        final_tbl <- tbl
        if (!is.null(pref) && length(pref) > 0) {
          # Keep only preferred columns that exist, in the specified order; append the rest afterwards.
          existing <- colnames(tbl)
          pref_valid <- intersect(pref, existing)
          remainder <- setdiff(existing, pref_valid)
          new_order <- c(pref_valid, remainder)
          final_tbl <- final_tbl[, new_order, drop = FALSE]
        }
        datatable(final_tbl, options = list(pageLength = 15, scrollX = TRUE))
      }
    }, server = TRUE)
  })
}

# ---------------------------
# App UI and Server
# ---------------------------
ui <- fluidPage(
  titlePanel("3-Tab Shiny App (path input, preferred column order)"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home", homeModuleUI("home")),
    tabPanel("Filter", filterModuleUI("filter")),
    tabPanel("Data", dataModuleUI("data"))
  )
)

server <- function(input, output, session) {
  shared_data <- reactiveValues(
    data = NULL,            # original data from uploaded TSV (or path-read TSV)
    table_filtered = NULL,  # final table after applying slider filter
    preferred_cols = NULL   # user-specified column order (character vector)
  )

  homeModuleServer("home", shared_data)
  filterModuleServer("filter", shared_data)
  dataModuleServer("data", shared_data)
}

# Return shiny.appobj so runApp("app.R") works.
shinyApp(ui = ui, server = server)