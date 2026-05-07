#' Log Viewer Module
#' @param id Module ID
#' @return Shiny UI object (log_viewer_ui) or NULL (log_viewer_server)
#' @export
# Log viewer module (strict, uses lgr's own numeric<->name mapping)
# - Lists all files in logs_dir
# - Numeric input for "lines to show"
# - Level dropdown (default: info)
# - Strict JSON parsing with jsonlite; errors if a line can't be parsed
# - Displays "LEVEL  TIMESTAMP  message"
# - Filters by min level using lgr::get_log_levels()
# - Regex search on simplified text, auto-refresh, and download

log_viewer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    shiny::fluidRow(
      shiny::column(12,
        shiny::strong("Current Shiny app session: "),
        shiny::span(shiny::textOutput(ns("session_id"), inline = TRUE))
      )
    ),
    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 9,
        shiny::selectizeInput(ns("file"), "Select log file:", choices = character(0), width = "100%"),
        shiny::textInput(ns("search"), "Search (regex, optional):", value = "", placeholder = "e.g. error|warn|go"),
        shiny::verbatimTextOutput(ns("log_tail"), placeholder = TRUE)
      ),
      shiny::column(
        width = 3,
        shiny::numericInput(ns("tail_lines"), "Lines to show", value = 1000, min = 50, max = 100000, step = 50),
        shiny::checkboxInput(ns("autorefresh"), "Auto-refresh", value = TRUE),
        shiny::numericInput(ns("refresh_ms"), "Refresh every (ms)", value = 2000, min = 250, step = 250),
        shiny::selectInput(ns("level"), "Minimum severity:", choices = c("debug","info","warn","error"),
                           selected = "error", selectize = FALSE),
        shiny::downloadButton(ns("download_log"), "Download selected log")
      )
    )
  )
}

#' Log Viewer Module Server
#' @param id Module ID
#' @param logs_dir Directory containing log files (default: "logs")
#' @param session_logfile_reactive Optional reactive that returns the current session's log file path (for default selection)
#' @return NULL
#' @export

log_viewer_server <- function(id, logs_dir = "logs", session_logfile_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("The log viewer requires the 'jsonlite' package. Please install it: install.packages('jsonlite')")
    }
    if (!requireNamespace("lgr", quietly = TRUE)) {
      stop("The log viewer requires the 'lgr' package. Please install it: install.packages('lgr')")
    }

    `%||%` <- function(x, y) if (is.null(x)) y else x

    # Get lgr's level mapping once (names -> numeric), and helpers
    levels_vec <- lgr::get_log_levels()   # e.g., c(trace=100, debug=200, info=400, warn=500, error=600, fatal=700) [example]
    # Convert numeric -> name using lgr's mapping
    num_to_name <- function(n) {
      nm <- names(levels_vec)[match(n, unname(levels_vec))]
      ifelse(is.na(nm), NA_character_, nm)
    }

    # List logs in directory (current + historical)
    list_logs <- function(dir) {
      if (!dir.exists(dir)) return(character(0))
      files <- list.files(dir, pattern = "\\.(jsonl|log|txt)(\\.gz)?$", full.names = TRUE)
      if (length(files)) {
        ord <- order(file.mtime(files), decreasing = TRUE)
        files <- files[ord]
      }
      files
    }

    files_reactive <- shiny::reactiveVal(character(0))

    refresh_files <- function() {
      files_reactive(list_logs(logs_dir))
    }

    # Initial + periodic refresh for file list
    shiny::observe({
      refresh_files()
      if (isTRUE(input$autorefresh)) {
        shiny::invalidateLater(as.integer(input$refresh_ms %||% 2000), session)
      }
    })

    shiny::observeEvent(files_reactive(), {
      files <- files_reactive()
      lbls  <- if (length(files)) basename(files) else character(0)

      # Build choices
      choices <- if (length(files)) stats::setNames(files, lbls) else character(0)

      # Determine current selection (if any)
      current <- shiny::isolate(input$file)
      current_is_valid <- is.character(current) && nzchar(current) && current %in% files

      # Compute latest by modification time (robust to NAs)
      latest <- character(0)
      if (length(files)) {
        mt <- suppressWarnings(file.mtime(files))
        # Handle any NA mtimes by treating them as the oldest
        mt[is.na(mt)] <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
        latest <- files[[ which.max(mt) ]]
      }

      # Selection rule:
      # - If the current selection is valid, keep it.
      # - Otherwise, select the latest file (if any).
      selected <- if (current_is_valid) current else latest

      shiny::updateSelectizeInput(
        session, "file",
        choices = choices,
        selected = selected %||% character(0),
        server = TRUE
      )
    }, ignoreInit = FALSE)

    read_tail <- function(path, n) {
      if (!length(path) || !file.exists(path)) return(character(0))
      lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
      # drop blank lines (allowed)
      lines[nzchar(trimws(lines))]
    }

    # Strict parse JSONL line via jsonlite, using lgr's level mapping
    parse_line_strict <- function(line) {
      obj <- tryCatch(jsonlite::fromJSON(line), error = function(e) NULL)
      if (is.null(obj) || !is.list(obj)) {
        stop("Failed to parse a log line as JSON.")
      }

      # Resolve level: prefer string name; else numeric -> name via levels_vec
      lvl_name <- NULL
      lvl_num  <- NULL

      if (!is.null(obj$level_name)) {
        lvl_name <- tolower(as.character(obj$level_name))
        if (!lvl_name %in% names(levels_vec)) {
          stop(sprintf("Unknown level_name '%s' in log line.", lvl_name))
        }
        lvl_num <- levels_vec[[lvl_name]]
      } else if (!is.null(obj$level)) {
        if (is.numeric(obj$level)) {
          lvl_num <- as.integer(obj$level)
          lvl_name <- num_to_name(lvl_num)
          if (is.na(lvl_name)) stop(sprintf("Unknown numeric level '%s' in log line.", as.character(obj$level)))
        } else {
          lvl_name <- tolower(as.character(obj$level))
          if (!lvl_name %in% names(levels_vec)) {
            stop(sprintf("Unknown level '%s' in log line.", lvl_name))
          }
          lvl_num <- levels_vec[[lvl_name]]
        }
      } else {
        stop("Missing 'level'/'level_name' in log line.")
      }

      # Message
      msg <- obj$msg
      if (is.null(msg) || length(msg) != 1) stop("Missing or invalid 'msg' in log line.")
      msg <- as.character(msg)

      # Timestamp (optional)
      ts <- obj$timestamp %||% obj$time %||% obj$t %||% ""

      list(level_name = lvl_name, level_num = lvl_num, timestamp = as.character(ts), msg = msg)
    }

    output$session_id <- shiny::renderText({
      # Print the session token obtained from the server
      paste(session$token)
    })

    output$log_tail <- shiny::renderText({
      # Ensure periodic refresh of the rendered content itself
      if (isTRUE(input$autorefresh)) {
        shiny::invalidateLater(as.integer(input$refresh_ms %||% 2000), session)
      }

      sel <- input$file
      if (is.null(sel) || !nzchar(sel)) return("No log file selected.")
      n  <- as.integer(input$tail_lines %||% 1000)
      raw <- read_tail(sel, n)
      if (!length(raw)) return("<no matching log lines>")

      # Strict parse; bubble parse errors to UI
      parsed <- try(lapply(raw, parse_line_strict), silent = TRUE)
      if (inherits(parsed, "try-error")) {
        shiny::validate(shiny::need(FALSE, paste0("Log parse error: ", as.character(parsed))))
      }

      lvl_names <- vapply(parsed, function(x) x$level_name, character(1))
      lvl_nums  <- vapply(parsed, function(x) x$level_num, integer(1))
      ts        <- vapply(parsed, function(x) x$timestamp, character(1))
      msgs      <- vapply(parsed, function(x) x$msg, character(1))

      # Filter by min level using numeric threshold from lgr
      min_lvl_name <- input$level %||% "info"
      min_lvl_num  <- levels_vec[[min_lvl_name]]
      keep <- !is.na(lvl_nums) & lvl_nums >= min_lvl_num

      disp <- paste0(toupper(lvl_names), "  ", ts, "  ", msgs)

      # Optional regex search on simplified text
      srch <- input$search %||% ""
      if (nzchar(srch)) keep <- keep & grepl(srch, disp, perl = TRUE)

      disp <- disp[keep]
      if (!length(disp)) return("<no matching log lines>")
      paste(disp, collapse = "\n")
    })

    output$download_log <- shiny::downloadHandler(
      filename = function() {
        sel <- input$file
        if (is.null(sel) || !nzchar(sel)) return("shiny_log.jsonl")
        paste0("shiny_", basename(sel))
      },
      content = function(file) {
        sel <- input$file
        if (is.null(sel) || !nzchar(sel) || !file.exists(sel)) {
          writeLines("No log file available.", con = file)
        } else {
          file.copy(sel, file, overwrite = TRUE)
        }
      }
    )
  })
}