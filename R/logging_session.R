# Per-session structured logging using lgr (no rotation).
# - Each Shiny session writes to its own JSONL log file (absolute path)
# - On app startup, purge log files older than 100 days
# - Console output: only the message (no level/time/fields)
# - JSONL file keeps full structured fields (token/ip/path/user etc.) by default
# - Minimal-friction helpers: log_info("msg"), log_warn("msg"), log_error("msg"), log_debug("msg")
#   - No need to pass session; auto-detected via shiny::getDefaultReactiveDomain()
#   - Named ... become structured fields: log_info("Saved", file = path, ok = TRUE)
#   - Unnamed ... are sprintf() args: log_info("Loaded %s rows", n)

# ---- Helper internals for minimal-friction API ----
`%||null%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x

# ---- Utilities ----
.ensure_logs_dir_abs <- function(logs_dir) {
  if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(logs_dir, winslash = "/", mustWork = TRUE)
}

log_default_dir <- function() "logs"

# Optional privacy override (not required by default)
# By default, all fields are included in JSONL. Console/viewer don't show fields.
set_log_privacy <- function(include_token = TRUE,
                            include_ip = TRUE,
                            include_path = TRUE,
                            include_user = TRUE) {
  options(
    app.log.include_token = isTRUE(include_token),
    app.log.include_ip    = isTRUE(include_ip),
    app.log.include_path  = isTRUE(include_path),
    app.log.include_user  = isTRUE(include_user)
  )
  invisible(NULL)
}

#' Setup application logging
#'
#' Initializes per-app logging with lgr, purges old logs, optionally logs to console.
#' @param level Logging threshold: "info", "debug", etc.
#' @param logs_dir Directory to store logs.
#' @param older_than_days Purge logs older than this many days.
#' @param console Whether to log to console.
#' @export
setup_app_logging <- function(level = "info",
                              logs_dir = log_default_dir(),
                              older_than_days = 100,
                              console = interactive()) {
  if (!requireNamespace("lgr", quietly = TRUE)) {
    stop("Please install 'lgr' for structured logging: install.packages('lgr')")
  }

  lg <- lgr::get_logger("shinyapp")
  # Remove any existing appenders to avoid duplicates during dev reload
  lapply(names(lg$appenders), function(nm) try(lg$remove_appender(nm), silent = TRUE))
  lg$set_threshold(level)
  lg$set_propagate(FALSE)  # prevent bubbling to root (no duplicate console)

  if (isTRUE(console)) {
    # Console prints ONLY the message (no level, no timestamp, no fields)
    lg$add_appender(
      lgr::AppenderConsole$new(
        layout = lgr::LayoutFormat$new("%m")
      ),
      name = "console"
    )
  }

  logs_dir_abs <- .ensure_logs_dir_abs(logs_dir)
  .purge_old_logs(logs_dir = logs_dir_abs, older_than_days = older_than_days)

  lg$info("Application started",
          pid = Sys.getpid(),
          r_version = as.character(getRversion()),
          shiny_version = tryCatch(as.character(utils::packageVersion("shiny")), error = function(e) NA_character_),
          lgr_version = tryCatch(as.character(utils::packageVersion("lgr")), error = function(e) NA_character_),
          logs_dir = logs_dir_abs,
          retention_days = older_than_days)

  if (requireNamespace("shiny", quietly = TRUE)) {
    shiny::onStop(function() {
      lgr::get_logger("shinyapp")$info("Application stopping")
    })
  }

  # Capture unhandled Shiny errors to console logger (best-effort)
  options(shiny.error = function(e) {
    msg <- paste0("Unhandled error: ", conditionMessage(e))
    lgr::get_logger("shinyapp")$error(msg,
      stack = paste(utils::capture.output(sys.calls()), collapse = " | "))
  })

  invisible(TRUE)
}

# Delete log files older than N days in logs_dir
.purge_old_logs <- function(logs_dir, older_than_days = 100) {
  if (!dir.exists(logs_dir)) return(invisible(0L))
  files <- list.files(logs_dir, full.names = TRUE, recursive = FALSE)
  if (!length(files)) return(invisible(0L))
  cutoff <- Sys.time() - as.difftime(older_than_days, units = "days")
  mt <- file.mtime(files)
  old <- which(!is.na(mt) & mt < cutoff)
  n <- 0L
  if (length(old)) {
    for (i in old) {
      f <- files[[i]]
      if (grepl("\\.(jsonl|log|txt)(\\.gz)?$", f, ignore.case = TRUE)) {
        try(unlink(f, force = TRUE), silent = TRUE)
        n <- n + 1L
      }
    }
  }
  lgr::get_logger("shinyapp")$info("Purged old log files", removed = n, older_than_days = older_than_days)
  invisible(n)
}

# Build a per-session ABSOLUTE log file path
.session_log_filepath <- function(session, logs_dir = log_default_dir(), prefix = "session") {
  base <- .ensure_logs_dir_abs(logs_dir)
  ts  <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tok <- tryCatch(session$token, error = function(e) NULL)
  tok <- if (is.null(tok)) sprintf("pid%s", Sys.getpid()) else gsub("[^A-Za-z0-9._-]", "_", tok)
  file.path(base, sprintf("%s_%s_%s.jsonl", prefix, ts, tok))
}


#' Start per-session logging
#'
#' Initializes a per-session logger that writes to a dedicated JSONL log file.
#' Also logs session start and end events.
#' @param session Shiny session (automatically detected if NULL).
#' @param logs_dir Directory to store logs.
#' @param level Optional logging threshold for this session (inherits from app logger if NULL).
#' @param prefix Prefix for the log filename (default "session").
#' @param console Whether to propagate logs to the app-level console logger.
#' @return The absolute path to the log file (invisibly).
#' @export

start_session_logger <- function(session,
                                 logs_dir = log_default_dir(),
                                 level = NULL,
                                 prefix = "session",
                                 console = TRUE) {
  stopifnot(!is.null(session))
  if (!requireNamespace("lgr", quietly = TRUE)) {
    stop("Please install 'lgr' for structured logging: install.packages('lgr')")
  }

  path <- .session_log_filepath(session, logs_dir = logs_dir, prefix = prefix)

  # Create a dedicated logger for this session under the app namespace
  name <- paste0("shinyapp/", tryCatch(session$token, error = function(e) "session"))
  lg <- lgr::get_logger(name)
  # Clear any existing appenders on hot reloads
  lapply(names(lg$appenders), function(nm) try(lg$remove_appender(nm), silent = TRUE))

  # Inherit threshold and console from parent unless overridden
  if (!is.null(level)) lg$set_threshold(level)
  lg$set_propagate(console)  # bubble up to shinyapp for single console print
  # lg$set_propagate(FALSE) # no console print from session logger

  # Only per-session file appender here (JSONL with full detail)
  file_app <- lgr::AppenderFile$new(
    file = path,
    layout = lgr::LayoutJson$new()
  )
  lg$add_appender(file_app, name = "file")

  # Stash logger + path on session for convenience
  session$userData$logger  <- lg
  session$userData$logfile <- path

  # Safe accessors for clientData (reactive) at startup
  safe_client <- function(key) {
    tryCatch(shiny::isolate(session$clientData[[key]]), error = function(e) NULL)
  }

  # Session lifecycle events (info level) print the session token as well
  log_info("Session started: %s", session$token, .session = session, path = safe_client("url_pathname") %||null% NA_character_, query = safe_client("url_search") %||null% NA_character_)

  session$onSessionEnded(function() {
    # Try logging; if process is closing or path gone, don't error
    try(log_info("Session ended: %s", session$token, .session = session), silent = TRUE)
    # Remove the file appender to flush and release the handle
    try(lg$remove_appender("file"), silent = TRUE)
  })

  invisible(path)
}

# Resolve session from explicit argument or reactive domain
.resolve_session <- function(session = NULL) {
  if (!is.null(session)) return(session)
  if (requireNamespace("shiny", quietly = TRUE)) {
    return(shiny::getDefaultReactiveDomain())
  }
  NULL
}

# Include all fields by default in JSONL (console ignores fields)
.session_fields <- function(session = NULL, user_id = NULL) {
  include_token <- isTRUE(getOption("app.log.include_token", TRUE))
  include_ip    <- isTRUE(getOption("app.log.include_ip", TRUE))
  include_path  <- isTRUE(getOption("app.log.include_path", TRUE))
  include_user  <- isTRUE(getOption("app.log.include_user", TRUE))

  fields <- list()

  if (include_token) {
    fields$token <- if (is.null(session)) NA_character_
                    else tryCatch(session$token, error = function(e) NA_character_)
  }
  if (include_ip) {
    ip <- NULL
    if (!is.null(session) && !is.null(session$request)) {
      ip <- tryCatch(session$request$HTTP_X_FORWARDED_FOR, error = function(e) NULL)
      if (is.null(ip)) ip <- tryCatch(session$request$REMOTE_ADDR, error = function(e) NULL)
    }
    fields$ip <- ip %||null% NA_character_
  }
  if (include_path) {
    fields$path <- if (is.null(session)) NA_character_
                   else tryCatch(shiny::isolate(session$clientData$url_pathname), error = function(e) NA_character_) %||null% NA_character_
  }
  if (include_user) {
    fields$user <- user_id %||null% NA_character_
  }

  fields
}

# Ensure fields are a named, scalar list (best-effort, robust with empty)
.normalize_fields <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())
  if (!is.list(x)) x <- list(value = x)

  nm <- names(x)
  if (is.null(nm)) nm <- rep("", length(x))
  if (length(nm) != length(x)) nm <- rep_len(nm, length(x))
  empty <- which(is.na(nm) | nm == "")
  if (length(empty)) nm[empty] <- paste0("field", empty)
  names(x) <- nm

  for (i in seq_along(x)) {
    val <- x[[i]]
    if (length(val) != 1L || is.list(val)) {
      x[[i]] <- paste(utils::capture.output(str(val, give.attr = FALSE)), collapse = " ")
    }
  }
  x
}

# Split ... into unnamed (format args) and named (fields)
.split_dots <- function(...) {
  dots <- list(...)
  if (!length(dots)) return(list(unnamed = list(), named = list()))
  nm <- names(dots); if (is.null(nm)) nm <- rep("", length(dots))
  unnamed_idx <- which(nm == "" | is.na(nm))
  named_idx   <- setdiff(seq_along(dots), unnamed_idx)
  list(
    unnamed = if (length(unnamed_idx)) dots[unnamed_idx] else list(),
    named   = if (length(named_idx))   dots[named_idx]   else list()
  )
}

# Build final message + fields
# - If there are unnamed dots, treat msg as sprintf() format string
# - Merge explicit .fields (or legacy 'fields') with named dots (named dots take precedence)
.coerce_msg_fields <- function(msg, ..., .fields = NULL, fields = NULL) {
  parts <- .split_dots(...)
  final_msg <- tryCatch(
    if (length(parts$unnamed)) do.call(sprintf, c(list(fmt = as.character(msg)[1L]), parts$unnamed)) else as.character(msg)[1L],
    error = function(e) as.character(msg)[1L]
  )
  base_fields <- if (!is.null(.fields)) .fields else fields
  if (is.null(base_fields)) base_fields <- list()
  if (!is.list(base_fields)) base_fields <- list(value = base_fields)
  out_fields <- utils::modifyList(base_fields, parts$named, keep.null = TRUE)
  list(msg = final_msg, fields = out_fields)
}

# Pick the right logger (session-specific if present; else app logger)
.get_target_logger <- function(session = NULL) {
  lg <- NULL
  if (!is.null(session)) {
    lg <- tryCatch(session$userData$logger, error = function(e) NULL)
  }
  if (is.null(lg)) lg <- lgr::get_logger("shinyapp")
  lg
}

# Base invoker that expands fields with do.call (no tidy-eval)
.log_emit <- function(level_fn, msg, ..., .session = NULL, .fields = NULL, .user = NULL,
                      # legacy arg names for backward-compat
                      session = NULL, fields = NULL, user_id = NULL) {
  # rs <- .resolve_session(.session %||null% session)
  rs <- .resolve_session(if (!is.null(.session)) .session else session)
  mf <- .coerce_msg_fields(msg, ..., .fields = .fields, fields = fields)
  args <- c(list(msg = mf$msg), .session_fields(session = rs, user_id = (.user %||null% user_id)), .normalize_fields(mf$fields))
  do.call(level_fn, args)
  invisible(NULL)
}

# ---- Public minimal-friction helpers ----
#' Log an info-level message with optional structured fields
#' @param msg Message (string or format string if ... are provided).
#' @param ... Optional arguments:
#'   - Named arguments become structured fields (e.g. user = user_id).
#'   - Unnamed arguments are treated as sprintf() args to format msg.
#' @param .session Optional Shiny session (auto-detected if NULL).
#' @param .fields Optional named list of additional structured fields.
#' @param .user Optional user ID to include as 'user' field.
#' @param session Legacy alias for .session.
#' @param fields Legacy alias for .fields.
#' @param user_id Legacy alias for .user.
#' @return NULL (invisibly).
#' @export
log_info <- function(msg, ..., .session = NULL, .fields = NULL, .user = NULL,
                     session = NULL, fields = NULL, user_id = NULL) {
  .log_emit(.get_target_logger(.resolve_session(.session %||null% session))$info,
            msg, ..., .session = .session, .fields = .fields, .user = .user,
            session = session, fields = fields, user_id = user_id)
}

log_warn <- function(msg, ..., .session = NULL, .fields = NULL, .user = NULL,
                     session = NULL, fields = NULL, user_id = NULL) {
  .log_emit(.get_target_logger(.resolve_session(.session %||null% session))$warn,
            msg, ..., .session = .session, .fields = .fields, .user = .user,
            session = session, fields = fields, user_id = user_id)
}

log_error <- function(msg, ..., .session = NULL, .fields = NULL, .user = NULL,
                      session = NULL, fields = NULL, user_id = NULL) {
  .log_emit(.get_target_logger(.resolve_session(.session %||null% session))$error,
            msg, ..., .session = .session, .fields = .fields, .user = .user,
            session = session, fields = fields, user_id = user_id)
}

#' Log a debug-level message with optional structured fields
#' @param msg Message (string or format string if ... are provided).
#' @param ... Optional arguments:
#'   - Named arguments become structured fields (e.g. user = user_id).
#'   - Unnamed arguments are treated as sprintf() args to format msg.
#' @param .session Optional Shiny session (auto-detected if NULL).
#' @param .fields Optional named list of additional structured fields.
#' @param .user Optional user ID to include as 'user' field.
#' @param session Legacy alias for .session.
#' @param fields Legacy alias for .fields.
#' @param user_id Legacy alias for .user.
#' @return NULL (invisibly).
#' @export
log_debug <- function(msg, ..., .session = NULL, .fields = NULL, .user = NULL,
                      session = NULL, fields = NULL, user_id = NULL) {
  .log_emit(.get_target_logger(.resolve_session(.session %||null% session))$debug,
            msg, ..., .session = .session, .fields = .fields, .user = .user,
            session = session, fields = fields, user_id = user_id)
}

# Time an expression and log the duration (ms)
log_timed <- function(label, expr, ..., .session = NULL, .fields = NULL, .user = NULL,
                      session = NULL, fields = NULL, user_id = NULL, level = c("info","debug")) {
  level <- match.arg(level)
  t0 <- proc.time()
  on.exit({
    dt_ms <- round(1000 * (proc.time() - t0)[["elapsed"]], 1)
    if (level == "info") {
      log_info(sprintf("timing: %s", label), elapsed_ms = dt_ms, .session = .session, .fields = .fields,
               .user = .user, session = session, fields = fields, user_id = user_id, ...)
    } else {
      log_debug(sprintf("timing: %s", label), elapsed_ms = dt_ms, .session = .session, .fields = .fields,
                .user = .user, session = session, fields = fields, user_id = user_id, ...)
    }
  })
  force(expr)
  invisible(NULL)
}


#' Use minimal headless logging (lgr only; no token/ip/path/user fields)
#'
#' Call once in a non-Shiny/headless context (e.g. inside run_puzzle_pipeline)
#' to suppress per-session fields and simplify console output.
#'
#' Effects:
#'   - Disables token/ip/path/user via set_log_privacy(FALSE, ...)
#'   - Ensures the 'shinyapp' logger has a console appender with a minimal layout.
#'   - Does NOT alter file appenders (session JSONL logs keep full structured fields
#'     unless you also call set_log_privacy(FALSE, ...) before starting session loggers).
#'
#' If you want JSON files ALSO to omit those fields, call use_headless_logging_lgr()
#' BEFORE start_session_logger().
#'
#' @param layout Format string for console (default: \code{\%l [\%t] \%m}).
#'   Tokens: \code{\%l} = level, \code{\%t} = timestamp, \code{\%m} = message.
#' @param threshold Optional logging threshold override (e.g., "info", "debug").
#'                  NULL leaves existing threshold unchanged.
#' @return TRUE (invisibly)
#' @export
use_headless_logging_lgr <- function(layout = "%l [%t] %m",
                                     threshold = NULL) {
  if (!requireNamespace("lgr", quietly = TRUE)) {
    stop("Package 'lgr' not installed; cannot configure logging.")
  }

  # Disable inclusion of session-derived fields globally.
  set_log_privacy(include_token = FALSE,
                  include_ip    = FALSE,
                  include_path  = FALSE,
                  include_user  = FALSE)

  lg <- lgr::get_logger("shinyapp")

  if (!is.null(threshold)) {
    lg$set_threshold(threshold)
  }

  # Remove any existing console appender named "console-headless" to avoid duplicates.
  if ("console-headless" %in% names(lg$appenders)) {
    try(lg$remove_appender("console-headless"), silent = TRUE)
  }

  # Add (or replace) a minimal console appender.
  lg$add_appender(
    lgr::AppenderConsole$new(
      layout = lgr::LayoutFormat$new(layout)
    ),
    name = "console-headless"
  )

  # Prevent propagation to root to avoid duplicate console prints.
  lg$set_propagate(FALSE)

  invisible(TRUE)
}