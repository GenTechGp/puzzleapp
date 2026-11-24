# URL-token access control for the web app.
#
# run_app() resolves a token and stores it in the "puzzleapp.token" option.
# The app then requires ?token=<token> on the page request (auth_ui) and on
# every Shiny session (auth_session_ok), which is the websocket that carries
# all app data. Everything sensitive is reachable only through a session:
# IGV files and QC reports are served with session$registerDataObj(), whose
# URLs embed the random per-session id. So a request without the token gets a
# 403 page and can never obtain a session.
#
# The token travels in the query string rather than a cookie because browsers
# do not separate cookies by port: two app instances tunnelled to
# localhost:8898 and localhost:8899 would overwrite each other's cookie.

.token_env_var <- "PUZZLEAPP_TOKEN"

#' Resolve the access token for run_app()
#'
#' @param token `TRUE` (use the `PUZZLEAPP_TOKEN` environment variable, else
#'   generate a random one), `FALSE` (no token), or a string.
#' @return The token string, or `NULL` when access control is off.
#' @keywords internal
resolve_token <- function(token) {
  if (isFALSE(token)) return(NULL)
  if (is.character(token) && length(token) == 1 && nzchar(token)) return(token)
  if (!isTRUE(token)) {
    stop("`token` must be TRUE, FALSE or a non-empty string.")
  }
  from_env <- Sys.getenv(.token_env_var, unset = "")
  if (nzchar(from_env)) return(from_env)
  random_token()
}

# 128 random bits as hex. /dev/urandom where available; R's RNG otherwise
# (Windows), which is weaker but still unguessable over a network in practice.
random_token <- function() {
  bytes <- tryCatch(
    readBin("/dev/urandom", what = "raw", n = 16L),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (length(bytes) != 16L) {
    bytes <- as.raw(sample.int(256L, 16L, replace = TRUE) - 1L)
  }
  paste(format(bytes), collapse = "")
}

# Append token=<token> to a URL's query string.
token_url <- function(url, token) {
  if (is.null(token)) return(url)
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  paste0(url, sep, "token=", utils::URLencode(token, reserved = TRUE))
}

# Does a raw query string ("?a=1&token=...") carry the expected token?
query_has_token <- function(query_string, token) {
  if (is.null(token)) return(TRUE)
  supplied <- shiny::parseQueryString(query_string %||% "")$token
  is.character(supplied) && length(supplied) == 1 && identical(supplied, token)
}

#' Wrap a Shiny UI so the page is served only with the right token
#'
#' @param ui A UI object or a function of `req`.
#' @return A UI function returning `ui` or a 403 response.
#' @keywords internal
auth_ui <- function(ui) {
  function(req) {
    if (!query_has_token(req$QUERY_STRING, getOption("puzzleapp.token"))) {
      return(shiny::httpResponse(
        status = 403L,
        content_type = "text/html; charset=UTF-8",
        content = paste0(
          "<!DOCTYPE html><html><head><title>PuzzleApp</title></head><body>",
          "<h1>403 Forbidden</h1>",
          "<p>Open the full link printed when puzzleapp started, ",
          "including <code>?token=...</code>.</p></body></html>"
        )
      ))
    }
    if (is.function(ui)) ui(req) else ui
  }
}

#' Check a new Shiny session's token, closing the session if it is wrong
#'
#' Call first thing in the server function and return if it is `FALSE`. The
#' query string comes from the browser's own URL, so a client that opens the
#' websocket directly still has to know the token.
#'
#' @param session The Shiny session.
#' @return `TRUE` if the session may proceed.
#' @keywords internal
auth_session_ok <- function(session) {
  token <- getOption("puzzleapp.token")
  if (query_has_token(shiny::isolate(session$clientData$url_search), token)) {
    return(TRUE)
  }
  session$close()
  FALSE
}

# Serve one local HTML file through the session, returning its relative URL.
# Replaces addResourcePath(), whose fixed app-wide URLs needed no session.
register_html_file <- function(session, name, path) {
  session$registerDataObj(
    name = name,
    data = list(path = path),
    filterFunc = function(data, req) {
      shiny::httpResponse(
        status = 200L,
        content_type = "text/html; charset=UTF-8",
        content = readBin(data$path, what = "raw", n = file.info(data$path)$size)
      )
    }
  )
}
