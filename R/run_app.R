#' Launch the Shiny App
#'
#' This function launches the Shiny application included in the `puzzleapp` package.
#' It automatically locates the app directory within the package installation.
#'
#' @param port Integer. Port number on which to run the Shiny app. Default is 8888.
#' @param host Character. Host address to bind the app to.
#'   - Use "127.0.0.1" for local testing, or on a shared machine behind an SSH tunnel.
#'   - Use "0.0.0.0" to accept connections on every interface (needed inside
#'     Docker). Default is "0.0.0.0".
#' @param token Access token required in the URL (`?token=...`).
#'   - `TRUE` (default): use the `PUZZLEAPP_TOKEN` environment variable if set,
#'     otherwise generate a random token.
#'   - A string: use that token.
#'   - `FALSE`: no access control; anyone who can reach the port can use the app.
#'
#'   The full URL, token included, is printed at startup and opened in the
#'   browser or RStudio Viewer automatically when R is interactive.
#'
#' @return Invisibly returns the Shiny app object.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Run locally
#'   puzzleapp::run_app(host = "127.0.0.1")
#'
#'   # Run on HPC behind an SSH tunnel
#'   puzzleapp::run_app(port = 9999, host = "127.0.0.1")
#' }
run_app <- function(port = 8888, host = "0.0.0.0", token = TRUE) {
  # Find the app directory inside the installed package
  app_dir <- system.file("app", package = "puzzleapp")

  if (app_dir == "") {
    stop(
      "Could not find app directory. ",
      "Try re-installing the 'puzzleapp' package."
    )
  }

  # Check if shiny is installed
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required but not installed. Please install it first.")
  }

  tok <- resolve_token(token)
  old <- options(puzzleapp.token = tok)
  on.exit(options(old), add = TRUE)

  shown_host <- if (host %in% c("0.0.0.0", "::")) "localhost" else host
  message("puzzleapp: open ",
          token_url(sprintf("http://%s:%s/", shown_host, port), tok))

  # Open the browser (or RStudio's Viewer, which sets shiny.launch.browser to
  # a function) at the URL with the token, so the page is not a 403.
  browser_opt <- getOption("shiny.launch.browser", interactive())
  launch <- if (is.function(browser_opt)) {
    function(url) browser_opt(token_url(url, tok))
  } else if (isTRUE(browser_opt)) {
    function(url) utils::browseURL(token_url(url, tok))
  } else {
    FALSE
  }

  # Run the Shiny app
  invisible(shiny::runApp(
    appDir = app_dir,
    host = host,       # bind to user-specified host
    port = port,
    launch.browser = launch,
    display.mode = "normal"
  ))
}
