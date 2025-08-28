#' Launch the Shiny App
#'
#' This function launches the Shiny application included in the `puzzleapp` package.
#' It automatically locates the app directory within the package installation.
#'
#' @param port Integer. Port number on which to run the Shiny app. Default is 8888.
#' @param host Character. Host address to bind the app to.
#'   - Use "127.0.0.1" for local testing.
#'   - Use "0.0.0.0" for HPC remote access (requires SSH tunnel or OnDemand). Default is "0.0.0.0".
#'
#' @return Invisibly returns the Shiny app object.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Run locally
#'   puzzleapp::run_app(host = "127.0.0.1")
#'
#'   # Run on HPC / remote server
#'   puzzleapp::run_app(port = 9999, host = "0.0.0.0")
#' }
run_app <- function(port = 8888, host = "0.0.0.0") {
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
  
  # Run the Shiny app
  invisible(shiny::runApp(
    appDir = app_dir,
    host = host,       # bind to user-specified host
    port = port,
    display.mode = "normal"
  ))
}
