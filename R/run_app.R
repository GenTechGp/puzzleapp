#' Run the Puzzle App
#'
#' Launches the Shiny application contained in this package.
#'
#' @return Runs the Shiny app
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "puzzleapp")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `puzzleapp`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
