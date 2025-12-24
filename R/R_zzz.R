# Map package's inst/www to a URL prefix so Shiny can serve static files
.onLoad <- function(libname, pkgname) {
  www_dir <- system.file("www", package = pkgname)
  if (nzchar(www_dir) && dir.exists(www_dir)) {
    # Use a unique prefix to avoid collisions
    shiny::addResourcePath("puzzleapp-assets", www_dir)
  }
}