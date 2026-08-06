# Single source for the version string shown on the Home tab, in Help > About,
# and by `puzzleapp --version`. Read from DESCRIPTION at run time so a bump in
# DESCRIPTION cannot leave a hardcoded copy behind.
puzzleapp_version <- function() {
  as.character(utils::packageVersion("puzzleapp"))
}

# Map package's inst/www to a URL prefix so Shiny can serve static files
.onLoad <- function(libname, pkgname) {
  www_dir <- system.file("www", package = pkgname)
  if (nzchar(www_dir) && dir.exists(www_dir)) {
    # Use a unique prefix to avoid collisions
    shiny::addResourcePath("puzzleapp-assets", www_dir)
  }
}