
<!-- README.md is generated from README.Rmd. Please edit that file -->

# puzzleapp

<!-- badges: start -->

[![R-CMD-check](https://github.com/KCCGGenomeTechLab/puzzleapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KCCGGenomeTechLab/puzzleapp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Visualise and analyse variants

## Quick Installation

``` bash
DEST_DIR=~/puzzleapp_installation
wget https://github.com/KCCGGenomeTechLab/puzzleapp/blob/pack/tests/install_from_github.sh && chmod +x install_from_github.sh && ./install_from_github.sh --dest ${DEST_DIR}
lib_path <- "${DEST_DIR}/Rlib"
.libPaths(c(lib_path, .libPaths()))
library(puzzleapp)
run_app(port=8888) # change port if needed
```

## Installation

``` bash
git clone --branch pack --depth 1 git@github.com:KCCGGenomeTechLab/puzzleapp.git
cd puzzleapp
```

``` r
local_lib <- "optional path"
.libPaths(c(local_lib, .libPaths()))

install.packages(
  [path to repo],
  repos = NULL,
  type = "source",
  lib = local_lib,
  dependencies = TRUE
)

library(puzzleapp)
run_app()
```

# For NCI Gadi if89 users

1.  Visit [NCI Batch Connect
    Dashboard](https://are.nci.org.au/pun/sys/dashboard/batch_connect/sessions)
2.  Launch an RStudio job. Remember to add `gdata/if89` to the storage
    parameter. Example parameters:

<!-- -->

      mem=64GB
      ncpus=4
      jobfs=10GB
      storage=gdata/if89+gdata/project1+gdata/project2
      modules=R/4.5.0

3.  (Optional) File -\> Quit Session -\> Start New Session
4.  Run the following code in the R console

<!-- -->

    lib_path <- "/g/data/if89/testdir/puzzleapp/app/26102025/Rlib"
    if (file.access(lib_path, 4) != 0) cat("no access to /g/data/if89\n")
    .libPaths(c(lib_path, .libPaths()))
    library(puzzleapp)
    run_app(port=8888) # change port if needed

## Development

``` r
local_lib <- "optional path"
.libPaths(c(local_lib, .libPaths()))

devtools::install([path to repo])

library(puzzleapp)
run_app()

remove.packages("puzzleapp", lib=local_lib)

# Other
devtools::document()    # Run after editing functions, roxygen comments, or _PACKAGE.R
devtools::load_all()    # Run while testing functions interactively
devtools::install()     # Run after changes to test outside load_all()
devtools::check(clean = TRUE)
devtools::build_readme() # Update README.md
```
