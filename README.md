
<!-- README.md is generated from README.Rmd. Please edit that file -->

# puzzleapp

<!-- badges: start -->

[![R-CMD-check](https://github.com/KCCGGenomeTechLab/puzzleapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KCCGGenomeTechLab/puzzleapp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Visualise and analyse variants

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
