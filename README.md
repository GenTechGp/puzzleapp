# puzzleapp

<!-- badges: start -->

[![R-CMD-check](https://github.com/KCCGGenomeTechLab/puzzleapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KCCGGenomeTechLab/puzzleapp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Visualise and analyse variants

## Quick Installation

Try the following code in your R console:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("KCCGGenomeTechLab/puzzleapp", dependencies = TRUE, lib = "optional path")
library(puzzleapp)
run_app()
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

```
      mem=64GB
      ncpus=4
      jobfs=10GB
      storage=gdata/if89+gdata/project1+gdata/project2
      modules=R/4.5.0
```
3.  (Optional) File -\> Quit Session -\> Start New Session
4.  Run the following code in the R console
```
    library(puzzleapp, lib="/g/data/if89/testdir/puzzleapp/app/31102025/Rlib")
    run_app(port=8888) # change port if needed
```

## Working directory (shared-safe mode)

```
# Example directory tree
puzzleapp/            <- Base folder (read-only for group users)
├── saved_filters/     <- Safe folder for files
│   ├── f0.tsv         <- You can add new files here
│   ├── f1.tsv         <- Cannot overwrite other users’ files
│   └── f2.tsv
└── saved_sessions/    <- Safe folder for subdirectories
    ├── sample_A/      <- You can create new sessions under this sample
    │   ├── session_a/ <- Cannot overwrite existing session_a (other users’ data)
    │   └── session_b/ <- You can create new sessions under this sample
    └── sample_B/      <- You can create this new sample folder
        └── session_c/ <- You can create sessions under your own sample
```
* `puzzleapp/`: traverse and list only, do not create files or subdirs here.
* `saved_filters/`: add your own files; cannot delete/overwrite others’ files.
* `saved_sessions/`: create new sample folders or session subdirs; cannot overwrite existing sessions of other users.
* `saved_sessions/sample/`: add subdir freely; existing sessions (subdirs) from others are protected.

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

Officially supported: R ≥ 4.4 (Bioconductor 3.19).

### R 4.3.x installation notes

- igvShiny is not available on Bioconductor for R 4.3 (Bioconductor
  3.18); install it from GitHub instead.
- Then install puzzleapp from a local checkout after lowering the R
  version requirement in DESCRIPTION.

``` r
remotes::install_github("gladkia/igvShiny", upgrade = "never")
```

``` bash
sed -i 's/Depends: R (>= 4.4)/Depends: R (>= 4.3)/' DESCRIPTION
R CMD INSTALL [path to repo]
```
