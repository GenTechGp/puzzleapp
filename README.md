# puzzleapp

<!-- badges: start -->

[![R-CMD-check](https://github.com/GenTechGp/puzzleapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GenTechGp/puzzleapp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Visualise and analyse variants

## Quick Installation

Try the following code in your R console:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("GenTechGp/puzzleapp", dependencies = TRUE, lib = "optional path")
library(puzzleapp)
run_app()
```

## Installation

``` bash
git clone --depth 1 https://github.com/GenTechGp/puzzleapp.git
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
3. Connect/go to Rstudio. File -\> Quit Session -\> Start New Session
4.  Run the following code in the R console
```
dirs <- basename(list.dirs(base <- "/g/data/if89/testdir/puzzleapp/app", FALSE, FALSE))
d <- dirs[order(as.Date(dirs, "%d%m%Y"), decreasing = TRUE)][1]
cat("Using library path:", lib <- file.path(base, d, "Rlib"), "\n")
.libPaths(c(lib, .libPaths())); library(puzzleapp); run_app(port = 8895)
```

#### (Optional) Connect without RStudio 
3. Get the compute node number (e.g. gadi-cpu-bdw-0123)
4. Run the following in the local command line (change N(=0123) and PORT(=8895) as needed)
```
N="0123";PORT=8895;ssh -L ${PORT}:localhost:${PORT} gadi -t ssh -L ${PORT}:localhost:${PORT} gadi-cpu-bdw-${N}
```

5. Connect on a separate terminal
```
ssh gadi
ssh gadi-cpu-bdw-${N}
module load intel-compiler-llvm/2025.2.0
module load intel-mkl/2025.2.0
module load R/4.5.0
R

dirs <- basename(list.dirs(base <- "/g/data/if89/testdir/puzzleapp/app", FALSE, FALSE))
d <- dirs[order(as.Date(dirs, "%d%m%Y"), decreasing = TRUE)][1]
cat("Using library path:", lib <- file.path(base, d, "Rlib"), "\n")
.libPaths(c(lib, .libPaths())); library(puzzleapp); run_app(port = 8895)
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

## Command-line based pipeline

Run the core variant filtering without Shiny web application. The pipeline is driven by:
- a YAML config, and
- a tab-delimited filter table with two columns: `Key` and `Value`.

### Usage
```
library(puzzleapp)

run_pipeline(
  config_yaml = "path/to/config.yml",
  filter_table = "path/to/filters.tsv",
  output_dir = "pipeline_output",
  nthreads = 4L,                 # threads for fread/processing
  verbose = TRUE                 # progress messages
)
```

### Parameters

- config_yaml: Path to the pipeline YAML (e.g., tests/configs/samples.yml).
- filter_table: Path to the two‑column filters file (e.g., tests/filters/*).
- output_dir: Directory to write outputs (default: "pipeline_output").
- nthreads: Number of threads for reading/processing (default: 4).
- verbose: Whether to print progress messages (default: TRUE).


## Preprocessing

A one-time preprocessing step converts VCF files into the tab-delimited TSV format consumed by the app.

```r
library(puzzleapp)

run_preprocess(
  config_yaml = "path/to/config.yml"
)
```

The config YAML must contain a `paths` block pointing to input VCFs and the desired output TSV paths:

```yaml
paths:
  snvs_vcf:  /path/to/sample.snvs.vcf.gz   # SNV/indel input
  snvs_tsv:  /path/to/output_snv.tsv        # SNV output (written by preprocess)
  svs_vcf:   /path/to/sample.svs.vcf.gz    # SV input
  svs_tsv:   /path/to/output_sv.tsv         # SV output (written by preprocess)

samples:
  - sample_id: SAMPLE1
    kinship: proband
  - sample_id: SAMPLE2
    kinship: mother
```

### VCF field mapping

The pipeline maps VCF field names to canonical output column names using bundled TSV mapping files. Different VCF callers and reference genomes can use different field names for the same data — this is handled through **user override mapping files** specified in the YAML:

```yaml
paths:
  # ... vcf/tsv paths above ...
  snv_field_mapping: /path/to/hs1_snv_field_mapping.tsv   # optional SNV override
  sv_field_mapping:  /path/to/hs1_sv_field_mapping.tsv    # optional SV override
```

An override TSV only needs rows for fields that differ from the canonical defaults. Missing non-required fields degrade gracefully (warning + NA) rather than stopping.

See [`inst/extdata/docs/vcf_field_mapping.md`](inst/extdata/docs/vcf_field_mapping.md) for full documentation of the mapping schema, computation types, and how to add support for a new VCF caller.

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
