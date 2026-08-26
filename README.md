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

Nothing to install — puzzleapp is available as an `if89` module, which supplies
the R library, a `puzzleapp` launcher, and shared PanelApp and HPO database
snapshots so it works out of the box. Those snapshots are pinned at the time
the module was built; see [Local databases](#local-databases-panelapp-and-hpo)
for how to point at a newer one.

``` bash
module use /g/data/if89/apps/modulefiles
module load puzzleapp/0.2.1
puzzleapp --version
```

Your project needs `gdata/if89` in its storage flags, or `/g/data/if89` will
not be mounted and the module will point at paths that do not exist. Add every
other project holding your data as well — BAM/CRAM files, VCFs, reference
genomes and annotations — since anything not listed in `storage` is invisible
to the job. For example,
`storage=gdata/if89+gdata/project1+scratch/project1+gdata/project2`.

## Via the OnDemand RStudio dashboard

1.  Visit [NCI Batch Connect
    Dashboard](https://are.nci.org.au/pun/sys/dashboard/batch_connect/sessions)
2.  Launch an RStudio job. Add `gdata/if89` to the storage parameter, and set
    the two module fields under **Show advanced settings**:

```
      mem=64GB
      ncpus=4
      jobfs=10GB
      storage=gdata/if89+gdata/project1+gdata/project2

      Module directories:  /g/data/if89/apps/modulefiles
      Modules:             puzzleapp/0.2.1
```

3. Connect/go to Rstudio. File -\> Quit Session -\> Start New Session
4.  Run the following code in the R console

```r
library(puzzleapp)
run_app(port = 8895)
```

Launch it from the R console rather than the RStudio terminal — RStudio Server
proxies console-launched Shiny apps into the Viewer automatically.

## As a batch job

`tests/qsub_launch.sh` starts the app on a compute node and writes the exact
SSH tunnel command to `ssh_connect_<jobid>.txt`. Edit the `#PBS -P` and
`-l storage=` lines for your project, then:

``` bash
qsub -v PORT=8895 tests/qsub_launch.sh
```

## Via SSH, without RStudio

Forward the port and run the app in the same session:

``` bash
# from your laptop
ssh -L 8895:localhost:8895 gadi

# in that session
module use /g/data/if89/apps/modulefiles
module load puzzleapp/0.2.1
puzzleapp --port 8895 --host 127.0.0.1
```

Then open `http://localhost:8895`.

This runs on a *login* node, so it suits a quick look only — use a batch job or
ARE for real work. Bind to `127.0.0.1` there so the app is not reachable by
everyone else on that node.

## Notes on the R version

The module pins **R/4.4.2**, and pulls in `Rlib/4.4.2` and
`intel-compiler/2021.10.0` automatically. Do not load another R version
alongside it — in the ARE form, that means leaving an `R/x.y.z` out of the
Modules field.

If you keep a personal R library, make sure it is not built against a different
R. An unconditional `.libPaths()` call in `~/.Rprofile` puts your packages
ahead of the module's, and if they were compiled under another R version, R
fails to load them with an "undefined symbol" error. Use a per-version
directory instead:

``` r
local({
  p <- file.path("/path/to/your/Rlibs", as.character(getRversion()))
  if (dir.exists(p)) .libPaths(c(p, .libPaths()))
})
```

`puzzleapp --version` reports the module version, the package version R
actually loaded, the R version and the library path. If the first two disagree,
something else on your library path is shadowing the module.

## Launching the app

``` r
run_app(port = 8888, host = "0.0.0.0")
```

* `port` — port to listen on. Default `8888`.
* `host` — interface to bind to. Default `"0.0.0.0"`, which binds **all**
  interfaces so the app is reachable from other machines that can route to the
  host. That is what you want behind an SSH tunnel or NCI OnDemand. On a
  multi-user or internet-facing machine, prefer `host = "127.0.0.1"` and reach
  it through a tunnel — the app has no authentication of its own.

`tests/qsub_launch.sh` is a PBS script that starts the app on a Gadi compute
node and writes the exact SSH tunnel command to `ssh_connect_<jobid>.txt`. Edit
the `#PBS -P` and `-l storage=` lines for your project, then
`qsub -v PORT=8895 tests/qsub_launch.sh`.

## Local databases (PanelApp and HPO)

The home screen has a **Load PanelApp and HPO from local DB** checkbox, enabled
by default. When it is on, the app reads two files:

| Source | Expected file | Default location |
| --- | --- | --- |
| PanelApp Australia | `all_panels.tsv` | `~/.puzzleapp/panelapp` |
| HPO phenotypes | `phenotype_to_genes.txt` | `~/.puzzleapp/phenotype` |
| VEP consequences | `vep_consequences_*.tsv` | bundled in the package |

Neither of the first two is bundled with the R package — the app starts without
them, but panel and phenotype filtering is unavailable until they are in place.

The `if89` module supplies both, so it works with no setup. That snapshot is
fixed at the version the module was built with, and PanelApp in particular is
updated often — **download a current copy and point at it** whenever the panel
data matters to your results. The steps below apply equally to setting up your
own copy from scratch and to superseding the module's.

A directory may either hold the file directly, or hold `<Month>_<Year>`
subdirectories (for example `March_2026/all_panels.tsv`), in which case the
most recent one is used. That lets you keep dated snapshots side by side.

### Populating them

Both are published as GitHub releases. Pick a release tag and download its
asset into a matching `<Month>_<Year>` directory:

``` bash
# PanelApp Australia — https://github.com/KCCGGenomeTechLab/PanelAppDB/releases
mkdir -p ~/.puzzleapp/panelapp/August_2026
curl -fsSL -o ~/.puzzleapp/panelapp/August_2026/all_panels.tsv \
  https://github.com/KCCGGenomeTechLab/PanelAppDB/releases/download/v2026-08-03/all_panels.tsv

# HPO — https://github.com/obophenotype/human-phenotype-ontology/releases
mkdir -p ~/.puzzleapp/phenotype/June_2026
curl -fsSL -o ~/.puzzleapp/phenotype/June_2026/phenotype_to_genes.txt \
  https://github.com/obophenotype/human-phenotype-ontology/releases/download/v2026-06-23/phenotype_to_genes.txt
```

Substitute the current tags — `gh release view --repo KCCGGenomeTechLab/PanelAppDB`
and the equivalent for HPO will tell you what they are.

VEP consequences — a snapshot ships in the package. To regenerate it against a
specific Ensembl VEP release:

``` bash
./tests/download_vep_db.sh release/115.2 /tmp/ensembl_vep_115
```

### Pointing somewhere else

Defaults live in `inst/extdata/app.conf` inside the installed package. Do not
edit that file; override it instead. Precedence is **R option > environment
variable > `app.conf`**.

Per user or per session, in `~/.Rprofile` or before `run_app()`:

``` r
options(puzzleapp.panelapp_db_dir  = "/g/data/if89/.../db/panelapp")
options(puzzleapp.phenotype_db_dir = "/g/data/if89/.../db/phenotype")
```

Site-wide, for a shared install or a Unix modulefile, which cannot set R
options:

``` bash
export PUZZLEAPP_PANELAPP_DB_DIR=/g/data/if89/.../db/panelapp
export PUZZLEAPP_PHENOTYPE_DB_DIR=/g/data/if89/.../db/phenotype
```

Whichever source is used is written to the session log, so it is always clear
where a database was read from — check it after overriding to confirm you got
the snapshot you meant.

On the `if89` module the environment variables are already set, so use the R
option to override: it takes precedence, and needs no change to the module.

``` r
# ~/.Rprofile, or run before run_app()
options(puzzleapp.panelapp_db_dir = "~/.puzzleapp/panelapp")
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

- config_yaml: Path to the pipeline YAML (e.g., tests/configs/sample.yaml).
- filter_table: Path to the two‑column filters file (e.g., tests/filters/*).
- output_dir: Directory to write outputs (default: "pipeline_output").
- nthreads: Number of threads for reading/processing (default: 4).
- verbose: Whether to print progress messages (default: TRUE).


## Preprocessing

A one-time preprocessing step converts VCF files into the tab-delimited TSV format consumed by the app.

```r
library(puzzleapp)

run_preprocess(
  config_yaml = "path/to/config.yml",
  validate    = TRUE,   # strict key checks on the YAML (default TRUE)
  verbose     = TRUE    # progress messages (default TRUE)
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
devtools::test()        # testthat only
```

README.md is edited directly — there is no README.Rmd to knit.

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
