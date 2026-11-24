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

## Docker

A prebuilt image is published to [GHCR](https://github.com/GenTechGp/puzzleapp/pkgs/container/puzzleapp)
on every tagged release — no R or Bioconductor installation needed:

``` bash
docker pull ghcr.io/gentechgp/puzzleapp:latest
```

Pin a specific released version instead of `latest` by tag, e.g.
`ghcr.io/gentechgp/puzzleapp:0.2.3`.

The image dispatches on the first argument to one of three subcommands, each
wrapping the R function of the same shape documented elsewhere in this
README. Mount your data directory (and use the same path inside the
container as on the host) so paths inside your YAML configs resolve.

### Web app

``` bash
docker run --rm -p 8888:8888 -v /path/to/data:/path/to/data \
  ghcr.io/gentechgp/puzzleapp:latest webapp
```

Open the `http://localhost:8888/?token=...` link printed in the log, about 20s
after start. Add `-e PUZZLEAPP_TOKEN=<secret>` to choose the token. See
[Launching the app](#launching-the-app).

For panel/phenotype filtering (see [Local databases](#local-databases-panelapp-and-hpo)),
also mount your `~/.puzzleapp` directory, since the container runs as `root`:

``` bash
docker run --rm -p 8888:8888 \
  -v /path/to/data:/path/to/data \
  -v ~/.puzzleapp:/root/.puzzleapp \
  ghcr.io/gentechgp/puzzleapp:latest webapp
```

### Preprocess

Wraps [`run_preprocess()`](#preprocessing):

``` bash
docker run --rm -v /path/to/data:/path/to/data \
  ghcr.io/gentechgp/puzzleapp:latest preprocess /path/to/data/config.yaml
```

### Command-line pipeline

Wraps [`run_pipeline()`](#command-line-based-pipeline):

``` bash
docker run --rm -v /path/to/data:/path/to/data \
  ghcr.io/gentechgp/puzzleapp:latest pipeline \
  /path/to/data/config.yaml /path/to/data/filters.tsv /path/to/data/output_dir
```

`output_dir` is optional (defaults to `pipeline_output`) — pass an absolute
path under your mount so the output lands somewhere visible on the host.

# For NCI Gadi if89 users

From v0.2.5, puzzleapp is an `if89` Singularity module. Load `singularity`
first; the module does not load it:

``` bash
module load singularity
module use -a /g/data/if89/shpcroot/modules
module load ghcr.io/gentechgp/puzzleapp/0.2.5
puzzleapp <preprocess|pipeline|webapp> ...   # same subcommands as Docker
```

Jobs need `gdata/if89` in `-l storage`, plus every project holding your data,
e.g. `storage=gdata/if89+gdata/project1+scratch/project1`.

## Web app (batch job + SSH tunnel)

Edit the `#PBS -P` and `-l storage` lines of `tests/qsub_launch.sh`, then:

``` bash
qsub tests/qsub_launch.sh
cat ssh_connect_<jobid>.txt      # once the job is running
```

That file has two lines to use: an `ssh -N -L 8898:localhost:<port> -J ...`
command to run on your laptop, which may ask for your credentials twice, and a
`http://localhost:8898/?token=...` link to open. The app listens only on the
compute node's loopback and requires the token, so other Gadi users cannot use
it. Treat the link as a password. Stop the app with `qdel <jobid>`.

ARE Virtual Desktop and RStudio sessions run inside a container that cannot
start Singularity, so use the batch job above.

## Launching the app

``` r
run_app(port = 8888, host = "0.0.0.0", token = TRUE)
```

* `port` — port to listen on. Default `8888`.
* `host` — `"0.0.0.0"` (default) listens on every interface, as Docker needs.
  On a shared machine use `"127.0.0.1"` and an SSH tunnel.
* `token` — the app only answers URLs carrying `?token=<token>`. `TRUE` uses
  the `PUZZLEAPP_TOKEN` environment variable, else a random token; a string sets
  it; `FALSE` disables the check. The full link is printed at startup and opened
  automatically in an interactive session or RStudio.

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

`snvs_tsv`/`svs_tsv` may end in `.tsv` or `.tsv.gz` — `run_preprocess()` writes a gzip-compressed file automatically when the path ends in `.gz`, and puzzleapp reads either transparently, so compressing the output TSVs is a safe way to save disk space.

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
