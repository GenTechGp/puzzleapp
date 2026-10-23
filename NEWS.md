# puzzleapp 0.2.4

## New features

* SNV preprocessing reads SpliceAI scores written by `bcftools annotate` as
  `INFO/SpliceAI`, as well as those from the VEP SpliceAI plugin in `CSQ`. The
  mapping gained an `INFO_SUB` field type (`TAG|SUBFIELD`, e.g.
  `SpliceAI|DS_AG`), and an output column may now list several sources. When
  the header declares both, `INFO/SpliceAI` is used for every record. When a
  record carries several entries, one per overlapping gene, the entry with the
  highest delta score is kept whole. See `vcf_field_mapping.md`.
* `snvs_tsv` and `svs_tsv` may end in `.tsv.gz`. Preprocessing then writes
  compressed output and the app reads it back transparently. This adds
  `R.utils` to Imports.
* At load time, and in `run_pipeline()`, the session log reports how gene
  symbols overlap between the variant data, PanelApp and the HPO phenotype
  table, separately for SNV and SV data.
* When filters are applied, typed genes that cannot take effect are reported
  as notifications and in the log:
  * custom genes with no variants;
  * custom genes found only in PanelApp, which may be outdated names;
  * subtract genes outside the selected panels, or with no panel selected;
  * names that are not recognised at all.

  These checks are informational and never block filtering.

## Fixes

* PanelApp and custom gene filters missed variants in genes whose symbol is
  not all upper case, such as `C19orf12`. Symbols are now matched to the
  data's own casing.

## Performance

* SNV preprocessing is about 30% faster. Only the FORMAT and sample columns
  now go through the long/wide reshape, and only the mapped CSQ fields are
  split out. Decompression uses multithreaded `bgzip` when it is available,
  and only the VCF header is read to collect the `##` lines. This adds
  `stringi` to Imports.

# puzzleapp 0.2.3

## Fixes

* The Docker publish job lacked `packages: write`, so 0.2.2 pushed no image to
  GHCR. 0.2.3 is the first release with a published image.

# puzzleapp 0.2.2

## New features

* A Docker image, `ghcr.io/gentechgp/puzzleapp`, is built from the release
  tarball on every tagged release and tagged with the version and `latest`.
  Its entrypoint takes a subcommand: `preprocess <config.yaml>`,
  `pipeline <config.yaml> <filter_table.tsv> [output_dir]`, or
  `webapp [port]`. See the Docker section of the README.
* The README documents the if89 module on NCI Gadi: OnDemand RStudio, batch
  job, and SSH use.

# puzzleapp 0.2.1

## New features

* The package version is now shown in the top-right corner of the tab bar, on
  the same line as the tabs, not only under Help > About. It is absolutely
  positioned, so it consumes no layout space of its own.
* The if89 module's launcher gained `puzzleapp --version`, reporting the module
  version, the package version R actually loaded, the R version and the library
  path — a mismatch between the first two indicates a broken install.

## Fixes

* `run_app()` failed immediately with "could not find function
  `genome_server_js`" when launched from an installed package. The bundled
  Shiny app called this internal helper by bare name, which only resolves when
  the package is loaded with `devtools::load_all()`, not via
  `library(puzzleapp)`. It is now called as `puzzleapp:::genome_server_js()`.
  0.2.0 cannot start the web UI; use this release instead.
* Help > About reported a hardcoded "v0.0.1" and linked to a `pack` branch that
  no longer exists. The version is now read from `DESCRIPTION` at run time via
  a single internal helper shared with the Home tab and the launcher, so the
  three cannot disagree.

# puzzleapp 0.2.0

First tagged release, and the version packaged as an NCI `if89` module.

## New features

* The PanelApp, HPO phenotype and VEP consequence database directories can now
  be set with environment variables (`PUZZLEAPP_PANELAPP_DB_DIR`,
  `PUZZLEAPP_PHENOTYPE_DB_DIR`, `PUZZLEAPP_VEP_CONSEQUENCES_DB_DIR`) in
  addition to the existing R options. Precedence is R option > environment
  variable > `inst/extdata/app.conf`. This lets a shared site install (such as
  a Unix modulefile, which cannot set R options) point every user at one copy
  of the databases, while leaving individual users free to override it.

## Documentation

* README rewritten: documents the local database setup, the `host` argument to
  `run_app()`, and installation via the `if89` module rather than the old
  `/g/data/if89/testdir` staging path.

## Fixes

* `tests/install_from_github.sh` defaulted to a `pack` branch that no longer
  exists; it now defaults to `main`.

# puzzleapp 0.1.0

* Initial untagged development version.
