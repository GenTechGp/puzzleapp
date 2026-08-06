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
