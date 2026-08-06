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
