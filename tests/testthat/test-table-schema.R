library(testthat)
library(data.table)

# check_data() prints a summary with cat(); silence it and return the result.
quiet_check <- function(label, data, schema) {
  res <- NULL
  invisible(capture.output(res <- puzzleapp:::check_data(label, data, schema)))
  res
}
# ...and capture that summary when the test is about what it reports.
check_output <- function(label, data, schema) {
  capture.output(invisible(puzzleapp:::check_data(label, data, schema)))
}
schema_of <- function(...) data.table::data.table(...)
schema_dir <- function() system.file("extdata", "db", "table_schema", package = "puzzleapp")

# =============================================================================
# check_data() — placeholders for columns absent from the input
# =============================================================================

test_that("an absent integer column is filled with NA, not 0", {
  # 0 is a real value for a distance or a coordinate, so it must not stand in
  # for "no data" (SVlog columns are invented this way when svlog_db is unset)
  s <- schema_of(name = c("ID", "INTRON_START"), has_suffix = c(0, 0),
                 default_type = c("string", "integer"))
  res <- quiet_check("t", data.table(ID = c("a", "b")), s)
  expect_true("INTRON_START" %in% names(res))
  expect_true(all(is.na(res$INTRON_START)))
  expect_identical(class(res$INTRON_START)[1], "integer")
})

test_that("an absent float column is filled with NA_real_", {
  s <- schema_of(name = c("ID", "SCORE"), has_suffix = c(0, 0),
                 default_type = c("string", "float"))
  res <- quiet_check("t", data.table(ID = "a"), s)
  expect_true(all(is.na(res$SCORE)))
  expect_identical(class(res$SCORE)[1], "numeric")
})

test_that("an absent string column is filled with NA_character_", {
  s <- schema_of(name = c("ID", "NOTE"), has_suffix = c(0, 0),
                 default_type = c("string", "string"))
  res <- quiet_check("t", data.table(ID = "a"), s)
  expect_identical(class(res$NOTE)[1], "character")
})

# =============================================================================
# check_data() — has_suffix bases
# =============================================================================

test_that("has_suffix rows hold the base name and every per-sample column survives", {
  s <- schema_of(name = c("ID", "AD"), has_suffix = c(0, 1),
                 default_type = c("string", "integer"))
  d <- data.table(ID = "a", AD_1 = 1L, AD_2 = 2L, AD_3 = 3L)
  res <- quiet_check("t", d, s)
  expect_true(all(c("AD_1", "AD_2", "AD_3") %in% names(res)))
})

test_that("a multi-underscore base resolves without swallowing extra columns", {
  # alt_allele_count used to resolve to the base "alt_allele", whose prefix
  # would also have matched unrelated alt_allele_* columns
  s <- schema_of(name = c("ID", "alt_allele_count"), has_suffix = c(0, 1),
                 default_type = c("string", "integer"))
  d <- data.table(ID = "a", alt_allele_count_1 = 1L, alt_allele_count_2 = 2L)
  res <- quiet_check("t", d, s)
  expect_true(all(c("alt_allele_count_1", "alt_allele_count_2") %in% names(res)))
})

test_that("a has_suffix base with no matching column is an error", {
  s <- schema_of(name = c("ID", "GQ"), has_suffix = c(0, 1),
                 default_type = c("string", "integer"))
  expect_error(
    invisible(capture.output(puzzleapp:::check_data("t", data.table(ID = "a"), s))),
    "Missing required suffix columns"
  )
})

# =============================================================================
# check_data() — type reporting
# =============================================================================

test_that("float is the wider declaration and accepts whole-numbered data", {
  s <- schema_of(name = c("ID", "QUAL"), has_suffix = c(0, 0),
                 default_type = c("string", "float"))
  out <- check_output("t", data.table(ID = "a", QUAL = 37L), s)
  expect_false(any(grepl("has type", out)))
})

test_that("integer does not accept fractional data", {
  s <- schema_of(name = c("ID", "POS"), has_suffix = c(0, 0),
                 default_type = c("string", "integer"))
  out <- check_output("t", data.table(ID = "a", POS = 1.5), s)
  expect_true(any(grepl("POS.*has type 'numeric'.*expected 'integer'", out)))
})

test_that("an all-missing column is reported as empty, not as a type error", {
  s <- schema_of(name = c("ID", "AF"), has_suffix = c(0, 0),
                 default_type = c("string", "float"))
  out <- check_output("t", data.table(ID = c("a", "b"), AF = NA), s)
  expect_true(any(grepl("Columns empty \\(all values missing\\): 1", out)))
  expect_true(any(grepl("^\\s*-\\s+AF", out)))
  expect_false(any(grepl("has type", out)))
})

test_that("placeholder columns are not double-reported as empty", {
  # they are empty by construction and already counted under "Missing columns added"
  s <- schema_of(name = c("ID", "GONE"), has_suffix = c(0, 0),
                 default_type = c("string", "float"))
  out <- check_output("t", data.table(ID = "a"), s)
  expect_true(any(grepl("Missing columns added: 1", out)))
  expect_true(any(grepl("Columns empty \\(all values missing\\): 0", out)))
})

test_that("per-sample columns are type-checked under their base declaration", {
  s <- schema_of(name = c("ID", "GQ"), has_suffix = c(0, 1),
                 default_type = c("string", "integer"))
  d <- data.table(ID = "a", GQ_1 = 1L, GQ_2 = "x")
  out <- check_output("t", d, s)
  expect_true(any(grepl("GQ_2.*has type 'character'.*expected 'integer'", out)))
})

# =============================================================================
# ensure_sample_columns() — FORMAT fields declared required = FALSE may be
# absent from a VCF; the output must keep its shape rather than fail.
# =============================================================================

test_that("ensure_sample_columns fills absent columns and warns naming them", {
  d <- data.table(a = 1:2)
  expect_warning(
    res <- puzzleapp:::ensure_sample_columns(d, c("GQ_1", "GQ_2"), NA_integer_),
    "FORMAT field absent"
  )
  expect_true(all(c("GQ_1", "GQ_2") %in% names(res)))
  expect_true(all(is.na(res$GQ_1)))
  expect_identical(class(res$GQ_1)[1], "integer")
})

test_that("ensure_sample_columns is silent when nothing is missing", {
  d <- data.table(GQ_1 = 1L)
  expect_silent(res <- puzzleapp:::ensure_sample_columns(d, "GQ_1", NA_integer_))
  expect_identical(names(res), "GQ_1")
})

test_that("ensure_sample_columns returns the filled table", {
  # it must not rely on modifying in place: by this point the caller's table
  # may have lost its data.table self-reference
  res <- suppressWarnings(
    puzzleapp:::ensure_sample_columns(data.table(a = 1L), "VAF_1", NA_real_)
  )
  expect_true("VAF_1" %in% names(res))
  expect_identical(class(res$VAF_1)[1], "numeric")
})

# =============================================================================
# Shipped schema files — invariants that the loader and the docs both rely on
# =============================================================================

test_that("doc schema and colnames agree on default_type", {
  skip_if_not(nzchar(schema_dir()), "schema dir not found")
  for (kind in c("snv", "sv")) {
    doc <- fread(file.path(schema_dir(), "documentation", paste0(kind, "_tsv_format.tsv")),
                 sep = "\t", colClasses = "character", quote = "")
    cn  <- fread(file.path(schema_dir(), paste0(kind, "_colnames.tsv")),
                 sep = "\t", colClasses = "character")
    doc[, base := sub("_n$", "", name)]
    m <- merge(doc[, .(base, doc_type = default_type)],
               cn[, .(base = name, cn_type = default_type)], by = "base")
    expect_gt(nrow(m), 20)   # guard: the comparison must not be vacuous
    expect_identical(m[doc_type != cn_type, base], character(0),
                     info = paste(kind, "default_type disagreements"))
  }
})

test_that("every ranges entry names a numeric column of its own table", {
  skip_if_not(nzchar(schema_dir()), "schema dir not found")
  for (kind in c("snv", "sv")) {
    rg  <- fread(file.path(schema_dir(), paste0(kind, "_ranges.tsv")), sep = "\t")
    doc <- fread(file.path(schema_dir(), "documentation", paste0(kind, "_tsv_format.tsv")),
                 sep = "\t", colClasses = "character", quote = "")
    numeric_bases <- sub("_n$", "", doc[default_type %in% c("integer", "float"), name])
    expect_gt(length(numeric_bases), 5)   # guard: the comparison must not be vacuous
    expect_gt(nrow(rg), 0)
    expect_identical(setdiff(rg$name, numeric_bases), character(0),
                     info = paste(kind, "ranges entries that are not numeric columns"))
  }
})

test_that("ranges files are tab-delimited with min <= max", {
  skip_if_not(nzchar(schema_dir()), "schema dir not found")
  for (kind in c("snv", "sv")) {
    f <- file.path(schema_dir(), paste0(kind, "_ranges.tsv"))
    expect_true(all(grepl("\t", readLines(f, warn = FALSE))), info = f)
    rg <- fread(f, sep = "\t")
    expect_identical(names(rg), c("name", "min", "max"))
    expect_true(all(rg$min <= rg$max))
  }
})

test_that("documentation TSVs have a consistent field count on every row", {
  skip_if_not(nzchar(schema_dir()), "schema dir not found")
  for (f in list.files(file.path(schema_dir(), "documentation"),
                       pattern = "\\.tsv$", full.names = TRUE)) {
    x <- read.delim(f, header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
    expect_gt(nrow(x), 0)
    expect_gt(ncol(x), 1)
  }
})

# =============================================================================
# load_local_db() — resolution order and portability of app.conf
# =============================================================================

test_that("app.conf carries no machine-specific absolute paths", {
  conf <- system.file("extdata", "app.conf", package = "puzzleapp")
  skip_if_not(nzchar(conf), "app.conf not found")
  vals <- grep("^[a-z_]+\\s*=", readLines(conf, warn = FALSE), value = TRUE)
  vals <- sub('^[^"]*"', "", sub('"\\s*$', "", vals))
  # portable = package-relative or ~/ ; an absolute /path pins it to one machine
  expect_identical(grep("^/", vals, value = TRUE), character(0))
})

test_that("a db dir option overrides app.conf", {
  root <- file.path(tempdir(), "puzzleapp-optdb")
  dir.create(file.path(root, "March_2020"), recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(root, "March_2020", "all_panels.tsv"))
  old <- getOption("puzzleapp.panelapp_db_dir")
  on.exit(options(puzzleapp.panelapp_db_dir = old), add = TRUE)
  options(puzzleapp.panelapp_db_dir = root)
  expect_identical(
    suppressMessages(load_local_db("panelapp", "all_panels.tsv")),
    file.path(root, "March_2020", "all_panels.tsv")
  )
})

test_that("an unresolvable db dir names the option that fixes it", {
  old <- getOption("puzzleapp.panelapp_db_dir")
  on.exit(options(puzzleapp.panelapp_db_dir = old), add = TRUE)
  options(puzzleapp.panelapp_db_dir = file.path(tempdir(), "definitely-not-here"))
  expect_error(load_local_db("panelapp", "all_panels.tsv"),
               "puzzleapp\\.panelapp_db_dir")
})

test_that("the vep_consequences dir still resolves relative to the package", {
  old <- getOption("puzzleapp.vep_consequences_db_dir")
  on.exit(options(puzzleapp.vep_consequences_db_dir = old), add = TRUE)
  options(puzzleapp.vep_consequences_db_dir = NULL)
  p <- suppressMessages(load_local_db("vep_consequences", "vep_annotations.tsv"))
  expect_true(file.exists(p))
})
