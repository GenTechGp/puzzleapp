library(testthat)
library(data.table)

# =============================================================================
# escape_expr_value() — filter predicates are assembled as R source and
# eval(parse())'d, so any apostrophe in a value must not close the literal.
# =============================================================================

test_that("escape_expr_value leaves ordinary values untouched", {
  expect_identical(puzzleapp:::escape_expr_value("missense_variant"), "missense_variant")
  expect_identical(puzzleapp:::escape_expr_value(c("a", "b")), c("a", "b"))
})

test_that("escape_expr_value escapes apostrophes", {
  expect_identical(puzzleapp:::escape_expr_value("5'UTR variant"), "5\\'UTR variant")
})

test_that("escape_expr_value escapes backslashes before apostrophes", {
  # a lone backslash must be doubled so the literal survives parsing
  expect_identical(puzzleapp:::escape_expr_value("a\\b"), "a\\\\b")
})

test_that("escaped values produce a parseable expression", {
  vals <- c("5'UTR variant", "3'UTR variant", "Intron variant")
  expr <- sprintf("grepl('%s', X, ignore.case = TRUE)",
                  paste(puzzleapp:::escape_expr_value(vals), collapse = "|"))
  expect_silent(parse(text = expr))
})

test_that("escaping does not change what the pattern matches", {
  vals <- c("5'UTR variant", "3'UTR variant")
  expr <- sprintf("grepl('%s', X, ignore.case = TRUE)",
                  paste(puzzleapp:::escape_expr_value(vals), collapse = "|"))
  X <- c("5'UTR variant", "intron_variant")
  expect_identical(eval(parse(text = expr)), c(TRUE, FALSE))
})

# =============================================================================
# text_filter()
# =============================================================================

test_that("text_filter returns NULL for an empty value set", {
  expect_null(puzzleapp:::text_filter("VEP_CONSEQUENCE", character(0)))
})

test_that("text_filter output parses when values contain apostrophes", {
  # regression: the shipped Dominant_DeNovo_Permissive preset carries
  # "5'UTR variant", which used to abort filtering with a syntax error
  expr <- puzzleapp:::text_filter("VEP_CONSEQUENCE", c("5'UTR variant", "3'UTR variant"))
  expect_silent(parse(text = expr))
})

test_that("text_filter output evaluates against data", {
  expr <- puzzleapp:::text_filter("VEP_CONSEQUENCE", c("stop_gained", "5'UTR variant"))
  d <- data.table(VEP_CONSEQUENCE = c("stop_gained", "5'UTR variant", "intron_variant"))
  expect_identical(d[, eval(parse(text = expr))], c(TRUE, TRUE, FALSE))
})

# =============================================================================
# puzzlecore_parse_filter_table() — consequence label -> VEP term mapping.
# The Shiny path maps when reading filter files; headless callers must pass
# vep_consequences or annotation filters silently match nothing.
# =============================================================================

vep_path <- function() {
  system.file("extdata", "db", "vep_consequences", "October_2025",
              "vep_annotations.tsv", package = "puzzleapp")
}
preset_path <- function() {
  system.file("extdata", "pre_saved_filters", "Dominant_DeNovo_Permissive.tsv",
              package = "puzzleapp")
}

test_that("filter table keeps human labels when no vep_consequences is supplied", {
  skip_if_not(nzchar(preset_path()), "preset not found")
  f <- puzzlecore_parse_filter_table(preset_path())
  expect_true(any(grepl("Stop gained", f$snv_filters$annotation_filter, fixed = TRUE)))
})

test_that("filter table maps labels to VEP terms when vep_consequences is supplied", {
  skip_if_not(nzchar(preset_path()) && nzchar(vep_path()), "fixtures not found")
  vc <- puzzlecore_load_vep_consequences(vep_path())
  f <- puzzlecore_parse_filter_table(preset_path(), vep_consequences = vc)
  expect_true("stop_gained" %in% f$snv_filters$annotation_filter)
  expect_true("5_prime_UTR_variant" %in% f$snv_filters$annotation_filter)
  # the mapped terms carry no apostrophes
  expect_false(any(grepl("'", f$snv_filters$annotation_filter, fixed = TRUE)))
})

test_that("consequence mapping is idempotent", {
  skip_if_not(nzchar(preset_path()) && nzchar(vep_path()), "fixtures not found")
  vc <- puzzlecore_load_vep_consequences(vep_path())
  once  <- puzzlecore_parse_filter_table(preset_path(), vep_consequences = vc)
  # feed the already-mapped values back through the mapper
  dt <- data.table(V1 = "SNV_Annotation",
                   V2 = paste(once$snv_filters$annotation_filter, collapse = ";"))
  twice <- puzzlecore_parse_filter_table(dt, vep_consequences = vc)
  expect_identical(twice$snv_filters$annotation_filter, once$snv_filters$annotation_filter)
})

test_that("a filter table without annotation keys is unaffected by mapping", {
  skip_if_not(nzchar(vep_path()), "vep table not found")
  vc <- puzzlecore_load_vep_consequences(vep_path())
  dt <- data.table(V1 = "SNV_Genotype quality", V2 = "20")
  expect_identical(
    puzzlecore_parse_filter_table(copy(dt), vep_consequences = vc),
    puzzlecore_parse_filter_table(copy(dt))
  )
})

# =============================================================================
# shiny.error hook — Shiny invokes it with NO arguments, so a function(e)
# signature throws and masks every real error in the app.
# =============================================================================

test_that("the shiny.error hook takes no arguments", {
  skip_if_not_installed("shiny")
  old <- getOption("shiny.error")
  on.exit(options(shiny.error = old), add = TRUE)
  invisible(capture.output(suppressMessages(
    setup_app_logging(logs_dir = file.path(tempdir(), "puzzleapp-test-logs"), console = FALSE)
  )))
  h <- getOption("shiny.error")
  expect_true(is.function(h))
  expect_length(formals(h), 0L)
})

test_that("the shiny.error hook never throws, so it cannot mask the real error", {
  skip_if_not_installed("shiny")
  old <- getOption("shiny.error")
  on.exit(options(shiny.error = old), add = TRUE)
  invisible(capture.output(suppressMessages(
    setup_app_logging(logs_dir = file.path(tempdir(), "puzzleapp-test-logs"), console = FALSE)
  )))
  # called bare, exactly as shiny does, with no error in flight
  expect_no_error(getOption("shiny.error")())
})
