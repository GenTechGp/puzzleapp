library(testthat)
library(yaml)
library(data.table)

# Helper: path to the shared test config
sample_yaml_path <- function() testthat::test_path("../configs/sample.yaml")

# =============================================================================
# Basic YAML structure
# =============================================================================

test_that("sample.yaml can be read without error", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_type(cfg, "list")
})

test_that("sample.yaml has top-level keys: samples, paths, dependencies", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_true("samples"      %in% names(cfg))
  expect_true("paths"        %in% names(cfg))
  expect_true("dependencies" %in% names(cfg))
})

# =============================================================================
# samples block
# =============================================================================

test_that("samples block has 4 entries", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_length(cfg$samples, 4)
})

test_that("each sample has required fields: sample_id, kinship, status, sex, code", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  required <- c("sample_id", "kinship", "status", "sex", "code")
  for (s in cfg$samples) {
    expect_true(all(required %in% names(s)),
                info = paste("Sample missing fields:", s$sample_id))
  }
})

test_that("first sample is the proband", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_equal(cfg$samples[[1]]$kinship, "proband")
  expect_equal(cfg$samples[[1]]$status,  "affected")
  expect_equal(cfg$samples[[1]]$code,    1L)
})

test_that("proband sample_id matches expected value", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_equal(cfg$samples[[1]]$sample_id, "SAMPLE_ID-00-RF-01")
})

test_that("sample codes are unique integers 1..N", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  codes <- vapply(cfg$samples, function(s) s$code, integer(1))
  expect_equal(sort(codes), seq_along(cfg$samples))
})

test_that("sample_ids are all unique", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  ids <- vapply(cfg$samples, function(s) s$sample_id, character(1))
  expect_equal(length(ids), length(unique(ids)))
})

test_that("sex values are all valid", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  valid_sex <- c("male", "female", "NA", "unknown")
  for (s in cfg$samples) {
    expect_true(tolower(s$sex) %in% valid_sex,
                info = paste("Invalid sex for", s$sample_id, ":", s$sex))
  }
})

test_that("kinship values are all non-empty strings", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  for (s in cfg$samples) {
    expect_true(nzchar(s$kinship),
                info = paste("Empty kinship for", s$sample_id))
  }
})

# =============================================================================
# paths block
# =============================================================================

test_that("paths block has snvs_vcf, snvs_tsv, svs_vcf, svs_tsv", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_true("snvs_vcf" %in% names(cfg$paths))
  expect_true("snvs_tsv" %in% names(cfg$paths))
  expect_true("svs_vcf"  %in% names(cfg$paths))
  expect_true("svs_tsv"  %in% names(cfg$paths))
})

test_that("paths values are non-empty strings", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  for (key in names(cfg$paths)) {
    val <- cfg$paths[[key]]
    expect_true(is.character(val) && nzchar(val),
                info = paste("paths$", key, "is empty or not a string"))
  }
})

test_that("snvs_tsv and svs_tsv end with .tsv", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_true(grepl("\\.tsv$", cfg$paths$snvs_tsv))
  expect_true(grepl("\\.tsv$", cfg$paths$svs_tsv))
})

test_that("snvs_vcf and svs_vcf end with .vcf.gz", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_true(grepl("\\.vcf\\.gz$", cfg$paths$snvs_vcf))
  expect_true(grepl("\\.vcf\\.gz$", cfg$paths$svs_vcf))
})

# =============================================================================
# dependencies block
# =============================================================================

test_that("dependencies block has panel_app, vep_consequences, phenotype_data", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  expect_true("panel_app"        %in% names(cfg$dependencies))
  expect_true("vep_consequences" %in% names(cfg$dependencies))
  expect_true("phenotype_data"   %in% names(cfg$dependencies))
})

test_that("dependency paths are non-empty strings", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  for (key in names(cfg$dependencies)) {
    val <- cfg$dependencies[[key]]
    expect_true(is.character(val) && nzchar(val),
                info = paste("dependencies$", key, "is empty"))
  }
})

# =============================================================================
# Downstream: pedigree building (as run_preprocess does it)
# =============================================================================

test_that("pedigree data.table can be built from samples block", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())
  ped <- data.table::rbindlist(lapply(cfg$samples, data.table::as.data.table), fill = TRUE)
  expect_true(data.table::is.data.table(ped))
  expect_true("sample_id" %in% names(ped))
  expect_true("kinship"   %in% names(ped))
  expect_equal(nrow(ped), length(cfg$samples))
})

test_that("pedigree built from sample.yaml passes puzzlecore_check_pedigree_sanity", {
  skip_if_not(file.exists(sample_yaml_path()), "sample.yaml not found")
  cfg <- yaml::read_yaml(sample_yaml_path())

  # Mirrors how run_pipeline() builds the samples_list
  samples_list <- lapply(cfg$samples, function(s) {
    list(
      sample_id = s$sample_id,
      kinship   = if (is.null(s$kinship) || s$kinship == "NA") "unknown" else s$kinship,
      status    = if (is.null(s$status)  || s$status  == "NA") "unknown" else s$status,
      sex       = if (is.null(s$sex)     || s$sex     == "NA") "unknown" else s$sex,
      code      = s$code,
      bam       = s$bam,
      coverage  = s$coverage
    )
  })

  issues <- puzzlecore_check_pedigree_sanity(samples_list)
  expect_length(issues, 0)
})

# =============================================================================
# run_preprocess() validation errors (no VCF files needed)
# =============================================================================

test_that("run_preprocess() errors on missing yaml file", {
  expect_error(
    run_preprocess("nonexistent_config.yaml"),
    regexp = "not found|No such file",
    ignore.case = TRUE
  )
})

test_that("run_preprocess() errors when YAML has no samples block", {
  tmp <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(paths = list(snvs_vcf = "a.vcf.gz", snvs_tsv = "a.tsv")), tmp)
  on.exit(unlink(tmp))
  expect_error(run_preprocess(tmp), regexp = "samples")
})

test_that("run_preprocess() errors when YAML has no paths block", {
  tmp <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(samples = list(list(sample_id = "S1", kinship = "proband"))), tmp)
  on.exit(unlink(tmp))
  expect_error(run_preprocess(tmp), regexp = "paths")
})

test_that("run_preprocess() errors when snvs_vcf given but snvs_tsv missing", {
  tmp <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(
    samples = list(list(sample_id = "S1", kinship = "proband")),
    paths   = list(snvs_vcf = tempfile(fileext = ".vcf.gz"))  # no snvs_tsv
  ), tmp)
  on.exit(unlink(tmp))
  expect_error(run_preprocess(tmp), regexp = "snvs_tsv")
})

test_that("run_preprocess() errors when neither snvs_vcf nor svs_vcf is provided", {
  tmp <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(
    samples = list(list(sample_id = "S1", kinship = "proband")),
    paths   = list(work_dir = "/tmp")
  ), tmp)
  on.exit(unlink(tmp))
  expect_error(run_preprocess(tmp), regexp = "snvs_vcf|svs_vcf")
})
