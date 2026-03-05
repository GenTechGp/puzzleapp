library(testthat)
library(data.table)

# =============================================================================
# Tests for puzzlecore_check_pedigree_sanity()
# =============================================================================

test_that("pedigree sanity: valid trio returns no issues", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected",   sex = "male",   code = 1),
    list(sample_id = "S2", kinship = "father",  status = "unaffected", sex = "male",   code = 2),
    list(sample_id = "S3", kinship = "mother",  status = "unaffected", sex = "female", code = 3)
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_length(issues, 0)
})

test_that("pedigree sanity: missing proband raises issue", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "father",  status = "unaffected", sex = "male",   code = 1),
    list(sample_id = "S2", kinship = "mother",  status = "unaffected", sex = "female", code = 2)
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_true(any(grepl("proband", issues, ignore.case = TRUE)))
})

test_that("pedigree sanity: two probands raises issue", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected", sex = "male",   code = 1),
    list(sample_id = "S2", kinship = "proband", status = "affected", sex = "female", code = 2)
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_true(any(grepl("proband", issues, ignore.case = TRUE)))
})

test_that("pedigree sanity: proband code != 1 raises issue", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected", sex = "male", code = 2)
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_true(any(grepl("code", issues, ignore.case = TRUE)))
})

test_that("pedigree sanity: duplicate sample_ids raise issue", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected",   sex = "male", code = 1),
    list(sample_id = "S1", kinship = "father",  status = "unaffected", sex = "male", code = 2)
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_true(any(grepl("duplicate", issues, ignore.case = TRUE)))
})

test_that("pedigree sanity: two fathers raises issue", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected",   sex = "male",   code = 1),
    list(sample_id = "S2", kinship = "father",  status = "unaffected", sex = "male",   code = 2),
    list(sample_id = "S3", kinship = "father",  status = "unaffected", sex = "male",   code = 3)
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_true(any(grepl("father", issues, ignore.case = TRUE)))
})

test_that("pedigree sanity: missing required field raises issue", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected", code = 1)  # sex missing
  )
  issues <- puzzlecore_check_pedigree_sanity(pedigree)
  expect_true(any(grepl("sex", issues, ignore.case = TRUE)))
})

# =============================================================================
# Tests for puzzlecore_allele_count()
# =============================================================================

test_that("allele_count: Homozygous Recessive affected returns '2'", {
  expect_equal(puzzlecore_allele_count("Homozygous Recessive", "affected", "male"), "2")
})

test_that("allele_count: Homozygous Recessive unaffected returns '0-1'", {
  expect_equal(puzzlecore_allele_count("Homozygous Recessive", "unaffected", "male"), "0-1")
})

test_that("allele_count: Dominant/De Novo affected returns '1-2'", {
  expect_equal(puzzlecore_allele_count("Dominant/De Novo", "affected", "male"), "1-2")
})

test_that("allele_count: Dominant/De Novo unaffected returns '0'", {
  expect_equal(puzzlecore_allele_count("Dominant/De Novo", "unaffected", "female"), "0")
})

test_that("allele_count: Compound Heterozygous affected returns '1'", {
  expect_equal(puzzlecore_allele_count("Compound Heterozygous", "affected", "male"), "1")
})

test_that("allele_count: X-Linked Recessive affected male returns '1'", {
  expect_equal(puzzlecore_allele_count("X-Linked Recessive", "affected", "male"), "1")
})

test_that("allele_count: X-Linked Recessive affected female returns '2'", {
  expect_equal(puzzlecore_allele_count("X-Linked Recessive", "affected", "female"), "2")
})

test_that("allele_count: X-Linked Recessive unaffected male returns '0'", {
  expect_equal(puzzlecore_allele_count("X-Linked Recessive", "unaffected", "male"), "0")
})

test_that("allele_count: status NA returns empty string", {
  expect_equal(puzzlecore_allele_count("Dominant/De Novo", "NA", "male"), "")
})

test_that("allele_count: unknown inheritance returns empty string", {
  expect_equal(puzzlecore_allele_count("Unknown Model", "affected", "male"), "")
})

# =============================================================================
# Tests for puzzlecore_compute_allele_table()
# =============================================================================

test_that("compute_allele_table: Homozygous Recessive trio", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected",   sex = "male",   code = 1),
    list(sample_id = "S2", kinship = "father",  status = "unaffected", sex = "male",   code = 2),
    list(sample_id = "S3", kinship = "mother",  status = "unaffected", sex = "female", code = 3)
  )
  result <- puzzlecore_compute_allele_table(pedigree, "Homozygous Recessive")
  expect_equal(result[["S1"]], "2")
  expect_equal(result[["S2"]], "0-1")
  expect_equal(result[["S3"]], "0-1")
})

test_that("compute_allele_table: empty pedigree returns empty list", {
  result <- puzzlecore_compute_allele_table(list(), "Dominant/De Novo")
  expect_length(result, 0)
})

test_that("compute_allele_table: empty inheritance returns empty list", {
  pedigree <- list(
    list(sample_id = "S1", kinship = "proband", status = "affected", sex = "male", code = 1)
  )
  result <- puzzlecore_compute_allele_table(pedigree, "")
  expect_length(result, 0)
})

# =============================================================================
# Tests for puzzlecore_parse_filter_table() using real filter files
# =============================================================================

test_that("parse_filter_table: f1.tsv parses SNV annotations correctly", {
  f1_path <- testthat::test_path("../filters/f1.tsv")
  skip_if_not(file.exists(f1_path), "f1.tsv not found")

  result <- puzzlecore_parse_filter_table(f1_path)

  expect_named(result, c("snv_filters", "sv_filters"))

  snv <- result$snv_filters
  expect_true("Stop gained" %in% snv$annotation_filter)
  expect_true("Missense variant" %in% snv$annotation_filter)
  expect_true("Pathogenic" %in% snv$clinvar_filter)
  expect_equal(snv$af_value, 0.001)
  expect_equal(snv$spliceai_filter, 0.1)
  expect_equal(snv$revel_value, 0)
  expect_true(snv$affected_only)
  expect_equal(snv$genotype_quality_value, 20)
  expect_equal(snv$inheritance_filter, "Dominant/De Novo")
  expect_true("HP:0001257" %in% snv$hpo_terms_list)
})

test_that("parse_filter_table: f1.tsv parses SV filters correctly", {
  f1_path <- testthat::test_path("../filters/f1.tsv")
  skip_if_not(file.exists(f1_path), "f1.tsv not found")

  result <- puzzlecore_parse_filter_table(f1_path)
  sv <- result$sv_filters

  expect_true(sv$affected_only)
  expect_null(sv$af_value)
  expect_equal(sv$genotype_quality_value, 20)
  expect_equal(sv$inheritance_filter, "Dominant/De Novo")
  expect_true("Mendeliome" %in% sv$panelapp_filter)
})

test_that("parse_filter_table: f2.tsv - blank numerics return NULL", {
  f2_path <- testthat::test_path("../filters/f2.tsv")
  skip_if_not(file.exists(f2_path), "f2.tsv not found")

  result <- puzzlecore_parse_filter_table(f2_path)
  snv <- result$snv_filters

  # f2 has blank SpliceAI, REVEL, AlphaMissense, gnomADv4 AF
  expect_null(snv$spliceai_filter)
  expect_null(snv$revel_value)
  expect_null(snv$af_value)
})

test_that("parse_filter_table: data.table input works (no file needed)", {
  dt <- data.table::data.table(
    V1 = c("SNV_Annotation", "SNV_Pathogenicity", "Inheritance", "Treat_Negative"),
    V2 = c("Missense variant;Stop gained", "Pathogenic", "Homozygous Recessive", "TRUE")
  )
  result <- puzzlecore_parse_filter_table(dt)
  expect_equal(result$snv_filters$annotation_filter, c("Missense variant", "Stop gained"))
  expect_equal(result$snv_filters$clinvar_filter, "Pathogenic")
  expect_equal(result$snv_filters$inheritance_filter, "Homozygous Recessive")
  expect_true(result$snv_filters$treat_negative)
})

test_that("parse_filter_table: missing key returns safe defaults", {
  dt <- data.table::data.table(
    V1 = c("SNV_Annotation"),
    V2 = c("Missense variant")
  )
  result <- puzzlecore_parse_filter_table(dt)
  # Keys not present in file should yield empty/NULL/FALSE defaults
  expect_length(result$snv_filters$clinvar_filter, 0)
  expect_null(result$snv_filters$af_value)
  expect_false(result$snv_filters$treat_negative)
  expect_false(result$snv_filters$affected_only)
})

# =============================================================================
# Tests for compare_allele_count() (internal helper)
# =============================================================================

test_that("compare_allele_count: exact match", {
  result <- puzzleapp:::compare_allele_count(c(0, 1, 2), "2")
  expect_equal(result, c(FALSE, FALSE, TRUE))
})

test_that("compare_allele_count: range match", {
  result <- puzzleapp:::compare_allele_count(c(0, 1, 2), "1-2")
  expect_equal(result, c(FALSE, TRUE, TRUE))
})

test_that("compare_allele_count: range 0-1", {
  result <- puzzleapp:::compare_allele_count(c(0, 1, 2), "0-1")
  expect_equal(result, c(TRUE, TRUE, FALSE))
})