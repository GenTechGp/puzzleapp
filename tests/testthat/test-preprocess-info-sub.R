library(testthat)
library(data.table)

# =============================================================================
# Tests for INFO_SUB mapping rows (bcftools annotate INFO/SpliceAI) and source
# precedence in extract_annotation_columns()
# =============================================================================

spliceai_header <- data.table(V1 = c(
  "##fileformat=VCFv4.2",
  paste0('##INFO=<ID=SpliceAI,Number=.,Type=String,Description="SpliceAIv1.3.1 variant annotation. ',
         'Format: ALLELE|SYMBOL|DS_AG|DS_AL|DS_DG|DS_DL|DP_AG|DP_AL|DP_DG|DP_DL">')
))
no_spliceai_header <- spliceai_header[1]

# CSQ is already split into columns at this point in process_snv_data()
make_dt <- function(info, csq_ds_dl = NULL) {
  dt <- data.table(INFO = info)
  if (!is.null(csq_ds_dl)) dt[, SpliceAI_pred_DS_DL := csq_ds_dl]
  dt
}

# Canonical CSQ + INFO_SUB rows for two output columns
map_rows <- function() {
  load_field_mapping("snv")[output_column %in% c("Donor_Loss", "_spliceai_symbol")]
}

test_that("canonical SNV mapping declares both sources for each SpliceAI column", {
  m <- load_field_mapping("snv")[grepl("SpliceAI", vcf_field)]
  expect_equal(m[, .N, by = output_column][, unique(N)], 2L)
  expect_setequal(m$field_type, c("CSQ", "INFO_SUB"))
})

test_that("pick_info_sub_entry keeps the whole entry with the highest delta score", {
  sf <- c("ALLELE", "SYMBOL", "DS_AG", "DS_AL", "DS_DG", "DS_DL", "DP_AG", "DP_AL", "DP_DG", "DP_DL")
  v  <- "T|NMBR|0.00|0.00|0.00|0.00|49|-6|-34|1,T|GJE1|0.00|0.00|0.00|0.09|19|-16|-6|39"
  expect_equal(pick_info_sub_entry(v, sf), "T|GJE1|0.00|0.00|0.00|0.09|19|-16|-6|39")

  tie <- "T|A|0.00|0.00|0.00|0.00|1|1|1|1,T|B|0.00|0.00|0.00|0.00|2|2|2|2"
  expect_equal(pick_info_sub_entry(tie, sf), "T|A|0.00|0.00|0.00|0.00|1|1|1|1")

  # without DS_* subfields there is no score to rank by
  expect_equal(pick_info_sub_entry("x|1,y|9", c("A", "B")), "x|1")
})

test_that("add_info_sub_columns splits INFO/SpliceAI and leaves '.' and absent records NA", {
  dt <- make_dt(c("SpliceAI=C|G1|0.01|0.02|0.03|0.04|1|2|3|4;X=1",
                  "X=1;SpliceAI=.",
                  "X=1",
                  "SpliceAI=C|G2|0.00|0.00|0.00|0.00|1|2|3|4,C|G3|0.00|0.00|0.00|0.50|5|6|7|8"))
  dt <- add_info_sub_columns(dt, map_rows(), spliceai_header)
  expect_equal(dt[["SpliceAI|DS_DL"]], c("0.04", NA, NA, "0.50"))
  expect_equal(dt[["SpliceAI|SYMBOL"]], c("G1", NA, NA, "G3"))
})

test_that("add_info_sub_columns adds nothing when the INFO header is absent", {
  dt <- add_info_sub_columns(make_dt("SpliceAI=C|G1|0|0|0|0|1|2|3|4"), map_rows(), no_spliceai_header)
  expect_named(dt, "INFO")
})

test_that("add_info_sub_columns rejects a vcf_field not written TAG|SUBFIELD", {
  bad <- data.table(vcf_field = "SpliceAI", output_column = "Donor_Loss",
                    field_type = "INFO_SUB", required = "FALSE", note = "")
  expect_error(add_info_sub_columns(make_dt("X=1"), bad, spliceai_header), "TAG\\|SUBFIELD")
})

test_that("VEP-only VCF: CSQ value is kept, not overwritten by the undeclared INFO source", {
  dt <- add_info_sub_columns(make_dt("X=1", csq_ds_dl = "0.42"), map_rows(), no_spliceai_header)
  m  <- map_rows()[output_column == "Donor_Loss"]
  expect_silent(extract_annotation_columns(dt, m))
  expect_equal(dt$Donor_Loss, "0.42")
})

test_that("INFO/SpliceAI header declared: it wins for every record, including '.' records", {
  dt <- make_dt(c("SpliceAI=C|G1|0.01|0.02|0.03|0.04|1|2|3|4", "SpliceAI=."),
                csq_ds_dl = c("0.42", "0.99"))
  dt <- add_info_sub_columns(dt, map_rows(), spliceai_header)
  expect_silent(extract_annotation_columns(dt, map_rows()))
  expect_equal(dt$Donor_Loss, c("0.04", NA))
  expect_equal(dt$`_spliceai_symbol`, c("G1", NA))
})

test_that("neither source declared: column is NA with one warning naming both sources", {
  dt <- make_dt("X=1")
  m  <- map_rows()[output_column == "Donor_Loss"]
  expect_warning(extract_annotation_columns(dt, m), "SpliceAI|DS_DL', 'SpliceAI_pred_DS_DL", fixed = TRUE)
  expect_true(all(is.na(dt$Donor_Loss)))
})

test_that("a required output column with no declared source stops", {
  m <- map_rows()[output_column == "Donor_Loss"][, required := "TRUE"]
  expect_error(extract_annotation_columns(make_dt("X=1"), m), "Required annotation field")
})
