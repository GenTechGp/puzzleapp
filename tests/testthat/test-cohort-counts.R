library(testthat)
library(data.table)

# =============================================================================
# Cohort carrier counts: paths$snvs_cohort_counts, read_cohort_counts(),
# add_cohort_counts(), and inst/scripts/make_cohort_counts.sh
# =============================================================================

write_tsv <- function(lines, gz = FALSE) {
  path <- tempfile(fileext = if (gz) ".tsv.gz" else ".tsv")
  con <- if (gz) gzfile(path, "w") else file(path, "w")
  writeLines(lines, con)
  close(con)
  path
}

header <- "CHROM\tPOS\tREF\tALT\tN_Cohort"

# --- read_cohort_counts() ----------------------------------------------------

test_that("reads plain and gzipped tables, ignoring extra columns", {
  lines <- c(paste0(header, "\textra"), "chr1\t10\tA\tC\t3\tx", "chr2\t5\tG\tA\t1\tx")
  for (gz in c(FALSE, TRUE)) {
    counts <- read_cohort_counts(write_tsv(lines, gz = gz))
    expect_named(counts, c("CHROM", "POS", "REF", "ALT", "N_Cohort"))
    expect_equal(nrow(counts), 2L)
    expect_type(counts$POS, "integer")
    expect_type(counts$N_Cohort, "integer")
    expect_equal(counts$N_Cohort, c(3L, 1L))
  }
})

test_that("keeps allele strings as text", {
  # T/F would otherwise be read as logical, and a numeric-only CHROM as integer
  counts <- read_cohort_counts(write_tsv(c(header, "1\t10\tT\tF\t2")))
  expect_type(counts$CHROM, "character")
  expect_equal(counts$REF, "T")
  expect_equal(counts$ALT, "F")
})

test_that("stops on a missing file", {
  expect_error(read_cohort_counts(tempfile(fileext = ".tsv")), "not found")
})

test_that("stops on a missing column, naming it", {
  path <- write_tsv(c("CHROM\tPOS\tREF\tN_Cohort", "chr1\t10\tA\t3"))
  expect_error(read_cohort_counts(path), "missing column\\(s\\) ALT")
})

test_that("stops on a duplicated allele", {
  path <- write_tsv(c(header, "chr1\t10\tA\tC\t3", "chr1\t10\tA\tC\t4"))
  expect_error(read_cohort_counts(path), "more than one row for chr1 10 A C")
})

test_that("stops on counts or positions that are not whole numbers >= 0", {
  bad <- list(
    N_Cohort = "chr1\t10\tA\tC\t1.5",
    N_Cohort = "chr1\t10\tA\tC\tNA",
    N_Cohort = "chr1\t10\tA\tC\t-1",
    POS      = "chr1\tabc\tA\tC\t2"
  )
  for (i in seq_along(bad)) {
    expect_error(read_cohort_counts(write_tsv(c(header, bad[[i]]))),
                 paste("column", names(bad)[i], "must hold whole numbers"))
  }
})

# --- add_cohort_counts() -----------------------------------------------------

snv_records <- function() {
  data.table(CHROM = c("chr1", "chr1", "chr1", "chr3"),
             POS   = c(10L, 50L, 50L, 7L),
             REF   = c("A", "A", "A", "C"),
             ALT   = c("C", "C", "T", "G"),
             ord   = 1:4)
}

test_that("joins on exact allele, keeping row order; unmatched records are NA", {
  path <- write_tsv(c(header, "chr1\t10\tA\tC\t3", "chr1\t50\tA\tT\t2", "chr2\t5\tG\tA\t1"))
  dt <- snv_records()
  expect_output(add_cohort_counts(dt, path), "matched for 2/4 records")
  expect_equal(dt$ord, 1:4)
  expect_equal(dt$N_Cohort, c(3L, NA, 2L, NA))
})

test_that("no table gives an all-NA integer column", {
  dt <- snv_records()
  add_cohort_counts(dt, NA)
  expect_type(dt$N_Cohort, "integer")
  expect_true(all(is.na(dt$N_Cohort)))
})

test_that("warns when no record matches (e.g. chr1 vs 1)", {
  path <- write_tsv(c(header, "1\t10\tA\tC\t3"))
  dt <- snv_records()
  expect_warning(capture.output(add_cohort_counts(dt, path)), "No SNV records matched")
})

# --- run_preprocess() key handling -------------------------------------------

write_config <- function(paths) {
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(list(samples = list(list(sample_id = "S1", kinship = "proband")),
                        paths = paths), path)
  path
}

test_that("run_preprocess() stops when snvs_vcf_cohort is set, pointing to snvs_cohort_counts", {
  cfg <- write_config(list(snvs_vcf = "in.vcf.gz", snvs_tsv = "out.tsv",
                           snvs_vcf_cohort = "/some/cohort.vcf.gz"))
  expect_error(run_preprocess(cfg, verbose = FALSE),
               "snvs_vcf_cohort is no longer supported.*snvs_cohort_counts")
})

test_that("run_preprocess() treats an empty snvs_vcf_cohort as unset", {
  # Passes the cohort check and fails later, on the missing SNV VCF
  cfg <- write_config(list(snvs_vcf = "missing.vcf.gz", snvs_tsv = tempfile(),
                           snvs_vcf_cohort = ""))
  expect_error(run_preprocess(cfg, verbose = FALSE), "SNV VCF not found")
})

test_that("run_preprocess() stops early on a missing snvs_cohort_counts file", {
  cfg <- write_config(list(snvs_vcf = "missing.vcf.gz", snvs_tsv = tempfile(),
                           snvs_cohort_counts = "/no/such/counts.tsv.gz"))
  expect_error(run_preprocess(cfg, verbose = FALSE),
               "snvs_cohort_counts specified but file not found")
})

# --- make_cohort_counts.sh ---------------------------------------------------

test_that("make_cohort_counts.sh counts carriers like puzzleapp always has", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("bcftools")) || !nzchar(Sys.which("bgzip")),
          "bcftools/bgzip not on PATH")
  script <- system.file("scripts", "make_cohort_counts.sh", package = "puzzleapp")
  skip_if(!nzchar(script), "make_cohort_counts.sh not installed")

  # Every allele has 3 carriers out of 5, except chr1:30 (none): het/hom,
  # half-calls, a multi-allelic site that the script splits, haploid with
  # diploid, and an allele in two records (chr1:60 A>G, carriers S1+S2 and
  # S1+S3: S1 is counted once, so 3 rather than 4)
  vcf_lines <- c(
    "##fileformat=VCFv4.2",
    "##contig=<ID=chr1,length=1000>",
    "##contig=<ID=chrX,length=1000>",
    '##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">',
    paste("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT",
          "S1", "S2", "S3", "S4", "S5", sep = "\t"),
    paste("chr1", 10, "het_hom",   "A", "C",   ".", ".", ".", "GT", "0/1", "0|1", "1/1", "0/0", "./.", sep = "\t"),
    paste("chr1", 20, "halfcalls", "A", "G",   ".", ".", ".", "GT", "./1", "1/.", ".|1", "./0", ".",   sep = "\t"),
    paste("chr1", 30, "none",      "A", "T",   ".", ".", ".", "GT", "0/0", "./.", "0/0", ".",   "0|0", sep = "\t"),
    paste("chr1", 50, "multi",     "A", "C,T", ".", ".", ".", "GT", "1/2", "0/2", "2/2", "0/1", "1/1", sep = "\t"),
    paste("chr1", 60, "dup_a",     "A", "G",   ".", ".", ".", "GT", "0/1", "1/1", "0/0", "./.", "0/0", sep = "\t"),
    paste("chr1", 60, "dup_b",     "A", "G,T", ".", ".", ".", "GT", "1|0", "0/0", "./1", "0/2", ".",   sep = "\t"),
    paste("chrX", 40, "haploid",   "G", "A",   ".", ".", ".", "GT", "1",   "0",   "1/1", "0/1", ".",   sep = "\t")
  )
  vcf <- tempfile(fileext = ".vcf")
  writeLines(vcf_lines, vcf)
  expect_equal(system2("bgzip", c("-f", shQuote(vcf))), 0L)
  vcf_gz <- paste0(vcf, ".gz")

  run_script <- function() {
    out <- tempfile(fileext = ".tsv.gz")
    status <- system2("bash", shQuote(c(script, vcf_gz, out, "1")),
                      stdout = FALSE, stderr = FALSE)
    expect_equal(status, 0L)
    counts <- read_cohort_counts(out)
    setorder(counts, CHROM, POS, ALT)
    expect_equal(paste(counts$CHROM, counts$POS, counts$REF, counts$ALT),
                 c("chr1 10 A C", "chr1 20 A G", "chr1 50 A C", "chr1 50 A T",
                   "chr1 60 A G", "chr1 60 A T", "chrX 40 G A"))
    expect_equal(counts$N_Cohort, c(3L, 3L, 3L, 3L, 3L, 1L, 3L))
  }

  # Without an index, the duplicated sites are re-read by streaming the VCF;
  # with one, by region lookup
  run_script()
  expect_equal(system2("bcftools", c("index", "-t", shQuote(vcf_gz))), 0L)
  run_script()
})

test_that("make_cohort_counts.sh fails cleanly on a missing input", {
  skip_on_cran()
  script <- system.file("scripts", "make_cohort_counts.sh", package = "puzzleapp")
  skip_if(!nzchar(script), "make_cohort_counts.sh not installed")
  out <- tempfile(fileext = ".tsv.gz")
  status <- suppressWarnings(system2("bash", shQuote(c(script, "/no/such.vcf.gz", out)),
                                     stdout = FALSE, stderr = FALSE))
  expect_false(status == 0L)
  expect_false(file.exists(out))
})
