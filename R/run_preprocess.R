# NOTE: Despite the .py extension this is R code. Rename to preprocess_vcf.R if appropriate.
#' Preprocess SNV/SV VCFs using a YAML config that already defines output TSV paths
#'
#' This function:
#'   1. Reads a pipeline YAML file.
#'   2. If paths$snvs_vcf and paths$snvs_tsv are present, runs SNV preprocessing and writes to paths$snvs_tsv.
#'   3. If paths$svs_vcf and paths$svs_tsv are present, runs SV preprocessing and writes to paths$svs_tsv.
#'   4. Optionally uses paths$snvs_vcf_cohort for cohort carrier counts (passed to process_snv_data).
#'   5. Does NOT modify or rewrite the YAML file.
#'
#' Expected YAML structure (minimum):
#' paths:
#'   snvs_vcf: /abs/path/to/input.snvs.vcf.gz            # optional (for SNV)
#'   snvs_tsv: /abs/path/to/output_snv.tsv               # required if snvs_vcf given
#'   snvs_vcf_cohort: /abs/path/to/cohort.snvs.vcf.gz    # optional (enables N_Cohort)
#'   svs_vcf:  /abs/path/to/input.svs.vcf.gz             # optional (for SV)
#'   svs_tsv:  /abs/path/to/output_sv.tsv                # required if svs_vcf given
#'   svlog_static: /abs/path/to/svlog_static.tsv         # optional (SVlog static)
#'   svlog_db:     /abs/path/to/svlog_db.tsv             # optional (SVlog database)
#' samples:
#'   - sample_id: SAMPLE1
#'     kinship: proband
#'   - sample_id: SAMPLE2
#'     kinship: mother
#'
#' @param config_yaml Path to the YAML configuration file.
#' @param validate Logical; if TRUE (default) perform strict key checks.
#' @param verbose Logical; print progress messages (default TRUE).
#' @param version Integer; 1 (default) uses process_snv/sv_data, 2 uses the
#'   mapping-driven process_snv/sv_data_v2. When version = 2, the optional
#'   paths$vcf_field_mapping key in the YAML is passed as a user override TSV.
#' @return Invisibly returns a list with elements:
#'   $snv_path (written SNV TSV or NULL),
#'   $sv_path (written SV TSV or NULL),
#'   $snv_dt (data.table or NULL),
#'   $sv_dt (data.table or NULL),
#'   $snvs_vcf_cohort (cohort VCF path or NA)
#' @export
run_preprocess <- function(config_yaml,
                       validate = TRUE,
                       verbose  = TRUE,
                       version  = 1L) {
  yaml_path <- config_yaml
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required.")
  }
  if (!file.exists(yaml_path)) {
    stop("YAML file not found: ", yaml_path)
  }
  cfg <- yaml::read_yaml(yaml_path)

  if (validate) {
    if (is.null(cfg$samples) || !length(cfg$samples)) {
      stop("YAML must contain a non-empty 'samples' list.")
    }
    if (is.null(cfg$paths)) {
      stop("YAML must contain a 'paths' block.")
    }
  }

  # Build pedigree (only sample_id + kinship needed)
  ped <- data.table::rbindlist(lapply(cfg$samples, data.table::as.data.table), fill = TRUE)
  if (!all(c("sample_id", "kinship") %in% names(ped))) {
    stop("Each sample must include 'sample_id' and 'kinship'.")
  }
  pedigree_data <- ped[, .(sample_id, kinship)]

  paths <- cfg$paths %||% list()
  if (verbose) {
    cat("Paths keys:\n")
    print(names(paths))
  }

  # Extract paths (scalar expected)
  snvs_vcf          <- paths$snvs_vcf          %||% NULL
  snvs_tsv          <- paths$snvs_tsv          %||% NULL
  snvs_vcf_cohort   <- paths$snvs_vcf_cohort   %||% NA
  svs_vcf           <- paths$svs_vcf           %||% NULL
  svs_tsv           <- paths$svs_tsv           %||% NULL
  svlog_static      <- paths$svlog_static      %||% NULL
  svlog_db          <- paths$svlog_db          %||% NULL
  vcf_field_mapping <- paths$vcf_field_mapping %||% NULL

  # Validation rules
  if (!is.null(snvs_vcf) && is.null(snvs_tsv)) {
    stop("paths$snvs_tsv must be specified when paths$snvs_vcf is present.")
  }
  if (!is.null(svs_vcf) && is.null(svs_tsv)) {
    stop("paths$svs_tsv must be specified when paths$svs_vcf is present.")
  }
  if (is.null(snvs_vcf) && is.null(svs_vcf)) {
    stop("At least one of paths$snvs_vcf or paths$svs_vcf must be defined.")
  }
  if (!is.na(snvs_vcf_cohort) && !file.exists(snvs_vcf_cohort)) {
    stop("snvs_vcf_cohort specified but file not found: ", snvs_vcf_cohort)
  }

  result <- list(
    snv_path = NULL,
    sv_path = NULL,
    snv_dt = NULL,
    sv_dt = NULL,
    snvs_vcf_cohort = snvs_vcf_cohort,
    svlog_static = svlog_static,
    svlog_db = svlog_db
  )

  # Ensure output directory for each TSV exists (if paths provided)
  ensure_parent_dir <- function(p) {
    dirp <- dirname(p)
    if (!dir.exists(dirp)) {
      dir.create(dirp, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # SNV preprocessing
  if (!is.null(snvs_vcf)) {
    if (!file.exists(snvs_vcf)) {
      stop("SNV VCF not found: ", snvs_vcf)
    }
    if (verbose) message("[preprocess] SNV: ", snvs_vcf, " -> ", snvs_tsv)
    if (verbose && !is.na(snvs_vcf_cohort)) {
      message("[preprocess] SNV cohort VCF: ", snvs_vcf_cohort)
    }
    ensure_parent_dir(snvs_tsv)
    snv_dt <- if (version == 2L) {
      process_snv_data_v2(
        snvs_vcf          = snvs_vcf,
        pedigree_data     = pedigree_data,
        snvs_vcf_cohort   = snvs_vcf_cohort,
        vcf_field_mapping = vcf_field_mapping
      )
    } else {
      process_snv_data(
        snvs_vcf        = snvs_vcf,
        pedigree_data   = pedigree_data,
        snvs_vcf_cohort = snvs_vcf_cohort
      )
    }

    # snv_dt <- snv_dt[1:10000, ]
    data.table::fwrite(snv_dt, snvs_tsv, sep = "\t", quote = FALSE, na = "NA")
    result$snv_path <- snvs_tsv
    result$snv_dt <- snv_dt
    if (verbose) message("[preprocess] Wrote SNV TSV: ", snvs_tsv)
  }

  # SV preprocessing (only if both VCF and TSV path provided)
  if (!is.null(svs_vcf)) {
    if (!file.exists(svs_vcf)) {
      stop("SV VCF not found: ", svs_vcf)
    }
    if (verbose) message("[preprocess] SV: ", svs_vcf, " -> ", svs_tsv)
    ensure_parent_dir(svs_tsv)
    sv_dt <- if (version == 2L) {
      process_sv_data_v2(
        svs_vcf           = svs_vcf,
        pedigree_data     = pedigree_data,
        svlog_static      = svlog_static,
        svlog_db          = svlog_db,
        vcf_field_mapping = vcf_field_mapping
      )
    } else {
      process_sv_data(
        svs_vcf       = svs_vcf,
        pedigree_data = pedigree_data,
        svlog_static  = svlog_static,
        svlog_db      = svlog_db
      )
    }
    data.table::fwrite(sv_dt, svs_tsv, sep = "\t", quote = FALSE, na = "NA")
    result$sv_path <- svs_tsv
    result$sv_dt <- sv_dt
    if (verbose) message("[preprocess] Wrote SV TSV: ", svs_tsv)
  }

  # generate coverage and vaf qc plots html paths
  generate_qc_htmls_from_config(yaml_path)

  invisible(result)
}

# Robust null-coalescing helper (vector-safe)
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 0) return(y)
  if (all(is.na(x))) return(y)
  if (is.character(x)) {
    tx <- trimws(x)
    if (all(!nzchar(tx))) return(y)
  }
  x
}