# VCF processing helpers v2 — mapping-driven, tolerant of missing fields.
# Canonical field mappings live in inst/extdata/preprocess/{snv,sv}_field_mapping.tsv.
# Users can supply an optional override TSV via the vcf_field_mapping config key.

# -------------------------------------------------------------------------
# Mapping loader
# -------------------------------------------------------------------------

#' Load and merge canonical + user field mappings
#'
#' @param variant_type "snv" or "sv"
#' @param user_mapping_path Optional path to a user override TSV (same schema).
#' @return data.table with columns:
#'   vcf_field, vcf_field_2, output_column, field_type, required, computation, note
#' @keywords internal
load_field_mapping <- function(variant_type, user_mapping_path = NULL) {
  fname <- paste0(variant_type, "_field_mapping.tsv")
  canonical_file <- system.file("extdata", "preprocess", fname, package = "puzzleapp")
  if (canonical_file == "") {
    stop("Canonical field mapping not found: ", fname)
  }

  mapping <- data.table::fread(canonical_file, sep = "\t", header = TRUE,
                                colClasses = "character", na.strings = "")

  if (!is.null(user_mapping_path) && !is.na(user_mapping_path) &&
      nzchar(user_mapping_path)) {
    if (!file.exists(user_mapping_path)) {
      warning("User field mapping file not found (ignored): ", user_mapping_path)
    } else {
      user_map <- data.table::fread(user_mapping_path, sep = "\t", header = TRUE,
                                     colClasses = "character", na.strings = "")
      # User rows override canonical rows matched on output_column
      mapping <- mapping[!output_column %in% user_map$output_column]
      mapping <- data.table::rbindlist(list(mapping, user_map),
                                        use.names = TRUE, fill = TRUE)
    }
  }

  mapping[is.na(computation), computation := "direct"]
  mapping[is.na(required),    required    := "FALSE"]
  mapping
}

# -------------------------------------------------------------------------
# CSQ extraction helper
# -------------------------------------------------------------------------

#' Extract CSQ-mapped columns from a data.table, with graceful NA fill.
#'
#' @param dt        data.table containing all CSQ subfields as columns.
#' @param mapping   Mapping data.table (from load_field_mapping), CSQ rows only.
#' @return Named list: list(cols = named character vector of old->new name pairs,
#'                          missing = character vector of output columns filled with NA)
#' @keywords internal
extract_csq_columns <- function(dt, mapping) {
  csq_rows <- mapping[field_type == "CSQ"]
  missing_out <- character(0)

  for (i in seq_len(nrow(csq_rows))) {
    vcf_col  <- csq_rows$vcf_field[i]
    out_col  <- csq_rows$output_column[i]
    is_req   <- isTRUE(as.logical(csq_rows$required[i]))

    if (!vcf_col %in% names(dt)) {
      if (is_req) {
        stop("Required CSQ field missing from VCF annotation: ", vcf_col)
      }
      warning("CSQ field '", vcf_col, "' not found; '", out_col, "' will be NA.")
      dt[, (out_col) := NA_character_]
      missing_out <- c(missing_out, out_col)
    } else if (vcf_col != out_col) {
      dt[, (out_col) := get(vcf_col)]
    }
  }

  invisible(missing_out)
}

# -------------------------------------------------------------------------
# FORMAT field resolver for SNV (GT/GQ/AD/DP)
# -------------------------------------------------------------------------

#' Resolve FORMAT fields for SNV from the mapping, return selected field names.
#' @keywords internal
snv_format_fields <- function(mapping) {
  fmt <- mapping[field_type == "FORMAT" & computation == "direct"]
  unique(fmt$vcf_field)
}

# -------------------------------------------------------------------------
# FORMAT field resolver for SV (GT/GQ/DV/DR)
# -------------------------------------------------------------------------

#' Resolve FORMAT fields for SV from the mapping, return selected field names.
#' @keywords internal
sv_format_fields <- function(mapping) {
  fmt <- mapping[field_type == "FORMAT_SV"]
  unique(na.omit(c(fmt$vcf_field, fmt$vcf_field_2)))
}

# -------------------------------------------------------------------------
# Internal helpers shared with v1 (re-exported here so v2 is self-contained)
# -------------------------------------------------------------------------

read_vcf_long_v2 <- function(vcf_path) {
  vcf <- data.table::fread(cmd = paste("zcat", vcf_path),
                           sep = "\t", skip = "#CHROM", header = TRUE)
  header <- data.table::fread(cmd = paste("zgrep ^#CHROM", vcf_path),
                              sep = "\t", header = FALSE)
  data.table::setnames(vcf, sub("^#", "", unlist(header[1, ])))

  fixed_cols <- c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT")
  sample_cols <- setdiff(names(vcf), fixed_cols)

  data.table::melt(vcf, id.vars = fixed_cols, measure.vars = sample_cols,
                   variable.name = "Sample", value.name = "Genotype")
}

process_snv_vcf_v2 <- function(vcf_long) {
  vcf_long <- vcf_long[!grepl("^\\./\\.", Genotype)]
  vcf_long[, VariantID := paste(CHROM, POS, REF, ALT, sep = "_")]
  vcf_long[, VariantIndex := seq_len(.N), by = VariantID]
  vcf_long[, Sample := sub("^[0-9]+_", "", Sample)]
  vcf_long[, GT := tstrsplit(Genotype, ":", fixed = TRUE)[[1]]]
  vcf_long[, AlleleCount := vapply(strsplit(GT, "[/|]"),
                                   function(x) sum(x != "." & x != "0"),
                                   integer(1))]
  final <- vcf_long[AlleleCount > 0,
                    .(CHROM, POS, VariantID, Sample, Genotype, GT, AlleleCount, VariantIndex)]
  merge(final, final[, .(N_Cohort = .N), by = VariantID], by = "VariantID", all.x = TRUE)
}

# -------------------------------------------------------------------------
# process_snv_data_v2
# -------------------------------------------------------------------------

#' Process SNV VCF to wide annotated table (v2 — mapping-driven)
#'
#' @param snvs_vcf          Path to gzipped SNV/indel VCF (.vcf.gz)
#' @param pedigree_data     data.table with columns sample_id, kinship
#' @param snvs_vcf_cohort   Optional cohort VCF (.vcf.gz) for N_Cohort (default NA)
#' @param vcf_field_mapping Optional path to user field-mapping override TSV
#' @return data.table
#' @export
process_snv_data_v2 <- function(snvs_vcf, pedigree_data,
                                 snvs_vcf_cohort   = NA,
                                 vcf_field_mapping = NULL) {
  cat("Processing SNV VCF (v2):", snvs_vcf, "\n")
  if (!file.exists(snvs_vcf)) stop("SNV VCF file not found: ", snvs_vcf)

  mapping <- load_field_mapping("snv", vcf_field_mapping)

  out        <- read_and_normalise_vcf(snvs_vcf)
  snvs_data  <- out$vcf_data
  vcf_header <- out$vcf_header

  csq_format <- vcf_header[grepl("##INFO=<ID=CSQ", V1)]
  if (!nrow(csq_format)) stop("CSQ format not found in VCF header.")
  csq_columns <- stringr::str_match(csq_format$V1, "Format: (.*)>")[, 2]
  csq_columns <- gsub('"$', '', csq_columns)
  csq_columns <- unlist(strsplit(csq_columns, "\\|"))
  if (!length(csq_columns)) stop("Failed to parse CSQ columns list.")

  snvs_data <- unique(snvs_data)

  # --- pedigree ordering ---------------------------------------------------
  trio <- pedigree_data$sample_id
  missing_samp <- setdiff(trio, names(snvs_data))
  if (length(missing_samp)) stop("Missing samples in VCF: ", paste(missing_samp, collapse = ", "))

  kinship_order <- c(proband = 1, mother = 2, father = 3)
  pedigree_data[, sort_order := kinship_order[kinship]]
  pedigree_data[is.na(sort_order), sort_order := 4]
  data.table::setorder(pedigree_data, sort_order)
  trio <- pedigree_data$sample_id

  data.table::setnames(snvs_data, names(snvs_data), gsub("#", "", names(snvs_data)))
  snvs_data[, VariantID := paste(CHROM, POS, REF, ALT, sep = "_")]
  snvs_data[, ID := paste0(CHROM, "_", POS, "_", .I)]

  # --- melt to long, expand FORMAT fields ----------------------------------
  id_vars <- setdiff(names(snvs_data), trio)
  snvs_data_melt <- data.table::melt(
    snvs_data, id.vars = id_vars, variable.name = "Sample", value.name = "Value"
  )
  if (!"FORMAT" %in% names(snvs_data_melt)) stop("FORMAT column missing.")

  snvs_data_melt[, num_fields := lengths(strsplit(FORMAT, ":"))]
  max_fields <- max(snvs_data_melt$num_fields)
  snvs_data_melt[, FORMAT := ifelse(max_fields - num_fields > 0,
                                    paste0(FORMAT, strrep(":NA", max_fields - num_fields)),
                                    FORMAT)]
  snvs_data_melt[, Value := ifelse(max_fields - num_fields > 0,
                                   paste0(Value, strrep(":NA", max_fields - num_fields)),
                                   Value)]

  format_split <- snvs_data_melt[, tstrsplit(FORMAT, ":", fixed = TRUE)]
  format_split <- data.table::melt(
    cbind(snvs_data_melt[, .(ID, Sample)], format_split),
    id.vars = c("ID", "Sample")
  )[, .(ID, Sample, FORMAT = value)]
  value_split <- snvs_data_melt[, tstrsplit(Value, ":", fixed = TRUE)]
  value_split <- data.table::melt(
    cbind(snvs_data_melt[, .(ID, Sample)], value_split),
    id.vars = c("ID", "Sample")
  )[, .(ID, Sample, Value = value)]

  snvs_data_melt <- merge(
    snvs_data_melt[, setdiff(names(snvs_data_melt), c("FORMAT", "Value")), with = FALSE],
    cbind(format_split, value_split[, .(Value)]),
    by = c("ID", "Sample")
  )
  snvs_data_melt <- snvs_data_melt[FORMAT != "NA"]
  snvs_data_melt[, num_fields := NULL]

  trio_dt <- data.table::data.table(Sample = trio, Code = seq_along(trio))
  snvs_data_melt <- merge(snvs_data_melt, trio_dt, by = "Sample")

  # --- FORMAT field selection driven by mapping ----------------------------
  fmt_direct <- mapping[field_type == "FORMAT" & computation == "direct", vcf_field]
  snvs_data_melt <- snvs_data_melt[FORMAT %in% fmt_direct]

  # AD: keep only the alt allele count (second element)
  if ("AD" %in% fmt_direct) {
    snvs_data_melt[FORMAT == "AD", Value := tstrsplit(Value, ",", fixed = TRUE)[2]]
  }

  snvs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]

  wide_formula <- paste(
    names(snvs_data_melt)[!names(snvs_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")],
    collapse = " + "
  )
  wide_formula <- sprintf("%s ~ FORMAT", wide_formula)
  snvs_data_melt <- data.table::dcast(snvs_data_melt, wide_formula, value.var = "Value")

  # --- VAF computation -----------------------------------------------------
  num_samples <- nrow(pedigree_data)
  ad_cols  <- paste0("AD_", seq_len(num_samples))
  dp_cols  <- paste0("DP_", seq_len(num_samples))
  vaf_cols <- paste0("VAF_", seq_len(num_samples))
  gt_cols  <- paste0("GT_", seq_len(num_samples))
  gq_cols  <- paste0("GQ_", seq_len(num_samples))

  vaf_row <- mapping[field_type == "FORMAT" & computation == "ratio"]
  if (nrow(vaf_row)) {
    for (i in seq_along(ad_cols)) {
      if (ad_cols[i] %in% names(snvs_data_melt) && dp_cols[i] %in% names(snvs_data_melt)) {
        snvs_data_melt[, (vaf_cols[i]) :=
          round(as.integer(get(ad_cols[i])) / as.integer(get(dp_cols[i])), 2)]
      } else {
        snvs_data_melt[, (vaf_cols[i]) := NA_real_]
      }
    }
  }

  # --- CSQ parsing ---------------------------------------------------------
  snvs_data_melt[, CSQ := ifelse(
    grepl("(?:^|;)CSQ=", INFO),
    sub(".*(?:^|;)CSQ=([^;]+).*", "\\1", INFO),
    NA_character_
  )]
  csq_split <- snvs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
  n_expected <- length(csq_columns)
  n_actual   <- if (!is.null(csq_split)) ncol(csq_split) else 0L
  if (n_actual == n_expected - 1L) {
    csq_split[, paste0("V", n_actual + 1L) := ""]
    n_actual <- ncol(csq_split)
  }
  if (n_actual != n_expected) {
    stop("CSQ column count mismatch. Expected ", n_expected, ", got ", n_actual, ".")
  }
  data.table::setnames(csq_split, csq_columns)
  snvs_data_melt <- cbind(snvs_data_melt, csq_split)

  # --- Apply CSQ field mapping (rename + graceful NA fill) -----------------
  extract_csq_columns(snvs_data_melt, mapping)

  # --- SpliceAI compound string --------------------------------------------
  sym_col  <- "_spliceai_symbol"
  has_spliceai <- all(c(sym_col, "Acceptor_Gain", "Acceptor_Loss",
                        "Donor_Gain", "Donor_Loss") %in% names(snvs_data_melt))
  if (has_spliceai) {
    dp_ag <- if ("_spliceai_dp_ag" %in% names(snvs_data_melt)) snvs_data_melt$`_spliceai_dp_ag` else ""
    dp_al <- if ("_spliceai_dp_al" %in% names(snvs_data_melt)) snvs_data_melt$`_spliceai_dp_al` else ""
    dp_dg <- if ("_spliceai_dp_dg" %in% names(snvs_data_melt)) snvs_data_melt$`_spliceai_dp_dg` else ""
    dp_dl <- if ("_spliceai_dp_dl" %in% names(snvs_data_melt)) snvs_data_melt$`_spliceai_dp_dl` else ""

    snvs_data_melt[, SpliceAI_pred := ifelse(
      get(sym_col) == "" | is.na(get(sym_col)),
      NA_character_,
      paste(ALT, get(sym_col),
            Acceptor_Gain, Acceptor_Loss, Donor_Gain, Donor_Loss,
            dp_ag, dp_al, dp_dg, dp_dl,
            sep = "|")
    )]
  } else {
    snvs_data_melt[, SpliceAI_pred := NA_character_]
    if (!all(c("Donor_Loss", "Donor_Gain", "Acceptor_Loss", "Acceptor_Gain") %in% names(snvs_data_melt))) {
      for (col in c("Donor_Loss", "Donor_Gain", "Acceptor_Loss", "Acceptor_Gain")) {
        if (!col %in% names(snvs_data_melt)) snvs_data_melt[, (col) := NA_real_]
      }
    }
  }

  # --- gnomAD / ClinVar links ----------------------------------------------
  snvs_data_melt[, GNOMADv4 := sprintf(
    "https://gnomad.broadinstitute.org/variant/%s-%s-%s-%s?dataset=gnomad_r4",
    CHROM, POS, REF, ALT
  )]

  if ("AF" %in% names(snvs_data_melt)) {
    snvs_data_melt[, AF := suppressWarnings(as.numeric(AF))]
    snvs_data_melt[AF > 0, gnomAD_ID :=
      paste0(stringr::str_remove(CHROM, "chr"), "-", POS, "-", REF, "-", ALT)]
    snvs_data_melt[AF > 0 & !is.na(gnomAD_ID), gnomAD_ID := sprintf(
      '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4" target="_blank">%s</a>',
      gnomAD_ID, gnomAD_ID
    )]
  } else {
    snvs_data_melt[, AF := NA_real_]
    snvs_data_melt[, gnomAD_ID := NA_character_]
  }

  if ("N_HOM_ALT" %in% names(snvs_data_melt)) {
    snvs_data_melt[, N_HOM_ALT := suppressWarnings(as.numeric(N_HOM_ALT))]
  } else {
    snvs_data_melt[, N_HOM_ALT := NA_real_]
  }

  if ("CLINVAR_ID" %in% names(snvs_data_melt)) {
    snvs_data_melt[!is.na(CLINVAR_ID) & CLINVAR_ID != "", CLINVAR_ID := sprintf(
      '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank">%s</a>',
      CLINVAR_ID, CLINVAR_ID
    )]
    snvs_data_melt[CLINVAR_ID == "", CLINVAR_ID := NA_character_]
  } else {
    snvs_data_melt[, CLINVAR_ID := NA_character_]
  }

  # --- Cohort counts -------------------------------------------------------
  if (is.na(snvs_vcf_cohort)) {
    snvs_data_melt[, N_Cohort := NA_integer_]
  } else {
    snv_vcf_long <- read_vcf_long_v2(snvs_vcf_cohort)
    final_snv    <- process_snv_vcf_v2(snv_vcf_long)
    cohort_counts <- unique(final_snv[, .(VariantID, N_Cohort)])
    snvs_data_melt <- merge(snvs_data_melt, cohort_counts, by = "VariantID", all.x = TRUE)
  }

  # --- Ensure all expected output columns exist ----------------------------
  required_csq_out <- c("GENE_ID", "GENE_SYMBOL", "TRANSCRIPT", "HGVSg", "HGVSc", "HGVSp",
                         "VEP_CONSEQUENCE", "CLINVAR", "SIFT", "PolyPhen", "REVEL",
                         "am_class", "am_pathogenicity", "CADD_PHRED", "CADD_RAW")
  for (col in required_csq_out) {
    if (!col %in% names(snvs_data_melt)) snvs_data_melt[, (col) := NA_character_]
  }

  # --- Build core output table ---------------------------------------------
  core <- snvs_data_melt[, .(
    ID,
    CATEGORY   = "SNV & Indel",
    VAR_TYPE   = ifelse(nchar(REF) == nchar(ALT), "SNV",
                        ifelse(nchar(REF) > nchar(ALT), "Deletion", "Insertion")),
    CHROM, POS,
    VAR_LENGTH = ifelse(nchar(REF) == nchar(ALT), 1L, abs(nchar(REF) - nchar(ALT))),
    REF, ALT, QUAL, FILTER,
    GENE_ID, GENE_SYMBOL, TRANSCRIPT,
    HGVSg, HGVSc, HGVSp,
    VEP_CONSEQUENCE,
    CLINVAR,
    CLINVAR_ID,
    gnomAD_ID,
    AF,
    N_HOM_ALT,
    N_Cohort,
    SIFT, PolyPhen, REVEL,
    am_class, am_pathogenicity,
    CADD_PHRED, CADD_RAW,
    SpliceAI_pred,
    Donor_Loss     = suppressWarnings(as.numeric(Donor_Loss)),
    Donor_Gain     = suppressWarnings(as.numeric(Donor_Gain)),
    Acceptor_Loss  = suppressWarnings(as.numeric(Acceptor_Loss)),
    Acceptor_Gain  = suppressWarnings(as.numeric(Acceptor_Gain))
  )]

  full <- cbind(core,
                snvs_data_melt[, c(ad_cols, dp_cols, vaf_cols, gt_cols, gq_cols), with = FALSE])

  full[, CLINVAR := stringr::str_replace_all(CLINVAR, "&", ",")]
  full[CLINVAR == "", CLINVAR := NA]

  for (col_name in grep("^GT_", names(full), value = TRUE)) {
    idx     <- tstrsplit(col_name, "_", fixed = TRUE)[[2]]
    new_col <- paste0("alt_allele_count_", idx)
    full[, (new_col) :=
      2L - (stringr::str_count(get(col_name), "0") +
            stringr::str_count(get(col_name), "\\."))
    ]
  }

  cat("SNV data processed (v2).\n")
  return(unique(full))
}

# -------------------------------------------------------------------------
# process_sv_data_v2
# -------------------------------------------------------------------------

#' Process SV VCF to wide annotated table (v2 — mapping-driven)
#'
#' @param svs_vcf           Path to gzipped SV VCF (.vcf.gz)
#' @param pedigree_data     data.table with columns sample_id, kinship
#' @param svs_vcf_cohort    Optional cohort VCF (currently unused; reserved)
#' @param svlog_static      Optional SVlog static TSV for annotation
#' @param svlog_db          Optional SVlog database TSV (reserved)
#' @param vcf_field_mapping Optional path to user field-mapping override TSV
#' @return data.table
#' @export
process_sv_data_v2 <- function(svs_vcf, pedigree_data,
                                svs_vcf_cohort   = NA,
                                svlog_static     = NULL,
                                svlog_db         = NULL,
                                vcf_field_mapping = NULL) {
  cat("Processing SV VCF (v2):", svs_vcf, "\n")
  if (!file.exists(svs_vcf)) stop("SV VCF file not found: ", svs_vcf)

  mapping <- load_field_mapping("sv", vcf_field_mapping)

  # --- SVlog ---------------------------------------------------------------
  svlog_dt <- NULL
  if (!is.null(svlog_static)) {
    if (!file.exists(svlog_static)) stop("SVlog file not found: ", svlog_static)
    cat("Using SVlog table:", svlog_static, "\n")
    svlog_dt <- data.table::fread(svlog_static, sep = "\t", header = TRUE)

    if ("variant_id" %in% names(svlog_dt)) {
      data.table::setnames(svlog_dt, "variant_id", "ID")
    }
    data.table::setnames(svlog_dt, names(svlog_dt), toupper(names(svlog_dt)))

    cols_to_drop <- intersect(c("CHROM", "START", "END"), names(svlog_dt))
    if (length(cols_to_drop)) svlog_dt[, (cols_to_drop) := NULL]

    if ("CONSEQUENCE" %in% names(svlog_dt)) {
      data.table::setnames(svlog_dt, "CONSEQUENCE", "SVLOG_CONSEQUENCE")
    }
  }

  # --- Read + normalise VCF ------------------------------------------------
  out        <- read_and_normalise_vcf(svs_vcf)
  svs_data   <- out$vcf_data
  vcf_header <- out$vcf_header

  svs_data[, ID := make.unique(as.character(ID), sep = ".")]

  csq_format <- vcf_header[grepl("##INFO=<ID=CSQ", V1)]
  if (!nrow(csq_format)) stop("CSQ format not found in VCF header.")
  csq_columns <- stringr::str_match(csq_format$V1, "Format: (.*)>")[, 2]
  csq_columns <- gsub('"$', '', csq_columns)
  csq_columns <- unlist(strsplit(csq_columns, "\\|"))
  if (!length(csq_columns)) stop("Failed to parse CSQ columns list.")

  svs_data <- unique(svs_data)

  # --- Pedigree ordering ---------------------------------------------------
  trio <- pedigree_data$sample_id
  missing_samp <- setdiff(trio, names(svs_data))
  if (length(missing_samp)) stop("Missing samples in VCF: ", paste(missing_samp, collapse = ", "))

  pedigree_data[, sort_order := c(proband = 1, mother = 2, father = 3)[kinship]]
  pedigree_data[is.na(sort_order), sort_order := 4]

  data.table::setnames(svs_data, names(svs_data), gsub("#", "", names(svs_data)))
  svs_data[, VariantID := ID]

  # --- Melt to long, expand FORMAT fields ----------------------------------
  id_vars <- setdiff(names(svs_data), trio)
  svs_data_melt <- data.table::melt(
    svs_data, id.vars = id_vars, variable.name = "Sample", value.name = "Value"
  )
  if (!"FORMAT" %in% names(svs_data_melt)) stop("FORMAT column missing.")

  svs_data_melt[, num_fields := lengths(strsplit(FORMAT, ":"))]
  max_fields <- max(svs_data_melt$num_fields)
  svs_data_melt[, FORMAT := ifelse(max_fields - num_fields > 0,
                                   paste0(FORMAT, strrep(":NA", max_fields - num_fields)),
                                   FORMAT)]
  svs_data_melt[, Value := ifelse(max_fields - num_fields > 0,
                                  paste0(Value, strrep(":NA", max_fields - num_fields)),
                                  Value)]

  format_split <- svs_data_melt[, tstrsplit(FORMAT, ":", fixed = TRUE)]
  format_split <- data.table::melt(
    cbind(svs_data_melt[, .(ID, Sample)], format_split),
    id.vars = c("ID", "Sample")
  )[, .(ID, Sample, FORMAT = value)]
  value_split <- svs_data_melt[, tstrsplit(Value, ":", fixed = TRUE)]
  value_split <- data.table::melt(
    cbind(svs_data_melt[, .(ID, Sample)], value_split),
    id.vars = c("ID", "Sample")
  )[, .(ID, Sample, Value = value)]

  svs_data_melt <- merge(
    svs_data_melt[, setdiff(names(svs_data_melt), c("FORMAT", "Value")), with = FALSE],
    cbind(format_split, value_split[, .(Value)]),
    by = c("ID", "Sample")
  )
  svs_data_melt <- svs_data_melt[FORMAT != "NA"]
  svs_data_melt[, num_fields := NULL]

  trio_dt <- data.table::data.table(Sample = trio, Code = seq_along(trio))
  svs_data_melt <- merge(svs_data_melt, trio_dt, by = "Sample")

  # --- FORMAT field selection driven by mapping ----------------------------
  sv_fmt_fields <- sv_format_fields(mapping)
  svs_data_melt <- svs_data_melt[FORMAT %in% sv_fmt_fields]
  svs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]

  wide_formula <- paste(
    names(svs_data_melt)[!names(svs_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")],
    collapse = " + "
  )
  wide_formula <- sprintf("%s ~ FORMAT", wide_formula)
  svs_data_melt <- data.table::dcast(svs_data_melt, wide_formula, value.var = "Value")

  # --- Computed per-sample columns from mapping ----------------------------
  num_samples <- nrow(pedigree_data)
  ad_cols  <- paste0("AD_", seq_len(num_samples))
  dp_cols  <- paste0("DP_", seq_len(num_samples))
  vaf_cols <- paste0("VAF_", seq_len(num_samples))
  gt_cols  <- paste0("GT_", seq_len(num_samples))
  gq_cols  <- paste0("GQ_", seq_len(num_samples))

  dv_row  <- mapping[field_type == "FORMAT_SV" & computation == "direct" &
                       output_column == "AD_n"]
  sum_row <- mapping[field_type == "FORMAT_SV" & computation == "sum"]
  rat_row <- mapping[field_type == "FORMAT_SV" & computation == "ratio"]

  dv_field <- if (nrow(dv_row))  paste0(dv_row$vcf_field[1],  "_") else "DV_"
  dr_field <- if (nrow(sum_row)) paste0(sum_row$vcf_field_2[1], "_") else "DR_"

  for (i in seq_len(num_samples)) {
    dv_col <- paste0(dv_field, i)
    dr_col <- paste0(dr_field, i)
    gq_col <- paste0("GQ_", i)

    if (dv_col %in% names(svs_data_melt)) {
      svs_data_melt[, (ad_cols[i]) := as.integer(get(dv_col))]
    } else {
      svs_data_melt[, (ad_cols[i]) := NA_integer_]
    }
    if (gq_col %in% names(svs_data_melt)) {
      svs_data_melt[, (gq_col) := as.integer(get(gq_col))]
    }
    if (dv_col %in% names(svs_data_melt) && dr_col %in% names(svs_data_melt)) {
      svs_data_melt[, (dp_cols[i]) :=
        as.integer(get(dv_col)) + as.integer(get(dr_col))]
      svs_data_melt[, (vaf_cols[i]) :=
        round(as.integer(get(dv_col)) /
              (as.integer(get(dv_col)) + as.integer(get(dr_col))), 2)]
    } else {
      svs_data_melt[, (dp_cols[i])  := NA_integer_]
      svs_data_melt[, (vaf_cols[i]) := NA_real_]
    }
  }

  # --- CSQ parsing ---------------------------------------------------------
  svs_data_melt[, CSQ := stringr::str_extract(INFO, "CSQ=[^;]+")]
  svs_data_melt[, CSQ := sub("^CSQ=", "", CSQ)]
  csq_split <- svs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
  if (ncol(csq_split) < length(csq_columns)) {
    missing_n <- length(csq_columns) - ncol(csq_split)
    for (k in seq_len(missing_n)) {
      csq_split[, paste0("V", ncol(csq_split) + 1) :=
        ifelse(is.na(V1), NA_character_, "")]
    }
  }
  data.table::setnames(csq_split, csq_columns)
  svs_data_melt <- cbind(svs_data_melt, csq_split)

  # --- Apply CSQ field mapping ---------------------------------------------
  extract_csq_columns(svs_data_melt, mapping)

  # --- INFO field extraction via mapping -----------------------------------
  info_rows <- mapping[field_type == "INFO"]
  for (i in seq_len(nrow(info_rows))) {
    tag      <- info_rows$vcf_field[i]
    out_col  <- info_rows$output_column[i]
    comp     <- info_rows$computation[i]
    pattern  <- paste0(".*", tag, "=([^;]*).*")

    raw_val <- ifelse(
      grepl(paste0("(^|;)", tag, "="), svs_data_melt$INFO),
      sub(pattern, "\\1", svs_data_melt$INFO),
      NA_character_
    )

    if (identical(comp, "abs")) {
      svs_data_melt[, (out_col) := suppressWarnings(abs(as.numeric(raw_val)))]
    } else {
      svs_data_melt[, (out_col) := raw_val]
    }
  }

  # Fallback for VAR_TYPE / VAR_LENGTH if INFO extraction gave nothing
  if (!"VAR_TYPE"   %in% names(svs_data_melt)) svs_data_melt[, VAR_TYPE   := NA_character_]
  if (!"VAR_LENGTH" %in% names(svs_data_melt)) svs_data_melt[, VAR_LENGTH := NA_real_]

  # --- SVscanner tags (generic INFO loop, unchanged from v1) ---------------
  svscanner_tags <- c(
    "RM_CLASSIFICATION", "RM_RECIPROCAL", "RM_TOTAL_SV_COVERAGE",
    "TRF_CLASSIFICATION", "TRF_SV_COVERAGE", "TRF_PERIOD_SIZE",
    "TRF_COPY_NUMBER", "TRF_TOTAL_SV_COVERAGE",
    "CONSENSUS_REPEAT", "FINAL_CLASSIFICATION"
  )
  for (tag in svscanner_tags) {
    if (!tag %in% names(svs_data_melt)) {
      svs_data_melt[, (tag) := ifelse(
        grepl(paste0("(^|;)", tag, "="), INFO),
        sub(paste0(".*", tag, "=([^;]*).*"), "\\1", INFO),
        NA_character_
      )]
    }
  }

  # --- gnomAD / ClinVar links ----------------------------------------------
  if ("AF" %in% names(svs_data_melt)) {
    svs_data_melt[, AF := suppressWarnings(as.numeric(AF))]
  } else {
    svs_data_melt[, AF := NA_real_]
  }

  if ("gnomAD_ID" %in% names(svs_data_melt)) {
    svs_data_melt[, gnomAD_ID := sub(".*?(DEL|DUP|INV|INS|CNV|TRA|BND)_", "\\1_", gnomAD_ID)]
    svs_data_melt[!is.na(AF) & AF > 0 & !is.na(gnomAD_ID), gnomAD_ID := sprintf(
      '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_sv_r4" target="_blank">%s</a>',
      gnomAD_ID, gnomAD_ID
    )]
  } else {
    svs_data_melt[, gnomAD_ID := NA_character_]
  }

  required_cols <- c("CLIN_SIG", "CLINVAR_ID", "gnomAD_sv_N_HOMALT")
  for (col in required_cols) if (!col %in% names(svs_data_melt)) svs_data_melt[[col]] <- NA

  if (!is.na(svs_vcf_cohort)) {
    cat("SV cohort VCF not used. TODO: decide\n")
  }

  # --- Ensure CSQ output columns exist -------------------------------------
  sv_csq_out <- c("GENE_ID", "GENE_SYMBOL", "TRANSCRIPT",
                  "HGVSg", "HGVSc", "HGVSp", "VEP_CONSEQUENCE")
  for (col in sv_csq_out) {
    if (!col %in% names(svs_data_melt)) svs_data_melt[, (col) := NA_character_]
  }

  # --- Build core output table ---------------------------------------------
  core <- svs_data_melt[, .(
    ID,
    CATEGORY   = "SV",
    VAR_TYPE,
    CHROM, POS,
    VAR_LENGTH = abs(suppressWarnings(as.numeric(VAR_LENGTH))),
    REF, ALT, QUAL, FILTER,
    GENE_ID, GENE_SYMBOL, TRANSCRIPT,
    HGVSg, HGVSc, HGVSp,
    VEP_CONSEQUENCE,
    RM_CLASSIFICATION, RM_RECIPROCAL, RM_TOTAL_SV_COVERAGE,
    TRF_CLASSIFICATION, TRF_SV_COVERAGE, TRF_PERIOD_SIZE,
    TRF_COPY_NUMBER, TRF_TOTAL_SV_COVERAGE,
    CONSENSUS_REPEAT, FINAL_CLASSIFICATION
  )]

  full <- cbind(core,
                svs_data_melt[, c(ad_cols, dp_cols, vaf_cols, gt_cols, gq_cols), with = FALSE])

  # --- SVlog merge ---------------------------------------------------------
  if (!is.null(svlog_dt)) {
    overlap <- setdiff(intersect(names(svlog_dt), names(full)), "ID")
    if (length(overlap)) {
      data.table::setnames(svlog_dt, old = overlap, new = paste0("SVLOG_", overlap))
    }
    full <- merge(full, svlog_dt, by = "ID", all = TRUE)
  }

  for (col_name in grep("^GT_", names(full), value = TRUE)) {
    idx     <- tstrsplit(col_name, "_", fixed = TRUE)[[2]]
    new_col <- paste0("alt_allele_count_", idx)
    full[, (new_col) :=
      2L - (stringr::str_count(get(col_name), "0") +
            stringr::str_count(get(col_name), "\\."))
    ]
  }

  cat("SV data processed (v2).\n")
  return(unique(full))
}
