# VCF processing helpers — mapping-driven, tolerant of missing fields.
# Canonical field mappings live in inst/extdata/preprocess/{snv,sv}_field_mapping.tsv.
# Users can supply optional override TSVs via snv_field_mapping / sv_field_mapping config keys.

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

read_vcf_long <- function(vcf_path) {
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

process_snv_vcf <- function(vcf_long) {
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
# process_snv_data
# -------------------------------------------------------------------------

#' Process SNV VCF to wide annotated table (v2 — mapping-driven)
#'
#' @param snvs_vcf          Path to gzipped SNV/indel VCF (.vcf.gz)
#' @param pedigree_data     data.table with columns sample_id, kinship
#' @param snvs_vcf_cohort   Optional cohort VCF (.vcf.gz) for N_Cohort (default NA)
#' @param snv_field_mapping Optional path to user SNV field-mapping override TSV
#' @return data.table
#' @export
process_snv_data <- function(snvs_vcf, pedigree_data,
                                 snvs_vcf_cohort   = NA,
                                 snv_field_mapping = NULL) {
  cat("Processing SNV VCF:", snvs_vcf, "\n")
  if (!file.exists(snvs_vcf)) stop("SNV VCF file not found: ", snvs_vcf)

  mapping <- load_field_mapping("snv", snv_field_mapping)

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
    snv_vcf_long <- read_vcf_long(snvs_vcf_cohort)
    final_snv    <- process_snv_vcf(snv_vcf_long)
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

  cat("SNV data processed.\n")
  return(unique(full))
}

# -------------------------------------------------------------------------
# process_sv_data
# -------------------------------------------------------------------------

#' Process SV VCF to wide annotated table (v2 — mapping-driven)
#'
#' @param svs_vcf          Path to gzipped SV VCF (.vcf.gz)
#' @param pedigree_data    data.table with columns sample_id, kinship
#' @param svs_vcf_cohort   Optional cohort VCF (currently unused; reserved)
#' @param svlog_static     Optional SVlog static TSV for annotation
#' @param svlog_db         Optional SVlog database TSV (reserved)
#' @param sv_field_mapping Optional path to user SV field-mapping override TSV
#' @return data.table
#' @export
process_sv_data <- function(svs_vcf, pedigree_data,
                                svs_vcf_cohort  = NA,
                                svlog_static    = NULL,
                                svlog_db        = NULL,
                                sv_field_mapping = NULL) {
  cat("Processing SV VCF:", svs_vcf, "\n")
  if (!file.exists(svs_vcf)) stop("SV VCF file not found: ", svs_vcf)

  mapping <- load_field_mapping("sv", sv_field_mapping)

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

  cat("SV data processed.\n")
  return(unique(full))
}

# -------------------------------------------------------------------------
# VCF reader + CSQ normaliser (shared by SNV and SV paths)
# -------------------------------------------------------------------------

#' Read VCF and normalise functional annotations to CSQ
#'
#' Returns data unchanged if CSQ is already present. Otherwise converts ANN
#' (SnpEff) or BCSQ (BCFtools) to a pseudo-CSQ field using the mapping tables
#' in inst/extdata/preprocess/.
#'
#' @param vcf_path Path to gzipped VCF (.vcf.gz)
#' @return list(vcf_data, vcf_header)
#' @keywords internal
read_and_normalise_vcf <- function(vcf_path) {
  if (!file.exists(vcf_path)) {
    stop("VCF file not found: ", vcf_path)
  }

  vcf_data <- data.table::fread(
    cmd    = paste("gunzip -c", vcf_path),
    sep    = "\t",
    skip   = "#CHROM",
    header = TRUE
  )

  vcf_header <- data.table::fread(
    cmd    = paste("zgrep '^##' ", vcf_path),
    sep    = "\n",
    header = FALSE
  )

  if (!"V1" %in% names(vcf_header)) {
    data.table::setnames(vcf_header, names(vcf_header), "V1")
  }

  has_csq  <- any(grepl("^##INFO=<ID=CSQ\\b",  vcf_header$V1))
  has_ann  <- any(grepl("^##INFO=<ID=ANN\\b",  vcf_header$V1))
  has_bcsq <- any(grepl("^##INFO=<ID=BCSQ\\b", vcf_header$V1))

  if (has_csq) {
    return(list(vcf_data = vcf_data, vcf_header = vcf_header))
  }

  header_file <- system.file("extdata", "preprocess", "CSQ_ANN_BSQ_header.txt", package = "puzzleapp")
  if (header_file == "") stop("Resource file 'CSQ_ANN_BSQ_header.txt' not found in extdata.")
  hdr_lines <- readLines(header_file, warn = FALSE)

  csq_header_line <- hdr_lines[grepl("^##INFO=<ID=CSQ\\b", hdr_lines)]
  if (!length(csq_header_line)) stop("Canonical CSQ header line not found in CSQ_ANN_BSQ_header.txt.")

  csq_format_str <- sub('.*Format: *([^"]*)".*', "\\1", csq_header_line)
  csq_columns    <- trimws(strsplit(csq_format_str, "\\|")[[1]])
  if (!length(csq_columns)) stop("Failed to parse canonical CSQ columns from CSQ_ANN_BSQ_header.txt.")

  mapping_file <- system.file("extdata", "preprocess", "CSQ_mapping.tsv", package = "puzzleapp")
  if (mapping_file == "") stop("Resource file 'CSQ_mapping.tsv' not found in extdata.")

  csq_map <- data.table::fread(mapping_file, sep = "\t", header = TRUE)
  required_map_cols <- c("VEP_CSQ_field", "SnpEff_ANN_field", "BCFtools_BCSQ_field")
  missing_map_cols  <- setdiff(required_map_cols, names(csq_map))
  if (length(missing_map_cols)) {
    stop("CSQ_mapping.tsv missing column(s): ", paste(missing_map_cols, collapse = ", "))
  }

  if (!"INFO" %in% names(vcf_data)) stop("VCF data does not contain an INFO column.")

  if (has_ann) {
    tag            <- "ANN"
    mapping_col    <- "SnpEff_ANN_field"
    tag_header_idx <- which(grepl("^##INFO=<ID=ANN\\b", vcf_header$V1))[1]
  } else if (has_bcsq) {
    tag            <- "BCSQ"
    mapping_col    <- "BCFtools_BCSQ_field"
    tag_header_idx <- which(grepl("^##INFO=<ID=BCSQ\\b", vcf_header$V1))[1]
  } else {
    stop("Neither CSQ, ANN nor BCSQ INFO header found in VCF: ", vcf_path)
  }

  tag_header_raw <- vcf_header$V1[tag_header_idx]

  if (tag == "ANN") {
    if (!grepl("Functional annotations:", tag_header_raw)) {
      stop("ANN INFO header does not contain 'Functional annotations:' text.")
    }
    tag_format_str <- sub(".*Functional annotations: *'([^']*)'.*", "\\1", tag_header_raw)
  } else {
    if (!grepl("Format:", tag_header_raw)) {
      stop("BCSQ INFO header does not contain 'Format:' text.")
    }
    tag_format_str <- sub('.*Format: *([^"]*)".*', "\\1", tag_header_raw)
  }
  tag_columns <- trimws(strsplit(tag_format_str, "\\|")[[1]])
  if (!length(tag_columns)) stop("Failed to parse ", tag, " columns from INFO header.")

  cat("Normalising ", tag, " to CSQ for VCF: ", vcf_path, "\n", sep = "")
  csq_vec <- build_csq_from_tag(
    info_vec    = vcf_data$INFO,
    tag         = tag,
    tag_columns = tag_columns,
    csq_columns = csq_columns,
    csq_map     = csq_map,
    mapping_col = mapping_col
  )
  cat("CSQ normalisation complete.\n")

  empty_info <- is.na(vcf_data$INFO) | vcf_data$INFO == ""
  vcf_data[empty_info & !is.na(csq_vec) & csq_vec != "",
           INFO := paste0("CSQ=", csq_vec[empty_info & !is.na(csq_vec) & csq_vec != ""])]
  vcf_data[!empty_info & !is.na(csq_vec) & csq_vec != "",
           INFO := paste0(INFO, ";CSQ=", csq_vec[!empty_info & !is.na(csq_vec) & csq_vec != ""])]

  cat("Ensuring CSQ INFO header is present in VCF header.\n")
  if (!any(grepl("^##INFO=<ID=CSQ\\b", vcf_header$V1))) {
    info_idx <- grep("^##INFO=", vcf_header$V1)
    if (length(info_idx)) {
      insert_pos <- max(info_idx) + 1L
      vcf_header <- data.table::rbindlist(list(
        vcf_header[seq_len(insert_pos - 1L)],
        data.table::data.table(V1 = csq_header_line),
        vcf_header[seq(insert_pos, nrow(vcf_header))]
      ), use.names = TRUE, fill = TRUE)
    } else {
      vcf_header <- data.table::rbindlist(
        list(vcf_header, data.table::data.table(V1 = csq_header_line)),
        use.names = TRUE, fill = TRUE
      )
    }
  }

  list(vcf_data = vcf_data, vcf_header = vcf_header)
}

# -------------------------------------------------------------------------
# Generic tag -> CSQ normaliser
# -------------------------------------------------------------------------

#' Build CSQ strings from an annotation tag (ANN or BCSQ)
#'
#' @param info_vec    Character vector of INFO column values.
#' @param tag         Tag name in INFO ("ANN" or "BCSQ").
#' @param tag_columns Character vector: field order for the tag (from header).
#' @param csq_columns Canonical CSQ schema (from CSQ header).
#' @param csq_map     Mapping table with VEP_CSQ_field + mapping cols.
#' @param mapping_col Column name in csq_map to use.
#' @return Character vector of CSQ strings, one per element of info_vec.
#' @keywords internal
build_csq_from_tag <- function(info_vec, tag, tag_columns,
                                csq_columns, csq_map, mapping_col) {
  tag_pattern <- paste0(tag, "=[^;]+")
  tag_raw <- stringr::str_extract(info_vec, tag_pattern)
  tag_val <- sub(paste0("^", tag, "="), "", tag_raw)
  tag_val[is.na(tag_raw)] <- NA_character_

  if (!mapping_col %in% names(csq_map)) {
    stop("Mapping column ", mapping_col, " not found in csq_map.")
  }
  map_subset <- csq_map[!is.na(get(mapping_col)) & get(mapping_col) != ""]

  build_one <- function(one_val) {
    if (is.na(one_val) || one_val == "") return(NA_character_)

    entry  <- strsplit(one_val, ",", fixed = TRUE)[[1]][1]
    fields <- strsplit(entry, "\\|")[[1]]
    if (length(fields) < length(tag_columns)) {
      fields <- c(fields, rep("", length(tag_columns) - length(fields)))
    }
    names(fields) <- tag_columns

    csq_vals <- character(length(csq_columns))
    for (j in seq_along(csq_columns)) {
      val     <- ""
      map_row <- map_subset[VEP_CSQ_field == csq_columns[j]]
      if (nrow(map_row) == 1L) {
        tag_name <- map_row[[mapping_col]]
        if (!is.null(tag_name) && !is.na(tag_name) &&
            nzchar(tag_name) && tag_name %in% names(fields)) {
          val <- fields[[tag_name]]
        }
      }
      csq_vals[j] <- ifelse(is.na(val), "", val)
    }
    paste(csq_vals, collapse = "|")
  }

  vapply(tag_val, build_one, character(1L))
}
