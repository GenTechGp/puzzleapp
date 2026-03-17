# Legacy VCF processing helpers (SNV + SV)
# Note: relies on data.table and stringr (declare in DESCRIPTION Imports).
# Uses shell tools zcat/gunzip/zgrep for speed.

# Internal: read gzipped VCF to long form (per-sample genotype)
read_vcf_long <- function(vcf_path) {
  vcf <- data.table::fread(cmd = paste("zcat", vcf_path),
                           sep = "\t", skip = "#CHROM", header = TRUE)
  header <- data.table::fread(cmd = paste("zgrep ^#CHROM", vcf_path),
                              sep = "\t", header = FALSE)
  data.table::setnames(vcf, sub("^#", "", unlist(header[1, ])))

  fixed_cols <- c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT")
  sample_cols <- setdiff(names(vcf), fixed_cols)

  data.table::melt(
    vcf,
    id.vars = fixed_cols,
    measure.vars = sample_cols,
    variable.name = "Sample",
    value.name = "Genotype"
  )
}

# Internal: cohort counting for SNVs
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

# Internal: cohort counting for SVs
process_sv_vcf <- function(vcf_long) {
  vcf_long <- vcf_long[Genotype != "./.:.:.:.:.:."]
  vcf_long[, VariantIndex := seq_len(.N), by = ID]
  vcf_long[, Sample := sub("^[0-9]+_", "", Sample)]
  vcf_long[, GT := tstrsplit(Genotype, ":", fixed = TRUE)[[1]]]
  vcf_long[, AlleleCount := vapply(strsplit(GT, "[/|]"),
                                   function(x) sum(x != "." & x != "0"),
                                   integer(1))]

  idlist_table <- unique(vcf_long[, .(ID, INFO)])
  idlist_table[, IDLIST := data.table::fifelse(grepl("IDLIST=", INFO),
                                               sub(".*IDLIST=([^;]+).*", "\\1", INFO),
                                               NA_character_)]
  idlist_table[, IDLIST_VEC := strsplit(IDLIST, ",")]
  idlist_long <- idlist_table[
    , .(VariantIndex = seq_along(IDLIST_VEC[[1]]),
        VariantID = IDLIST_VEC[[1]]),
    by = ID
  ]

  merged <- merge(
    vcf_long[, .(CHROM, POS, ID, Sample, Genotype, GT, AlleleCount, VariantIndex)],
    idlist_long, by = c("ID", "VariantIndex")
  )
  merged <- merged[AlleleCount > 0]
  merge(merged, merged[, .(N_Cohort = .N), by = ID], by = "ID", all.x = TRUE)
}

#' Process SNV VCF to wide annotated table (legacy)
#' @param snvs_vcf Path to gzipped SNV/indel VCF (.vcf.gz)
#' @param pedigree_data data.table with columns sample_id, kinship
#' @param snvs_vcf_cohort Optional cohort VCF (.vcf.gz) for N_Cohort (default NA)
#' @return data.table
#' @export
process_snv_data <- function(snvs_vcf, pedigree_data, snvs_vcf_cohort = NA) {
  cat("Processing SNV VCF:", snvs_vcf, "\n")
  if (!file.exists(snvs_vcf)) stop("SNV VCF file not found: ", snvs_vcf)

  # snvs_data <- data.table::fread(cmd = paste("gunzip -c", snvs_vcf),
  #                                sep = "\t", skip = "#CHROM", header = TRUE)
  # vcf_header <- data.table::fread(cmd = paste("zgrep '^##' ", snvs_vcf),
  #                                 sep = "\n", header = FALSE)

  out <- read_and_normalise_vcf(snvs_vcf)
  snvs_data <- out$vcf_data
  vcf_header <- out$vcf_header

  csq_format <- vcf_header[grepl("##INFO=<ID=CSQ", V1)]
  if (!nrow(csq_format)) stop("CSQ format not found in VCF header.")
  csq_columns <- stringr::str_match(csq_format$V1, "Format: (.*)>")[, 2]
  csq_columns <- gsub('"$', '', csq_columns)
  csq_columns <- unlist(strsplit(csq_columns, "\\|"))
  if (!length(csq_columns)) stop("Failed to parse CSQ columns list.")

  snvs_data <- unique(snvs_data)

  trio <- pedigree_data$sample_id
  missing <- setdiff(trio, names(snvs_data))
  if (length(missing)) stop("Missing samples in VCF: ", paste(missing, collapse = ", "))

  kinship_order <- c(proband = 1, mother = 2, father = 3)
  pedigree_data[, sort_order := kinship_order[kinship]]
  pedigree_data[is.na(sort_order), sort_order := 4]
  data.table::setorder(pedigree_data, sort_order)
  trio <- pedigree_data$sample_id

  data.table::setnames(snvs_data, names(snvs_data), gsub("#", "", names(snvs_data)))
  snvs_data[, VariantID := paste(CHROM, POS, REF, ALT, sep = "_")]
  snvs_data[, ID := paste0(CHROM, "_", POS, "_", .I)]

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

  snvs_data_melt <- snvs_data_melt[FORMAT %in% c("GT", "GQ", "AD", "DP")]
  snvs_data_melt[FORMAT == "AD", Value := tstrsplit(Value, ",", fixed = TRUE)[2]]
  snvs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]

  wide_formula <- paste(
    names(snvs_data_melt)[!names(snvs_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")],
    collapse = " + "
  )
  wide_formula <- sprintf("%s ~ FORMAT", wide_formula)
  snvs_data_melt <- data.table::dcast(snvs_data_melt, wide_formula, value.var = "Value")

  id_vars2 <- names(snvs_data_melt)
  ad_vars <- grep("^AD_", id_vars2, value = TRUE)
  dp_vars <- grep("^DP_", id_vars2, value = TRUE)
  gq_vars <- grep("^GQ_", id_vars2, value = TRUE)
  vaf_vars <- sub("^AD_", "VAF_", ad_vars)

  for (i in seq_along(ad_vars)) {
    snvs_data_melt[, (vaf_vars[i]) :=
      round(as.integer(get(ad_vars[i])) / as.integer(get(dp_vars[i])), 2)]
  }

  # snvs_data_melt[, CSQ := stringr::str_extract(INFO, "CSQ=[^;]+")]
  # snvs_data_melt[, CSQ := sub("^CSQ=", "", CSQ)]
  snvs_data_melt[, CSQ := ifelse(
    grepl("(?:^|;)CSQ=", INFO),
    sub(".*(?:^|;)CSQ=([^;]+).*", "\\1", INFO),
    NA_character_
  )]
  csq_split <- snvs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
  n_expected <- length(csq_columns)
  n_actual   <- if (!is.null(csq_split)) ncol(csq_split) else 0L
  if (n_actual == n_expected - 1L) {
    # Special case: exactly one column short → treat as missing final field
    # (e.g. last VEP field empty, "…||" case)
    csq_split[, paste0("V", n_actual + 1L) := ""]
    n_actual <- ncol(csq_split)
  }
  if (n_actual != n_expected) {
    stop(
      "CSQ column count mismatch after parsing. ",
      "Expected ", n_expected, " fields per CSQ entry (from header), got ", n_actual, "."
    )
  }
  data.table::setnames(csq_split, csq_columns)
  snvs_data_melt <- cbind(snvs_data_melt, csq_split)
  

  snvs_data_melt[, SpliceAI_pred := paste(
    SpliceAI_pred_SYMBOL, SpliceAI_pred_DS_DL, SpliceAI_pred_DS_DG,
    SpliceAI_pred_DS_AL, SpliceAI_pred_DS_AG
  )]

  snvs_data_melt[, SpliceAI_pred := ifelse(
    SpliceAI_pred_SYMBOL == "",
    NA,
    paste(
      ALT,
      SpliceAI_pred_SYMBOL,
      SpliceAI_pred_DS_AG, SpliceAI_pred_DS_AL,
      SpliceAI_pred_DS_DG, SpliceAI_pred_DS_DL,
      SpliceAI_pred_DP_AG, SpliceAI_pred_DP_AL,
      SpliceAI_pred_DP_DG, SpliceAI_pred_DP_DL,
      sep = "|"
    )
  )]

  snvs_data_melt[, GNOMADv4 := sprintf(
    "https://gnomad.broadinstitute.org/variant/%s-%s-%s-%s?dataset=gnomad_r4", CHROM, POS, REF, ALT
  )]

  num_samples <- nrow(pedigree_data)
  ad_cols <- paste0("AD_", seq_len(num_samples))
  dp_cols <- paste0("DP_", seq_len(num_samples))
  vaf_cols <- paste0("VAF_", seq_len(num_samples))
  gt_cols <- paste0("GT_", seq_len(num_samples))
  gq_cols <- paste0("GQ_", seq_len(num_samples))

  snvs_data_melt[, AF := suppressWarnings(as.numeric(gnomAD_AF_joint))]
  snvs_data_melt[AF > 0, gnomAD_ID :=
    paste0(stringr::str_remove(CHROM, "chr"), "-", POS, "-", REF, "-", ALT)]
  snvs_data_melt[AF > 0 & !is.na(gnomAD_ID), gnomAD_ID := sprintf(
    '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4" target="_blank">%s</a>',
    gnomAD_ID, gnomAD_ID
  )]

  snvs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
    '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank">%s</a>',
    ClinVar, ClinVar
  )]

  if (is.na(snvs_vcf_cohort)) {
    snvs_data_melt[, N_Cohort := NA_integer_]
  } else {
    snv_vcf_long <- read_vcf_long(snvs_vcf_cohort)
    final_snv <- process_snv_vcf(snv_vcf_long)
    final_snv[, fam := sub("-RF.*$", "", Sample)]
    pedigree_data[, fam := sub("-RF.*$", "", sample_id)]
    final_snv <- merge(
      final_snv[, .(VariantID, fam, N_Cohort)],
      pedigree_data[, .(fam, kinship, sort_order)],
      by = "fam"
    )[, fam := NULL]
    snvs_data_melt <- merge(
      snvs_data_melt, final_snv[, .(VariantID, N_Cohort)],
      by = "VariantID", all.x = TRUE
    )
  }

  # ADD back legacy columns: CADD_PHRED, CADD_RAW and SpliceAI component scores
  core <- snvs_data_melt[, .(
    ID,
    CATEGORY = "SNV & Indel",
    VAR_TYPE = ifelse(nchar(REF) == nchar(ALT), "SNV",
                      ifelse(nchar(REF) > nchar(ALT), "Deletion", "Insertion")),
    CHROM, POS,
    VAR_LENGTH = ifelse(nchar(REF) == nchar(ALT), 1, abs(nchar(REF) - nchar(ALT))),
    REF, ALT, QUAL, FILTER,
    GENE_ID = Gene,
    GENE_SYMBOL = SYMBOL,
    TRANSCRIPT = Feature,
    HGVSg, HGVSc, HGVSp,
    VEP_CONSEQUENCE = Consequence,
    CLINVAR = CLIN_SIG,
    CLINVAR_ID,
    gnomAD_ID,
    AF,
    N_HOM_ALT = suppressWarnings(as.numeric(gnomAD_nhomalt_joint)),
    N_Cohort,
    SIFT, PolyPhen, REVEL,
    am_class, am_pathogenicity,
    CADD_PHRED, CADD_RAW,
    SpliceAI_pred,
    Donor_Loss = suppressWarnings(as.numeric(SpliceAI_pred_DS_DL)),
    Donor_Gain = suppressWarnings(as.numeric(SpliceAI_pred_DS_DG)),
    Acceptor_Loss = suppressWarnings(as.numeric(SpliceAI_pred_DS_AL)),
    Acceptor_Gain = suppressWarnings(as.numeric(SpliceAI_pred_DS_AG))
  )]

  full <- cbind(core, snvs_data_melt[, c(ad_cols, dp_cols, vaf_cols, gt_cols, gq_cols), with = FALSE])

  full[, CLINVAR := stringr::str_replace_all(CLINVAR, "&", ",")]
  full[CLINVAR == "", CLINVAR := NA]

  # compute alt allele count directly from each GT_* column
  for (col_name in grep("^GT_", names(full), value = TRUE)) {
    idx <- tstrsplit(col_name, "_", fixed = TRUE)[[2]]
    new_col <- paste0("alt_allele_count_", idx)
    full[, (new_col) :=
      2L - (stringr::str_count(get(col_name), "0") +
            stringr::str_count(get(col_name), "\\."))
    ]
  }

  cat("SNV data processed.\n")
  return(unique(full))
}

#' Process SV VCF to wide annotated table (legacy)
#' @param svs_vcf Path to gzipped SV VCF (.vcf.gz)
#' @param pedigree_data data.table with columns sample_id, kinship
#' @param svs_vcf_cohort Optional cohort VCF (.vcf.gz) for N_Cohort (default NA)
#' @param svlog_static Optional SVlog static TSV file for annotation (default NULL)
#' @param svlog_db Optional SVlog database TSV file for annotation (default NULL)
#' @return data.table
#' @export
process_sv_data <- function(svs_vcf, pedigree_data, 
                            svs_vcf_cohort = NA, svlog_static = NULL, svlog_db = NULL) {
  cat("Processing SV VCF:", svs_vcf, "\n")
  if (!file.exists(svs_vcf)) stop("SV VCF file not found: ", svs_vcf)
  svlog_dt <- NULL
  if (!is.null(svlog_static)) {
    if (!file.exists(svlog_static)) {
      stop("SVlog file not found: ", svlog_static)
    }
    cat("Using SVlog table:", svlog_static, "\n")
    svlog_dt <- data.table::fread(svlog_static, sep = "\t", header = TRUE)
    
    # 1) Rename variant_id -> ID (if present)
    if ("variant_id" %in% names(svlog_dt)) {
      data.table::setnames(svlog_dt, "variant_id", "ID")
    }
    
    # 2) Capitalise all column names
    data.table::setnames(
      svlog_dt,
      old = names(svlog_dt),
      new = toupper(names(svlog_dt))
    )
    
    # 3) Drop CHROM / START / END if present
    cols_to_drop <- intersect(c("CHROM", "START", "END"), names(svlog_dt))
    if (length(cols_to_drop)) {
      svlog_dt[, (cols_to_drop) := NULL]
    }
    
    # 4) Rename CONSEQUENCE -> SVLOG_CONSEQUENCE if present
    if ("CONSEQUENCE" %in% names(svlog_dt)) {
      data.table::setnames(svlog_dt, "CONSEQUENCE", "SVLOG_CONSEQUENCE")
    }
  }
  # svs_data <- data.table::fread(cmd = paste("gunzip -c", svs_vcf),
  #                               sep = "\t", skip = "#CHROM", header = TRUE)
  # vcf_header <- data.table::fread(cmd = paste("zgrep '^##' ", svs_vcf),
  #                                 sep = "\n", header = FALSE)

  out <- read_and_normalise_vcf(svs_vcf) # CSQ header is different for SV vcfs. will that be a problem? todo:
  svs_data <- out$vcf_data
  vcf_header <- out$vcf_header

  svs_data[, ID := make.unique(as.character(ID), sep = ".")]
  csq_format <- vcf_header[grepl("##INFO=<ID=CSQ", V1)]
  if (!nrow(csq_format)) stop("CSQ format not found in VCF header.")
  csq_columns <- stringr::str_match(csq_format$V1, "Format: (.*)>")[, 2]
  csq_columns <- gsub('"$', '', csq_columns)
  csq_columns <- unlist(strsplit(csq_columns, "\\|"))
  if (!length(csq_columns)) stop("Failed to parse CSQ columns list.")

  svs_data <- unique(svs_data)

  trio <- pedigree_data$sample_id
  missing <- setdiff(trio, names(svs_data))
  if (length(missing)) stop("Missing samples in VCF: ", paste(missing, collapse = ", "))

  pedigree_data[, sort_order := c(proband = 1, mother = 2, father = 3)[kinship]]
  pedigree_data[is.na(sort_order), sort_order := 4]

  data.table::setnames(svs_data, names(svs_data), gsub("#", "", names(svs_data)))
  svs_data[, VariantID := ID]
  #svs_data[, ID := paste0(CHROM, "_", POS, "_", .I)]

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

  svs_data_melt <- svs_data_melt[FORMAT %in% c("GT", "GQ", "DR", "DV")]
  svs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]

  wide_formula <- paste(
    names(svs_data_melt)[!names(svs_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")],
    collapse = " + "
  )
  wide_formula <- sprintf("%s ~ FORMAT", wide_formula)
  svs_data_melt <- data.table::dcast(svs_data_melt, wide_formula, value.var = "Value")

  id_vars2 <- names(svs_data_melt)
  dv_vars <- grep("^DV_", id_vars2, value = TRUE)
  dr_vars <- grep("^DR_", id_vars2, value = TRUE)
  gq_vars <- grep("^GQ_", id_vars2, value = TRUE)
  ad_vars <- sub("^DR_", "AD_", dr_vars)
  dp_vars <- sub("^DR_", "DP_", dr_vars)
  vaf_vars <- sub("^AD_", "VAF_", ad_vars)

  for (i in seq_along(ad_vars)) {
    svs_data_melt[, (ad_vars[i]) := as.integer(get(dv_vars[i]))]
    svs_data_melt[, (gq_vars[i]) := as.integer(get(gq_vars[i]))]
    svs_data_melt[, (dp_vars[i]) := as.integer(get(dv_vars[i])) + as.integer(get(dr_vars[i]))]
    svs_data_melt[, (vaf_vars[i]) :=
      round(as.integer(get(dv_vars[i])) /
              (as.integer(get(ad_vars[i])) + as.integer(get(dr_vars[i]))), 2)]
  }

  svs_data_melt[, CSQ := stringr::str_extract(INFO, "CSQ=[^;]+")]
  svs_data_melt[, CSQ := sub("^CSQ=", "", CSQ)]
  csq_split <- svs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
  if (ncol(csq_split) < length(csq_columns)) {
    missing <- length(csq_columns) - ncol(csq_split)
    for (i in seq_len(missing)) {
      csq_split[, paste0("V", ncol(csq_split) + 1) := ifelse(is.na(V1), NA_character_, "")]
    }
  }
  data.table::setnames(csq_split, csq_columns)
  svs_data_melt <- cbind(svs_data_melt, csq_split)

  svs_data_melt[, SpliceAI_pred := NA_character_]
  svs_data_melt[, SVTYPE := sub(".*SVTYPE=([^;]*);.*", "\\1", INFO)]
  svs_data_melt[, SVLEN := suppressWarnings(as.numeric(sub(".*SVLEN=([^;]*);.*", "\\1", INFO)))]
  
  svscanner_tags <- c(
    "RM_CLASSIFICATION", "RM_RECIPROCAL", "RM_TOTAL_SV_COVERAGE",
    "TRF_CLASSIFICATION", "TRF_SV_COVERAGE", "TRF_PERIOD_SIZE",
    "TRF_COPY_NUMBER", "TRF_TOTAL_SV_COVERAGE",
    "CONSENSUS_REPEAT", "FINAL_CLASSIFICATION"
  )
  
  for (tag in svscanner_tags) {
    colname <- tag
    if (!colname %in% names(svs_data_melt)) {
      svs_data_melt[, (colname) := ifelse(
        grepl(paste0("(^|;)", tag, "="), INFO),
        sub(paste0(".*", tag, "=([^;]*).*"), "\\1", INFO),
        NA_character_
      )]
    }
  }

  num_samples <- nrow(pedigree_data)
  ad_cols <- paste0("AD_", seq_len(num_samples))
  dp_cols <- paste0("DP_", seq_len(num_samples))
  vaf_cols <- paste0("VAF_", seq_len(num_samples))
  gt_cols <- paste0("GT_", seq_len(num_samples))
  gq_cols <- paste0("GQ_", seq_len(num_samples))

  if ("gnomAD_sv_AF" %in% names(svs_data_melt)) {
    svs_data_melt[, AF := suppressWarnings(as.numeric(gnomAD_sv_AF))]
  } else {
    svs_data_melt[, AF := NA_real_]
  }
  if ("gnomAD_sv" %in% names(svs_data_melt)) {
    svs_data_melt[, gnomAD_ID := sub(".*?(DEL|DUP|INV|INS|CNV|TRA|BND)_", "\\1_", gnomAD_sv)]
    svs_data_melt[AF > 0 & !is.na(gnomAD_ID), gnomAD_ID := sprintf(
      '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_sv_r4" target="_blank">%s</a>',
      gnomAD_ID, gnomAD_ID
    )]
  } else {
    svs_data_melt[, gnomAD_ID := NA_character_]
  }
  if ("ClinVar" %in% names(svs_data_melt)) {
    svs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
      '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank">%s</a>',
      ClinVar, ClinVar
    )]
  } else {
    svs_data_melt[, CLINVAR_ID := NA_character_]
  }

  if (is.na(svs_vcf_cohort)) {
    svs_data_melt[, N_Cohort := NA_integer_]
  } else {
    sv_vcf_long <- read_vcf_long(svs_vcf_cohort)
    final_sv <- process_sv_vcf(sv_vcf_long)
    final_sv[, fam := sub("-RF.*$", "", Sample)]
    pedigree_data[, fam := sub("-RF.*$", "", sample_id)]
    final_sv <- merge(
      final_sv[, .(VariantID, fam, N_Cohort)],
      pedigree_data[, .(fam, kinship, sort_order)],
      by = "fam"
    )[, fam := NULL]
    svs_data_melt <- merge(
      svs_data_melt, final_sv[, .(VariantID, N_Cohort)],
      by = "VariantID", all.x = TRUE
    )
  }

  required_cols <- c("CLIN_SIG", "CLINVAR_ID", "gnomAD_ID", "gnomAD_sv_N_HOMALT")
  for (col in required_cols) if (!col %in% names(svs_data_melt)) svs_data_melt[[col]] <- NA

  # ADD legacy columns (CADD + SpliceAI component placeholders) to SV core
  # core: keep your usual columns + SVscanner tags
  core <- svs_data_melt[, .(
    ID,
    CATEGORY = "SV",
    VAR_TYPE = SVTYPE,
    CHROM, POS,
    VAR_LENGTH = abs(SVLEN),
    REF, ALT, QUAL, FILTER,
    GENE_ID = Gene,
    GENE_SYMBOL = SYMBOL,
    TRANSCRIPT = Feature,
    HGVSg, HGVSc, HGVSp,
    VEP_CONSEQUENCE = Consequence,
    RM_CLASSIFICATION,
    RM_RECIPROCAL,
    RM_TOTAL_SV_COVERAGE,
    TRF_CLASSIFICATION,
    TRF_SV_COVERAGE,
    TRF_PERIOD_SIZE,
    TRF_COPY_NUMBER,
    TRF_TOTAL_SV_COVERAGE,
    CONSENSUS_REPEAT,
    FINAL_CLASSIFICATION
  )]

  full <- cbind(core, svs_data_melt[, c(ad_cols, dp_cols, vaf_cols, gt_cols, gq_cols), with = FALSE])
  
  # If SVlog table is present, merge it in by ID
  if (!is.null(svlog_dt)) {
    # Avoid overwriting shared columns apart from ID
    overlap <- intersect(names(svlog_dt), names(full))
    overlap <- setdiff(overlap, "ID")
    if (length(overlap)) {
      data.table::setnames(
        svlog_dt,
        old = overlap,
        new = paste0("SVLOG_", overlap)
      )
    }
    
    full <- merge(full, svlog_dt, by = "ID", all = TRUE)
  }

  for (col_name in grep("^GT_", names(full), value = TRUE)) {
    idx <- tstrsplit(col_name, "_", fixed = TRUE)[[2]]
    new_col <- paste0("alt_allele_count_", idx)
    full[, (new_col) :=
      2L - (stringr::str_count(get(col_name), "0") +
            stringr::str_count(get(col_name), "\\."))
    ]
  }

  cat("SV data processed.\n")
  return(unique(full))
}


#' Read VCF and normalise functional annotations to CSQ
#'
#' This helper reads a gzipped VCF and:
#' - If a CSQ INFO header is present, returns the data and header unchanged.
#' - Else, if an ANN INFO header is present, it builds a pseudo-CSQ INFO field
#'   from ANN using CSQ_mapping.tsv.
#' - Else, if a BCSQ INFO header is present, it builds a pseudo-CSQ INFO field
#'   from BCSQ using CSQ_mapping.tsv.
#' - Else, errors.
#'
#' It assumes:
#' - A resource file "CSQ_ANN_BSQ_header.txt" containing at least the canonical
#'   CSQ INFO header line (used to derive csq_columns).
#' - A resource file "CSQ_mapping.tsv" with columns:
#'     VEP_CSQ_field, SnpEff_ANN_field, BCFtools_BCSQ_field
#'
#' @param vcf_path Path to gzipped VCF (.vcf.gz)
#' @return list with elements:
#'   - vcf_data   : data.table of VCF body (variants)
#'   - vcf_header  : data.table of header lines (one per row, col V1)
#' @keywords internal
read_and_normalise_vcf <- function(vcf_path) {
  if (!file.exists(vcf_path)) {
    stop("VCF file not found: ", vcf_path)
  }

  # 1) Read VCF body and header as in process_snv_data()
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

  # Ensure header column name is V1
  if (!"V1" %in% names(vcf_header)) {
    data.table::setnames(vcf_header, names(vcf_header), "V1")
  }

  # 2) If CSQ already present, just return as-is
  has_csq  <- any(grepl("^##INFO=<ID=CSQ\\b",  vcf_header$V1))
  has_ann  <- any(grepl("^##INFO=<ID=ANN\\b",  vcf_header$V1))
  has_bcsq <- any(grepl("^##INFO=<ID=BCSQ\\b", vcf_header$V1))

  if (has_csq) {
    return(list(vcf_data = vcf_data, vcf_header = vcf_header))
  }

  # 3) Load canonical CSQ header + mapping table once
  header_file <- system.file("extdata", "preprocess", "CSQ_ANN_BSQ_header.txt", package = "puzzleapp")
  if (header_file == "") {
    stop("Resource file 'CSQ_ANN_BSQ_header.txt' not found in extdata.")
  }
  hdr_lines <- readLines(header_file, warn = FALSE)

  csq_header_line <- hdr_lines[grepl("^##INFO=<ID=CSQ\\b", hdr_lines)]
  if (!length(csq_header_line)) {
    stop("Canonical CSQ header line not found in CSQ_ANN_BSQ_header.txt.")
  }

  # Parse canonical CSQ column order from the header line
  csq_format_str <- sub('.*Format: *([^"]*)".*', "\\1", csq_header_line)
  csq_columns    <- strsplit(csq_format_str, "\\|")[[1]]
  csq_columns    <- trimws(csq_columns)
  if (!length(csq_columns)) {
    stop("Failed to parse canonical CSQ columns from CSQ_ANN_BSQ_header.txt.")
  }

  mapping_file <- system.file("extdata", "preprocess", "CSQ_mapping.tsv", package = "puzzleapp")
  if (mapping_file == "") {
    stop("Resource file 'CSQ_mapping.tsv' not found in extdata.")
  }

  csq_map <- data.table::fread(mapping_file, sep = "\t", header = TRUE)
  required_cols <- c("VEP_CSQ_field", "SnpEff_ANN_field", "BCFtools_BCSQ_field")
  missing_cols  <- setdiff(required_cols, names(csq_map))
  if (length(missing_cols)) {
    stop("CSQ_mapping.tsv is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }

  if (!"INFO" %in% names(vcf_data)) {
    stop("VCF data does not contain an INFO column.")
  }

  # 4) Decide which tag to normalise (ANN or BCSQ)
  if (has_ann) {
    tag         <- "ANN"
    mapping_col <- "SnpEff_ANN_field"
    tag_header_idx <- which(grepl("^##INFO=<ID=ANN\\b", vcf_header$V1))[1]
  } else if (has_bcsq) {
    tag         <- "BCSQ"
    mapping_col <- "BCFtools_BCSQ_field"
    tag_header_idx <- which(grepl("^##INFO=<ID=BCSQ\\b", vcf_header$V1))[1]
  } else {
    stop("Neither CSQ, ANN nor BCSQ INFO header found in VCF: ", vcf_path)
  }

  tag_header_raw <- vcf_header$V1[tag_header_idx]
  tag_columns <- NULL
  # 5) Parse tag-specific column order from its header using explicit patterns
  if (tag == "ANN") {
    # SnpEff ANN: "Functional annotations: 'Allele | Annotation | ...'"
    if (!grepl("Functional annotations:", tag_header_raw)) {
      stop("ANN INFO header does not contain 'Functional annotations:' text.")
    }
    tag_format_str <- sub(".*Functional annotations: *'([^']*)'.*", "\\1", tag_header_raw)
    tag_columns <- strsplit(tag_format_str, "\\|")[[1]]
    tag_columns <- trimws(tag_columns)
  } else if (tag == "BCSQ") {
    # BCFtools/csq BCSQ: "... Format: Consequence|gene|transcript|biotype|strand|amino_acid_change|dna_change"
    if (!grepl("Format:", tag_header_raw)) {
      stop("BCSQ INFO header does not contain 'Format:' text.")
    }
    tag_format_str <- sub('.*Format: *([^"]*)".*', "\\1", tag_header_raw)
    tag_columns <- strsplit(tag_format_str, "\\|")[[1]]
    tag_columns <- trimws(tag_columns)
  } else {
    stop("Unsupported tag '", tag, "' in read_and_normalise_vcf (expected ANN or BCSQ).")
  }

  if (!length(tag_columns)) {
    stop("Failed to parse ", tag, " columns from INFO header.")
  }

  # 6) Build CSQ strings from the chosen tag, using generic normaliser
  cat("Normalising ", tag, " to CSQ for VCF: ", vcf_path, "\n", sep = "")
  csq_vec <- .build_csq_from_tag(
    info_vec    = vcf_data$INFO,
    tag         = tag,
    tag_columns = tag_columns,
    csq_columns = csq_columns,
    csq_map     = csq_map,
    mapping_col = mapping_col
  )
  cat("CSQ normalisation complete.\n")

  # 7) Inject CSQ into INFO
  empty_info <- is.na(vcf_data$INFO) | vcf_data$INFO == ""
  vcf_data[empty_info, INFO := ifelse(
    is.na(csq_vec[empty_info]) | csq_vec[empty_info] == "",
    INFO,
    paste0("CSQ=", csq_vec[empty_info])
  )]

  vcf_data[!empty_info & !is.na(csq_vec) & csq_vec != "",
            INFO := paste0(INFO, ";CSQ=", csq_vec[!empty_info & !is.na(csq_vec) & csq_vec != ""])]

  # 8) Add canonical CSQ header line to vcf_header, if not present
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
  list(
    vcf_data  = vcf_data,
    vcf_header = vcf_header
  )
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
#' @param csq_map     Mapping table (data.table) with VEP_CSQ_field + mapping cols.
#' @param mapping_col Column name in csq_map to use ("SnpEff_ANN_field" or "BCFtools_BCSQ_field").
#' @return Character vector of CSQ strings, one per element of info_vec.
#' @keywords internal
.build_csq_from_tag <- function(info_vec,
                                tag,
                                tag_columns,
                                csq_columns,
                                csq_map,
                                mapping_col) {
  # Extract TAG=... from INFO
  tag_pattern <- paste0(tag, "=[^;]+")
  tag_raw <- stringr::str_extract(info_vec, tag_pattern)
  tag_val <- sub(paste0("^", tag, "="), "", tag_raw)
  tag_val[is.na(tag_raw)] <- NA_character_

  # Pre-filter mapping to rows where this tag's field is non-empty
  if (!mapping_col %in% names(csq_map)) {
    stop("Mapping column ", mapping_col, " not found in csq_map.")
  }
  map_subset <- csq_map[!is.na(get(mapping_col)) & get(mapping_col) != ""]

  build_one <- function(one_val) {
    if (is.na(one_val) || one_val == "") {
      return(NA_character_)
    }

    # Multiple entries per record are comma-separated; pick the first for now
    entries <- strsplit(one_val, ",", fixed = TRUE)[[1]]
    entry   <- entries[1]

    # Split entry into tag columns
    fields <- strsplit(entry, "\\|")[[1]]
    if (length(fields) < length(tag_columns)) {
      fields <- c(fields, rep("", length(tag_columns) - length(fields)))
    }
    names(fields) <- tag_columns

    # Construct CSQ columns in canonical order
    csq_vals <- character(length(csq_columns))

    for (j in seq_along(csq_columns)) {
      csq_name <- csq_columns[j]
      val <- ""

      map_row <- map_subset[VEP_CSQ_field == csq_name]
      if (nrow(map_row) == 1L) {
        tag_name <- map_row[[mapping_col]]
        if (!is.null(tag_name) && !is.na(tag_name) &&
            nzchar(tag_name) && tag_name %in% names(fields)) {
          val <- fields[[tag_name]]
        }
      }

      csq_vals[j] <- ifelse(is.na(val), "", val)
    }

    out <- paste(csq_vals, collapse = "|")
    # Debug: count pipes for BCSQ path
    # if (tag == "BCSQ") {
    #   pc <- nchar(out) - nchar(gsub("\\|", "", out))
    #   message("DEBUG BCSQ->CSQ: ", out, "  [", pc, " pipes]")
    # }
    out
  }

  vapply(tag_val, build_one, character(1L))
}