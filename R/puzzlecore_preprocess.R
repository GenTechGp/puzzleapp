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

  snvs_data <- data.table::fread(cmd = paste("gunzip -c", snvs_vcf),
                                 sep = "\t", skip = "#CHROM", header = TRUE)
  vcf_header <- data.table::fread(cmd = paste("zgrep '^##' ", snvs_vcf),
                                  sep = "\n", header = FALSE)

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

  snvs_data_melt[, CSQ := stringr::str_extract(INFO, "CSQ=[^;]+")]
  snvs_data_melt[, CSQ := sub("^CSQ=", "", CSQ)]
  csq_split <- snvs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
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
    '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
    gnomAD_ID, gnomAD_ID
  )]

  snvs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
    '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
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
    CONSEQUENCE = Consequence,
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
#' @return data.table
#' @export
process_sv_data <- function(svs_vcf, pedigree_data, svs_vcf_cohort = NA) {
  cat("Processing SV VCF:", svs_vcf, "\n")
  if (!file.exists(svs_vcf)) stop("SV VCF file not found: ", svs_vcf)

  svs_data <- data.table::fread(cmd = paste("gunzip -c", svs_vcf),
                                sep = "\t", skip = "#CHROM", header = TRUE)
  vcf_header <- data.table::fread(cmd = paste("zgrep '^##' ", svs_vcf),
                                  sep = "\n", header = FALSE)

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
  svs_data[, ID := paste0(CHROM, "_", POS, "_", .I)]

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
      '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_sv_r4" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
      gnomAD_ID, gnomAD_ID
    )]
  } else {
    svs_data_melt[, gnomAD_ID := NA_character_]
  }
  if ("ClinVar" %in% names(svs_data_melt)) {
    svs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
      '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
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
    CONSEQUENCE = Consequence,
    CLINVAR = CLIN_SIG,
    CLINVAR_ID,
    gnomAD_ID,
    AF,
    N_HOM_ALT = suppressWarnings(as.numeric(gnomAD_sv_N_HOMALT)),
    N_Cohort,
    SIFT, PolyPhen,
    REVEL = NA_real_,
    am_class = NA_character_,
    am_pathogenicity = NA_character_,
    CADD_PHRED, CADD_RAW,
    SpliceAI_pred,
    Donor_Loss = NA_real_,
    Donor_Gain = NA_real_,
    Acceptor_Loss = NA_real_,
    Acceptor_Gain = NA_real_
  )]

  full <- cbind(core, svs_data_melt[, c(ad_cols, dp_cols, vaf_cols, gt_cols, gq_cols), with = FALSE])

  full[, CLINVAR := stringr::str_replace_all(CLINVAR, "&", ",")]
  full[CLINVAR == "", CLINVAR := NA]

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