suppressMessages(library(data.table))
suppressMessages(library(stringr))

# # Function to generate pedigree data
# generate_pedigree_data <- function(opt) {
#   
#   # Ensure multiple entries are handled correctly
#   siblings <- if (!is.null(opt$sibling)) unlist(opt$sibling) else NULL
#   uncles <- if (!is.null(opt$uncle)) unlist(opt$uncle) else NULL
#   aunts <- if (!is.null(opt$aunt)) unlist(opt$aunt) else NULL
#   
#   # Generate pedigree data
#   pedigree_data <- data.table(
#     sample_id = c(
#       opt$proband, opt$father, opt$mother, siblings,
#       opt$maternal_grandfather, opt$maternal_grandmother,
#       opt$paternal_grandfather, opt$paternal_grandmother,
#       uncles, aunts
#     ),
#     kinship = c(
#       "proband", "father", "mother", rep("sibling", length(siblings)),
#       "maternal_grandfather", "maternal_grandmother",
#       "paternal_grandfather", "paternal_grandmother",
#       rep("uncle", length(uncles)), rep("aunt", length(aunts))
#     )
#   )
#   
#   pedigree_data <- pedigree_data[!is.na(sample_id)]  # Remove NAs
#   return(pedigree_data)
# }

read_vcf_long <- function(vcf_path) {
  vcf <- fread(cmd = paste("zcat", vcf_path), sep = "\t", skip = "#CHROM", header = TRUE)
  header <- fread(cmd = paste("zgrep ^#CHROM", vcf_path), sep = "\t", header = FALSE)
  colnames(vcf) <- sub("^#", "", unlist(header[1, ]))
  
  fixed_cols <- c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT")
  sample_cols <- setdiff(names(vcf), fixed_cols)
  
  melt(vcf, id.vars = fixed_cols, measure.vars = sample_cols,
       variable.name = "Sample", value.name = "Genotype")
}

process_sv_vcf <- function(vcf_long) {
  vcf_long <- vcf_long[Genotype != "./.:.:.:.:.:."]  # keep only called variants
  vcf_long[, VariantIndex := seq_len(.N), by = ID]
  vcf_long[, Sample := sub("^[0-9]+_", "", Sample)]
  vcf_long[, GT := tstrsplit(Genotype, ":", fixed = TRUE)[[1]]]
  vcf_long[, AlleleCount := sapply(strsplit(GT, "[/|]"), function(x) sum(x != "." & x != "0"))]
  
  idlist_table <- unique(vcf_long[, .(ID, INFO)])
  idlist_table[, IDLIST := fifelse(grepl("IDLIST=", INFO),
                                   sub(".*IDLIST=([^;]+).*", "\\1", INFO),
                                   NA_character_)]
  idlist_table[, IDLIST_VEC := strsplit(IDLIST, ",")]
  idlist_long <- idlist_table[, .(VariantIndex = seq_along(IDLIST_VEC[[1]]),
                                  VariantID = IDLIST_VEC[[1]]), by = ID]
  
  merged <- merge(
    vcf_long[, .(CHROM, POS, ID, Sample, Genotype, GT, AlleleCount, VariantIndex)],
    idlist_long, by = c("ID", "VariantIndex"))
  
  merged <- merged[AlleleCount > 0]
  merge(merged, merged[, .(N_Cohort = .N), by = ID], by = "ID", all.x = TRUE)
}

process_snv_vcf <- function(vcf_long) {
  vcf_long <- vcf_long[!grepl("^\\./\\.", Genotype)]
  vcf_long[, VariantID := paste(CHROM, POS, REF, ALT, sep = "_")]
  vcf_long[, VariantIndex := seq_len(.N), by = VariantID]
  vcf_long[, Sample := sub("^[0-9]+_", "", Sample)]
  vcf_long[, GT := tstrsplit(Genotype, ":", fixed = TRUE)[[1]]]
  vcf_long[, AlleleCount := sapply(strsplit(GT, "[/|]"), function(x) sum(x != "." & x != "0"))]
  
  final <- vcf_long[AlleleCount > 0, .(CHROM, POS, VariantID, Sample, Genotype, GT, AlleleCount, VariantIndex)]
  merge(final, final[, .(N_Cohort = .N), by = VariantID], by = "VariantID", all.x = TRUE)
}

# snvs_vcf <- "/g/data/kr68/projects/KISKUM_Myop/LRS00117/pipeface-v0.6.2/20250415-091359/LRS00117-00-RF-05/LRS00117-00-RF-05.hg38.clair3.snp_indel.phased.annotated.vcf.gz"
# pedigree_data <- data.table(sample_id = "LRS00117-00-RF-05", kinship = "proband")
# snvs_vcf_cohort <- "/g/data/kr68/puzzleapp/KISKUM_Myop/KISKUM_Myop.clair3.cohort_merged.split.vcf.gz"


# Function to process SNV data
process_snv_data <- function(snvs_vcf, pedigree_data, snvs_vcf_cohort = NA) {
  cat("Processing SNV VCF: ", snvs_vcf, "\n")
  
  # Error handling: Check if the VCF file exists
  if (!file.exists(snvs_vcf)) {
    stop(paste("SNV VCF file not found:", snvs_vcf))
  }
  
  # Try to read the VCF file, catch errors
  tryCatch({
    snvs_data <- fread(cmd = paste("gunzip -c", snvs_vcf), sep = "\t", skip = "#CHROM", header = TRUE)
  }, error = function(e) {
    stop("Error reading SNV VCF file:", e)
  })

  # Check if the data is empty
  if (nrow(snvs_data) == 0) {
    stop("SNV VCF contains no variant records.")
  }
  
  # Read the VCF file header to extract CSQ format
  vcf_header <- tryCatch({
    fread(cmd = paste("zgrep '^##' ", snvs_vcf), sep = "\n", header = FALSE)
  }, error = function(e) {
    stop("Error reading VCF header:", e)
  })
  
  # Extract the CSQ format from the header
  csq_format <- vcf_header[grepl("##INFO=<ID=CSQ", V1)]
  if (nrow(csq_format) == 0) {
    stop("CSQ format not found in VCF header.")
  }
  
  csq_columns <- str_match(csq_format$V1, "Format: (.*)>")[, 2]
  csq_columns <- gsub('"$', '', csq_columns)
  csq_columns <- unlist(strsplit(csq_columns, "\\|"))
  
  # Error handling: Ensure the CSQ columns are properly extracted
  if (length(csq_columns) == 0) {
    stop("Error extracting CSQ columns from the VCF header.")
  }
  
  # Remove duplicate rows from SNV data
  snvs_data <- unique(snvs_data)
  
  # Trio handling: Check if the trio samples are present in the VCF file
  trio <- pedigree_data$sample_id
  missing_samples <- setdiff(trio, names(snvs_data))
  if (length(missing_samples) > 0) {
    stop(paste("Missing trio samples in VCF:", paste(missing_samples, collapse = ", ")))
  }
  
  # Define sorting priority
  kinship_order <- c("proband" = 1, "mother" = 2, "father" = 3)
  
  # Assign priority (default to 4 for other relatives)
  pedigree_data[, sort_order := kinship_order[kinship]]
  pedigree_data[is.na(sort_order), sort_order := 4]
  
  # Sort by defined order
  setorder(pedigree_data, sort_order)
  
  # Extract sorted trio sample IDs
  trio <- pedigree_data$sample_id
  
  # Remove the "#" character from column names
  setnames(snvs_data, old = names(snvs_data), new = gsub("#", "", names(snvs_data)))
  snvs_data[, VariantID := paste(CHROM, POS, REF, ALT, sep = "_")]
  snvs_data[, ID := paste0(CHROM, "_", POS, "_", .I)]
  
  # Melt the data.table based on the trio samples
  variables <- names(snvs_data)
  snvs_data_melt <- melt(snvs_data, id.vars = variables[!(variables %in% trio)], variable.name = "Sample", value.name = "Value")
  
  # Error handling: Ensure FORMAT column exists before proceeding
  if (!"FORMAT" %in% names(snvs_data_melt)) {
    stop("FORMAT column not found in the VCF file.")
  }
  
  # Measure the number of fields in each row of the FORMAT column
  snvs_data_melt[, num_fields := lengths(strsplit(FORMAT, ":"))]
  max_fields <- max(snvs_data_melt$num_fields)
  
  # Adjust the FORMAT and Value columns for missing fields
  snvs_data_melt[, FORMAT := ifelse(max_fields - num_fields > 0,
                                    paste0(FORMAT, strrep(":NA", max_fields - num_fields)),
                                    FORMAT)]
  snvs_data_melt[, Value := ifelse(max_fields - num_fields > 0,
                                   paste0(Value, strrep(":NA", max_fields - num_fields)),
                                   Value)]
  
  # Split the FORMAT and Value columns and merge back into the data.table
  format_splits <- snvs_data_melt[, tstrsplit(FORMAT, ":", fixed = TRUE)]
  format_splits <- melt(cbind(snvs_data_melt[, .(ID, Sample)], format_splits), id.vars = c("ID", "Sample"))[, .(ID, Sample, FORMAT = value)]
  value_splits <- snvs_data_melt[, tstrsplit(Value, ":", fixed = TRUE)]
  value_splits <- melt(cbind(snvs_data_melt[, .(ID, Sample)], value_splits), id.vars = c("ID", "Sample"))[, .(ID, Sample, Value = value)]
  snvs_data_melt <- merge(snvs_data_melt[, setdiff(names(snvs_data_melt), c("FORMAT", "Value")), with = FALSE],
                          cbind(format_splits, value_splits[, .(Value)]), by = c("ID", "Sample"))
  
  snvs_data_melt <- snvs_data_melt[FORMAT != "NA"]
  snvs_data_melt[, num_fields := NULL]
  
  # Memory cleanup
  rm(format_splits, value_splits, snvs_data)
  gc()
  
  # Create a data.table for the trio sample codes
  trio_dt <- data.table(Sample = trio, Code = seq(1, length(trio)))
  
  # Merge consequence_data_melt with the trio sample codes
  snvs_data_melt <- merge(snvs_data_melt, trio_dt, by = "Sample")
  
  # Filter rows to keep only GT, AD, and DP fields
  snvs_data_melt <- snvs_data_melt[FORMAT %in% c("GT","GQ", "AD", "DP")]
  
  # Process allele depth (AD) to extract the number of reads supporting the alternate allele
  snvs_data_melt[FORMAT == "AD", Value := tstrsplit(Value, ",", fixed = TRUE)[2]]
  
  # Convert FORMAT to character if it's not already
  snvs_data_melt[, FORMAT := as.character(FORMAT)]
  
  # Add sample codes to the FORMAT column for trio-specific values
  snvs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]
  
  # Cast the melted data back to wide format
  formula <- paste(names(snvs_data_melt)[!names(snvs_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")], collapse = " + ")
  formula <- sprintf("%s ~ FORMAT", formula)
  snvs_data_melt <- data.table(dcast(snvs_data_melt, formula, value.var = "Value"))
  
  id_vars <- names(snvs_data_melt)
  ad_vars <- grep("^AD_", id_vars, value = TRUE)
  gq_vars <- grep("^GQ_", id_vars, value = TRUE)
  dp_vars <- grep("^DP_", id_vars, value = TRUE)
  vaf_vars <- gsub("AD","VAF",ad_vars)
  # Calculate the VAF values and assign to the data.table
  for (i in seq_along(ad_vars)) {
    snvs_data_melt[, (vaf_vars[i]) := round(as.integer(get(ad_vars[i])) / as.integer(get(dp_vars[i])),2)]
  }
  
  # Add variant annotation and GNOMAD links
  snvs_data_melt[, CSQ := str_extract(INFO, "CSQ=[^;]+")]
  snvs_data_melt[, CSQ := gsub("^CSQ=", "", CSQ)]
  csq_split_data <- snvs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
  setnames(csq_split_data, csq_columns)
  snvs_data_melt <- cbind(snvs_data_melt, csq_split_data)
  
  snvs_data_melt[,SpliceAI_pred:=paste(SpliceAI_pred_SYMBOL,SpliceAI_pred_DS_DL,SpliceAI_pred_DS_DG,SpliceAI_pred_DS_AL,SpliceAI_pred_DS_AG)]
  
  snvs_data_melt[, SpliceAI_pred := ifelse(
    SpliceAI_pred_SYMBOL=="",
    NA,  # If SpliceAI_pred_SYMBOL is NA, set SpliceAI_compact to NA
    paste(
      ALT,                                            # The alternate allele
      SpliceAI_pred_SYMBOL,                           # Gene symbol
      SpliceAI_pred_DS_AG, SpliceAI_pred_DS_AL,       # Delta scores for AG (Acceptor Gain), AL (Acceptor Loss)
      SpliceAI_pred_DS_DG, SpliceAI_pred_DS_DL,       # Delta scores for DG (Donor Gain), DL (Donor Loss)
      SpliceAI_pred_DP_AG, SpliceAI_pred_DP_AL,       # Delta positions for AG, AL
      SpliceAI_pred_DP_DG, SpliceAI_pred_DP_DL,       # Delta positions for DG, DL
      sep = "|"                                       # Separator for compact format
    )
  )]
  
  snvs_data_melt[, GNOMADv4 := sprintf("https://gnomad.broadinstitute.org/variant/%s-%s-%s-%s?dataset=gnomad_r4", CHROM, POS, REF, ALT)]
  
  # snvs_data_melt[,CATEGORY := "SNV & Indel"]
  # snvs_data_melt[,VAR_TYPE := ifelse(nchar(REF) == nchar(ALT),"SNV",ifelse(nchar(REF) > nchar(ALT),"Deletion","Insertion"))]
  
  num_samples <- dim(pedigree_data)[1]
  # Generate the column names for AD, DP, VAF, and GT
  ad_cols <- paste0("AD_", 1:num_samples)
  dp_cols <- paste0("DP_", 1:num_samples)
  vaf_cols <- paste0("VAF_", 1:num_samples)
  gt_cols <- paste0("GT_", 1:num_samples)
  gq_cols <- paste0("GQ_", 1:num_samples)
  
  snvs_data_melt[,AF := as.numeric(gnomAD_AF_joint)]
  snvs_data_melt[AF>0,gnomAD_ID := paste0(str_remove(CHROM, 'chr'), '-', POS, '-', REF, '-', ALT)]
  snvs_data_melt[AF > 0, gnomAD_ID := sprintf(
    '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
    gsub("^.*variant/", "", gnomAD_ID), gsub("^.*variant/", "", gnomAD_ID)
  )]
  
  snvs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
    '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
    ClinVar, ClinVar
  )]
  
  if (is.na(snvs_vcf_cohort)) {
    cat("Cohort VCF is NA.\n")
    snvs_data_melt[, N_Cohort := NA_integer_]
  } else {
    cat("Cohort VCF is not NA.\n")
    snv_vcf_long <- read_vcf_long(snvs_vcf_cohort)
    final_snv <- process_snv_vcf(snv_vcf_long)
    final_snv[ , fam := sub("-RF.*$", "", Sample) ]
    pedigree_data[ , fam := sub("-RF.*$", "", sample_id) ]
    final_snv <- merge(
      final_snv[ , .(VariantID, fam, N_Cohort) ],
      pedigree_data[ , .(fam, kinship, sort_order) ],
      by = "fam"
    )
    final_snv <- final_snv[,fam:=NULL]
    snvs_data_melt <- merge(snvs_data_melt, final_snv[, .(VariantID, N_Cohort)],
                           by = "VariantID", all.x = TRUE)
  }

  snvs_data_melt <- cbind(snvs_data_melt[,.(ID,CATEGORY="SNV & Indel",VAR_TYPE=ifelse(nchar(REF) == nchar(ALT),"SNV",ifelse(nchar(REF) > nchar(ALT),"Deletion","Insertion")),CHROM,POS,
                                            VAR_LENGTH=ifelse(nchar(REF) == nchar(ALT), 1, abs(nchar(REF) - nchar(ALT))),REF,ALT,QUAL,FILTER,GENE_ID=Gene,GENE_SYMBOL=SYMBOL,TRANSCRIPT=Feature,HGVSg,HGVSc,HGVSp,
                                            CONSEQUENCE=Consequence,CLINVAR=CLIN_SIG,CLINVAR_ID,gnomAD_ID,AF,N_HOM_ALT=as.numeric(gnomAD_nhomalt_joint),N_Cohort,SIFT,PolyPhen,REVEL,am_class,am_pathogenicity,CADD_PHRED,CADD_RAW,SpliceAI_pred,Donor_Loss=as.numeric(SpliceAI_pred_DS_DL),Donor_Gain=as.numeric(SpliceAI_pred_DS_DG),Acceptor_Loss=as.numeric(SpliceAI_pred_DS_AL),Acceptor_Gain=as.numeric(SpliceAI_pred_DS_AG))],
                          snvs_data_melt[,c(ad_cols,dp_cols,vaf_cols,gt_cols,gq_cols),with=FALSE])
  
  snvs_data_melt[,CLINVAR:=str_replace_all(CLINVAR,"&",",")]
  snvs_data_melt[CLINVAR=="",CLINVAR:=NA]
  
  # Count the number of alt alleles
  for (col_name in names(snvs_data_melt)[grepl("GT_",names(snvs_data_melt))]) {
    new_col_name <- paste0("alt_allele_count_", tstrsplit(col_name,"_",fixed=TRUE)[[2]])
    snvs_data_melt[, (new_col_name) := rowSums(sapply(.SD, function(x) 2 - (str_count(x, "0")+str_count(x, "\\.")))), .SDcols = col_name]
  }
  
  # Final output
  cat("SNV data processed.\n")
  return(snvs_data_melt)
}

# svs_vcf <- "/g/data/kr68/projects/KISKUM_Ataxia/LRS00198/pipeface-v0.6.1/20250319-115707/LRS00198-00-RF-01/LRS00198-00-RF-01.hg38.cutesv.sv.annotated.vcf.gz"
# pedigree_data <- data.table(sample_id = "LRS00198-00-RF-01", kinship = "proband")
# svs_vcf_cohort <- "/g/data/kr68/puzzleapp/KISKUM_Ataxia/KISKUM_Ataxia.clair3.cutesv.cohort_merged.vcf.gz"

# Function to process SV data
process_sv_data <- function(svs_vcf, pedigree_data, svs_vcf_cohort = NA) {
  cat("Processing SV VCF: ", svs_vcf, "\n")
  
  # Error handling: Check if the VCF file exists
  if (!file.exists(svs_vcf)) {
    stop(paste("SV VCF file not found:", svs_vcf))
  }
  
  # Try to read the VCF file, catch errors
  tryCatch({
    svs_data <- fread(cmd = paste("gunzip -c", svs_vcf), sep = "\t", skip = "#CHROM", header = TRUE)
  }, error = function(e) {
    stop("Error reading SV VCF file:", e)
  })

  # Check if the data is empty
  if (nrow(svs_data) == 0) {
    stop("SV VCF contains no variant records.")
  }
  
  # Read the VCF file header to extract CSQ format
  vcf_header <- tryCatch({
    fread(cmd = paste("zgrep '^##' ", svs_vcf), sep = "\n", header = FALSE)
  }, error = function(e) {
    stop("Error reading VCF header:", e)
  })
  
  # Extract the CSQ format from the header
  csq_format <- vcf_header[grepl("##INFO=<ID=CSQ", V1)]
  if (nrow(csq_format) == 0) {
    stop("CSQ format not found in VCF header.")
  }
  
  csq_columns <- str_match(csq_format$V1, "Format: (.*)>")[, 2]
  csq_columns <- gsub('"$', '', csq_columns)
  csq_columns <- unlist(strsplit(csq_columns, "\\|"))
  
  # Error handling: Ensure the CSQ columns are properly extracted
  if (length(csq_columns) == 0) {
    stop("Error extracting CSQ columns from the VCF header.")
  }
  
  # Remove duplicate rows from sv data
  svs_data <- unique(svs_data)
  
  # Trio handling: Check if the trio samples are present in the VCF file
  trio <- pedigree_data$sample_id
  missing_samples <- setdiff(trio, names(svs_data))
  if (length(missing_samples) > 0) {
    stop(paste("Missing trio samples in VCF:", paste(missing_samples, collapse = ", ")))
  }
  
  # Sort the trio samples
  trio <- sort(trio)
  # Define sorting priority
  kinship_order <- c("proband" = 1, "mother" = 2, "father" = 3)
  
  # Assign priority (default to 4 for other relatives)
  pedigree_data[, sort_order := kinship_order[kinship]]
  pedigree_data[is.na(sort_order), sort_order := 4]
  
  # Remove the "#" character from column names
  setnames(svs_data, old = names(svs_data), new = gsub("#", "", names(svs_data)))
  #svs_data[, ID := paste(CHROM, POS, REF, ALT, sep = "_")]
  svs_data[, VariantID := ID]
  svs_data[, ID := paste0(CHROM, "_", POS, "_", .I)]
  
  # Melt the data.table based on the trio samples
  variables <- names(svs_data)
  svs_data_melt <- melt(svs_data, id.vars = variables[!(variables %in% trio)], variable.name = "Sample", value.name = "Value")
  
  # Error handling: Ensure FORMAT column exists before proceeding
  if (!"FORMAT" %in% names(svs_data_melt)) {
    stop("FORMAT column not found in the VCF file.")
  }
  
  # Measure the number of fields in each row of the FORMAT column
  svs_data_melt[, num_fields := lengths(strsplit(FORMAT, ":"))]
  max_fields <- max(svs_data_melt$num_fields)
  
  # Adjust the FORMAT and Value columns for missing fields
  svs_data_melt[, FORMAT := ifelse(max_fields - num_fields > 0,
                                   paste0(FORMAT, strrep(":NA", max_fields - num_fields)),
                                   FORMAT)]
  svs_data_melt[, Value := ifelse(max_fields - num_fields > 0,
                                  paste0(Value, strrep(":NA", max_fields - num_fields)),
                                  Value)]
  
  # Split the FORMAT and Value columns and merge back into the data.table
  format_splits <- svs_data_melt[, tstrsplit(FORMAT, ":", fixed = TRUE)]
  format_splits <- melt(cbind(svs_data_melt[, .(ID, Sample)], format_splits), id.vars = c("ID", "Sample"))[, .(ID, Sample, FORMAT = value)]
  value_splits <- svs_data_melt[, tstrsplit(Value, ":", fixed = TRUE)]
  value_splits <- melt(cbind(svs_data_melt[, .(ID, Sample)], value_splits), id.vars = c("ID", "Sample"))[, .(ID, Sample, Value = value)]
  svs_data_melt <- merge(svs_data_melt[, setdiff(names(svs_data_melt), c("FORMAT", "Value")), with = FALSE],
                         cbind(format_splits, value_splits[, .(Value)]), by = c("ID", "Sample"))
  
  svs_data_melt <- svs_data_melt[FORMAT != "NA"]
  svs_data_melt[, num_fields := NULL]
  
  # Memory cleanup
  rm(format_splits, value_splits, svs_data)
  gc()
  
  # Create a data.table for the trio sample codes
  trio_dt <- data.table(Sample = trio, Code = seq(1, length(trio)))
  
  # Merge consequence_data_melt with the trio sample codes
  svs_data_melt <- merge(svs_data_melt, trio_dt, by = "Sample")
  
  # Filter rows to keep only GT, AD, and DP fields
  svs_data_melt <- svs_data_melt[FORMAT %in% c("GT","GQ","DR", "DV")]
  
  # Process allele depth (AD) to extract the number of reads supporting the alternate allele
  #svs_data_melt[FORMAT == "AD", Value := tstrsplit(Value, ",", fixed = TRUE)[2]]
  
  # Convert FORMAT to character if it's not already
  svs_data_melt[, FORMAT := as.character(FORMAT)]
  
  # Add sample codes to the FORMAT column for trio-specific values
  svs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]
  
  # Cast the melted data back to wide format
  formula <- paste(names(svs_data_melt)[!names(svs_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")], collapse = " + ")
  formula <- sprintf("%s ~ FORMAT", formula)
  svs_data_melt <- data.table(dcast(svs_data_melt, formula, value.var = "Value"))
  
  id_vars <- names(svs_data_melt)
  dv_vars <- grep("^DV_", id_vars, value = TRUE)
  dr_vars <- grep("^DR_", id_vars, value = TRUE)
  gq_vars <- grep("^GQ_", id_vars, value = TRUE)
  dp_vars <- gsub("DR","DP",dr_vars)
  ad_vars <- gsub("DR","AD",dr_vars)
  vaf_vars <- gsub("AD","VAF",ad_vars)
  # Calculate the VAF values and assign to the data.table
  for (i in seq_along(ad_vars)) {
    svs_data_melt[, (ad_vars[i]) := as.integer(get(dv_vars[i]))]
    svs_data_melt[, (gq_vars[i]) := as.integer(get(gq_vars[i]))]
    svs_data_melt[, (dp_vars[i]) := round((as.integer(get(dv_vars[i])) + as.integer(get(dr_vars[i]))), 2)]
    svs_data_melt[, (vaf_vars[i]) := round(as.integer(get(dv_vars[i])) / 
                                             (as.integer(get(ad_vars[i])) + as.integer(get(dr_vars[i]))), 2)]
  }
  
  # Add variant annotation and GNOMAD links
  svs_data_melt[, CSQ := str_extract(INFO, "CSQ=[^;]+")]
  svs_data_melt[, CSQ := gsub("^CSQ=", "", CSQ)]
  csq_split_data <- svs_data_melt[, tstrsplit(CSQ, "|", fixed = TRUE)]
  # Check if the number of columns matches csq_columns
  if (ncol(csq_split_data) < length(csq_columns)) {
    # Add empty columns to match csq_columns
    missing_cols <- length(csq_columns) - ncol(csq_split_data)
    for (i in 1:missing_cols) {
      csq_split_data[, paste0("V", ncol(csq_split_data) + 1) := ifelse(is.na(V1), as.character(NA), "")]
    }
  }
  setnames(csq_split_data, csq_columns)
  svs_data_melt <- cbind(svs_data_melt, csq_split_data)
  
  svs_data_melt[,SpliceAI_pred:=NA]
  svs_data_melt[, SVTYPE := sub(".*SVTYPE=([^;]*);.*", "\\1", INFO)]
  svs_data_melt[, SVLEN := as.numeric(sub(".*SVLEN=([^;]*);.*", "\\1", INFO))]
  
  num_samples <- dim(pedigree_data)[1]
  # Generate the column names for AD, DP, VAF, and GT
  ad_cols <- paste0("AD_", 1:num_samples)
  dp_cols <- paste0("DP_", 1:num_samples)
  vaf_cols <- paste0("VAF_", 1:num_samples)
  gt_cols <- paste0("GT_", 1:num_samples)
  gq_cols <- paste0("GQ_", 1:num_samples)
  
#  svs_data_melt[,AF := as.numeric(gnomAD_sv_AF)]
#  svs_data_melt[, gnomAD_ID := sub(".*?(DEL|DUP|INV|INS|CNV|TRA|BND)_", "\\1_", gnomAD_sv)]
#  svs_data_melt[AF > 0, gnomAD_ID := sprintf(
#    '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_sv_r4" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
#    gsub("^.*variant/", "", gnomAD_ID), gsub("^.*variant/", "", gnomAD_ID)
#  )]
#  svs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
#    '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
#    ClinVar, ClinVar
#  )]

  # Handle missing columns safely
if ("gnomAD_sv_AF" %in% names(svs_data_melt)) {
  svs_data_melt[, AF := suppressWarnings(as.numeric(gnomAD_sv_AF))]
} else {
  svs_data_melt[, AF := NA_real_]
  warning("gnomAD_sv_AF column is missing. AF set to NA.")
}

if ("gnomAD_sv" %in% names(svs_data_melt)) {
  svs_data_melt[, gnomAD_ID := sub(".*?(DEL|DUP|INV|INS|CNV|TRA|BND)_", "\\1_", gnomAD_sv)]

  svs_data_melt[AF > 0 & !is.na(gnomAD_ID), gnomAD_ID := sprintf(
    '<a href="https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_sv_r4" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
    gsub("^.*variant/", "", gnomAD_ID), gsub("^.*variant/", "", gnomAD_ID)
  )]
} else {
  svs_data_melt[, gnomAD_ID := NA_character_]
  warning("gnomAD_sv column is missing. gnomAD_ID set to NA.")
}

if ("ClinVar" %in% names(svs_data_melt)) {
  svs_data_melt[!is.na(ClinVar) & ClinVar != "", CLINVAR_ID := sprintf(
    '<a href="https://www.ncbi.nlm.nih.gov/clinvar/variation/%s/" target="_blank" style="color: blue; text-decoration: underline; cursor: pointer;">%s</a>',
    ClinVar, ClinVar
  )]
} else {
  svs_data_melt[, CLINVAR_ID := NA_character_]
  warning("ClinVar column is missing. CLINVAR_ID set to NA.")
}
  
  if (is.na(svs_vcf_cohort)) {
    svs_data_melt[, N_Cohort := NA_integer_]
  } else {
    sv_vcf_long <- read_vcf_long(svs_vcf_cohort)
    final_sv <- process_sv_vcf(sv_vcf_long)
    final_sv[ , fam := sub("-RF.*$", "", Sample) ]
    pedigree_data[ , fam := sub("-RF.*$", "", sample_id) ]
    final_sv <- merge(
      final_sv[ , .(VariantID, fam, N_Cohort) ],
      pedigree_data[ , .(fam, kinship, sort_order) ],
      by = "fam"
    )
    final_sv <- final_sv[,fam:=NULL]
    # final_sv <- merge(final_sv[, .(VariantID, sample_id = Sample, N_Cohort)],
    #                   pedigree_data,
    #                   by = "sample_id")
    svs_data_melt <- merge(svs_data_melt, final_sv[, .(VariantID, N_Cohort)],
                           by = "VariantID", all.x = TRUE)
  }

# Ensure required columns exist
required_cols <- c("CLIN_SIG", "CLINVAR_ID", "gnomAD_ID", "gnomAD_sv_N_HOMALT")
for (col in required_cols) {
  if (!col %in% names(svs_data_melt)) {
    svs_data_melt[[col]] <- NA
  }
}
  
  svs_data_melt <- cbind(svs_data_melt[,.(ID,CATEGORY="SV",VAR_TYPE=SVTYPE,CHROM,POS,
                                          VAR_LENGTH=abs(SVLEN),REF,ALT,QUAL,FILTER,GENE_ID=Gene,GENE_SYMBOL=SYMBOL,TRANSCRIPT=Feature,HGVSg,HGVSc,HGVSp,
                                          CONSEQUENCE=Consequence,CLINVAR=CLIN_SIG,CLINVAR_ID,gnomAD_ID,AF,N_HOM_ALT=as.numeric(gnomAD_sv_N_HOMALT),N_Cohort,SIFT,PolyPhen,REVEL=NA,am_class=NA,am_pathogenicity=NA,CADD_PHRED,CADD_RAW,SpliceAI_pred=NA,Donor_Loss=NA,Donor_Gain=NA,Acceptor_Loss=NA,Acceptor_Gain=NA)],
                         svs_data_melt[,c(ad_cols,dp_cols,vaf_cols,gt_cols,gq_cols),with=FALSE])
  
  svs_data_melt[,CLINVAR:=str_replace_all(CLINVAR,"&",",")]
  svs_data_melt[CLINVAR=="",CLINVAR:=NA]
  
  # Count the number of alt alleles
  for (col_name in names(svs_data_melt)[grepl("GT_",names(svs_data_melt))]) {
    new_col_name <- paste0("alt_allele_count_", tstrsplit(col_name,"_",fixed=TRUE)[[2]])
    svs_data_melt[, (new_col_name) := rowSums(sapply(.SD, function(x) 2 - (str_count(x, "0")+str_count(x, "\\.")))), .SDcols = col_name]
  }
  
  # Final output
  cat("SNV data processed.\n")
  return(svs_data_melt)
  
}
