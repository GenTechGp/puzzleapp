#!/usr/bin/env Rscript
# Combined Processing Script: Process_Data.R
# usage: ./Process_Data.R <config> <out.RData>
# --------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  stop("usage: ./Process_Data.R <config> <out.RData>")
}

config_path <- args[1]
rdata_output <- args[2]

if (!file.exists(config_path)) {
  stop(paste("Configuration file not found at:", config_path))
}
config <- yaml::read_yaml(config_path)

################################################################################

# Load required libraries
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(yaml)

################################################################################

# Add checks to see if the contents of the YAML file are correct
if (is.null(config$samples)) {
  stop("YAML file is missing 'samples' section.")
}
if (is.null(config$paths)) {
  stop("YAML file is missing 'paths' section.")
}
if (is.null(config$dependencies)) {
  stop("YAML file is missing 'dependencies' section.")
}

################################################################################

# Convert the list of samples into a data.table
pedigree_data <- rbindlist(lapply(config$samples, as.data.table))

# BAM files list
bam_files <- pedigree_data$bam

# Error handling for coverage paths
valid_coverage_paths <- lapply(pedigree_data$coverage, file.exists)

if (any(!unlist(valid_coverage_paths))) {
  warning("Some coverage file paths are missing or invalid. Setting coverage_data to NULL.")
  coverage_data <- NULL
} else {
  # Coverage data with added sample ID column
  coverage_data <- rbindlist(lapply(1:nrow(pedigree_data), function(i) {
    sample_id <- pedigree_data$sample_id[i]
    coverage_path <- pedigree_data$coverage[i]

    # Read the coverage data and add a new column for the sample ID
    coverage_dt <- fread(coverage_path, header = TRUE)
    coverage_dt[, sample_id := sample_id]  # Add sample ID column

    return(coverage_dt)
  }))
  coverage_data <- coverage_data[,.(CHROM=chromosome,AVERAGE_COVERAGE=average_depth,SAMPLE=sample_id)]
  coverage_data <- coverage_data[!str_detect(CHROM,"_")]
}

# Remove 'bam' and 'coverage' columns from pedigree_data
pedigree_data[, c("bam", "coverage") := NULL]

# Access other paths and dependencies
snvs_vcf <- config$paths$snvs_vcf
svs_vcf <- config$paths$svs_vcf

#chain_hg38_to_chm13 <- config$dependencies$chain_hg38_to_chm13
panel_app <- config$dependencies$panel_app
vep_consequences <- config$dependencies$vep_consequences
phenotype_data <- config$dependencies$phenotype_data

# Error handling for somalier path
if (!is.null(config$paths$somalier)) {
  somalier <- config$paths$somalier
  if (!file.exists(somalier)) {
    warning("Somalier file path is invalid. Setting somalier to NULL.")
    somalier <- NULL
  }
} else {
  warning("Somalier variable is not defined in YAML file. Setting somalier to NULL.")
  somalier <- NULL
}

################################################################################

# Check if file dependencies exist

required_paths <- list(
  snvs_vcf = snvs_vcf,
  svs_vcf = svs_vcf,
  #chain_hg38_to_chm13 = chain_hg38_to_chm13,
  panel_app = panel_app,
  vep_consequences = vep_consequences,
  phenotype_data = phenotype_data
)

for (name in names(required_paths)) {
  if (!file.exists(required_paths[[name]])) {
    stop(paste("Required file not found:", name, "at path:", required_paths[[name]]))
  }
}

################################################################################

# Function to process SNV data with error and trio handling
process_snv_data <- function(snvs_vcf, pedigree_data) {
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

  # Sort the trio samples
  trio <- sort(trio)

  # Remove the "#" character from column names
  setnames(snvs_data, old = names(snvs_data), new = gsub("#", "", names(snvs_data)))
  snvs_data[, ID := paste(CHROM, POS, REF, ALT, sep = "_")]

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
  snvs_data_melt <- snvs_data_melt[FORMAT %in% c("GT", "AD", "DP")]

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

  snvs_data_melt <- cbind(snvs_data_melt[,.(ID,CATEGORY="SNV & Indel",VAR_TYPE=ifelse(nchar(REF) == nchar(ALT),"SNV",ifelse(nchar(REF) > nchar(ALT),"Deletion","Insertion")),CHROM,POS,
                                            VAR_LENGTH=ifelse(nchar(REF) == nchar(ALT), 1, abs(nchar(REF) - nchar(ALT))),REF,ALT,QUAL,FILTER,GENE_ID=Gene,GENE_SYMBOL=SYMBOL,TRANSCRIPT=Feature,HGVSg,HGVSc,HGVSp,
                                            CONSEQUENCE=Consequence,CLINVAR=CLIN_SIG,AF=as.numeric(gnomAD_AF_joint),N_HOM_ALT=as.numeric(gnomAD_nhomalt_joint),SIFT,PolyPhen,REVEL,am_class,am_pathogenicity,CADD_PHRED,CADD_RAW,SpliceAI_pred,Donor_Loss=as.numeric(SpliceAI_pred_DS_DL),Donor_Gain=as.numeric(SpliceAI_pred_DS_DG),Acceptor_Loss=as.numeric(SpliceAI_pred_DS_AL),Acceptor_Gain=as.numeric(SpliceAI_pred_DS_AG))],
                          snvs_data_melt[,c(ad_cols,dp_cols,vaf_cols,gt_cols),with=FALSE])

  snvs_data_melt[,CLINVAR:=str_replace_all(CLINVAR,"&",",")]
  snvs_data_melt[CLINVAR=="",CLINVAR:=NA]

  # Count the number of alt alleles
  for (col_name in names(snvs_data_melt)[grepl("GT_",names(snvs_data_melt))]) {
    new_col_name <- paste0("alt_allele_count_", tstrsplit(col_name,"_",fixed=TRUE)[[2]])
    snvs_data_melt[, (new_col_name) := rowSums(sapply(.SD, function(x) 2 - (str_count(x, "0")+str_count(x, "\\.")))), .SDcols = col_name]
  }

  # Final output
  cat("SNV data processed.\n")
  return(snvs_data_melt[QUAL>=30])
}

################################################################################

# Main execution
snv_data <- process_snv_data(snvs_vcf,pedigree_data)

# Define sample id
sample <- pedigree_data[1,sample_id]

# Define pre-selected variables
preselected_vars <- c("ID","CHROM","POS","GT_1")

# PanelApp genes
panel_app <- fread(panel_app,header=TRUE)
panel_app_genes <- panel_app[,c(1,4,5,8)]
panel_app_genes[, Sources := str_extract(Sources, "Expert Review ([[:alnum:].]+)")]
panel_app_genes[,Sources:= str_remove(Sources,"Expert Review ")]
panel_app_genes[,Model_Of_Inheritance:=tstrsplit(Model_Of_Inheritance,",")[1]]
panel_app_genes <- as.data.table(panel_app_genes)
panel_app_vars <- c("Gene_Symbol","Sources","Level4","Level2","Model_Of_Inheritance")

# VEP consequences
vep_consequences <- fread(vep_consequences,header=TRUE)

# Phenotype
phenotype_data <- fread(phenotype_data,header=TRUE)

# Save specific objects into an .RData file
processed_data <- rbind(snv_data)

save(sample, processed_data, pedigree_data, panel_app_genes, coverage_data, somalier,
     vep_consequences, preselected_vars, panel_app, panel_app_vars, snvs_vcf, svs_vcf, bam_files, phenotype_data, file = rdata_output)
