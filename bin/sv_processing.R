#!/usr/bin/env Rscript

#.libPaths(c("/g/data/kr68/andre/R_libs"))
.libPaths(c("/g/data/kr68/andre/R_libs_yaml"))

suppressMessages(library(optparse))

source("/g/data/kr68/andre/puzzleapp/bin/processing_functions.R")  # Load common functions

# Define command-line options
option_list <- list(
  make_option("--vcf", type = "character", help = "Path to the VCF file", metavar = "file"),
  make_option("--sample_ids", type = "character", help = "Comma-separated list of sample IDs", metavar = "string"),
  make_option("--kinship_labels", type = "character", help = "Comma-separated list of corresponding kinship labels", metavar = "string"),
  make_option("--output", type = "character", help = "Path to save the output file", metavar = "file"),
  make_option("--vcf_cohort", type = "character", default = NA, help = "Optional path to cohort VCF file", metavar = "file")
)

# Parse command-line arguments
opt <- parse_args(OptionParser(option_list = option_list))

# Validate required inputs
if (is.null(opt$vcf) || is.null(opt$sample_ids) || is.null(opt$kinship_labels) || is.null(opt$output)) {
  stop("Error: --vcf, --sample_ids, --kinship_labels, and --output arguments are required.\n", call. = FALSE)
}

# Split comma-separated lists
sample_ids <- strsplit(opt$sample_ids, ",")[[1]]
kinships <- strsplit(opt$kinship_labels, ",")[[1]]

if (length(sample_ids) != length(kinships)) {
  stop("Error: Number of sample IDs and kinships must match.\n", call. = FALSE)
}

# Create pedigree data.frame
pedigree_data <- data.table(sample_id = sample_ids, kinship = kinships)

# Process the SNV data
processed_data <- process_sv_data(opt$vcf, pedigree_data, svs_vcf_cohort = opt$vcf_cohort)

# Save the output
fwrite(processed_data, opt$output,  sep = "\t", quote = FALSE, na = "NA")

cat("SNV data processed and saved to:", opt$output, "\n")
