# Get command-line arguments
args <- commandArgs(trailingOnly = TRUE)

.libPaths(c("/g/data/kr68/andre/R_libs",.libPaths()))

# Load the necessary library
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

# Assign the sample name from the arguments
#sample <- args[1]
sample <- "RDN0312-00_combined_PB"

start_time <- Sys.time()  # Record the start time

# Define the project directory and sample
project_dir <- '/g/data/kr68/andre/shinyApp'

# Load scripts to process the data
cat("Loading LoadSupportingData.R script...\n")
source(sprintf("%s/Modules/LoadSupportingData.R",project_dir))
cat("Finished LoadSupportingData.R script.\n")
cat("Loading Process_SNV_data.R script...\n")
source(sprintf("%s/Modules/Process_SNV_data.R",project_dir))
cat("Finished Process_SNV_data.R script.\n")
cat("Loading Process_SV_data.R script...\n")
source(sprintf("%s/Modules/Process_SV_data.R",project_dir))
cat("Finished Process_SV_data.R script.\n")

processed_data <- rbind(as.data.frame(snvs_processed_data),as.data.frame(svs_processed_data))

# Create the directory if it doesn't exist
output_dir <- sprintf("%s/Trios/%s", project_dir, sample)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save specific objects into an .RData file
save(sample, project_dir, processed_data, pedigree_data, panel_app_genes,
     vep_consequences, preselected_vars, panel_app, panel_app_vars, snvs_vcf, svs_vcf, bam_files, chain_hg38_to_chm13,phenotype_data,coverage_data,somalier, file = sprintf("%s/Trios/%s/%s.RData",project_dir,sample,sample))

#rm(sample, snvs_processed_data, svs_processed_data, pedigree_data, panel_app_genes,vep_consequences, preselected_vars, panel_app, panel_app_vars, snvs_vcf, svs_vcf, bam_files,chain_hg38_to_chm13,phenotype_data,coverage_data)

end_time <- Sys.time()  # Record the end time
elapsed_time <- end_time - start_time  # Calculate the elapsed time

print(paste("Elapsed time:", elapsed_time))
