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
sample <- "RBW401"
project <- "AMAMAL_PKD"

start_time <- Sys.time()  # Record the start time

# Define the project directory and sample
project_dir <- sprintf('/g/data/kr68/projects/%s',project)
pipeface_dir <- system(sprintf("ls -dtr %s/%s/pipeface-*/* | head -n1",project_dir,sample), intern = TRUE)

app_dir <- sprintf('/g/data/kr68/andre/%s',project)

# Load scripts to process the data
cat("Loading LoadSupportingData.R script...\n")
source(sprintf("%s/Modules/LoadSupportingData.R",app_dir))
cat("Finished LoadSupportingData.R script.\n")
cat("Loading Process_SNV_data.R script...\n")
source(sprintf("%s/Modules/Process_SNV_data.R",app_dir))
cat("Finished Process_SNV_data.R script.\n")
cat("Loading Process_SV_data.R script...\n")
source(sprintf("%s/Modules/Process_SV_data.R",app_dir))
#cat("Finished Process_SV_data.R script.\n")

#processed_data <- rbind(as.data.frame(snvs_processed_data),as.data.frame(svs_processed_data))
processed_data <- rbind(snvs_processed_data)

# Replace project_dir with app_dir in pipeface_dir
new_pipeface_dir <- gsub(project_dir, "/g/data/kr68/projects/APP_TEST", pipeface_dir)



project_dir <- "."

snvs_vcf <- "Data/RBW401.hg38.clair3.snp_indel.phased.vcf.gz"
svs_vcf <- "Data/RBW401.hg38.sniffles.sv.phased.vcf.gz"
bam_files[[1]] <- "Data/RBW401.hg38.minimap2.whatshap.sorted.haplotagged.bam"
new_pipeface_dir <- "App_Data/RBW401/pipeface-v0.1.0/20240821-210312"

# Create the directory recursively
dir.create(new_pipeface_dir, recursive = TRUE, showWarnings = FALSE)

chain_hg38_to_chm13 <- "References/hg38/hg38-chm13v2.over.chain"

# Save specific objects into an .RData file
save(sample, project_dir, processed_data, pedigree_data, panel_app_genes,
     vep_consequences, preselected_vars, panel_app, panel_app_vars, snvs_vcf, svs_vcf, bam_files, chain_hg38_to_chm13,phenotype_data, file = sprintf("%s/%s.RData",new_pipeface_dir,sample))

#rm(sample, snvs_processed_data, svs_processed_data, pedigree_data, panel_app_genes,vep_consequences, preselected_vars, panel_app, panel_app_vars, snvs_vcf, svs_vcf, bam_files,chain_hg38_to_chm13,phenotype_data,coverage_data)

end_time <- Sys.time()  # Record the end time
elapsed_time <- end_time - start_time  # Calculate the elapsed time

print(paste("Elapsed time:", elapsed_time))
