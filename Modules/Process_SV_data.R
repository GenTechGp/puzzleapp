#.libPaths(c("/g/data/kr68/andre/R_libs",.libPaths()))

# Get the path to the latest VCF file
svs_vcf <- system(sprintf("ls -tr %s/*.sv.phased.vcf.gz | tail -n1", pipeface_dir), intern = TRUE)

# # Read the VCF file into a data.table
# svs_data <- fread(cmd = paste("gunzip -c", svs_vcf), sep = "\t", skip = "#CHROM", header = TRUE)
# 
# # Remove the "#" character from column names
# setnames(svs_data, old = names(svs_data), new = gsub("#", "", names(svs_data)))
# 
# # Split the INFO column by ";"
# svs_data[, INFO_split := strsplit(INFO, ";")]
# 
# # Get the names of all columns except INFO_split
# id_vars <- setdiff(names(svs_data), c("INFO_split"))
# 
# # Define the trio samples and sort them
# trio <- pedigree_data$sample_id
# trio <- sort(trio)
# 
# # Melt the data.table to long format based on INFO_split
# melted_data <- svs_data[, .(INFO_field = unlist(INFO_split)), by = id_vars]
# 
# # Filter rows that start with "PREDICTED"
# melted_data <- melted_data[grepl("^PREDICTED", INFO_field)]
# 
# # Concatenate PREDICTED fields into a single CONSEQUENCE field
# consequence_data <- melted_data[, .(CONSEQUENCE = paste(INFO_field, collapse = ";")), by = id_vars]
# 
# # Extract SVTYPE and SVLEN
# consequence_data[,SVTYPE:=str_remove(str_extract(INFO, "SVTYPE=[^;]+"),"SVTYPE=")]
# consequence_data[,SVLEN:=abs(as.numeric(str_remove(str_extract(INFO, "SVLEN=[^;]+"),"SVLEN=")))]
# 
# # Define fixed columns and create the new column order
# fixed_cols <- setdiff(names(consequence_data), trio)
# new_order <- c(fixed_cols, trio)
# setcolorder(consequence_data, new_order)
# 
# # Melt the data.table based on the trio samples
# variables <- names(consequence_data)
# consequence_data_melt <- data.table(melt(consequence_data, id.vars = variables[!(variables %in% trio)], variable.name = "Sample", value.name = "Value"))
# 
# # Convert Sample column to character type
# consequence_data_melt$Sample <- as.character(consequence_data_melt$Sample)
# 
# # Split the FORMAT and Value columns and merge back into the data.table
# consequence_data_melt <- merge(
#   consequence_data_melt[, c(names(consequence_data_melt)[!names(consequence_data_melt) %in% c("FORMAT", "Value")]), with = FALSE],
#   consequence_data_melt[, .(FORMAT = unlist(strsplit(FORMAT, ":", fixed = TRUE)), Value = unlist(strsplit(Value, ":", fixed = TRUE))), by = list(ID, Sample)],
#   by = c("ID", "Sample")
# )
# 
# # Create a data.table for the trio sample codes
# trio <- data.table(Sample = trio, Code = seq(1, length(trio)))
# 
# # Merge consequence_data_melt with the trio sample codes
# consequence_data_melt <- merge(consequence_data_melt, trio, by = "Sample")
# 
# # Calculate DP as the sum of DR and DV values
# dp_column <- data.table(melt(consequence_data_melt[FORMAT %in% c("DR", "DV"), .(DP = sum(as.numeric(Value))), by = list(ID, Sample)], id.vars = c("ID", "Sample"), variable.name = "FORMAT", value.name = "Value"))
# 
# # Get the names of all columns except FORMAT and Value
# selected_columns <- setdiff(names(consequence_data_melt), c("FORMAT", "Value"))
# 
# # Merge DP values back into the consequence_data_melt
# dp_column <- merge(unique(consequence_data_melt[, ..selected_columns]), dp_column, by = c("Sample", "ID"))
# 
# # Append DP rows to consequence_data_melt
# consequence_data_melt <- rbind(consequence_data_melt, dp_column)
# 
# # Rename DV to AD
# consequence_data_melt[FORMAT == "DV", FORMAT := "AD"]
# 
# # Filter rows to keep only GT, AD, and DP
# consequence_data_melt <- consequence_data_melt[FORMAT %in% c("GT", "AD", "DP")]
# 
# # Add sample codes to FORMAT column
# consequence_data_melt$FORMAT <- as.character(consequence_data_melt$FORMAT)
# consequence_data_melt[, FORMAT := paste(c(FORMAT, Code), collapse = "_"), by = 1:nrow(consequence_data_melt)]
# 
# # Cast the melted data back to wide format
# formula <- paste(names(consequence_data_melt)[!names(consequence_data_melt) %in% c("FORMAT", "Value", "Sample", "Code")], collapse = " + ")
# formula <- sprintf("%s ~ FORMAT", formula)
# consequence_data_dcast <- data.table(dcast(consequence_data_melt, formula, value.var = "Value"))
# 
# id_vars <- names(consequence_data_dcast)
# ad_vars <- grep("^AD_", id_vars, value = TRUE)
# dp_vars <- grep("^DP_", id_vars, value = TRUE)
# vaf_vars <- gsub("AD","VAF",ad_vars)
# # Calculate the VAF values and assign to the data.table
# for (i in seq_along(ad_vars)) {
#   consequence_data_dcast[, (vaf_vars[i]) := round(as.integer(get(ad_vars[i])) / as.integer(get(dp_vars[i])),2)]
# }
# 
# # Finalize the processed data with additional columns
# svs_processed_data <- consequence_data_dcast[, .(
#   ID, CATEGORY = "SV",VAR_TYPE=SVTYPE, CHROM, POS,VAR_LENGTH=SVLEN, REF, ALT, QUAL, FILTER, GENE_ID = NA, GENE_SYMBOL = NA, TRANSCRIPT = NA,
#   CONSEQUENCE, CLINVAR = NA, AF = NA, SIFT = NA, PolyPhen = NA, REVEL = NA,SpliceAI_pred=NA,Donor_Loss=NA,Donor_Gain=NA,Acceptor_Loss=NA,Acceptor_Gain=NA,
#   AD_1, AD_2, AD_3, DP_1, DP_2, DP_3, VAF_1, VAF_2, VAF_3, GT_1, GT_2, GT_3
# )]
# 
# # Function to extract gene names from CONSEQUENCE string
# extract_genes <- function(consequence) {
#   # Split the consequence string by ";"
#   key_value_pairs <- strsplit(consequence, ";")[[1]]
#   # Extract gene names only from pairs containing "="
#   genes <- sapply(key_value_pairs, function(pair) {
#     if (grepl("=", pair)) {
#       gsub(".*=", "", pair)
#     } else {
#       NA  # Return NA for pairs without "="
#     }
#   })
#   # Remove NA values from the genes vector
#   genes <- genes[!is.na(genes)]
#   # Concatenate the gene names into a comma-separated list
#   paste(genes, collapse = ",")
# }
# 
# # Extract gene symbols from GATK consequences
# svs_processed_data[, GENE_SYMBOL := sapply(CONSEQUENCE, extract_genes)]
# 
# # Gene name, gene id and transcript id dictionary
# gene_name_dict <- system(sprintf("ls -tr %s/References/hg38/MANE.GRCh38.v1.3.ensembl_genomic.transcript_gene_names.tab", project_dir), intern = TRUE)
# gene_name_dict <- fread(gene_name_dict, header=FALSE)
# setnames(gene_name_dict, old = names(gene_name_dict), new = c("transcript_id","gene_id","gene_name"))
# 
# # Function to map gene names to gene IDs
# map_gene_names_to_ids <- function(gene_names, dict, map_to = "gene_id") {
#   gene_list <- unlist(strsplit(gene_names, ","))
#   if (map_to == "gene_id") {
#     ids <- dict[gene_name %in% gene_list, unique(gene_id)]
#   } else if (map_to == "transcript_id") {
#     ids <- dict[gene_name %in% gene_list, unique(transcript_id)]
#   } else {
#     stop("Invalid mapping option. Choose 'gene_id' or 'transcript_id'.")
#   }
#   paste(ids, collapse = ",")
# }
# 
# svs_processed_data[, GENE_ID := sapply(GENE_SYMBOL, map_gene_names_to_ids, dict = gene_name_dict, map_to = "gene_id")]
# svs_processed_data[, TRANSCRIPT := sapply(GENE_SYMBOL, map_gene_names_to_ids, dict = gene_name_dict, map_to = "transcript_id")]
# 
# # Count the number of alt alleles
# for (col_name in names(svs_processed_data)[grepl("GT_",names(svs_processed_data))]) {
#   new_col_name <- paste0("alt_allele_count_", tstrsplit(col_name,"_",fixed=TRUE)[[2]])
#   svs_processed_data[, (new_col_name) := rowSums(sapply(.SD, function(x) 2 - (str_count(x, "0")+str_count(x, "\\.")))), .SDcols = col_name]
# }
# 
# svs_processed_data[,ClinVar:=NA]
# svs_processed_data[,GNOMADv4:=NA]
# 
# #write.table(svs_processed_data,sprintf("%s/Variant_calls/jasmine/1.1.4/RDN0214-00_combined_PB/20240621//%s",project_dir,"RDN0214.trio.processed_sv_data.tab"),quote=FALSE,sep="\t")
# 
# #svs_processed_data <- fread(sprintf("%s/Variant_calls/jasmine/1.1.4/RDN0214-00_combined_PB/20240621//%s",project_dir,"RDN0214.trio.processed_sv_data.v2.tab"),header=TRUE)
