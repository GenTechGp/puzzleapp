#.libPaths(c("/g/data/kr68/andre/R_libs",.libPaths()))

# Convert VCF to ensembl
convert_to_ensembl <- function(chromosome, position, ref, alt) {
  # Assuming chromosome is in the format "20"
  # Construct the Ensembl-style representation
  #chromosome <- gsub("^chr", "", chromosome)
  if (nchar(ref) == 1 && nchar(alt) == 1) {
    # Substitution
    return(paste0(chromosome, "_", position, "_", ref, "/", alt))
  } else if (nchar(ref) > nchar(alt)) {
    # Deletion
    return(paste0(chromosome, "_", position + 1,'_', substr(ref,nchar(alt)+1,nchar(ref)), "/-"))
  } else if (nchar(ref) < nchar(alt)) {
    # Insertion
    return(paste0(chromosome, "_", position+1,"_","-/", substr(alt,nchar(ref)+1,nchar(alt))))
  } else {
    # Handle other cases as needed
    return("Unsupported variant type")
  }
}
 
# Get the path to the latest VCF file
snvs_vcf <- system(sprintf("ls -tr %s/*.snp_indel.phased.vcf.gz | tail -n1", pipeface_dir), intern = TRUE)

# Read the VCF file into a data.table
snvs_data <- fread(cmd = paste("gunzip -c", snvs_vcf), sep = "\t", skip = "#CHROM", header = TRUE)

snvs_data <- unique(snvs_data)

# Remove the "#" character from column names
setnames(snvs_data, old = names(snvs_data), new = gsub("#", "", names(snvs_data)))
snvs_data[, ID := paste(CHROM, POS, REF, ALT, sep = "_")]

# Define the trio samples and sort them
trio <- pedigree_data$sample_id
trio <- sort(trio)

# Melt the data.table based on the trio samples
variables <- names(snvs_data)
snvs_data_melt <- melt(snvs_data, id.vars = variables[!(variables %in% trio)], variable.name = "Sample", value.name = "Value")


# Measure the number of fields in each row of the FORMAT column
snvs_data_melt[, num_fields := lengths(strsplit(FORMAT, ":"))]

# Determine the maximum number of fields across all rows
max_fields <- max(snvs_data_melt$num_fields)

snvs_data_melt[, FORMAT := ifelse(max_fields - num_fields > 0,
  paste0(FORMAT, strrep(":NA", max_fields - num_fields)),
  FORMAT
)]

snvs_data_melt[, Value := ifelse(max_fields - num_fields > 0,
                                  paste0(Value, strrep(":NA", max_fields - num_fields)),
                                  Value
)]

# Split the FORMAT and Value columns and merge back into the data.table
format_splits <- snvs_data_melt[, tstrsplit(FORMAT, ":", fixed = TRUE)]
format_splits <- melt(cbind(snvs_data_melt[, .(ID, Sample)],format_splits),id.vars=c("ID","Sample"))[,.(ID,Sample,FORMAT=value)]
value_splits <- snvs_data_melt[, tstrsplit(Value, ":", fixed = TRUE)]
value_splits <- melt(cbind(snvs_data_melt[, .(ID, Sample)],value_splits),id.vars=c("ID","Sample"))[,.(ID,Sample,Value=value)]
id_vars <- names(snvs_data_melt)
snvs_data_melt <- merge(snvs_data_melt[,setdiff(id_vars,c("FORMAT","Value")),with=FALSE],cbind(format_splits,value_splits[,.(Value)]),by=c("ID","Sample"))
snvs_data_melt <- snvs_data_melt[FORMAT!="NA"]
snvs_data_melt[,num_fields:=NULL]

# Remove the intermediate objects to free up memory
rm(format_splits, value_splits,snvs_data)
gc()

# Create a data.table for the trio sample codes
trio <- data.table(Sample = trio, Code = seq(1, length(trio)))

# Merge consequence_data_melt with the trio sample codes
snvs_data_melt <- merge(snvs_data_melt, trio, by = "Sample")

# Filter rows to keep only GT, AD, and DP
snvs_data_melt <- snvs_data_melt[FORMAT %in% c("GT", "AD", "DP")]

# Get number of reads supporting the alternate allele
snvs_data_melt[FORMAT=="AD",Value:=tstrsplit(Value,",",fixed=TRUE)[2]]

# Convert FORMAT to character if it's not already
snvs_data_melt[, FORMAT := as.character(FORMAT)]

# Efficiently add sample codes to FORMAT column
snvs_data_melt[, FORMAT := paste0(FORMAT, "_", Code)]

# Identify false het variants that need to be converted to homozygous (Needs to be dealt with)
false_het_variants <- snvs_data_melt[,.N,by=ID][N>9,ID]

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

# Convert VCF to ensembl
#snvs_data_melt[, Uploaded_variation := ID]
snvs_data_melt[,Uploaded_variation:=convert_to_ensembl(CHROM,POS,toupper(REF),toupper(ALT)),by=ID]
snvs_data_melt

# Open VEP output
file_basename <- basename(snvs_vcf)
annotation_dir <- system(sprintf("ls -dtr %s/%s/annotator-* | tail -n1",project_dir,sample), intern = TRUE)
annotator_settings_files <- system(sprintf("grep %s %s/*/annotator_settings.txt | cut -d ':' -f1", file_basename, annotation_dir), intern = TRUE)
annotator_settings_files <- dirname(annotator_settings_files)
annotator_files <- sapply(annotator_settings_files, function(dir) system(sprintf("ls %s", file.path(dir, "*vep.annotated.tsv.gz")), intern = TRUE))
# Check which annotator files contain the pattern '--everything'
contains_everything <- sapply(annotator_files, function(file) {
  # Use zgrep to search for the pattern '--everything' in the gzipped file
  result <- system(sprintf("zgrep 'everything' %s", file), intern = TRUE)
  
  # Return TRUE if the pattern is found, FALSE otherwise
  return(length(result) > 0)
})
# Display the annotator files that contain the '--everything' pattern
annotator_files <- annotator_files[contains_everything]
annotator_files_info <- file.info(annotator_files)
annotator_files <- annotator_files[order(annotator_files_info$ctime)]
annotator_file <- annotator_files[length(annotator_files)]

vep_output <- system(sprintf("ls -tr %s",annotator_file), intern = TRUE)
vep_data <- unique(fread(vep_output, skip = "## VEP command-line:", header = TRUE, na.strings = "-"))
setnames(vep_data, old = names(vep_data), new = gsub("#", "", names(vep_data)))

# Open Clinvar links
# clinvar <- system(sprintf("ls -tr %s/References/hg38/clinvar.url.tab", app_dir), intern = TRUE)
# clinvar <- fread(clinvar, header=TRUE)
# clinvar[,Uploaded_variation:=ID]
# clinvar[,ID:=NULL]
# 
# vep_data <- merge(vep_data,clinvar,by="Uploaded_variation",all.x=TRUE)


# Merge joint call and VEP output
snvs_data_melt <- merge(snvs_data_melt,vep_data,by="Uploaded_variation")

num_samples <- dim(pedigree_data)[1]
# Generate the column names for AD, DP, VAF, and GT
ad_cols <- paste0("AD_", 1:num_samples)
dp_cols <- paste0("DP_", 1:num_samples)
vaf_cols <- paste0("VAF_", 1:num_samples)
gt_cols <- paste0("GT_", 1:num_samples)

snvs_data_melt <- cbind(snvs_data_melt[,.(ID,CATEGORY="SNV & Indel",VAR_TYPE=ifelse(nchar(REF) == nchar(ALT),"SNV",ifelse(nchar(REF) > nchar(ALT),"Deletion","Insertion")),CHROM,POS,
                                           VAR_LENGTH=ifelse(nchar(REF) == nchar(ALT), 1, abs(nchar(REF) - nchar(ALT))),REF,ALT,QUAL,FILTER,GENE_ID=Gene,GENE_SYMBOL=SYMBOL,TRANSCRIPT=Feature,
                                           CONSEQUENCE=Consequence,CLINVAR=CLIN_SIG,AF=gnomADv4.1_AF,N_HOM_ALT=gnomADv4.1_nhomalt,SIFT,PolyPhen,REVEL,CADD_PHRED,CADD_RAW,SpliceAI_pred,SpliceAI_split=SpliceAI_pred)],
                        snvs_data_melt[,c(ad_cols,dp_cols,vaf_cols,gt_cols),with=FALSE])

# Remove the intermediate objects to free up memory
rm(vep_data)
gc()

# Split the SpliceAI_pred column
snvs_data_melt <- data.table(snvs_data_melt %>%
                                    separate(SpliceAI_split, into = c("Gene", "Donor_Loss", "Donor_Gain", "Acceptor_Loss", "Acceptor_Gain", "Pos1", "Pos2", "Pos3", "Pos4"), sep = "\\|", convert = TRUE) %>%
                                    select(-Gene, -Pos1, -Pos2, -Pos3, -Pos4))  # Remove columns that are not needed

# Count the number of alt alleles
for (col_name in names(snvs_data_melt)[grepl("GT_",names(snvs_data_melt))]) {
  new_col_name <- paste0("alt_allele_count_", tstrsplit(col_name,"_",fixed=TRUE)[[2]])
  snvs_data_melt[, (new_col_name) := rowSums(sapply(.SD, function(x) 2 - (str_count(x, "0")+str_count(x, "\\.")))), .SDcols = col_name]
}

rename_object <- function(old_name, new_name) {
  if (exists(old_name, envir = .GlobalEnv)) {
    .GlobalEnv[[new_name]] <- .GlobalEnv[[old_name]]
    rm(list = old_name, envir = .GlobalEnv)
  } else {
    warning(paste("Object", old_name, "does not exist in the global environment."))
  }
}

rename_object("snvs_data_melt", "snvs_processed_data")

# Reorder columns by moving "ClinVar" to the end
snvs_processed_data <- snvs_processed_data[, .SD, .SDcols = c(setdiff(names(snvs_processed_data), "ClinVar"))]

snvs_processed_data[, GNOMADv4 := paste0(str_remove(CHROM, 'chr'), '-', POS, '-', REF, '-', ALT)]
snvs_processed_data[,GNOMADv4 := sprintf("https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4",GNOMADv4)]
snvs_processed_data[, GNOMADv4 := paste0("<a href='",GNOMADv4,"'>",GNOMADv4,"</a>")]

#write.table(snvs_data_melt,sprintf("%s/Variant_calls/deepvariant_deeptrio/1.6.0/RDN0214-00_combined_PB/20240620/%s",project_dir,"RDN0214.trio.processed_snv_data.tab"),quote=FALSE,sep="\t")

#snvs_processed_data <- fread(sprintf("%s/Variant_calls/deepvariant_deeptrio/1.6.0/RDN0214-00_combined_PB/20240620/%s",project_dir,"RDN0214.trio.processed_snv_data.v2.tab"),header=TRUE)

