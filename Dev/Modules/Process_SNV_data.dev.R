#.libPaths(c("/g/data/kr68/andre/R_libs",.libPaths()))
 
# Get the path to the latest VCF file
snvs_vcf <- system(sprintf("ls -tr %s/Variant_calls/deepvariant_deeptrio/*/%s/*/*.*.trio.joint_call.phased.split.vcf.gz | tail -n1", project_dir, sample), intern = TRUE)

# Read the VCF file into a data.table
snvs_data <- fread(cmd = paste("gunzip -c", snvs_vcf), sep = "\t", skip = "#CHROM", header = TRUE)

snvs_data <- unique(snvs_data)

# Remove the "#" character from column names
setnames(snvs_data, old = names(snvs_data), new = gsub("#", "", names(snvs_data)))

# Define the trio samples and sort them
trio <- pedigree_data$sample_id
trio <- sort(trio)

# Melt the data.table based on the trio samples
variables <- names(snvs_data)
snvs_data_melt <- melt(snvs_data, id.vars = variables[!(variables %in% trio)], variable.name = "Sample", value.name = "Value")

# Split the FORMAT and Value columns and merge back into the data.table
snvs_data_melt[, FORMAT := sub("(GT:DP:AD:GQ):.*", "\\1", FORMAT)]
snvs_data_melt[, Value := sub("(.*?:.*?:.*?:.*?):.*", "\\1", Value)]
format_splits <- snvs_data_melt[, tstrsplit(FORMAT, ":", fixed = TRUE)]
format_splits <- melt(cbind(snvs_data_melt[, .(ID, Sample)],format_splits),id.vars=c("ID","Sample"))[,.(ID,Sample,FORMAT=value)]
value_splits <- snvs_data_melt[, tstrsplit(Value, ":", fixed = TRUE)]
value_splits <- melt(cbind(snvs_data_melt[, .(ID, Sample)],value_splits),id.vars=c("ID","Sample"))[,.(ID,Sample,Value=value)]
id_vars <- names(snvs_data_melt)
snvs_data_melt <- merge(snvs_data_melt[,setdiff(id_vars,c("FORMAT","Value")),with=FALSE],cbind(format_splits,value_splits[,.(Value)]),by=c("ID","Sample"))

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

# Convert VCF to ensembl
snvs_data_melt[, Uploaded_variation := ID]

# Open VEP output
vep_output <- system(sprintf("ls -tr %s/Variant_calls/deepvariant_deeptrio/*/%s/*/*.*.hg38.VEP.tab | tail -n1",project_dir,sample), intern = TRUE)
vep_data <- unique(fread(vep_output, skip = "## VEP command-line:", header = TRUE, na.strings = "-"))
setnames(vep_data, old = names(vep_data), new = gsub("#", "", names(vep_data)))

# Open Clinvar links
clinvar <- system(sprintf("ls -tr %s/References/hg38/clinvar.url.tab", project_dir), intern = TRUE)
clinvar <- fread(clinvar, header=TRUE)
clinvar[,Uploaded_variation:=ID]
clinvar[,ID:=NULL]

vep_data <- merge(vep_data,clinvar,by="Uploaded_variation",all.x=TRUE)


# Merge joint call and VEP output
snvs_data_melt <- merge(snvs_data_melt,vep_data,by="Uploaded_variation")

snvs_data_melt <- snvs_data_melt[,.(ID,CATEGORY="SNV & Indel",VAR_TYPE=ifelse(nchar(REF) == nchar(ALT),"SNV",ifelse(nchar(REF) > nchar(ALT),"Deletion","Insertion")),CHROM,POS,
                                           VAR_LENGTH=ifelse(nchar(REF) == nchar(ALT), 1, abs(nchar(REF) - nchar(ALT))),REF,ALT,QUAL,FILTER,GENE_ID=Gene,GENE_SYMBOL=SYMBOL,TRANSCRIPT=Feature,
                                           CONSEQUENCE=Consequence,CLINVAR=CLIN_SIG,AF=gnomADv4_AF,SIFT,PolyPhen,REVEL,SpliceAI_pred,SpliceAI_split=SpliceAI_pred,AD_1,AD_2,AD_3,DP_1,DP_2,DP_3,GT_1,GT_2,GT_3,ClinVar)]

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
snvs_processed_data <- snvs_processed_data[, .SD, .SDcols = c(setdiff(names(snvs_processed_data), "ClinVar"), "ClinVar")]

snvs_processed_data[, GNOMADv4 := paste0(str_remove(CHROM, 'chr'), '-', POS, '-', REF, '-', ALT)]
snvs_processed_data[,GNOMADv4 := sprintf("https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4",GNOMADv4)]
snvs_processed_data[, GNOMADv4 := paste0("<a href='",GNOMADv4,"'>",GNOMADv4,"</a>")]

#write.table(snvs_data_melt,sprintf("%s/Variant_calls/deepvariant_deeptrio/1.6.0/RDN0214-00_combined_PB/20240620/%s",project_dir,"RDN0214.trio.processed_snv_data.tab"),quote=FALSE,sep="\t")

#snvs_processed_data <- fread(sprintf("%s/Variant_calls/deepvariant_deeptrio/1.6.0/RDN0214-00_combined_PB/20240620/%s",project_dir,"RDN0214.trio.processed_snv_data.v2.tab"),header=TRUE)

