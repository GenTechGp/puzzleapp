.libPaths(c("/g/data/kr68/andre/R_libs",.libPaths()))

# Trio information
pedigree_data <- data.table(sample_id=sample,kinship="proband",status="affected",sex=NA,code=1)

# Path to BAM files
bam_files <- lapply(pedigree_data[order(code),sample_id],function(x) sprintf("%s/%s.hg38.minimap2.whatshap.sorted.haplotagged.bam",pipeface_dir,x))

# Path to Coverage Data
# coverage_data <- lapply(pedigree_data[order(code),sample_id],function(x) sprintf("%s/%s.hg38.depth.tsv",pipeface_dir,x))
# coverage_data <- rbindlist(lapply(coverage_data,function(x) fread(x,header=TRUE)))
# setnames(coverage_data, old = names(coverage_data), new =c("SAMPLE","CHROM","NUM_BASES","SIZE","AVERAGE_COVERAGE"))

# Liftover chain files
chain_hg38_to_chm13 <- sprintf("%s/References/hg38/hg38-chm13v2.over.chain",project_dir)

# Pre-selected variables
preselected_vars <- c("ID","CATEGORY","VAR_TYPE","CHROM","POS","GENE_SYMBOL","CONSEQUENCE","CLINVAR","AF","GT_1")

# PanelApp genes
panel_app <- system(sprintf("ls -tr %s/Annotations/all_panel_app.tsv",app_dir), intern = TRUE)
panel_app <- fread(panel_app,header=TRUE)
panel_app_genes <- panel_app[,c(1,4,5,8)]
panel_app_genes[, Sources := str_extract(Sources, "Expert Review ([[:alnum:].]+)")]
panel_app_genes[,Sources:= str_remove(Sources,"Expert Review ")]
panel_app_genes[,Model_Of_Inheritance:=tstrsplit(Model_Of_Inheritance,",")[1]]
panel_app_genes <- as.data.table(panel_app_genes)
panel_app_vars <- c("Gene_Symbol","Sources","Level4","Level2","Model_Of_Inheritance")

# VEP consequences
vep_consequences <- system(sprintf("ls -tr %s/Annotations/vep_annotations.tsv",app_dir), intern = TRUE)
vep_consequences <- fread(vep_consequences,header=TRUE)

# Phenotype
phenotype_data <- system(sprintf("ls -tr %s/Annotations/phenotype_to_genes.txt",app_dir), intern = TRUE)
phenotype_data <- fread(phenotype_data,header=TRUE)

# Somalier analysis
# somalier <- system(sprintf("ls -tr %s/Variant_calls/deepvariant_deeptrio/*/%s/*/somalier/*.somalier.pairs.tsv | tail -n1", project_dir, sample), intern = TRUE)
# somalier <- fread(somalier,header=TRUE)
# setnames(somalier, old = names(somalier), new = gsub("#", "", names(somalier)))