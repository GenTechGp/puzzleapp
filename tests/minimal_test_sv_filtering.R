#library(puzzleapp)  # or whatever the package name is

config_yaml  <- "/g/data/kr68/andre/SVlog/LRS00341_20251209/LRS00341.config.sniffles.yaml"
filter_table <- "/g/data/kr68/andre/SVlog/LRS00443_20251209/f1.tsv"

cfg   <- yaml::read_yaml(config_yaml)
dep   <- cfg$dependencies

# 1) Pedigree
samples_list <- lapply(cfg$samples, function(s) list(
  sample_id = s$sample_id,
  kinship   = if (is.null(s$kinship) || s$kinship == "NA") "unknown" else s$kinship,
  status    = if (is.null(s$status)  || s$status  == "NA") "unknown" else s$status,
  sex       = if (is.null(s$sex)     || s$sex     == "NA") "unknown" else s$sex,
  code      = s$code,
  bam       = s$bam,
  coverage  = s$coverage
))
pedigree <- data.table::rbindlist(lapply(samples_list, as.data.table), fill = TRUE)

# 2) Dependencies
panel_app_genes  <- puzzlecore_load_panel_app_data(dep$panel_app)
vep_consequences <- puzzlecore_load_vep_consequences(dep$vep_consequences)
phenotype_data   <- puzzlecore_load_phenotype_data(dep$phenotype_data)

# 3) Filters (SV only)
filters <- .parse_filter_table(filter_table)
filters_sv <- filters$sv_filters

# 4) Allele counts (still needed, even for SV)
allele_counts <- puzzlecore_compute_allele_table(
  samples_list,
  filters$snv_filters$inheritance_filter
)
allele_counts_dt <- data.table::data.table(
  sample_id    = names(allele_counts),
  allele_count = unlist(allele_counts, use.names = FALSE)
)

# 5) Load SV TSV directly
sv_path <- cfg$paths$svs_tsv
sv_dt   <- puzzlecore_read_variant_tsv(sv_path, nthreads = 4)

# 6) Load SVlog database TSV
svlog_db <- cfg$paths$svlog_db
svlog_db   <- fread(svlog_db)

# 7) Run *just* the SV filter
sv_filtered <- puzzlecore_variant_filter(
  data             = sv_dt,
  filters          = filters_sv,
  pedigree         = pedigree,
  allele_tab       = allele_counts_dt,
  panel_app_genes  = panel_app_genes,
  vep_consequences = vep_consequences,
  phenotype_data   = phenotype_data,
  is_snv           = FALSE,
  svlog_db         = svlog_db
)