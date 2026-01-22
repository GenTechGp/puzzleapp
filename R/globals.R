utils::globalVariables(c(
  # Existing
  ".", ".I", ".N", ".SD", ".row_id",
  "Entity_Name", "GENE_SYMBOL", "GT_1", "GT_2", "GT_3",
  "HPO_COUNT", "HPO_ID", "ID", "INHERITANCE", "Level4",
  "Model_Of_Inheritance", "NOTES", "PANEL_APP", "PRIORITY",
  "PRIORITYFlag", "Sources", "VAR_COUNT", "VAR_COUNT_1",
  "VAR_COUNT_2", "alt_allele_count_1", "clinvar_override",
  "consequence", "gene_symbol", "hpo_id", "i.HPO_COUNT",
  "i.HPO_ID", "i.INHERITANCE", "i.NOTES", "i.PANEL_APP",
  "i.PRIORITY", "kinship", "sample_id", "spliceai_override",
  "term", "value",

  # New (from R CMD check NOTE)
  "V1", "sort_order", "VariantID", "CHROM", "POS", "REF", "ALT",
  "num_fields", "FORMAT", "Value", "Sample", "Code", "CSQ", "INFO",
  "SpliceAI_pred", "SpliceAI_pred_SYMBOL", "SpliceAI_pred_DS_DL",
  "SpliceAI_pred_DS_DG", "SpliceAI_pred_DS_AL", "SpliceAI_pred_DS_AG",
  "SpliceAI_pred_DP_AG", "SpliceAI_pred_DP_AL", "SpliceAI_pred_DP_DG",
  "SpliceAI_pred_DP_DL", "GNOMADv4", "AF", "gnomAD_AF_joint",
  "gnomAD_ID", "ClinVar", "CLINVAR_ID", "N_Cohort", "fam", "QUAL",
  "FILTER", "Gene", "SYMBOL", "Feature", "HGVSg", "HGVSc", "HGVSp",
  "Consequence", "CLIN_SIG", "gnomAD_nhomalt_joint", "SIFT",
  "PolyPhen", "REVEL", "am_class", "am_pathogenicity", "CADD_PHRED",
  "CADD_RAW", "CLINVAR", "Genotype", "VariantIndex", "GT",
  "AlleleCount", "SVTYPE", "SVLEN", "gnomAD_sv_AF", "gnomAD_sv",
  "gnomAD_sv_N_HOMALT", "IDLIST", "IDLIST_VEC", "VEP_CONSEQUENCE", "VAR_TYPE", "CATEGORY",


  "has_suffix", "default_type", "HET", "HOM", "svlog_id", "SRC", "gnomAD_pass",
  "gnomAD_AF_max", "ont1000g_pass", "ont1000g_max_carriers", "NUM_FAMS",
  "internal_pass", "CLNSIG", "clinvar_pass", "clinvar_labels", "pass_svlog",
  "internal_max_carriers", "internal_max_families", "RM_CLASSIFICATION",
  "RM_RECIPROCAL", "RM_TOTAL_SV_COVERAGE", "TRF_CLASSIFICATION", "TRF_SV_COVERAGE",
  "TRF_PERIOD_SIZE", "TRF_COPY_NUMBER", "TRF_TOTAL_SV_COVERAGE", "CONSENSUS_REPEAT", "FINAL_CLASSIFICATION",
  "..keep_final", "name", "STRING", "carriers", "INTRON_LENGTH", "..snv_colnames", "..sv_colnames",

  "SAMPLE",
  "AVERAGE_COVERAGE",
  "avg_auto",
  "variable",
  "width"
))
