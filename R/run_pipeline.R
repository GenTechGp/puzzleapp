#' Headless pipeline runner for puzzleapp filtering
#'
#' Run the core variant filtering without Shiny using a YAML config and a
#' tab-delimited filter table (two columns: Key <tab> Value).
#'
#' This function uses the existing helpers from puzzlecore_* files where possible:
#' - puzzlecore_read_variant_tsv() to read variant TSVs
#' - puzzlecore_load_panel_app_data() to read PanelApp data
#' - puzzlecore_load_phenotype_data() to read phenotype mappings
#' - puzzlecore_load_vep_consequences() to read VEP consequences
#' - puzzlecore_variant_filter() to perform the filtering
#'
#' @param config_yaml Path to pipeline YAML (see pipeline_samples.yml schema)
#' @param filter_table Path to the two-column filters file (Key<TAB>Value)
#' @param output_dir Directory to write outputs (default: "pipeline_output")
#' @param nthreads Integer threads used for fread (default: 4)
#' @param verbose Logical for progress messages (default: TRUE)
#' @return Invisibly returns a list: list(snv_result = data.table|NULL, sv_result = data.table|NULL)
#' @export
run_pipeline <- function(
  config_yaml,
  filter_table,
  output_dir = "pipeline_output",
  nthreads = 4L,
  verbose = TRUE
) {
  use_headless_logging_lgr(layout = "%l [%t] %m", threshold = "info")

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Please install.packages('yaml').")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required. Please install.packages('data.table').")
  }

  stopifnot(file.exists(config_yaml), file.exists(filter_table))

  # Ensure output dir
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Read YAML config (samples + paths + dependencies)
  cfg <- yaml::read_yaml(config_yaml)

  # Pedigree from samples
  if (is.null(cfg$samples)) stop("Config YAML must contain 'samples:' list.")
  samples_list <- lapply(seq_along(cfg$samples), function(i) {
    s <- cfg$samples[[i]]
    # if "NA" then set to "unknown"
    list(
      sample_id = s$sample_id,
      kinship   = if (is.null(s$kinship) || s$kinship == "NA") "unknown" else s$kinship,
      status    = if (is.null(s$status) || s$status == "NA") "unknown" else s$status,
      sex       = if (is.null(s$sex) || s$sex == "NA") "unknown" else s$sex,
      code      = s$code,
      bam       = s$bam,
      coverage  = s$coverage
    )
  })
  issues <- puzzlecore_check_pedigree_sanity(samples_list)
  if (length(issues) > 0) {
    for (msg in issues) {
      cat("Pedigree issue:", msg, "\n")
    }
    stop("Pedigree validation failed.")
  }

  # Convert the list of samples into a data.table
  pedigree <- tryCatch({
    data.table::rbindlist(lapply(samples_list, as.data.table), fill = TRUE)
  }, error = function(e) {
    cat("Error processing pedigree data:", e$message, "\n")
    stop("Failed to process pedigree data.")
  })

  # Load dependencies via existing helpers where possible
  if (is.null(cfg$dependencies)) stop("Config YAML must contain 'dependencies:' block.")
  dep <- cfg$dependencies
  panel_app_genes   <- puzzlecore_load_panel_app_data(dep$panel_app)
  vep_consequences  <- puzzlecore_load_vep_consequences(dep$vep_consequences)
  phenotype_data    <- puzzlecore_load_phenotype_data(dep$phenotype_data)

  # Parse 2-column filter table (Key<TAB>Value) and map to filter lists
  filters       <- .parse_filter_table(filter_table)
  filters_snv   <- filters$snv_filters
  filters_sv    <- filters$sv_filters
  allele_counts <- puzzlecore_compute_allele_table(
    samples_list,
    filters_snv$inheritance_filter
  ) # named list
  allele_counts_dt <- data.table(
    sample_id    = names(allele_counts),
    allele_count = unlist(allele_counts, use.names = FALSE)
  )

  snv_path <- cfg$paths$snvs_tsv %||% ""
  sv_path  <- cfg$paths$svs_tsv  %||% ""
  if (!file.exists(snv_path)) {
    stop("SNV TSV file does not exist: ", snv_path)
  }
  if (!file.exists(sv_path)) {
    stop("SV TSV file does not exist: ", sv_path)
  }
  snv_data <- puzzlecore_read_variant_tsv(snv_path, nthreads = nthreads)
  sv_data <- puzzlecore_read_variant_tsv(sv_path, nthreads = nthreads)

  filtered_data <- puzzlecore_variant_filter(
    snv_data = snv_data,
    sv_data = sv_data,
    snv_filters = filters_snv,
    sv_filters = filters_sv,
    pedigree = pedigree,
    allele_tab = allele_counts_dt,
    panel_app_genes = panel_app_genes,
    vep_consequences = vep_consequences,
    phenotype_data = phenotype_data
  )
  result_snv <- filtered_data$snv
  result_sv <- filtered_data$sv
  if (verbose) message(sprintf("[pipeline] SNV filtered rows: %s", nrow(result_snv)))
  if (verbose) message(sprintf("[pipeline] SV filtered rows: %s", nrow(result_sv)))
  # cat("colnames SNV:", paste(colnames(result_snv), collapse = ", "), "\n")
  out_snv <- file.path(output_dir, "filtered_snv.tsv")
  write.table(result_snv, file = out_snv, sep = "\t", row.names = FALSE, quote = FALSE)
  if (verbose) message("[pipeline] Wrote: ", out_snv)
  out_sv <- file.path(output_dir, "filtered_sv.tsv")
  write.table(result_sv, file = out_sv, sep = "\t", row.names = FALSE, quote = FALSE)
  if (verbose) message("[pipeline] Wrote: ", out_sv)

  cat("Pipeline completed successfully.\n")
  invisible(list(
    snv_result = result_snv,
    sv_result = result_sv
  ))
}

# ---------- Internal helpers (not exported) ----------

# Two-column filter file parser (Key<TAB>Value).
.parse_filter_table <- function(path) {
  dt <- data.table::fread(path, header = FALSE, sep = "\t", data.table = TRUE)
  if (ncol(dt) < 2) stop("Filter file must have two columns: Key<TAB>Value")
  data.table::setnames(dt, c("key", "value"))
  dt[, key := trimws(key)]
  dt[, value := trimws(value)]

  # helpers
  get_first <- function(k) {
    v <- dt[key == k, value]
    if (length(v) == 0) return(NULL)
    v[1]
  }
  as_vec <- function(k) {
    v <- get_first(k)
    if (is.null(v) || !nzchar(v)) return(character(0))
    y <- unlist(strsplit(v, "[,;]+"))
    y <- trimws(y)
    y[nzchar(y)]
  }
  as_scalar_str <- function(k) {
    v <- get_first(k)
    if (is.null(v) || !nzchar(v)) return("")
    v
  }
  as_bool <- function(k, default = FALSE) {
    v <- get_first(k)
    if (is.null(v) || !nzchar(v)) return(default)
    tolower(v) %in% c("true", "1", "yes", "y")
  }
  as_num <- function(k) {
    v <- get_first(k)
    if (is.null(v) || !nzchar(v)) return(NULL)
    suppressWarnings({
      out <- as.numeric(v)
    })
    if (is.na(out)) return(NULL)
    out
  }

  snv_filters <- list(
    # Vectors -> character(0) when blank
    clinvar_filter                         = as_vec("SNV_Pathogenicity"),
    annotation_filter                      = as_vec("SNV_Annotation"),
    panelapp_filter                        = as_vec("PanelApp_Genes"),
    custom_genes                           = as_vec("Custom_Genes"),
    substract_panelapp_gene_lists_filter   = as_vec("Substract_PanelApp_Genes_Lists"),
    substract_panelapp_genes_filter        = as_vec("Substract_PanelApp_Genes"),
    hpo_terms_list                         = as_vec("HPO_Terms"),

    # Numerics -> NULL when blank
    af_value               = as_num("SNV_gnomADv4 AF"),
    revel_value            = as_num("SNV_REVEL"),
    spliceai_filter        = as_num("SNV_SpliceAI score"),
    genotype_quality_value = as_num("SNV_Genotype quality"),
    allele_balance_value   = as_num("SNV_Allele balance"),

    # Scalars (strings) guarded by nzchar() downstream
    sift_filter           = as_scalar_str("SNV_SIFT"),
    polyphen_filter       = as_scalar_str("SNV_PolyPhen"),
    inheritance_filter    = as_scalar_str("Inheritance"),

    # Booleans
    treat_negative              = as_bool("Treat_Negative", FALSE),
    affected_only              = as_bool("SNV_Affected only", FALSE),
    inheritance_panelapp_gene  = as_bool("Inheritance_PanelApp_Gene", FALSE)
  )

  sv_filters <- list(
    # Vectors -> character(0) when blank
    annotation_filter                      = as_vec("SV_Annotation"),
    sv_features                            = as_vec("SV_SV type"),
    panelapp_filter                        = as_vec("PanelApp_Genes"),
    custom_genes                           = as_vec("Custom_Genes"),
    substract_panelapp_gene_lists_filter   = as_vec("Substract_PanelApp_Genes_Lists"),
    substract_panelapp_genes_filter        = as_vec("Substract_PanelApp_Genes"),
    hpo_terms_list                         = as_vec("HPO_Terms"),

    # Numerics -> NULL when blank
    af_value             = as_num("SV_gnomADv4 AF"),
    min_svlen            = as_num("SV_Min SV Length"),
    max_svlen            = as_num("SV_Max SV Length"),
    genotype_quality_value = as_num("SV_Genotype quality"),
    allele_balance_value   = as_num("SV_Allele balance"),
    inheritance_filter     = as_scalar_str("Inheritance"),

    # Booleans
    treat_negative             = as_bool("Treat_Negative", FALSE),
    affected_only              = as_bool("SV_Affected only", FALSE),
    inheritance_panelapp_gene  = as_bool("Inheritance_PanelApp_Gene", FALSE)
  )

  # Debug print (optional)
  # print(snv_filters)
  # print(sv_filters)

  list(
    snv_filters = snv_filters,
    sv_filters  = sv_filters
  )
}