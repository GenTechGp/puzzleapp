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
  filters       <- puzzlecore_parse_filter_table(filter_table, vep_consequences = vep_consequences)
  filters_snv   <- filters$snv_filters
  filters_sv    <- filters$sv_filters
  allele_counts_dt <- puzzlecore_allele_counts_table(samples_list, filters_snv$inheritance_filter, filters_snv$custom_allele_counts)
  # cat("Allele counts data.table:\n")
  # print(allele_counts_dt)

  svlog_db <- NULL
  if (!is.null(cfg$paths$svlog_db)) {
    #todo: validate svlog_db path and format before reading
    svlog_db   <- fread(cfg$paths$svlog_db)
  }

  snv_path <- cfg$paths$snvs_tsv %||% ""
  sv_path  <- cfg$paths$svs_tsv  %||% ""
  snv_data <- NULL
  sv_data  <- NULL
  if (!file.exists(snv_path)) {
    cat("SNV TSV file does not exist: ", snv_path, "\n")
  } else {
    snv_data <- puzzlecore_read_variant_tsv(snv_path, nthreads = nthreads)
  }
  if (!file.exists(sv_path)) {
    cat("SV TSV file does not exist: ", sv_path, "\n")
  } else {
    add_svlog_columns <- !is.null(svlog_db)
    sv_data <- puzzlecore_read_variant_tsv(sv_path, nthreads = nthreads, snv=FALSE, add_svlog_columns = add_svlog_columns, svlog_db = svlog_db )
  }

  filtered_data <- puzzlecore_variant_filter(
    snv_data = snv_data,
    sv_data = sv_data,
    snv_filters = filters_snv,
    sv_filters = filters_sv,
    pedigree = pedigree,
    allele_tab = allele_counts_dt,
    panel_app_genes = panel_app_genes,
    vep_consequences = vep_consequences,
    phenotype_data = phenotype_data,
    svlog_db = svlog_db
  )
  result_snv <- filtered_data$snv
  result_sv <- filtered_data$sv
  if (verbose && !is.null(snv_data) && !is.null(result_snv)) message(sprintf("[pipeline] SNV filtered rows: %s/%s", nrow(result_snv), nrow(snv_data)))
  if (verbose && !is.null(sv_data) && !is.null(result_sv)) message(sprintf("[pipeline] SV filtered rows: %s/%s", nrow(result_sv), nrow(sv_data)))
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
