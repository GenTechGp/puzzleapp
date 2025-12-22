add_extra_columns <- function(dt) {
  # Return NULL immediately if input is NULL
  if (is.null(dt)) return(NULL)

  # Define extra columns with their default types
  extra_columns <- list(
    PRIORITY = 0L,            # integer
    NOTES = NA_character_,
    INHERITANCE = NA_character_,
    PANEL_APP = NA_character_,
    HPO_ID = NA_character_,
    HPO_COUNT = 0L,        # numeric
    spliceai_override = FALSE,      # logical
    clinvar_override = FALSE,       # logical
    PRIORITYFlag = as.logical(NA)           # logical later
  )
  # Add any missing columns
  for (col in names(extra_columns)) {
    if (!(col %in% names(dt))) {
      dt[[col]] <- rep(extra_columns[[col]], nrow(dt))
    }
  }
  dt
}

check_data <- function(label, data, table_schema) {
  table_schema[, has_suffix := as.integer(has_suffix)]
  orig_names <- names(data)

  # 1) Add missing non-suffix columns (fixed)
  fixed_names <- table_schema[has_suffix == 0, name]
  missing_fixed <- setdiff(fixed_names, orig_names)
  if (length(missing_fixed)) {
    for (nm in missing_fixed) {
      dtype <- table_schema[name == nm, default_type][1]
      val <- if (dtype == "integer") 0L else if (dtype == "float") 0 else NA_character_
      data[, (nm) := val]
    }
  }

  # 2) Check suffix columns exist (error if any base missing)
  suffix_bases <- unique(vapply(table_schema[has_suffix == 1, name], function(nm) {
    parts <- strsplit(nm, "_", fixed = TRUE)[[1]]
    if (length(parts) >= 2) paste(parts[-length(parts)], collapse = "_") else nm
  }, character(1)))

  prefixes <- if (length(suffix_bases)) paste0(suffix_bases, "_") else character(0)
  base_has_cols <- function(base) any(startsWith(orig_names, paste0(base, "_")))
  missing_bases <- suffix_bases[!vapply(suffix_bases, base_has_cols, logical(1))]
  if (length(missing_bases)) {
    stop("Missing required suffix columns for base(s): ", paste(missing_bases, collapse = ", "))
  }

  # 3) Drop any other columns
  starts_with_any <- function(x, prefs) {
    if (!length(prefs)) return(rep(FALSE, length(x)))
    out <- rep(FALSE, length(x))
    for (p in prefs) out <- out | startsWith(x, p)
    out
  }
  allowed_existing <- unique(c(intersect(fixed_names, orig_names), orig_names[starts_with_any(orig_names, prefixes)]))
  dropped_cols <- setdiff(orig_names, allowed_existing)

  # Final subset includes allowed existing + the newly added fixed columns
  keep_final <- unique(c(allowed_existing, missing_fixed))
  data <- data[, ..keep_final]

  cat(sprintf("Data check for %s:\n", label))
  cat(sprintf("Missing columns added: %d\n", length(missing_fixed)))
  # print missing columns added
  if (length(missing_fixed) > 0) {
    cat("Added missing columns:\n")
    for (col in missing_fixed) {
      cat(" - ", col, "\n")
    }
  }
  cat(sprintf("Columns dropped: %d\n", length(dropped_cols)))
  if (length(dropped_cols) > 0) {
    cat("Dropped columns:\n")
    for (col in dropped_cols) {
      cat(" - ", col, "\n")
    }
  }
  return(data)
}

# export the functino in roxygen
#' Read Variant TSV with Extra Columns
#' This function reads a variant TSV file and adds extra columns if they are missing.
#' @param file_path The path to the variant TSV file.
#' @param nthreads The number of threads to use for reading the file.
#' @param snv Logical indicating if the file is for SNVs (TRUE) or SVs (FALSE).
#' @param add_svlog_columns Logical indicating whether to add SVlog-specific columns (only for SVs).
#' @return A data.table containing the variant data with extra columns added.
#' @examples
#' # variant_data <- puzzlecore_read_variant_tsv("path/to/variant_file.tsv", nthreads = 4)
#' @export
puzzlecore_read_variant_tsv <- function(file_path, nthreads, snv=TRUE, add_svlog_columns = FALSE) {
    # data <- data.table::fread(file_path, nThread = nthreads, na.strings = c("", ".", "NA"), nrows = 2000)
    data <- data.table::fread(file_path, nThread = nthreads, na.strings = c("", ".", "NA"))
    # rename columns VEP_CONSEQUENCE to CONSEQUENCE if available
    if ("VEP_CONSEQUENCE" %in% names(data)) {
      setnames(data, "VEP_CONSEQUENCE", "CONSEQUENCE")
    }
    if (snv){
      table_schema <- fread(system.file("extdata", "db", "table_schema", "snv_colnames.tsv", package = "puzzleapp"), nThread = nthreads)
      data <- check_data("snv", data, table_schema)
    } else {
      table_schema <- fread(system.file("extdata", "db", "table_schema", "sv_colnames.tsv", package = "puzzleapp"), nThread = nthreads)
      data <- check_data("sv", data, table_schema)
    }
    # print(colnames(data))

    data <- add_extra_columns(data)

    # CONSEQUENCE column has & symbol. replace with ;
    if ("CONSEQUENCE" %in% names(data)) {
      data[, CONSEQUENCE := gsub("&", ";", CONSEQUENCE)]
    }

    # Fix CLINVAR empty values
    if ("CLINVAR" %in% names(data)) {
      # Step 1: convert to character first (safe replacement)
      data[, CLINVAR := as.character(CLINVAR)]
      
      # Step 2: replace NA or empty
      data[is.na(CLINVAR) | CLINVAR == "", CLINVAR := "Not_available"]
    }

    factor_cols <- c("VAR_TYPE", "CHROM", "CONSEQUENCE", "CLINVAR")
    # factor_cols <- c("VAR_TYPE", "CHROM", "CONSEQUENCE", "CLINVAR","GENE_ID", "GENE_SYMBOL")
    for (col in factor_cols) {
      if (col %in% names(data)) data[, (col) := as.factor(get(col))]
    }
    # and any column that starts with GT_
    gt_cols <- grep("^GT_", names(data), value = TRUE)
    for (col in gt_cols) {
      data[, (col) := as.factor(get(col))]
    }

    if (add_svlog_columns) {
      # add dummy rows for ClinVar, InternalCohort, ONT1000G, gnomAD, gnomAD_AF_max, ONT1000G_carriers_max, Internal_carriers_max, Internal_families_max, ClinVar_CLASS
      sv_extra_cols <- list(
        ClinVar = NA_character_,
        InternalCohort = NA_character_,
        ONT1000G = NA_character_,
        gnomAD = NA_character_,
        gnomAD_AF_max = NA_real_,
        ONT1000G_carriers_max = NA_integer_,
        Internal_carriers_max = NA_integer_,
        Internal_families_max = NA_integer_,
        ClinVar_CLASS = NA_character_
      )
      for (col in names(sv_extra_cols)) {
        if (!(col %in% names(data))) {
          data[[col]] <- rep(sv_extra_cols[[col]], nrow(data))
        }
      }
    }

    return(data)
}

# export the function in roxygen
#' Load and Process PanelApp Data
#' This function loads PanelApp data from a specified file and processes it to extract relevant information.
#' @param file The path to the PanelApp data file.
#' @return A data.table containing the processed PanelApp gene data.
#' @examples
#' # panel_app_genes <- puzzlecore_load_panel_app_data("path/to/panelapp_file.tsv")
#' # Processed behavior of the Sources column:
#' # Original Sources              -> Processed Sources
#' # ------------------------------------------------
#' # "Expert Review Green"         -> "Green"
#' # "Expert Review Red"           -> "Red"
#' # Other                      -> Unclassified"
#' @export
puzzlecore_load_panel_app_data <- function(file) {
  stopifnot(file.exists(file))
  # read as data.table and ensure it's data.table-aware
  panel_app <- data.table::fread(file, header = TRUE, data.table = TRUE, nThread = nthreads)
  # required columns
  required_cols <- c("Entity_Name", "Sources", "Level4", "Model_Of_Inheritance")
  missing <- setdiff(required_cols, names(panel_app))
  if (length(missing) > 0) {
    warning("Missing required columns in PanelApp file: ", paste(missing, collapse = ", "))
    return(NULL)
  }
  # keep all the columns (not only the required ones)
  panel_app_genes <- data.table::copy(panel_app)
  panel_app_genes[, Sources := str_extract(Sources, "Expert Review ([[:alnum:].]+)")]
  panel_app_genes[, Sources := str_remove(Sources, "Expert Review ")]
  panel_app_genes[, Model_Of_Inheritance := tstrsplit(panel_app$Model_Of_Inheritance, ",")[[1]]]
  panel_app_genes <- as.data.table(panel_app_genes)
  panel_app_genes
}

# export the function in roxygen
#' Load Phenotype Data
#' This function loads phenotype-to-genes data from a TSV file.
#' @param file Path to the phenotype TSV file.
#' @return A data frame containing phenotype-to-gene mappings.
#' @examples
#' # phenotype_data <- puzzlecore_load_phenotype_data("path/to/phenotype_file.tsv")
#' @export
puzzlecore_load_phenotype_data <- function(file) {
  stopifnot(file.exists(file))
  # Read the file
  dt <- fread(file, header = TRUE, nThread = nthreads)
  return(dt)
}

# export the function in roxygen
#' Load VEP Consequences Data
#' This function loads VEP consequences data from a TSV file.
#' @param file Path to the VEP consequences TSV file.
#' @return A data frame containing VEP consequences information.
#' @examples
#' # vep_consequences <- puzzlecore_load_vep_consequences("path/to/vep_consequences_file.tsv")
#' @export
puzzlecore_load_vep_consequences <- function(file) {
  stopifnot(file.exists(file))
  # Read the file
  dt <- data.table::fread(file = file, header = TRUE, nThread = nthreads)
  # Debug print removed for production use
  return(dt)
}

#' Check Pedigree Sanity
#' This function checks the sanity of a pedigree list.
#' It verifies that there is exactly one proband, that the proband's code is 1,
#' that there is at most one father and one mother, and that there are no duplicate sample IDs.
#' @param pedigree A list of pedigree entries, each containing sample_id, kinship, status, sex, and code.
#' @return A list of issues found in the pedigree. If no issues are found, the list will be empty.
#' @examples
#' pedigree <- list(
#'   list(sample_id = "S1", kinship = "proband", status = "affected", sex = "male", code = 1),
#'   list(sample_id = "S2", kinship = "father", status = "unaffected", sex = "male", code = 2),
#'   list(sample_id = "S3", kinship = "mother", status = "unaffected", sex = "female", code = 3)
#' )
#' issues <- puzzlecore_check_pedigree_sanity(pedigree)
#' if (length(issues) == 0) {
#'   print("Pedigree is sane.")
#' } else {
#'   print(issues)
#' }
#' @export
puzzlecore_check_pedigree_sanity <- function(pedigree) {
  issues <- list()
  # check required fields exist for all entries
  required_fields <- c("sample_id", "kinship", "status", "sex", "code")
  for (i in seq_along(pedigree)) {
    missing_fields <- setdiff(required_fields, names(pedigree[[i]]))
    if (length(missing_fields) > 0) {
      issues <- c(issues, paste0("Sample ", i, " missing fields: ", paste(missing_fields, collapse = ", ")))
    }
  }
  # collect kinship info
  kinships <- vapply(pedigree, function(x) x$kinship, character(1))
  sample_ids <- vapply(pedigree, function(x) x$sample_id, character(1))
  # proband count
  if (sum(kinships == "proband") != 1) {
    issues <- c(issues, paste("Expected exactly 1 proband, found", sum(kinships == "proband")))
  }
  # proband code should be 1
  proband_index <- which(kinships == "proband")
  if (length(proband_index) == 1) {
    proband_code <- pedigree[[proband_index]]$code
    if (proband_code != 1) {
      issues <- c(issues, paste("Proband code should be 1, found", proband_code))
    }
  }
  # father/mother uniqueness
  if (sum(kinships == "father") > 1) {
    issues <- c(issues, paste("More than one father found (", sum(kinships == "father"), ")", sep=""))
  }
  if (sum(kinships == "mother") > 1) {
    issues <- c(issues, paste("More than one mother found (", sum(kinships == "mother"), ")", sep=""))
  }
  # duplicate sample_id
  if (anyDuplicated(sample_ids)) {
    issues <- c(issues, "Duplicate sample_id values found")
  }
  return(issues)
}


#' Compute Allele Count for a Sample
#' This function computes the allele count for a given sample based on the inheritance model,
#' status, and sex.
#' @param inher The inheritance model (e.g., "Homozygous Recessive", "Dominant/ De Novo", "Compound Heterozygous", "X-Linked Recessive").
#' @param status The status of the sample ("affected", "unaffected", or "NA").
#' @param sex The sex of the sample ("male" or "female").
#' @return A string representing the allele count for the sample.
#' @examples
#' puzzlecore_allele_count("Homozygous Recessive", "affected", "male") # returns "2"
#' puzzlecore_allele_count("Dominant/ De Novo", "unaffected", "female") # returns "0"
#' @export
puzzlecore_allele_count <- function(inher, status, sex) {
  if (status == "NA") return("")  # special case

  if (inher == "Homozygous Recessive") {
    if (status == "affected") "2" else "0-1"
  } else if (inher == "Dominant/De Novo") {
    if (status == "affected") "1-2" else "0"
  } else if (inher == "Compound Heterozygous") {
    if (status == "affected") "1" else "0-1"
  } else if (inher == "X-Linked Recessive") {
    if (sex == "male") {
      if (status == "affected") "1" else "0"
    } else {
      if (status == "affected") "2" else "0-1"
    }
  } else {
    ""
  }
}


#' Compute Allele Table for Pedigree
#' This function computes the allele count table for a given pedigree and inheritance model.
#' @param pedigree A list of pedigree entries, each containing sample_id, kinship, status, sex, and code.
#' @param inher The inheritance model as a string.
#' @return A named list where each name is a sample_id and the value is the corresponding allele count.
#' @examples
#' pedigree <- list(
#'   list(sample_id = "S1", kinship = "proband", status = "affected", sex = "male", code = 1),
#'   list(sample_id = "S2", kinship = "father", status = "unaffected", sex = "male", code = 2),
#'   list(sample_id = "S3", kinship = "mother", status = "unaffected", sex = "female", code = 3)
#' )
#' allele_table <- puzzlecore_compute_allele_table(pedigree, "Homozygous Recessive")
#' print(allele_table)
#' @export
puzzlecore_compute_allele_table <- function(pedigree, inher) {
  if (length(pedigree) == 0 || inher == "") return(list())
  res <- list()
  for (sample in pedigree) {
    sid <- sample$sample_id
    res[[sid]] <- puzzlecore_allele_count(inher, sample$status, sample$sex)
    # cat("Sample:", sid, "Allele Count:", res[[sid]], "status:", sample$status, "sex:", sample$sex, "\n")
  }
  res
}

#' Parse Filter Table
#' This function parses a filter table from a given file path and extracts SNV and SV filters.
#' @param path The path to the filter table file or a data.table object.
#' @return A list containing two elements: snv_filters and sv_filters, each being a list of filter parameters.
#' @examples
#' # filters <- puzzlecore_parse_filter_table("path/to/filter_table.tsv")
#' @export
puzzlecore_parse_filter_table <- function(path) {
  # if path is a data.table already, use it directly
  if (is.data.table(path)) {
    dt <- path
  } else {
    dt <- data.table::fread(path, header = FALSE, sep = "\t", data.table = TRUE)
  }
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
    substract_panelapp_gene_lists_filter   = as_vec("Substract_PanelApp_Gene_Lists"),
    substract_panelapp_genes_filter        = as_vec("Substract_PanelApp_Genes"),
    hpo_terms_list                         = as_vec("HPO_Terms"),

    # todo: support both legacy (non_underscore) and new (with_underscore) keys and document the filter table format
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
    custom_allele_counts    = as_vec("Custom_Allele_Counts"),

    # Booleans
    treat_negative              = as_bool("Treat_Negative", FALSE),
    affected_only              = as_bool("SNV_Affected only", FALSE),
    inheritance_panelapp_gene  = as_bool("Inheritance_PanelApp_Gene", FALSE)
  )

  sv_filters <- list(
    # ------------------------------------------------------------------
    # Common / shared filters
    # ------------------------------------------------------------------
    inheritance_filter       = as_scalar_str("Inheritance"),
    custom_allele_counts    = as_vec("Custom_Allele_Counts"),
    panelapp_filter          = as_vec("PanelApp_Genes"),
    custom_genes             = as_vec("Custom_Genes"),
    substract_panelapp_gene_lists_filter = as_vec("Substract_PanelApp_Gene_Lists"),
    substract_panelapp_genes_filter      = as_vec("Substract_PanelApp_Genes"),
    hpo_terms_list           = as_vec("HPO_Terms"),
    
    annotation_filter        = as_vec("SV_Annotation"),
    
    genotype_quality_value   = as_num("SV_Genotype quality"),
    allele_balance_value     = as_num("SV_Allele balance"),
    af_value                 = as_num("SV_SVlog_gnomAD_AF"),
    
    # ------------------------------------------------------------------
    # SV type/size + labels
    # ------------------------------------------------------------------
    sv_features              = as_vec("SV_SV type"),
    min_svlen                = as_num("SV_Min SV Length"),
    max_svlen                = as_num("SV_Max SV Length"),
    
    clinvar_filter           = as_vec("SV_Pathogenicity"),
    classification_filter    = as_vec("SV_Classification"),
    
    # ------------------------------------------------------------------
    # Tier prioritisation
    # ------------------------------------------------------------------
    keeping_tiers            = as_vec("SV_Keeping"),
    filtering_out_tiers      = as_vec("SV_Filtering_out"),
    
    # ------------------------------------------------------------------
    # Genomic context
    # ------------------------------------------------------------------
    intronic_splice_max_dist      = as_num("SV_intronic_splice_max_dist"),
    intronic_min_len_intron_ratio = as_num("SV_intronic_min_len_intron_ratio"),
    tad_max_dist                  = as_num("SV_tad_max_dist"),
    enhancer_max_dist             = as_num("SV_enhancer_max_dist"),
    intra_tad_only                = as_bool("SV_intra_tad_only",  FALSE),
    inter_tad_only                = as_bool("SV_inter_tad_only", FALSE),
    
    # ------------------------------------------------------------------
    # SVlog matching + thresholds
    # ------------------------------------------------------------------
    svlog_annotation_filter  = as_vec("SV_SVlog_Annotation"),
    svlog_advanced_keeping  = as_vec("SV_SVlog_Advanced_Keeping"),
    svlog_advanced_filtering_out  = as_vec("SV_SVlog_Advanced_Filtering_out"),
    svlog_matching_mode      = as_scalar_str("SV_SVlog_matching_mode"),
    svlog_min_recip_overlap  = as_num("SV_SVlog_min_recip_overlap"),
    svlog_max_break_distance = as_num("SV_SVlog_max_break_distance"),
    svlog_max_abs_dlen       = as_num("SV_SVlog_max_abs_dlen"),
    svlog_gnomad_af          = as_num("SV_SVlog_gnomAD_AF"),
    svlog_1000g_max_carriers = as_num("SV_SVlog_1000G_max_carriers"),
    svlog_internal_max_carriers = as_num("SV_SVlog_internal_max_carriers"),
    svlog_internal_max_families = as_num("SV_SVlog_internal_max_families"),
    
    # ------------------------------------------------------------------
    # Flags
    # ------------------------------------------------------------------
    treat_negative            = as_bool("Treat_Negative", FALSE),
    affected_only             = as_bool("SV_Affected only", FALSE),
    inheritance_panelapp_gene = as_bool("Inheritance_PanelApp_Gene", FALSE)
  )
  
  # Debug print (optional)
  # print(snv_filters)
  # print(sv_filters)

  list(
    snv_filters = snv_filters,
    sv_filters  = sv_filters
  )
}


#' Generate Allele Counts Table
#' This function generates a data.table of allele counts for samples based on the specified inheritance filter
#' and custom allele counts.
#' @param samples_list A list of pedigree entries, each containing sample_id, kinship, status, sex, and code.
#' @param inheritance_filter The inheritance filter to use (e.g., "Homozygous Recessive", "Dominant/ De Novo", "Custom").
#' @param custom_allele_counts A vector of custom allele counts in the format "sample_id:count".
#' @return A data.table with columns sample_id and allele_count.
#' @export 
puzzlecore_allele_counts_table <- function(samples_list, inheritance_filter, custom_allele_counts) {
  allele_counts_dt <- NULL
  if (inheritance_filter == "Custom") {
    if (is.null(custom_allele_counts) || length(custom_allele_counts) == 0) {
      stop("SNV Inheritance filter is set to 'Custom' but no Custom_Allele_Counts provided.")
    }

    allele_counts_dt <- data.table::rbindlist(lapply(custom_allele_counts, function(x) {
      x <- trimws(x)
      pos <- regexpr(":", x, fixed = TRUE)
      if (pos < 1) {
        stop("Invalid format for Custom_Allele_Counts: ", x, ". Expected format: sample_id:count")
      }

      sample_id   <- trimws(substr(x, 1, pos - 1))
      allele_count <- trimws(substr(x, pos + 1, nchar(x)))

      # allow NA counts
      # na_tokens <- c("", ".", "NA")
      # allele_count <- if (toupper(count_token) %in% toupper(na_tokens)) NA_character_ else count_token

      data.table(sample_id = sample_id, allele_count = allele_count)
    }))
  } else {
    allele_counts <- puzzlecore_compute_allele_table(
      samples_list,
      inheritance_filter
    ) # named list
    cat("Allele counts:\n")
    print(allele_counts)
    allele_counts_dt <- data.table(
      sample_id    = names(allele_counts),
      allele_count = unlist(allele_counts, use.names = FALSE)
    )
  }
  allele_counts_dt
}