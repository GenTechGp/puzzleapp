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
# export the functino in roxygen
#' Read Variant TSV with Extra Columns
#' This function reads a variant TSV file and adds extra columns if they are missing.
#' @param file_path The path to the variant TSV file.
#' @param nthreads The number of threads to use for reading the file.
#' @return A data.table containing the variant data with extra columns added.
#' @examples
#' # variant_data <- puzzlecore_read_variant_tsv("path/to/variant_file.tsv", nthreads = 4)
#' @export
puzzlecore_read_variant_tsv <- function(file_path, nthreads) {
    data <- data.table::fread(file_path, nThread = nthreads, na.strings = c("", ".", "NA"))
    data <- add_extra_columns(data)

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