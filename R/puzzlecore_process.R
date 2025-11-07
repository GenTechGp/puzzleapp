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
    data <- data.table::fread(file_path, nThread = nthreads)
    data <- add_extra_columns(data)
    return(data)
}

# export the function in roxygen
#' Load and Process PanelApp Data
#' This function loads PanelApp data from a specified file and processes it to extract relevant information.
#' @param file The path to the PanelApp data file.
#' @return A data.table containing the processed PanelApp gene data.
#' @examples
#' # panel_app_genes <- load_panel_app_data("path/to/panelapp_file.tsv")
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