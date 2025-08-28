#' DB Utility
#'#' Utility functions to load and manage database files.
#'

#' Load VEP Annotations Mapping
#'
#' Utility function to load VEP consequence-to-term mappings from a TSV file.
#'
#' @param file Path to the VEP annotations TSV file. If NULL, loads the default file from the package.
#' @return A named list where each key is a consequence and the value is a vector of terms.
load_vep_map <- function(file = NULL) {
  if (is.null(file)) {
    # Locate the TSV file inside inst/extdata/
    cat("Loading default VEP annotations mapping.\n")
    file <- system.file("extdata", "vep_annotations.tsv", package = "puzzleapp")
  }
  stopifnot(file.exists(file))
  # Read the file
  # Default file is tab-delimited (TSV). For space-delimited files, use sep = " ".
  df <- read.delim(file, stringsAsFactors = FALSE, sep = "\t")
  # Create a named list: key = consequence, value = vector of terms
  vep_map <- split(df$term, df$consequence)
  # Debug print removed for production use
  return(vep_map)
}


#' Load Phenotype Data
#'
#' Utility function to load phenotype-to-genes data from a TSV file.
#'
#' @param file Path to the phenotype TSV file. If NULL, loads the default file from the package.
#' @return A data frame containing phenotype-to-gene mappings, typically with columns such as phenotype ID, phenotype name, gene symbol, and gene ID.
load_phenotype_data <- function(file = NULL) {
  if (is.null(file)) {
    cat("Loading default HPO data.\n")
    # Locate the TSV file inside inst/extdata/
    file <- system.file("extdata", "phenotype_to_genes.txt", package = "puzzleapp")
  }
  stopifnot(file.exists(file))
  # Read the file
  df <- read.delim(file, stringsAsFactors = FALSE, sep = "\t")
  return(df)
}


#' Load PanelApp Data
#'
#' Utility function to load PanelApp data from a TSV file.
#'
#' @param file Path to the PanelApp TSV file. If NULL, loads the default file from the package.
#' @return A data frame containing PanelApp data.
load_panel_app_data <- function(file = NULL) {
  if (is.null(file)) {
    cat("Loading default PanelApp data.\n")
    # Locate the TSV file inside inst/extdata/
    file <- system.file("extdata", "all_panel_app.tsv", package = "puzzleapp")
  }
  stopifnot(file.exists(file))
  # Read the file
  df <- read.delim(file, stringsAsFactors = FALSE, sep = "\t")
  return(df)
}

