#' DB Utility
#'#' Utility functions to load and manage database files.
#'

#' Load VEP Annotations Mapping
#'
#' Utility function to load VEP consequence-to-term mappings from a TSV file.
#'
#' @param file Path to the VEP annotations TSV file. If NULL, loads the default file from the package.
#' @return A named list where each key is a consequence and the value is a vector of terms.
load_vep_consequences <- function(file = NULL) {
  if (is.null(file)) {
    # Locate the TSV file inside inst/extdata/
    file <- system.file("extdata", "vep_annotations.tsv", package = "puzzleapp")
  }
  stopifnot(file.exists(file))
  # Read the file
  dt <- data.table::fread(file = file, header = TRUE)
  # Debug print removed for production use
  return(dt)
}

# #' Load Phenotype Data
# #'
# #' Utility function to load phenotype-to-genes data from a TSV file.
# #'
# #' @param file Path to the phenotype TSV file. If NULL, loads the default file from the package.
# #' @return A data frame containing phenotype-to-gene mappings, typically with columns such as phenotype ID, phenotype name, gene symbol, and gene ID.
# load_phenotype_data <- function(file = NULL) {
#   if (is.null(file)) {
#     cat("Loading default HPO data.\n")
#     # Locate the TSV file inside inst/extdata/
#     file <- system.file("extdata", "phenotype_to_genes.txt", package = "puzzleapp")
#   }
#   stopifnot(file.exists(file))
#   # Read the file
#   df <- read.delim(file, stringsAsFactors = FALSE, sep = "\t")
#   return(df)
# }

#' Load Phenotype Data
#'
#' Utility function to load phenotype-to-genes data from a TSV file.
#'
#' @param file Path to the phenotype TSV file. If NULL, loads the default file from the package.
#' @return A data frame containing phenotype-to-gene mappings, typically with columns such as phenotype ID, phenotype name, gene symbol, and gene ID.
load_phenotype_data <- function(file = NULL) {
  if (is.null(file)) {
    # Locate the TSV file inside inst/extdata/
    file <- system.file("extdata", "phenotype_to_genes.txt", package = "puzzleapp")
    if (!file.exists(file)) {
      alt_file <- system.file("extdata", "phenotype_to_genes.txt.tar.bz2", package = "puzzleapp")
      cat("Please Decompress file:", alt_file, "\n")
      # exit now
      stop("Decompress the file and try again.")
    }
  }

  stopifnot(file.exists(file))
  # Read the file
  dt <- fread(file, nThread = 2, header = TRUE)
  return(dt)
}

#' Load PanelApp Data
#'
#' Utility function to load PanelApp data from a TSV file.
#'
#' @param file Path to the PanelApp TSV file. If NULL, loads the default file from the package.
#' @return A data frame containing PanelApp data.
# Processed behavior of the Sources column:

# Original Sources              -> Processed Sources
# ------------------------------------------------
# "Expert Review Green"         -> "Green"
# "Expert Review Red"           -> "Red"
# Other                      -> Unclassified"
load_panel_app_data <- function(file = NULL) {
  if (is.null(file)) {
    file <- system.file("extdata", "all_panel_app.tsv", package = "puzzleapp")
  }
  stopifnot(file.exists(file))
  # read as data.table and ensure it's data.table-aware
  panel_app <- data.table::fread(file, header = TRUE, nThread = 2, data.table = TRUE)
  # required columns
  required_cols <- c("Entity_Name", "Sources", "Level4", "Model_Of_Inheritance")
  missing <- setdiff(required_cols, names(panel_app))
  if (length(missing) > 0) {
    warning("Missing required columns in PanelApp file: ", paste(missing, collapse = ", "))
    return(NULL)
  }
  # keep only required columns
  panel_app_genes <- data.table::copy(panel_app[, required_cols, with = FALSE])
  panel_app_genes[, Sources := str_extract(Sources, "Expert Review ([[:alnum:].]+)")]
  panel_app_genes[, Sources := str_remove(Sources, "Expert Review ")]
  panel_app_genes[, Model_Of_Inheritance := tstrsplit(panel_app$Model_Of_Inheritance, ",")[[1]]]
  panel_app_genes <- as.data.table(panel_app_genes)
  panel_app_genes
}

add_extra_columns <- function(dt) {
  # Return NULL immediately if input is NULL
  if (is.null(dt)) return(NULL)

  # Define extra columns with their default types
  extra_columns <- list(
    PRIORITY = NA_integer_,            # integer
    NOTES = NA_character_,
    INHERITANCE = NA_character_,
    PANEL_APP = NA_character_,
    HPO_ID = NA_character_,
    HPO_COUNT = NA_real_,        # numeric
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

collect_inputs <- function(input) {
  messages <- list()
  # Collect sample info
  n <- input$num_individuals
  samples <- lapply(seq_len(n), function(i) {
    list(
      sample_id = input[[paste0("sample_id_", i)]],
      kinship   = input[[paste0("kinship_", i)]],
      status    = input[[paste0("status_", i)]],
      sex       = input[[paste0("sex_", i)]],
      code      = input[[paste0("code_", i)]],
      bam       = input[[paste0("bam_", i)]],
      coverage  = input[[paste0("coverage_", i)]]
    )
  })
  cat("Collected sample info for", length(samples), "individuals.\n")
  cat("Samples:", str(samples), "\n")

  # Convert the list of samples into a data.table
  pedigree_data <- tryCatch({
    rbindlist(lapply(samples, as.data.table), fill = TRUE)
  }, error = function(e) {
    messages <<- c(messages, paste("Error processing pedigree data:", e$message))
    stop("Error processing pedigree data:", e$message)
  })

  # SNVs
  snvs_data <- NULL
  if (!is.null(input$snvs_tsv) && nzchar(input$snvs_tsv)) {
    if (file.exists(input$snvs_tsv)) {
      snvs_data <- data.table::fread(input$snvs_tsv)
      snvs_data <- add_extra_columns(snvs_data)
    } else {
      # shiny::showNotification("SNVs & Indels TSV file not found.", type = "error")
      messages <- c(messages, "SNVs & Indels TSV file not found.")
    }
  }

  # SVs
  svs_data <- NULL
  if (!is.null(input$svs_tsv) && nzchar(input$svs_tsv)) {
    if (file.exists(input$svs_tsv)) {
      svs_data <- data.table::fread(input$svs_tsv)
      svs_data <- add_extra_columns(svs_data)
    } else {
      # shiny::showNotification("SVs TSV file not found.", type = "error")
      messages <- c(messages, "SVs TSV file not found.")
    }
  }

  # PanelApp
  panel_app_data <- NULL
  if (!is.null(input$panel_app) && nzchar(input$panel_app)) {
    cat("input$panel_app:", input$panel_app, "\n")
    if (file.exists(input$panel_app)) {
      panel_app_data <- load_panel_app_data(file = input$panel_app)
    } else {
      # shiny::showNotification("PanelApp TSV file not found.", type = "error")
      messages <- c(messages, "PanelApp TSV file not found.")
    }
  } else {
    cat("Loading from PanelApp database.\n")
    panel_app_data <- load_panel_app_data()
  }

  # VEP consequences
  vep_consequences <- NULL
  if (!is.null(input$vep_consequences) && nzchar(input$vep_consequences)) {
    if (file.exists(input$vep_consequences)) {
      vep_consequences <- load_vep_consequences(file = input$vep_consequences)
    } else {
      # shiny::showNotification("VEP consequence annotations file not found.", type = "error")
      messages <- c(messages, "VEP consequence annotations file not found.")
    }
  } else {
    cat("Loading from VEP consequences database.\n")
    vep_map <- load_vep_consequences()
  }

  # Phenotype
  phenotype_data <- NULL
  if (!is.null(input$phenotype_data) && nzchar(input$phenotype_data)) {
    if (file.exists(input$phenotype_data)) {
      phenotype_data <- load_phenotype_data(file = input$phenotype_data)
    } else {
      # shiny::showNotification("Human Phenotype Ontology TSV file not found.", type = "error")
      messages <- c(messages, "Human Phenotype Ontology TSV file not found.")

    }
  } else {
    cat("Loading from HPO database.\n")
    phenotype_data <- load_phenotype_data()
  }

  # Return everything as a list
  list(
    messages = messages,
    samples = samples,
    pedigree = pedigree_data,
    snvs_data = snvs_data,
    svs_data = svs_data,
    panel_app_data = panel_app_data,
    vep_consequences = vep_consequences,
    phenotype_data = phenotype_data
  )
}

prepare_table <- function(dt, selected_cols) {
  cat("Preparing table with selected columns.\n")
  cat("Selected columns:", paste(selected_cols, collapse = ", "), "\n")
  if (is.null(dt)) return(NULL)
  # Ensure all selected columns exist in dt
  existing_cols <- intersect(selected_cols, colnames(dt))
  # Subset and reorder the data table using [[ ]] style
  dt_subset <- dt[, existing_cols, with = FALSE]
  dt_subset
}