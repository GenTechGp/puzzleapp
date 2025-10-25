#' DB Utility
#'#' Utility functions to load and manage database files.
#' @keywords internal
#' @noRd
NULL

# boundary_table.R
# Build a boundary table from any data.frame/data.table and pad it so that
# an initial slice (e.g., first 10%) fully shows the boundary rows at the top.
#
# NEW (per latest spec):
# - ALWAYS prepend TWO dummy rows (dummy1 first, dummy2 second).
# - Each column receives two distinct, non-NA synthetic values (as feasible) using
#   deterministic type-specific rules, independent of existing data diversity.
#
#   Type rules:
#     * Character:  dummy1_<orig-or-NA>, dummy2_<orig-or-NA>
#     * Logical:    TRUE, FALSE   (order fixed)
#     * Numeric (double): 0, 100
#     * Integer:    -100L, 100L
#     * integer64:  -100, 100 (as integer64)  (treated like integer)
#     * Date:       origin - 100 days, origin + 100 days
#                   (1969-09-23, 1970-04-11)
#     * POSIXct/POSIXt: epoch + 0 sec, epoch + 100 sec
#     * Factor:
#         - If >= 2 distinct non-NA levels present: use the first two distinct levels.
#         - If exactly 1 distinct non-NA level: dummy1 = that level; dummy2 = synthetic level dummy2_<col>.
#         - If all NA (0 distinct levels): add two synthetic levels dummy1_<col>, dummy2_<col>.
#       (We only add synthetic levels if the column has fewer than 2 distinct non-NA levels.)
#     * Other / unsupported exotic types: replicate template values in both dummy rows (may not guarantee diversity).
#
# - Factor level expansion (if include_factor=TRUE) still adds a row per level (including synthetic ones).
# - Numeric floor/ceil boundary rows retained (if include_numeric=TRUE).
# - Padding logic unchanged.
#
# Parameters:
#   x, round_base, slice_pct, include_numeric, include_factor, add_row_id, verbose, logger
#
# Returns:
#   data.table with:
#     1. dummy1 row
#     2. dummy2 row
#     3. numeric min/max rows (optional)
#     4. factor level rows (optional)
#     5. padding rows
#   .row_id appended last (creation order).
#
# Logging tags:
#   [boundary][dummy] detail of dummy assignments
#   Existing tags preserved.

make_boundary_table <- function(x,
                                round_base = 10,
                                slice_pct = 10,
                                include_numeric = TRUE,
                                include_factor = TRUE,
                                add_row_id = TRUE,
                                verbose = TRUE,
                                logger = function(...) cat(...)) {
  stopifnot(is.data.frame(x))
  if (!inherits(x, "data.table")) x <- data.table::as.data.table(x)

  # ---- Helpers ---------------------------------------------------
  logf <- function(...) {
    if (isTRUE(verbose)) logger(paste0(..., collapse = ""), "\n")
  }
  floor_to_base <- function(v, base = 10) {
    if (inherits(v, "integer64")) v <- suppressWarnings(as.numeric(v))
    base * floor(v / base)
  }
  ceil_to_base <- function(v, base = 10) {
    if (inherits(v, "integer64")) v <- suppressWarnings(as.numeric(v))
    base * ceiling(v / base)
  }
  is_integer64 <- function(v) inherits(v, "integer64")
  is_date      <- function(v) inherits(v, "Date")
  is_posix     <- function(v) inherits(v, "POSIXct") || inherits(v, "POSIXt")
  is_numeric_like <- function(v) is.numeric(v) || is_integer64(v) || is_date(v) || is_posix(v)

  # ---- Empty input handling --------------------------------------
  if (nrow(x) == 0) {
    out <- data.table::copy(x)[0]
    if (add_row_id) {
      if (!(".row_id" %in% names(out))) out[, .row_id := integer()]
      data.table::setcolorder(out, c(setdiff(names(out), ".row_id"), ".row_id"))
    }
    logf("[boundary] Input has 0 rows; returning empty with same columns.")
    return(out)
  }

  template <- data.table::copy(x[1])

  cols    <- names(x)
  is_num  <- vapply(x, is_numeric_like, logical(1))
  is_fac  <- vapply(x, is.factor,       logical(1))
  is_char <- vapply(x, is.character,    logical(1))
  is_logi <- vapply(x, is.logical,      logical(1))

  num_cols  <- cols[is_num]
  fac_cols  <- cols[is_fac]
  char_cols <- cols[is_char]
  other_cols <- setdiff(cols, c(num_cols, fac_cols, char_cols))

  logf(sprintf("[boundary] Columns: numeric-like=%s | factor=%s | other=%s",
               paste(num_cols, collapse = ", "),
               paste(fac_cols, collapse = ", "),
               paste(other_cols, collapse = ", ")))

  # ---- Diagnostics (unchanged baseline) --------------------------
  for (col in cols) {
    v <- x[[col]]
    cls <- paste(class(v), collapse = ",")
    if (col %in% num_cols) {
      v_no_na <- v[!is.na(v)]
      if (length(v_no_na)) {
        v_min_raw <- suppressWarnings(min(v_no_na))
        v_max_raw <- suppressWarnings(max(v_no_na))
        if (is_date(v)) {
          v_min_floor <- as.Date(floor_to_base(as.numeric(v_min_raw), round_base), origin = "1970-01-01")
          v_max_ceil  <- as.Date(ceil_to_base(as.numeric(v_max_raw),  round_base), origin = "1970-01-01")
        } else if (is_posix(v)) {
          v_min_floor <- as.POSIXct(floor_to_base(as.numeric(v_min_raw), round_base), origin = "1970-01-01", tz = attr(v, "tzone"))
            v_max_ceil <- as.POSIXct(ceil_to_base(as.numeric(v_max_raw),  round_base), origin = "1970-01-01", tz = attr(v, "tzone"))
        } else {
          v_min_floor <- suppressWarnings(floor_to_base(v_min_raw, round_base))
          v_max_ceil  <- suppressWarnings(ceil_to_base(v_max_raw,  round_base))
        }
        logf(sprintf("[boundary][numeric] %s (class=%s): min=%s max=%s | floored(min)=%s ceiled(max)=%s",
                     col, cls,
                     as.character(v_min_raw), as.character(v_max_raw),
                     as.character(v_min_floor), as.character(v_max_ceil)))
      } else {
        logf(sprintf("[boundary][numeric] %s (class=%s): all NA", col, cls))
      }
    } else if (col %in% fac_cols) {
      levs <- levels(v)
      logf(sprintf("[boundary][factor] %s (class=%s): n_levels=%d | levels=[%s]",
                   col, cls, length(levs), paste(levs, collapse = ", ")))
    } else {
      sample_val <- tryCatch(as.character(template[[col]])[1], error = function(e) "<unprintable>")
      logf(sprintf("[boundary][other] %s (class=%s): sample='%s'", col, cls, sample_val))
    }
  }

  # ---- Construct two dummy rows ----------------------------------
  dummy1 <- data.table::copy(template)
  dummy2 <- data.table::copy(template)

  # Date endpoints for dummy rows: origin ± 100 days
  date_dummy1 <- as.Date(as.numeric(as.Date("1970-01-01")) - 100, origin = "1970-01-01") # 1969-09-23
  date_dummy2 <- as.Date(as.numeric(as.Date("1970-01-01")) + 100, origin = "1970-01-01") # 1970-04-11

  for (col in cols) {
    v <- x[[col]]

    # Character
    if (is_char[col]) {
      original_val <- as.character(template[[col]])[1]
      if (is.na(original_val)) original_val <- "NA"
      val1 <- paste0("dummy1_", original_val)
      val2 <- paste0("dummy2_", original_val)
      dummy1[[col]] <- val1
      dummy2[[col]] <- val2
      logf(sprintf("[boundary][dummy] %s character -> (%s, %s)", col, val1, val2))
      next
    }

    # Logical
    if (is_logi[col]) {
      dummy1[[col]] <- TRUE
      dummy2[[col]] <- FALSE
      logf(sprintf("[boundary][dummy] %s logical -> (TRUE, FALSE)", col))
      next
    }

    # Factor
    if (is_fac[col]) {
      v_non_na_levels <- unique(as.character(v[!is.na(v)]))
      n_used <- length(v_non_na_levels)
      # If 2+ distinct non-NA levels: pick first two
      if (n_used >= 2) {
        l1 <- v_non_na_levels[1]
        l2 <- v_non_na_levels[2]
        # Ensure both are in levels (they are by definition)
        dummy1[[col]] <- factor(l1, levels = levels(v))
        dummy2[[col]] <- factor(l2, levels = levels(v))
        logf(sprintf("[boundary][dummy] %s factor >=2 levels -> (%s, %s)", col, l1, l2))
      } else if (n_used == 1) {
        existing <- v_non_na_levels[1]
        syn2 <- paste0("dummy2_", col)
        levs <- levels(v)
        if (!(syn2 %in% levs)) {
          levs <- c(levs, syn2)
          # Update column levels in original data & template
          x[[col]] <- factor(as.character(x[[col]]), levels = levs)
          template[[col]] <- factor(as.character(template[[col]]), levels = levs)
        }
        dummy1[[col]] <- factor(existing, levels = levs)
        dummy2[[col]] <- factor(syn2, levels = levs)
        logf(sprintf("[boundary][dummy] %s factor single level -> (%s, %s)", col, existing, syn2))
      } else { # all NA
        syn1 <- paste0("dummy1_", col)
        syn2 <- paste0("dummy2_", col)
        levs <- levels(v)
        add <- setdiff(c(syn1, syn2), levs)
        if (length(add)) {
          levs <- c(levs, add)
          x[[col]] <- factor(as.character(x[[col]]), levels = levs)
          template[[col]] <- factor(as.character(template[[col]]), levels = levs)
        }
        dummy1[[col]] <- factor(syn1, levels = levs)
        dummy2[[col]] <- factor(syn2, levels = levs)
        logf(sprintf("[boundary][dummy] %s factor all NA -> (%s, %s)", col, syn1, syn2))
      }
      next
    }

    # Numeric-like (numeric/integer/integer64/Date/POSIXct)
    if (is_num[col]) {
      # integer64?
      if (is_integer64(v)) {
        if (requireNamespace("bit64", quietly = TRUE)) {
          dummy1[[col]] <- bit64::as.integer64(-100)
          dummy2[[col]] <- bit64::as.integer64(100)
        } else {
          # Fallback numeric (will lose integer64 class)
            dummy1[[col]] <- -100
            dummy2[[col]] <- 100
        }
        logf(sprintf("[boundary][dummy] %s integer64 -> (-100, 100)", col))
      } else if (is.integer(v)) {
        dummy1[[col]] <- as.integer(-100)
        dummy2[[col]] <- as.integer(100)
        logf(sprintf("[boundary][dummy] %s integer -> (-100, 100)", col))
      } else if (is_date(v)) {
        dummy1[[col]] <- date_dummy1
        dummy2[[col]] <- date_dummy2
        logf(sprintf("[boundary][dummy] %s Date -> (%s, %s)", col,
                     as.character(date_dummy1), as.character(date_dummy2)))
      } else if (is_posix(v)) {
        tz <- attr(v, "tzone")
        dummy1[[col]] <- as.POSIXct(0, origin = "1970-01-01", tz = tz)
        dummy2[[col]] <- as.POSIXct(100, origin = "1970-01-01", tz = tz)
        logf(sprintf("[boundary][dummy] %s POSIXct -> (%s, %s)", col,
                     as.character(dummy1[[col]]), as.character(dummy2[[col]])))
      } else if (is.numeric(v)) { # double
        dummy1[[col]] <- 0
        dummy2[[col]] <- 100
        logf(sprintf("[boundary][dummy] %s numeric -> (0, 100)", col))
      }
      next
    }

    # Other / unsupported: duplicate template (may not yield distinct)
    logf(sprintf("[boundary][dummy] %s other/unhandled -> (template, template)", col))
  }

  rows_list <- list(dummy1, dummy2)

  # ---- Numeric min/max rows (optional) ---------------------------
  if (include_numeric && length(num_cols)) {
    row_min <- data.table::copy(template)
    row_max <- data.table::copy(template)

    for (col in num_cols) {
      v <- x[[col]]
      v_no_na <- v[!is.na(v)]
      if (!length(v_no_na)) next

      vmin <- suppressWarnings(min(v_no_na))
      vmax <- suppressWarnings(max(v_no_na))

      if (is_date(v)) {
        vmin_r <- as.Date(floor_to_base(as.numeric(vmin), round_base), origin = "1970-01-01")
        vmax_r <- as.Date(ceil_to_base(as.numeric(vmax),  round_base), origin = "1970-01-01")
      } else if (is_posix(v)) {
        vmin_r <- as.POSIXct(floor_to_base(as.numeric(vmin), round_base), origin = "1970-01-01", tz = attr(v, "tzone"))
        vmax_r <- as.POSIXct(ceil_to_base(as.numeric(vmax),  round_base), origin = "1970-01-01", tz = attr(v, "tzone"))
      } else if (is_integer64(v)) {
        vmin_r <- suppressWarnings(floor_to_base(vmin, round_base))
        vmax_r <- suppressWarnings(ceil_to_base(vmax,  round_base))
      } else {
        vmin_r <- suppressWarnings(floor_to_base(vmin, round_base))
        vmax_r <- suppressWarnings(ceil_to_base(vmax,  round_base))
      }

      data.table::set(row_min, j = col, value = vmin_r)
      data.table::set(row_max, j = col, value = vmax_r)
    }

    rows_list <- c(rows_list, list(row_min, row_max))
  }

  # ---- Factor level rows (optional) ------------------------------
  if (include_factor && length(fac_cols)) {
    for (col in fac_cols) {
      levs <- levels(x[[col]])
      for (lv in levs) {
        r <- data.table::copy(template)
        r[[col]] <- factor(lv, levels = levs)
        rows_list <- c(rows_list, list(r))
      }
    }
  }

  # ---- Combine boundary rows -------------------------------------
  boundary <- data.table::rbindlist(rows_list, use.names = TRUE, fill = TRUE)
  data.table::setcolorder(boundary, names(x))

  # ---- Padding logic ---------------------------------------------
  s <- as.numeric(slice_pct)
  if (!is.finite(s) || s <= 0) s <- 100
  if (s > 100) s <- 100

  nb <- nrow(boundary)
  n_required <- ceiling(nb * 100 / s)
  k_needed <- max(0L, n_required - nb)

  logf(sprintf("[boundary] boundary_rows=%d | slice_pct=%.2f | total_required=%d | filler_needed=%d",
               nb, s, n_required, k_needed))

  if (k_needed > 0) {
    n_src <- nrow(x)
    idx <- rep(seq_len(n_src), length.out = k_needed)
    filler <- x[idx, , drop = FALSE]
    out <- data.table::rbindlist(list(boundary, filler), use.names = TRUE, fill = TRUE)
  } else {
    out <- boundary
  }

  # ---- Append .row_id --------------------------------------------
  if (add_row_id) {
    if (".row_id" %in% names(out)) out[, .row_id := NULL]
    out[, .row_id := .I]
    data.table::setcolorder(out, c(setdiff(names(out), ".row_id"), ".row_id"))
  }

  logf(sprintf("[boundary] final_rows=%d | cols=%d", nrow(out), ncol(out)))
  out
}

nthreads <- 8  # Number of threads for data.table operations

#' Utility function to load local database files for PanelApp, VEP consequences, and Phenotype data.
#' Reads the database directory from the configuration file and loads the latest available database files.
#' @return A list containing paths to the local database files.
load_local_db <- function() {
  conf_file <- system.file("extdata", "app.conf", package = "puzzleapp")
  db_dir <- NULL
  
  # --- Read db_dir from app.conf ---
  if (file.exists(conf_file)) {
    lines <- readLines(conf_file)
    db_dir_line <- grep('^db_dir\\s*=\\s*".*"$', lines, value = TRUE)
    if (length(db_dir_line) > 0) {
      db_dir <- sub('^db_dir\\s*=\\s*"', "", db_dir_line[1])
      db_dir <- sub('"$', "", db_dir)
    }
  }

  # --- Find the latest month_year directory ---
  if (!is.null(db_dir) && dir.exists(db_dir)) {
    dirs <- list.dirs(db_dir, full.names = TRUE, recursive = FALSE)
    if (length(dirs) > 0) {
      dir_dates <- sapply(basename(dirs), function(d) {
        parts <- strsplit(d, "_")[[1]]
        if (length(parts) == 2) {
          month <- match(tolower(parts[1]), tolower(month.name))
          year <- suppressWarnings(as.integer(parts[2]))
          if (!is.na(month) && !is.na(year)) {
            return(as.Date(sprintf("%04d-%02d-01", year, month)))
          }
        }
        return(NA)
      })

      # Keep only valid parsed directories
      valid_idx <- which(!is.na(dir_dates))
      if (length(valid_idx) > 0) {
        newest_dir <- dirs[valid_idx[which.max(dir_dates[valid_idx])]]
        db_dir <- newest_dir
      } else {
        warning("No valid month_year directories found under ", db_dir)
      }
    } else {
      warning("No subdirectories found under ", db_dir)
    }
  } else {
    warning("db_dir not found or invalid in configuration.")
  }

  cat("Using local DB directory:", db_dir, "\n")
  if (is.null(db_dir) || !dir.exists(db_dir)) {
    stop("Local DB directory does not exist: ", db_dir)
  }
  db_list <- list(
    panel_app = file.path(db_dir, "all_panel_app.tsv"),
    vep_consequences = file.path(db_dir, "vep_annotations.tsv"),
    phenotype_data = file.path(db_dir, "phenotype_to_genes.txt")
  )
  # Verify files exist
  for (file in db_list) {
    if (!file.exists(file)) {
      stop("Required file does not exist: ", file)
    }
  }
  db_list
}

load_vep_consequences <- function(file) {
  stopifnot(file.exists(file))
  # Read the file
  dt <- data.table::fread(file = file, header = TRUE, nThread = nthreads)
  # Debug print removed for production use
  return(dt)
}

#' Load Phenotype Data
#'
#' Utility function to load phenotype-to-genes data from a TSV file.
#'
#' @param file Path to the phenotype TSV file. If NULL, loads the default file from the package.
#' @return A data frame containing phenotype-to-gene mappings, typically with columns such as phenotype ID, phenotype name, gene symbol, and gene ID.
load_phenotype_data <- function(file) {
  stopifnot(file.exists(file))
  # Read the file
  dt <- fread(file, header = TRUE, nThread = nthreads)
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
load_panel_app_data <- function(file) {
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
      snvs_data <- data.table::fread(input$snvs_tsv, nThread = nthreads)
      snvs_data <- add_extra_columns(snvs_data)
      default_dt <- make_boundary_table(snvs_data, round_base = 10, slice_pct = 100, add_row_id = TRUE)
      snvs_data <- add_row_id(snvs_data)
    } else {
      # shiny::showNotification("SNVs & Indels TSV file not found.", type = "error")
      messages <- c(messages, "SNVs & Indels TSV file not found.")
    }
  }

  # SVs
  svs_data <- NULL
  if (!is.null(input$svs_tsv) && nzchar(input$svs_tsv)) {
    if (file.exists(input$svs_tsv)) {
      svs_data <- data.table::fread(input$svs_tsv, nThread = nthreads)
      svs_data <- add_extra_columns(svs_data)
      svs_data <- add_row_id(svs_data)
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
    phenotype_data = phenotype_data,
    default_dt = default_dt
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

bump_version <- function(version_type = c("data", "panelapp"), shared_rx) {
  if (version_type == "data") {
    shared_rx$data_version(shared_rx$data_version() + 1L)
  } else if (version_type == "panelapp") {
    shared_rx$panelapp_version(shared_rx$panelapp_version() + 1L)
  }
}


add_row_id <- function(df) {
  df$.row_id <- seq_len(nrow(df))
  df
}

v_kinships <- c("proband", "mother", "father", "sibling", "brother", "sister", "uncle", "aunt", "grandparent", "grandfather", "grandmother", "unknown")
v_statuses <- c("affected", "unaffected", "unknown")
v_sexes    <- c("male", "female", "unknown")

sanitise_samples <- function(samples) {
  for (i in seq_along(samples)) {
    s <- samples[[i]]
    # Sanitize kinship
    if (!is.null(s$kinship) && !(s$kinship %in% v_kinships)) {
      log_debug(sprintf("Sanitizing kinship for sample %s: %s -> unknown", s$sample_id %||% paste0("index_", i), s$kinship))
      samples[[i]]$kinship <- "unknown"
    }
    # Sanitize status
    if (!is.null(s$status) && !(s$status %in% v_statuses)) {
      log_debug(sprintf("Sanitizing status for sample %s: %s -> unknown", s$sample_id %||% paste0("index_", i), s$status))
      samples[[i]]$status <- "unknown"
    }
    # Sanitize sex
    if (!is.null(s$sex) && !(s$sex %in% v_sexes)) {
      log_debug(sprintf("Sanitizing sex for sample %s: %s -> unknown", s$sample_id %||% paste0("index_", i), s$sex))
      samples[[i]]$sex <- "unknown"
    }
  }
  samples
}