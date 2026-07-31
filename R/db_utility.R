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
#     * Numeric (double): 0, 100 (unless overridden by data_ranges)
#     * Integer:    0L, 100L (unless overridden by data_ranges)
#     * integer64:  0, 100 (as integer64)  (treated like integer)
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
#   x, round_base, slice_pct, include_numeric, include_factor, data_ranges,
#   add_row_id, verbose, logger
#
# data_ranges (optional):
#   A data.frame/data.table with columns: name (character), min, max.
#   When provided, for INTEGER and DOUBLE columns whose names match `name`,
#   the TWO DUMMY ROW values will be set to (min, max) for that column.
#   The numeric min/max rows (if include_numeric=TRUE) will also use these values.
#   Non-numeric (int64, Date, POSIXct, factor, logical, character) columns are ignored.
#
# Returns:
#   data.table with:
#     1. dummy1 row
#     2. dummy2 row
#     3. numeric min/max rows (optional; overridden by data_ranges for int/double)
#     4. factor level rows (optional)
#     5. padding rows
#   .row_id appended last (creation order).
#
# Logging tags:
#   [boundary][dummy] detail of dummy assignments
#   [boundary][ranges] detail of data_ranges overrides used/ignored
#   Existing tags preserved.

make_boundary_table <- function(x,
                                round_base = 10,
                                slice_pct = 10,
                                include_numeric = TRUE,
                                include_factor = TRUE,
                                data_ranges = NULL,
                                add_row_id = TRUE,
                                verbose = FALSE,
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

  num_cols   <- cols[is_num]
  fac_cols   <- cols[is_fac]
  char_cols  <- cols[is_char]
  other_cols <- setdiff(cols, c(num_cols, fac_cols, char_cols))

  logf(sprintf("[boundary] Columns: numeric-like=%s | factor=%s | other=%s",
               paste(num_cols, collapse = ", "),
               paste(fac_cols, collapse = ", "),
               paste(other_cols, collapse = ", ")))

  # ---- Prepare data_ranges overrides (ints and doubles only) -----
  ranges_dt <- NULL
  numeric_overridable <- vapply(x, function(v) is.numeric(v) && !is_integer64(v), logical(1))
  numeric_overridable_names <- names(x)[numeric_overridable]

  if (!is.null(data_ranges)) {
    provided <- if (!inherits(data_ranges, "data.table")) {
      data.table::as.data.table(data_ranges)
    } else {
      data.table::copy(data_ranges)
    }

    required_cols <- c("name", "min", "max")
    if (!all(required_cols %in% names(provided))) {
      logf(sprintf("[boundary][ranges] Ignoring data_ranges: missing columns, found=%s need=%s",
                   paste(names(provided), collapse = ","), paste(required_cols, collapse = ",")))
    } else {
      provided[, name := as.character(name)]
      provided <- provided[!is.na(name)]
      provided_names <- provided$name
      not_in_x <- setdiff(provided_names, cols)
      non_numeric <- intersect(intersect(provided_names, cols), setdiff(cols, numeric_overridable_names))
      if (length(not_in_x)) {
        logf(sprintf("[boundary][ranges] Names not in `x` ignored: %s", paste(not_in_x, collapse = ", ")))
      }
      if (length(non_numeric)) {
        logf(sprintf("[boundary][ranges] Non (int/double) columns ignored: %s", paste(non_numeric, collapse = ", ")))
      }
      ranges_dt <- provided[name %in% numeric_overridable_names]
      if (nrow(ranges_dt)) {
        ranges_dt <- ranges_dt[!duplicated(name)]
        data.table::setkey(ranges_dt, name)
        logf(sprintf("[boundary][ranges] Will override min/max for: %s", paste(ranges_dt$name, collapse = ", ")))
      } else {
        logf("[boundary][ranges] data_ranges provided but none applicable (int/double columns).")
        ranges_dt <- NULL
      }
    }
  }

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
          v_max_ceil  <- as.POSIXct(ceil_to_base(as.numeric(v_max_raw),  round_base), origin = "1970-01-01", tz = attr(v, "tzone"))
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
      if (n_used >= 2) {
        l1 <- v_non_na_levels[1]
        l2 <- v_non_na_levels[2]
        dummy1[[col]] <- factor(l1, levels = levels(v))
        dummy2[[col]] <- factor(l2, levels = levels(v))
        logf(sprintf("[boundary][dummy] %s factor >=2 levels -> (%s, %s)", col, l1, l2))
      } else if (n_used == 1) {
        existing <- v_non_na_levels[1]
        syn2 <- paste0("dummy2_", col)
        levs <- levels(v)
        if (!(syn2 %in% levs)) {
          levs <- c(levs, syn2)
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
      # integer64: never overridden by data_ranges (ints/doubles only)
      if (is_integer64(v)) {
        if (requireNamespace("bit64", quietly = TRUE)) {
          dummy1[[col]] <- bit64::as.integer64(0)
          dummy2[[col]] <- bit64::as.integer64(100)
        } else {
          dummy1[[col]] <- 0
          dummy2[[col]] <- 100
        }
        logf(sprintf("[boundary][dummy] %s integer64 -> (0, 100)", col))
      } else if (is.integer(v)) {
        # Use data_ranges if provided
        used_override <- FALSE
        if (!is.null(ranges_dt) && (col %in% ranges_dt$name)) {
          rng <- ranges_dt[col]
          if (nrow(rng) == 1) {
            rmin <- rng$min
            rmax <- rng$max
            if (!is.na(rmin) && !is.na(rmax)) {
              if (is.numeric(rmin) && is.numeric(rmax) && rmin > rmax) {
                tmp <- rmin; rmin <- rmax; rmax <- tmp
              }
              dummy1[[col]] <- as.integer(rmin)
              dummy2[[col]] <- as.integer(rmax)
              used_override <- TRUE
              logf(sprintf("[boundary][dummy][ranges] %s integer -> (%s, %s)",
                           col, as.character(dummy1[[col]]), as.character(dummy2[[col]])))
            }
          }
        }
        if (!used_override) {
          dummy1[[col]] <- as.integer(0)
          dummy2[[col]] <- as.integer(100)
          logf(sprintf("[boundary][dummy] %s integer -> (0, 100)", col))
        }
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
        used_override <- FALSE
        if (!is.null(ranges_dt) && (col %in% ranges_dt$name)) {
          rng <- ranges_dt[col]
          if (nrow(rng) == 1) {
            rmin <- rng$min
            rmax <- rng$max
            if (!is.na(rmin) && !is.na(rmax)) {
              if (is.numeric(rmin) && is.numeric(rmax) && rmin > rmax) {
                tmp <- rmin; rmin <- rmax; rmax <- tmp
              }
              dummy1[[col]] <- as.numeric(rmin)
              dummy2[[col]] <- as.numeric(rmax)
              used_override <- TRUE
              logf(sprintf("[boundary][dummy][ranges] %s numeric -> (%s, %s)",
                           col, as.character(dummy1[[col]]), as.character(dummy2[[col]])))
            }
          }
        }
        if (!used_override) {
          dummy1[[col]] <- 0
          dummy2[[col]] <- 100
          logf(sprintf("[boundary][dummy] %s numeric -> (0, 100)", col))
        }
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

      # If data_ranges provided for this int/double column, use it
      used_override <- FALSE
      if (!is.null(ranges_dt) && (col %in% ranges_dt$name) && is.numeric(v) && !is_integer64(v) && !is_date(v) && !is_posix(v)) {
        rng <- ranges_dt[col]
        if (nrow(rng) == 1) {
          rmin <- rng$min
          rmax <- rng$max
          if (!is.na(rmin) && !is.na(rmax)) {
            if (is.numeric(rmin) && is.numeric(rmax) && rmin > rmax) {
              logf(sprintf("[boundary][ranges] %s has min > max in data_ranges; swapping (%s, %s) -> (%s, %s)",
                           col, as.character(rmin), as.character(rmax), as.character(rmax), as.character(rmin)))
              tmp <- rmin; rmin <- rmax; rmax <- tmp
            }
            if (is.integer(v)) {
              vmin_r <- as.integer(rmin); vmax_r <- as.integer(rmax)
            } else {
              vmin_r <- as.numeric(rmin); vmax_r <- as.numeric(rmax)
            }
            data.table::set(row_min, j = col, value = vmin_r)
            data.table::set(row_max, j = col, value = vmax_r)
            logf(sprintf("[boundary][ranges] %s override -> min=%s max=%s",
                         col, as.character(vmin_r), as.character(vmax_r)))
            used_override <- TRUE
          } else {
            logf(sprintf("[boundary][ranges] %s override skipped due to NA min/max", col))
          }
        }
      }
      if (used_override) next

      # Fall back to observed min/max with floor/ceil
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

# Utility function to load local database files for PanelApp, VEP consequences, and Phenotype data.
# Reads the database directory from the configuration file using a given key
# and returns the full path to the requested file.
load_local_db <- function(key, file_name) {
  conf_file <- system.file("extdata", "app.conf", package = "puzzleapp")

  # --- Resolve the directory: option first, then the shipped app.conf --------
  # app.conf lives inside the installed package, so editing it per machine is
  # awkward and its value cannot be portable. An R option lets each user or
  # site point at their own copy without touching the install, e.g. in
  # ~/.Rprofile or before run_app():
  #   options(puzzleapp.panelapp_db_dir = "/g/data/if89/.../db/panelapp")
  opt_name <- paste0("puzzleapp.", key, "_db_dir")
  db_dir <- getOption(opt_name)
  source_desc <- sprintf("option %s", opt_name)

  if (is.null(db_dir) || !nzchar(db_dir)) {
    if (!file.exists(conf_file)) {
      stop("Configuration file not found: ", conf_file)
    }

    # --- Read config file lines ---
    lines <- readLines(conf_file)

    # --- Construct the pattern for this key ---
    pattern <- sprintf("^%s_db_dir\\s*=\\s*\".*\"$", key)
    db_line <- grep(pattern, lines, value = TRUE)

    if (length(db_line) == 0) {
      stop(sprintf("No %s_db_dir entry in %s, and option %s is unset.",
                   key, conf_file, opt_name))
    }

    # --- Extract db_dir path ---
    db_dir <- sub(sprintf("^%s_db_dir\\s*=\\s*\"", key), "", db_line[1])
    db_dir <- sub("\"$", "", db_dir)
    source_desc <- sprintf("%s_db_dir in %s", key, conf_file)
  }

  # --- Handle relative paths like 'extdata/db/vep_consequences' ---
  if (!dir.exists(db_dir)) {
    pkg_path <- system.file(package = "puzzleapp")
    possible_dir <- file.path(pkg_path, db_dir)
    if (dir.exists(possible_dir)) {
      db_dir <- possible_dir
    } else {
      stop(sprintf(
        "%s database directory does not exist: %s (from %s). Point it elsewhere with options(%s = \"/path/to/%s\") before run_app().",
        key, db_dir, source_desc, opt_name, key))
    }
  }
  log_info(sprintf("[load_local_db] %s directory resolved from %s: %s", key, source_desc, db_dir))

  # --- Handle panelapp/phenotype case: pick latest month_year subdirectory ---
  subdirs <- list.dirs(db_dir, full.names = TRUE, recursive = FALSE)
  if (length(subdirs) > 0) {
    dir_dates <- sapply(basename(subdirs), function(d) {
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

    valid_idx <- which(!is.na(dir_dates))
    if (length(valid_idx) > 0) {
      db_dir <- subdirs[valid_idx[which.max(dir_dates[valid_idx])]]
    }
  }

  db_path <- file.path(db_dir, file_name)
  if (!file.exists(db_path)) {
    stop("Required file does not exist: ", db_path)
  }

  message("Using local DB file: ", db_path)
  return(db_path)
}

expand_ranges_by_code <- function(data_ranges_dt, samples, names_to_expand) {
  stopifnot(is.data.table(data_ranges_dt))
  # extract unique, non-NA codes
  codes <- unique(vapply(samples, function(s) s$code, numeric(1)))
  codes <- codes[!is.na(codes)]
  if (length(codes) == 0)
    return(copy(data_ranges_dt))
  # rows that need expanding
  to_expand <- data_ranges_dt[name %in% names_to_expand]
  if (nrow(to_expand) == 0)
    return(copy(data_ranges_dt))
  # expand by code values (not sequence index)
  expanded <- to_expand[
    , .(code = codes), by = .(name, min, max)
  ][
    , name := paste0(name, "_", code)
  ][
    , code := NULL
  ]
  # combine back
  rbind(data_ranges_dt[!name %in% names_to_expand], expanded, use.names = TRUE)
}

collect_inputs <- function(input, add_svlog_columns = FALSE, svlog_db = NULL) {
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
  issues <- puzzlecore_check_pedigree_sanity(samples)
  if (length(issues) > 0) {
    for (msg in issues) {
      messages <- c(messages, paste("Pedigree issue:", msg))
    }
    return(list(messages = messages))
  }
  cat("Collected sample info for", length(samples), "individuals.\n")
  cat("Samples:", str(samples), "\n")

  # Convert the list of samples into a data.table
  pedigree_data <- tryCatch({
    rbindlist(lapply(samples, as.data.table), fill = TRUE)
  }, error = function(e) {
    messages <<- c(messages, paste("Error processing pedigree data:", e$message))
    return(list(messages = messages))
  })

  # SNVs
  snvs_data <- NULL
  snv_default_dt <- NULL
  if (!is.null(input$snvs_tsv) && nzchar(input$snvs_tsv)) {
    if (file.exists(input$snvs_tsv)) {
      snvs_data <- puzzlecore_read_variant_tsv(input$snvs_tsv, nthreads = nthreads)
      data_ranges_dt <- fread(system.file("extdata", "db", "table_schema", "snv_ranges.tsv", package = "puzzleapp"), nThread = nthreads)
      data_ranges_dt <- expand_ranges_by_code(data_ranges_dt, samples, names_to_expand = c("VAF", "alt_allele_count"))
      snv_default_dt <- make_boundary_table(snvs_data, round_base = 10, slice_pct = 100, add_row_id = TRUE, data_ranges = data_ranges_dt)
      snvs_data <- add_row_id(snvs_data)
    } else {
      # shiny::showNotification("SNVs & Indels TSV file not found.", type = "error")
      messages <- c(messages, "SNVs & Indels TSV file not found.")
      return(list(messages = messages))
    }
  } else {
    snvs_data <- data.table::data.table()
    snv_default_dt <- data.table::data.table()
  }

  # SVs
  svs_data <- NULL
  sv_default_dt <- NULL
  if (!is.null(input$svs_tsv) && nzchar(input$svs_tsv)) {
    if (file.exists(input$svs_tsv)) {
      svs_data <- puzzlecore_read_variant_tsv(input$svs_tsv, nthreads = nthreads, snv = FALSE, add_svlog_columns = add_svlog_columns, svlog_db = svlog_db)
      data_ranges_dt <- fread(system.file("extdata", "db", "table_schema", "sv_ranges.tsv", package = "puzzleapp"), nThread = nthreads)
      data_ranges_dt <- expand_ranges_by_code(data_ranges_dt, samples, names_to_expand = c("VAF", "alt_allele_count"))
      sv_default_dt <- make_boundary_table(svs_data, round_base = 10, slice_pct = 100, add_row_id = TRUE, data_ranges = data_ranges_dt)
      svs_data <- add_row_id(svs_data)
    } else {
      # shiny::showNotification("SVs TSV file not found.", type = "error")
      messages <- c(messages, "SVs TSV file not found.")
      return(list(messages = messages))
    }
  } else {
    svs_data <- data.table::data.table()
    sv_default_dt <- data.table::data.table()
  }

  # PanelApp
  panel_app_data <- NULL
  panel_app_default_dt <- NULL
  if (!is.null(input$panel_app) && nzchar(input$panel_app)) {
    cat("input$panel_app:", input$panel_app, "\n")
    if (file.exists(input$panel_app)) {
      panel_app_data <- puzzlecore_load_panel_app_data(file = input$panel_app)
      panel_app_default_dt <- make_boundary_table(panel_app_data, round_base = 10, slice_pct = 100, add_row_id = TRUE)
      panel_app_data <- add_row_id(panel_app_data)
    } else {
      # shiny::showNotification("PanelApp TSV file not found.", type = "error")
      messages <- c(messages, "PanelApp TSV file not found.")
      return(list(messages = messages))
    }
  } else {
    messages <- c(messages, "PanelApp TSV file not provided.")
    return(list(messages = messages))
  }

  # Phenotype
  phenotype_data <- NULL
  phenotype_default_dt <- NULL
  if (!is.null(input$phenotype_data) && nzchar(input$phenotype_data)) {
    if (file.exists(input$phenotype_data)) {
      phenotype_data <- puzzlecore_load_phenotype_data(file = input$phenotype_data)
      phenotype_default_dt <- make_boundary_table(phenotype_data, round_base = 10, slice_pct = 100, add_row_id = TRUE)
      phenotype_data <- add_row_id(phenotype_data)
    } else {
      # shiny::showNotification("Human Phenotype Ontology TSV file not found.", type = "error")
      messages <- c(messages, "Human Phenotype Ontology TSV file not found.")
      return(list(messages = messages))
    }
  } else {
    messages <- c(messages, "Human Phenotype Ontology TSV file not provided.")
    return(list(messages = messages))
  }

  # SNVs vcf
  snvs_vcf <- input$snvs_vcf
  svs_vcf <- input$svs_vcf

  # Return everything as a list
  list(
    messages = messages,
    samples = samples,
    pedigree = pedigree_data,
    snvs_data = snvs_data,
    svs_data = svs_data,
    snv_default_dt = snv_default_dt,
    sv_default_dt = sv_default_dt,
    panel_app_data = panel_app_data,
    panel_app_default_dt = panel_app_default_dt,
    phenotype_data = phenotype_data,
    phenotype_default_dt = phenotype_default_dt,
    snvs_vcf = snvs_vcf,
    svs_vcf = svs_vcf
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

bump_version <- function(version_type = c("data", "panelapp", "igv", "genesymbol", "hpoid", "qcplot"), shared_rx) {
  if (version_type == "data") {
    shared_rx$data_version(shared_rx$data_version() + 1L)
  } else if (version_type == "panelapp") {
    shared_rx$panelapp_version(shared_rx$panelapp_version() + 1L)
  } else if (version_type == "igv") {
    shared_rx$igv_version(shared_rx$igv_version() + 1L)
  } else if (version_type == "genesymbol") {
    shared_rx$genesymbol_version(shared_rx$genesymbol_version() + 1L)
  } else if (version_type == "hpoid") {
    shared_rx$hpoid_version(shared_rx$hpoid_version() + 1L)
  } else if (version_type == "qcplot") {
    shared_rx$qcplot_version(shared_rx$qcplot_version() + 1L)
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

create_work_dir <- function(work_dir, sticky = TRUE) {
  # Ensure last dir is ".puzzleapp"
  if (basename(work_dir) != ".puzzleapp") {
    work_dir <- file.path(work_dir, ".puzzleapp")
  }
  
  # -------------------------------
  # 1. .puzzleapp directory
  # -------------------------------
  # - Only owner can read/write/execute (0700)
  # - No one else can create files or subdirs here
  if (!dir.exists(work_dir)) {
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    Sys.chmod(work_dir, mode = "0750", use_umask = FALSE)
  }
  
  # -------------------------------
  # 2. saved_filters directory
  # -------------------------------
  # Safe shared folder for storing files
  # Permissions if sticky = TRUE:
  # - Owner: rwx
  # - Group: rwx
  # - Others: none
  # - Sticky bit (+t): users cannot delete/rename files owned by others
  # - Setgid (+s): new files inherit the directory's group
  saved_filters_dir <- file.path(work_dir, "saved_filters")
  if (!dir.exists(saved_filters_dir)) {
    dir.create(saved_filters_dir, recursive = TRUE, showWarnings = FALSE)
    if (sticky) {
      # Sys.chmod(saved_filters_dir, mode = "2770", use_umask = FALSE) # setgid
      # Sys.chmod(saved_filters_dir, mode = "1770", use_umask = FALSE) # sticky bit
      Sys.chmod(saved_filters_dir, mode = "3770", use_umask = FALSE)
    }
  }
  
  # -------------------------------
  # 3. saved_sessions directory
  # -------------------------------
  # Safe shared folder for storing subdirectories
  # Permissions if sticky = TRUE:
  # - Owner: rwx
  # - Group: rwx
  # - Others: none
  # - Sticky bit (+t): users cannot delete/rename subdirs owned by others
  # - Setgid (+s): new subdirs inherit the directory's group
  saved_sessions_dir <- file.path(work_dir, "saved_sessions")
  if (!dir.exists(saved_sessions_dir)) {
    dir.create(saved_sessions_dir, recursive = TRUE, showWarnings = FALSE)
    if (sticky) {
      # Sys.chmod(saved_sessions_dir, mode = "2770", use_umask = FALSE) # setgid
      # Sys.chmod(saved_sessions_dir, mode = "1770", use_umask = FALSE) # sticky bit
      Sys.chmod(saved_sessions_dir, mode = "3770", use_umask = FALSE)
    }
  }
  
  # Return paths for convenience
  invisible(list(
    work_dir = work_dir,
    saved_filters_dir = saved_filters_dir,
    saved_sessions_dir = saved_sessions_dir
  ))
}


# Helper: create a directory with optional sticky/setgid permissions
create_safe_dir <- function(dir_path, sticky = TRUE) {
  log_info(sprintf("Creating safe directory: %s\n", dir_path))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    if (.Platform$OS.type == "unix" && sticky) {
      # Convert desired permissions to octal
      # owner rwx = 7, group rwx = 7, others none = 0
      # sticky + setgid = 1 + 2 = 3 in leading digit
      # Example: sticky+setgid + rwxrwx--- = 3770
      Sys.chmod(dir_path, mode = "3770", use_umask = FALSE)
      log_info(sprintf("Set sticky and setgid permissions on: %s\n", dir_path))
    }
  }
  return(dir_path)
}
