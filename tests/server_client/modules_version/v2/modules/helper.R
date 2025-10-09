# helpers.R
# Build a boundary table from any data.frame/data.table and pad it so that
# a given initial slice (e.g., 10%) fully shows the boundary rows at the top.
#
# Updates:
# - Numeric-like columns now use floor-to-base for the min and ceil-to-base for the max.
# - Detailed per-column diagnostics printed (min/max raw and floored/ceiled; factor levels; sample for others).
# - NEW: Always prepend a "dummy" row derived from the first row:
#     * Character columns are prefixed with "dummy" (NA becomes "dummyNA").
#     * Numeric-like, factor, and other columns are left unchanged from the first row.
#
# Behavior:
# - The first boundary row is the "dummy" row as described above.
# - Numeric-like (numeric, integer64, Date, POSIXt):
#     * Two rows: one with each numeric-like column set to floor(min, base),
#       and one with each set to ceil(max, base). Non-numeric columns take values
#       from the first row of the input.
# - Factor:
#     * For each factor column, one row per level for that column, with other columns
#       taken from the first row. Levels preserved.
# - If both numeric-like and factor exist, include both sets (duplicates allowed).
# - Pads with rows from the input so that floor(slice_pct/100 * nrow(result)) >= n_boundary.
# - Appends .row_id as the last column in creation order (boundary rows first).
#
# Parameters:
# - x: data.frame or data.table
# - round_base: base step for floor/ceil (default 10). For Date this is days; for POSIXct it's seconds.
# - slice_pct: percentage (0 < slice_pct <= 100) used by your initial slice in the Data module.
# - include_numeric/include_factor: toggle boundary components.
# - add_row_id: append .row_id last.
# - verbose: print diagnostics.
# - logger: function used for logging (default cat).
#
# Returns: data.table with .row_id last.

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

  # Local helpers
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
  is_date <- function(v) inherits(v, "Date")
  is_posix <- function(v) inherits(v, "POSIXct") || inherits(v, "POSIXt")
  is_numeric_like <- function(v) is.numeric(v) || is_integer64(v) || is_date(v) || is_posix(v)

  # Empty input => preserve structure
  if (nrow(x) == 0) {
    out <- data.table::copy(x)[0]
    if (add_row_id) {
      if (!(".row_id" %in% names(out))) out[, .row_id := integer()]
      data.table::setcolorder(out, c(setdiff(names(out), ".row_id"), ".row_id"))
    }
    logf("[boundary] Input has 0 rows; returning empty with same columns.")
    return(out)
  }

  # Use first row as template for non-target columns
  template <- data.table::copy(x[1])

  # Identify column types
  cols <- names(x)
  is_num <- vapply(x, is_numeric_like, logical(1))
  is_fac <- vapply(x, is.factor, logical(1))
  is_char <- vapply(x, is.character, logical(1))

  num_cols <- cols[is_num]
  fac_cols <- cols[is_fac]
  char_cols <- cols[is_char]
  other_cols <- setdiff(cols, c(num_cols, fac_cols))

  logf(sprintf("[boundary] Columns: numeric-like=%s | factor=%s | other=%s",
               paste(num_cols, collapse = ", "), paste(fac_cols, collapse = ", "),
               paste(other_cols, collapse = ", ")))

  # Per-column diagnostics
  for (col in cols) {
    v <- x[[col]]
    cls <- paste(class(v), collapse = ",")
    if (col %in% num_cols) {
      v_no_na <- v[!is.na(v)]
      if (length(v_no_na)) {
        v_min_raw <- suppressWarnings(min(v_no_na))
        v_max_raw <- suppressWarnings(max(v_no_na))
        # floor/ceil to base, handling Date/POSIX separately
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
                     col, cls, as.character(v_min_raw), as.character(v_max_raw),
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

  rows_list <- list()

  # Always prepend a "dummy" row derived from the first row
  dummy_row <- data.table::copy(template)
  if (length(char_cols)) {
    for (col in char_cols) {
      # Prefix "dummy" to character columns; NA becomes "dummyNA"
      dummy_row[[col]] <- paste0("dummy", as.character(dummy_row[[col]]))
    }
  }
  rows_list <- c(rows_list, list(dummy_row))
  logf(sprintf("[boundary] Added dummy row with prefixed character columns: %s",
               if (length(char_cols)) paste(char_cols, collapse = ", ") else "<none>"))

  # Numeric min/max rows using floor/ceil
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

  # Factor-level rows: one row per level for each factor column
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

  # If nothing was added beyond the dummy row, keep current rows_list as-is
  boundary <- data.table::rbindlist(rows_list, use.names = TRUE, fill = TRUE)

  # Restore original column order
  data.table::setcolorder(boundary, names(x))

  # Compute padding so the first slice shows all boundary rows
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

  # .row_id last, preserving creation order (boundary first)
  if (add_row_id) {
    if (".row_id" %in% names(out)) out[, .row_id := NULL]
    out[, .row_id := .I]
    data.table::setcolorder(out, c(setdiff(names(out), ".row_id"), ".row_id"))
  }

  logf(sprintf("[boundary] final_rows=%d | cols=%d", nrow(out), ncol(out)))
  out
}