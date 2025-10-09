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