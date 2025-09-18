# dataset_editing.R (dataset_specific.R)
# Central place for dataset-specific editing rules, validation, and dynamic row highlighting JS.
#
# Implements:
#   A. PRIORITY is now editable (added to editable columns list).
#   B. Stricter / friendlier validation for PRIORITY:
#        - Accept only pure integer strings: ^[+-]?[0-9]+$
#        - Reject (do NOT coerce) floats, empty strings, non-digits -> edit refused with message.
#        - Optional: allow a single "." or blank to mean "clear" (currently DISABLED; see comment).
#   C. dataset_after_edit derives PRIORITYFlag from PRIORITY:
#        PRIORITY > 0  -> TRUE
#        PRIORITY < 0  -> FALSE
#        PRIORITY == 0 or NA -> NA
#   D. PRIORITYFlag is protected (not editable; attempts are rejected).
#
# Existing highlight rules (unchanged):
#   - Sepal.Width == 3              -> row-sw3
#   - spliceai_override == TRUE     -> row-spliceai-override
#   - clinvar_override == TRUE      -> row-clinvar-override
#   - PRIORITYFlag TRUE/FALSE       -> row-priority-true / row-priority-false (NA = no priority color)
#
# NOTE:
#   - If PRIORITYFlag column does not exist but PRIORITY does, after_edit will create it on-the-fly.
#   - Adjust MAX_ABS_PRIORITY if you want to constrain magnitude.
#
# ------------------------------------------------------------------
# Editable columns specification
# ------------------------------------------------------------------
dataset_editable_cols <- function(dataset_key, df_full) {
  # Allow Sepal.Width, NOTES, and PRIORITY (derived PRIORITYFlag remains read-only)
  intersect(c("Sepal.Width", "NOTES", "PRIORITY", "QUAL"), names(df_full))
}

# ------------------------------------------------------------------
# Validation / coercion for edits
# ------------------------------------------------------------------
dataset_validate_and_coerce <- function(dataset_key, df_full, colname, value_raw) {

  # ---- Guard: block edits to derived PRIORITYFlag explicitly ----
  if (identical(colname, "PRIORITYFlag")) {
    return(list(
      ok = FALSE,
      value = NULL,
      message = "PRIORITYFlag is derived from PRIORITY; edit PRIORITY instead.",
      type = "error"
    ))
  }

  # ---- Sepal.Width existing rule ----
  if (identical(colname, "Sepal.Width")) {
    new_num <- suppressWarnings(as.numeric(value_raw))
    if (!is.finite(new_num) || new_num < 2 || new_num > 4) {
      return(list(
        ok = FALSE,
        value = NULL,
        message = "Sepal.Width must be numeric in [2, 4]. Edit discarded.",
        type = "error"
      ))
    }
    return(list(ok = TRUE, value = round(new_num, 3)))
  }

  # ---- NOTES free-text (simple) ----
  if (identical(colname, "NOTES")) {
    txt <- as.character(value_raw)
    txt <- trimws(txt)
    max_len <- 1000
    if (nchar(txt, allowNA = FALSE) > max_len) {
      return(list(
        ok = FALSE,
        value = NULL,
        message = sprintf("NOTES must be <= %d characters. Edit discarded.", max_len),
        type = "error"
      ))
    }
    if (identical(txt, "")) txt <- NA_character_  # optional normalization
    return(list(ok = TRUE, value = txt))
  }

  # ---- PRIORITY stricter integer-only validation ----
  if (identical(colname, "PRIORITY")) {
    raw <- trimws(as.character(value_raw))

    # Optional CLEAR semantics (currently disabled):
    # if (raw %in% c("", ".")) return(list(ok = TRUE, value = 0L, message="Cleared to 0 (neutral).", type="info"))

    # Strict integer pattern: optional +/-, then digits
    if (!grepl("^[+-]?[0-9]+$", raw)) {
      return(list(
        ok = FALSE,
        value = NULL,
        message = "PRIORITY must be an integer (no decimals or letters).",
        type = "error"
      ))
    }

    v <- suppressWarnings(as.integer(raw))

    if (is.na(v)) {
      # Should be rare given regex; treat as error
      return(list(
        ok = FALSE,
        value = NULL,
        message = "PRIORITY parsing failed unexpectedly.",
        type = "error"
      ))
    }

    # Optional magnitude guard
    MAX_ABS_PRIORITY <- 1e6
    if (abs(v) > MAX_ABS_PRIORITY) {
      return(list(
        ok = FALSE,
        value = NULL,
        message = sprintf("PRIORITY magnitude must be <= %d.", MAX_ABS_PRIORITY),
        type = "error"
      ))
    }

    return(list(ok = TRUE, value = v))
  }

  # ---- Fallback generic (for unforeseen editable additions) ----
  old_col <- df_full[[colname]]
  if (is.numeric(old_col)) {
    new_val <- suppressWarnings(as.numeric(value_raw))
  } else if (is.logical(old_col)) {
    new_val <- tolower(trimws(as.character(value_raw))) %in% c("true", "t", "1", "yes", "y")
  } else {
    new_val <- as.character(value_raw)
  }
  list(ok = TRUE, value = new_val)
}

# ------------------------------------------------------------------
# Post-commit hook: derive PRIORITYFlag after PRIORITY edits
# ------------------------------------------------------------------
dataset_after_edit <- function(dataset_key, df_full, row_idx, colname, old_value, new_value) {
  # Re-derive PRIORITYFlag only when PRIORITY was edited and column exists
  if (identical(colname, "PRIORITY") && "PRIORITY" %in% names(df_full)) {

    # Ensure PRIORITYFlag column exists (add by-reference if missing)
    if (!"PRIORITYFlag" %in% names(df_full)) {
      df_full[, PRIORITYFlag := NA]
    }

    p <- df_full[row_idx, PRIORITY]
    flag <- if (is.na(p) || p == 0) {
      NA
    } else if (p > 0) {
      TRUE
    } else {
      FALSE
    }

    # By-reference update of only that row
    data.table::set(df_full, i = row_idx, j = "PRIORITYFlag", value = flag)
  }

  invisible(df_full)
}

# ------------------------------------------------------------------
# Dataset-specific CSS (row highlight classes)
# ------------------------------------------------------------------
dataset_specific_css <- function() {
  singleton(
    tags$style(HTML("
      table.dataTable tbody tr.row-sw3 { background-color: #fff6d5 !important; }
      table.dataTable tbody tr.row-spliceai-override { background-color: #FFFF0099 !important; }
      table.dataTable tbody tr.row-clinvar-override { background-color: #FFA50099 !important; }
      table.dataTable tbody tr.row-priority-true  { background-color: #90EE90  !important; }
      table.dataTable tbody tr.row-priority-false { background-color: #FFCCCC  !important; }
    "))
  )
}

# ------------------------------------------------------------------
# JS highlight injection
# ------------------------------------------------------------------
dataset_specific_js_highlight <- function() {
  c(
    "/* Dataset-specific multi-rule row highlighting */",
    "var HIGHLIGHT_COL_SEPAL = 'Sepal.Width';",
    "var HIGHLIGHT_COL_SPLICEAI = 'spliceai_override';",
    "var HIGHLIGHT_COL_CLINVAR = 'clinvar_override';",
    "var HIGHLIGHT_COL_PRIORITY = 'PRIORITYFlag';",

    "function findInternalColIndexByName(name){",
    "  var idx = -1;",
    "  try {",
    "    table.columns().every(function(i){",
    "      if(idx !== -1) return;",
    "      var h = this.header();",
    "      var t = (h && h.textContent) ? h.textContent.trim() : '';",
    "      if (t === name) idx = i;",
    "    });",
    "  } catch(e){ if(console && console.warn) console.warn('findInternalColIndexByName error', name, e); }",
    "  return idx;",
    "}",

    "function applyAllHighlights(){",
    "  var tbl = table;",
    "  var allClasses = ['row-sw3','row-spliceai-override','row-clinvar-override','row-priority-true','row-priority-false'];",
    "  tbl.rows({page:'current'}).every(function(){",
    "    var $r = $(this.node());",
    "    for(var i=0;i<allClasses.length;i++){ $r.removeClass(allClasses[i]); }",
    "  });",

    "  var idxSepal    = findInternalColIndexByName(HIGHLIGHT_COL_SEPAL);",
    "  var idxSplice   = findInternalColIndexByName(HIGHLIGHT_COL_SPLICEAI);",
    "  var idxClinvar  = findInternalColIndexByName(HIGHLIGHT_COL_CLINVAR);",
    "  var idxPriority = findInternalColIndexByName(HIGHLIGHT_COL_PRIORITY);",

    "  tbl.rows({page:'current'}).every(function(){",
    "    var rowIdx = this.index();",
    "    var $row = $(this.node());",

    "    if(idxSepal >= 0){",
    "      var v = tbl.cell(rowIdx, idxSepal).data();",
    "      var match=false;",
    "      if(v !== undefined && v !== null){",
    "        var num = parseFloat((''+v).replace(/,/g,''));",
    "        if(!isNaN(num)) match = (num === 3); else match = ((''+v).trim()==='3');",
    "      }",
    "      if(match) $row.addClass('row-sw3');",
    "    }",

    "    if(idxSplice >= 0){",
    "      var sv = tbl.cell(rowIdx, idxSplice).data();",
    "      if(sv === true || sv === 'TRUE' || sv === 'True' || sv === 'true' || sv === 1 || sv === '1'){",
    "        $row.addClass('row-spliceai-override');",
    "      }",
    "    }",

    "    if(idxClinvar >= 0){",
    "      var cv = tbl.cell(rowIdx, idxClinvar).data();",
    "      if(cv === true || cv === 'TRUE' || cv === 'True' || cv === 'true' || cv === 1 || cv === '1'){",
    "        $row.addClass('row-clinvar-override');",
    "      }",
    "    }",

    "    if(idxPriority >= 0){",
    "      var pvRaw = tbl.cell(rowIdx, idxPriority).data();",
    "      var pvStr = (pvRaw === null || pvRaw === undefined) ? '' : (''+pvRaw).trim();",
    "      var isTrue = (pvRaw === true) || /^(true|TRUE|True|1)$/.test(pvStr);",
    "      var isFalse = (pvRaw === false) || /^(false|FALSE|False|0)$/.test(pvStr);",
    "      if(isTrue){",
    "        $row.addClass('row-priority-true');",
    "      } else if(isFalse){",
    "        $row.addClass('row-priority-false');",
    "      }",
    "    }",

    "  });",
    "}"
  )
}