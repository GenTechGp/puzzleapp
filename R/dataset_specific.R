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
# Existing highlight rules (plus new cell-level highlights for SpliceAI numeric columns):
#   - Sepal.Width == 3                        -> row-sw3
#   - spliceai_override == TRUE               -> cell-splice-high (cell-level)
#   - clinvar_override == TRUE                -> cell-clinvar-high (cell-level)
#   - PRIORITYFlag TRUE/FALSE                 -> row-priority-true / row-priority-false (NA = no priority color)
#   - Any of Acceptor_Gain/Loss, Donor_Gain/Loss > 0.4 -> cell-splice-score-high (cell-level)
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
# Dataset-specific CSS (row/cell highlight classes)
# ------------------------------------------------------------------
dataset_specific_css <- function() {
  singleton(
    tags$style(HTML("
      table.dataTable tbody tr.row-sw3 { background-color: #fff6d5 !important; }
      /* table.dataTable tbody tr.row-spliceai-override { background-color: #FFFF0099 !important; } */
      /* table.dataTable tbody tr.row-clinvar-override { background-color: #FFA50099 !important; } */
      table.dataTable tbody tr.row-priority-true  { background-color: #90EE90  !important; }
      table.dataTable tbody tr.row-priority-false { background-color: #FFCCCC  !important; }

      /* Cell-level highlight for overrides and scores */
      /* table.dataTable tbody td.cell-splice-high       { background-color: #FFFF0099 !important; } */ /* spliceai_override == TRUE */
      table.dataTable tbody td.cell-clinvar-high      { background-color: #FFA50099 !important; } /* clinvar_override == TRUE */
      table.dataTable tbody td.cell-splice-score-high { background-color: #FFFF0099 !important; } /* SpliceAI numeric > threshold */
    "))
  )
}

# ------------------------------------------------------------------
# JS highlight injection
# ------------------------------------------------------------------
dataset_specific_js_highlight <- function() {
  c(
    "/* Dataset-specific multi-rule row/cell highlighting */",
    "var HIGHLIGHT_COL_SEPAL    = 'Sepal.Width';",
    "var HIGHLIGHT_COL_SPLICEAI = 'SpliceAI_pred';",
    "var HIGHLIGHT_COL_CLINVAR  = 'CLINVAR';",
    "var HIGHLIGHT_COL_CLINVAR_OVERRIDE  = 'clinvar_override';",
    "var HIGHLIGHT_COL_PRIORITY = 'PRIORITYFlag';",

    "/* SpliceAI numeric columns; threshold is provided by server via Shiny custom message */",
    "var SPLICE_NUMERIC_COLS = ['Acceptor_Gain','Acceptor_Loss','Donor_Gain','Donor_Loss'];",
    "var SPLICE_NUMERIC_THRESHOLD = 0;",

    "/* Getter for per-table threshold (defaults to 0 if unset) */",
    "function getSpliceThreshold(){",
    "  try {",
    "    var tbl = table;",
    "    var node = tbl.table().node();",
    "    var v = $(node).data('splice-threshold');",
    "    var x = parseFloat(v);",
    "    if (isNaN(x)) x = 1.0;",
    "    return x;",
    "  } catch(e){ return 1.0; }",
    "}",

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
    "  var thr = getSpliceThreshold();",

    "  /* Clear row-level classes on current page */",
    "  var allClasses = ['row-sw3','row-clinvar-override','row-priority-true','row-priority-false'];",
    "  tbl.rows({page:'current'}).every(function(){",
    "    var $r = $(this.node());",
    "    for(var i=0;i<allClasses.length;i++){ $r.removeClass(allClasses[i]); }",
    "  });",

    "  /* Resolve column indices */",
    "  var idxSepal    = findInternalColIndexByName(HIGHLIGHT_COL_SEPAL);",
    "  var idxSplice   = findInternalColIndexByName(HIGHLIGHT_COL_SPLICEAI);",
    "  var idxClinvar  = findInternalColIndexByName(HIGHLIGHT_COL_CLINVAR);",
    "  var idxClinvarOverride  = findInternalColIndexByName(HIGHLIGHT_COL_CLINVAR_OVERRIDE);",
    "  var idxPriority = findInternalColIndexByName(HIGHLIGHT_COL_PRIORITY);",
    "  var idxSpliceNums = [];",
    "  for (var s=0; s<SPLICE_NUMERIC_COLS.length; s++){",
    "    idxSpliceNums.push(findInternalColIndexByName(SPLICE_NUMERIC_COLS[s]));",
    "  }",

    "  /* Clear cell-level classes on current page before re-applying */",
    "  if(idxSplice >= 0){",
    "    $(tbl.cells({page:'current'}, idxSplice).nodes()).removeClass('cell-splice-high');",
    "  }",
    "  for (var c=0; c<idxSpliceNums.length; c++){",
    "    var ci = idxSpliceNums[c];",
    "    if (ci >= 0){",
    "      $(tbl.cells({page:'current'}, ci).nodes()).removeClass('cell-splice-score-high');",
    "    }",
    "  }",
    "  if(idxClinvar >= 0){",
    "    $(tbl.cells({page:'current'}, idxClinvar).nodes()).removeClass('cell-clinvar-high');",
    "  }",

    "  /* Apply highlights */",
    "  tbl.rows({page:'current'}).every(function(){",
    "    var rowIdx = this.index();",
    "    var $row = $(this.node());",

    "    /* Sepal.Width == 3 -> row highlight */",
    "    if(idxSepal >= 0){",
    "      var v = tbl.cell(rowIdx, idxSepal).data();",
    "      var match=false;",
    "      if(v !== undefined && v !== null){",
    "        var num = parseFloat((''+v).replace(/,/g,''));",
    "        if(!isNaN(num)) match = (num === 3); else match = ((''+v).trim()==='3');",
    "      }",
    "      if(match) $row.addClass('row-sw3');",
    "    }",

    "    /* spliceai_override -> cell highlight */",
    "    /* if(idxSplice >= 0){",
    "      var sv = tbl.cell(rowIdx, idxSplice).data();",
    "      if(sv === true || sv === 'TRUE' || sv === 'True' || sv === 'true' || sv === 1 || sv === '1'){",
    "        var cellNode = tbl.cell(rowIdx, idxSplice).node();",
    "        if(cellNode) $(cellNode).addClass('cell-splice-high');",
    "      }",
    "    }, */",

    "    /* SpliceAI numeric columns > threshold -> highlight idxSplice if any exceed threshold */",
    "    var spliceTriggered = false;",

    "    /* SpliceAI numeric columns > threshold -> cell highlight */",
    "    for (var k=0; k<idxSpliceNums.length; k++){",
    "      var ci2 = idxSpliceNums[k];",
    "      if (ci2 >= 0){",
    "        var raw = tbl.cell(rowIdx, ci2).data();",
    "        var n = (raw===null || raw===undefined) ? NaN : parseFloat((''+raw).replace(/,/g,''));",
    "        if (!isNaN(n) && n > thr){",
    "          spliceTriggered = true;",
    "          var node = tbl.cell(rowIdx, ci2).node();",
    "          if (node) $(node).addClass('cell-splice-score-high');",
    "        }",
    "      }",
    "    }",

    "   if (spliceTriggered && idxSplice >= 0) {",
    "      var spliceCellNode = tbl.cell(rowIdx, idxSplice).node();",
    "      if (spliceCellNode) $(spliceCellNode).addClass('cell-splice-score-high');",
    "   }",

    "    /* clinvar_override -> cell highlight */",
    "    if(idxClinvarOverride >= 0 && idxClinvar >= 0){",
    "      var cv = tbl.cell(rowIdx, idxClinvarOverride).data();",
    "      if(cv === true || cv === 'TRUE' || cv === 'True' || cv === 'true' || cv === 1 || cv === '1'){",
    "        var cellNode2 = tbl.cell(rowIdx, idxClinvar).node();",
    "        if(cellNode2) $(cellNode2).addClass('cell-clinvar-high');",
    "      }",
    "    }",

    "    /* PRIORITYFlag row highlight */",
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
