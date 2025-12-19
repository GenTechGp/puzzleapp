#' Data Module
#' @param id Module ID
#' @return Shiny UI object
#' @export
# Multi-dataset Data module using shared_store$data_for_data (named list of datasets).
# Preserves existing editing, reordering, snapshotting, slice modal, export TSV, and DT readiness logic.
# Added: persistent row highlight for Sepal.Width == 3 (survives column hide).

dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .dtsb-searchBuilder {
        display: inline-flex !important;
        align-items: center;
        gap: 6px;
      }

      .dtsb-add {
        margin-left: 6px;
        white-space: nowrap;
      }
    ")),
    tags$style(HTML(sprintf("
      /* scope rules to the DT container so we don't affect other selectize widgets */
      #%s table.dataTable thead .selectize-control .selectize-dropdown,
      #%s table.dataTable thead .selectize-control .selectize-dropdown .selectize-dropdown-content,
      #%s table.dataTable thead .selectize-control .selectize-dropdown .option {
        white-space: nowrap !important;
        max-width: none !important;
        overflow-x: auto !important;
        overflow-y: auto !important;
        box-sizing: content-box !important;
      }
      #%s table.dataTable thead .selectize-control .selectize-dropdown {
        width: auto !important;
        min-width: 0 !important;
        z-index: 2000 !important;
      }
      #%s table.dataTable thead .selectize-control .selectize-dropdown-content .option {
        display: block !important;
        white-space: nowrap !important;
        padding-right: 16px !important;
      }
    ", ns("tbl"), ns("tbl"), ns("tbl"), ns("tbl"), ns("tbl")))),
    tags$style(HTML("
      /* DT ColVis dropdown: single column with ~20 items visible and vertical scroll */
      div.dt-button-collection {
        max-height: 640px;   /* ~20 rows depending on line-height */
        overflow-y: auto !important;
        overflow-x: hidden;
      }
    ")),
    if (exists("dataset_specific_css")) dataset_specific_css(),
    DTOutput(ns("tbl")),
    tags$script(HTML(sprintf("
    Shiny.addCustomMessageHandler('%s', function(txt) {
      if (typeof txt === 'undefined') return;
      var sel = '#%s';
      var el = $(sel);
      if (!el.length) {
        // Button not yet in DOM; retry shortly
        setTimeout(function(){
          var el2 = $(sel);
          if (el2.length) el2.text(txt);
        }, 200);
      } else {
        el.text(txt);
      }
    });
    ", ns("updateSliceBtn"), ns("slice_summary_btn")))),
    tags$script(HTML(sprintf("
    Shiny.addCustomMessageHandler('%s', function(msg) {
      try {
        var v = parseFloat(msg && msg.value);
        var $widget = $('#%s');
        if (!$widget.length) return;

        // Find the actual table inside the DT widget container
        var $table = $widget.find('table.dataTable').first();
        if (!$table.length) {
          // Table not in DOM yet; retry shortly
          return setTimeout(function(){ Shiny.onInputChange('___noop', Date.now()); Shiny.addCustomMessageHandler('%s', arguments.callee)(msg); }, 200);
        }

        // Get the existing DataTable API (this does not re-initialize)
        var dt = $table.DataTable();
        if (!isNaN(v)) {
          $(dt.table().node()).data('splice-threshold', v);
        }
        // Trigger a redraw so your existing draw.dt hook runs applyAllHighlights
        dt.draw(false);
      } catch(e){
        if (console && console.error) console.error('set_splice_threshold handler error', e);
      }
    });
    ", ns("set_splice_threshold"), ns("tbl"), ns("set_splice_threshold"))))
  )
}

#' Data Module Server
#' @param id Module ID
#' @param shared_store Shared store (named list)
#' @param shared_rx Shared reactive values (list of reactiveVal)
#' @param dataset_names Optional character vector of allowed dataset names (NULL = all)
#' @param prefix Optional prefix for handling multiple different datasets
#' @param initial_row_threshold Integer; threshold for large datasets (default: 200,000 rows)
#' @return NULL
#' @export

dataServer <- function(id, shared_store, shared_rx, dataset_names = NULL, prefix = "", initial_row_threshold = 200000) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(x, y) if (is.null(x)) y else x

    log_enabled <- if (!is.null(shared_store$verbose_level) && is.numeric(shared_store$verbose_level)) {
      shared_store$verbose_level >= 1L
    } else {
      FALSE
    }
    logf <- function(...) if (isTRUE(log_enabled)) cat(...)

    # ---- Constants ----
    INITIAL_SLICE_PCT <- c(0, 100)
    BIG_DB_INITIAL_SLICE_PCT <- c(0, 10)  # show first 10% for large datasets initially
    DB_INITIAL_ROW_THRESHOLD <- initial_row_threshold  # threshold for large datasets

    # ---- Data module local state ----
    active <- reactiveVal(NULL)           # active dataset key from shared_store$data_for_data
    slice_pct <- reactiveVal(INITIAL_SLICE_PCT)    # default: first 100%
    rendering_counter <- reactiveVal(0L)  # no UI; used only for logging
    rendered <- reactiveVal(FALSE)        # has the table been rendered once?
    dt_ready <- reactiveVal(FALSE)        # DataTables in browser is initialized

    # append prefix to synthetic key
    synthetic_key <- sprintf("[%s]_Boundary", prefix)
    include_synthetic <- TRUE
    allowed_dataset_names <- eventReactive(shared_rx$data_version(), {
      all_current <- names(shared_store$data_for_data) %||% character(0)
      allowed <- if (is.null(dataset_names)) all_current else intersect(all_current, dataset_names)
      if (isTRUE(include_synthetic) && synthetic_key %in% all_current)
        allowed <- union(allowed, synthetic_key)
      sort(unique(allowed))
    }, ignoreNULL = TRUE)

    output$active_label <- renderText({ active() %||% "<none>" })

    # ---- Helpers (private to Data) ----
    apply_slice <- function(df_full, slice) {
      n <- nrow(df_full)
      if (!n) return(df_full)
      pmin <- max(0, min(slice))
      pmax <- min(100, max(slice))
      i <- floor(pmin / 100 * max(n - 1, 0)) + 1L
      j <- floor(pmax / 100 * n)
      if (j < i) j <- i
      if (i < 1L) i <- 1L
      if (j > n) j <- n
      base_idx <- if (".row_id" %in% names(df_full)) order(df_full$.row_id) else seq_len(n)
      sel <- base_idx[i:j]
      df_full[sel, , drop = FALSE]
    }
    fmt_row <- function(row_df) {
      vals <- lapply(row_df, function(x) as.character(x)[1])
      paste(paste0(names(vals), "=", unlist(vals, use.names = FALSE)), collapse = ", ")
    }

    # DT proxy (created after the widget is defined)
    proxy <- NULL

    # Track initial column names to ensure replaceData always receives identical structure
    initial_colnames <- NULL

    # ---- Function to render DT ONCE (no reactive deps inside) ----
    render_tbl_once <- function() {
      if (rendered()) return(invisible(NULL))

      act0 <- isolate(active())
      if (is.null(act0)) return(invisible(NULL))

      df_full0 <- isolate({
        ds <- shared_store$data_for_data[[act0]]
        if (is.null(ds)) data.frame(.row_id = integer()) else ds
      })

      # Fast initial slice for performance
      df0 <- isolate(apply_slice(df_full0, slice_pct()))
      initial_colnames <<- names(df0)

      # Capture preferred columns at first render (static, one time)
      # if dataset_names is not null use 1st otherwise use active()
      if (!is.null(dataset_names)) {
        pref_init <- isolate(shared_store$preferred_cols[[dataset_names[1]]] %||% character(0))
      } else {
        pref_init <- isolate(shared_store$preferred_cols[[active()]] %||% character(0))
      }
      pref_js <- jsonlite::toJSON(unname(as.character(pref_init)), auto_unbox = TRUE)

      output$tbl <- renderDT({
        # Render once; avoid reactive deps; compute initial view with isolate
        n <- isolate(rendering_counter()) + 1L
        rendering_counter(n)
        log_debug(sprintf("[Data] [Render] rendering_counter = %d", n))

        cn <- colnames(df0)
        rid_idx0 <- which(cn == ".row_id") - 1L
        id_idx0 <- which(colnames(df0) == "ID") - 1L
        genesymbol_idx0 <- which(colnames(df0) == "GENE_SYMBOL") - 1L
        hpo_id_idx0 <- which(colnames(df0) == "HPO_ID") - 1L

        # Client-side renderer for the ID column (display only; underlying data remains raw)
        render_link_js <- JS("
          function(data, type, row, meta) {
            if (type !== 'display') return data;
            var esc = $('<div>').text(data == null ? '' : String(data)).html();
            if (!esc) return data;
            return '<a href=\"#\" class=\"id-link\" data-id=\"' + esc + '\">' + esc + '</a>';
          }
        ")

        # todo: treat multi-valued GENE_SYMBOL columns
        render_gene_symbol_js <- JS("
          function(data, type, row, meta) {
            if (type !== 'display') return data;
            var esc = $('<div>').text(data == null ? '' : String(data)).html();
            if (!esc) return data;
            return '<a href=\"#\" class=\"gene-symbol\" data-gene-symbol=\"' + esc + '\">' + esc + '</a>';
          }
        ")

        # the HPO_ID column will have multiple HPO IDs separated by ; we need to split them and create links for each. but keep them in the same cell separated by ; no new lines.
        render_hpo_id_js <- JS("
          function(data, type, row, meta) {
            if (type !== 'display') return data;
            if (data == null || data === '') return data;
            var ids = String(data).split(';');
            var links = ids.map(function(id) {
              var esc = $('<div>').text(id.trim()).html();
              return '<a href=\"#\" class=\"hpo-id-link\" data-hpo-id=\"' + esc + '\">' + esc + '</a>';
            });
            return links.join('; ');
          }
        ")

        column_defs <- list()
        if (length(rid_idx0)) {
          column_defs <- list(
            list(targets = rid_idx0, visible = FALSE, searchable = FALSE, orderable = FALSE, className = "noVis")
          )
        }

        # Extend column_defs with the ID render, if the ID column exists
        if (length(id_idx0)) {
          column_defs <- c(
            column_defs,
            list(list(targets = id_idx0, render = render_link_js))
          )
        }

        if (length(genesymbol_idx0)) {
          column_defs <- c(
            column_defs,
            list(list(targets = genesymbol_idx0, render = render_gene_symbol_js))
          )
        }

        if (length(hpo_id_idx0)) {
          column_defs <- c(
            column_defs,
            list(list(targets = hpo_id_idx0, render = render_hpo_id_js))
          )
        }

        # Namespaced input ids for JS callbacks
        id_snapshot <- ns("tbl_snapshot")
        id_edit_row <- ns("tbl_edit_row_id")
        id_edit_col <- ns("tbl_edit_col_orig0")
        id_ready <- ns("tbl_ready")
        id_selected_igv <- ns("selected_igv_id") # input id for IGV selection
        selected_genesymbol <- ns("selected_genesymbol") # input id for Gene Symbol selection
        selected_hpo_id <- ns("selected_hpo_id") # input id for HPO selection

        # Pre-encode strings for safe JS embedding
        id_snapshot_json <- jsonlite::toJSON(id_snapshot, auto_unbox = TRUE)
        id_edit_row_json <- jsonlite::toJSON(id_edit_row, auto_unbox = TRUE)
        id_edit_col_json <- jsonlite::toJSON(id_edit_col, auto_unbox = TRUE)
        id_ready_json    <- jsonlite::toJSON(id_ready,    auto_unbox = TRUE)
        id_selected_igv_json <- jsonlite::toJSON(id_selected_igv, auto_unbox = TRUE)
        selected_genesymbol_json <- jsonlite::toJSON(selected_genesymbol, auto_unbox = TRUE)
        selected_hpo_id_json <- jsonlite::toJSON(selected_hpo_id, auto_unbox = TRUE)
        initial_colnames_json <- jsonlite::toJSON(unname(as.character(names(df0))), auto_unbox = TRUE)
        ds_js <- if (exists("dataset_specific_js_highlight")) dataset_specific_js_highlight() else character(0)
        cb_lines <- c(
          "var tbl = table;",
          "var ridIdx0 = ", ifelse(length(rid_idx0), rid_idx0, "null"), ";",
          "var preferredInit = ", pref_js, ";",
          "var initialColNames = ", initial_colnames_json, ";",

          ds_js,

          "function computeSnapshot(reason){",
          "  try {",
          "    var s = tbl.settings()[0];",
          "    if (!s || !s.aoColumns) {",
          "      Shiny.setInputValue(", id_snapshot_json, ", { headers: [], orderAll: [], dataSrc: [], reason: reason||'unspecified', version: 'exact-colrow-v3', ts: Date.now() }, {priority:'event'});",
          "      return;",
          "    }",
          "    var nCols = s.aoColumns.length;",
          "    var orderAll = (tbl.colReorder && typeof tbl.colReorder.order === 'function') ? tbl.colReorder.order() : (function(){ var a=[]; for (var i=0;i<nCols;i++) a.push(i); return a; })();",
          "    var headers = [];",
          "    for (var k=0; k<orderAll.length; k++){",
          "      var i = orderAll[k];",
          "      var c = s.aoColumns[i];",
          "      if (c && c.bVisible) {",
          "        var nm = (initialColNames && initialColNames[i] != null) ? String(initialColNames[i]) : String(i);",
          "        headers.push(nm);",
          "      }",
          "    }",
          "    var dataSrc = tbl.columns().dataSrc();",
          "    if (dataSrc && dataSrc.toArray) dataSrc = dataSrc.toArray();",
          "    Shiny.setInputValue(", id_snapshot_json, ", {",
          "      headers: headers,",
          "      orderAll: orderAll,",
          "      dataSrc: dataSrc,",
          "      reason: reason || 'unspecified',",
          "      version: 'exact-colrow-v3',",
          "      ts: Date.now()",
          "    }, {priority:'event'});",
          "  } catch(e){ if (console && console.error) console.error('computeSnapshot error', e); }",
          "}",

          "function captureEditCtx(node){",
          " try{",
          "  var $td=$(node).closest('td');",
          "  if($td.length===0) return;",
          "  var cell=tbl.cell($td);",
          "  if(!cell || !cell.any()) return;",
          "  var idx=cell.index();",
          "  var colDisp=idx.column;",
          "  var colOrig=(tbl.colReorder && tbl.colReorder.transpose)?tbl.colReorder.transpose(colDisp,'toOriginal'):colDisp;",
          "  var rowData=tbl.row(idx.row).data();",
          "  if(!rowData || ridIdx0==null || isNaN(ridIdx0)) return;",
          "  var rid=rowData[ridIdx0];",
          "  if(rid!=null){",
          "    Shiny.setInputValue(", id_edit_row_json, ", rid, {priority:'event'});",
          "    Shiny.setInputValue(", id_edit_col_json, ", colOrig, {priority:'event'});",
          "  }",
          " }catch(e){ console && console.error && console.error('captureEditCtx error', e); }",
          "}",

          "function applyPreferredOnce(){",
          " try{",
          "  if(applyPreferredOnce.done) return;",
          "  var preferred = Array.isArray(preferredInit)?preferredInit:[];",
          "  if(!preferred.length){ applyPreferredOnce.done = true; return; }",
          "  var nCols=tbl.columns().count();",
          "  var headersAll=[];",
          "  tbl.columns().every(function(i){",
          "    var h=this.header();",
          "    headersAll.push(h && h.textContent ? h.textContent.trim() : (''+i));",
          "  });",
          "  var nameToIdx={};",
          "  for(var i=0;i<headersAll.length;i++){ var nm=headersAll[i]; if(nameToIdx[nm]==null) nameToIdx[nm]=i; }",
          "  var prefIdx=[];",
          "  for(var k=0;k<preferred.length;k++){ var idx=nameToIdx[preferred[k]]; if(idx!=null && prefIdx.indexOf(idx)===-1) prefIdx.push(idx); }",
          "  var allIdx=[]; for(var j=0;j<nCols;j++) allIdx.push(j);",
          "  var restIdx=allIdx.filter(function(j){ return prefIdx.indexOf(j)===-1; });",
          "  var toHide=restIdx.filter(function(j){ return j!==ridIdx0; });",
          "  if(toHide.length){ tbl.columns(toHide).visible(false, false); }",
          "  var orderVec=prefIdx.concat(restIdx);",
          "  if(tbl.colReorder && typeof tbl.colReorder.order==='function'){ tbl.colReorder.order(orderVec, true); }",
          "  tbl.columns.adjust().draw(false);",
          "  applyPreferredOnce.done=true;",
          "  console.log('[DT] preferred applied once:', preferred);",
          " }catch(errApply){ console && console.error && console.error('applyPreferredOnce error', errApply); }",
          "}",

          "tbl.on('init.dt', function(){ applyPreferredOnce(); computeSnapshot('init'); applyAllHighlights(); Shiny.setInputValue(", id_ready_json, ", Date.now(), {priority:'event'}); });",
          "tbl.on('column-reorder.dt', function(){ computeSnapshot('colreorder'); applyAllHighlights(); });",
          "tbl.on('column-visibility.dt', function(){ computeSnapshot('colvisibility'); applyAllHighlights(); });",
          "tbl.on('order.dt', function(){ computeSnapshot('order'); applyAllHighlights(); });",
          "tbl.on('search.dt', function(){ computeSnapshot('search'); applyAllHighlights(); });",
          "tbl.on('draw.dt', function(){ computeSnapshot('draw'); applyAllHighlights(); Shiny.setInputValue(", id_ready_json, ", Date.now(), {priority:'event'}); });",

          "tbl.on('focusin.dt','tbody td',function(e){ captureEditCtx(this); });",
          "tbl.on('focusin.dt','tbody input, tbody textarea',function(e){ captureEditCtx(this); });",
          "tbl.on('focusout.dt','tbody td', function(e){ captureEditCtx(this); });",
          "tbl.on('focusout.dt','tbody input, tbody textarea', function(e){ captureEditCtx(this); });",

          "setTimeout(function(){ computeSnapshot('tick0'); }, 0);",
          "setTimeout(function(){ computeSnapshot('tick50'); }, 50);",
          "computeSnapshot('manual');",

          "$(tbl.table().node()).off('click.idlink').on('click.idlink', 'a.id-link', function(e){",
          "  e.preventDefault();",
          "  var id = $(this).data('id');",
          "  if (id != null) {",
          "    Shiny.setInputValue(", id_selected_igv_json, ", id, {priority:'event'});",
          "  }",
          "});",

          "$(tbl.table().node()).off('click.genesymbol').on('click.genesymbol', 'a.gene-symbol', function(e){",
          "  e.preventDefault();",
          "  var geneSymbol = $(this).data('gene-symbol');",
          "  if (geneSymbol != null) {",
          "    Shiny.setInputValue(", selected_genesymbol_json, ", geneSymbol, {priority:'event'});",
          "  }",
          "});",

          "$(tbl.table().node()).off('click.hpoidlink').on('click.hpoidlink', 'a.hpo-id-link', function(e){",
          "  e.preventDefault();",
          "  var hpoId = $(this).data('hpo-id');",
          "  if (hpoId != null) {",
          "    Shiny.setInputValue(", selected_hpo_id_json, ", hpoId, {priority:'event'});",
          "  }",
          "});"

        )

        cb_code <- paste(cb_lines, collapse = "\n")
        cb <- JS(cb_code)
        # Filename function for TSV export
        filename_js <- JS("
          function() {
            var ts = new Date().toISOString().replace(/[:.]/g, '-');
            var v = window.prompt('Enter file name (without extension):', 'export-' + ts);
            if (!v) v = 'export-' + ts;
            v = String(v).trim()
                        .replace(/[\\\\/:*?\"<>|]+/g, '-')  // sanitize illegal filename chars
                        .replace(/\\s+/g, '_');             // spaces -> underscores
            return v; // Buttons will append .tsv
          }
        ")

        # Minimal-quoting TSV customize function
        tsv_minimal_quotes_js <- JS("
          function (output, config) {
            var res = [];
            var i = 0, n = output.length;
            var field = '';
            var inQuotes = false;
            var line = [];

            function flushField() {
              var needsQuote = /[\\t\\r\\n\"]/g.test(field);
              if (needsQuote) {
                var escaped = field.replace(/\"/g, '\"\"');
                line.push('\"' + escaped + '\"');
              } else {
                line.push(field);
              }
              field = '';
            }

            while (i < n) {
              var ch = output[i++];
              if (inQuotes) {
                if (ch === '\"') {
                  if (i < n && output[i] === '\"') { // escaped quote
                    field += '\"';
                    i++;
                  } else {
                    inQuotes = false;
                  }
                } else {
                  field += ch;
                }
              } else {
                if (ch === '\"') {
                  inQuotes = true;
                } else if (ch === '\\t') {
                  flushField();
                } else if (ch === '\\r') {
                  if (i < n && output[i] === '\\n') { i++; }
                  flushField();
                  res.push(line.join('\\t'));
                  line = [];
                } else if (ch === '\\n') {
                  flushField();
                  res.push(line.join('\\t'));
                  line = [];
                } else {
                  field += ch;
                }
              }
            }
            // Flush last field/line
            flushField();
            if (line.length) res.push(line.join('\\t'));
            return res.join('\\r\\n');
          }
        ")

        widget <- datatable(
          df0,
          filter = list(position = "top", clear = TRUE),
          selection = "none",
          escape = FALSE,
          rownames = FALSE,
          extensions = c("ColReorder", "Buttons", "SearchBuilder"),
          editable = list(target = "cell", numeric = "none"),
          options = list(
            dom = "QBlfrtip",
            language = list(
              searchBuilder = list(
                title = ""   # <- removes "Custom Search Builder"
              )
            ),
            searchBuilder = TRUE,
            # search = list(regex = TRUE, caseInsensitive = TRUE),
            buttons = list(
              list(
                extend = "colvis",
                columns = ":not(.noVis)",
                text = "Columns"
              ),
              # list(
              #   extend = "collection",
              #   text = "% Data Shown",
              #   action = JS(sprintf("function (e, dt, node, config) { Shiny.setInputValue('%s', true, {priority: 'event'}); }", ns("open_data_modal")))
              # ),
              list(
                extend = "collection",
                text = "0-100% filtered data",   # initial placeholder; will be overwritten
                attr = list(id = ns("slice_summary_btn")),
                className = "btn-slice-summary",
                action = JS(sprintf(
                  "function (e, dt, node, config) { 
                    Shiny.setInputValue('%s', true, {priority: 'event'});
                  }",
                  ns("open_data_modal")
                ))
              ),
              list(
                extend = "collection",
                text = "Download",
                buttons = list(
                  list(
                    extend = "csvHtml5",
                    text = "Current page + visible columns",
                    fieldSeparator = "\t",
                    extension = ".tsv",
                    bom = TRUE,
                    title = NULL,
                    filename = filename_js,
                    exportOptions = list(
                      columns = ":visible:not(.noVis)",
                      modifier = list(
                        search = "applied",
                        order = "applied",
                        page = "current"
                      ),
                      stripHtml = TRUE
                    ),
                    customize = tsv_minimal_quotes_js
                  ),
                  list(
                    extend = "csvHtml5",
                    text = "Current page + all columns",
                    fieldSeparator = "\t",
                    extension = ".tsv",
                    bom = TRUE,
                    title = NULL,
                    filename = filename_js,
                    exportOptions = list(
                      columns = ":not(.noVis)",   # includes hidden columns except those tagged .noVis (e.g., .row_id)
                      modifier = list(
                        search = "applied",
                        order = "applied",
                        page = "current"
                      ),
                      stripHtml = TRUE
                    ),
                    customize = tsv_minimal_quotes_js
                  )
                )
              ),
              list(extend = 'collection', text = 'Save', dropIcon = TRUE,
                action = JS(paste0(
                  "function(e, dt, node, config) {",
                    "Shiny.setInputValue('", ns("save_table"), "', true, {priority: 'event'});",
                  "}")
                ))
            ),
            colReorder = TRUE,
            pageLength = 50,
            lengthMenu = list(
              c(10, 15, 25, 50, 100, 1000, 10000, -1),
              c("10", "15", "25", "50", "100", "1,000", "10,000", "All")
            ),
            deferRender = TRUE,
            # scrollX = TRUE,
            # autoWidth = TRUE,
            columnDefs = column_defs
          ),
          callback = cb
        )
        widget
      }, server = TRUE)
      outputOptions(output, "tbl", suspendWhenHidden = FALSE)

      # Create proxy now that the table exists (IMPORTANT: scope with session)
      proxy <<- dataTableProxy("tbl", session = session)
      rendered(TRUE)
      log_info("[Data] DT rendered in UI (proxy created)")

      # Note: No initial push here; dt_ready gate will handle the first update
    }

    # ---- View updater (safe no-op if proxy not ready) ----
    update_view <- function(resetPaging = TRUE) {
      if (!rendered() || !dt_ready() || is.null(proxy)) return(invisible(NULL))
      act <- active()
      if (is.null(act)) return(invisible(NULL))
      df_full <- shared_store$data_for_data[[act]]
      if (is.null(df_full)) return(invisible(NULL))

      
      # fetch filtered start
      # total_n <- nrow(df_full)
      # # --- Retrieve mask (may not exist yet) ---
      # mask_list <- shared_store$filter_for_data
      # mask <- if (!is.null(mask_list)) mask_list[[act]] else NULL
      # valid_mask <- !is.null(mask) && is.logical(mask) && length(mask) == total_n
      # if (!valid_mask) {
      #   # Fallback: no mask:treat all rows as passing
      #   filtered_idx <- if (total_n) seq_len(total_n) else integer(0)
      # } else {
      #   # Missing column policy was "all FALSE" for missing; that's already in mask
      #   # Just take rows with TRUE
      #   filtered_idx <- which(mask)
      # }
      # filtered_n <- length(filtered_idx)
      # # --- Build filtered subset BEFORE slicing ---
      # if (filtered_n == 0) {
      #   # Preserve column structure (important for replaceData)
      #   df_filtered <- df_full[0, , drop = FALSE]
      # } else {
      #   # NOTE: filtered_idx is in ascending order already (which() behavior)
      #   df_filtered <- df_full[filtered_idx, , drop = FALSE]
      # }
      # # --- Apply existing percentage slice logic to filtered subset ---
      # # Reuse apply_slice WITHOUT modifying its internal logic:
      # # It treats input data as the "universe", so now the universe is df_filtered
      # view_df <- apply_slice(df_filtered, slice_pct())
      # fetch filtered end
      
      view_df <- apply_slice(df_full, slice_pct())

      # cat(sprintf(
      #   "[Data] update_view total=%d filtered=%d slice_rows=%d cols=%d\n",
      #   total_n, filtered_n, nrow(view_df), ncol(view_df)
      # ))

      # Diagnostics: structure and column equality with initial table
      log_info(sprintf("[Data] update_view rows=%d cols=%d", nrow(view_df), ncol(view_df)))
      if (!is.null(initial_colnames) && !identical(names(view_df), initial_colnames)) {
        log_error("[Data][ERROR] Column mismatch in update_view:")
        log_error(sprintf("  initial: %s", paste(initial_colnames, collapse = ", ")))
        log_error(sprintf("  current: %s", paste(names(view_df), collapse = ", ")))
        showNotification("Column mismatch in Data update_view; see console.", type = "error", duration = NULL)
        return(invisible(NULL))
      }
      replaceData(proxy, data = view_df, resetPaging = resetPaging, rownames = FALSE, clearSelection = "none")
    }
    # ---- Helper to push current dataset's splice threshold to the client ----
    push_splice_threshold <- function() {
      if (!rendered() || !dt_ready()) return(invisible(NULL))
      act <- active()
      if (is.null(act)) return(invisible(NULL))
      val <- tryCatch({
        vfd <- shared_store$value_for_data
        if (!is.null(vfd) && !is.null(vfd[[act]]) && !is.null(vfd[[act]]$splice_numeric_threshold)) {
          as.numeric(vfd[[act]]$splice_numeric_threshold)
        } else { 1.0 }
      }, error = function(e) 1.0)
      session$sendCustomMessage(ns("set_splice_threshold"), list(value = val, ts = as.numeric(Sys.time()) * 1000))
    }
    # ---- Sync helper: choices, visibility, and active selection ----
    sync_choices_and_active <- function(reason = "unspecified") {
      slice_pct(INITIAL_SLICE_PCT)

      choices_all <- allowed_dataset_names()
      curr_active <- isolate(active())

      if (!rendered()) {
        active_new <- if (!is.null(curr_active) && curr_active %in% choices_all) curr_active else {
          if (synthetic_key %in% choices_all) synthetic_key else (choices_all[1] %||% NULL)
        }
        visible_choices <- choices_all
      } else {
        non_synth <- setdiff(choices_all, synthetic_key)
        visible_choices <- if (length(non_synth) >= 1) non_synth else choices_all
        if (!is.null(curr_active) && curr_active %in% visible_choices) {
          active_new <- curr_active
        } else if (length(non_synth) > 0) {
          active_new <- non_synth[1]
        } else if (synthetic_key %in% choices_all) {
          active_new <- synthetic_key
        } else {
          active_new <- NULL
        }
      }

      selected_ui <- if (!is.null(active_new) && active_new %in% visible_choices) {
        active_new
      } else if (length(visible_choices) >= 1) {
        visible_choices[1]
      } else NULL

      updateSelectInput(session, "dataset_select", choices = visible_choices, selected = selected_ui)

      if (!identical(active(), active_new)) {
        log_debug(sprintf("[Data] sync(%s) -> active set to '%s'", reason, as.character(active_new %||% "<none>")))
        active(active_new)
      }

      # get the active() dataset and check if its nrow > 200k. if so set slice_pct to c(0,10)
      df_active <- shared_store$data_for_data[[active()]]
      if (!is.null(df_active) && nrow(df_active) > DB_INITIAL_ROW_THRESHOLD) {
        slice_pct(BIG_DB_INITIAL_SLICE_PCT)
      }
    }

    # ---- Dataset selection UI sync on version bumps ----
    observeEvent(shared_rx$data_version(), {
      sync_choices_and_active("version")

      # Render or update view
      choices_all <- allowed_dataset_names()
      log_info(sprintf("[Data] data_version = %d, choices = %s", shared_rx$data_version(), paste(choices_all, collapse = ", ")))
      if (!rendered()) {
        if (length(choices_all)) {
          log_info(sprintf("[Data] First version bump detected (%d): rendering table (active=%s)", shared_rx$data_version(), active()))
          render_tbl_once()
        }
      } else {
        log_info(sprintf("[Data] Version bump detected: %d (active=%s)", shared_rx$data_version(), active()))
        update_view(resetPaging = TRUE)
        push_splice_threshold()
      }
    }, ignoreInit = FALSE)

    # ---- Apply button: switch active dataset ----
    observeEvent(input$use_dataset, {
      sel <- input$dataset_select
      visible_choices <- isolate({
        choices_all <- allowed_dataset_names()
        if (rendered() && length(setdiff(choices_all, synthetic_key)) >= 1) {
          setdiff(choices_all, synthetic_key)
        } else {
          choices_all
        }
      })
      if (length(visible_choices) == 0) return()
      if (!is.null(sel) && sel %in% visible_choices) {
        active(sel)
      }
    })

    # When active changes, update the view (keep pagination)
    observeEvent(active(), {
      log_debug(sprintf("[Data] Switch -> active=%s", active()))
      update_view(resetPaging = FALSE)
      push_splice_threshold()
    }, ignoreInit = TRUE)

    # ---- % Data Shown modal ----
    observeEvent(input$open_data_modal, {
      showModal(modalDialog(
        title = "Adjust Data Display",
        sliderInput(
          ns("data_slider"), "Data Shown (%)",
          min = 0, max = 100,
          value = slice_pct(), step = 1, post = "%",
          dragRange = TRUE
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_data_percentage"), "Apply")
        ),
        easyClose = FALSE
      ))
    })

    observe({
      shared_rx$data_version()
      rng <- slice_pct()
      req(!is.null(rng), length(rng) == 2)
      # depend on shared_rx$data_version() to update on dataset changes
      log_info(sprintf("triggered slice summary update: %s", paste(rng, collapse = ",")))
      lo <- as.integer(min(rng, na.rm = TRUE))
      hi <- as.integer(max(rng, na.rm = TRUE))
      # get the total number of rows in the active dataset after filtering
      total_rows <- nrow(shared_store$data_for_data[[active()]])
      label <- sprintf("%d-%d%% filtered data (total:%d)", lo, hi, total_rows)
      session$sendCustomMessage(ns("updateSliceBtn"), label)
    })

    observeEvent(input$confirm_data_percentage, {
      removeModal()
      rng <- input$data_slider
      if (is.null(rng) || length(rng) != 2) return()
      slice_pct(rng)
      log_debug(sprintf("[Data] Slice set to %d%%-%d%% (active=%s)", as.integer(rng[1]), as.integer(rng[2]), active()))
      update_view(resetPaging = TRUE)
    })

    # ---- Dedicated DT readiness gate (decoupled from logging) ----
    observeEvent(input$tbl_ready, {
      if (!dt_ready()) {
        dt_ready(TRUE)
        log_info("[Data] DT is ready in browser; enabling replaceData updates.")
        update_view(resetPaging = TRUE)
        # Now that we're rendered, re-sync to hide synthetic if real datasets exist
        sync_choices_and_active("dt_ready")
        push_splice_threshold()
      }
    }, once = TRUE, ignoreInit = FALSE)

    # ---- Snapshot logging (optional) ----
    snapshot_counter <- reactiveVal(0L)
    observeEvent(input$tbl_snapshot, ignoreInit = FALSE, {
      snap <- input$tbl_snapshot
      ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      n <- isolate(snapshot_counter()) + 1L
      snapshot_counter(n)

      logf("\n===== tbl_snapshot #", n, " @ ", ts, " =====\n", sep = "")
      logf("[reason] ", snap$reason %||% "unspecified",
          " | [version] ", snap$version %||% "unknown",
          " | [ts] ", as.character(snap$ts), "\n", sep = "")
      logf("[headers (browser, visible)] ", paste((snap$headers %||% character()), collapse = ", "), "\n", sep = "")
      if (!is.null(snap$orderAll)) logf("[orderAll (ColReorder, 0-based, all cols)] ", paste(as.integer(snap$orderAll), collapse = ","), "\n", sep = "")
      if (!is.null(snap$dataSrc))  logf("[dataSrc] ", paste(as.integer(snap$dataSrc), collapse = ","), "\n", sep = "")
      logf("===== end tbl_snapshot =====\n")
    })

    # ---- Inline edits (commit to FULL data; re-slice) ----
    observeEvent(input$tbl_cell_edit, ignoreInit = TRUE, {
      if (!rendered()) return()
      info_raw <- input$tbl_cell_edit
      if (is.null(info_raw)) return()

      info <- if (is.data.frame(info_raw)) as.list(info_raw[nrow(info_raw), , drop = FALSE]) else info_raw
      value_raw <- if (!is.null(info$value)) as.character(info$value) else if (!is.null(info$newValue)) as.character(info$newValue) else NA_character_

      row_id    <- isolate(input$tbl_edit_row_id)
      col_orig0 <- isolate(input$tbl_edit_col_orig0)

      act <- active()
      if (is.null(act) || identical(act, synthetic_key)) {
        if (identical(act, synthetic_key)) {
          showNotification("Edits to this dummy dataset will not be saved.", type = "warning", duration = 2)
        }
        return()
      }

      df_full <- shared_store$data_for_data[[act]]
      if (is.null(df_full) || !data.table::is.data.table(df_full)) {
        showNotification("Dataset not editable (missing or not a data.table).", type = "error", duration = 3)
        return()
      }

      # Ensure .row_id exists and is unique
      if (!".row_id" %in% names(df_full)) {
        showNotification("Missing .row_id column; cannot map edits.", type = "error", duration = 3)
        return()
      }
      if (anyDuplicated(df_full$.row_id)) {
        showNotification("Non-unique .row_id; cannot safely edit.", type = "error", duration = 3)
        return()
      }
      if (!data.table::haskey(df_full) || data.table::key(df_full)[1] != ".row_id") {
        data.table::setkey(df_full, .row_id)
      }

      # Validate mapping inputs
      if (is.null(row_id) || is.na(suppressWarnings(as.integer(row_id)))) {
        showNotification("Row id missing; retry edit.", type = "warning", duration = 2)
        update_view(FALSE); return()
      }
      if (is.null(col_orig0) || is.na(suppressWarnings(as.integer(col_orig0)))) {
        showNotification("Column index missing; retry edit.", type = "warning", duration = 2)
        update_view(FALSE); return()
      }

      row_id    <- as.integer(row_id)
      col_orig0 <- as.integer(col_orig0)
      cn        <- colnames(df_full)
      col_idx1  <- col_orig0 + 1L
      if (col_idx1 < 1L || col_idx1 > length(cn)) {
        showNotification("Column out of range.", type = "warning", duration = 2)
        update_view(FALSE); return()
      }

      colname <- cn[col_idx1]
      if (identical(colname, ".row_id")) {
        showNotification(".row_id is not editable.", type = "warning", duration = 2)
        update_view(FALSE); return()
      }

      # Confirm target row exists
      if (df_full[.(row_id), .N] == 0L) {
        showNotification("Row not found; refresh dataset.", type = "error", duration = 2)
        update_view(FALSE); return()
      }

      # Editable allowlist
      editable_cols <- dataset_editable_cols(act, df_full)
      if (!(colname %in% editable_cols)) {
        showNotification(sprintf("Column '%s' is read-only.", colname), type = "warning", duration = 2)
        update_view(FALSE); return()
      }

      old_val <- df_full[.(row_id), get(colname)]

      # Validate / coerce
      v <- dataset_validate_and_coerce(act, df_full, colname, value_raw)
      if (!isTRUE(v$ok)) {
        showNotification(v$message %||% "Invalid value.", type = v$type %||% "error", duration = 3)
        update_view(FALSE); return()
      }
      new_val <- v$value

      # Assign in working copy (keyed set)
      df_full[.(row_id), (colname) := new_val]
      dataset_after_edit(act, df_full, row_id, colname, old_val, new_val)

      # Optional sync to original
      update_original <- TRUE
      if (update_original) {
        tryCatch({
          df_orig <- shared_store$original_data[[act]]
          if (!is.null(df_orig) && data.table::is.data.table(df_orig)) {
            if (!".row_id" %in% names(df_orig)) stop("Original dataset missing .row_id")
            if (anyDuplicated(df_orig$.row_id)) stop("Original dataset has duplicate .row_id")
            if (!data.table::haskey(df_orig) || data.table::key(df_orig)[1] != ".row_id") {
              data.table::setkey(df_orig, .row_id)
            }
            if (!identical(
              lobstr::obj_addr(df_orig),
              lobstr::obj_addr(shared_store$data_for_data[[act]])
            )) {
              df_orig[.(row_id), (colname) := new_val]
              dataset_after_edit(act, df_orig, row_id, colname, old_val, new_val)
            } else {
              log_error("[edit] Original and working share pointer; is this intended?")
            }
          }
        }, error = function(e) {
          showNotification("Failed to update original data.", type = "warning", duration = 3)
          log_error(sprintf("original sync failed: %s", conditionMessage(e)))
        })
      }

      update_view(resetPaging = FALSE)
      showNotification("Saved", type = "message", duration = 1.2)
      log_info(sprintf("[Edit OK] .row_id=%d col=%s old=%s new=%s",
                  row_id, colname, as.character(old_val), as.character(new_val)))
    })


    observeEvent(input$save_table, {
      work_dir <- shared_store$work_dir
      default_name <- paste0("table-", Sys.Date(), ".tsv")
      default_name <- file.path(work_dir, default_name)
      showModal(modalDialog(
        title = "Save table to server path",
        textInput(ns("save_path"), "Server file path (including filename)", value = default_name),
        checkboxInput(ns("save_allow_overwrite"), "Allow overwrite", value = FALSE),
        checkboxInput(ns("save_visible_only"), "Visible columns only", value = TRUE),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_table_confirm"), "Save")
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$save_table_confirm, {
      req(input$save_path)
      path <- trimws(input$save_path)
      allow_ovr <- isTRUE(input$save_allow_overwrite)
      visible_only <- isTRUE(input$save_visible_only)

      if (identical(path, "") || is.null(path)) {
        showNotification("Please provide a valid server path.", type = "error")
        return()
      }

      if (file.exists(path) && !allow_ovr) {
        removeModal()
        showModal(modalDialog(
          title = "Confirm overwrite",
          sprintf("The file already exists at: %s", path),
          p("Do you want to overwrite it?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("overwrite_yes"), "Yes"),
            actionButton(ns("overwrite_no"), "No")
          ),
          easyClose = FALSE
        ))
        session$userData$pending_save <- list(path = path, visible_only = visible_only)
        return()
      }

      tryCatch({
        perform_save(path, visible_only)
        removeModal()
        showNotification(sprintf("Table saved to %s", path), type = "message", duration = 6)
      }, error = function(e) {
        showNotification(sprintf("Failed to save table: %s", e$message), type = "error", duration = 8)
      })
    })

    observeEvent(input$overwrite_yes, {
      pending <- session$userData$pending_save
      if (is.null(pending) || is.null(pending$path)) {
        removeModal()
        showNotification("No pending save found.", type = "error")
        return()
      }
      path <- pending$path
      visible_only <- pending$visible_only %||% TRUE
      tryCatch({
        perform_save(path, visible_only)
        removeModal()
        showNotification(sprintf("File overwritten: %s", path), type = "message", duration = 6)
      }, error = function(e) {
        removeModal()
        showNotification(sprintf("Failed to overwrite file: %s", e$message), type = "error", duration = 8)
      })
      session$userData$pending_save <- NULL
    })

    observeEvent(input$overwrite_no, {
      pending <- session$userData$pending_save
      prev_path <- if (!is.null(pending$path)) pending$path else paste0("table-", Sys.Date(), ".tsv")
      prev_selected <- if (!is.null(pending$visible_only)) pending$visible_only else TRUE
      session$userData$pending_save <- NULL
      removeModal()
      showModal(modalDialog(
        title = "Save table to server path",
        textInput(ns("save_path"), "Server file path (including filename)", value = prev_path),
        checkboxInput(ns("save_allow_overwrite"), "Allow overwrite", value = FALSE),
        checkboxInput(ns("save_visible_only"), "Selected columns only", value = prev_selected),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_table_confirm"), "Save")
        ),
        easyClose = FALSE
      ))
    })

    # ---- Save table to server path logic ----
    perform_save <- function(path, visible_only) {
      # Get the active dataset
      act <- isolate(active())
      tbl <- isolate(shared_store$data_for_data[[act]])
      if (is.null(tbl)) stop("No filtered table available to save.")
      # Read the latest DT snapshot (visible headers + overall order)
      snap <- isolate(input$tbl_snapshot)
      # Start from the full table
      final_tbl <- tbl
      # If visible_only, keep only the visible columns, in the same order they appear
      if (isTRUE(visible_only)) {
        vis_headers <- as.character(snap$headers %||% character(0))
        if (length(vis_headers)) {
          keep <- vis_headers[vis_headers %in% colnames(final_tbl)]
          if (!length(keep)) stop("No visible columns detected to save.")
          # tbl is data table
          # final_tbl <- final_tbl[, ..keep]
          final_tbl <- tbl[, .SD, .SDcols = keep]
        } else {
          stop("No visible columns detected to save (table not ready?).")
        }
      }
      if (is.null(final_tbl) || !ncol(final_tbl)) {
        stop("No data to save. Ensure the table is rendered and visible columns are available.")
      }
      # Validate directory exists
      dirpath <- dirname(path)
      if (!dir.exists(dirpath)) {
        stop(sprintf("Directory '%s' does not exist. The server will not create directories automatically.", dirpath))
      }
      # Write TSV
      # if .row_id is present, remove it before writing
      if (".row_id" %in% colnames(final_tbl)) {
        final_tbl <- final_tbl[, !".row_id", with = FALSE]
      }
      write.table(final_tbl, file = path, sep = "\t", row.names = FALSE, quote = FALSE)
      TRUE
    }

    # Forward clicked IDs to shared_rx; do NOT switch tabs here
    observeEvent(input$selected_igv_id, {
      id_clicked <- input$selected_igv_id
      log_info(sprintf("[Data] ID clicked for IGV: %s", as.character(id_clicked)))
      # get the active dataset and get the row corresponding to the clicked ID
      act <- isolate(active())
      tbl <- isolate(shared_store$data_for_data[[act]])
      if (!is.null(tbl)) {
        # get the row corresponding to the clicked ID
        row <- tbl[tbl$ID == id_clicked, , drop = FALSE]
        columns <- names(row)
        selected <- c("ID", "CHROM", "POS", "VAR_LENGTH")
        if (!all(selected %in% columns)) {
          missing_cols <- setdiff(selected, columns)
          log_info(sprintf("[Data] Missing columns for ID %s: %s", as.character(id_clicked), paste(missing_cols, collapse = ", ")))
          return()
        }
        var_type <- if("VAR_TYPE" %in% names(row)) row$VAR_TYPE[1] else NULL
        if (is.null(var_type)){
          log_info(sprintf("[Data] Missing VAR_TYPE for ID %s", as.character(id_clicked)))
          return()
        }
        chrom <- if ("CHROM" %in% names(row)) row$CHROM[1] else NULL
        pos <- if ("POS" %in% names(row)) row$POS[1] else NULL
        var_length <- if ("VAR_LENGTH" %in% names(row)) row$VAR_LENGTH[1] else NULL
        alt <- if ("ALT" %in% names(row)) row$ALT[1] else NULL
        log_info(sprintf("[Data] ID %s maps to CHROM=%s POS=%s VAR_LENGTH=%s VAR_TYPE=%s", as.character(id_clicked), as.character(chrom), as.character(pos), as.character(var_length), as.character(var_type)))
        info_list <- list(
          ID =  as.character(id_clicked),
          CHROM = as.character(chrom),
          POS = as.integer(pos),
          VAR_LENGTH = as.integer(var_length),
          ALT = as.character(alt),
          VAR_TYPE = as.character(var_type)
        )
        # add to the shared_store$igv_data list
        shared_store$igv_data[["igv_info"]] <- info_list
        bump_version(version_type = "igv", shared_rx = shared_rx)
      }
    }, ignoreInit = TRUE)

    # obsever gene symbol clicks
    observeEvent(input$selected_genesymbol, {
      gene_symbol <- input$selected_genesymbol
      log_info(sprintf("[Data] Gene symbol clicked: %s", as.character(gene_symbol)))
      # store in shared_store$gene_symbol_data
      shared_store$gene_symbol_data <- as.character(gene_symbol)
      bump_version(version_type = "genesymbol", shared_rx = shared_rx)
    }, ignoreInit = TRUE)

    # observer for shared_rx$genesymbol_version and if the module is panelapp then update the search box
    observeEvent(shared_rx$genesymbol_version(), {
      req(rendered())
      # only if prefix is "panel_app"
      if (prefix != "panel_app") return()
      log_info("[Data] genesymbol_version bumped; updating search box")
      gene_symbol <- shared_store$gene_symbol_data
      DT::updateSearch(proxy, keywords = list(global = gene_symbol))
    }, ignoreInit = TRUE)

    # observer HPO ID clicks
    observeEvent(input$selected_hpo_id, {
      hpo_id <- input$selected_hpo_id
      log_info(sprintf("[Data] HPO ID clicked: %s", as.character(hpo_id)))
      # store in shared_store$hpo_id_data
      shared_store$hpo_id_data <- as.character(hpo_id)
      bump_version(version_type = "hpoid", shared_rx = shared_rx)
    }, ignoreInit = TRUE)

    # observer for shared_rx$hpoid_version and if the module is phenotype then update the search box
    observeEvent(shared_rx$hpoid_version(), {
      req(rendered())
      # only if prefix is "phenotype"
      if (prefix != "phenotype") return()
      log_info("[Data] hpoid_version bumped; updating search box")
      hpo_id <- shared_store$hpo_id_data
      DT::updateSearch(proxy, keywords = list(global = hpo_id))
    }, ignoreInit = TRUE)

  }) # # end moduleServer
} # end data_module_server
