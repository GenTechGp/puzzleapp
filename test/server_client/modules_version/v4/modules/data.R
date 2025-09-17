# data_module.R
# Multi-dataset Data module using shared_store$data_for_data (named list of datasets).
# Preserves existing editing, reordering, snapshotting, slice modal, export TSV, and DT readiness logic.
# Added: persistent row highlight for Sepal.Width == 3 (survives column hide).

dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* DT ColVis dropdown: single column with ~20 items visible and vertical scroll */
      div.dt-button-collection {
        max-height: 640px;   /* ~20 rows depending on line-height */
        overflow-y: auto; !important;
        overflow-x: hidden;
      }
    ")),
    if (exists("dataset_specific_css")) dataset_specific_css(),
    fluidRow(
      column(5,),
      column(2,
        tags$div(strong("Active dataset:"), textOutput(ns("active_label"), inline = TRUE)),
      ),
      column(2,
        tags$div(strong("Slice summary:"), textOutput(ns("slice_summary"), inline = TRUE))
      ),
      column(2,
        selectInput(ns("dataset_select"), label = NULL, choices = character(0)),
      ),
      column(1,
        actionButton(ns("use_dataset"), "Switch dataset", icon = icon("check"))
      )
    ),
    DTOutput(ns("tbl"))
  )
}

dataServer <- function(id, shared_store, shared_rx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(x, y) if (is.null(x)) y else x

    log_enabled <- if (!is.null(shared_store$verbose_level) && is.numeric(shared_store$verbose_level)) {
      shared_store$verbose_level >= 1L
    } else {
      FALSE
    }
    logf <- function(...) if (isTRUE(log_enabled)) cat(...)

    # ---- Data module local state ----
    active <- reactiveVal(NULL)           # active dataset key from shared_store$data_for_data
    slice_pct <- reactiveVal(c(0, 10))    # default: first 10%
    rendering_counter <- reactiveVal(0L)  # no UI; used only for logging
    rendered <- reactiveVal(FALSE)        # has the table been rendered once?
    dt_ready <- reactiveVal(FALSE)        # DataTables in browser is initialized

    output$active_label <- renderText({ active() %||% "<none>" })

    # ---- Helpers (private to Data) ----
    apply_slice <- function(df_full, slice) {
      n <- nrow(df_full)
      if (!n) return(df_full)
      pmin <- max(0, min(slice))
      pmax <- min(100, max(slice))
      i <- floor(pmin / 100 * max(n - 1, 0)) + 1L
      j <- floor(pmax / 100 * n)
      if (j < i) j <- i              # enforce ≥ 1 row
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
      pref_init <- isolate(shared_store$preferred_cols %||% character(0))
      pref_js <- jsonlite::toJSON(unname(as.character(pref_init)), auto_unbox = TRUE)

      output$tbl <- renderDT({
        # Render once; avoid reactive deps; compute initial view with isolate
        n <- isolate(rendering_counter()) + 1L
        rendering_counter(n)
        cat(sprintf("[Data] [Render] rendering_counter = %d\n", n))

        cn <- colnames(df0)
        rid_idx0 <- which(cn == ".row_id") - 1L

        column_defs <- list()
        if (length(rid_idx0)) {
          column_defs <- list(
            list(targets = rid_idx0, visible = FALSE, searchable = FALSE, orderable = FALSE, className = "noVis")
          )
        }

        # Namespaced input ids for JS callbacks
        id_snapshot <- ns("tbl_snapshot")
        id_edit_row <- ns("tbl_edit_row_id")
        id_edit_col <- ns("tbl_edit_col_orig0")
        id_ready <- ns("tbl_ready")

        # Pre-encode strings for safe JS embedding
        id_snapshot_json <- jsonlite::toJSON(id_snapshot, auto_unbox = TRUE)
        id_edit_row_json <- jsonlite::toJSON(id_edit_row, auto_unbox = TRUE)
        id_edit_col_json <- jsonlite::toJSON(id_edit_col, auto_unbox = TRUE)
        id_ready_json    <- jsonlite::toJSON(id_ready,    auto_unbox = TRUE)
        ds_js <- if (exists("dataset_specific_js_highlight")) dataset_specific_js_highlight() else character(0)
        cb_lines <- c(
          "var tbl = table;",
          "var ridIdx0 = ", ifelse(length(rid_idx0), rid_idx0, "null"), ";",
          "var preferredInit = ", pref_js, ";",

          ds_js,

          "function computeSnapshot(reason){",
          " try{",
          "  var headers=[];",
          "  tbl.columns(':visible').every(function(idx){",
          "    var label=(this.header() && this.header().textContent)?this.header().textContent.trim():(''+idx);",
          "    headers.push(label);",
          "  });",
          "  var orderAll = tbl.colReorder ? tbl.colReorder.order() : null;",
          "  if(!orderAll){ orderAll=[]; var nCols=tbl.columns().count(); for(var i=0;i<nCols;i++) orderAll.push(i); }",
          "  var dataSrc = tbl.columns().dataSrc();",
          "  if(dataSrc && dataSrc.toArray) dataSrc = dataSrc.toArray();",
          "  Shiny.setInputValue(", id_snapshot_json, ", {",
          "    headers: headers, orderAll: orderAll, dataSrc: dataSrc, reason: reason || 'unspecified', version: 'exact-colrow-v3', ts: Date.now()",
          "  }, {priority:'event'});",
          " }catch(e){ console && console.error && console.error('computeSnapshot error', e); }",
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

          "tbl.on('mousedown.dt','tbody td',function(e){ captureEditCtx(this); });",
          "tbl.on('focusin.dt','tbody td',function(e){ captureEditCtx(this); });",
          "tbl.on('focusin.dt','tbody input, tbody textarea',function(e){ captureEditCtx(this); });",

          "setTimeout(function(){ computeSnapshot('tick0'); }, 0);",
          "setTimeout(function(){ computeSnapshot('tick50'); }, 50);",
          "computeSnapshot('manual');"
        )

        cb_code <- paste(cb_lines, collapse = "\n")
        cb <- JS(cb_code)
        # Filename function for TSV export
        filename_js <- JS("
          function() {
            var ts = new Date().toISOString().replace(/[:.]/g, '-');
            return 'export-' + ts; // no extension here; Buttons will append .tsv
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
          rownames = FALSE,
            filter = "top",
          editable = list(target = "cell", numeric = "none"),
          extensions = c("ColReorder", "Buttons"),
          options = list(
            dom = "Blfrtip",
            buttons = list(
              list(
                extend = "colvis",
                columns = ":not(.noVis)",
                text = "Columns"
              ),
              list(
                extend = "collection",
                text = "% Data Shown",
                action = JS(sprintf("function (e, dt, node, config) { Shiny.setInputValue('%s', true, {priority: 'event'}); }", ns("open_data_modal")))
              ),
              list(
                extend = "csvHtml5",
                text = "Download (visible current page)",
                fieldSeparator = "\t",
                extension = ".tsv",   # key line: let Buttons append .tsv
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
              )
            ),
            colReorder = TRUE,
            pageLength = 10,
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

      # Create proxy now that the table exists (IMPORTANT: scope with session)
      proxy <<- dataTableProxy("tbl", session = session)
      rendered(TRUE)
      cat("[Data] DT rendered in UI (proxy created)\n")

      # Note: No initial push here; dt_ready gate will handle the first update
    }

    # ---- View updater (safe no-op if proxy not ready) ----
    update_view <- function(resetPaging = TRUE) {
      if (!rendered() || !dt_ready() || is.null(proxy)) return(invisible(NULL))
      act <- active()
      if (is.null(act)) return(invisible(NULL))
      df_full <- shared_store$data_for_data[[act]]
      if (is.null(df_full)) return(invisible(NULL))

      # fetch filtered
      total_n <- nrow(df_full)
      # --- Retrieve mask (may not exist yet) ---
      mask_list <- shared_store$filter_for_data
      mask <- if (!is.null(mask_list)) mask_list[[act]] else NULL
      valid_mask <- !is.null(mask) && is.logical(mask) && length(mask) == total_n
      if (!valid_mask) {
        # Fallback: no mask → treat all rows as passing
        filtered_idx <- if (total_n) seq_len(total_n) else integer(0)
      } else {
        # Missing column policy was "all FALSE" for missing; that's already in mask
        # Just take rows with TRUE
        filtered_idx <- which(mask)
      }
      filtered_n <- length(filtered_idx)
      # --- Build filtered subset BEFORE slicing ---
      if (filtered_n == 0) {
        # Preserve column structure (important for replaceData)
        df_filtered <- df_full[0, , drop = FALSE]
      } else {
        # NOTE: filtered_idx is in ascending order already (which() behavior)
        df_filtered <- df_full[filtered_idx, , drop = FALSE]
      }
      # --- Apply existing percentage slice logic to filtered subset ---
      # Reuse apply_slice WITHOUT modifying its internal logic:
      # It treats input data as the "universe", so now the universe is df_filtered
      view_df <- apply_slice(df_filtered, slice_pct())
      # view_df <- apply_slice(df_full, slice_pct())

      cat(sprintf(
        "[Data] update_view total=%d filtered=%d slice_rows=%d cols=%d\n",
        total_n, filtered_n, nrow(view_df), ncol(view_df)
      ))

      # Diagnostics: structure and column equality with initial table
      cat(sprintf("[Data] update_view rows=%d cols=%d\n", nrow(view_df), ncol(view_df)))
      if (!is.null(initial_colnames) && !identical(names(view_df), initial_colnames)) {
        cat("[Data][WARN] Column mismatch in update_view:\n")
        cat("  initial: ", paste(initial_colnames, collapse = ", "), "\n", sep = "")
        cat("  current: ", paste(names(view_df), collapse = ", "), "\n", sep = "")
        return(invisible(NULL))
      }
      replaceData(proxy, data = view_df, resetPaging = resetPaging, rownames = FALSE, clearSelection = "none")
    }

    # ---- Sync helper: choices, visibility, and active selection ----
    sync_choices_and_active <- function(reason = "unspecified") {
      choices_all <- sort(names(shared_store$data_for_data) %||% character(0))
      curr_active <- isolate(active())
      if (!rendered()) {
        # Pre-render: prefer synthetic to show the demo first; keep all choices visible
        active_new <- if (!is.null(curr_active) && curr_active %in% choices_all) curr_active else {
          if ("[Synthetic] Boundary" %in% choices_all) "[Synthetic] Boundary" else (choices_all[1] %||% NULL)
        }
        visible_choices <- choices_all
      } else {
        # Post-render: hide synthetic if any real dataset exists; prefer a sensible real dataset
        non_synth <- setdiff(choices_all, "[Synthetic] Boundary")
        visible_choices <- if (length(non_synth) >= 1) non_synth else choices_all
        if (!is.null(curr_active) && curr_active %in% visible_choices) {
          active_new <- curr_active
        } else if (length(non_synth) > 0) {
          active_new <- non_synth[1]
        } else if ("[Synthetic] Boundary" %in% choices_all) {
          active_new <- "[Synthetic] Boundary"
        } else {
          active_new <- NULL
        }
      }

      # UI selection must be among visible choices
      selected_ui <- if (!is.null(active_new) && active_new %in% visible_choices) active_new else {
        if (length(visible_choices) >= 1) visible_choices[1] else NULL
      }
      updateSelectInput(session, "dataset_select", choices = visible_choices, selected = selected_ui)
      cat("active():", active(), "\n")
      cat("active_new():", active_new, "\n")
      if (!identical(active(), active_new)) {
        cat(sprintf("[Data] sync(%s) -> active set to '%s'\n", reason, as.character(active_new %||% "<none>")))
        active(active_new)
      }

    }

    # ---- Dataset selection UI sync on version bumps ----
    observeEvent(shared_rx$version(), {
      sync_choices_and_active("version")

      # Render or update view
      choices_all <- names(shared_store$data_for_data)
      if (!rendered()) {
        if (length(choices_all)) {
          cat(sprintf("[Data] First version bump detected (%d): rendering table (active=%s)\n", shared_rx$version(), active()))
          render_tbl_once()
        }
      } else {
        cat(sprintf("[Data] Version bump detected: %d (active=%s)\n", shared_rx$version(), active()))
        update_view(resetPaging = TRUE)
      }
    }, ignoreInit = FALSE)

    # ---- Apply button: switch active dataset ----
    observeEvent(input$use_dataset, {
      sel <- input$dataset_select
      visible_choices <- isolate({
        # Rebuild visibility the same way as sync (without side effects)
        choices_all <- sort(names(shared_store$data_for_data) %||% character(0))
        if (rendered() && length(setdiff(choices_all, "[Synthetic] Boundary")) >= 1) {
          setdiff(choices_all, "[Synthetic] Boundary")
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
      cat(sprintf("[Data] Switch -> active=%s\n", active()))
      update_view(resetPaging = FALSE)
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
        easyClose = TRUE
      ))
    })

    output$slice_summary <- renderText({
      rng <- slice_pct()
      if (is.null(rng) || length(rng) != 2) return("")
      lo <- as.integer(min(rng, na.rm = TRUE))
      hi <- as.integer(max(rng, na.rm = TRUE))
      sprintf("Showing %d–%d%%", lo, hi)
    })

    observeEvent(input$confirm_data_percentage, {
      removeModal()
      rng <- input$data_slider
      if (is.null(rng) || length(rng) != 2) return()
      slice_pct(rng)
      cat(sprintf("[Data] Slice set to %d%%–%d%% (active=%s)\n", as.integer(rng[1]), as.integer(rng[2]), active()))
      update_view(resetPaging = TRUE)
    })

    # ---- Dedicated DT readiness gate (decoupled from logging) ----
    observeEvent(input$tbl_ready, {
      if (!dt_ready()) {
        dt_ready(TRUE)
        cat("[Data] DT is ready in browser; enabling replaceData updates.\n")
        update_view(resetPaging = TRUE)
        # Now that we're rendered, re-sync to hide synthetic if real datasets exist
        sync_choices_and_active("dt_ready")
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

      row_id <- isolate(input$tbl_edit_row_id)
      col_orig0 <- isolate(input$tbl_edit_col_orig0)

      act <- active()
      if (is.null(act)) return()
      df_full <- shared_store$data_for_data[[act]]
      if (is.null(df_full)) return()
      cn <- colnames(df_full)
      scn <- isolate(snapshot_counter())

      cat("\n---- edit event (using snapshot #", scn, ") ----\n", sep = "")
      cat(sprintf("[edit raw] value=%s client_row_id=%s client_col_orig0=%s\n",
                  as.character(value_raw), as.character(row_id), as.character(col_orig0)))

      if (is.null(row_id) || is.na(suppressWarnings(as.integer(row_id)))) {
        cat("[edit] client .row_id missing; rejecting edit\n")
        showNotification("Couldn't map row; please retry.", type = "warning", duration = 2)
        update_view(resetPaging = FALSE)
        return()
      }
      if (is.null(col_orig0) || is.na(suppressWarnings(as.integer(col_orig0)))) {
        cat("[edit] client original column index missing; rejecting edit\n")
        showNotification("Couldn't map column; please retry.", type = "warning", duration = 2)
        update_view(resetPaging = FALSE)
        return()
      }
      row_id <- as.integer(row_id)
      col_orig0 <- as.integer(col_orig0)

      row_idx <- if (".row_id" %in% names(df_full)) match(row_id, df_full$.row_id) else NA_integer_
      col_idx1 <- col_orig0 + 1L
      if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df_full)) {
        cat("[edit] .row_id not found in data; rejecting edit\n")
        showNotification("Row not found; please refresh.", type = "error", duration = 2)
        update_view(resetPaging = FALSE)
        return()
      }
      if (is.na(col_idx1) || col_idx1 < 1 || col_idx1 > ncol(df_full)) {
        cat("[edit] Column index out of range; rejecting edit\n")
        showNotification("Column not found; please retry.", type = "warning", duration = 2)
        update_view(resetPaging = FALSE)
        return()
      }
      colname <- cn[col_idx1]
      if (identical(colname, ".row_id")) {
        cat("[edit] Attempt to edit .row_id blocked; rejecting\n")
        showNotification("This column is not editable.", type = "warning", duration = 2)
        update_view(resetPaging = FALSE)
        return()
      }

      # Dataset-specific allowlist (no fallback)
      editable_cols <- dataset_editable_cols(act, df_full)
      if (!(colname %in% editable_cols)) {
        cat(sprintf("[edit] Column '%s' is read-only; rejecting\n", colname))
        showNotification(sprintf("Column '%s' is read-only.", colname), type = "warning", duration = 2)
        update_view(resetPaging = FALSE)
        return()
      }

      # BEFORE
      row_before <- df_full[row_idx, , drop = FALSE]
      cat("[row before] ", fmt_row(row_before), "\n", sep = "")

      # Dataset-specific validation/coercion (no fallback)
      v <- dataset_validate_and_coerce(act, df_full, colname, value_raw)
      if (!isTRUE(v$ok)) {
        showNotification(v$message %||% "Invalid value.", type = v$type %||% "error", duration = 3)
        update_view(resetPaging = FALSE)
        return()
      }
      new_val <- v$value

      old_val <- df_full[[colname]][row_idx]
      df_full[[colname]][row_idx] <- new_val

      # Optional post-commit hook (dataset-specific cascades; no fallback)
      df_full <- dataset_after_edit(act, df_full, row_idx, colname, old_val, new_val)

      # AFTER
      row_after <- df_full[row_idx, , drop = FALSE]
      cat("[row after ] ", fmt_row(row_after), "\n", sep = "")

      # Persist back to shared store and update view
      shared_store$data_for_data[[act]] <- df_full
      update_view(resetPaging = FALSE)

      cat(sprintf("[Edit OK] .row_id=%d row=%d col=%s old=%s new=%s\n",
                  as.integer(row_id), row_idx, colname, as.character(old_val), as.character(new_val)))
      showNotification("Saved", type = "message", duration = 1.2)
      # If Home needs to react to edits, uncomment next line to bump version:
      # shared_rx$version(shared_rx$version() + 1L)
    })
  })
}