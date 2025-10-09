library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT editing with exact .row_id and exact original column index (no guesswork) + TSV export + Slicing"),
  fluidRow(
    column(3, numericInput("n", "Sample size (n)", value = 30, min = 1, max = nrow(iris), step = 1)),
    column(3, actionButton("resample", "Resample n", icon = icon("shuffle"))),
    column(3, actionButton("refresh", "Refresh (noise)", icon = icon("rotate"))),
    column(3, actionButton("switch", "Switch dataset A/B", icon = icon("exchange")))
  ),
  verbatimTextOutput("render_count", placeholder = TRUE),
  DTOutput("tbl")
)

server <- function(input, output, session) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # -------------------- Helpers --------------------
  add_row_id <- function(df) {
    df$.row_id <- seq_len(nrow(df))
    df
  }
  sample_base <- function(n) {
    n <- max(1, min(n, nrow(iris)))
    iris[sample.int(nrow(iris), n, replace = FALSE), , drop = FALSE]
  }
  make_variant_from <- function(df, seed_offset = 0) {
    set.seed(123 + as.integer(seed_offset))
    d <- df
    nums <- sapply(d, is.numeric)
    nums[".row_id"] <- FALSE
    d[nums] <- lapply(d[nums], function(col) col + rnorm(length(col), sd = 0.2))
    d
  }
  fmt_row <- function(row_df) {
    vals <- lapply(row_df, function(x) as.character(x)[1])
    paste(paste0(names(vals), "=", unlist(vals, use.names = FALSE)), collapse = ", ")
  }

  # Slice utility: map [pmin, pmax] in [0,100] to a contiguous row window over a stable order (.row_id asc)
  apply_slice <- function(df_full, slice_pct) {
    n <- nrow(df_full)
    if (!n) return(df_full)
    pmin <- max(0, min(slice_pct))
    pmax <- min(100, max(slice_pct))
    i <- floor(pmin / 100 * max(n - 1, 0)) + 1L
    j <- floor(pmax / 100 * n)
    if (j < i) j <- i                          # enforce at least 1 row
    if (i < 1L) i <- 1L
    if (j > n) j <- n
    base_idx <- order(df_full$.row_id)
    sel <- base_idx[i:j]
    df_full[sel, , drop = FALSE]
  }

  # -------------------- Initial data (full) + initial slice (first 10%) --------------------
  n0 <- isolate(input$n %||% 30)
  base0 <- add_row_id(sample_base(n0))
  data_A_full0 <- base0
  data_B_full0 <- make_variant_from(base0, seed_offset = 1)

  rv <- reactiveValues(
    data_full = list(A = data_A_full0, B = data_B_full0),  # immutable bases for slicing
    active = "A",
    refresh_seed = 1L,
    slice_pct = c(0, 10)  # start with first 10% of data
  )

  # Editable columns
  editable_cols <- c("Sepal.Width")

  # -------------------- Render DT once; update via replaceData (NO reactive deps) --------------------
  rendering_counter <- reactiveVal(0)
  output$render_count <- renderText({
    paste("rendering_counter =", rendering_counter())
  })

  output$tbl <- renderDT({
    # Render once; avoid reactive deps on rv; compute initial view in isolate
    n <- isolate(rendering_counter()) + 1
    rendering_counter(n)
    cat(sprintf("[Render] rendering_counter = %d\n", n))

    df_full_initial <- isolate(rv$data_full[[rv$active]])
    df <- isolate(apply_slice(df_full_initial, rv$slice_pct))

    cn <- colnames(df)
    rid_idx0 <- which(cn == ".row_id") - 1L

    # Hide .row_id and exclude it from colvis UI
    column_defs <- list()
    if (length(rid_idx0)) {
      column_defs <- list(
        list(targets = rid_idx0, visible = FALSE, searchable = FALSE, orderable = FALSE, className = "noVis")
      )
    }

    # JS:
    # - computeSnapshot(reason): diagnostics
    # - captureEditCtx(node): captures exact .row_id and exact original column index and sends them to Shiny
    cb <- JS(sprintf("
      var tbl = table;
      var ridIdx0 = %d; // original index (0-based) of .row_id

      function computeSnapshot(reason) {
        try {
          var headers = [];
          tbl.columns(':visible').every(function(idx){
            var label = (this.header() && this.header().textContent) ? this.header().textContent.trim() : ('' + idx);
            headers.push(label);
          });

          var orderAll = tbl.colReorder ? tbl.colReorder.order() : null;
          if (!orderAll) {
            orderAll = [];
            var nCols = tbl.columns().count();
            for (var i = 0; i < nCols; i++) orderAll.push(i);
          }

          var dataSrc = tbl.columns().dataSrc();
          if (dataSrc && dataSrc.toArray) dataSrc = dataSrc.toArray();

          Shiny.setInputValue('tbl_snapshot', {
            headers: headers,
            orderAll: orderAll,
            dataSrc: dataSrc,
            reason: reason || 'unspecified',
            version: 'exact-colrow-v3',
            ts: Date.now()
          }, {priority: 'event'});
        } catch (e) {
          console && console.error && console.error('computeSnapshot error', e);
        }
      }

      function captureEditCtx(node) {
        try {
          var $td = $(node).closest('td');
          if ($td.length === 0) return;

          var cell = tbl.cell($td);
          if (!cell || !cell.any()) return;
          var idx = cell.index(); // {row: r, column: c}

          var colDisp = idx.column;
          var colOrig = (tbl.colReorder && tbl.colReorder.transpose)
            ? tbl.colReorder.transpose(colDisp, 'toOriginal')
            : colDisp;

          var rowData = tbl.row(idx.row).data();
          if (!rowData || ridIdx0 == null || isNaN(ridIdx0)) return;
          var rid = rowData[ridIdx0];

          if (rid != null) {
            Shiny.setInputValue('tbl_edit_row_id', rid, {priority: 'event'});
            Shiny.setInputValue('tbl_edit_col_orig0', colOrig, {priority: 'event'});
          }
        } catch (e) {
          console && console.error && console.error('captureEditCtx error', e);
        }
      }

      // Snapshot events
      tbl.on('init.dt',               function(){ computeSnapshot('init'); });
      tbl.on('column-reorder.dt',     function(){ computeSnapshot('colreorder'); });
      tbl.on('column-visibility.dt',  function(){ computeSnapshot('colvisibility'); });
      tbl.on('order.dt',              function(){ computeSnapshot('order'); });
      tbl.on('search.dt',             function(){ computeSnapshot('search'); });
      tbl.on('draw.dt',               function(){ computeSnapshot('draw'); });

      // Capture row/col context near edits
      tbl.on('mousedown.dt', 'tbody td', function(e){ captureEditCtx(this); });
      tbl.on('focusin.dt',   'tbody td', function(e){ captureEditCtx(this); });
      tbl.on('focusin.dt',   'tbody input, tbody textarea', function(e){ captureEditCtx(this); });

      // Seed early snapshot
      setTimeout(function(){ computeSnapshot('tick0'); }, 0);
      setTimeout(function(){ computeSnapshot('tick50'); }, 50);
      computeSnapshot('manual');
    ", rid_idx0))

    # Filename function for TSV export (ISO timestamp, safe for files; extension is handled via 'extension')
    filename_js <- JS("
      function() {
        var ts = new Date().toISOString().replace(/[:.]/g, '-');
        return 'export-' + ts;
      }
    ")

    # Minimal quoting customize: keep quotes only when a cell contains tab/newline/quote
    # Works by parsing the uniformly quoted output from csvHtml5 and rebuilding minimally quoted TSV.
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
              // Shouldn't occur (csvHtml5 quotes all fields), but handle gracefully
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

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      editable = list(target = "cell"),
      extensions = c("ColReorder", "Buttons"),
      options = list(
        # Show Buttons + Length control + Filter + table + info + paging
        dom = "Blfrtip",
        buttons = list(
          list(
            extend = "colvis",
            columns = ":not(.noVis)",
            text = "Columns"
          ),
          # IMPORTANT: Provide 'extend' so DT's wrapper accepts this custom button.
          list(
            extend = "collection",
            text = "% Data Shown",
            action = JS("function (e, dt, node, config) { Shiny.setInputValue('open_data_modal', true, {priority: 'event'}); }")
          ),
          list(
            extend = "csvHtml5",
            text = "Export TSV — visible + current page",
            fieldSeparator = "\t",
            fieldBoundary = "\"",    # start with full quoting; we'll reduce quotes in customize
            bom = TRUE,
            extension = "tsv",       # force .tsv extension
            title = NULL,
            filename = filename_js,   # base name; extension handled above
            exportOptions = list(
              columns = ":visible:not(.noVis)",     # honor visibility, exclude .row_id
              modifier = list(
                search = "applied",
                order = "applied",
                page = "current"                    # current page only
              ),
              stripHtml = TRUE
            ),
            customize = tsv_minimal_quotes_js       # convert to minimal quoting safely
          )
        ),
        colReorder = TRUE,
        pageLength = 10,
        lengthMenu = list(
          c(10, 15, 25, 50, 100, 1000, 10000, -1),
          c("10", "15", "25", "50", "100", "1,000", "10,000", "All")
        ),
        deferRender = TRUE,
        scrollX = TRUE,
        columnDefs = column_defs
      ),
      callback = cb
    )
  }, server = TRUE)

  # Create proxy after the widget is defined
  proxy <- dataTableProxy("tbl")

  # Update the displayed slice in the widget (keeps widget instance; no re-render)
  update_view <- function(resetPaging = TRUE) {
    df_full <- rv$data_full[[rv$active]]
    view_df <- apply_slice(df_full, rv$slice_pct)
    replaceData(proxy, data = view_df, resetPaging = resetPaging, rownames = FALSE, clearSelection = "none")
  }

  # -------------------- % Data Shown modal (global, two-ended slider) --------------------
  observeEvent(input$open_data_modal, {
    showModal(modalDialog(
      title = "Adjust Data Display",
      sliderInput(
        "data_slider", "Data Shown (%)",
        min = 0, max = 100,
        value = rv$slice_pct, step = 1, post = "%",
        dragRange = TRUE
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_data_percentage", "Apply")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_data_percentage, {
    removeModal()
    if (is.null(input$data_slider) || length(input$data_slider) != 2) return()
    # Save global slice and re-derive the view from the FULL dataset
    rv$slice_pct <- input$data_slider
    cat(sprintf("[Slice] Set to %d%%–%d%% on dataset %s (N=%d)\n",
                as.integer(rv$slice_pct[1]), as.integer(rv$slice_pct[2]),
                rv$active, nrow(rv$data_full[[rv$active]])))
    update_view(resetPaging = TRUE)
  })

  # -------------------- Resample / Refresh / Switch --------------------
  observeEvent(input$resample, {
    n <- input$n %||% 30
    base <- add_row_id(sample_base(n))
    rv$data_full$A <- base
    rv$data_full$B <- make_variant_from(base, seed_offset = 1)
    rv$active <- "A"
    rv$refresh_seed <- 1L
    update_view(resetPaging = TRUE)
    cat(sprintf("[Action] Resample n=%d -> active=%s\n", n, rv$active))
  })

  observeEvent(input$refresh, {
    # Add small noise to numeric columns of the FULL current dataset except .row_id
    s <- rv$refresh_seed + 1L
    rv$refresh_seed <- s
    df_full <- isolate(rv$data_full[[rv$active]])
    nums <- sapply(df_full, is.numeric); nums[".row_id"] <- FALSE
    set.seed(1000 + s)
    if (any(nums)) {
      df_full[nums] <- Map(function(col) col + rnorm(length(col), sd = 0.05), df_full[nums])
    }
    rv$data_full[[rv$active]] <- df_full
    update_view(resetPaging = FALSE)
    cat(sprintf("[Action] Refresh(seed=%d) -> active=%s\n", s, rv$active))
  })

  observeEvent(input$switch, {
    rv$active <- if (rv$active == "A") "B" else "A"
    update_view(resetPaging = TRUE)
    cat(sprintf("[Action] Switch -> active=%s (N=%d)\n", rv$active, nrow(rv$data_full[[rv$active]])))
  })

  # -------------------- Snapshot logging with counter (no vis2orig) --------------------
  snapshot_counter <- reactiveVal(0)
  observeEvent(input$tbl_snapshot, ignoreInit = FALSE, {
    snap <- input$tbl_snapshot
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    n <- isolate(snapshot_counter()) + 1L
    snapshot_counter(n)

    cat("\n===== tbl_snapshot #", n, " @ ", ts, " =====\n", sep = "")
    cat("[reason] ", snap$reason %||% "unspecified",
        " | [version] ", snap$version %||% "unknown",
        " | [ts] ", as.character(snap$ts), "\n", sep = "")
    cat("[headers (browser, visible)] ", paste((snap$headers %||% character()), collapse = ", "), "\n", sep = "")

    if (!is.null(snap$orderAll)) {
      cat("[orderAll (ColReorder, 0-based, all cols)] ", paste(as.integer(snap$orderAll), collapse = ","), "\n", sep = "")
    }
    if (!is.null(snap$dataSrc)) {
      cat("[dataSrc] ", paste(as.integer(snap$dataSrc), collapse = ","), "\n", sep = "")
    }
    cat("===== end tbl_snapshot =====\n")
  })

  # -------------------- Inline edits (commit to FULL data; re-slice; no fallbacks) --------------------
  observeEvent(input$tbl_cell_edit, ignoreInit = TRUE, {
    info_raw <- input$tbl_cell_edit
    if (is.null(info_raw)) return()

    info <- if (is.data.frame(info_raw)) as.list(info_raw[nrow(info_raw), , drop = FALSE]) else info_raw
    value_raw <- if (!is.null(info$value)) as.character(info$value) else if (!is.null(info$newValue)) as.character(info$newValue) else NA_character_

    # Strict requirements: client-sent .row_id and original column index
    row_id <- isolate(input$tbl_edit_row_id)
    col_orig0 <- isolate(input$tbl_edit_col_orig0)

    df_full <- isolate(rv$data_full[[rv$active]])
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

    row_idx <- match(row_id, df_full$.row_id)
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

    # Allowlist
    if (!(colname %in% editable_cols)) {
      cat(sprintf("[edit] Column '%s' is read-only; rejecting\n", colname))
      showNotification(sprintf("Column '%s' is read-only.", colname), type = "warning", duration = 2)
      update_view(resetPaging = FALSE)
      return()
    }

    # Print full row BEFORE edit
    row_before <- df_full[row_idx, , drop = FALSE]
    cat("[row before] ", fmt_row(row_before), "\n", sep = "")

    # Validation + commit
    if (identical(colname, "Sepal.Width")) {
      new_num <- suppressWarnings(as.numeric(value_raw))
      if (!is.finite(new_num) || new_num < 2 || new_num > 4) {
        cat(sprintf("[edit] validation failed: %s not in [2,4]\n", as.character(value_raw)))
        showNotification("Sepal.Width must be numeric in [2, 4]. Edit discarded.", type = "error", duration = 3)
        update_view(resetPaging = FALSE)
        return()
      }
      new_val <- round(new_num, 3)
    } else {
      old_col <- df_full[[colname]]
      if (is.numeric(old_col))       new_val <- suppressWarnings(as.numeric(value_raw))
      else if (is.logical(old_col))  new_val <- tolower(trimws(as.character(value_raw))) %in% c('true','t','1','yes','y')
      else                           new_val <- as.character(value_raw)
    }

    old_val <- df_full[[colname]][row_idx]
    df_full[[colname]][row_idx] <- new_val

    # Print full row AFTER edit
    row_after <- df_full[row_idx, , drop = FALSE]
    cat("[row after ] ", fmt_row(row_after), "\n", sep = "")

    # Save FULL data and update the sliced view
    rv$data_full[[rv$active]] <- df_full
    update_view(resetPaging = FALSE)

    cat(sprintf("[Edit OK] .row_id=%d row=%d col=%s old=%s new=%s\n",
                as.integer(row_id), row_idx, colname, as.character(old_val), as.character(new_val)))
    showNotification("Saved", type = "message", duration = 1.2)
  })
}

shinyApp(ui, server)