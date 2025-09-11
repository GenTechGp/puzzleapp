library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT editing with exact .row_id and exact original column index (no guesswork)"),
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

  # -------------------- Initial data (sampled) --------------------
  n0 <- isolate(input$n %||% 30)
  base0 <- add_row_id(sample_base(n0))
  data_A0 <- base0
  data_B0 <- make_variant_from(base0, seed_offset = 1)

  rv <- reactiveValues(
    data = list(A = data_A0, B = data_B0),
    active = "A",
    refresh_seed = 1L
  )

  # Editable columns
  editable_cols <- c("Sepal.Width")

  # -------------------- Render DT once; update via replaceData (NO reactive deps) --------------------
  rendering_counter <- reactiveVal(0)
  output$render_count <- renderText({
    paste("rendering_counter =", rendering_counter())
  })

  output$tbl <- renderDT({
    # Render once; avoid reactive deps on rv$data so edits don't re-render the widget
    n <- isolate(rendering_counter()) + 1
    rendering_counter(n)
    cat(sprintf("[Render] rendering_counter = %d\n", n))

    df <- isolate(rv$data[[rv$active]])

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
    # - computeSnapshot(reason): publishes minimal diagnostics (no vis2orig)
    # - captureEditCtx(node): captures exact .row_id and exact original column index and sends them to Shiny
    cb <- JS(sprintf("
      var tbl = table;
      var ridIdx0 = %d; // original index (0-based) of .row_id

      function computeSnapshot(reason) {
        try {
          var headers = [];
          // Collect visible headers, left-to-right, just for human-readable logging
          tbl.columns(':visible').every(function(idx){
            var label = (this.header() && this.header().textContent) ? this.header().textContent.trim() : ('' + idx);
            headers.push(label);
          });

          // Full column order (positions -> original indexes), includes hidden columns
          var orderAll = tbl.colReorder ? tbl.colReorder.order() : null;
          if (!orderAll) {
            orderAll = [];
            var nCols = tbl.columns().count();
            for (var i = 0; i < nCols; i++) orderAll.push(i);
          }

          var dataSrc = tbl.columns().dataSrc();
          if (dataSrc && dataSrc.toArray) dataSrc = dataSrc.toArray();

          Shiny.setInputValue('tbl_snapshot', {
            headers: headers,          // cosmetic, visible only
            orderAll: orderAll,        // mapping including hidden
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

          // Use the cell's index to get both row and column (in current space)
          var cell = tbl.cell($td);
          if (!cell || !cell.any()) return;
          var idx = cell.index(); // {row: r, column: c}

          // Compute original column index using ColReorder transpose
          var colDisp = idx.column;
          var colOrig = (tbl.colReorder && tbl.colReorder.transpose)
            ? tbl.colReorder.transpose(colDisp, 'toOriginal')
            : colDisp;

          // Row's .row_id from the data vector
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

      // Snapshot events (keep verbose by choice; no vis2orig)
      tbl.on('init.dt',               function(){ computeSnapshot('init'); });
      tbl.on('column-reorder.dt',     function(){ computeSnapshot('colreorder'); });
      tbl.on('column-visibility.dt',  function(){ computeSnapshot('colvisibility'); });
      tbl.on('order.dt',              function(){ computeSnapshot('order'); });
      tbl.on('search.dt',             function(){ computeSnapshot('search'); });
      tbl.on('draw.dt',               function(){ computeSnapshot('draw'); });

      // Capture the exact row_id and original column index at the moment of user interaction
      tbl.on('mousedown.dt', 'tbody td', function(e){ captureEditCtx(this); });
      tbl.on('focusin.dt',   'tbody td', function(e){ captureEditCtx(this); });
      // Also capture when inputs inside cells receive focus (during inline edit)
      tbl.on('focusin.dt',   'tbody input, tbody textarea', function(e){ captureEditCtx(this); });

      // Seed early snapshot
      setTimeout(function(){ computeSnapshot('tick0'); }, 0);
      setTimeout(function(){ computeSnapshot('tick50'); }, 50);
      computeSnapshot('manual');
    ", rid_idx0))

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      editable = list(target = "cell"),
      extensions = c("ColReorder", "Buttons"),
      options = list(
        dom = "Bfrtip",
        buttons = list(
          list(extend = "colvis", columns = ":not(.noVis)", text = "Columns")
        ),
        colReorder = TRUE,
        pageLength = 10,
        deferRender = TRUE,
        scrollX = TRUE,
        columnDefs = column_defs
      ),
      callback = cb
    )
  }, server = TRUE)

  proxy <- dataTableProxy("tbl")

  update_table <- function(new_data) {
    stopifnot("'.row_id' must exist in data" = ".row_id" %in% names(new_data))
    rv$data[[rv$active]] <- new_data
    replaceData(proxy, data = new_data, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
  }

  # -------------------- Resample / Refresh / Switch --------------------
  observeEvent(input$resample, {
    n <- input$n %||% 30
    base <- add_row_id(sample_base(n))
    rv$data$A <- base
    rv$data$B <- make_variant_from(base, seed_offset = 1)
    rv$active <- "A"
    rv$refresh_seed <- 1L
    replaceData(proxy, data = rv$data[[rv$active]], resetPaging = TRUE, rownames = FALSE, clearSelection = "none")
    cat(sprintf("[Action] Resample n=%d -> active=%s\n", n, rv$active))
  })

  observeEvent(input$refresh, {
    # Add small noise to numeric columns except .row_id
    s <- rv$refresh_seed + 1L
    rv$refresh_seed <- s
    df_now <- isolate(rv$data[[rv$active]])
    nums <- sapply(df_now, is.numeric); nums[".row_id"] <- FALSE
    set.seed(1000 + s)
    if (any(nums)) {
      df_now[nums] <- Map(function(col) col + rnorm(length(col), sd = 0.05), df_now[nums])
    }
    update_table(df_now)
    cat(sprintf("[Action] Refresh(seed=%d) -> active=%s\n", s, rv$active))
  })

  observeEvent(input$switch, {
    rv$active <- if (rv$active == "A") "B" else "A"
    replaceData(proxy, data = rv$data[[rv$active]], resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
    cat(sprintf("[Action] Switch -> active=%s\n", rv$active))
  })

  # -------------------- Snapshot logging with counter (no vis2orig) --------------------
  snapshot_counter <- reactiveVal(0)

  observeEvent(input$tbl_snapshot, ignoreInit = FALSE, {
    snap <- input$tbl_snapshot
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Increment and print snapshot counter
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

  # -------------------- Inline edits (REQUIRE client .row_id AND original column index; no fallbacks) --------------------
  observeEvent(input$tbl_cell_edit, ignoreInit = TRUE, {
    info_raw <- input$tbl_cell_edit
    if (is.null(info_raw)) return()

    # Normalize to last row if it's a data.frame (rare)
    info <- if (is.data.frame(info_raw)) as.list(info_raw[nrow(info_raw), , drop = FALSE]) else info_raw

    # Extract raw edit fields (for logging only)
    value_raw <- if (!is.null(info$value)) as.character(info$value) else if (!is.null(info$newValue)) as.character(info$newValue) else NA_character_

    # Strict requirements: client-sent .row_id and original column index
    row_id <- isolate(input$tbl_edit_row_id)
    col_orig0 <- isolate(input$tbl_edit_col_orig0)

    df <- isolate(rv$data[[rv$active]])
    cn <- colnames(df)
    scn <- isolate(snapshot_counter())

    cat("\n---- edit event (using snapshot #", scn, ") ----\n", sep = "")
    cat(sprintf("[edit raw] value=%s client_row_id=%s client_col_orig0=%s\n",
                as.character(value_raw), as.character(row_id), as.character(col_orig0)))

    # Hard requirement checks
    if (is.null(row_id) || is.na(suppressWarnings(as.integer(row_id)))) {
      cat("[edit] client .row_id missing; rejecting edit\n")
      showNotification("Couldn't map row; please retry.", type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }
    if (is.null(col_orig0) || is.na(suppressWarnings(as.integer(col_orig0)))) {
      cat("[edit] client original column index missing; rejecting edit\n")
      showNotification("Couldn't map column; please retry.", type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }
    row_id <- as.integer(row_id)
    col_orig0 <- as.integer(col_orig0)

    # Resolve to data locations
    row_idx <- match(row_id, df$.row_id)
    col_idx1 <- col_orig0 + 1L
    if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df)) {
      cat("[edit] .row_id not found in data; rejecting edit\n")
      showNotification("Row not found; please refresh.", type = "error", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }
    if (is.na(col_idx1) || col_idx1 < 1 || col_idx1 > ncol(df)) {
      cat("[edit] Column index out of range; rejecting edit\n")
      showNotification("Column not found; please retry.", type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }
    colname <- cn[col_idx1]
    if (identical(colname, ".row_id")) {
      cat("[edit] Attempt to edit .row_id blocked; rejecting\n")
      showNotification("This column is not editable.", type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }

    # Allowlist
    if (!(colname %in% editable_cols)) {
      cat(sprintf("[edit] Column '%s' is read-only; rejecting\n", colname))
      showNotification(sprintf("Column '%s' is read-only.", colname), type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }

    # Print full row BEFORE edit
    row_before <- df[row_idx, , drop = FALSE]
    cat("[row before] ", fmt_row(row_before), "\n", sep = "")

    # Validation + commit
    if (identical(colname, "Sepal.Width")) {
      new_num <- suppressWarnings(as.numeric(value_raw))
      if (!is.finite(new_num) || new_num < 2 || new_num > 4) {
        cat(sprintf("[edit] validation failed: %s not in [2,4]\n", as.character(value_raw)))
        showNotification("Sepal.Width must be numeric in [2, 4]. Edit discarded.", type = "error", duration = 3)
        replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
        return()
      }
      new_val <- round(new_num, 3)
    } else {
      old_col <- df[[colname]]
      if (is.numeric(old_col))       new_val <- suppressWarnings(as.numeric(value_raw))
      else if (is.logical(old_col))  new_val <- tolower(trimws(as.character(value_raw))) %in% c('true','t','1','yes','y')
      else                           new_val <- as.character(value_raw)
    }

    old_val <- df[[colname]][row_idx]
    df[[colname]][row_idx] <- new_val

    # Print full row AFTER edit
    row_after <- df[row_idx, , drop = FALSE]
    cat("[row after ] ", fmt_row(row_after), "\n", sep = "")

    # Save and update
    rv$data[[rv$active]] <- df
    replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")

    cat(sprintf("[Edit OK] .row_id=%d row=%d col=%s old=%s new=%s\n",
                as.integer(row_id), row_idx, colname, as.character(old_val), as.character(new_val)))
    showNotification("Saved", type = "message", duration = 1.2)
  })
}

shinyApp(ui, server)