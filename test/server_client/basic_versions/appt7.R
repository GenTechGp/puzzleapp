library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT editing with exact .row_id from client (no guesswork)"),
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
  current_data <- reactive(rv$data[[rv$active]])

  # Editable columns
  editable_cols <- c("Sepal.Width")

  # -------------------- Render DT once; update via replaceData (NO reactive deps) --------------------
  rendering_counter <- reactiveVal(0)
  output$render_count <- renderText({
    paste("rendering_counter =", rendering_counter())
  })

  output$tbl <- renderDT({
    # Render once; never depend on rv$data so edits don't re-render the widget
    n <- isolate(rendering_counter()) + 1
    rendering_counter(n)
    cat(sprintf("[Render] rendering_counter = %d\n", n))

    df <- isolate(rv$data[[rv$active]])

    cn <- colnames(df)
    rid_idx0 <- which(cn == ".row_id") - 1L
    column_defs <- list()
    if (length(rid_idx0)) {
      column_defs <- list(
        list(targets = rid_idx0, visible = FALSE, searchable = FALSE, orderable = FALSE)
      )
    }

    # JS:
    # - computeSnapshot(reason): publishes vis2orig (no rowIdsVis), headers, orderAll, dataSrc
    # - captureEditRowId(node): reads the exact .row_id from the clicked/focused row and sends tbl_edit_row_id
    cb <- JS(sprintf("
      var tbl = table;
      var ridIdx0 = %d; // original index (0-based) of .row_id

      function computeSnapshot(reason) {
        try {
          var headers = [];
          var vis2orig = [];

          // Full column order (positions -> original indexes), includes hidden columns
          var orderAll = tbl.colReorder ? tbl.colReorder.order() : null;
          if (!orderAll) {
            orderAll = [];
            var nCols = tbl.columns().count();
            for (var i = 0; i < nCols; i++) orderAll.push(i);
          }

          // Walk LEFT->RIGHT current order, append only VISIBLE columns to the snapshot
          for (var pos = 0; pos < orderAll.length; pos++) {
            var origIdx = orderAll[pos];
            var colApi  = tbl.column(origIdx);
            if (!colApi.visible()) continue;
            vis2orig.push(origIdx);

            var th = colApi.header();
            var label = th && th.textContent ? th.textContent.trim() : ('' + origIdx);
            headers.push(label);
          }

          var dataSrc = tbl.columns().dataSrc();
          if (dataSrc && dataSrc.toArray) dataSrc = dataSrc.toArray();

          Shiny.setInputValue('tbl_snapshot', {
            vis2orig: vis2orig,        // display (visible only) -> original (0-based)
            headers: headers,          // cosmetic
            orderAll: orderAll,        // full mapping including hidden
            dataSrc: dataSrc,
            reason: reason || 'unspecified',
            version: 'rowid-mapping-v2',
            ts: Date.now()
          }, {priority: 'event'});
        } catch (e) {
          console && console.error && console.error('computeSnapshot error', e);
        }
      }

      function captureEditRowId(node) {
        try {
          var $td = $(node).closest('td');
          var row = tbl.row($td.closest('tr'));
          if (!row || !row.any()) return;
          var rowData = row.data();
          if (!rowData || ridIdx0 == null || isNaN(ridIdx0)) return;
          var rid = rowData[ridIdx0];
          if (rid != null) {
            Shiny.setInputValue('tbl_edit_row_id', rid, {priority: 'event'});
          }
        } catch (e) {
          console && console.error && console.error('captureEditRowId error', e);
        }
      }

      // Snapshot events (verbose by choice)
      tbl.on('init.dt',               function(){ computeSnapshot('init'); });
      tbl.on('column-reorder.dt',     function(){ computeSnapshot('colreorder'); });
      tbl.on('column-visibility.dt',  function(){ computeSnapshot('colvisibility'); });
      tbl.on('order.dt',              function(){ computeSnapshot('order'); });
      tbl.on('search.dt',             function(){ computeSnapshot('search'); });
      tbl.on('draw.dt',               function(){ computeSnapshot('draw'); });

      // Capture the exact .row_id at the moment of user interaction
      tbl.on('mousedown.dt', 'tbody td', function(e){ captureEditRowId(this); });
      tbl.on('focusin.dt',   'tbody td', function(e){ captureEditRowId(this); });
      // Also capture when inputs inside cells receive focus (during inline edit)
      tbl.on('focusin.dt',   'tbody input, tbody textarea', function(e){ captureEditRowId(this); });

      // A couple of extra ticks to seed early snapshot
      setTimeout(function(){ computeSnapshot('tick0'); }, 0);
      setTimeout(function(){ computeSnapshot('tick50'); }, 50);
      computeSnapshot('manual');
    ", rid_idx0))

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      editable = list(target = "cell"),
      extensions = c("ColReorder"),
      options = list(
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
    df_now <- isolate(current_data())
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

  # -------------------- Snapshot logging with counter --------------------
  snapshot_counter <- reactiveVal(0)

  observeEvent(input$tbl_snapshot, ignoreInit = FALSE, {
    snap <- input$tbl_snapshot
    df <- isolate(current_data())
    cn <- colnames(df)
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Increment and print snapshot counter
    n <- isolate(snapshot_counter()) + 1L
    snapshot_counter(n)

    # Reconstruct display headers from mapping (authoritative)
    hdr_from_map <- character()
    if (!is.null(snap$vis2orig)) {
      idx1 <- as.integer(unlist(snap$vis2orig)) + 1L
      ok <- !is.na(idx1) & idx1 >= 1 & idx1 <= length(cn)
      hdr_from_map <- ifelse(ok, cn[idx1], NA_character_)
    }

    cat("\n===== tbl_snapshot #", n, " @ ", ts, " =====\n", sep = "")
    cat("[reason] ", snap$reason %||% "unspecified",
        " | [version] ", snap$version %||% "unknown",
        " | [ts] ", as.character(snap$ts), "\n", sep = "")
    cat("[headers (browser, cosmetic)] ", paste((snap$headers %||% character()), collapse = ", "), "\n", sep = "")
    cat("[headers (server from vis2orig)] ", paste(hdr_from_map, collapse = ", "), "\n", sep = "")

    if (!is.null(snap$vis2orig)) {
      cat("[vis2orig display->original (0-based)]\n", sep = "")
      for (i in seq_along(snap$vis2orig)) {
        o0 <- as.integer(snap$vis2orig[[i]])
        o1 <- o0 + 1L
        nm <- if (!is.na(o1) && o1 >= 1 && o1 <= length(cn)) cn[o1] else NA_character_
        cat(sprintf("  display %d -> original %d (%s)\n", i - 1L, o0, nm))
      }
    } else {
      cat("[vis2orig] NULL\n")
    }

    if (!is.null(snap$orderAll)) {
      cat("[orderAll (ColReorder, 0-based, all cols)] ", paste(as.integer(snap$orderAll), collapse = ","), "\n", sep = "")
    }
    if (!is.null(snap$dataSrc)) {
      cat("[dataSrc] ", paste(as.integer(snap$dataSrc), collapse = ","), "\n", sep = "")
    }
    cat("===== end tbl_snapshot =====\n")
  })

  # -------------------- Inline edits (REQUIRE client .row_id; no fallbacks) --------------------
  observeEvent(input$tbl_cell_edit, ignoreInit = TRUE, {
    info_raw <- input$tbl_cell_edit
    if (is.null(info_raw)) return()

    # Normalize to last row if it's a data.frame
    info <- if (is.data.frame(info_raw)) as.list(info_raw[nrow(info_raw), , drop = FALSE]) else info_raw

    # Extract raw edit fields
    col_disp0 <- suppressWarnings(as.integer(info$col))           # display column index (0-based in most DT setups)
    value_raw <- if (!is.null(info$value)) as.character(info$value) else if (!is.null(info$newValue)) as.character(info$newValue) else NA_character_

    # Strict requirements: must have both col mapping and client-sent .row_id
    snap <- isolate(input$tbl_snapshot)
    row_id <- isolate(input$tbl_edit_row_id)

    df <- isolate(current_data())
    cn <- colnames(df)
    scn <- isolate(snapshot_counter())

    cat("\n---- edit event (using snapshot #", scn, ") ----\n", sep = "")
    cat(sprintf("[edit raw] col(display,0b)=%s value=%s client_row_id=%s\n",
                as.character(col_disp0), as.character(value_raw), as.character(row_id)))

    # Resolve column via vis2orig -> stable colname
    colname <- NA_character_
    if (!is.null(snap) && !is.null(snap$vis2orig) && !is.na(col_disp0)) {
      vis2orig <- as.integer(unlist(snap$vis2orig))
      if ((col_disp0 + 1L) >= 1 && (col_disp0 + 1L) <= length(vis2orig)) {
        orig_idx0 <- vis2orig[col_disp0 + 1L]
        orig_idx1 <- orig_idx0 + 1L
        if (!is.na(orig_idx1) && orig_idx1 >= 1 && orig_idx1 <= length(cn)) {
          colname <- cn[orig_idx1]
          cat(sprintf("[translate col] via vis2orig: display %d -> original %d -> colname=%s\n",
                      col_disp0, orig_idx0, colname))
        }
      }
    }

    # Hard requirement checks
    if (is.na(colname) || !nzchar(colname)) {
      cat("[edit] could not resolve column; rejecting edit\n")
      showNotification("Couldn't map column; please retry.", type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }
    if (is.null(row_id) || is.na(suppressWarnings(as.integer(row_id)))) {
      cat("[edit] client .row_id missing; rejecting edit\n")
      showNotification("Couldn't map row; please retry.", type = "warning", duration = 2)
      replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
      return()
    }
    row_id <- as.integer(row_id)

    # Find the data row index by key
    row_idx <- match(row_id, df$.row_id)
    if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df)) {
      cat("[edit] .row_id not found in data; rejecting edit\n")
      showNotification("Row not found; please refresh.", type = "error", duration = 2)
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