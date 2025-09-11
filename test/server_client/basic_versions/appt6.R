# Shiny app: DT server-side with show/hide + reorder; export obeys visible cols, order, and typed filters
# Filtering semantics (export only):
# - Per-column: typed parsing
#   * Numeric: <=, >=, <, >, =, ==, != ; ranges (.., ..., :, -, [a,b], (a,b), etc.); sets (comma or JSON array); single number equality with tolerance
#   * Text/factor: sets (comma or JSON array) => exact membership (case-insensitive); else literal contains (case-insensitive)
# - Global: case-insensitive literal "contains" across visible columns only
# - Invalid filter text is ignored; NA defaults (NAs don't match non-empty terms)
library(shiny)
library(DT)
library(jsonlite)

ui <- fluidPage(
  titlePanel("DT server-side: export visible columns, order, and typed filters"),
  fluidRow(
    column(4, actionButton("refresh", "Refresh data", icon = icon("rotate"))),
    column(4, actionButton("switch", "Switch dataset", icon = icon("exchange"))),
    column(4, downloadButton("download_tsv", "Export TSV"))
  ),
  DTOutput("tbl")
)

server <- function(input, output, session) {
  # Base dataset (schema stays identical)
  base_data <- iris

  # Create a variant with the same schema (adds small noise to numeric columns)
  make_variant <- function(seed_offset = 0) {
    set.seed(123 + seed_offset)
    d <- base_data
    nums <- sapply(d, is.numeric)
    d[nums] <- lapply(d[nums], function(col) col + rnorm(length(col), sd = 0.2))
    d
  }

  data_A <- base_data
  data_B <- make_variant(1)

  # Track the dataset currently shown (for export only; do NOT use in renderDT)
  current_data <- reactiveVal(data_A)

  # JS callback: capture visible columns (display order), per-column and global search
  js_callback <- JS("
    var tbl = table;

    function headerNamesDisplay() {
      // Only the first header row to avoid the per-column filter row
      return $(tbl.table().header()).find('tr').first().find('th')
        .map(function(){ return this.textContent.trim(); }).get();
    }

    function pushState() {
      // Visible columns in display order (hidden columns' <th> are not present)
      var visibleCols = headerNamesDisplay();
      // orderCols equals visibleCols (display order)
      var orderCols = visibleCols.slice();

      // Per-column search terms (by column header text) from DataTables API
      var colSearchByName = {};
      tbl.columns().every(function(idx){
        var name = $(this.header()).text().trim();
        var term = this.search() || '';
        colSearchByName[name] = term;
      });

      // Global search
      var globalSearch = tbl.search();

      Shiny.setInputValue('tbl_state', {
        visibleCols: visibleCols,
        orderCols: orderCols,
        colSearchByName: colSearchByName,
        globalSearch: globalSearch,
        timestamp: Date.now()
      }, {priority: 'event'});
    }

    // Update Shiny on relevant events and once at init
    tbl.on('search.dt column-visibility.dt column-reorder.dt draw.dt', pushState);
    pushState();

    // Ensure latest state right before download starts
    $(document).on('click', '#download_tsv', function() {
      pushState();
    });
  ")

  # Render the table ONCE with a static dataset to preserve UI state
  output$tbl <- renderDT({
    datatable(
      data_A,  # static initial data to establish schema
      filter = "top",
      rownames = FALSE,
      extensions = c("Buttons", "ColReorder"),
      options = list(
        dom = "Bfrtip",
        buttons = list("colvis"),
        colReorder = TRUE,
        processing = TRUE,
        pageLength = 10,
        searchDelay = 400,
        deferRender = TRUE,
        scrollX = TRUE
      ),
      callback = js_callback
    )
  }, server = TRUE)

  proxy <- dataTableProxy("tbl")

  # Update table data without rebuilding the widget (preserves filters/visibility/order)
  update_table <- function(new_data) {
    current_data(new_data)  # keep for export
    replaceData(
      proxy,
      data = new_data,
      resetPaging = FALSE,
      rownames = FALSE,
      clearSelection = "none"
    )
  }

  # Refresh data: simulate new data with the same schema
  refresh_seed <- reactiveVal(1)
  observeEvent(input$refresh, {
    s <- refresh_seed() + 1
    refresh_seed(s)
    update_table(make_variant(s))
  })

  # Switch between two datasets with identical schema
  current_is_A <- reactiveVal(TRUE)
  observeEvent(input$switch, {
    if (isTRUE(current_is_A())) {
      update_table(data_B)
      current_is_A(FALSE)
    } else {
      update_table(data_A)
      current_is_A(TRUE)
    }
  })

  # Hold the last known state from the DataTable (order + visibility + filters)
  state <- reactiveVal(NULL)
  observeEvent(input$tbl_state, ignoreInit = FALSE, {
    state(input$tbl_state)
  })

  # Utility: coerce list-like input to a character vector
  as_char <- function(x, default = character(0)) {
    if (is.null(x)) return(default)
    as.character(unlist(x, use.names = FALSE))
  }

  # ---------- Helper parsing and matching utilities ----------

  # Safe tolower for vectors, preserving NA
  tolower_safe <- function(x) {
    y <- x
    isna <- is.na(y)
    y <- tolower(as.character(y))
    y[isna] <- NA_character_
    y
  }

  # Detect JSON array looking string
  looks_like_json_array <- function(s) {
    grepl("^\\s*\\[", s) && grepl("\\]\\s*$", s)
  }

  # Parse a text set from term: JSON array or comma-separated list; returns character vector (lowercased) or NULL
  parse_text_set <- function(term) {
    t <- trimws(term)
    if (t == "") return(NULL)
    items <- NULL
    if (looks_like_json_array(t)) {
      items <- tryCatch(fromJSON(t), error = function(e) NULL)
    }
    if (is.null(items) && grepl(",", t, fixed = TRUE)) {
      # Split comma list
      parts <- strsplit(t, ",", fixed = TRUE)[[1]]
      parts <- trimws(parts)
      # Unquote each item if quoted
      parts <- gsub("^[\"']|[\"']$", "", parts)
      parts <- parts[nzchar(parts)]
      items <- parts
    }
    if (length(items)) {
      return(tolower(items))
    }
    NULL
  }

  # Build text predicate and description
  make_text_predicate <- function(term) {
    t <- trimws(term)
    if (t == "") return(list(pred = function(x) rep(TRUE, length(x)), desc = "none"))
    # Set membership?
    set_vals <- parse_text_set(t)
    if (!is.null(set_vals)) {
      pred <- function(x) {
        xv <- tolower_safe(x)
        !is.na(xv) & (xv %in% set_vals)
      }
      desc <- sprintf("in {%s}", paste(set_vals, collapse = ", "))
      return(list(pred = pred, desc = desc))
    }
    # Fallback: case-insensitive literal contains (avoid regex; avoid ignore.case warning)
    needle <- tolower(t)
    pred <- function(x) {
      xv <- tolower_safe(x)
      !is.na(xv) & grepl(needle, xv, fixed = TRUE)
    }
    desc <- sprintf("contains '%s' (ci)", t)
    list(pred = pred, desc = desc)
  }

  # Numeric helpers
  NUM <- "[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?"

  parse_numeric <- function(s) {
    as.numeric(s)
  }

  make_numeric_predicate <- function(term) {
    t <- trimws(term)
    if (t == "") return(list(pred = function(x) rep(TRUE, length(x)), desc = "none"))

    # 1) JSON array -> set membership
    if (looks_like_json_array(t)) {
      nums <- tryCatch(fromJSON(t), error = function(e) NULL)
      if (is.numeric(nums) && length(nums)) {
        vals <- as.numeric(nums)
        pred <- function(x) {
          xv <- as.numeric(x)
          !is.na(xv) & (xv %in% vals)
        }
        desc <- sprintf("in {%s}", paste(signif(vals, 8), collapse = ", "))
        return(list(pred = pred, desc = desc))
      }
    }

    # 2) Bracketed/parenthesized range: ([ min , max ])
    m <- regexec(sprintf("^\\s*([\\[\\(])\\s*(%s)\\s*,\\s*(%s)\\s*([\\]\\)])\\s*$", NUM, NUM), t, perl = TRUE)
    reg <- regmatches(t, m)[[1]]
    if (length(reg)) {
      left <- reg[2]; a <- parse_numeric(reg[3]); b <- parse_numeric(reg[4]); right <- reg[5]
      if (is.finite(a) && is.finite(b)) {
        if (a > b) { tmp <- a; a <- b; b <- tmp }
        incL <- (left == "["); incR <- (right == "]")
        pred <- function(x) {
          xv <- as.numeric(x)
          ok <- rep(FALSE, length(xv))
          ok <- if (incL) xv >= a else xv > a
          ok <- ok & if (incR) xv <= b else xv < b
          ok & !is.na(xv)
        }
        desc <- sprintf("range %s%s, %s%s",
                        if (incL) "[" else "(",
                        signif(a, 8),
                        signif(b, 8),
                        if (incR) "]" else ")")
        return(list(pred = pred, desc = desc))
      }
    }

    # 3) Delimited ranges: a..b, a...b, a:b, a-b (inclusive by default)
    m <- regexec(sprintf("^\\s*(%s)\\s*(?:\\.\\.\\.?|:|-)\\s*(%s)\\s*$", NUM, NUM), t, perl = TRUE)
    reg <- regmatches(t, m)[[1]]
    if (length(reg)) {
      a <- parse_numeric(reg[2]); b <- parse_numeric(reg[3])
      if (is.finite(a) && is.finite(b)) {
        if (a > b) { tmp <- a; a <- b; b <- tmp }
        pred <- function(x) {
          xv <- as.numeric(x)
          !is.na(xv) & xv >= a & xv <= b
        }
        desc <- sprintf("range [%s, %s]", signif(a, 8), signif(b, 8))
        return(list(pred = pred, desc = desc))
      }
    }

    # 4) Comparison operators
    m <- regexec(sprintf("^\\s*(<=|>=|<|>|==|=|!=)\\s*(%s)\\s*$", NUM), t, perl = TRUE)
    reg <- regmatches(t, m)[[1]]
    if (length(reg)) {
      op <- reg[2]; val <- parse_numeric(reg[3])
      if (is.finite(val)) {
        pred <- switch(op,
          "<"  = function(x) { xv <- as.numeric(x); !is.na(xv) & (xv <  val) },
          ">"  = function(x) { xv <- as.numeric(x); !is.na(xv) & (xv >  val) },
          "<=" = function(x) { xv <- as.numeric(x); !is.na(xv) & (xv <= val) },
          ">=" = function(x) { xv <- as.numeric(x); !is.na(xv) & (xv >= val) },
          "!=" = function(x) { xv <- as.numeric(x); !is.na(xv) & (xv != val) },
          "=",
          "==" = {
            tol <- 1e-8 * max(1, abs(val))
            function(x) {
              xv <- as.numeric(x)
              !is.na(xv) & (abs(xv - val) <= tol)
            }
          }
        )
        desc <- if (op %in% c("=", "==")) {
          sprintf("== %s (±tol)", signif(val, 8))
        } else {
          sprintf("%s %s", op, signif(val, 8))
        }
        return(list(pred = pred, desc = desc))
      }
    }

    # 5) Comma-separated set
    if (grepl(",", t, fixed = TRUE)) {
      parts <- strsplit(t, ",", fixed = TRUE)[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      nums <- suppressWarnings(as.numeric(parts))
      nums <- nums[is.finite(nums)]
      if (length(nums)) {
        pred <- function(x) {
          xv <- as.numeric(x)
          !is.na(xv) & (xv %in% nums)
        }
        desc <- sprintf("in {%s}", paste(signif(nums, 8), collapse = ", "))
        return(list(pred = pred, desc = desc))
      }
    }

    # 6) Single number => equality with tolerance
    val <- suppressWarnings(as.numeric(t))
    if (is.finite(val)) {
      tol <- 1e-8 * max(1, abs(val))
      pred <- function(x) {
        xv <- as.numeric(x)
        !is.na(xv) & (abs(xv - val) <= tol)
      }
      desc <- sprintf("== %s (±tol)", signif(val, 8))
      return(list(pred = pred, desc = desc))
    }

    # Invalid: ignore
    list(pred = function(x) rep(TRUE, length(x)), desc = "ignored (invalid)")
  }

  # ---------- Build export data with typed filters ----------

  prepare_export_data <- reactive({
    df <- current_data()
    cn <- colnames(df)
    st <- state()

    # Visible columns (display order) from JS; if missing, fall back to all columns
    orderCols <- if (!is.null(st) && !is.null(st$orderCols)) as_char(st$orderCols, cn) else cn
    visibleCols <- if (!is.null(st) && !is.null(st$visibleCols)) as_char(st$visibleCols, cn) else cn

    # Sanity: ensure names exist in df
    orderCols <- intersect(orderCols, cn)
    visibleCols <- intersect(visibleCols, cn)

    # Per-column search terms by name
    colSearchMap <- if (!is.null(st) && !is.null(st$colSearchByName)) st$colSearchByName else list()
    colSearchVec <- if (length(colSearchMap)) {
      vapply(colSearchMap, function(v) if (is.null(v)) "" else as.character(v)[1], character(1))
    } else {
      setNames(rep("", length(cn)), cn)
    }
    # Ensure every visible column has an entry
    if (length(setdiff(visibleCols, names(colSearchVec))) > 0) {
      missing <- setdiff(visibleCols, names(colSearchVec))
      colSearchVec <- c(colSearchVec, setNames(rep("", length(missing)), missing))
    }
    # Trim whitespace from search strings
    colSearchVec <- setNames(trimws(unname(colSearchVec)), names(colSearchVec))

    # 1) Apply per-column filters (VISIBLE columns only) with typed semantics
    if (length(visibleCols) > 0 && nrow(df) > 0) {
      keep <- rep(TRUE, nrow(df))
      for (name in visibleCols) {
        term <- colSearchVec[[name]]
        if (is.null(term) || !nzchar(term)) next

        col <- df[[name]]
        if (is.numeric(col)) {
          nf <- make_numeric_predicate(term)
          cond <- nf$pred(col)
        } else {
          tf <- make_text_predicate(term)
          cond <- tf$pred(col)
        }

        # NAs default: don't match non-empty terms
        cond[is.na(cond)] <- FALSE
        keep <- keep & cond
      }
      df <- df[keep, , drop = FALSE]
    }

    # 2) Apply global search across VISIBLE columns only (case-insensitive literal contains)
    globalSearch <- if (!is.null(st) && !is.null(st$globalSearch)) as.character(st$globalSearch) else ""
    globalSearch <- trimws(globalSearch)
    if (nzchar(globalSearch) && length(visibleCols) > 0 && nrow(df) > 0) {
      needle <- tolower(globalSearch)
      # Build lowercase text for visible cols, replace NA with ""
      any_match <- rep(FALSE, nrow(df))
      for (name in visibleCols) {
        v <- df[[name]]
        v_chr <- tolower_safe(v)
        v_chr[is.na(v_chr)] <- ""
        any_match <- any_match | grepl(needle, v_chr, fixed = TRUE)
      }
      df <- df[any_match, , drop = FALSE]
    }

    # 3) Final columns = VISIBLE columns in DISPLAY ORDER
    final_cols <- orderCols[orderCols %in% visibleCols]
    if (length(final_cols) == 0) final_cols <- cn

    df[, final_cols, drop = FALSE]
  })

  # Download handler: export ALL rows matching filters on visible columns, in display order
  output$download_tsv <- downloadHandler(
    filename = function() "table_export.tsv",
    content = function(file) {
      df <- prepare_export_data()

      # Debug: print current state and exported dimensions
      st <- state()
      cn <- colnames(current_data())
      orderCols <- if (!is.null(st) && !is.null(st$orderCols)) as_char(st$orderCols, cn) else cn
      visibleCols <- if (!is.null(st) && !is.null(st$visibleCols)) as_char(st$visibleCols, cn) else cn
      colSearchMap <- if (!is.null(st) && !is.null(st$colSearchByName)) st$colSearchByName else list()
      globalSearch <- if (!is.null(st) && !is.null(st$globalSearch)) as.character(st$globalSearch) else ""

      # Prepare parsed interpretations per visible column
      interpretations <- c()
      for (name in visibleCols) {
        term <- ""
        if (length(colSearchMap) && name %in% names(colSearchMap)) {
          term <- as.character(colSearchMap[[name]])[1]
          term <- trimws(term)
        }
        if (!nzchar(term)) next
        col <- current_data()[[name]]
        if (is.numeric(col)) {
          nf <- make_numeric_predicate(term)
          interpretations <- c(interpretations, sprintf("%s: %s", name, nf$desc))
        } else {
          tf <- make_text_predicate(term)
          interpretations <- c(interpretations, sprintf("%s: %s", name, tf$desc))
        }
      }

      final_cols <- colnames(df)

      cat(sprintf("[Export] visibleCols : %s\n", paste(visibleCols, collapse = ", ")))
      cat(sprintf("[Export] orderCols   : %s\n", paste(orderCols, collapse = ", ")))
      cat(sprintf("[Export] final_cols  : %s\n", paste(final_cols, collapse = ", ")))
      cat(sprintf("[Export] globalSearch: '%s'\n", trimws(globalSearch)))
      if (length(interpretations)) {
        cat(sprintf("[Export] parsed per-col: %s\n", paste(interpretations, collapse = " | ")))
      } else {
        cat("[Export] parsed per-col: none\n")
      }
      cat(sprintf("[Export] dims        : %d rows x %d cols\n", nrow(df), ncol(df)))

      write.table(
        df,
        file = file,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )
    }
  )
}

shinyApp(ui, server)