# Shiny app: DT server-side with show/hide + reorder; export obeys visible cols, order, and filters
# Filtering semantics (export only): case-insensitive literal "contains" (no regex, no smart)
library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT server-side: export visible columns, order, and filters"),
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

      // Per-column search terms (by column header text)
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
        # Note: The UI may still use DataTables 'smart' search internally,
        # but export uses the semantics defined in this server code.
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

  # Build export data:
  # 1) Determine visible columns and their order
  # 2) Apply per-column filters (visible columns only) with case-insensitive literal contains
  # 3) Apply global search across visible columns only (same semantics)
  # 4) Export visible columns in display order
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

    # Per-column search terms by name (we'll only apply those for visible columns)
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

    # 2) Apply per-column filters (VISIBLE columns only), case-insensitive literal contains
    if (length(visibleCols) > 0) {
      keep <- rep(TRUE, nrow(df))
      for (name in visibleCols) {
        term <- colSearchVec[[name]]
        if (!is.null(term) && nzchar(term)) {
          vals <- as.character(df[[name]])
          vals[is.na(vals)] <- ""
          keep <- keep & grepl(term, vals, ignore.case = TRUE, fixed = TRUE)
        }
      }
      df <- df[keep, , drop = FALSE]
    }

    # 3) Apply global search across VISIBLE columns only
    globalSearch <- if (!is.null(st) && !is.null(st$globalSearch)) as.character(st$globalSearch) else ""
    globalSearch <- trimws(globalSearch)
    if (nzchar(globalSearch) && length(visibleCols) > 0 && nrow(df) > 0) {
      vis_mat <- lapply(visibleCols, function(name) {
        v <- as.character(df[[name]])
        v[is.na(v)] <- ""
        v
      })
      # any column in the row contains the term
      any_match <- Reduce(`|`, lapply(vis_mat, function(v) grepl(globalSearch, v, ignore.case = TRUE, fixed = TRUE)))
      df <- df[any_match, , drop = FALSE]
    }

    # 4) Final columns = VISIBLE columns in DISPLAY ORDER
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

      # Prepare a compact per-column filter printout for visible columns only
      colSearchVec <- if (length(colSearchMap)) {
        vapply(colSearchMap, function(v) if (is.null(v)) "" else as.character(v)[1], character(1))
      } else character(0)
      visFilters <- paste(sprintf("%s='%s'",
                                 intersect(visibleCols, names(colSearchVec)),
                                 trimws(colSearchVec[intersect(visibleCols, names(colSearchVec))])),
                          collapse = ", ")

      final_cols <- colnames(df)

      cat(sprintf("[Export] visibleCols : %s\n", paste(visibleCols, collapse = ", ")))
      cat(sprintf("[Export] orderCols   : %s\n", paste(orderCols, collapse = ", ")))
      cat(sprintf("[Export] final_cols  : %s\n", paste(final_cols, collapse = ", ")))
      cat(sprintf("[Export] globalSearch: '%s'\n", trimws(globalSearch)))
      cat(sprintf("[Export] colFilters  : %s\n", visFilters))
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