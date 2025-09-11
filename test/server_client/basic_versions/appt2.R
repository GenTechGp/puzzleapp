# Minimal Shiny app: DT server-side with persistent filters, col vis/order, and TSV export
library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT server-side with filters, show/hide, reorder, and TSV export"),
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

  # JS callback: mirror table state to Shiny (order, visibility, per-col filters by name, global search)
  js_callback <- JS("
    var tbl = table;

    function getColNames() {
      return tbl.columns().header().toArray().map(function(th){ return th.textContent.trim(); });
    }

    function getColSearchMap() {
      var map = {};
      tbl.columns().every(function(i){
        var name = $(this.header()).text().trim();
        var $inp = $('input, select', this.header());
        var v = $inp.length ? $inp.val() : '';
        if (v == null) v = '';
        map[name] = v;
      });
      return map;
    }

    function pushState() {
      var colNames = getColNames();
      var orderIdx = (tbl.colReorder && tbl.colReorder.order) ? tbl.colReorder.order() : colNames.map(function(_,i){return i;});
      var orderCols = orderIdx.map(function(i){ return colNames[i]; });
      var visibleCols = tbl.columns().indexes().toArray()
        .filter(function(i){ return tbl.column(i).visible(); })
        .map(function(i){ return colNames[i]; });
      var colSearchByName = getColSearchMap();
      var globalSearch = tbl.search();

      Shiny.setInputValue('tbl_state', {
        orderCols: orderCols,
        visibleCols: visibleCols,
        colSearchByName: colSearchByName,
        globalSearch: globalSearch,
        timestamp: Date.now()
      }, {priority: 'event'});
    }

    // Update Shiny on relevant events and once at init
    tbl.on('search.dt column-visibility.dt column-reorder.dt draw.dt', pushState);
    pushState();
  ")

  # IMPORTANT: Render the table ONCE with a static dataset.
  # Do not reference current_data() here, to avoid re-rendering on refresh/switch.
  output$tbl <- renderDT({
    datatable(
      data_A,                    # static initial data to establish schema
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
        # stateSave = TRUE  # enable if you want cross-browser-reload persistence
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

  # Hold the last known state from the DataTable (order, visibility, filters)
  state <- reactiveVal(NULL)
  observeEvent(input$tbl_state, ignoreInit = FALSE, {
    state(input$tbl_state)
  })

  # Utility: coerce list-like input to a character vector
  as_char <- function(x, default = character(0)) {
    if (is.null(x)) return(default)
    as.character(unlist(x, use.names = FALSE))
  }

  # Build export data: apply filters (case-insensitive contains), then respect order and visibility
  prepare_export_data <- reactive({
    df <- current_data()
    cn <- colnames(df)

    st <- state()

    orderCols <- if (!is.null(st) && !is.null(st$orderCols)) as_char(st$orderCols, cn) else cn
    visibleCols <- if (!is.null(st) && !is.null(st$visibleCols)) as_char(st$visibleCols, cn) else cn
    globalSearch <- if (!is.null(st) && !is.null(st$globalSearch)) as.character(st$globalSearch) else ""

    # Per-column filters by column name
    colSearchMap <- if (!is.null(st) && !is.null(st$colSearchByName)) st$colSearchByName else list()
    # Convert to a named character vector (names = column names)
    if (length(colSearchMap)) {
      colSearchVec <- vapply(colSearchMap, function(v) if (is.null(v)) "" else as.character(v)[1], character(1))
    } else {
      colSearchVec <- setNames(rep("", length(cn)), cn)
    }
    # Ensure every column name has an entry
    if (length(setdiff(cn, names(colSearchVec))) > 0) {
      missing <- setdiff(cn, names(colSearchVec))
      colSearchVec <- c(colSearchVec, setNames(rep("", length(missing)), missing))
    }

    # Apply per-column filters (case-insensitive contains on stringified values)
    keep <- rep(TRUE, nrow(df))
    for (name in cn) {
      val <- colSearchVec[[name]]
      if (!is.null(val) && nzchar(val)) {
        keep <- keep & grepl(val, as.character(df[[name]]), ignore.case = TRUE)
      }
    }
    df <- df[keep, , drop = FALSE]

    # Apply global search across all columns (case-insensitive contains)
    if (nzchar(globalSearch)) {
      any_match <- apply(df, 1, function(row) {
        any(grepl(globalSearch, as.character(row), ignore.case = TRUE))
      })
      df <- df[any_match, , drop = FALSE]
    }

    # Respect column order and visibility (by name)
    orderCols <- intersect(orderCols, cn)
    visibleCols <- intersect(visibleCols, cn)
    final_cols <- orderCols[orderCols %in% visibleCols]
    if (length(final_cols) == 0) final_cols <- cn # fallback if all hidden

    df[, final_cols, drop = FALSE]
  })

  # Download handler: export ALL filtered rows (not just current page) to TSV
  output$download_tsv <- downloadHandler(
    filename = function() "table_export.tsv",
    content = function(file) {
      df <- prepare_export_data()
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