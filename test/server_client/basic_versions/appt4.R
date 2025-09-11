# Shiny app: DT server-side with show/hide + reorder; export VISIBLE columns in DISPLAY ORDER
library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT server-side: export visible columns in current order"),
  fluidRow(
    column(4, actionButton("refresh", "Refresh data", icon = icon("rotate"))),
    column(4, actionButton("switch", "Switch dataset", icon = icon("exchange"))),
    column(4, downloadButton("download_tsv", "Export TSV (visible cols, current order)"))
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

  # JS callback: capture visible columns IN DISPLAY ORDER from the first header row
  js_callback <- JS("
    var tbl = table;

    function headerNamesDisplay() {
      // Use only the first header row to avoid the per-column filter row
      return $(tbl.table().header()).find('tr').first().find('th')
        .map(function(){ return this.textContent.trim(); }).get();
    }

    function pushState() {
      // Visible columns in display order (hidden columns' <th> are not present)
      var visibleCols = headerNamesDisplay();
      // orderCols equals visibleCols (display order) for export purposes
      var orderCols = visibleCols.slice();

      Shiny.setInputValue('tbl_state', {
        visibleCols: visibleCols,
        orderCols: orderCols,
        timestamp: Date.now()
      }, {priority: 'event'});
    }

    // Update Shiny when visibility/reorder changes and at init
    tbl.on('column-visibility.dt column-reorder.dt draw.dt', pushState);
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

  # Hold the last known state from the DataTable (order + visibility)
  state <- reactiveVal(NULL)
  observeEvent(input$tbl_state, ignoreInit = FALSE, {
    state(input$tbl_state)
  })

  # Utility: coerce list-like input to a character vector
  as_char <- function(x, default = character(0)) {
    if (is.null(x)) return(default)
    as.character(unlist(x, use.names = FALSE))
  }

  # Build export data: subset to VISIBLE columns, in CURRENT DISPLAY ORDER
  prepare_export_data <- reactive({
    df <- current_data()
    cn <- colnames(df)
    st <- state()

    orderCols <- if (!is.null(st) && !is.null(st$orderCols)) as_char(st$orderCols, cn) else cn
    visibleCols <- if (!is.null(st) && !is.null(st$visibleCols)) as_char(st$visibleCols, cn) else cn

    # Sanity: ensure names exist in df
    orderCols <- intersect(orderCols, cn)
    visibleCols <- intersect(visibleCols, cn)

    # Final columns = display order filtered to visible ones
    final_cols <- orderCols[orderCols %in% visibleCols]
    if (length(final_cols) == 0) {
      final_cols <- cn
    }

    df[, final_cols, drop = FALSE]
  })

  # Download handler: export ALL rows, only visible columns, in current order
  output$download_tsv <- downloadHandler(
    filename = function() "table_export.tsv",
    content = function(file) {
      df <- prepare_export_data()

      # Debug: print current column order and export dimensions
      st <- state()
      orderCols <- if (!is.null(st) && !is.null(st$orderCols)) as_char(st$orderCols, colnames(current_data())) else colnames(current_data())
      visibleCols <- if (!is.null(st) && !is.null(st$visibleCols)) as_char(st$visibleCols, colnames(current_data())) else colnames(current_data())
      final_cols <- colnames(df)

      cat(sprintf("[Export] visibleCols: %s\n", paste(visibleCols, collapse = ", ")))
      cat(sprintf("[Export] orderCols   : %s\n", paste(orderCols, collapse = ", ")))
      cat(sprintf("[Export] final_cols  : %s\n", paste(final_cols, collapse = ", ")))
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