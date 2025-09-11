# Shiny app: DT server-side with show/hide columns; export only VISIBLE columns to TSV
library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT server-side: export only visible columns"),
  fluidRow(
    column(4, actionButton("refresh", "Refresh data", icon = icon("rotate"))),
    column(4, actionButton("switch", "Switch dataset", icon = icon("exchange"))),
    column(4, downloadButton("download_tsv", "Export TSV (visible columns only)"))
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

  # JS callback: mirror table state to Shiny using DataTables API.
  # Also, force a fresh state push right before exporting.
  js_callback <- JS("
    var tbl = table;

    function currentHeaderNames() {
      return tbl.columns().header().toArray().map(function(th){ return th.textContent.trim(); });
    }

    function pushState() {
      // Visible columns by name (in current display order)
      var allNames = currentHeaderNames();
      var visibleIdx = tbl.columns(':visible').indexes().toArray();
      var visibleCols = visibleIdx.map(function(i){ return allNames[i]; });

      Shiny.setInputValue('tbl_state', {
        visibleCols: visibleCols,
        timestamp: Date.now()
      }, {priority: 'event'});
    }

    // Update Shiny on relevant events and once at init
    tbl.on('column-visibility.dt draw.dt', pushState);
    pushState();

    // Ensure the latest state is sent right before download starts
    $(document).on('click', '#download_tsv', function() {
      pushState();
    });
  ")

  # IMPORTANT: Render the table ONCE with a static dataset to preserve UI state
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

  # Hold the last known state from the DataTable (only visibility for this step)
  state <- reactiveVal(NULL)
  observeEvent(input$tbl_state, ignoreInit = FALSE, {
    state(input$tbl_state)
  })

  # Utility: coerce list-like input to a character vector
  as_char <- function(x, default = character(0)) {
    if (is.null(x)) return(default)
    as.character(unlist(x, use.names = FALSE))
  }

  # Build export data: only subset by VISIBLE columns; ignore order/filters for now
  prepare_export_data <- reactive({
    df <- current_data()
    cn <- colnames(df)
    st <- state()

    # Visible columns by name (as reported by JS)
    visibleCols <- if (!is.null(st) && !is.null(st$visibleCols)) as_char(st$visibleCols, cn) else cn
    visibleCols <- intersect(visibleCols, cn)

    if (length(visibleCols) == 0) {
      # If user hid everything, fall back to all columns
      visibleCols <- cn
    }

    df[, visibleCols, drop = FALSE]
  })

  # Download handler: export ALL rows for only the visible columns
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