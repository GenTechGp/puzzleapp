# Minimal Shiny app: DT server-side with persistent column filters across data changes
library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("DT server-side with persistent column filters"),
  fluidRow(
    column(6, actionButton("refresh", "Refresh data", icon = icon("rotate"))),
    column(6, actionButton("switch", "Switch dataset", icon = icon("exchange")))
  ),
  DTOutput("tbl")
)

server <- function(input, output, session) {
  # Base dataset (schema stays the same)
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

  # Initial render: server-side processing + built-in header filters
  output$tbl <- renderDT({
    datatable(
      data_A,
      filter = "top",
      rownames = FALSE,
      options = list(
        processing = TRUE,
        pageLength = 10,
        searchDelay = 400,
        deferRender = TRUE
        # Note: stateSave left FALSE by default to avoid cross-reload surprises.
        # If you ever want filters to persist across browser reloads:
        # stateSave = TRUE
      )
    )
  }, server = TRUE)

  proxy <- dataTableProxy("tbl")

  # Helper: update data without rebuilding the widget (preserves filters)
  update_table <- function(new_data) {
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
}

shinyApp(ui, server)