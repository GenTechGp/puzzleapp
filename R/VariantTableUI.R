tabMainUI <- function(ns) {
  div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "datatable.css")
    ),
    DT::dataTableOutput(ns("table"))
  )
}

tabUI <- function(id, tab_label) {
  ns <- NS(id)
  tabPanel(tab_label,
    tabMainUI(ns),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
    )
  )
}
