#' Variants Tab UI
#' @param id Module ID
#' @export
#' @import shiny
variants_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 12,
        div(style = "margin-bottom:10px;",
            actionButton(ns("reset_table"), "Reset Table Layout")
        )
      )
    ),
    # Max width + ellipsis for cells
    tags$style(HTML("
      table.dataTable td {
        max-width: 200px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
    ")),
    DT::dataTableOutput(ns("variants_table"))
  )
}