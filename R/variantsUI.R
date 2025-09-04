#' Variants Tab UI
#' @param id Module ID
#' @export
#' @import shiny
variants_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(style = "margin-bottom:10px;",
            shiny::actionButton(ns("reset_table"), "Reset Table Layout")
        )
      )
    ),
    # Max width + ellipsis for cells
    shiny::tags$style(shiny::HTML("
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