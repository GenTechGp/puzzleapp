#' Variants Tab UI
#' @param id Module ID
#' @export
#' @import shiny
# variants_ui <- function(id) {
#   ns <- shiny::NS(id)
#   shiny::tagList(
#     shiny::tags$style(shiny::HTML("
#       table.dataTable td {
#         max-width: 200px;
#         white-space: nowrap;
#         overflow: hidden;
#         text-overflow: ellipsis;
#       }
#     ")),
#     DT::dataTableOutput(ns("variants_table"))
#   )
# }
variants_ui <- function(id) {
  ns <- NS(id)
  DTOutput(ns("table"))
}