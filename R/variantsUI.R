#' Variants Tab UI
#' @param id Module ID
#' @export
#' @import shiny
variants_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("variants_table"))
  )
}
