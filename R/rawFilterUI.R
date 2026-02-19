#' Raw Filter Tab UI
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
rawFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::br(),
    fluidRow(
      column(6, style = "padding: 1;", textInput(ns("load_from_file"), label = NULL, placeholder = "Filter file path:", width = "100%")),
      column(1, actionButton(ns("btn_load_raw_filters"), "Load filter file", class = "btn-primary")),
      column(1, actionButton(ns("btn_apply_raw_filters"), "Apply filters", class = "btn-primary")),
      column(4)
    ),
    fluidRow(
      column(
        12,
        DT::DTOutput(ns("filter_table"))
      )
    )
  )
}