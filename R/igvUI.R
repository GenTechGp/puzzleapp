igvInputUI <- function(ns) {
  tagList(
    fluidRow(
      column(2, numericInput(ns("igv_max_window"), "Max window size:", 10000, 0)),
      column(1, numericInput(ns("igv_flanking"), "Flanking size:", 200, 0)),
      column(2, textInput(ns("igv_var_id"), "Variant ID:", value = "")),
      column(1, actionButton(ns("search_button"), "Search"), style = "margin-top: 25px;"),
      #empty column for spacing
      column(2, ""),
      column(2, textInput(ns("genome_coords"), "Genome coordinates:", placeholder = "chr:start-end or chr:pos")),
      column(1, actionButton(ns("update_button"), "Show"), style = "margin-top: 25px;"),
      tags$script(HTML(sprintf("
        $(document).on('keypress', function(e) {
          if(e.which == 13 && $('#%s').is(':focus')) {
            $('#%s').click();
          }
        });
      ", ns("igv_var_id"), ns("search_button")))),
      tags$script(HTML(sprintf("
        $(document).on('keypress', function(e) {
          if(e.which == 13 && $('#%s').is(':focus')) {
            $('#%s').click();
          }
        });
      ", ns("genome_coords"), ns("update_button"))))
    )
  )
}

#' IGV Tab UI
#' @param id Module ID
#' @param tab_label Label for the tab
#' @return Shiny UI object
#' @export
#' @import shiny
igvUI <- function(id, tab_label) {
  ns <- NS(id)
  tagList(
    igvInputUI(ns),
    igvShinyOutput(ns("igvShiny_0"))
  )
}
