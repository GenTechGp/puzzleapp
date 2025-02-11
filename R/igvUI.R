igvInputUI <- function(ns) {
  fluidRow(
    column(2, textInput(ns("igv_var_id"), "Variant ID:", value = "")),
    column(2, numericInput(ns("igv_max_window"), "Max window size:", 10000, 0)),
    column(2, numericInput(ns("igv_flanking"), "Flanking size:", 200, 0)),
    column(2, actionButton(ns("coords_button"), "get coords")),
    column(2, textInput(ns("genome_coords"), "Genome coordinates:",
              placeholder = "chr:start-end")),
    tags$script(HTML(sprintf("
      $(document).on('keypress', function(e) {
        if(e.which == 13 && $('#%s').is(':focus')) {
          $('#%s').click();
        }
      });
    ", ns("igv_var_id"), ns("coords_button"))))
  )
}

igvUI <- function(id, tab_label) {
  ns <- NS(id)
  tabPanel(tab_label,
    igvInputUI(ns),
    igvShinyOutput(ns("igvShiny_0"))
  )
}
