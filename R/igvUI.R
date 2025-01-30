igvSidebarUI <- function(ns) {
  sidebarPanel(
    textInput(ns("igv_var_id"), "Variant ID:", value = ""),
    numericInput(ns("igv_max_window"), "Max window size:", 10000, 0),
    numericInput(ns("igv_flanking"), "Flanking size:", 200, 0),
    actionButton(ns("coords_button"), "get coords"),
    br(),
    br(),
    textInput(ns("genome_coords"), "Genome coordinates:",
              placeholder = "chr:start-end"),
    tags$script(HTML(sprintf("
      $(document).on('keypress', function(e) {
        if(e.which == 13 && $('#%s').is(':focus')) {
          $('#%s').click();
        }
      });
    ", ns("igv_var_id"), ns("coords_button")))),
    width = 2
  )
}

igvUI <- function(id, tab_label) {
  ns <- NS(id)
  tabPanel(tab_label,
    sidebarLayout(
      igvSidebarUI(ns),
      mainPanel(igvShinyOutput(ns("igvShiny_0")), width = 10)
    )
  )
}
