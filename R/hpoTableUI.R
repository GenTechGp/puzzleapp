hpoSidebarUI <- function(ns) {
  sidebarPanel(width = 3,
    textInput(ns("hpo_term"), "HPO term:", value = ""),
    actionButton(ns("hpo_search"), label = "search"),
    br(), br(),
    textInput(ns("gene_symbol"), "Gene symbol:", value = ""),
    actionButton(ns("gene_search"), label = "search")
  )
}

hpoTabUI <- function(id, tab_label) {
  ns <- NS(id)
  tabPanel(tab_label,
    sidebarLayout(
      hpoSidebarUI(ns),
      mainPanel(width = 9, DT::dataTableOutput(ns("hpo_table")))
    ),
    tags$head(
      tags$style(HTML("hr {border-top: 1.75px solid #D3D3D3;}"))
    )
  )
}
