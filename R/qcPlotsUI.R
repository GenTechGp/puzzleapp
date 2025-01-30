qcSidebarUI <- function(ns) {
  sidebarPanel(width = 3,
               uiOutput(ns("coverage_ui")),
               uiOutput(ns("vaf_ui")),
               uiOutput(ns("somalier_ui"))
  )
}

qcMainUI <- function(ns) {
  mainPanel(width = 9,
            uiOutput(ns("plot_output1")),
            uiOutput(ns("plot_output2")),
            uiOutput(ns("somalier_output"))
  )
}

qcPlotsUI <- function(id, tab_label) {
  ns <- NS(id)
  tabPanel(tab_label,
           sidebarLayout(
             qcSidebarUI(ns),
             qcMainUI(ns)
           )
  )
}

