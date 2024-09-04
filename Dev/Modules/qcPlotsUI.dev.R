qcPlotsUI <- function(id,tab_label) {
  ns <- NS(id)
  tabPanel(tab_label,
           sidebarLayout(
             sidebarPanel(width = 3,
                          bsCollapse(id = ns("collapse_coverage"), open = "Coverage analysis",
                                     bsCollapsePanel("Coverage analysis",
                                                     selectInput(ns("plot_type1"), "Coverage plot:",
                                                                 choices = c("Average coverage", "Normalised coverage")),
                                                     style = "primary")
                          ),
                          bsCollapse(id = ns("collapse_vaf"), open = "VAF distribution",
                                     bsCollapsePanel("VAF distribution",
                                                     selectInput(ns("plot_type2"), "Allele fraction:",
                                                                 choices = c("Allele fraction")),
                                                     style = "primary")
                          ),
                          bsCollapse(id = ns("collapse_somalier"), open = "Somalier analysis",
                                     bsCollapsePanel("Somalier analysis",
                                                     selectInput(ns("x_var"), "X-axis:",
                                                                 choices = c("relatedness", "ibs0", "ibs2", "hom_concordance", 
                                                                             "shared_hets","shared_hom_alts","hets_ab"), selected = "ibs0"),
                                                     selectInput(ns("y_var"), "Y-axis:",
                                                                 choices = c("relatedness", "ibs0", "ibs2", "hom_concordance", 
                                                                            "shared_hets","shared_hom_alts","hets_ab"), selected = "ibs2"),
                                                     style = "primary")
                          )
             ),
             mainPanel( width = 9,
                        uiOutput(ns("plot_output1")),
                        hr(),
                        uiOutput(ns("plot_output2")),
                        hr(),
                        plotlyOutput(ns("somalier_plot")),
                        DT::dataTableOutput(ns("definitions_table"))
             )
           )
           # tags$head(
           #   tags$style(HTML("hr {border-top: 1.75px solid #D3D3D3;}"))
           # )
  )
}