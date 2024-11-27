qcSidebarUI <- function(ns, cov, som, vaf) {
  soms <- c("relatedness", "ibs0", "ibs2", "hom_concordance", "shared_hets",
            "shared_hom_alts", "hets_ab")

  sidebarPanel(width = 3,
    conditionalPanel(
      condition = ifelse(cov, "true", "false"),
      collapseUI("collapse_coverage", "Coverage analysis", "primary",
        selectInput(ns("plot_type1"), "Coverage plot:",
                    c("Average coverage", "Normalised coverage"))
      ),
    ),
    conditionalPanel(
      condition = ifelse(vaf, "true", "false"),
      collapseUI("collapse_vaf", "VAF distribution", "primary",
        # TODO: only one selection?!
        selectInput(ns("plot_type2"), "Allele fraction:", c("Allele fraction"))
      ),
    ),
    conditionalPanel(
      condition = ifelse(som, "true", "false"),
      collapseUI("collapse_somalier", "Somalier analysis", "primary",
        selectInput(ns("x_var"), "X-axis:", soms, "ibs0"),
        selectInput(ns("y_var"), "Y-axis:", soms, "ibs2")
      ),
    )
  )
}

qcMainUI <- function(ns, cov, som, vaf) {
  mainPanel(width = 9,
    conditionalPanel(
      condition = ifelse(cov, "true", "false"),
      uiOutput(ns("plot_output1")),
      hr()
    ),
    conditionalPanel(
      condition = ifelse(vaf, "true", "false"),
      uiOutput(ns("plot_output2"))
    ),
    conditionalPanel(
      condition = ifelse(som, "true", "false"),
      hr(),
      plotlyOutput(ns("somalier_plot")),
      DT::dataTableOutput(ns("definitions_table"))
    )
  )
}

qcPlotsUI <- function(id, tab_label, cov, som, vaf) {
  ns <- NS(id)
  tabPanel(tab_label,
    sidebarLayout(
      qcSidebarUI(ns, cov, som, vaf),
      qcMainUI(ns, cov, som, vaf)
    )
  )
}
