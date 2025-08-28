library(shiny)
# library(DT)
library(yaml)
library(data.table)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Home", home_ui("home")),
    tabPanel("Filter", selectFiltersUI("filter")),
    tabPanel("SNVs and Indels", variants_ui("snv_variants")),
    tabPanel("SVs", variants_ui("sv_variants")),
    tabPanel("PanelApp", variants_ui("panelapp")),
    tabPanel("Phenotype", variants_ui("phenotype")),
  )
)

server <- function(input, output, session) {
  # shared reactive store
  shared_data <- reactiveValues(
    data = NULL,
    paths = list()
  )

  home_server("home", shared_data)
  selectFiltersServer("filter", shared_data)
  variants_server("snv_variants", reactive(shared_data$snvs_data_filtered))
  variants_server("sv_variants", reactive(shared_data$svs_data_filtered))
  variants_server("panelapp", reactive(shared_data$panel_app_data))
  variants_server("phenotype", reactive(shared_data$phenotype_data))
}

shinyApp(ui, server)
