igvUI <- function(id, tab_label, chain_label) {
  ns <- NS(id)
  tabPanel(tab_label,
           sidebarLayout(
             sidebarPanel(
               textInput(ns("genome_coords"), "Genome coordinates:", value="",placeholder = "chr:start-end"),
               actionButton(ns("genome_coords_search"), "search"),
               br(),
               br(),
               tags$p(tags$strong("Load tracks:")),
               tags$div(
                 actionButton(ns("snvs_vcf"), "SNVs/Indels VCF"),
                 style = "margin-bottom: 10px;"
               ),
               tags$div(
                 actionButton(ns("svs_vcf"), "SVs VCF"),
                 style = "margin-bottom: 10px;"
               ),
               uiOutput(ns("dynamicButtons")),
               br(),
               br(),
               width=2
             ),
             mainPanel(
               igvShinyOutput(ns('igvShiny_0')),
               width=10
             )
           )
  )
}