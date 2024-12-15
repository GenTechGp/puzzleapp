kinship_labels <- c("proband", "mother", "father", "brother", "uncle")

buttons <- function(ns, kinship) {
  lapply(seq_along(kinship_labels), function(i) {
    if (kinship_labels[i] %in% kinship) {
      tags$div(actionButton(ns(sprintf("add%sBAMTrackButton", capitalize_word(kinship_labels[i]))), sprintf("%s BAM", capitalize_word(kinship_labels[i]))), style = "margin-bottom: 10px;")
    }
  })
}

igvSidebarUI <- function(ns, kinship) {
  sidebarPanel(
    textInput(ns("genome_coords"), "Genome coordinates:",
              placeholder = "chr:start-end"),
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
    tagList(buttons(ns, kinship)),
    br(),
    br(),
    width = 2
  )
}

igvUI <- function(id, tab_label, kinship) {
  ns <- NS(id)
  tabPanel(tab_label,
    sidebarLayout(
      igvSidebarUI(ns, kinship),
      mainPanel(igvShinyOutput(ns("igvShiny_0")), width = 10)
    )
  )
}
