#' Home Tab UI
#'
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidRow(
      column(6, style = "padding: 1;", textInput(ns("yml_path"), label = NULL, placeholder = ".yml config file path (optional)", width = "100%")),
      column(2, actionButton(ns("load_yml"), "Load from file", class = "btn-primary")),
      column(4)  # empty space
    ),

    strong("Number of Individuals (set this value first):"),
    fluidRow(
      column(1, numericInput(ns("num_individuals"), label = NULL, value = 1, min = 1, step = 1)),
      column(11)  # empty space
    ),

    uiOutput(ns("samples_panel")),

    fluidRow(
      column(6, style = "padding: 1;", textInput(ns("snvs_vcf"), "SNVs & Indels VCF:", width = "100%")),
      column(6, style = "padding: 1;", textInput(ns("snvs_tsv"), "SNVs & Indels TSV:", width = "100%"))
    ),
    fluidRow(
      column(6, style = "padding: 1;", textInput(ns("svs_vcf"), "SVs VCF:", width = "100%")),
      column(6, style = "padding: 1;", textInput(ns("svs_tsv"), "SVs TSV:", width = "100%"))
    ),
    fluidRow(
      column(4, style = "padding: 1;", textInput(ns("panel_app"), "PanelApp DB:", placeholder = "optional. leave blank to load from internal db", width = "100%")),
      column(4, style = "padding: 1;", textInput(ns("vep_consequences"), "VEP consequence annotations:", placeholder = "optional. leave blank to load from internal db", width = "100%")),
      column(4, style = "padding: 1;", textInput(ns("phenotype_data"), "Human Phenotype Ontology DB:", placeholder = "optional. leave blank to load from internal db", width = "100%"))
    ),

    fluidRow(
      column(2, actionButton(ns("clear_inputs"), "Clear inputs and delete loaded data", class = "btn-danger")),
      column(2, actionButton(ns("load_data"), "Load Data", class = "btn-primary")),
      column(8)  # empty space
    ),
    br(),
    # Feedback text
    textOutput(ns("status"))
  )

}

