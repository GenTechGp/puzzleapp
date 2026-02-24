#' About Tab UI
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::br(),
    tags$p("PuzzleApp version: v0.0.1"),
    tags$p("README: ", tags$a(href = "https://github.com/KCCGGenomeTechLab/puzzleapp/blob/pack/README.md", "https://github.com/KCCGGenomeTechLab/puzzleapp/blob/pack/README.md", target = "_blank")),
    tags$p("Table Documentation: "),
    tabPanel(
      "Table Documentation",
      tabsetPanel(
        id = "doc_tabs",
        tabPanel("Config YAML", DT::dataTableOutput(ns("config_yaml_format"))),
        tabPanel("Filter TSV", DT::dataTableOutput(ns("filter_tsv_format"))),
        tabPanel("SNV TSV", DT::dataTableOutput(ns("snv_tsv_format"))),
        tabPanel("SV TSV", DT::dataTableOutput(ns("sv_tsv_format")))
      )
    )
  )
}

#' About Tab Server
#' @param id Module ID
#' @return Shiny server module
#' @export
aboutServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$config_yaml_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "config_yaml_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE)
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$filter_tsv_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "filter_tsv_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE)
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$snv_tsv_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "snv_tsv_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE)
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$sv_tsv_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "sv_tsv_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE)
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
    }
  )
}
