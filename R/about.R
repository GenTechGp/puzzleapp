#' About Tab UI
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::br(),
    tags$p(paste0("PuzzleApp version: v", puzzleapp_version())),
    tags$p("README: ", tags$a(href = "https://github.com/GenTechGp/puzzleapp/blob/main/README.md", "https://github.com/GenTechGp/puzzleapp/blob/main/README.md", target = "_blank")),
    tags$p("Table Documentation: "),
    tabPanel(
      "Table Documentation",
      tabsetPanel(
        id = "doc_tabs",
        tabPanel("Config YAML", DT::dataTableOutput(ns("config_yaml_format"))),
        tabPanel("Filter TSV", DT::dataTableOutput(ns("filter_tsv_format"))),
        tabPanel("SNV TSV", DT::dataTableOutput(ns("snv_tsv_format"))),
        tabPanel("SV TSV", DT::dataTableOutput(ns("sv_tsv_format"))),
        tabPanel("SVlog predicates", DT::dataTableOutput(ns("svlog_predicates_format"))),
        tabPanel("VEP Consequences", DT::dataTableOutput(ns("vep_consequences_format")))
      )
    )
  )
}

#' About Tab Server
#' @param id Module ID
#' @param shared_store Shared reactive store for app data
#' @param shared_rx Shared reactive values for app state
#' @return Shiny server module
#' @export
aboutServer <- function(id, shared_store, shared_rx) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$config_yaml_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "config_yaml_format.tsv", package = "puzzleapp")
        # ensure the tsv is UTF-8 encoded to avoid issues with special characters
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$filter_tsv_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "filter_tsv_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$snv_tsv_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "snv_tsv_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$sv_tsv_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "sv_tsv_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$svlog_predicates_format <- DT::renderDataTable({
        doc_path <- system.file("extdata", "db", "table_schema", "documentation", "svlog_predicates_format.tsv", package = "puzzleapp")
        doc_data <- read.delim(doc_path, header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
      output$vep_consequences_format <- DT::renderDataTable({
        doc_data <- shared_store$vep_consequences
        DT::datatable(doc_data, options = list(pageLength = nrow(doc_data), scrollX = TRUE))
      })
    }
  )
}
