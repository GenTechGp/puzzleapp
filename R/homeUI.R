preferencesUI <- function(ns) {
  
  # Helper function to create collapsible selectize inputs
  createPreferencesSection <- function(id, title) {
    selectizeInput(ns(id), title, 
                   choices = NULL, 
                   multiple = TRUE, 
                   options = list(plugins = c("drag_drop")), 
                   width = "100%")
  }
  
  collapseUI("preferences_collapse", "Preferences", "primary",
             div(
               fluidRow(
                 column(12, createPreferencesSection("variants_preferences", "Variants")),
                 column(12, createPreferencesSection("panelapp_preferences", "PanelApp")),
                 column(12, createPreferencesSection("phenotype_preferences", "Phenotype")),
                 column(12, div(actionButton(ns("update_preferences"), "Save preferences", 
                                             class = "btn-primary"), 
                                style = "margin-top: 10px; text-align: left;"))
               )
             )
  )
}

sampleUI <- function(ns, panel_app_genes) {
  
  collapseUI("samples_collapse", "Data", "primary",
             div(
               # Apply CSS for controlling column widths for ONLY the meta table
               tags$head(
                 tags$style(HTML(sprintf("
                 .meta-table table {
                   width: 100%%;
                 }
                 .meta-table th, .meta-table td {
                   text-align: left;
                   padding: 8px;
                 }
                 .table.shiny-table>thead>tr>th, .table.shiny-table>thead>tr>td, .table.shiny-table>tbody>tr>th, .table.shiny-table>tbody>tr>td, .table.shiny-table>tfoot>tr>th, .table.shiny-table>tfoot>tr>td {
		              padding-left: 0px;}
                 .meta-table th:nth-child(1), .meta-table td:nth-child(1) { width: 10%%; } /* Sample ID */
                 .meta-table th:nth-child(2), .meta-table td:nth-child(2) { width: 8%%; } /* Kinship */
                 .meta-table th:nth-child(3), .meta-table td:nth-child(3) { width: 8%%; } /* Status */
                 .meta-table th:nth-child(4), .meta-table td:nth-child(4) { width: 8%%; } /* Sex */
                 .meta-table th:nth-child(5), .meta-table td:nth-child(5) { width: 33%%; } /* BAM */
                 .meta-table th:nth-child(6), .meta-table td:nth-child(6) { width: 33%%; } /* Coverage */
               "))),
                 tags$style(HTML("
                  .yaml-button-row {
                    display: flex;
                    align-items: flex-end;
                    height: 60px;
                  }
                  .yaml-button-row > div {
                    margin-right: 10px;
                  }
                "))),
               
               # fluidRow(
               #   column(12,
               #          div(textInput(ns("sample_path"), "Path to dataset:", 
               #                        placeholder = "Path to your sample folder", 
               #                        width = "100%"), 
               #              style = "width: 100%;"))
               # )
             ),
             br(),
             # fluidRow(
             #   column(8,
             #          div(textInput(ns("yaml_path"), "Path to YAML:", 
             #                        width = "100%"), 
             #              style = "width: 100%;"))
             # ),
             fluidRow(
               column(6,
                      textInput(ns("yaml_path"), "Path to YAML:", width = "100%")
               ),
               column(6,
                      div(class = "yaml-button-row",
                          div(actionButton(ns("load_yaml"), "Load YAML", class = "btn-primary")),
                          div(actionButton(ns("clear_inputs"), "Clear Inputs", class = "btn-danger"))
                      )
               )
             ),
             fluidRow(
               column(1, numericInput(ns("num_individuals"), "Individuals:", 
                                      value = 1, min = 1, step = 1, width = "50%"))
             ),
             fluidRow(
               column(12, div(class = "meta-table", tableOutput(ns("meta"))))
             ),
             fluidRow(
               column(6,
                      div(textInput(ns("snvs_vcf_path"), "SNVs & Indels VCF:", 
                                    placeholder = "", 
                                    width = "100%"), 
                          style = "width: 100%;")),
               column(6,
                      div(textInput(ns("snvs_tsv_path"), "SNVs & Indels TSV:", 
                                    placeholder = "", 
                                    width = "100%"), 
                          style = "width: 100%;")),
             ),
             fluidRow(
               column(6,
                      div(textInput(ns("sv_vcf_path"), "SVs VCF:", 
                                    placeholder = "", 
                                    width = "100%"), 
                          style = "width: 100%;")),
               column(6,
                      div(textInput(ns("svs_tsv_path"), "SVs TSV:", 
                                    placeholder = "", 
                                    width = "100%"), 
                          style = "width: 100%;")),
             ),
             fluidRow(
               column(6,
                      div(textInput(ns("panelapp_path"), "PanelApp TSV:", 
                                    placeholder = "", 
                                    width = "100%"), 
                          style = "width: 100%;")),
               column(6,
                      div(textInput(ns("hpo_phenotype_path"), "Human Phenotype Ontology TSV:", 
                                    placeholder = "", 
                                    width = "100%"), 
                          style = "width: 100%;")),
             ),
             fluidRow(
               column(12, div(actionButton(ns("load_sample"), "Load data",
                                           class = "btn-primary"),
                              style = "margin-top: 10px; text-align: left; width: 100%;"))
             )
  )
}

homeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('reloadPage', function(message) {
          location.reload();
        });
      "))
    ),
    h2("Welcome to JigSeq PuzzleApp!"),
    p("This application allows you to explore and analyse genomic data. Start by loading a new sample or using the navigation tabs above."),
    #br(),
    # fluidRow(
    #   textInput(ns("sample_path"), "Enter Sample Path:", placeholder = "Path to your sample folder")
    # ),
    # fluidRow(
    #   actionButton(ns("load_sample"), "Load Sample", class = "btn-primary")
    # ),
    fluidRow(
      sampleUI(ns)
    ),
    #br(),
    fluidRow(
      preferencesUI(ns)
    )
  )
}
