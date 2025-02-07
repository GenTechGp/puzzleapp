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
               fluidRow(
                 column(12,
                        div(textInput(ns("sample_path"), "Path to dataset:", 
                                      placeholder = "Path to your sample folder", 
                                      width = "100%"), 
                            style = "width: 100%;"))
               )
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
