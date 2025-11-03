#' @keywords internal
prefOptsUI <- function(ns) {
  createPreferencesSection <- function(id, title) {
    selectizeInput(ns(id), title, choices = NULL, multiple = TRUE, options = list(plugins = c("drag_drop")), width = "100%")
  }
  fluidRow(
    column(12, createPreferencesSection("snv_preferences", "SNV columns")),
    column(12, createPreferencesSection("sv_preferences", "SV columns")),
    column(12, createPreferencesSection("panelapp_preferences", "PanelApp columns")),
    column(12, createPreferencesSection("phenotype_preferences", "Phenotype columns")),
    column(12, div(actionButton(ns("update_preferences"), "Save preferred columns", class = "btn-primary"), style = "margin-top: 10px; text-align: left;"))
  )
}

dbOptsUI <- function(ns) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("panel_app"), "PanelApp DB:", placeholder = "", width = "100%")),
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("phenotype_data"), "Human Phenotype Ontology DB:", placeholder = "", width = "100%"))
    )
  )
}


#' Home Tab UI
#'
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    shiny::fluidRow(
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("yml_path"), label = NULL, placeholder = ".yml config file path (optional)", width = "100%")),
      shiny::column(3, 
      shiny::actionButton(ns("load_yml"), "Load from file", class = "btn-primary")
      ),
      shiny::column(3)  # empty space
    ),

    shiny::strong("Number of Individuals (set this value first):"),
    shiny::fluidRow(
      shiny::column(1, shiny::numericInput(ns("num_individuals"), label = NULL, value = 1, min = 1, step = 1)),
      shiny::column(11)  # empty space
    ),

    shiny::uiOutput(ns("samples_panel")),

    shiny::fluidRow(
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("snvs_vcf"), "SNVs & Indels VCF:", width = "100%")),
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("snvs_tsv"), "SNVs & Indels TSV:", width = "100%"))
    ),
    shiny::fluidRow(
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("svs_vcf"), "SVs VCF:", width = "100%")),
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("svs_tsv"), "SVs TSV:", width = "100%"))
    ),

    shiny::fluidRow(
      shiny::column(6, style = "padding: 1;", shiny::textInput(ns("work_dir"), "Working directory:", placeholder = "optional. leave blank to use $HOME dir", width = "100%")),
      shiny::column(6, style="padding:1;", shiny::selectizeInput(ns("igv_genome"), "IGV genome:", choices=c("hg38","hg19","mm10","rn6"), selected="hg38", multiple=FALSE, options=list(create=TRUE, placeholder="Select or type a genome..."), width="100%"))
    ),

    shiny::br(),
    shiny::fluidRow(
      shiny::column(12, shiny::checkboxInput(ns("load_local_db"), "Load PanelApp and HPO from local DB", value = TRUE))
    ),
    shiny::div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_db", "+", style="padding:0 6px;min-width:30px;"), shiny::span("Show database options", id="toggle_db_label")),
    shiny::div(id="db_container", style="display:none;margin-top:10px;", dbOptsUI(ns)),
    shiny::tags$script(shiny::HTML("$('#toggle_db').on('click',function(){var c=$('#db_container');var b=$('#toggle_db');var l=$('#toggle_db_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide database options');}else{b.text('+');l.text('Show database options');}});")),
    shiny::br(),

    shiny::div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_pref", "+", style="padding:0 6px;min-width:30px;"), shiny::span("Show preferred column lists (add/remove/reorder)", id="toggle_pref_label")),
    shiny::div(id="pref_container", style="display:none;margin-top:10px;", prefOptsUI(ns)),
    shiny::tags$script(shiny::HTML("$('#toggle_pref').on('click',function(){var c=$('#pref_container');var b=$('#toggle_pref');var l=$('#toggle_pref_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide preferred column lists (add/remove/reorder)');}else{b.text('+');l.text('Show preferred column lists (add/remove/reorder)');}});")),
    shiny::br(),

    shiny::fluidRow(
      shiny::column(2, shiny::actionButton(ns("clear_inputs"), "Clear inputs and delete loaded data", class = "btn-danger")),
      shiny::column(2, shiny::actionButton(ns("load_data"), "Load Data", class = "btn-primary")),
      shiny::column(8)  # empty space
    ),
    shiny::br(),
    # Feedback text
    shiny::textOutput(ns("status")),

  )

}
