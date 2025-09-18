library(shiny)
library(DT)
library(yaml)
library(data.table)
library(shinyjs)
library(jsonlite)
library(reactable)
library(stringr)
library(dplyr)
library(lobstr)

ui <- fluidPage(
  tags$script(HTML("
    function getCookie(name) {
      let match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
      if (match) return JSON.parse(match[2]);
      return null;
    }
    // Server -> Client: set cookie
    Shiny.addCustomMessageHandler('set_cookie', function(message) {
      document.cookie = message.name + '=' + JSON.stringify(message.value) + '; path=/; max-age=31536000';
      // update input immediately after setting
      Shiny.setInputValue('home-cookie_prefs', getCookie(message.name), {priority: 'event'});
    });
    // Client -> Server: always send cookie on connect
    $(document).on('shiny:connected', function() {
      var cookieVal = getCookie('user_prefs');
      Shiny.setInputValue('home-cookie_prefs', cookieVal, {priority: 'event'});
    });
  ")),
  useShinyjs(),
  tabsetPanel(
    tabPanel("Home", home_ui("home")),
    tabPanel("Filter", selectFiltersUI("filter")),
    tabPanel("Variants", dataUI("Variants")),
    # tabPanel("SNV and Indels", tabUI("legacy_snv_variants", "SNVs & Indels")),
    # tabPanel("SVs", variants_ui("legacy_sv_variants")),
    # tabPanel("(snvs)", variants_ui("snv_variants")),
    # tabPanel("(svs)", variants_ui("sv_variants")),
    # tabPanel("PanelApp", variants_ui("panelapp")),
    # tabPanel("Phenotype", variants_ui("phenotype")),
  )
)

server <- function(input, output, session) {
  # shared reactive store
  shared_data <- reactiveValues(
    samples = NULL,
    pedigree = NULL,
    snvs_data = NULL,
    svs_data = NULL,
    snvs_data_filtered = NULL,
    svs_data_filtered = NULL,
    panel_app_data = NULL,
    vep_map = NULL,
    phenotype_data = NULL,
    vep_consequences = NULL,
    legacy_snvs_data_filtered = NULL,
    legacy_svs_data_filtered = NULL,
    pref = list(variants = NULL, panelapp = NULL, phenotype = NULL, working_dir = ""),
    work_dir = NULL,
    paths = list()
  )
  # Shared storage (plain variables) and reactive version token
  shared_store <- new.env(parent = emptyenv())
  shared_store$data_for_data  <- list()
  shared_store$original_data  <- list()
  shared_store$preferred_cols <- character(0)
  shared_store$samples <- NULL
  shared_store$pedigree <- NULL
  shared_store$panel_app_data <- NULL
  shared_store$vep_map <- NULL
  shared_store$phenotype_data <- NULL
  shared_store$vep_consequences <- NULL
  shared_store$work_dir <- NULL
  shared_store$verbose_level <- 0L
  shared_rx <- list(
    data_version = reactiveVal(0L),
    panelapp_version = reactiveVal(0L)

  )

  home_server("home", shared_data, shared_store, shared_rx)
  selectFiltersServer("filter", shared_data, shared_store, shared_rx)
  dataServer("Variants", shared_store, shared_rx)
  
}

shinyApp(ui, server)
