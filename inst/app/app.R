library(shiny)
library(DT)
library(yaml)
library(data.table)
library(shinyjs)
library(jsonlite)
library(reactable)
library(stringr)
library(dplyr)

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
    tabPanel("SNV and Indels", tabUI("legacy_snv_variants", "SNVs & Indels")),
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
    dependencies = NULL,
    snvs_data = NULL,
    svs_data = NULL,
    snvs_data_filtered = NULL,
    svs_data = NULL,
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

  home_server("home", shared_data)
  selectFiltersServer("filter", shared_data)
  observe({
      req(shared_data$pref)  # wait until pref is available
      req(shared_data$pref$variants)
      req(shared_data$pref$working_dir)
      req(shared_data$legacy_snvs_data_filtered)
      selected <- isolate(shared_data$pref$variants)
      pref <- isolate(shared_data$pref)
      filtered_data <- reactiveVal(NULL)
      filtered_data(shared_data$legacy_snvs_data_filtered)
      # browser()
      tabServer(
        id = "legacy_snv_variants",
        filtered_data = filtered_data,
        selected = selected,
        pref = pref,
        selected_igv_id = reactive(input$igv_sample),
        exclude = NULL
      )
    })
  
  
  # variants_server("legacy_sv_variants", reactive(shared_data$legacy_svs_data_filtered), reactiveVal(shared_data$pref$variants))
  
  # variants_server("sv_variants", reactive(shared_data$svs_data_filtered))
  
  # variants_server("panelapp", reactive(shared_data$panel_app_data))
  # variants_server("phenotype", reactive(shared_data$phenotype_data))
}

shinyApp(ui, server)
