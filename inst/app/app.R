library(shiny)
library(DT)
library(yaml)
library(data.table)
library(shinyjs)
library(jsonlite)

ui <- fluidPage(
  tags$script(HTML("
    function getCookie(name) {
      let match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
      if (match) return JSON.parse(match[2]);
      return null;
    }

    // Optional: update cookie when server sends set_cookie
    Shiny.addCustomMessageHandler('set_cookie', function(message) {
      document.cookie = message.name + '=' + JSON.stringify(message.value) + '; path=/; max-age=31536000';
      Shiny.setInputValue('home-cookie_prefs', getCookie(message.name), {priority: 'event'});
    });
  ")),
  useShinyjs(),
  tabsetPanel(
    tabPanel("Home", home_ui("home")),
    tabPanel("Filter", selectFiltersUI("filter")),
    tabPanel("SNV and Indels", tabUI("legacy_snv_variants", "SNVs & Indels")),
    tabPanel("SVs", variants_ui("legacy_sv_variants")),
    tabPanel("(snvs)", variants_ui("snv_variants")),
    tabPanel("(svs)", variants_ui("sv_variants")),
    tabPanel("PanelApp", variants_ui("panelapp")),
    tabPanel("Phenotype", variants_ui("phenotype")),
  )
)

server <- function(input, output, session) {
  # shared reactive store
  shared_data <- reactiveValues(
    samples = NULL,
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
    pref = list(variants = NULL, panelapp = NULL, phenotype = NULL, outdir = ""),
    paths = list()
  )

  home_server("home", shared_data)
  selectFiltersServer("filter", shared_data)

  # Only initialize legacy tab modules after shared_data$pref exists
  observe({
    req(shared_data$pref)  # wait until pref is available
    req(shared_data$pref$variants)
    req(shared_data$pref$outdir)
    req(shared_data$legacy_snvs_data_filtered)
    selected <- isolate(shared_data$pref$variants)
    pref <- isolate(shared_data$pref)
    outdir <- isolate(shared_data$pref$outdir)
    filtered_data = reactiveVal(NULL)
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
  
  variants_server("legacy_sv_variants", reactive(shared_data$legacy_svs_data_filtered))
  
  variants_server("snv_variants", reactive(shared_data$snvs_data_filtered))
  variants_server("sv_variants", reactive(shared_data$svs_data_filtered))
  
  variants_server("panelapp", reactive(shared_data$panel_app_data))
  variants_server("phenotype", reactive(shared_data$phenotype_data))
}

shinyApp(ui, server)
