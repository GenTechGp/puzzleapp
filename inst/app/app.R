library(puzzleapp)

# App-level init: console logging + purge logs older than 100 days
# use the $HOME/puzzleapp_logs directory if available, otherwise "logs" in current dir
logs_dir <- if (nzchar(Sys.getenv("HOME"))) file.path(Sys.getenv("HOME"), "puzzleapp/logs") else "logs"
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
setup_app_logging(level = "debug", logs_dir = logs_dir, older_than_days = 100, console = TRUE)

ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right"),
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
  shinyjs::useShinyjs(),
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Home", home_ui("home")),
    tabPanel("Filter", selectFiltersUI("filter")),
    tabPanel("SNV/Indel", dataUI("snv_variants")),
    tabPanel("SV", dataUI("sv_variants")),
    tabPanel("IGV", igvUI("igv")),
    tabPanel("Logs", log_viewer_ui("log"))
  )
)

server <- function(input, output, session) {
  # Start per-session file logging
  start_session_logger(session, logs_dir = logs_dir, prefix = "session", console = TRUE)
  log_info(sprintf("Writing session logs to directory: %s", logs_dir))

  # Shared storage (plain variables) and reactive version token
  shared_store <- new.env(parent = emptyenv())
  shared_store$value_for_data  <- list()
  shared_store$data_for_data  <- list()
  shared_store$original_data  <- list()
  shared_store$preferred_cols <- list()
  shared_store$samples <- NULL
  shared_store$pedigree <- NULL
  shared_store$panel_app_data <- NULL
  shared_store$vep_map <- NULL
  shared_store$phenotype_data <- NULL
  shared_store$vep_consequences <- NULL
  shared_store$igv_data <- NULL
  shared_store$work_dir <- NULL
  shared_store$verbose_level <- 0L
  shared_rx <- list(
    data_version = reactiveVal(0L),
    panelapp_version = reactiveVal(0L),
    igv_version = reactiveVal(0L)
  )

  home_server("home", shared_store, shared_rx)
  selectFiltersServer("filter", shared_store, shared_rx)
  dataServer("snv_variants", shared_store, shared_rx, "SNV")
  dataServer("sv_variants", shared_store, shared_rx, "SV")
  igv_server("igv", shared_store, shared_rx)
  # Expose the current session's log to viewer as default selection
  log_viewer_server("log", logs_dir = logs_dir, session_logfile_reactive = shiny::reactive(session$userData$logfile))

  # log_debug("debug test")
  # log_info("info test")
  # log_warn("warning test")
  # log_error("error test")

  # Switch to IGV tab whenever an ID is clicked
  observeEvent(shared_rx$igv_version(), {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "IGV")
  }, ignoreInit = TRUE)

}

shinyApp(ui, server, options = list(launch.browser=TRUE))
