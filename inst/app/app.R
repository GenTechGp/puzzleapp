library(puzzleapp)
# options(warn = 2)

# App-level init: console logging + purge logs older than 100 days
# use the $HOME/.puzzleapp/logs directory if available, otherwise "logs" in current dir
logs_dir <- if (nzchar(Sys.getenv("HOME"))) file.path(Sys.getenv("HOME"), ".puzzleapp/logs") else "logs"
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
    
    document.documentElement.setAttribute('lang', 'en');

    // --- Dynamic page title (updated after data loads) ---
    Shiny.addCustomMessageHandler('update_title', function(title) {
      document.title = title;
    });

    // --- Unload warning (enable after data is loaded) ---
    var _warnOnLeave = false;
    function _beforeUnloadHandler(e) {
      e.preventDefault();
      e.returnValue = '';  // required for Chrome
    }
    Shiny.addCustomMessageHandler('set_leave_warning', function(message) {
      _warnOnLeave = message.enable;
      if (_warnOnLeave) {
        window.addEventListener('beforeunload', _beforeUnloadHandler);
      } else {
        window.removeEventListener('beforeunload', _beforeUnloadHandler);
      }
    });
  ")),
    tags$head(tags$title("PuzzleApp")),
    tags$head(tags$meta(name="description", content="PuzzleApp — genomic variant explorer")),
    # ::: because genome_server_js() is internal, like load_annotation_track()
    # whose message handler it registers. library(puzzleapp) above only puts
    # exported functions on the search path, so a bare call fails from an
    # installed package.
    tags$head(puzzleapp:::genome_server_js()),
    tags$head(tags$style(HTML("
    /* Group 1: Data tabs */
    .nav-tabs > li > a[data-value='Filter'],
    .nav-tabs > li > a[data-value='SNV/Indel'],
    .nav-tabs > li > a[data-value='SV'] {
        background-color: #e8f4ff;
        border-bottom: 2px solid #b3dafc;
        color: #1a5276;
    }
    /* Group 2: Interpretation tabs */
    .nav-tabs > li > a[data-value='PanelApp'],
    .nav-tabs > li > a[data-value='Phenotype'] {
        background-color: #f9f5e8;
        border-bottom: 2px solid #ead8a6;
        color: #7d5a00;
    }
    /* Hover style */
    .nav-tabs > li > a:hover {
      filter: brightness(0.95);
    }
    /* Active tab highlight */
    .nav-tabs > li.active > a {
        border-bottom: 2px solid #007bff !important;
        font-weight: bold;
    }
    /* btn-danger: darken background so white text clears 4.5:1 */
    .btn-danger {
        background-color: #b52b27;
        border-color: #8e1c18;
    }
    /* shiny-notification-message: darken text for WCAG AA contrast (default #31708f fails at ~4:1) */
    .shiny-notification-message {
        color: #0a3347;
    }
    /* irs-single: slider value bubble — white on #428bca is only ~3.6:1, darken bg to pass AA */
    .irs-single {
        background: #2471a3;
    }
    /* Version label, right-aligned on the tab bar. Absolutely positioned so it
       is out of the layout flow and costs no row of its own; the padding
       matches .nav-tabs > li > a so it sits on the same baseline as the tab
       labels. pointer-events:none keeps it from swallowing clicks if a narrow
       window lets the tabs run underneath it. */
    .puzzleapp-tabbar {
        position: relative;
    }
    .puzzleapp-version {
        position: absolute;
        top: 0;
        right: 0;
        padding: 10px 15px;
        font-size: 0.85em;
        color: #6c757d;
        pointer-events: none;
    }
  "))),
  shinyjs::useShinyjs(),
  tags$main(
  div(
  class = "puzzleapp-tabbar",
  div(class = "puzzleapp-version", paste0("v", puzzleapp:::puzzleapp_version())),
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Home", home_ui("home")),
    tabPanel("Filter", selectFiltersUI("filter")),
    tabPanel("SNV/Indel", dataUI("snv_variants")),
    tabPanel("SV", dataUI("sv_variants")),
    tabPanel("IGV", igvUI("igv")),
    tabPanel("PanelApp", dataUI("panel_app")),
    tabPanel("Phenotype", dataUI("phenotype")),
    tabPanel("QC Plots", qcPlots("qc_plots")),
    tabPanel(
      "Help",
      tabsetPanel(
        id = "help_tabs",
        tabPanel("Raw Filter", rawFilterUI("raw_filter")),
        tabPanel("Custom Annotation", customUI("custom_tab")),
        tabPanel("Logs", log_viewer_ui("log")),
        tabPanel("About", aboutUI("about"))
      )
    )
  )
  ) # div.puzzleapp-tabbar
  ) # tags$main
  # Workaround for Shiny bug #2845: checkboxGroupInput/radioButtons emit <label for="div-id">
  # which Chrome flags as invalid. Converts those group labels to <span> (aria-labelledby still works).
  ,tags$script(HTML("
    (function() {
      var labelable = {INPUT:1,SELECT:1,TEXTAREA:1,BUTTON:1,METER:1,OUTPUT:1,PROGRESS:1};
      function fixGroupLabels() {
        document.querySelectorAll('label.control-label[for]').forEach(function(labelEl) {
          var target = document.getElementById(labelEl.getAttribute('for'));
          if (target && !labelable[target.tagName]) {
            var span = document.createElement('span');
            Array.from(labelEl.attributes).forEach(function(attr) {
              if (attr.name !== 'for') span.setAttribute(attr.name, attr.value);
            });
            span.innerHTML = labelEl.innerHTML;
            labelEl.parentNode.replaceChild(span, labelEl);
          }
        });
      }
      // Fix DataTables search inputs missing id/name (generated by DT without these attrs)
      var _dtColIdx = 0, _szIdx = 0;
      function fixDTSearchInputs() {
        // Main table search (has aria-controls pointing to table id)
        document.querySelectorAll('input[type=search][aria-controls]:not([id])').forEach(function(el) {
          var tableId = el.getAttribute('aria-controls');
          el.setAttribute('id', 'dt-search-' + tableId);
          el.setAttribute('name', 'dt-search-' + tableId);
        });
        // Column filter inputs (no aria-controls, generated by DT filter top/bottom)
        document.querySelectorAll('input[type=search]:not([aria-controls]):not([id])').forEach(function(el) {
          var uid = 'dt-col-search-' + (_dtColIdx++);
          el.setAttribute('id', uid);
          el.setAttribute('name', uid);
        });
        // selectize.js internal width-measuring input (hidden, 4px wide, no id)
        document.querySelectorAll('input[autocomplete=new-password]:not([id])').forEach(function(el) {
          var uid = 'selectize-internal-' + (_szIdx++);
          el.setAttribute('id', uid);
          el.setAttribute('name', uid);
        });
        // igv.js internal inputs (locus search + any igv dialog inputs, version-agnostic)
        document.querySelectorAll('input.igv-search-input:not([id]), [class*=igv][class*=dialog] input:not([id]), [class*=igv-ui] input:not([id])').forEach(function(el, i) {
          el.setAttribute('id', 'igv-input-' + i);
          el.setAttribute('name', 'igv-input-' + i);
        });
      }
      // Ensure <select> elements have aria-label (sr-only label text not counted by Lighthouse)
      function fixSelectAriaLabels() {
        document.querySelectorAll('select:not([aria-label])').forEach(function(sel) {
          if (!sel.id) return;
          var label = document.querySelector('label[for=\"' + sel.id + '\"]');
          if (label) {
            var text = label.textContent.trim();
            if (text) sel.setAttribute('aria-label', text);
          }
        });
      }
      function fixAll() { fixGroupLabels(); fixDTSearchInputs(); fixSelectAriaLabels(); }
      fixAll();
      new MutationObserver(fixAll).observe(
        document.documentElement, {childList:true, subtree:true}
      );
    })();
  "))
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
  shared_store$phenotype_data <- NULL
  shared_store$vep_consequences <- NULL
  shared_store$svlog_db <- NULL
  shared_store$igv_data <- NULL
  shared_store$gene_symbol_data <- NULL
  shared_store$hpo_id_data <- NULL
  shared_store$work_dir <- NULL
  shared_store$sticky_work_dir <- FALSE
  shared_store$html <- list()
  shared_store$verbose_level <- 0L
  shared_rx <- list(
    data_version = reactiveVal(0L),
    panelapp_version = reactiveVal(0L),
    igv_version = reactiveVal(0L),
    genesymbol_version = reactiveVal(0L),
    hpoid_version = reactiveVal(0L),
    qcplot_version = reactiveVal(0L)
  )
  shared_store$vep_consequences <- read.delim(system.file("extdata", "db", "vep_consequences", "October_2025", "vep_annotations.tsv", package = "puzzleapp"), header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  shared_store$vep_consequences <- data.table::as.data.table(shared_store$vep_consequences)

  home_server("home", shared_store, shared_rx)
  selectFiltersServer("filter", shared_store, shared_rx)
  dataServer("snv_variants", shared_store, shared_rx, "SNV", "SNV")
  dataServer("sv_variants", shared_store, shared_rx, "SV", "SV")
  igv_server("igv", shared_store, shared_rx)
  dataServer("panel_app", shared_store, shared_rx, "panel_app", "panel_app")
  dataServer("phenotype", shared_store, shared_rx, "phenotype", "phenotype", 2000000)

  # Expose the current session's log to viewer as default selection
  log_viewer_server("log", logs_dir = logs_dir, session_logfile_reactive = shiny::reactive(session$userData$logfile))
  rawFilterServer("raw_filter", shared_store, shared_rx)
  customServer("custom_tab")
  qcPlotsServer("qc_plots", shared_store, shared_rx)
  aboutServer("about", shared_store, shared_rx)
  # log_debug("debug test")
  # log_info("info test")
  # log_warn("warning test")
  # log_error("error test")

  # Switch to IGV tab whenever an ID is clicked
  observeEvent(shared_rx$igv_version(), {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "IGV")
  }, ignoreInit = TRUE)

  # Switch to PanelApp tab whenever a Gene Symbol is clicked
  observeEvent(shared_rx$genesymbol_version(), {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "PanelApp")
  }, ignoreInit = TRUE)

  # Switch to Data tab whenever an HPO ID is clicked
  observeEvent(shared_rx$hpoid_version(), {
    updateTabsetPanel(session, inputId = "main_tabs", selected = "Phenotype")
  }, ignoreInit = TRUE)

}

shinyApp(ui, server, options = list(launch.browser=TRUE))
