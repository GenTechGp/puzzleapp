set.seed(123)

# initializeSampleObjects <- function() {
#   objects_to_check <- c(
#     "sample", "processed_data", "pedigree_data", "panel_app_genes", "coverage_data",
#     "somalier", "vep_consequences", "panel_app", "panel_app_vars",
#     "snvs_vcf", "svs_vcf", "bam_files", "phenotype_data"
#   )
#   for (obj in objects_to_check) {
#     if (!exists(obj, envir = .GlobalEnv)) {
#       assign(obj, NULL, envir = .GlobalEnv)
#     }
#   }
# }
# 
# # Initialise variables if they don't exist
# initializeSampleObjects()

# Define UI
ui <- fluidPage(
  tags$head(
    # Add a custom CSS style for the header
    tags$style(HTML("
      .header {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 10px;
        border-bottom: 1px solid #D3D3D3;
        height: 150px; /* Adjust height as needed */
      }
      .header img {
        max-height: 120px; /* Adjust max height to make the image bigger */
      }
    "))
  ),
  
  # JavaScript to clear DataTables' local storage on first app load
  tags$script(HTML("
      $(document).on('shiny:connected', function() {
        if (!sessionStorage.getItem('appLoaded')) {
          Object.keys(localStorage).forEach(key => {
            if (key.includes('DataTables')) {
              localStorage.removeItem(key);
            }
          });
          sessionStorage.setItem('appLoaded', 'true'); // Prevents clearing on refresh
        }
      });
    ")),
  
  
  # Create the header with the image
  div(class = "header",
      img(src = paste0("logo.png?v=", Sys.time()), alt = "Logo")  # Append query string for cache busting
  ),
  use_busy_spinner(spin = "fading-circle", position = "top-right", color = "#0000FF"),
  tabsetPanel(
    id="tabs",
    tabPanel("Home", homeUI("tab0")),
    tabPanel("Filters", selectFiltersUI("tab1", panel_app_genes)),
    tabUI("tab2", "Variants"),
    igvUI("tab3", "IGV"),
    tabUI("tab4", "PanelApp"),
    tabUI("tab5", "Phenotype"),
    qcPlotsUI("tab6", "QC Plots")
  )
)

# Define server
server <- function(input, output, session) {
  
  # Log application start time and system info
  cat("\n--------------------------------------\n")
  cat(sprintf("PuzzleApp started\nDate & Time: %s\n", Sys.time()))
  cat(sprintf("Running on: %s | User: %s | R Version: %s\n",
              Sys.info()[["nodename"]], Sys.info()[["user"]], R.version.string))
  cat(sprintf("Working Directory: %s\n", getwd()))
  cat("--------------------------------------\n")
  
  cat("\n")
  # Check environment data on app startup
  data_status <- checkEnvironmentData()
  reload_trigger <- reactiveVal(NULL)
  processed_colnames <- reactiveVal(NULL)
  selected_igv_id <- reactiveVal(NULL)

  observe({
    if (exists("processed_data", envir = .GlobalEnv) && !is.null(processed_data)) {
      processed_colnames(colnames(processed_data))
    }
  })

  if (!data_status$success) {
    showNotification(
      paste("Missing data:", paste(data_status$missing, collapse = ", ")),
      type = "error"
    )

    # Disable all tabs except "Home"
    updateTabsetPanel(session, "tabs", selected = "Home")
    shinyjs::disable(selector = "#tabs li:not(:first-child)")
  } else {
    # Enable all tabs if data is available
    shinyjs::enable(selector = "#tabs li")
  }

  pref <- homeServer("tab0",reload_trigger,processed_colnames,pref)
  

  servers_initialised <- reactiveVal(FALSE)
  observe({
    req(pref$variants, pref$panelapp, pref$phenotype)
    req(data_status$success)
    vtabsel <- isolate(pref$variants)
    panel_app_vars <- isolate(pref$panelapp)
    phenotype_vars <- isolate(pref$phenotype)
    #print(pedigree_data)

  if (!servers_initialised()) {
  #if (data_status$success) {
    filtered_data <- selectFiltersServer("tab1",
                                         processed_data, pedigree_data,
                                         panel_app_genes, vep_consequences,
                                         phenotype_data, pref)
    exclude <- c("PRIORITY", "NOTES", "INHERITANCE", "PANEL_APP",
                 "HPO_ID", "HPO_COUNT", "spliceai_override",
                 "clinvar_override", "PRIORITYFlag")
    tabServer("tab2", filtered_data, vtabsel, pref,selected_igv_id)
    igvServer("tab3", processed_data, snvs_vcf, svs_vcf, bam_files, "hg38",
              pedigree_data$kinship,selected_igv_id)
    panel_app_output <- reactiveVal(as.data.frame(panel_app))
    exclude <- c("PRIORITY", "NOTES", "INHERITANCE", "PANEL_APP",
                 "HPO_ID", "HPO_COUNT", "spliceai_override",
                 "clinvar_override", "PRIORITYFlag")
    tabServer("tab4", panel_app_output, panel_app_vars, pref, NULL, exclude)
    phenotype_data_output <- reactiveVal(as.data.frame(phenotype_data))
    tabServer("tab5", phenotype_data_output, phenotype_vars, pref, NULL, exclude)
    qcPlotsServer("tab6", coverage_data, processed_data,
                  pedigree_data, somalier)
    servers_initialised(TRUE)
  }
  })

  # observeEvent(reload_trigger(), {
  #   selected_igv_id(NULL)
  #   vtabsel <- isolate(pref$variants)
  #   panel_app_vars <- isolate(pref$panelapp)
  #   phenotype_vars <- isolate(pref$phenotype)
  #   filtered_data <- selectFiltersServer("tab1",
  #                                        processed_data, pedigree_data,
  #                                        panel_app_genes, vep_consequences,
  #                                        phenotype_data, pref)
  #   exclude <- c("PRIORITY", "NOTES", "INHERITANCE", "PANEL_APP",
  #                "HPO_ID", "HPO_COUNT", "spliceai_override",
  #                "clinvar_override", "PRIORITYFlag")
  #   tabServer("tab2", filtered_data, vtabsel, pref, selected_igv_id)
  #   print(bam_files)
  #   igvServer("tab3", processed_data, snvs_vcf, svs_vcf, bam_files, "hg38",
  #             pedigree_data$kinship,selected_igv_id)
  #   panel_app_output <- reactiveVal(as.data.frame(panel_app))
  #   exclude <- c("PRIORITY", "NOTES", "INHERITANCE", "PANEL_APP",
  #                "HPO_ID", "HPO_COUNT", "spliceai_override",
  #                "clinvar_override", "PRIORITYFlag")
  #   tabServer("tab4", panel_app_output, panel_app_vars, pref, NULL, exclude)
  #   phenotype_data_output <- reactiveVal(as.data.frame(phenotype_data))
  #   tabServer("tab5", phenotype_data_output, phenotype_vars, pref, NULL, exclude)
  #   qcPlotsServer("tab6", coverage_data, processed_data,
  #                 pedigree_data, somalier)
  # })
  
  # # Switch to "Details Tab" when an ID is selected
  observeEvent(selected_igv_id(), {
    updateTabsetPanel(session, "tabs", selected = "IGV")
  })
  

}

# Run the Shiny app
shinyApp(ui, server, options = list(launch.browser=TRUE))
