outdir <- Sys.getenv("OUTDIR")
if (outdir == "")
  outdir <- "."
tracks_dir <- sprintf("%s/tracks", outdir)
project_dir <- getwd()

set.seed(123)

if (!is.null(panel_app)) {
  panel_app_ui <- names(panel_app)
} else {
  panel_app_ui <- c(
    "Entity_Name", "Entity_type", "Gene_Symbol",
    "Sources", "Level4", "Level3",
    "Level2", "Model_Of_Inheritance", "Phenotypes",
    "Omim", "Orphanet", "HPO",
    "Publications", "Description", "Flagged",
    "GEL_Status", "UserRatings_Green_amber_red", "version",
    "ready", "Mode_of_pathogenicity", "EnsemblId_GRch37",
    "EnsemblId_GRch38", "HGNC", "Position_Chromosome",
    "Position_GRCh37_Start", "Position_GRCh37_End", "Position_GRCh38_Start",
    "Position_GRCh38_End", "STR_Repeated_Sequence", "STR_Normal_Repeats",
    "STR_Pathogenic_Repeats", "Region_Haploinsufficiency_Score", "Region_Triplosensitivity_Score",
    "Region_Required_Overlap_Percentage", "Region_Variant_Type", "Region_Verbose_Name"
  )
}

vtabvars <- c(
  "PRIORITY", "NOTES", 
  if (exists("processed_data", envir = .GlobalEnv)) names(processed_data) else character(0), 
  "HPO_ID", "HPO_COUNT", "PANEL_APP", "INHERITANCE"
)

vtabsel <- c(
  "PRIORITY", "NOTES", 
  if (exists("preselected_vars", envir = .GlobalEnv)) preselected_vars else character(0)
)

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
    ")),
    # Add JavaScript handler to reload the page
    tags$script(HTML("
      Shiny.addCustomMessageHandler('reload', function(message) {
        location.reload();
      });
    "))
  ),
  # Create the header with the image
  div(class = "header",
      img(src = paste0("logo.png?v=", Sys.time()), alt = "Logo")  # Append query string for cache busting
  ),
  use_busy_spinner(spin = "fading-circle", position = "top-right", color = "#0000FF"),
  tabsetPanel(
    tabPanel("Home", homeUI("tab0")),
    tabPanel("Filters", selectFiltersUI("tab1", panel_app_genes)),
    tabUI("tab2", "Variants", outdir, TRUE, vtabvars, vtabsel),
    igvUI("tab3", "IGV", pedigree_data$kinship),
    tabUI("tab4", "PanelApp", outdir, FALSE, names(panel_app), panel_app_vars),
    tabUI("tab5", "Phenotype", outdir, FALSE, names(phenotype_data), names(phenotype_data)),
    qcPlotsUI("tab6", "QC Plots", !is.null(coverage_data),
              !is.null(somalier),
              !is.null(processed_data))
  )
)

# Define server
server <- function(input, output, session) {
  
  # Check environment data on app startup
  data_status <- checkEnvironmentData()
  
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
  
  homeServer("tab0")
  
  if (data_status$success) {
    print("inside")
    filtered_data <- selectFiltersServer("tab1",
                                         processed_data, pedigree_data,
                                         panel_app_genes, vep_consequences,
                                         phenotype_data)
    tabServer("tab2", filtered_data, vtabsel)
    igvServer("tab3", processed_data, snvs_vcf, svs_vcf, bam_files, "hg38",
              pedigree_data$kinship)
    panel_app_output <- reactiveVal(as.data.frame(panel_app))
    tabServer("tab4", panel_app_output, panel_app_vars)
    phenotype_data_output <- reactiveVal(as.data.frame(phenotype_data))
    tabServer("tab5", phenotype_data_output, names(phenotype_data))
    qcPlotsServer("tab6", coverage_data, processed_data,
                  pedigree_data, somalier)
  }
}

# Run the Shiny app
shinyApp(ui, server, options = list(launch.browser=TRUE))
