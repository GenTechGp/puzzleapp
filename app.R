outdir <- Sys.getenv("OUTDIR")
if (outdir == "")
  outdir <- "."
tracks_dir <- sprintf("%s/tracks", outdir)
project_dir <- getwd()

set.seed(123)

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
    tabUI("tab2", "Variants", outdir, TRUE),
    igvUI("tab3", "IGV", pedigree_data$kinship),
    tabUI("tab4", "PanelApp", outdir, FALSE),
    tabUI("tab5", "Phenotype", outdir, FALSE),
    qcPlotsUI("tab6", "QC Plots")
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
    filtered_data <- selectFiltersServer("tab1",
                                         processed_data, pedigree_data,
                                         panel_app_genes, vep_consequences,
                                         phenotype_data)
    vtabsel <- c("PRIORITY", "NOTES","ID","CHROM","POS","GT_1") 
    tabServer("tab2", filtered_data, vtabsel,TRUE)
    igvServer("tab3", processed_data, snvs_vcf, svs_vcf, bam_files, "hg38",
              pedigree_data$kinship)
    panel_app_output <- reactiveVal(as.data.frame(panel_app))
    panel_app_vars <- c("Gene_Symbol","Sources","Level4","Level2","Model_Of_Inheritance")
    tabServer("tab4", panel_app_output, panel_app_vars, FALSE)
    phenotype_vars <- c("hpo_id","hpo_name","ncbi_gene_id","gene_symbol","disease_id")
    phenotype_data_output <- reactiveVal(as.data.frame(phenotype_data))
    tabServer("tab5", phenotype_data_output, phenotype_vars, FALSE)
    qcPlotsServer("tab6", coverage_data, processed_data,
                  pedigree_data, somalier)
  }
}

# Run the Shiny app
shinyApp(ui, server, options = list(launch.browser=TRUE))
