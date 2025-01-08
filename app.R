outdir <- Sys.getenv("OUTDIR")
if (outdir == "")
	outdir <- "."
tracks_dir <- sprintf("%s/tracks", outdir)
project_dir <- getwd()

set.seed(123)

vtabvars <- c("PRIORITY", "NOTES", names(processed_data), "HPO_ID", "HPO_COUNT",
              "PANEL_APP", "INHERITANCE", "Color")
vtabsel <- c("PRIORITY", "NOTES", preselected_vars, "Color")

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
  # Create the header with the image
  div(class = "header",
      img(src = paste0("logo.png?v=", Sys.time()), alt = "Logo")  # Append query string for cache busting
  ),
  use_busy_spinner(spin = "fading-circle", position = "top-right", color = "#0000FF"),
  tabsetPanel(
    tabPanel("Home",
      selectFiltersUI(sprintf("%s-tab1", sample), panel_app_genes)
    ),
    tabUI(sprintf("%s-tab2", sample), "Variants", outdir, TRUE, vtabvars,
          vtabsel),
    igvUI(sprintf("%s-tab3", sample), "IGV", pedigree_data$kinship),
    tabUI(sprintf("%s-tab4", sample), "PanelApp", outdir, FALSE,
          names(panel_app), panel_app_vars),
    hpoTabUI(sprintf("%s-tab5", sample), "Phenotype"),
    qcPlotsUI(sprintf("%s-tab6", sample), "QC Plots", !is.null(coverage_data),
              !is.null(somalier),
              nrow(processed_data[CATEGORY=="SNV & Indel"]) == 0)
  )
)

# Define server
server <- function(input, output, session) {
  filtered_data <- selectFiltersServer(sprintf("%s-tab1", sample),
                                       processed_data, pedigree_data,
                                       panel_app_genes, vep_consequences,
                                       phenotype_data)
  tabServer(sprintf("%s-tab2", sample), filtered_data, vtabsel)
  igvServer(sprintf("%s-tab3", sample), processed_data, snvs_vcf, svs_vcf, bam_files, "hg38",
            pedigree_data$kinship)
  panel_app_output <- reactiveVal(as.data.frame(panel_app))
  tabServer(sprintf("%s-tab4", sample), panel_app_output, panel_app_vars)
  hpoTabServer(sprintf("%s-tab5", sample), phenotype_data)
  qcPlotsServer(sprintf("%s-tab6", sample), coverage_data, processed_data,
                pedigree_data, somalier)
}

# Run the Shiny app
shinyApp(ui, server, options = list(launch.browser=TRUE))
