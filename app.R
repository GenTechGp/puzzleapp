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
      tabUI(sprintf("%s-tab2",sample), "Variants", outdir, show_file_saving = TRUE),
      igvUI(sprintf("%s-tab3",sample), "IGV", chain_label = "(hg38 to chm13)"),
      tabUI(sprintf("%s-tab4",sample), "PanelApp", outdir, show_file_saving = FALSE),
      HPOtabUI(sprintf("%s-tab5",sample), "Phenotype"),
      qcPlotsUI(sprintf("%s-tab6",sample),"QC Plots",coverage_analysis=ifelse(!is.null(coverage_data),TRUE,FALSE),somalier_analysis=ifelse(!is.null(somalier),TRUE,FALSE),vaf_distribution_analysis=ifelse(nrow(processed_data[CATEGORY=="SNV & Indel"])==0,TRUE,FALSE))
  )
)

# Define server
server <- function(input, output, session) {
  filtered_data <- reactiveVal(NULL)
  filtered_data <- selectFiltersServer(id = sprintf("%s-tab1", sample), dataset = processed_data, pedigree = pedigree_data, panel_app_genes = panel_app_genes, vep_consequences = vep_consequences, phenotype_data = phenotype_data)
  tabServer(id=sprintf("%s-tab2",sample), filtered_data=filtered_data, vars = c("PRIORITY","NOTES",names(processed_data),"HPO_ID","HPO_COUNT","PANEL_APP","INHERITANCE","Color"),preselected_vars = c("PRIORITY","NOTES",preselected_vars,"Color"))
  igvServer(id=sprintf("%s-tab3",sample), snps_vcf_file = snvs_vcf, svs_vcf_file = svs_vcf, bam_file = bam_files, assembly = "hg38", kinship = pedigree_data$kinship)
  panel_app_output <- reactiveVal(as.data.frame(panel_app))
  tabServer(id=sprintf("%s-tab4",sample), filtered_data=panel_app_output, vars = names(panel_app),preselected_vars = panel_app_vars)
  HPOtabServer(id=sprintf("%s-tab5",sample), phenotype_data = phenotype_data)
  qcPlotsServer(id=sprintf("%s-tab6",sample),coverage_data=coverage_data,snvs_processed_data=processed_data,pedigree_data=pedigree_data,somalier=somalier)
}

# Run the Shiny app
shinyApp(ui, server, options = list(launch.browser=TRUE))
