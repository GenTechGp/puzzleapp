
bam_files <- lapply(bam_files,function(x)sprintf("%s/%s",project_dir,x))

# load Shiny Modules
source(sprintf("%s/Dev/Modules/SelectFilterUI.dev.R",project_dir))
source(sprintf("%s/Dev/Modules/SelectFilterServer.dev.test.20240807.R",project_dir))
source(sprintf("%s/Dev/Modules/VariantTableUI.dev.20240826.R",project_dir))
source(sprintf("%s/Dev/Modules/VariantTableServer.dev.20240826.R",project_dir))
source(sprintf("%s/Dev/Modules/igvUI.dev.R",project_dir))
source(sprintf("%s/Dev/Modules/igvServer.dev.R",project_dir))
source(sprintf("%s/Dev/Modules/hpoTableUI.dev.R",project_dir))
source(sprintf("%s/Dev/Modules/hpoTableServer.dev.R",project_dir))
source(sprintf("%s/Dev/Modules/qcPlotsUI.dev.R",project_dir))
source(sprintf("%s/Dev/Modules/qcPlotsServer.dev.R",project_dir))

format_time <- function(time) {
  paste(round(time["elapsed"], 3), "seconds")
}

#sample_id <- "test"

set.seed(123)

#setwd('/g/data/kr68/andre/shinyApp/Dev')

print(getwd())

# Define UI
ui <- fluidPage(
  # Banner at the top
  # tags$div(
  #   style = "background-color: #FFFFFF; color: black; padding: 10px; text-align: center; font-size: 36px; height: 5vh",
  #   "JigSeq PuzzleApp"
  # ),
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
             selectFiltersUI(sprintf("%s-%s", sample, "tab1"), "VEP", panel_app_genes = panel_app_genes)
    ),
    tabPanel("Variants",
            tabUI(sprintf("%s-%s",sample,"tab2"), "VEP",show_file_saving = TRUE)
    ),
     tabPanel("IGV",
              igvUI(sprintf("%s-%s",sample,"tab3"), "hg38", chain_label = "(hg38 to chm13)")
    ),
    tabPanel("PanelApp",
            tabUI(sprintf("%s-%s",sample,"tab4"), "VEP",show_file_saving = FALSE),
    ),
    tabPanel("Phenotype",
             HPOtabUI(sprintf("%s-%s",sample,"tab5"), "VEP")
    ),
    # tabPanel("QC Plots",
    #        qcPlotsUI(sprintf("%s-%s",sample,"tab6"),"VEP")
    # )
  )
)
#  processed_data[sample(nrow(processed_data), 100000), ]
# Define server
server <- function(input, output, session) {
  select_filters_time <- system.time({
    filtered_data <- selectFiltersServer(id = sprintf("%s-%s", sample, "tab1"), dataset = processed_data, pedigree = pedigree_data, panel_app_genes = panel_app_genes, vep_consequences = vep_consequences, phenotype_data = phenotype_data)
  })
  cat(paste("selectFiltersServer time:", format_time(select_filters_time), "\n"))
  tabServer(id=sprintf("%s-%s",sample,"tab2"), filtered_data=filtered_data, vars = c("PRIORITY","NOTES",names(processed_data),"HPO_ID","HPO_COUNT","PANEL_APP","INHERITANCE","Color"),preselected_vars = c("PRIORITY","NOTES",preselected_vars,"Color"))
  igvServer(id=sprintf("%s-%s",sample,"tab3"), snps_vcf_file = sprintf("%s/%s",project_dir,snvs_vcf), svs_vcf_file = sprintf("%s/%s",project_dir,svs_vcf), bam_file = bam_files, assembly = "hg38", chain_file = chain_hg38_to_chm13, kinship = pedigree_data$kinship)
  panel_app_output <- reactiveVal(as.data.frame(panel_app))
  tabServer(id=sprintf("%s-%s",sample,"tab4"), filtered_data=panel_app_output, vars = names(panel_app),preselected_vars = panel_app_vars)
  HPOtabServer(id=sprintf("%s-%s",sample,"tab5"), phenotype_data = phenotype_data)
  # qcPlotsServer(id=sprintf("%s-%s",sample,"tab6"),coverage_data=coverage_data,snvs_processed_data=processed_data,pedigree_data=pedigree_data,somalier=somalier)
}


# Run the Shiny app
shinyApp(ui = ui, server = server, options = list(launch.browser=TRUE))
#runApp(shinyApp(ui = ui, server = server))
#runApp(shinyApp(ui = ui, server = server), port=9834)