outdir <- Sys.getenv("OUTDIR")
if (outdir == "")
  outdir <- "."
tracks_dir <- sprintf("%s/tracks", outdir)
project_dir <- getwd()

set.seed(123)

#vtabvars <- c("PRIORITY", "NOTES", names(processed_data), "HPO_ID", "HPO_COUNT",
#              "PANEL_APP", "INHERITANCE")
#vtabsel <- c("PRIORITY", "NOTES", preselected_vars)

# initializeSampleObjects <- function() {
#   objects_to_check <- c(
#     "sample", "processed_data", "pedigree_data", "panel_app_genes", "coverage_data",
#     "somalier", "vep_consequences", "preselected_vars", "panel_app", "panel_app_vars",
#     "snvs_vcf", "svs_vcf", "bam_files", "phenotype_data"
#   )
#   for (obj in objects_to_check) {
#     if (!exists(obj, envir = .GlobalEnv)) {
#       assign(obj, NULL, envir = .GlobalEnv)
#     }
#   }
# }
# 
# # Example usage
# initializeSampleObjects()

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


HomeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Welcome to JigSeq PuzzleApp!"),
    p("This application allows you to explore and analyse genomic data. Start by loading a new sample or using the navigation tabs above."),
    br(),
    fluidRow(
      textInput(ns("sample_path"), "Enter Sample Path:", placeholder = "Path to your sample folder")
    ),
    fluidRow(
      actionButton(ns("load_sample"), "Load Sample", class = "btn-primary")
    ),
    br(),
    br(),
    uiOutput(ns("session_info"))
  )
}

HomeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Helper function to clear existing objects
    clearSampleObjects <- function() {
      objects_to_clear <- c(
        "sample", "processed_data", "pedigree_data", "panel_app_genes", "coverage_data",
        "somalier", "vep_consequences", "preselected_vars", "panel_app", "panel_app_vars",
        "snvs_vcf", "svs_vcf", "bam_files", "phenotype_data"
      )
      for (obj in objects_to_clear) {
        if (exists(obj, envir = .GlobalEnv)) {
          rm(list = obj, envir = .GlobalEnv)
        }
      }
    }
    
    # Observe sample path submission
    observeEvent(input$load_sample, {
      sample_path <- input$sample_path
      print("here")
      
      if (!file.exists(sample_path)) {
        showNotification("Invalid path. Please check the directory and try again.", type = "error")
      } else {
        # Clear existing objects
        clearSampleObjects()
        
        # Load the new sample
        showNotification("Loading sample...", type = "message")
        load(sample_path, envir = .GlobalEnv) # Adjust file name/path as needed
        showNotification("Sample loaded successfully!", type = "message")
        
        vtabvars <- c(
          "PRIORITY", "NOTES", 
          if (exists("processed_data", envir = .GlobalEnv)) names(processed_data) else character(0), 
          "HPO_ID", "HPO_COUNT", "PANEL_APP", "INHERITANCE"
        )
        print(vtabvars)
        
        vtabsel <- c(
          "PRIORITY", "NOTES", 
          if (exists("preselected_vars", envir = .GlobalEnv)) preselected_vars else character(0)
        )
        
        # Additional logic for processing the sample can be added here
        print(paste("Sample loaded from:", sample_path))
        
        # Trigger a full app reload
        session$sendCustomMessage("reload", list())
      }
    })
    
  })
}

checkEnvironmentData <- function() {
  required_objects <- c(
    "sample", "processed_data", "pedigree_data", "panel_app_genes",
    "coverage_data", "vep_consequences", "preselected_vars",
    "panel_app", "panel_app_vars", "snvs_vcf", "svs_vcf", "bam_files",
    "phenotype_data"
  )
  
  # Check if objects exist and are not NULL
  missing_objects <- required_objects[
    sapply(required_objects, function(obj) is.null(get0(obj, envir = .GlobalEnv)))
  ]
  
  if (length(missing_objects) > 0) {
    return(list(success = FALSE, missing = missing_objects))
  } else {
    return(list(success = TRUE, missing = NULL))
  }
}

tab_id <- "test"

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
    tabPanel("Home",
             HomeUI(sprintf("%s-tab0", tab_id))
    ),
    tabPanel("Filters",
             selectFiltersUI(sprintf("%s-tab1", tab_id), panel_app_genes)
    ),
    tabUI(sprintf("%s-tab2", tab_id), "Variants", outdir, TRUE,vtabvars, vtabsel),
    igvUI(sprintf("%s-tab3", tab_id), "IGV", pedigree_data$kinship),
    tabUI(sprintf("%s-tab4", tab_id), "PanelApp", outdir, FALSE,names(panel_app), panel_app_vars),
    tabUI(sprintf("%s-tab5", tab_id), "Phenotype", outdir, FALSE,names(phenotype_data), names(phenotype_data)),
    qcPlotsUI(sprintf("%s-tab6", tab_id), "QC Plots", !is.null(coverage_data),
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
  
  HomeServer(sprintf("%s-tab0", tab_id))
  
  if (data_status$success) {
    print("inside")
    filtered_data <- selectFiltersServer(sprintf("%s-tab1", tab_id),
                                         processed_data, pedigree_data,
                                         panel_app_genes, vep_consequences,
                                         phenotype_data)
    tabServer(sprintf("%s-tab2", tab_id), filtered_data, vtabsel)
    igvServer(sprintf("%s-tab3", tab_id), processed_data, snvs_vcf, svs_vcf, bam_files, "hg38",
              pedigree_data$kinship)
    panel_app_output <- reactiveVal(as.data.frame(panel_app))
    tabServer(sprintf("%s-tab4", tab_id), panel_app_output, panel_app_vars)
    phenotype_data_output <- reactiveVal(as.data.frame(phenotype_data))
    tabServer(sprintf("%s-tab5", tab_id), phenotype_data_output, names(phenotype_data))
    qcPlotsServer(sprintf("%s-tab6", tab_id), coverage_data, processed_data,
                  pedigree_data, somalier)
  }
}

# Run the Shiny app
shinyApp(ui, server, options = list(launch.browser=TRUE))