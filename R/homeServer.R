homeServer <- function(id,reload_trigger) {
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
        showNotification("Loading sample...", id = ns("notify_load"), type = "message")
        load(sample_path, envir = .GlobalEnv) # Adjust file name/path as needed


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
        reload_trigger(runif(1))
        removeNotification(ns("notify_load"))
        showNotification("Sample loaded successfully!", type = "message")
      }
    })

  })
}
