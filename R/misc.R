format_time <- function(time) {
  paste(round(time["elapsed"], 3), "seconds")
}

collapseUI <- function(id, title, style, ...) {
  box_id <- paste0(id, "_box")
  bsCollapse(open = box_id, multiple = TRUE,
    bsCollapsePanel(title = title, value = box_id, style = style, ...)
  )
}

capitalize_word <- function(word) {
  paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
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
