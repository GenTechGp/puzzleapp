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

# checkEnvironmentData <- function() {
#   required_objects <- c(
#     "sample", "processed_data", "pedigree_data", "panel_app_genes",
#     "coverage_data", "vep_consequences", 
#     "panel_app", "snvs_vcf", "svs_vcf", "bam_files",
#     "phenotype_data"
#   )
# 
#   # Check if objects exist and are not NULL
#   missing_objects <- required_objects[
#     sapply(required_objects, function(obj) is.null(get0(obj, envir = .GlobalEnv)))
#   ]
#   print(missing_objects)
# 
#   # Check if specific objects are data.frames or data.tables
#   invalid_objects <- c("processed_data", "panel_app_genes", "pedigree_data")
#   invalid_objects <- invalid_objects[
#     sapply(invalid_objects, function(obj) {
#       obj_val <- get0(obj, envir = .GlobalEnv)
#       !is.null(obj_val) && !inherits(obj_val, c("data.frame", "data.table"))
#     })
#   ]
#   print(invalid_objects)
# 
#   if (length(missing_objects) > 0 || length(invalid_objects) > 0) {
#     return(list(success = FALSE, missing = missing_objects))
#   } else {
#     return(list(success = TRUE, missing = NULL))
#   }
# }

checkEnvironmentData <- function() {
  required_objects <- c(
    "sample", "processed_data", "pedigree_data", "panel_app_genes",
    "coverage_data", "vep_consequences", 
    "panel_app", "snvs_vcf", "svs_vcf", "bam_files",
    "phenotype_data"
  )
  
  # Check if objects exist in .GlobalEnv and are not NULL
  missing_objects <- required_objects[
    !sapply(required_objects, function(obj) exists(obj, envir = .GlobalEnv) && !is.null(get(obj, envir = .GlobalEnv)))
  ]
  #print(missing_objects)
  
  # Check if specific objects are data.frames or data.tables
  invalid_objects <- c("processed_data", "panel_app_genes", "pedigree_data")
  invalid_objects <- invalid_objects[
    sapply(invalid_objects, function(obj) {
      if (!exists(obj, envir = .GlobalEnv)) return(TRUE)  # If missing, consider it invalid
      obj_val <- get(obj, envir = .GlobalEnv)
      !inherits(obj_val, c("data.frame", "data.table"))
    })
  ]
  #print(invalid_objects)
  
  # Return validation results
  if (length(missing_objects) > 0 || length(invalid_objects) > 0) {
    return(list(success = FALSE, missing = missing_objects, invalid = invalid_objects))
  } else {
    return(list(success = TRUE, missing = NULL, invalid = NULL))
  }
}