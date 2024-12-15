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
