tabFileSavingUI <- function(ns, outdir) {
  sample_id <- unlist(strsplit(ns("sample"), "-"))[1]
  date <- format(Sys.time(), "%Y%m%d_%H%M")
  default_file <- paste0(sample_id, ".shinyApp.", date)
  fmts <- c("Excel (.xlsx)" = "excel", "Tab-separated values (.tsv)" = "tsv",
            "Comma-separated values (.csv)" = "csv")
  scopes <- c("All variables" = "all",
              "Selected variables only" = "selected_only")

  fluidPage(
    textInput(ns("output_dir"), "Output directory:", value = outdir),
    selectInput(ns("filetype"), "File format:", fmts, "tsv"),
    selectInput(ns("out_scope"), "Scope:", scopes, "all"),
    textInput(ns("out_filename"), "File name prefix:", value = default_file),
    actionButton(ns("save_file"), "save")
  )
}

tabMainUI <- function(ns) {
  div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "datatable.css")
    ),
    DT::dataTableOutput(ns("table"))
  )
}

tabPanelUI <- function(ns, outdir, show_file_saving) {
  if (show_file_saving == TRUE) {
    sidebarLayout(
      sidebarPanel(width = 2, tabFileSavingUI(ns, outdir)),
      mainPanel(width = 10, tabMainUI(ns))
    )
  } else {
    tabMainUI(ns)
  }
}

tabUI <- function(id, tab_label, outdir, show_file_saving) {
  ns <- NS(id)
  tabPanel(tab_label,
    tabPanelUI(ns, outdir, show_file_saving),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
    )
  )
}
