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
    actionButton(ns("save_file"), "save"),
    hr()
  )
}

tabSelectUI <- function(ns) {
  ui <- uiOutput(ns("dynamic_select_vars"))
  collapseUI("select_collapse", "Select variables", "info", ui)
}

tabSidebarUI <- function(ns, outdir, show_file_saving, vars, selected) {
  if (show_file_saving == TRUE)
    save_ui <- tabFileSavingUI(ns, outdir)
  else
    save_ui <- NULL

  sidebarPanel(width = 2,
    save_ui,
    tabSelectUI(ns)
  )
}

tabMainUI <- function(ns) {
  mainPanel(width = 10,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "datatable.css")
    ),
    DT::dataTableOutput(ns("table"))
  )
}

tabUI <- function(id, tab_label, outdir, show_file_saving, vars, selected) {
  ns <- NS(id)
  tabPanel(tab_label,
    sidebarLayout(
      tabSidebarUI(ns, outdir, show_file_saving, vars, selected),
      tabMainUI(ns)
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
    )
  )
}
