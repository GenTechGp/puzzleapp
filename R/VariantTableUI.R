tabFileSavingUI <- function(ns, outdir) {
  sample_id <- unlist(strsplit(ns("sample"), "-"))[1]
  date <- format(Sys.time(), "%Y%m%d_%H%M")
  default_file <- paste0(sample_id, ".shinyApp.", date)
  fmts <- c("Excel (.xlsx)" = "excel", "Tab-separated values (.tsv)" = "tsv",
            "Tab-delimited text (.tab)" = "tab")
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

# Collapsible panel for ranking columns
tabRankUI <- function(ns) {
  ui <- uiOutput(ns("sortable_columns"))
  collapseUI("rank_collapse", "Re-order variables", "info", ui)
}

tabSelectUI <- function(ns) {
  ui <- uiOutput(ns("selected_vars_box"))
  collapseUI("select_collapse", "Select variables", "info", ui)
}

tabSidebarUI <- function(ns, outdir, show_file_saving) {
  if (show_file_saving == TRUE)
    save_ui <- tabFileSavingUI(ns, outdir)
  else
    save_ui <- NULL

  sidebarPanel(width = 2,
    save_ui,
    tabRankUI(ns),
    #uiOutput(ns("selected_vars_box"))
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

tabUI <- function(id, tab_label, outdir, show_file_saving) {
  ns <- NS(id)
  tabPanel(tab_label,
    sidebarLayout(
      tabSidebarUI(ns, outdir, show_file_saving),
      tabMainUI(ns)
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
    )
  )
}
