tabUI <- function(id,tab_label,vars, preselected_vars = character(0),show_file_saving=FALSE) {
  ns <- NS(id)
  tabPanel(tab_label,
           #use_busy_spinner(spin = "fading-circle", position = "top-right", color = "#0000FF",spin_id = ns("spin_table_rendering")),
           sidebarLayout(
             sidebarPanel(width = 2,
                          conditionalPanel(
                            condition = ifelse(show_file_saving, "true", "false"),
                            textInput(ns("output_dir"), "Output directory:", value = outdir),
                            selectInput(ns("filetype"), "File format:",
                                        choices = c("Excel (.xlsx)" = "excel",
                                                    "Tab-separated values (.tsv)" = "tsv",
                                                    "Tab-delimited text (.tab)" = "tab"),
                                        selected = "tsv"),
                            selectInput(ns("out_scope"), "Scope:",
                                        choices = c("All variables" = "all",
                                                    "Selected variables only" = "selected_only"),
                                        selected = "all"),
                            textInput(ns("out_filename"), "File name prefix:", value = paste0(unlist(strsplit(sample,"-"))[1],".shinyApp.",format(Sys.time(), "%Y%m%d_%H%M"))),
                            actionButton(ns("save_file"), "save"),
                            hr()
                          ),
                          # Collapsible panel for ranking columns
                          bsCollapse(
                            id = "rank_collapse", open = "Re-order variables",
                            bsCollapsePanel(
                              title = "Re-order variables",
                              uiOutput(ns("sortable_columns")),
                              style = "info"
                            )
                          ),
                          #uiOutput(ns("selected_vars_box"))
                          bsCollapse(
                            id = "select_collapse", open = "Select variables",
                            bsCollapsePanel("Select variables", 
                                            uiOutput(ns("selected_vars_box")),
                                            style = "info")
                          )
             ),
             mainPanel( width = 10,
                        tags$head(
                          tags$style(HTML("
          .dataTables_wrapper .dataTables_scrollBody table {
            width: 100%;
          }
          table.dataTable td {
            max-width: 20vw;
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
          }
        "))
                        ),
                        DT::dataTableOutput(ns("table")),
             )
           ),
           tags$head(
             tags$style(HTML("
                            .well {
                            background-color: #F4F4F4;
                            border: 1px solid #337ab7; /* Primary color border */
                            }
                            .tab-content {
                            background-color: white;
                            }
                            hr {border-top: 1.75px solid #D3D3D3;}
                             "))
           )
  )
}
