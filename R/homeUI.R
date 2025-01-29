homeUI <- function(id) {
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
    )
  )
}
