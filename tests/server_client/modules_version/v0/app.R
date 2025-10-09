library(shiny)
library(DT)

# Source modules
source("modules/home.R")
source("modules/data.R")

ui <- fluidPage(
  titlePanel("Home (build datasets) and Data (visualise + edit)"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home", homeUI("home")),
    tabPanel("Data", dataUI("data"))
  )
)

server <- function(input, output, session) {
  # Shared storage (plain variables) and reactive version token
  shared_store <- new.env(parent = emptyenv())  # holds $A and $B as plain data.frames
  shared_rx <- list(
    version = reactiveVal(0L)                   # bump when A/B are updated in Home
  )

  # Initialize/manage datasets in Home
  homeServer("home", shared_store, shared_rx)

  # Visualise/edit in Data (owns switching + slicing)
  dataServer("data", shared_store, shared_rx)
}

shinyApp(ui, server)