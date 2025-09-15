# app.R
library(shiny)
library(DT)
library(data.table)

source("modules/home.R")
source("modules/data.R")

ui <- fluidPage(
  titlePanel("Multi-dataset DT editor"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home", homeUI("home")),
    tabPanel("Data", dataUI("data"))
  )
)

server <- function(input, output, session) {
  # Shared stores
  # - shared_store: holds datasets and preferences (a reactiveValues list-like container)
  # - shared_rx: holds reactive flags/counters for cross-module signaling
  shared_store <- reactiveValues(
    data_for_data = list(),
    preferred_cols = character(0)
  )
  shared_rx <- list(
    version = reactiveVal(0L)
  )

  homeServer("home", shared_store, shared_rx)
  dataServer("data", shared_store, shared_rx)
}

shinyApp(ui, server)