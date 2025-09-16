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
  # Shared storage (plain environment) and reactive version token
  # - shared_store: plain per-session environment holding datasets and preferences
  # - shared_rx: reactive flags/counters for cross-module signaling
  shared_store <- new.env(parent = emptyenv())
  shared_store$data_for_data  <- list()
  shared_store$preferred_cols <- character(0)

  shared_rx <- list(
    version = reactiveVal(0L)
  )

  homeServer("home", shared_store, shared_rx)
  dataServer("data", shared_store, shared_rx)
}

shinyApp(ui, server)