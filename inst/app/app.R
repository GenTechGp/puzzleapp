library(shiny)

ui <- fluidPage(
  titlePanel("PuzzleApp"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number:", min = 1, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    hist(rnorm(input$n), main = "Random Histogram")
  })
}

shinyApp(ui, server)
