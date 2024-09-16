library(shiny)
library(shinyjs)

# Define the UI for the filter module
filterModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sidebar"),
    p("This is a minimal example of a Shiny app with a filter."),
    selectInput(ns("nameFilter"), "Select Name:", choices = c("All", "Alice", "Bob", "Charlie"), selected = "Alice"),
    actionButton(ns("applyFilter"), "Apply Filter")
  )
}

# Define the server logic for the filter module
filterModuleServer <- function(id, sample_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store filtered data
    filtered_data <- reactiveVal(sample_data)
    
    # Observe the applyFilter button
    observeEvent(input$applyFilter, {
      if (input$nameFilter == "All") {
        filtered_data(sample_data)
      } else {
        filtered_data(sample_data[sample_data$Name == input$nameFilter, ])
      }
    })
    
    # Trigger the apply_filter button click event when the app launches
    observe({
      shinyjs::click("applyFilter")
    })
    
    return(filtered_data)
  })
}

# Define the UI for the application
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Minimal Shiny App Example"),
  
  sidebarLayout(
    sidebarPanel(
      filterModuleUI("filterModule")
    ),
    
    mainPanel(
      h3("Table Output"),
      tableOutput("exampleTable")
    )
  )
)

# Define server logic for the application
server <- function(input, output) {
  # Create a sample data frame
  sample_data <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Age = c(25, 30, 35),
    Occupation = c("Engineer", "Doctor", "Artist")
  )
  
  # Call the filter module and get the filtered data
  filtered_data <- filterModuleServer("filterModule", sample_data)
  
  # Render the table
  output$exampleTable <- renderTable({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
