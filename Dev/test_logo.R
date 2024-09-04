library(shiny)

print(getwd())

# Define UI for application
ui <- fluidPage(
  tags$head(
    # Add a custom CSS style for the header
    tags$style(HTML("
      .header {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 10px;
        border-bottom: 1px solid #D3D3D3;
        height: 150px; /* Adjust height as needed */
      }
      .header img {
        max-height: 120px; /* Adjust max height to make the image bigger */
      }
    "))
  ),
  # Create the header with the image
  div(class = "header",
      img(src = paste0("logo.png?v=", Sys.time()), alt = "Logo")  # Append query string for cache busting
  ),
  # Main content (optional)
  sidebarLayout(
    sidebarPanel(
      h3("Sidebar")
    ),
    mainPanel(
      h3("Main Content")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
}

# Run the application
shinyApp(ui = ui, server = server)
