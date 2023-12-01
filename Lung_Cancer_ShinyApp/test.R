# app.R

# Load necessary libraries
library(shiny)

# Shiny app UI
ui <- fluidPage(
  # Use HTML tags to display the image
  HTML('<img src="www/IMG_0349.png" width="100%" height="auto" />')
)

# Shiny app server (placeholder server function)
server <- function(input, output) {
  # Your server logic goes here
}

# Run the Shiny app
shinyApp(ui, server)
