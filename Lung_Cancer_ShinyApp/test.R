library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Local Image Viewer"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Local Image",
        width = 6,
        img(src = "s", width = "100%")
      )
    )
  )
)

server <- function(input, output, session) {
  # Your server logic (if any) goes here
}

shinyApp(ui, server)
