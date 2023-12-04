library(shiny)

ui <- fluidPage(
  tags$head(
    tags$script(
      HTML("
        $(document).ready(function(){
          $('#myTabLink').click(function(){
            $('#myTab a[href=\"#tab2\"]').tab('show');
          });
        });
      ")
    )
  ),
  titlePanel("Link to Tab Example"),
  sidebarLayout(
    sidebarPanel(
      HTML("<a href='#' id='myTabLink'>Go to Tab 2</a>")
    ),
    mainPanel(
      tabsetPanel(
        id = "myTab",
        tabPanel("Tab 1", "Content of Tab 1"),
        tabPanel("Tab 2", "Content of Tab 2")
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui, server)
