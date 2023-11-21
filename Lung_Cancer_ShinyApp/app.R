library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

load('./information_gain.RData')
df <- read.csv('./survey lung cancer.csv')
df['GENDER'][df['GENDER']=='F'] = '1'
df['GENDER'][df['GENDER']=='M'] = '2'


# Function to plot bar graphs
bar.graph <- function(variable, a, b, title){
  data = c(sum(df['LUNG_CANCER']=='YES' & df[variable] == '1') / sum(df['LUNG_CANCER']=='YES'),
           sum(df['LUNG_CANCER']=='NO' & df[variable] == '1') / sum(df['LUNG_CANCER']=='NO'),
           sum(df['LUNG_CANCER']=='YES' & df[variable] == '2') / sum(df['LUNG_CANCER']=='YES'),
           sum(df['LUNG_CANCER']=='NO' & df[variable] == '2') / sum(df['LUNG_CANCER']=='NO'))
  
  al.freq = data.frame(Symptom = rep(c(a, b), each=2),
                       variable=rep(c('Lung Cancer', 'No Lung cancer'), times=2),
                       Value=as.vector(data))
  
  p <- ggplot(al.freq, aes(x = variable , y = Value, fill = Symptom)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "", y = "Probabilities", title = title) +
    geom_text(aes(label=round(Value, 2)), vjust=-0.5, size=4, hjust=0.5,
              position=position_dodge(width = 0.9)) +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Lung Cancer Dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Graphs", tabName = "Graphs", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
#               box(fluidRow(column(12, h3('Lung cancer remains a global health concern. While numerous studies have been done on the association
# between smoking and lung cancer, the role of other lifestyle factors, such as fatigue, chest pain, and alcohol
# consumption, in lung cancer risk remains less explored.')))),
              fluidRow(column(5, imageOutput("img1"))),

              # fluidRow(column(12, plotOutput('age_dist')))
              ),
      
      tabItem(tabName = "Graphs",
              box(title = "Information Gain Plot", status = "primary", solidHeader = TRUE,
                fluidRow(column(12, plotOutput('information_grain_plot', height = "380px", width = "100%")))),
              box(title = "Plot 1", status = "primary", solidHeader = TRUE,
                fluidRow(column(4, selectInput("variable", "Select a variable:", choices = res$column, selected = res$column[1]))),
                fluidRow(column(8, plotOutput('prob_barplot', height = "300px", width = "500px"))))
              )
      )
    )
  )


server <- function(input, output) {
  
  output$img1 = renderImage({
    list(src = "www/IMG_3049.jpg", width = "600px", height = "300px")},deleteFile=F)
  
  output$age_dist <- renderPlot({
    ggplot(df, aes(x=AGE, color=LUNG_CANCER)) + 
      geom_histogram(fill='lightblue', aes(y=..density..), bins = 30) + 
      geom_density(alpha=0.0, fill='#FF0000') + 
      scale_x_continuous(breaks = seq(min(df$AGE)-1, max(df$AGE), by = 10)) + 
      labs(x = "Age", y = "Density", title = 'Stacked histogram of Age given the Lung cancer') + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$information_grain_plot <- renderPlot({
    ggplot(res, aes(x=column, y=information)) + 
      geom_bar(stat="identity", fill='steelblue') + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(x = "Features", y = "Information Gain", title = 'Barplot of Information Gain of features') + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$prob_barplot <- renderPlot({
    bar.graph(input$variable, paste('Non', input$variable, sep='-'), input$variable, title=paste('Barplot of Probabilities of', input$variable))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
