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
      menuItem("Graphs", tabName = "Graphs", icon = icon("th")),
      menuItem("Information Gain", tabName = "inf_gain", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              HTML('<h1 style="font-family: Times New Roman, sans-serif; font-weight: bold; color: #3c8dbc; text-align: center;"
                   >Welcome to Lung Cancer Dataset Dashborad</h1>'),
              
              fluidRow(div(
                style = "display: flex; justify-content: center; align-items: center; height: 36vh;",
                img(src = "https://github.com/nagar-mayank/Visualization-project/blob/main/Lung_Cancer_ShinyApp/www/IMG_0349-removebg-preview.png?raw=true",
                                      height = "250px", width = "350px"))),
              HTML('<p style="font-family: Times New Roman; font-size: 25px; margin-left: 20px;"
                   >Lung cancer continues to pose a significant global health challenge. While extensive research has delved
                   into the connection between smoking and lung cancer, the impact of other lifestyle factors, including
                   fatigue, chest pain, and alcohol consumption, on lung cancer risk has received comparatively less attention.
                   This dashboard highlights the substantial role played by these factors in influencing the risk of lung cancer.</p>'),
              
              HTML("<p style='font-family: Times New Roman; font-size: 25px; margin-left: 20px;'
                   >I've utilized the lung cancer dataset sourced from <a href='https://www.kaggle.com/datasets/mysarahmadbhat/lung-cancer'
                   target='_blank'>Kaggle</a>, calculating the information gain for each feature.
                   The analysis reveals that `ALLERGY` exhibits the highest information gain, while `SMOKING` demonstrates the 
                   least. This unexpected outcome is visually represented in the Graph section of the Dashboard, showcasing
                   information gain values for better insight.</p>"),
              ),
      
      tabItem(tabName = "Graphs",
              box(title = "Plot 1", status = "primary", solidHeader = TRUE,
                  fluidRow(column(4, selectInput("barplot_var", "Select a variable:", choices = res$column, selected = res$column[1]))),
                  fluidRow(column(8, plotOutput('var_barplot', height = "300px", width = "500px"))))
      ),
      tabItem(tabName = "inf_gain",
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
    list(src = "https://github.com/nagar-mayank/Visualization-project/blob/main/Lung_Cancer_ShinyApp/www/IMG_0349.JPG?raw=true", width = "600px", height = "300px")},deleteFile=F)
  
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
  
  output$var_barplot <- renderPlot({
    ggplot(res, aes(y=input$barplot_var)) + 
      geom_bar(stat="identity", fill='steelblue') + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(x = "Feature", y = "Frequency", title = 'Barplot of frequency of variable') + 
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
