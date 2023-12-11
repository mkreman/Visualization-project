library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)


# df <- read.csv('./survey lung cancer.csv')
# load('./information_gain.RData')
# information_gain <- res
# library(stringr)
# information_gain <- information_gain %>%
#   +     mutate(column = str_replace_all(column, "\\.", " "))
# library(forcats)
# information_gain$column <- fct_reorder(information_gain$column, information_gain$information)
# df['GENDER'][df['GENDER']=='F'] = 'Female'
# df['GENDER'][df['GENDER']=='M'] = 'Male'
# df <- df %>% mutate_all(~ ifelse(. == 1, "NO", ifelse(. == 2, "YES", .)))
# save(df, information_gain, file='data.RData')

load('./data.RData')

# Function to plot bar graphs
bar.graph <- function(variable, a, b){
  if(variable=='GENDER'){
    data = c(sum(df['LUNG_CANCER']=='YES' & df[variable] == 'Male') / sum(df['LUNG_CANCER']=='YES'),
             sum(df['LUNG_CANCER']=='NO' & df[variable] == 'Male') / sum(df['LUNG_CANCER']=='NO'),
             sum(df['LUNG_CANCER']=='YES' & df[variable] == 'Female') / sum(df['LUNG_CANCER']=='YES'),
             sum(df['LUNG_CANCER']=='NO' & df[variable] == 'Female') / sum(df['LUNG_CANCER']=='NO'))
    al.freq = data.frame(Gender = rep(c(a, b), each=2),
                         variable=rep(c('Lung Cancer', 'No Lung cancer'), times=2),
                         Value=as.vector(data))
    p <- ggplot(al.freq, aes(x = variable , y = Value, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "", y = "Probabilities", title = '') +
      geom_text(aes(label=round(Value, 2)), vjust=-0.5, size=4, hjust=0.5,
                position=position_dodge(width = 0.9))
  }else{
    data = c(sum(df['LUNG_CANCER']=='YES' & df[variable] == 'YES') / sum(df['LUNG_CANCER']=='YES'),
             sum(df['LUNG_CANCER']=='NO' & df[variable] == 'YES') / sum(df['LUNG_CANCER']=='NO'),
             sum(df['LUNG_CANCER']=='YES' & df[variable] == 'NO') / sum(df['LUNG_CANCER']=='YES'),
             sum(df['LUNG_CANCER']=='NO' & df[variable] == 'NO') / sum(df['LUNG_CANCER']=='NO'))
    al.freq = data.frame(Symptom = rep(c(a, b), each=2),
                         variable=rep(c('Lung Cancer', 'No Lung cancer'), times=2),
                         Value=as.vector(data))
    p <- ggplot(al.freq, aes(x = variable , y = Value, fill = Symptom)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "", y = "Probabilities", title = '') +
      geom_text(aes(label=round(Value, 2)), vjust=-0.5, size=4, hjust=0.5,
                position=position_dodge(width = 0.9))
  }
  return(p)
}


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Lung Cancer Dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Analysis", tabName = "Graphs", icon = icon("th")),
      menuItem("Information Gain", tabName = "inf_gain", icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              HTML('<h1 style="font-family: Times New Roman, sans-serif; font-weight: bold; color: #3c8dbc; text-align: center;"
                   >Welcome to Lung Cancer Dataset Dashborad</h1>'),
              
              fluidRow(div(
                style = "display: flex; justify-content: center; align-items: center; height: 36vh;",
                img(src = "image1.png",
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
                   least. This unexpected outcome is visually represented in the Information Gain section of the Dashboard, showcasing
                   IG values for better insight.</p>")
              ),
      # Code for 'Graph' Page
      tabItem(tabName = "Graphs",
              HTML("<h1 style='font-family: Times New Roman; font-size: 25px; margin-left: 0px;font-weight: bold;color: #3c8dbc'
                   >Data Analysis of Lung Cancer Dataset
                   </h1>"),
              HTML("<p style='font-family: Times New Roman; font-size: 20px; margin-left: 0px;'
                   >The initial graph displays a boxplot depicting the distribution of ages concerning the selected variable.
                   The second graph illustrates the count of patients with or without the specified symptom.
                   </p>"),
              fluidRow(column(5, selectInput("graph_var", "Select a variable:", choices = information_gain$column, selected = information_gain$column[1]))),
              
              box(title = "Grouped boxplot of age given the variable", status = "primary", solidHeader = TRUE,
                  fluidRow(column(12, plotOutput('grouped_boxplot', width='100%')))),
              
              box(title = "Barplot showing distribution of the variable", status = "primary", solidHeader = TRUE,
                  fluidRow(column(12, plotOutput('barplot', width='100%')))),
              
              ),
      
      # Code for 'Information Gain' Page
      tabItem(tabName = "inf_gain",
              HTML("<h1 style='font-family: Times New Roman; font-size: 25px; margin-left: 0px;font-weight: bold;color: #3c8dbc'
                   >Information Gain and Observed Ratios 
                   </h1>"),
              HTML("<p style='font-family: Times New Roman; font-size: 20px; margin-left: 0px;'
                   >First graph show the information gain calculated for each variable and the second plot present a
                   barplot Illustrating the Ratio of Each Feature Given the Lung Cancer Status.
                   </p>"),
              box(title = "Information gain of each variable", status = "primary", solidHeader = TRUE,
                fluidRow(column(12, plotOutput('information_gain_plot', height = "400px", width = "100%")))),
              
              box(title = "Barplot of the ratio of lung cancer patients across variable categories", status = "primary", solidHeader = TRUE,
                fluidRow(column(5, selectInput("variable", "Select a variable:", choices = data.frame(column=information_gain$column)%>% filter(column != "AGE"), selected = information_gain$column[1]))),
                fluidRow(column(12, plotOutput('prob_barplot', height = "320px", width = "100%")))),
              column(12, HTML("<p style='font-family: Times New Roman; font-size: 20px; margin-left: 20px;'
                   >From the presented graph, it is evident that `Allergy` and `Alcohol Consumption` stand out
                   as more prominent features linked to lung cancer in comparison to Smoking and Shortness of Breath.
                   </p>")),
              )
      )
    )
  )


server <- function(input, output) {
  # Graphs for page Graphs
  output$grouped_boxplot <- renderPlot({
    ggplot(df, aes(y = AGE, color = !!sym(input$graph_var))) +
      geom_boxplot() + 
      labs(y = "Age", title = '') + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$barplot <- renderPlot({
    graph <- ggplot(df, aes(x = get(input$graph_var), fill=get(input$graph_var))) +
      geom_bar(stat='count') +
      labs(x = input$graph_var, y = "Count", title = '') +
      scale_fill_manual(name = input$graph_var, values = c('#f8766d', '#3c8dbc'))
    
    if(input$graph_var=='AGE'){graph}else{graph +
      geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=4, hjust=0.5,
                position=position_dodge(width = 0.9))}
    
  })
  
  # Graphs for page Information Gain
  output$information_gain_plot <- renderPlot({
    ggplot(information_gain, aes(x=column, y=information)) + 
      geom_bar(stat="identity", fill='steelblue') + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      labs(x = "Features", y = "Information Gain", title = '') + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$prob_barplot <- renderPlot({
    if(input$variable=='GENDER'){
      a='Male'
      b='Female'
    }else{
      a=input$variable
      b=paste('No', input$variable, sep=' ')
    }
    bar.graph(input$variable, a, b)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
