library(ggplot2)

df = read.csv('C:/Users/MkReman/Gdrive/Goodnote/CMI Data Science/Semester - 1/Visualization/Visualization-project/Lung_Cancer_ShinyApp/survey lung cancer.csv')

table(df[['SMOKING']])

ggplot(df, aes(y='SMOKING')) + 
  geom_bar(stat="identity", fill='steelblue') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Feature", y = "Frequency", title = 'Barplot of frequency of variable') + 
  theme(plot.title = element_text(hjust = 0.5))


col = 'SMOKING'
length(df[col][df['LUNG_CANCER'] == 'YES'])
length(df[col][df['LUNG_CANCER'] == 'NO'])

library(ggplot2) 
library(dplyr) 


count.data <- data.frame( 
  pilot_class = c(col, paste('Non-', col)), 
  n = c(length(df[col][df['LUNG_CANCER'] == 'YES']), length(df[col][df['LUNG_CANCER'] == 'NO'])), 
  proportion = c(length(df[col][df['LUNG_CANCER'] == 'YES'])/(length(df[col][df['LUNG_CANCER'] == 'YES'])+length(df[col][df['LUNG_CANCER'] == 'NO'])),
                 length(df[col][df['LUNG_CANCER'] == 'NO'])/(length(df[col][df['LUNG_CANCER'] == 'YES'])+length(df[col][df['LUNG_CANCER'] == 'NO']))) 
) 

count.data 

count.data <- count.data %>% 
  arrange(desc(pilot_class)) %>% 
  mutate(lab.ypos = cumsum(proportion) - 0.6*proportion) 

count.data 

ggplot(count.data, aes(x = "", y = proportion, fill = pilot_class)) + 
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar("y", start = 0)+ 
  geom_text(aes(y = lab.ypos, label = round(proportion,2)), color = "black")+ 
  theme_void()

#####################
# Install and load the ggplot2 package
# install.packages("ggplot2")
library(ggplot2)

# Create a sample data frame
data <- data.frame(
  category = c(col, paste('Non-', col, sep='')),
  value = c(length(df[col][df['LUNG_CANCER'] == 'YES']), length(df[col][df['LUNG_CANCER'] == 'NO'])) 
)

# Create a pie chart using ggplot2
ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  theme_minimal() +
  ggtitle("Pie Chart Example")

