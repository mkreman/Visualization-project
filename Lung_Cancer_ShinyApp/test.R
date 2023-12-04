library(ggplot2)
library(dplyr) 


df = read.csv('C:/Users/MkReman/Gdrive/Goodnote/CMI Data Science/Semester - 1/Visualization/Visualization-project/Lung_Cancer_ShinyApp/survey lung cancer.csv')

col = 'SMOKING'

count.data <- data.frame( 
  pilot_class = c(col, paste('Non-', col, sep='')), 
  n = c(length(df1[col][df1[col] == 'YES']), length(df1[col][df1[col] == 'NO'])),
  proportion = c(length(df1[col][df1[col] == 'YES'])/(nrow(df1)),
                 length(df1[col][df1[col] == 'NO'])/(nrow(df1))) 
) 

count.data <- count.data %>% 
  arrange(desc(pilot_class)) %>% 
  mutate(lab.ypos = cumsum(proportion) - 0.6*proportion) 

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

#############################
df1 <- df %>% mutate_all(~ ifelse(. == 1, "NO", ifelse(. == 2, "YES", .)))

col = 'ALCOHOL.CONSUMING'
ggplot(df1, aes(y = AGE, color=ALCOHOL.CONSUMING)) +
  geom_boxplot() + 
  labs(x = "Gender of Individual", y = "Age",
       title = 'Grouped Boxplot of Age') + 
  theme(plot.title = element_text(hjust = 0.5))
