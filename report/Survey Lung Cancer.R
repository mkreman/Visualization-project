library(ggplot2)


# install.packages('tidyverse')
df = read.csv('./db/survey lung cancer.csv')

head(df)
summary(df)

# Numbers of elements of the data-frame are NAN
sum(is.na(df))

# Duplicated values
dim(df[duplicated(df),])

dim(df)

# Encode the data set into categorical form
for(col in colnames(df)){
  if (!(col %in% c("GENDER", "AGE", "LUNG_CANCER"))){
    df[col][df[col] == "1"] = 0
    df[col][df[col] == "2"] = 1
  }
}

head(df)

# Histogram of age
df[df['GENDER']=='F', ]

ggplot(df[df['GENDER']=='F',], aes(x=AGE)) + 
  geom_histogram(fill='lightblue', aes(y=..density..)) + 
  geom_density(alpha=0.0, fill='#FF0000')

df[df['GENDER']=='M',] -> male_data

# df = transform(male_data['GENDER'], GENDER = as.numeric(GENDER))

hist()

ggplot(df, aes(x=AGE, color=LUNG_CANCER)) + 
  geom_histogram(fill='lightblue', aes(y=..density..)) + 
  geom_density(alpha=0.0, fill='#FF0000')

ggplot(df[df['GENDER']=='M',], aes(x=AGE, color=LUNG_CANCER)) + 
  geom_histogram(fill='lightblue', aes(y=..density..)) + 
  geom_density(alpha=0.2, fill='#FF0000')

ggplot(df[df['GENDER']=='F',], aes(x=AGE, color=LUNG_CANCER)) + 
  geom_histogram(fill='lightblue', aes(y=..density..)) + 
  geom_density(alpha=0.2, fill='#FF0000')

ggplot(data=df, aes(x=AGE)) + 
  geom_boxplot(fill='lightblue') + 
  facet_grid('GENDER') 

# Box plot of age
boxplot(df['AGE'][df['LUNG_CANCER']=="YES"], df['AGE'][df['LUNG_CANCER']=="NO"],
        col=c('green', 'red'),
        names=c('Patient with cancer', 'Patient without cancer'),
        main='Boxplot of age with respect to CANCER',
        ylab='AGE')

df_male <- df[df['GENDER']=='M',]
boxplot(df_male['AGE'][df_male['LUNG_CANCER']=="YES"],
        df_male['AGE'][df_male['LUNG_CANCER']=="NO"],
        col=c('green', 'red'),
        names=c('Patient with cancer', 'Patient without cancer'),
        main='Boxplot of age with respect to CANCER',
        ylab='AGE')

df_female <- df[df['GENDER']=="F",]
boxplot(df_male['AGE'][df_male['LUNG_CANCER']=="YES"],
        df_male['AGE'][df_male['LUNG_CANCER']=="NO"],
        df_female['AGE'][df_female['LUNG_CANCER']=="YES"],
        df_female['AGE'][df_female['LUNG_CANCER']=="NO"],
        col=c('green', 'red', 'yellow', 'black'),
        names=c('Patient with cancer', 'Patient without cancer', 'y', 'b'),
        main='Boxplot of age with respect to CANCER',
        ylab='AGE')


ggplot(df, aes(x = GENDER, y = AGE, color=LUNG_CANCER)) +
  geom_boxplot()


# AGE distribution respect to lung_cancer
ggplot(df, aes(x = LUNG_CANCER, y = AGE)) +
  geom_bin_2d()

ggplot(df, aes(x = LUNG_CANCER, y = SMOKING)) +
  geom_bar(stat="identity")

ggplot(data = df, mapping = aes(x = LUNG_CANCER, y = SMOKING)) + 
  geom_col() + 
  facet_grid(YELLOW_FINGERS~WHEEZING)


################################################################################
# Convert the datatype of columns into numeric
for(col in colnames(subset(df, select = -c(GENDER, LUNG_CANCER)))){
  df[,col] = as.numeric(df[,col])
}

################################################################################
# Calculating information Gain
# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)

entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}

IG_cat<-function(data,feature,target){
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),] 
  #use dplyr to compute e and p for each value of the feature
  dd_data <- data %>% group_by_at(feature) %>% summarise(e=entropy(get(target)), 
                                                         n=length(get(target)))
  
  #compute entropy for the parent
  e0<-entropy(data[,target])
  #calculate p for each value of feature
  dd_data$p<-dd_data$n/nrow(data)
  
  hfi = entropy(data[,feature])
  
  #compute IG
  IG <- (e0-sum(dd_data$p*dd_data$e))/hfi
  
  return(IG)
}

IG_cat(df, 'ALLERGY', 'LUNG_CANCER')

IGS = data.frame(column = colnames(df)[-16], information=c(1:15))
i = 1
for(col in colnames(df)){
  if(col != 'LUNG_CANCER'){
    IGS$information[i] = IG_cat(df, col, 'LUNG_CANCER')
    cat(col, IG_cat(df, col, 'LUNG_CANCER'), '\n')
  }
  i <- i + 1
}

IGS

IGS$column <- factor(IGS$column, levels = IGS$column[order(IGS$information, decreasing = TRUE)])

res = information_gain[order(information_gain$information,decreasing = TRUE),]
rownames(res) = c(1:15)
res

ggplot(data=IGS, aes(x=column, y=information)) + 
  geom_bar(stat="identity", fill='steelblue') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################################################################
library("ggpubr")

f1 = ggplot(df, aes(x = LUNG_CANCER, y = ALLERGY)) +
  geom_bar(stat="identity")

f2 = ggplot(df, aes(x = LUNG_CANCER, y = ALCOHOL.CONSUMING)) +
  geom_bar(stat="identity")

f3 = ggplot(df, aes(x = LUNG_CANCER, y = SWALLOWING.DIFFICULTY)) +
  geom_bar(stat="identity")

figures <- ggarrange(f1,f2,f3, ncol=2)
figures

################################################################################
# install.packages("plotly")
library(plotly)
library(tidyr)

df_1 = subset(df, select = -c(AGE, GENDER, LUNG_CANCER))

df2 = df_1 %>%
  pivot_longer(SMOKING:FATIGUE, names_to = "Variables", values_to = "response")

#  table %>%
#  ggplot(aes(x=response, y = Variables)) +
#  geom_bar(stat='identity') +
#  facet_wrap(vars(Variables), ncol=3) +
#  labs(x = "Variables", y = "Counts")

 ############################################################################

library('DescTools')
Gini(df$AGE)


library('mltools')
gini_impurities(as.vector(df$AGE))

a = as.vector(df$GENDER)
gini_impurities(a)



a =c("red", "red", "blue", "green")
################################################################################
# Create a bar chart
ggplot(df, aes(x = factor(LUNG_CANCER), fill = factor(ALLERGY))) +
  geom_bar() +
  labs(
    x = "Cancer",
    y = "Count",
  ) +
  ggtitle("Count of Cars by Cylinders and Gears") +
  scale_fill_discrete(name = "ALLERGY") +
  theme_minimal()

################# PLoting text over the barplot
data = as.data.frame(table(df[,'LUNG_CANCER']))
data

ggplot(data, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) +  # Add text labels above the bars
  labs(
    x = "Categories",
    y = "Values",
    title = "Bar Plot with Numbers Above Bars"
  )
############################################
# Probability Plotting
data = c(sum(df['LUNG_CANCER']=='YES' & df['ALLERGY'] == '1') / sum(df['LUNG_CANCER']=='YES'),
         sum(df['LUNG_CANCER']=='NO' & df['ALLERGY'] == '1') / sum(df['LUNG_CANCER']=='NO'),
         sum(df['LUNG_CANCER']=='YES' & df['ALLERGY'] == '2') / sum(df['LUNG_CANCER']=='YES'),
         sum(df['LUNG_CANCER']=='NO' & df['ALLERGY'] == '2') / sum(df['LUNG_CANCER']=='NO'))

al.freq = data.frame(Symptom = rep(c('Non-allergic', 'Allergic'), each=2),
                     variable=rep(c('Lung Cancer', 'No Lung cancer'), times=2),
                     Value=as.vector(data))

ggplot(al.freq, aes(x = variable , y = Value, fill = Symptom)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Probabilities", title = 'Barplot') +
  geom_text(aes(label=round(Value, 2)), vjust=-0.5, size=4, hjust=0)

###################################################
# Function
bar.graph <- function(variable, a, b){
  data = c(sum(df['LUNG_CANCER']=='YES' & df[variable] == '1') / sum(df['LUNG_CANCER']=='YES'),
           sum(df['LUNG_CANCER']=='NO' & df[variable] == '1') / sum(df['LUNG_CANCER']=='NO'),
           sum(df['LUNG_CANCER']=='YES' & df[variable] == '2') / sum(df['LUNG_CANCER']=='YES'),
           sum(df['LUNG_CANCER']=='NO' & df[variable] == '2') / sum(df['LUNG_CANCER']=='NO'))
  
  al.freq = data.frame(Symptom = rep(c(a, b), each=2),
                       variable=rep(c('Lung Cancer', 'No Lung cancer'), times=2),
                       Value=as.vector(data))
  
  p <- ggplot(al.freq, aes(x = variable , y = Value, fill = Symptom)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "", y = "Probabilities", title = 'Barplot') +
    geom_text(aes(label=round(Value, 2)), vjust=-0.5, size=4, hjust=0.5,
              position=position_dodge(width = 0.9))  
  return(p)
}

print(bar.graph('ALLERGY', 'Non-Allergic', 'Allergic'))

##################################################################
# Install and load the necessary packages
install.packages("vcd")
library(vcd)

# Create a sample dataset with categorical variables
data_df <- data.frame(
  Variable1 = factor(c("A", "B", "A", "C", "B")),
  Variable2 = factor(c("X", "Y", "X", "Z", "Y"))
)

# Create a contingency table
cont_table <- table(data_df$Variable1, data_df$Variable2)

# Create a heatmap of the contingency table
heatmap(cont_table, 
        col = cm.colors(256),  # Color palette
        scale = "none",        # Scaling option (none for raw counts)
        margins = c(5, 5),     # Margins around the heatmap
        main = "Heatmap of Categorical Variables"
)

data_ppsm <- select(df, c(PEER_PRESSURE, SMOKING))
cont_table <- table(data_ppsm$PEER_PRESSURE, data_ppsm$SMOKING)
heatmap(cont_table, 
        col = cm.colors(256),  # Color palette
        scale = "none",        # Scaling option (none for raw counts)
        margins = c(5, 5),     # Margins around the heatmap
        main = "Heatmap of Categorical Variables"
)
color_bar <- image.plot(zlim = range(cont_table), col = cm.colors(256), legend.lab = "Frequency")

