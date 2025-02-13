#my R script for data set analysing

#data loading
data <- read.csv("redwinequality.csv",sep=';',header=TRUE,stringsAsFactors = FALSE) #loading the csv wine data to r

#data preprocessing
sum(is.na(data))  # Total number of missing values

data_cleaned <- na.omit(data) #handle missing values in data

#exploratory data analysis (EDA) with summary statistics
summary(data_cleaned)

#data visualizations
library(ggplot2)
ggplot(data_cleaned, aes(x=alcohol content))+
  geom_histogram(binwidth = 0.5,fill="skyblue",color="black")+
  ggtitle("Distribution of Alcohol Content") #show the number of wines in each range of alcohol level

ggplot(data_cleaned, aes(y = alcohol)) + 
  geom_boxplot(fill = "lightblue") + 
  ggtitle("Boxplot of Alcohol Content")

ggplot(data_cleaned, aes(x = alcohol, y = quality)) +
  geom_point(color = "blue") +
  ggtitle("Alcohol vs Quality") +
  xlab("Alcohol") + ylab("Quality") #Scatter plot do display wine quality 

# Outlier detection using IQR
Q1 <- quantile(data_cleaned$alcohol, 0.25)
Q3 <- quantile(data_cleaned$alcohol, 0.75)
IQR_alcohol <- Q3 - Q1
outliers <- data_cleaned$alcohol[data_cleaned$alcohol < (Q1 - 1.5 * IQR_alcohol) | data_cleaned$alcohol > (Q3 + 1.5 * IQR_alcohol)]
print(outliers)

#Correlation Analysis

#correlation matrix
cor_matrix <- cor(data[, sapply(data_cleaned, is.numeric)]) # Compute correlation for numeric columns
print(cor_matrix)

#statistical analysis
# Mean, Median, Variance, and Standard Deviation for Alcohol
mean(data$alcohol, na.rm = TRUE)
median(data$alcohol, na.rm = TRUE)
var(data$alcohol, na.rm = TRUE)
sd(data$alcohol, na.rm = TRUE)


# Mean, Median, Variance, and Standard Deviation for Quality
mean(data$quality, na.rm = TRUE)
median(data$quality, na.rm = TRUE)
var(data$quality, na.rm = TRUE)
sd(data$quality, na.rm = TRUE)

#Hypothesis Testing

# Subsetting data
group1 <- data$alcohol[data$quality == 5]
group2 <- data$alcohol[data$quality > 5]

# T-test
t.test(group1, group2)




