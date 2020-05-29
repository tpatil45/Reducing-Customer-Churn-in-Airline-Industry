library(RJSONIO)
library(jsonlite)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(kernlab)

# Reading the csv with the data and viewing it
df <- read.csv('Cleaned_Data.csv')
View(df)
str(df)

# Creating a duplicate of the original data
# Removing the attributes which were determined to be unimportant for analysis
df_duplicate <- df
df_duplicate <- df_duplicate[,c(-1,-2,-7,-11,-15,-16,-18,-21,-22,-24,-26,-28,-29,-30,-31)]
View(df_duplicate)
str(df_duplicate)

# Convert attributes to numeric type
#df_duplicate$Airline.Status <- as.numeric(df_duplicate$Airline.Status)
#df_duplicate$Age <- as.numeric(df_duplicate$Age)
#df_duplicate$Gender <- as.numeric(df_duplicate$Gender)
#df_duplicate$Price.Sensitivity <- as.numeric(df_duplicate$Price.Sensitivity)
#df_duplicate$Flights.Per.Year <- as.numeric(df_duplicate$Flights.Per.Year)
#df_duplicate$Loyalty <- as.numeric(df_duplicate$Loyalty)
#df_duplicate$Type.of.Travel <- as.numeric(df_duplicate$Type.of.Travel)
column_names <- colnames(df_duplicate)
for (i in 1:length(column_names)){
  df_duplicate[,column_names[i]] <- as.numeric(df_duplicate[,column_names[i]])
}
str(df_duplicate)

# Converting the Likelihood to recommend score to promoter or detractor
df_duplicate$classification <- "Yes"
index <- which(df$Likelihood.to.recommend <7)
df_duplicate$classification[index] <- "No"
df_dd <- df_duplicate[,-16]
View(df_dd)

# Generating a random index for Train and test data segementation
randIndex <- sample(1:dim(df_dd)[1])
length(randIndex)

# Creating Train and Test data with 60:40 split
cutpoint_train <- floor(length(randIndex)*0.7)
train_data <- df_dd[randIndex[1:cutpoint_train],]
test_data <- df_dd[randIndex[(cutpoint_train+1):dim(df_dd)[1]],]

# SVM model with the all the relavent attributes
svmOutput <- ksvm(classification ~., data=train_data, kernel="rbfdot", kpar="automatic", C=5,cross=3,prob.model=TRUE)
svmOutput

# Testing the model and checking the error rate
svmPred <- predict(svmOutput, test_data)
str(svmPred)
conf_matrix <- table(svmPred, test_data$classification)
conf_matrix
error_rate <- (conf_matrix[1,2]+conf_matrix[2,1])/length(test_data$classification)
error_rate <- error_rate*100 
error_rate

