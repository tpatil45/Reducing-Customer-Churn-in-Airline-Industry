library(RJSONIO)
library(jsonlite)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(kernlab)

# ARM Data SVM
df <- read.csv('ARM_Data.csv')
passive_index <- which(df$Customer.Type=="Passive")
#df$Customer.Type[passive_index] <- "Promoter"
#df$Customer.Type <- as.numeric(df$Customer.Type)
df$Customer.Type <- as.factor(df$Customer.Type)
str(df)
column_names <- colnames(df)
for(i in 1:(length(column_names)-1)){
  df[,i] <- as.numeric(df[,i])
}
str(df)

randIndex <- sample(1:dim(df)[1])
length(randIndex)

cutpoint_train <- floor(length(randIndex)*0.6)
train_data <- df[randIndex[1:cutpoint_train],]
test_data <- df[randIndex[(cutpoint_train+1):dim(df)[1]],]

# SVM 3
svmOutput <- ksvm(Customer.Type ~., data=train_data, kernel="rbfdot", kpar="automatic", C=10,cross=3,prob.model=TRUE)
svmOutput

prediction <- predict(svmOutput, test_data)
xtab <- table(test_data$Customer.Type, prediction)
xtab
success_rate <- (xtab[1,1]+xtab[2,2]+xtab[3,3])/length(test_data$Customer.Type)
success_rate <- success_rate*100
success_rate
error_rate <- 100-success_rate
error_rate

