#Loyalty vs Likelihood to recommend plot
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Loyalty, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line")



LoyaltyData <- AD_Cleaned[,c(1,10,28)]
View(LoyaltyData)



#################### TRAIN - TEST #####################

library(dplyr)

set.seed(146)

stratified_sample <- sample_frac(LoyaltyData, 0.7)


TrainD <- tempD[stratified_sample$X, ]
Test <- tempD[-stratified_sample$X,]
View(TrainD)
View(Test)
str(Test)

TestD <- Test[,  - which(colnames(Test) == "Likelihood.to.recommend")]
View(TestD)


#################### Regression Model #######################

Loy_Model <- lm(Likelihood.to.recommend ~ Loyalty ,data = TrainD)
summary(Loy_Model)


#################### Prediction #######################

mypred <- round(predict(LetsRegret, TestD))


results <- data.frame(actual = Test$Likelihood.to.recommend, pred = mypred)
View(results)

for (i in 1:length(results$actual)) {
  if (results$actual[i] >= 8){
    results$actual_Type[i] = "Promoter"
  }
  else {
    results$actual_Type[i] = "Detractor"
  }
}

for (i in 1:length(results$pred)) {
  if (results$pred[i] >= 8){
    results$pred_Type[i] = "Promoter"
  }
  else {
    results$pred_Type[i] = "Detractor"
  }
}

View(results)

#################### Calculating Accuracy #######################
CorrectR <- which(results$actual_Type==results$pred_Type)

Accuracy <- length(CorrectR)/length(results$actual)
Accuracy
