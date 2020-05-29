#Running codes for clean test of Prep code
  dev.off() #clear the graph window
  rm(list = ls()) #clear all user objects from the enviornment 
  cat('\014') #clear the console

  setwd("C:/Users/hp/Desktop/IST 687/PROJECT")
  library(corrplot)
  library(ggplot2)
    
  AD_Cleaned <- read.csv("Cleaned_Data.csv")

  str(AD_Cleaned)
#Converting all the charcater variables to factor
  AD_Cleaned$Price.Sensitivity <- as.factor(AD_Cleaned$Price.Sensitivity)
  AD_Cleaned$Day.of.Month <- as.factor(AD_Cleaned$Day.of.Month)
  AD_Cleaned$Year.of.First.Flight <- as.factor(AD_Cleaned$Year.of.First.Flight)
  AD_Cleaned$Scheduled.Departure.Hour <- as.factor(AD_Cleaned$Scheduled.Departure.Hour)
  
#Converting all the integer variables to numeric  
  AD_Cleaned$Age <- as.numeric(AD_Cleaned$Age) 
  AD_Cleaned$Flights.Per.Year <- as.numeric(AD_Cleaned$Flights.Per.Year)
  AD_Cleaned$Total.Freq.Flyer.Accts <- as.numeric(AD_Cleaned$Total.Freq.Flyer.Accts)
  AD_Cleaned$Shopping.Amount.at.Airport <- as.numeric(AD_Cleaned$Shopping.Amount.at.Airport)
  AD_Cleaned$Eating.and.Drinking.at.Airport <- as.numeric(AD_Cleaned$Eating.and.Drinking.at.Airport)
  AD_Cleaned$Departure.Delay.in.Minutes <- as.numeric(AD_Cleaned$Departure.Delay.in.Minutes)
  AD_Cleaned$Arrival.Delay.in.Minutes <- as.numeric(AD_Cleaned$Arrival.Delay.in.Minutes)
  AD_Cleaned$Flight.time.in.minutes <- as.numeric(AD_Cleaned$Flight.time.in.minutes)
  AD_Cleaned$Flight.Distance <- as.numeric(AD_Cleaned$Flight.Distance)
  AD_Cleaned$Likelihood.to.recommend <- as.numeric(AD_Cleaned$Likelihood.to.recommend)
  

############################# Correlation Plot ###############################  
  
#Checking the correlation between the numeric data  
  Numeric_AD <- AD_Cleaned[sapply(AD_Cleaned, is.numeric)]
  corrTable <- cor(Numeric_AD)  
  #View(corrTable)
  corrplot(corrTable, type = "upper", method = "number", tl.cex=0.5)

#MULTICOLLINEARITY TEST:
#Arrival.delay is highly correlated to departure.delay
#Flight.distance is highly correlated to flight.time  
    
  
############################# Correlation & ANOVA ###############################  
  
#Creating a table for association of independent variables with dependent  
  AttrSelect <- data.frame(Columns = colnames(AD_Cleaned))
  for (i in 1:length(colnames(AD_Cleaned))){
    AttrSelect$Class[i] <- class(AD_Cleaned[ , i])
  }
  
  AttrSelect$Relation = 0
  View(AttrSelect)
  

#Checking the correlation for numeric variables
#Checking the ANOVA test value for factor variables  
  for(i in 1:length(AD_Cleaned)){
    if(AttrSelect[i,2] == "numeric"){
      AttrSelect$Relation[i] = cor(AD_Cleaned[,i], AD_Cleaned$Likelihood.to.recommend)
      AttrSelect$Test[i]<-"Correlation"
    }
    else{
      AOVTest <- aov(AD_Cleaned$Likelihood.to.recommend ~ AD_Cleaned[,i])
      AttrSelect$Relation[i] = unlist(summary(AOVTest))["Pr(>F)1"]
      AttrSelect$Test[i]<-"ANOVA"
        }
  }
  
#Deciding the cutoffs for feature selection  
  AttrSelect$Decision = "Reject"
  AttrSelect$Decision[AttrSelect$Test == "ANOVA" & AttrSelect$Relation <= 0.05] = "Accept"
  AttrSelect$Decision[AttrSelect$Test == "Correlation" & 
                       (AttrSelect$Relation>=0.10  & AttrSelect$Realtion <=0.80 )]  = "Accept"
  AttrSelect$Decision[AttrSelect$Test == "Correlation" & 
                       (AttrSelect$Relation<=-0.10  & AttrSelect$Relation >=-0.80 )]  = "Accept"

  View(AttrSelect)


############################# Chi-Square Test ###############################  
   
#Checking the association between independent factor variables   
  FactorD <- AD_Cleaned[sapply(AD_Cleaned, is.factor)]
  dim(FactorD)
  ChiResult <- matrix(nrow = 16, ncol = 16)
  for (i in 1:length(FactorD)) {
    for (j in 1:length(FactorD)) {
      CTest <- chisq.test(table(FactorD[ , i], FactorD[ , j]))
      ChiResult[i,j] <- unlist(CTest)["p.value"]
    }
  }
  ChiResult <- as.data.frame(ChiResult)
  colnames(ChiResult) <- colnames(FactorD)
  rownames(ChiResult) <- colnames(FactorD)
  View(ChiResult)

  
############################# Outlier treatment ###############################  
    
#Outlier Treatment by Winsorizing
Out_Treat_W = function(x){
  Q1 = quantile(x, 0.25)
  Q3 = quantile(x, 0.75)
  IQR = Q3 - Q1
  LC = Q1 - 1.5*IQR
  UC = Q3 + 1.5*IQR
  Out_Count = sum(x > UC | x < LC)
  UOut <- which(x > UC)
  LOut <- which(x < LC)
  for (i in 1:length(UOut)){
    x[UOut[i]] <- UC
  }
  for (i in 1:length(LOut)){
    x[LOut[i]] <- LC
  }
  OutInfo = list(TotalOutliers = Out_Count, LCutoff = LC, UCutoff = UC)
  print(OutInfo)
  return(x)
}

#Outlier Treatment by Median
Out_Treat_M = function(x){
  Q1 = quantile(x, 0.25)
  Q3 = quantile(x, 0.75)
  IQR = Q3 - Q1
  LC = Q1 - 1.5*IQR
  UC = Q3 + 1.5*IQR
  Out_Count = sum(x > UC | x < LC)
  TOut <- which(x > UC | x < LC)
  for (i in 1:length(TOut)){
    x[TOut[i]] <- median(x)
  }
  OutInfo = list(TotalOutliers = Out_Count, LCutoff = LC, UCutoff = UC)
  print(OutInfo)
  return(x)
}



######################### Exploratory Data Analysis ###########################


#Destination City  
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Destination.City, y = AD_Cleaned$Likelihood.to.recommend)) +
  geom_point(stat = "identity") + theme(axis.text.x=element_text(angle=90))
#Can't tell anything, Flat distribution, Constant median



#Origin City
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Origin.City, y = AD_Cleaned$Likelihood.to.recommend)) +
  geom_point(stat = "identity") + theme(axis.text.x=element_text(angle=90))
#Can't tell anything, Flat distribution, Constant median



#Destination state  
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Destination.State, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") +
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point") +
  theme(axis.text.x=element_text(angle=90))
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Destination.State, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "mean", colour = "red", size = 5, geom = "point") +
  theme(axis.text.x=element_text(angle=90))
#Can't tell anything, Flat distribution, Constant median



#Origin state
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Origin.State, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") +
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point") +
  theme(axis.text.x=element_text(angle=90))
#Can't tell anything, Flat distribution, Constant median



#Airline Status
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Airline.Status, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") +
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#We can clearly see that Silver, Gold and Platinum status people rate the airlines very high as compared to the Blue status 



#Age
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Age, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line") + 
  stat_summary(fun.y = "mean", colour = "blue", geom = "line")
#All values are from 20-85, so no chance of outlier
#It can be seen that the middle age people give pretty high ratings to the airlines,
#so, if we classify the age variable in 3 categories, it might help



#Gender
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Gender, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") +
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point") 
#So, male customers give higher ratings than the female customers
#male data skewed
#female data close to normal distribution
ggplot(data = AD_Cleaned, aes(x = AD_Cleaned$Gender, 
                                label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  labs(x = 'Gender', y = 'Count of People')



#Price Sensitivity
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Price.Sensitivity, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") +
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#We can see a decreasing trend in ratings with the increase in price sensitivity
#No, outliers as values take from 0 to 5 only.
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Price.Sensitivity, y = AD_Cleaned$Likelihood.to.recommend)) +
  geom_bar(stat = "identity")
#Very less number of observations for 0,3,4,5



#Year of first flight
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Year.of.First.Flight, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#Flat distribution, Constant median



#Flights per year
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Flights.Per.Year, y = AD_Cleaned$Likelihood.to.recommend)) +
  geom_bar(stat = "identity")
boxplot(AD_Cleaned$Flights.Per.Year, horizontal = T)

ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Flights.Per.Year, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line") + 
  stat_summary(fun.y = "mean", colour = "blue", geom = "line")
#Decreasing trend with increase in Flights per year
#Treat outliers by median method - normal distribution, random NPS in outlier region
#Tried outlier treatment, does not help in increasing predictive power



#Loyalty
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Loyalty, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line") 

boxplot(AD_Cleaned$Loyalty, horizontal = T)
#We can try using the group by function, create 2 groups, -1 - 0, 0 - 1
#Negative vs Positive
#No outliers, but data is slightly skewed
#Very random Recommendation Score but slight increasing trend



#Type of travel
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Type.of.Travel, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#Huge diffrence between categories of 3 people, personal travel people are definitely detractors



#Total Freq Flyer Accounts
boxplot(AD_Cleaned$Total.Freq.Flyer.Accts, horizontal = T)
summary(AD_Cleaned$Total.Freq.Flyer.Accts)

ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Total.Freq.Flyer.Accts, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")

ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Total.Freq.Flyer.Accts, y = AD_Cleaned$Likelihood.to.recommend)) +
  geom_bar(stat = "identity")
table(AD_Cleaned$Total.Freq.Flyer.Accts)
#Values are from 0 to 10, but after 5, data is scattered
tempD$Total.Freq.Flyer.Accts <- Out_Treat_W(tempD$Total.Freq.Flyer.Accts)




#Shopping
hist(AD_Cleaned$Shopping.Amount.at.Airport)
boxplot(AD_Cleaned$Shopping.Amount.at.Airport, horizontal = T)
#In this attribte the outliers should definitely be treated
#People who don't do shopping at the airport are too much
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Shopping.Amount.at.Airport, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line")
#Flat distribution, No use in linear regression
#No point in treating outliers




#Eating and Drinking
hist(AD_Cleaned$Eating.and.Drinking.at.Airport)
boxplot(AD_Cleaned$Eating.and.Drinking.at.Airport, horizontal = T)
#In this attribte the outliers should definitely be treated
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Eating.and.Drinking.at.Airport, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line")

#Winsorized outliers treatment used 
AD_Cleaned$Eating.and.Drinking.at.Airport <- Out_Treat_W(AD_Cleaned$Eating.and.Drinking.at.Airport)



#Class
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Class, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")

ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Class, y = AD_Cleaned$Likelihood.to.recommend)) +
  geom_bar(stat = "identity")
#Eco-Plus gives less Recommendation Score
table(AD_Cleaned$Class)



#Day of Month
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Day.of.Month, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#Flat distribution



#Flight Date
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Flight.date, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#Flat distrbution



#Scheduled depart hour
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Scheduled.Departure.Hour, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#Flat distribution



#Depart delay min
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Departure.Delay.in.Minutes, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")
#Messed up distribution



#Arrival delay min
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Arrival.Delay.in.Minutes, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") 
#Messed up distribution



#Flight Cancelled
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Flight.cancelled, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point") + 
  stat_summary(fun.y = "mean", colour = "blue", size = 5, geom = "point")
#Flight canceled people rate less
#Interesting fact: The diffrence is very less



#Flight Time
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Flight.time.in.minutes, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")
#can divide into 2 parts, 0-200(flat distribution), 200-400(messy distribution)



#Flight dist
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$Flight.Distance, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")
#might be good because upward trend



#olong
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$olong, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")



#olat
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$olat, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")



#dlong
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$dlong, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")



#dlat
ggplot(data = AD_Cleaned, mapping = aes(x = AD_Cleaned$dlat, y = AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", size = 5, geom = "point")
#dlat can be an excellent predictor 
#But it does make sense to include only dlat in model
#It will make the model overfitted



#EDA - WITHIN INDEPENDENT VARIABLES

PromData <- AD_Cleaned[which(AD_Cleaned$Customer.Type == "Promoter") , ]
DetrData <- AD_Cleaned[which(AD_Cleaned$Customer.Type == "Detractor") , ]
PassData <- AD_Cleaned[which(AD_Cleaned$Customer.Type == "Passive") , ]

#AGE
hist(AD_Cleaned$Age,breaks=seq(floor(min(AD_Cleaned$Age)),ceiling(max(AD_Cleaned$Age))),main="AGE", xlab="Height")

hist(PromData$Age,breaks=seq(floor(min(PromData$Age)),ceiling(max(PromData$Age))),main="AGE", xlab="Height")

hist(DetrData$Age,breaks=seq(floor(min(DetrData$Age)),ceiling(max(DetrData$Age))),main="AGE", xlab="Height")

ggplot(AD_Cleaned, aes(AD_Cleaned$Age,AD_Cleaned$Likelihood.to.recommend)) +
  stat_summary(fun.y = "median", colour = "red", geom = "line") + 
  stat_summary(fun.y = "mean", colour = "blue", geom = "line")



#Flight per year VS Loyalty
ggplot(AD_Cleaned, aes(AD_Cleaned$Flights.Per.Year, AD_Cleaned$Loyalty)) +
  geom_point()
#Strong Negative correlation



#olong vs dlong
ggplot(AD_Cleaned, aes(AD_Cleaned$olong, AD_Cleaned$dlong)) +
  geom_point()
#Strong positive correlation



#Arrival delay vs departure delay
ggplot(AD_Cleaned, aes(AD_Cleaned$Arrival.Delay.in.Minutes, AD_Cleaned$Departure.Delay.in.Minutes)) +
  geom_point()
#0.97 - Strong positive correlation



#Distance vs flight time
ggplot(AD_Cleaned, aes(AD_Cleaned$Flight.Distance, AD_Cleaned$Flight.time.in.minutes)) +
  geom_point()
#0.97 - Strong positive correlation



#male data distribution
MalSco <- AD_Cleaned[which(AD_Cleaned$Gender == "Male"),]
ggplot(MalSco, aes(Likelihood.to.recommend)) + geom_bar()



#female data distribtion
FemSco <- AD_Cleaned[which(AD_Cleaned$Gender == "Female"),]
ggplot(FemSco, aes(Likelihood.to.recommend)) + geom_bar()
#More women are passive


############################################################################



tempD <- AD_Cleaned


##########################################################################

#Selecting the variables based on:
#1] CORRELATION TEST
#2] ANOVA TEST
#3] MULTICOLLINEARITY
#4] EXPLORATORY DATA ANALYSIS
#5] P-VALUES


#Deleting "time" related variables
#Reason: pvalue & EDA & ANOVA 
tempD <- tempD[, - which(colnames(tempD) == "Year.of.First.Flight")]
tempD <- tempD[, - which(colnames(tempD) == "Flight.date")]
tempD <- tempD[, - which(colnames(tempD) == "Scheduled.Departure.Hour")]
tempD <- tempD[, - which(colnames(tempD) == "Day.of.Month")]


#Deleting city information variables
#Reason: pvalue & EDA
tempD <- tempD[, - which(colnames(tempD) == "Destination.City")]
tempD <- tempD[, - which(colnames(tempD) == "Origin.City")]



#Reason: Multi-collinearity
tempD <- tempD[, - which(colnames(tempD) == "Departure.Delay.in.Minutes")]
tempD <- tempD[, - which(colnames(tempD) == "Flight.Distance")]
#Reason: Chi-Square Test
tempD <- tempD[, - which(colnames(tempD) == "Partner.Name")]


#Location variables
#Reason: pvalues & EDA
tempD <- tempD[, - which(colnames(tempD) == "olong")]
tempD <- tempD[, - which(colnames(tempD) == "olat")]
tempD <- tempD[, - which(colnames(tempD) == "dlong")]
tempD <- tempD[, - which(colnames(tempD) == "dlat")]

#Reason: pvalue & ANOVA TEST
tempD <- tempD[, - which(colnames(tempD) == "Flight.cancelled")]




########################### GROUPING ##############################

#AGE
#Grouping Age variable by creating dummies for Young, Middle and Old people

for (i in 1:length(tempD$Age)) {
  if (tempD$Age[i] <= 30){
    tempD$Age_Young[i] = 1
  }
  else{
    tempD$Age_Young[i] = 0
  }
}
str(tempD$Age_Young)

for (i in 1:length(tempD$Age)) {
  if (tempD$Age[i] >= 50){
    tempD$Age_Old[i] = 1
  }
  else{
    tempD$Age_Old[i] = 0
  }
}


#Looking at the EDA of Age variable 
#Drastic change in slope twice
#SO, creating the interaction variable (Age * Dummy)
tempD$Young_Inter <- tempD$Age * tempD$Age_Young
tempD$Old_Inter <- tempD$Age * tempD$Age_Old



#Total.Freq.Flyer.Accts
#Total accounts dosent follow linear trend, instead it follows quadratic
#Creating a quadratic variable for total accounts

tempD$TFFA2 <- tempD$Total.Freq.Flyer.Accts * tempD$Total.Freq.Flyer.Accts



###################### Train -Test ####################################
library(dplyr)

set.seed(146)

stratified_sample <- sample_frac(tempD, 0.7)

View(stratified_sample)


TrainD <- tempD[stratified_sample$X,]
Test <- tempD[-stratified_sample$X,]

View(TrainD)
View(Test)
str(Test)

TestD <- Test[,- which(colnames(Test) == "Likelihood.to.recommend")]

View(TestD)
str(TestD)



####################### LINEAR MODEL ############################


LetsRegret <- lm(Likelihood.to.recommend ~. ,data = TrainD)

summary(LetsRegret)



####################### PREDICTING VALUES ############################


mypred <- round(predict(LetsRegret, TestD))



Results <- data.frame(Actual = Test$Likelihood.to.recommend, Prediction = mypred)
View(Results)


for (i in 1:length(Results$Actual)) {
  if (Results$Actual[i] >= 8){
    Results$Actual_Type[i] = "Promoter"
  }
  else {
    ResultsActual_Type[i] = "Detractor"
  }
}

for (i in 1:length(Results$Prediction)) {
  if (Results$Prediction[i] >= 8){
    Results$Prediction_Type[i] = "Promoter"
  }
  else {
    Results$Prediction_Type[i] = "Detractor"
  }
}

View(Results)


####################### CALCULATING THE ACCURACY ############################

Correct_V <- which(results$actual_Type==results$pred_Type)

Accuracy <- (length(Correct_V) * 100)/length(results$actual)
Accuracy