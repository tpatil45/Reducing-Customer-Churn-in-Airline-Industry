#Running codes for clean test of Prep code
  dev.off() #clear the graph window
  rm(list = ls()) #clear all user objects from the enviornment 
  cat('\014') #clear the console

#library(jsonlite)
#import the dataset
  #dataset <- url("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/8614410?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27fall2019-survey-M03.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191104T033853Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20191104%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=4a07cd98f4f582b2f3d2380d14ca5cc9293db39c60558d5f113a2e1d8919e0dd")
  #df <- jsonlite::fromJSON(dataset)

  setwd("C:/Users/hp/Desktop/IST 687/PROJECT")

#save the data
#write.csv(df, file = "AirplaneData.csv")
  AD <- read.csv("AirplaneData.csv")
  
#Viewing and understanding the data  
  View(AD)
  dim(AD) #Variables = 33, Observations = 10282
  str(AD)
  summary(AD)
  
#Checking the quartiles and mean/median of numerical variables  
  summary(AD[,sapply(AD, class) == "integer"])
  
#Number of variables of diffrent variables  
  sum(sapply(AD, class) == "factor")
  sum(sapply(AD, class) == "integer")
  sum(sapply(AD, class) == "numeric")
  
  
######################### Treating NA's #########################
  
  
  
#check for the Na's in every variable
  apply(is.na(AD),2,sum)
  
  
  a <- which(is.na(AD$Arrival.Delay.in.Minutes))
  length(a) #NA's in Arrival
  
  b <- which(is.na(AD$Departure.Delay.in.Minutes))
  length(b) #NA's in Departure
  
  c <- which(is.na(AD$Flight.time.in.minutes))
  length(c) #NA's in Flight Time
  
  d <- which(AD$Flight.cancelled == "Yes")
  length(d) #Cancelled Flight
  
  
  
  
  p <- a[(a %in% d)] #NA's in Arrival delay where flight.cancelled = YES
  length(p) #223/242 NA values where flight was cancelled
  
  q <- b[(b %in% d)] #NA's in Departure delay where flight.cancelled = YES
  length(q) #218/218 NA values where flight was cancelled
  
  r <- c[(c %in% d)] #NA's in flight.time where flight.cancelled = YES
  length(r) #223/242 NA values where flight was cancelled
  
  
#For flight.cancelled = YES, replacing the NA's with median
#Median because, best estimate for missing value when data is skewed
  AD$Departure.Delay.in.Minutes[q] <- round(median(AD$Departure.Delay.in.Minutes, na.rm = TRUE))
  AD$Arrival.Delay.in.Minutes[p] <- round(median(AD$Arrival.Delay.in.Minutes, na.rm = TRUE))
  AD$Flight.time.in.minutes[r] <- round(median(AD$Flight.time.in.minutes, na.rm = TRUE))
  

#NA's where flight.cancelled = NO
  a1 <- which(is.na(AD$Arrival.Delay.in.Minutes))
  length(a1)
  
  b1 <- which(is.na(AD$Departure.Delay.in.Minutes))
  length(b1)
  
  c1 <- which(is.na(AD$Flight.time.in.minutes))
  length(c1)
  
#Replace arrival delay NA's by corresponding departure delay values
#Reason: Flight departs late by x min, arrives late by x min
  AD$Arrival.Delay.in.Minutes[a1] <- AD$Departure.Delay.in.Minutes[a1]
  
#Regresing Flight distance to predict NA's in flight time
  
#Time variable without NA's  
  time <- AD$Flight.time.in.minutes[-c1]

#Corresponding Flight distances    
  dist <- AD$Flight.Distance[-c1]
  
#Distance Values for which time prediction needs to be done  
  test <- AD$Flight.Distance[c1]
  
#Linear model: X = Distance, Y = Time  
  trendline <- lm(time ~ dist)

#summary: R-Squared 0.94, pvalue < 0.0001  
  summary(trendline)
  
#Predicting the values
  pred <- round(predict(trendline, data.frame(dist = test)))
#replace flight time NA with pred
  AD$Flight.time.in.minutes[c1] <- pred
  

    
#NA for likely hood to recommend
  View(AD[which(is.na(AD$Likelihood.to.recommend)),])

#See all the flights running from this origin to destination
  View(AD[AD$Destination.City == "Salt Lake City, UT" & AD$Origin.City == "Rock Springs, WY", ])

#we found that all the airline are operated by partner code "OO", Northwestern Airlines
#So, we replace the NA with the median of those flights
  Salt_Rock  <- AD[AD$Destination.City == "Salt Lake City, UT" & AD$Origin.City == "Rock Springs, WY", ]
  AD$Likelihood.to.recommend[2498] <- median(Salt_Rock$Likelihood.to.recommend, na.rm = TRUE)

  
######################### Cleaning other data #########################  

      
#Removing the state code from cities
  AD$Destination.City <- gsub(",.*", "", AD$Destination.City)
  AD$Origin.City <- gsub(",.*", "", AD$Origin.City)
  
  
#Deleting FreeText - Not needed in EDA or Models
#But saving this dataset for Textmining 
  AD_Cleaned <- AD[, -which(colnames(AD) == 'freeText')]
  
#checking new data  
  dim(AD_Cleaned)  
  View(AD_Cleaned)
  
############################# Saving the cleaned data ###########################
  
  
#save the data
  write.csv(AD_Cleaned, file = "Cleaned_Data.csv")
    

  write.csv(AD, file = "FreeText.csv")