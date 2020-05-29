library(RJSONIO)
library(jsonlite)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(kernlab)
library(arules)
library(arulesViz)

# Read the data
df <- read.csv('ARM_Data.csv')
View(df)

# remove states attributes
df <- df[,c(-10,-11,-12)]
str(df)

# Generate transactions and apriori algorithm
df$Price.Sensitivity <- as.factor(df$Price.Sensitivity)
trans <- as(df, "transactions")
inspect(trans)
itemFrequency(trans)
itemFrequencyPlot(trans)
ruleset <- apriori(trans, 
                   parameter=list(support=0.15,confidence=0.5),
                   appearance = list(default="lhs", rhs=("Customer.Type=Detractor")))
inspect(ruleset)
inspectDT(ruleset)
plot(ruleset, method = "paracoord") # generate a plot for association rules

ruleset <- apriori(trans, 
                   parameter=list(support=0.2,confidence=0.5),
                   appearance = list(default="lhs", rhs=("Customer.Type=Promoter")))
inspect(ruleset)
inspectDT(ruleset)
plot(ruleset, method = "paracoord") # generate a plot for association rules
