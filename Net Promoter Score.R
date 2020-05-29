#Adding a variable classified as Promoter, Passive and Detractor
#Based on recommendation score

for (i in 1:length(AD_Cleaned$Likelihood.to.recommend)) {
  if (AD_Cleaned$Likelihood.to.recommend[i] > 8) {
    AD_Cleaned$Customer.Type[i] = "Promoter"
  }
  else if (AD_Cleaned$Likelihood.to.recommend[i] < 7) {
    AD_Cleaned$Customer.Type[i] = "Detractor"
  }
  else {
    AD_Cleaned$Customer.Type[i] = "Passive"
  }
}


#Calculating the Net-Promoter score for each partner airline

NPS <- as.data.frame.matrix(table(AD_Cleaned$Partner.Name, AD_Cleaned$Customer.Type))

NPS$NPS <- ((NPS$Promoter - NPS$Detractor)*100)/(NPS$Detractor + NPS$Passive + NPS$Promoter)
View(NPS)

