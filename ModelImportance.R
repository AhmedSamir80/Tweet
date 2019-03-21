#Run all ML model on data 
#apply 10-fold cross validation
#mix and match all possible combinations of parameters
#write results to excel file

CalculateAVGcsv <- function()
{
  library(dplyr)
  
  df1 <- read.csv(file="New folder/largest fmeasure/trialnum 1 .csv", header=TRUE, sep=",")
  
  df2 <- read.csv(file="New folder/largest fmeasure/trialnum 2 .csv", header=TRUE, sep=",")
  
  df3 <- read.csv(file="New folder/largest fmeasure/trialnum 3 .csv", header=TRUE, sep=",")
  
  df4 <- read.csv(file="New folder/largest fmeasure/trialnum 4 .csv", header=TRUE, sep=",")
  
  df5 <- read.csv(file="New folder/largest fmeasure/trialnum 5 .csv", header=TRUE, sep=",")
  
  df6 <- read.csv(file="New folder/largest fmeasure/trialnum 6 .csv", header=TRUE, sep=",")
  
  df7 <- read.csv(file="New folder/largest fmeasure/trialnum 7 .csv", header=TRUE, sep=",")
  
  df8 <- read.csv(file="New folder/largest fmeasure/trialnum 8 .csv", header=TRUE, sep=",")
  
  df9 <- read.csv(file="New folder/largest fmeasure/trialnum 9 .csv", header=TRUE, sep=",")
  
  df10 <- read.csv(file="New folder/largest fmeasure/trialnum 10 .csv", header=TRUE, sep=",")
  
  result = rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)
  result = aggregate (MeanDecreaseAccuracy~X, data=result, sum)
  print(result)
  
  write.table(result, "SumOfLargestfmeasure.csv", sep = ",", col.names = T, append = F) 
  
}




