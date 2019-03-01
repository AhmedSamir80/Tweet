#Run all ML model on data 
#apply 10-fold cross validation
#mix and match all possible combinations of parameters
#write results to excel file

AutoRunModel <- function(TimeStamp)
{
  library(tm)   #::Corpus, freqyency
  library(plyr) #::mutate()
  library(dplyr)#::select()
  
  Results = data.frame("Accuracy" = numeric() , "Model_name" = character() , 
                       "Stemmed" = logical(), "UseWords" = character(),
                       "Self_Centric" = logical(),"Feacture_Selector" = character(),
                       "idf" = logical(), "Account_Measures" = character(),
                       "Tweet_Sentm" = logical(), Synonym = logical(), stringsAsFactors = FALSE)
  
  UsersData = getAccountsData(TimeStamp)
  print("Users account measures loaded sucessfuly!!")
  
  #use only two categories of target
  #No Average depressed class...
  #ignore = which(UsersData$Class == "Average Depressed")
  #UsersData = UsersData  %>% filter(Class != "Average Depressed")
  #UsersData$Class = droplevels(UsersData$Class)
  #Tweets_corpus = CreateCorpus(TimeStamp, ignore = ignore)
  #OR...
  #Avg Merged with Depressed
  ignore = NULL #use all users
  library(stringr)
  UsersData$Class =as.factor(as.character(stringr::str_replace_all(UsersData$Class, "Average Depressed", "Depressed")))
  droplevels(UsersData$Class)
  Tweets_corpus = CreateCorpus(TimeStamp, ignore = ignore)
  #OR, just keep all
  #Tweets_corpus = CreateCorpus(TimeStamp)
  
  print("Corpus Created!!")
  for( Model_name in c("RF") ) #c("DS", "NB","SVM","RF"))
    for (Stemmed in c( TRUE,FALSE))
      for (UseWords in c("Non-sparce", "DepSent"))
        for (self_cntr in c(TRUE, FALSE))
          for( idf in c(TRUE, FALSE))
            for(FSelector in c("None", "InfoGain", "MostFreq")) #"Best"
              for (AccMsr in c("as_is", "norm", "catg"))
                for (Sentm in c("None","Avg", "Mixed"))
                  for(Synon in c(TRUE,FALSE))
                {
                    
                  if(Model_name == "RF")
                  {
                  for(RFTrees in c(500,1000,1500,2000))
                    for (MaxNodes in (300))#(300,200,100))
                  {
                  #10-fold cross Validation
                  Model_parms = data.frame(Model_name = Model_name,
                                           linear = TRUE, #tuned = False , 
                                           Stemmed = Stemmed,
                                           UseWords = UseWords,
                                           self_cntr = self_cntr, 
                                           FSelector = FSelector,
                                           idf = idf ,
                                           AccMsr = AccMsr,
                                           Sentm =  Sentm, 
                                           Synon = Synon,
                                           stringsAsFactors = FALSE,
                                           RFTrees = RFTrees,
                                           MaxNodes = MaxNodes)
                  CrossValidate(TimeStamp = TimeStamp, TwtCorpus = Tweets_corpus, 
                                         UsersData = UsersData,
                                         Model_parms = Model_parms)
                  }
                  
                  }
                  else
                  {
                    Model_parms = data.frame(Model_name = Model_name,
                                             linear = TRUE, #tuned = False , 
                                             Stemmed = Stemmed,
                                             UseWords = UseWords,
                                             self_cntr = self_cntr, 
                                             FSelector = FSelector,
                                             idf = idf ,
                                             AccMsr = AccMsr,
                                             Sentm =  Sentm, 
                                             Synon = Synon,
                                             stringsAsFactors = FALSE,
                                             RFTrees = 500,MaxNodes = 100)
                    
                    CrossValidate(TimeStamp = TimeStamp, TwtCorpus = Tweets_corpus, 
                                           UsersData = UsersData,
                                           Model_parms = Model_parms)
                  }
                  #Data is written once created no need for below line
                  #Results = rbind(Results, PerRun)
                  #Now we are returning the whole Confusion Matrix and printing it so no need for the below line
                  #print(paste(PerRun$Model_name, PerRun$Accuracy)) #model, accuracy
                  
                  #run one more time for radial
                  if(Model_name == "SVM")
                  {
                    Model_parms$linear = FALSE 
                    CrossValidate(TwtCorpus = Tweets_corpus, 
                                           UsersData = UsersData,Model_parms = Model_parms)
                    #Results = rbind(Results, PerRun)
                    #print(paste(PerRun$Model_name, PerRun$Accuracy)) #model, accuracy
                  }
                  
                }#model run
  
  
  #write result data to file
  #FileName = paste("Data/ModelResults_", TimeStamp, ".csv", sep = "")
  #write.csv(Results, file= FileName ,row.names=FALSE)
  #print(paste("results stored in file ", FileName))
  
}

CrossValidate<- function(TimeStamp,TwtCorpus, 
                         UsersData,Model_parms){
  
  #Accuracy <- rep(NA,10)
  
  for(n in 1:10)
  {
    CM = RunModel(TimeStamp, 
                   TwtCorpus = TwtCorpus, 
                   UsersData = UsersData,
                   Model_name = Model_parms$Model_name, 
                   linear = Model_parms$linear, #tuned = False , 
                   Stemmed = Model_parms$Stemmed,
                   UseWords = Model_parms$UseWords,
                   self_cntr = Model_parms$self_cntr, 
                   FSelector = Model_parms$FSelector,
                   idf = Model_parms$idf, 
                   AccMsr = Model_parms$AccMsr,
                   Sentm = Model_parms$Sentm,
                   Synon = Model_parms$Synon,
                   RFTrees = Model_parms$RFTrees)
  
   
    #print results..
    
    #Accuracy[n] <- Acc
    
  
  #Acc = max(Accuracy)
  if(Model_parms$Model_name == "SVM")
    Model_parms$Model_name = paste(Model_parms$Model_name, ifelse(Model_parms$linear, "-linear", "-radial"), sep = "")
  
  
  print("The CM matrix is...")
  #Uncomment the below line if you need the whole Confusion Matrix
  #print(CM)
  
  TN = CM$table[1,1];  TP = CM$table[2,2]
  FP= CM$table[1,2];   FN = CM$table[2,1]
  
  Acc = (TP+TN) / (TP+TN+FP+FN) 
  #precision
  #What proportion of positive identifications was actually correct?
  P = TP/(TP+FP)
  #recall
  #What proportion of actual positives was identified correctly?
  R = TP/(TP+FN)
  #fmeasure
  F1 = (2*P *R)/(P+R)
  
  
  print(paste("The Acc = ", Acc))
  print(paste("The precision = ", P))
  print(paste("The recall = ", R))
  print(paste("The fmeasure = ", F1))
  
  
  PerRun <- mutate(Model_parms, "Accuracy" = Acc)
  
  PerRun <- mutate(PerRun, "Percision" = P)
  
  PerRun <- mutate(PerRun, "recall" = R)
  
  PerRun <- mutate(PerRun, "fmeasure" = F1)
  write.table(PerRun, "Result.csv", sep = ",", col.names = F, append = T)  
  
  }#loop
  #return
  #PerRun
  
}



