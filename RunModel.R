RunModel<- function(TimeStamp,
                    TwtCorpus = NULL,
                    UsersData = NULL,
                    UseAcc = "all", 
                    Model_name = "DS",
                    linear = TRUE, tuned = FALSE, #options for SVM
                    Stemmed = FALSE,
                    UseWords = "Non-sparce",
                    self_cntr = FALSE, #exclude
                    FSelector = "None",
                    idf = FALSE, 
                    AccMsr = "as_is",
                    Sentm = "None",
                    Synon = FALSE,RFTrees = 500,MaxNodes = 100)
#possible values for parameters:
#UseAcc = {"all", "noAvg", "merge"} #account classes
#Model_name = {"DS", "NB","SVM"}
#Stemmed = {FALSE, TRUE}
#UseWords = {"Non-sparce", "DepSent"}
#self_cntr = {TRUE, FALSE}
#FSelector = {"None", "InfoGain", "Best", "MostFreq"}
#idf = {TRUE,FALSE}
#AccMsr = {"as_is", "norm", "catg"}
#Sentm = {"None","Avg", "Mixed"}
#Synon = {FALSE, TRUE}

{
  library(tm)   #::Corpus, freqyency
  library(dplyr)#::select()
  library(FSelector)#::infoGain()
  
  #TimeStamp = "02052018"
  if(is.null(UsersData)){
    #Read User Account Data
    UsersData = getAccountsData(TimeStamp)
    #construct the final data set that will be passed to the model
    TwtDataSet = getAccountMeasures(UsersData, AccMsr)
    print("Account measures read successfully")
  }
  else if(AccMsr == "as_is")
    TwtDataSet = UsersData
  else
    #in case of categorical or normalized
    TwtDataSet = getAccountMeasures(UsersData, type = AccMsr)
  
  if(Sentm == "Avg")
    TwtDataSet$SntmScore = UsersData$AvgSntmScore
  if(Sentm == "Mixed")
    TwtDataSet$SntmScore = UsersData$MixedSntmScore
  
  if(is.null(TwtCorpus)){
    #if not passed, create one!!
    if(UseAcc == "noAvg"){
      #No Average depressed class...
      ignoreAcc = which(TwtDataSet$Class == "Average Depressed")
      TwtDataSet = TwtDataSet  %>% filter(Class != "Average Depressed")
      TwtDataSet$Class = droplevels(TwtDataSet$Class)
      TwtCorpus = CreateCorpus(TimeStamp, ignore = ignoreAcc)
    }
    if(UseAcc == "merge")
    {
      TwtCorpus = CreateCorpus(TimeStamp)
      #Merge avg with depressed in one category
      library(stringr)
      TwtDataSet$Class =as.factor(as.character(stringr::str_replace_all(TwtDataSet$Class, "Average Depressed", "Depressed")))
      TwtDataSet$Class = droplevels(TwtDataSet$Class)
    }
    if(UseAcc == "all")
      TwtCorpus = CreateCorpus(TimeStamp)
    
    print("Corpus created successfully !!")
  } 
  
  stopwords = stopwords("english")
  if(self_cntr){
    #exclude self-centric words from the set of stop words
    stopwords = setdiff(stopwords, c("i", "me", "my", "myself",
                                     "we", "our","ours", "ourselves", 
                                     "you", "your", "yours", "yourself", "yourselves"))
  }
  TwtCorpus = tm_map(TwtCorpus, removeWords, c("the", "and", stopwords))
  
  #use stemmed words only
  if(Stemmed)
    TwtCorpus = tm_map(TwtCorpus,stemDocument)
  
  #To analyze the textual data, we use a Document-Term Matrix (DTM) representation:
  #documents as the rows, terms/words as the columns,
  #if ( Weighting == "idf") --> use the tf-idf(term frequency-inverse)
  # document frequency to score word frequency
  #otherwise use the default
  WgtFun = ifelse(idf, "weightTfIdf", "weightTf")
  dtm <- DocumentTermMatrix(TwtCorpus, control = list(weighting = WgtFun))
  
  #To reduce the dimension of the DTM, we can emove the less frequent terms 
  #such that the sparsity is less than 0.95
  if ( UseWords == "Non-sparce" )
    dtm = removeSparseTerms(dtm, 0.95) #sparcity 88%
  
  #use only sentiment wordsn from depressed class
  if ( UseWords == "DepSent" ){
    #find sentiment words in tweets
    pTerms = read.table("negative-words.txt", header = F, stringsAsFactors = F)[, 1]
    nTerms =read.table("positive-words.txt", header = F, stringsAsFactors = F)[, 1]
    dtm = as.matrix(dtm)
    x = which(TwtDataSet$Class == "Depressed")
    dtmx = dtm[x, , drop=FALSE] #only depressed accounts
    sentWords = base::intersect(base::union(pTerms, nTerms), colnames(dtmx))
    #keep only sentment words
    dtm = dtm[, sentWords, drop = FALSE]
    dtm = as.DocumentTermMatrix(dtm, weighting = WgtFun)
  }
  #in case of using Synon
  if (Synon){
    #annotate dtm in prep to use synon
    Words_POS = AnnotateWords(dtm)
    #update dtm to merge synonyms
    dtm = as.DocumentTermMatrix(FindSynon(dtm, Words_POS), weighting = WgtFun)
  }
  
  #Feature Extraction option
  if( FSelector == "InfoGain")
  {
    WordsDataSet = data.frame(Class = TwtDataSet$Class)
    WordsDataSet = cbind(WordsDataSet, as.matrix(dtm))
    x <- information.gain(Class ~ ., data=WordsDataSet, unit = "log2")
    x = cbind(word = row.names(x), x) #preserve row names 
    #write info_gain data to file
    #FileName = paste("Data/InfoGain_", Model_name, ".csv", sep = "")
    #write.csv(x, file= FileName ,row.names=FALSE)
    #print(paste("Info Gain data stored in file ", FileName))
    #Find words that has positive InfoGain 
    subset = as.character((x %>% filter(x$attr_importance>0))$word)
    dtm = as.matrix(dtm)
    dtm = dtm[,subset, drop=FALSE]
    dtm = as.DocumentTermMatrix(dtm, weighting = WgtFun)
  }
  else if ( FSelector == "Best"){
    WordsDataSet = data.frame(Class = TwtDataSet$Class)
    WordsDataSet = cbind(WordsDataSet, as.matrix(dtm))
    subset <- cfs(Class~., WordsDataSet)
    #print(as.simple.formula(subset, "Class"))
    dtm = as.matrix(dtm)
    dtm = dtm[,subset, drop=FALSE]
    dtm = as.DocumentTermMatrix(dtm, weighting = WgtFun)
  }
  else if(FSelector == "MostFreq"){
    #select words above average freq
    avgFreq = mean(colSums(as.matrix(dtm)))
    subset = tm::findFreqTerms(dtm, lowfreq = avgFreq, highfreq = Inf)
    dtm = dtm[,subset, drop=FALSE]
    dtm = as.DocumentTermMatrix(dtm, weighting = WgtFun)
  }
  
  #combine the dtm matrix with the user Account data
  TwtDataSet = cbind(TwtDataSet, as.matrix(dtm)) 
  
  
  #Split to testing and training set
  #using random sampling with 70-30%
  id_train <- sample(nrow(TwtDataSet),nrow(TwtDataSet)*0.70)
  train_accounts = TwtDataSet[id_train,]
  test_accounts = TwtDataSet[-id_train,]
  
  #run the model
  if (  Model_name == "DS" )#Decision Trees
  {
    library(rpart)
    library(rpart.plot)
    DesTree = rpart(Class~.,  method = "class", data = train_accounts)  
    #prp(DesTree) #plot the tree structure
    #Evaluate performance with the test set:
    pred = predict(DesTree, test_accounts,  type="class")
    #table(test_accounts$Class,pred,dnn=c("Obs","Pred"))
    #print(paste("Accuracy = ", mean(ifelse(test_accounts$Class == pred.tree, 1, 0))*100))
    #print(head(DesTree$variable.importance,10))
    
  } 
  
  if(Model_name == "RF")
  {
    #library(party)
    library(randomForest)
    library(e1071)
    print(RFTrees)
    names(train_accounts) <- make.names(names(train_accounts))
    forest <- randomForest(Class ~ ., data = train_accounts,ntree=RFTrees,maxnodes= MaxNodes,importance = TRUE)
    names(test_accounts) <- make.names(names(test_accounts))
    library(caret)
    pred = predict(forest, test_accounts)
    
  }
  
  if( Model_name == "SVM" ){ #Support Vector Machine
    library(e1071)
    if(linear)
      krnl = "linear"
    else
      krnl = "radial"
    
    if( !tuned ){
      MySVM <- svm(Class~., data=train_accounts, method="C-classification", kernel=krnl)
    }
    else {
      tune_out <- tune.svm(Class~., data=train_accounts,gamma=10^(-3:3),cost=c(0.01,0.1,1,10,100,1000),kernel=krnl)
      MySVM <- svm(Class~ ., data=train_accounts, method="C-classification", kernel=krnl,cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma)
    }
    pred = predict(MySVM, test_accounts)
    #table(test_accounts$Class,pred,dnn=c("Obs","Pred"))
    #print(paste("Accuracy = ", mean(ifelse(test_accounts$Class == pred.svm, 1, 0))*100.0))
  }
  
  #Naive Bayes 
  if (  Model_name == "NB" ){
    library(e1071)
    NBmodel <- naiveBayes(Class ~ ., data = train_accounts)
    #Evaluate performance with the test set:
    pred = predict(NBmodel, test_accounts)
    #table(test_accounts$Class,pred,dnn=c("Obs","Pred"))
    #print(paste("Accuracy = ", mean(ifelse(test_accounts$Class == pred.NB, 1, 0))*100.0))
  }
  #return accuracy
  library(caret)
  CM<- confusionMatrix(pred, test_accounts$Clas, positive = NULL, dnn = c("Prediction", "Reference"))
  
  Accuracy <- CM
  
    
}

getAccountMeasures<- function(UsersDataSet, type){
  #type = {"as_is", "norm", "catg"}
  
  UsersDataSet$Class = as.factor(UsersDataSet$Class)
  UsersDataSet$Time_of_posts = as.factor(UsersDataSet$Time_of_posts)
  
  if(type == "as_is" ){
    UsersDataSet = UsersDataSet %>% 
      select(Class,
             Number_of_followers,
             Following,
             Total_number_of_posts,
             Average_Posts_Per_day,
             Time_of_posts,
             Total_number_of_replies,
             Total_number_of_hashtags,
             Total_number_of_mentions,
             Total_number_emojies,
             Total_number_of_retweets,
             Total_number_retweeted,
             Total_Num_Pos_Terms,
             Total_Num_Neg_Terms)
    
  }
  else if (type == "norm"){
    N = UsersDataSet$Total_number_of_posts
    
    UsersDataSet$ReplyPercn = UsersDataSet$Total_number_of_replies/N
    UsersDataSet$HashPerPost = UsersDataSet$Total_number_of_hashtags/N
    UsersDataSet$MentionPerPost = UsersDataSet$Total_number_of_mentions/N
    UsersDataSet$EmojiPerPost = UsersDataSet$Total_number_emojies/N
    UsersDataSet$RetweetPercn = UsersDataSet$Total_number_retweeted/N
    UsersDataSet$PosTermsPerPost = UsersDataSet$Total_Num_Pos_Terms/N
    UsersDataSet$NegTermsPerPost = UsersDataSet$Total_Num_Neg_Terms/N
    
    UsersDataSet = UsersDataSet %>% 
      select(Class,
             Average_Posts_Per_day,
             Time_of_posts,
             ReplyPercn,
             HashPerPost,
             MentionPerPost,
             EmojiPerPost,
             RetweetPercn,
             PosTermsPerPost,
             NegTermsPerPost)
  }
  else if(type == "catg"){
    UsersDataSet$Posts_per_Day = getCategbyQntl(UsersDataSet$Average_Posts_Per_day)
    UsersDataSet$Followers = getCategbyQntl(UsersDataSet$Number_of_followers)
    UsersDataSet$Follows = getCategbyQntl(UsersDataSet$Following)
    UsersDataSet$No_of_Posts = getCategbyQntl(UsersDataSet$Total_number_of_posts)
    UsersDataSet$No_of_Replies = getCategbyQntl(UsersDataSet$Total_number_of_replies)
    UsersDataSet$No_of_Hashtags = getCategbyQntl(UsersDataSet$Total_number_of_hashtags)
    UsersDataSet$No_of_mentions = getCategbyQntl(UsersDataSet$Total_number_of_mentions)
    UsersDataSet$No_of_Emoji = getCategbyQntl(UsersDataSet$Total_number_emojies)
    UsersDataSet$No_of_Retweets = getCategbyQntl(UsersDataSet$Total_number_of_retweets)
    UsersDataSet$No_of_retweeted = getCategbyQntl(UsersDataSet$Total_number_retweeted)
    UsersDataSet$No_of_PosTerms = getCategbyQntl(UsersDataSet$Total_Num_Pos_Terms)
    UsersDataSet$No_of_NegTerms = getCategbyQntl(UsersDataSet$Total_Num_Neg_Terms)
    
    UsersDataSet = UsersDataSet %>% 
      select(Class,
             Posts_per_Day,
             Time_of_posts,
             Followers ,
             Follows ,
             No_of_Posts,
             No_of_Replies,
             No_of_Hashtags,
             No_of_mentions ,
             No_of_Emoji, 
             No_of_Retweets, 
             No_of_retweeted ,
             No_of_PosTerms ,
             No_of_NegTerms)
  }
  
  UsersDataSet #return
}

getCategbyQntl<- function(V){
  stats = summary(V)
  res = sapply( V , function(x){
    if(x >= as.numeric(stats[5]))#3rd Qntl
    {categ = "high"}
    else if (x >= as.numeric(stats[3]))#Median
    {categ = "average"}
    else if (x >= as.numeric(stats[2]))#1st Qntl
    {categ = "below Average"}
    else
    {categ = "low"}
  }
  )
  
  as.factor(res)
  
}
AnnotateWords <- function(dtm)
{
  #identify Part-of-speech for words in tweets Corpus
  
  library(openNLPmodels.en)
  library(openNLP)
  library(dplyr) #::select()
  library(stringr)#::str_replace_all()
  library(NLP) #::POSannotate()
  
  words = as.character(colnames(as.matrix(dtm)))
  ## Need sentence and word token annotations.
  sent_token_annotator <-Maxent_Sent_Token_Annotator()
  word_token_annotator <-Maxent_Word_Token_Annotator()
  a2 <-NLP::annotate(words, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator <-Maxent_POS_Tag_Annotator()
  aWords <-subset(NLP::annotate(words, pos_tag_annotator, a2), type == "word")
  tags <-sapply(aWords$features, '[[', "POS")
  
  #Word Net dictionary for synonyms is searchable only with
  #Part of speech type. Must be either "ADJECTIVE", "ADVERB", "NOUN", or "VERB".
  #"ADJECTIVE" ==> JJ, JJR, JJS
  #"ADVERB" ==> RB, RBR, RBS
  #"NOUN" --> NN, NNS, NNP, NNPS
  #"VERB" --> VB, VBD, VBG, VBN, VBP, VBZ
  words_POS = data.frame(word = words, POS = tags, stringsAsFactors = FALSE)
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "JJ", "ADJECTIVE")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "JR", "ADJECTIVE")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "JJS", "ADJECTIVE")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "RBR", "ADVERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "RB", "ADVERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "RBS", "ADVERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "NN", "NOUN")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "NNS", "NOUN")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "NNP", "NOUN")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "NNPS", "NOUN")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "VB", "VERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "VBD", "VERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "VBG", "VERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "VBN", "VERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "VBP", "VERB")
  words_POS$POS = stringr::str_replace_all(words_POS$POS,  "VBZ", "VERB")
  words_POS = words_POS %>% filter( POS == "NOUN" |
                                      POS == "VERB" |
                                      POS == "ADVERB" |
                                      POS == "ADJECTIVE")
  
  #return
  words_POS
  
}
FindSynon<- function(dtm, Words_POS)
{
  library(wordnet) #::synonyms()
  #set the location of the dictionary 
  setDict("WNdb-3.0/dict")
  Sys.setenv(WNHOME = "WNdb-3.0")
  
  dtm = as.matrix(dtm)
  dtm1 = dtm[, !(colnames(dtm) %in% Words_POS$word)] #keep words not valid POS
  dtm2 = dtm[, Words_POS$word] #use only words that has valid POS
  
  n = dim(dtm2)[2]
  i = 1
  while(i < n){
    #find synonyms
    x = synonyms(Words_POS$word[[i]], Words_POS$POS[[i]])                                  
    #search for synonyms in dtm
    y = which(colnames(dtm2) %in% x )
    if (length(y) > 1 ){
      #merge frequencies 
      Fsum = rowSums(dtm2[, y])
      #update word freq of major word
      dtm2[, y[1] ]= Fsum
      #delete synonym cols from dtm
      drops = y[2:length(y)]
      dtm2 = dtm2[, -drops ]
    }
    
    #update loop vars
    n = dim(dtm2)[2]
    i = i + 1
  }
  #return
  #print(paste("dtm words now = ", n) )
  #bind the words replaced with synon and other words that doesn't have synon
  dtm3 = cbind(dtm2, dtm1)
  
}
