#before running any code, 
#please download the following libraries
#ONLY once!!
install.packages("rtweet")
install.packages("httr")
install.packages("twitteR")
install.packages("dplyr")
install.packages("plyr")
install.packages("lubridate")
install.packages("chron")
install.packages("stringr")
install.packages("tm")
install.packages("FSelector")
install.packages("sentimentr")
install.packages("tm.lexicon.GeneralInquirer")
install.packages("e1071")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("NLP")
install.packages("openNLP")
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("wordnet")
install.packages("stringr")
install.packages("ROCR")
install.packages("pROC")

#To fetch user accounts from twitter
# 1) make sure that all user names are stored in file:
# "Data/users.csv" where the column named "user" holds 
#  the user accounts in twitter (@user)
# 2) open files : authorize.R, getUserData.R
# 3)Source the files
# 4)call function
    #getRawUsersData()
# result : all user account data will be stored in file
    # "Data/raw_usersData_xxxxxx.csv" (time stamped with date)
    # "Data/users.csv" file will be updated with (id, serial_no) columns
    
#To fetch Tweets from Twitter
    # 1) make sure that:
    #     a) all user names are stored in file:
    #      "Data/users.csv" where the column named "user" holds 
    #       the user accounts in twitter (@user) and "serial_no" 
    #       holds an internal numbering that will be used throughout the code
    #       and an "id" column that holds the user id in twitter
    #     b) an emoji dictionary file named "emoji_dictionary.csv"
    #       located at your working directory
    # 2) open files : authorize.R, getUserData.R
    # 3)Source the files
    # 4)call function
     #      getRawTweets()
      #Result : all recent tweets for all users will be written in files
           # under the folder "RawTweets", one file per user where
           # the file name "TweetData_userx_xxxxxx" holds user serial number 
           # as well as the time stamp for the fetch date.

#To Prepare Tweets from Raw data
    # 1) make sure that:
    #     a)all user names are stored in file:
    #       "Data/users.csv" where the column named "user" holds 
    #       the user accounts in twitter (@user) and "serial_no" 
    #       holds an internal numbering that will be used throughout the code
    #       and an "id" column that holds the user id in twitter
    #     b)all tweets data are stored under the "RawTweets" folder
    # 2) open file: "PrepareTweets.R"
    # 3) source the file
    # 4) call the function:
          #PrepareTweets(TimeStamp)
          # the time stamp should be the same as the one in tweets files
          # eg: TimeStamp = "02052018"
      # Result :  all tweets data for all users will be processed  one file per user where
          # and saved in files under the "Tweets" folder where
          # the file name "TweetData_userx_xxxxxx" holds user serial number 
          # as well as the time stamp for the fetch date.

#To Prepare User Data from Raw data
      # 1) make sure that all user data are stored under the "Data" folder
      # and all tweets were prepared and stored under the "Tweets" folder
      # 2) open file:  PrepareUserData.R"
      # 3) source the file
      # 4) call the function:
           #PrepareUserData(TimeStamp)
           # the time stamp should be the same as the one in tweets files
           # eg: TimeStamp = "02052018"
      # result : all user account data will be processed adding 
           # new calculated fields and will be stored in file
           # "Data/usersData_xxxxxx.csv" (time stamped with date)
    
#To Tokenize Tweets and compute word frequencies:
    # 1) make sure that:
           # a) all user data prepared are stored under the "Data" folder
    #      b) all tweets were prepared and stored under the "Tweets" folder
    # 2) open files: "TweetTokenize.R" and "TokenizeAllTweets.R"
    # 3) source the files
    # 4) call the function: 
          TimeStamp = "02052018"
          TokenizeAllTweets(TimeStamp, Stemmed=FALSE)
          #where the time stamp should be the same as the one in tweets files
          # eg: TimeStamp = "02052018"
          # and stemmed parameter indicates whether 
          # the words should be stemmed or not
    # result : all users Tweet data will be tokenized  
          # will be stored in a matrix like format where the words are the rows and
          # the users labels the columns in a file named 
          # "TokenFreq_Stemmed_xxxxxx.csv" in case of stemmed or 
          # "TokenFreq_non-Stemmed_xxxxxx.csv" in case of non-stemmed words 
          # again the file is (time stamped with date)
          
#To run a model on a certain data batch:
      # 1) make sure that:
          # a) all user data prepared are stored under the "Data" folder
          # b) all tweets were prepared and stored under the "Tweets" folder
      # 2) open file: "RunModel.R" ,"CreateCorpus.R", "getAccountsData.R"
          # 3) source the files
          # 4) call the function: 
          Accuracy = RunModel(TimeStamp, ...)
          #where the time stamp should be the same as the one in tweets files
          # eg: TimeStamp = "02052018"
          # and the rest of paprmeters are defined 
          # in details in file. 
          # notice that all parameters has default values 
      # result : the classification accuracy of 
          # the selected model and its associated parameters

#To automate the process of running different models with
# all possible combinations of parameter values
      # 1) make sure that:
          # a) all user data prepared are stored under the "Data" folder
          # b) all tweets were prepared and stored under the "Tweets" folder
      # 2) open file: "AutoRunModel.R" ,"CreateCorpus.R", "getAccountsData.R"
      # 3) source the files
      # 4) call the function: 
          AutoRunModel(TimeStamp)
          #where the time stamp should be the same as the one in tweets files
          # eg: TimeStamp = "02052018"
      # result : all model runs accuracy will be stored in 
          # a matrix like format where each row lists the parameters used for each run
          # as well as the computed accuracy in this particular case
          # al the results will be  stored in file
          # "Data/ModelResults_xxxxxx.csv" 
          # again the file is (time stamped with date)

#To measure the performance of selected Models
          # 1) make sure that:
          #   a) all user data prepared are stored under the "Data" folder
          #   b) all tweets were prepared and stored under the "Tweets" folder
          # 2) open file: "Plot_ROC.R" ,"CreateCorpus.R", 
          #               "getAccountsData.R", "RunModel_Pred.R"
          # 3) source the files
          # 4) call the function: 
          Plot_ROC(TimeStamp, UseAccounts = "all")
          #where the time stamp should be the same as the one in tweets files
          # eg: TimeStamp = "02052018"
          # result : 
          # in case of binary classification: 
          #      >> ROC curve is plotted 
          #      >> AUC, CM, precision, recall, and
          #      fmeasure are printed for all models
          # in case of multi class classification:
          # Only AUC values are printed for all models

          