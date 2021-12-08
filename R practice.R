rm(list = ls())   # clear global environment.

#USED LIBRARIES

library(stringr)  
library(mlr)
library(caret)
library(crayon)

##############################
#   load 4 files function    #function-1
##############################
load_data_into_big_df <- function(){
  
  cs6405_path<-'E:\\data\\Newsgroups\\'
  
  path1<-paste0(cs6405_path,'comp.sys.ibm.pc.hardware')
  file_lst1<-list.files(path=path1, pattern = '*')
  
  path2<-paste0(cs6405_path,'sci.electronics')
  file_lst2<-list.files(path=path2, pattern = '*')
  
  path3<-paste0(cs6405_path,'talk.politics.guns')
  file_lst3<-list.files(path=path3, pattern = '*')
  
  path4<-paste0(cs6405_path,'talk.politics.misc')
  file_lst4<-list.files(path=path4, pattern = '*')
  
  ##############
  # load 4 dataframes
  big_df_400 <- as.data.frame(NULL)
  
  # Dir 1
  for (nextfile in file_lst1){
    fName <- paste0(path1,'\\', nextfile) # derive the next file name (will be done individually for each file name in each subdirectory)
    data  <- file(fName, open="r")          
    lines <- readLines(data)                # read the data into the vector 'lines'. Each line will be a separate element
    big_df_400 <- rbind(big_df_400, c(99, rep(x = 0, ncol(big_df_400))))    
    
    for(i in 1:length(lines)){ # skip blanks
      if(lines[i] == ''){
        next
      }
      line <- strsplit(lines[i], " ")[[1]]  # split sentences into words by spaces
      
      for (nextword in 1:length(line)){
        if (line[nextword] %in% colnames(big_df_400)){
          cl_idx <- which(colnames(big_df_400) == line[nextword])           # find out which column refers to this word
          big_df_400[nrow(big_df_400),cl_idx] <- big_df_400[nrow(big_df_400),cl_idx] + 1 # increase word count by 1
        } else {
          # create new column for the new word and set frequency for this document to 1
          big_df_400 <- cbind(big_df_400, 0)              # create new column and use 0 to fill it with 0 for all docs
          colnames(big_df_400)[ncol(big_df_400)] <- line[nextword] # set new column's colname = new word
          big_df_400[nrow(big_df_400),ncol(big_df_400)] <- 1             # set the word frequency to 1 for the new word 
        }
      }
    }
    close(data)  # closes file
  }
  
  ## Dir 2
  
  for (nextfile in file_lst2){
    fName <- paste0(path2,'\\', nextfile) 
    data  <- file(fName, open="r")          
    lines <- readLines(data)                
    big_df_400 <- rbind(big_df_400, c(99, rep(x = 0, ncol(big_df_400))))   
    
    for(i in 1:length(lines)){ 
      if(lines[i] == ''){
        next
      }
      line <- strsplit(lines[i], " ")[[1]]  
      
      for (nextword in 1:length(line)){
        if (line[nextword] %in% colnames(big_df_400)){
          cl_idx <- which(colnames(big_df_400) == line[nextword])           
          big_df_400[nrow(big_df_400),cl_idx] <- big_df_400[nrow(big_df_400),cl_idx] + 1 
        } else {
          
          big_df_400 <- cbind(big_df_400, 0)              
          colnames(big_df_400)[ncol(big_df_400)] <- line[nextword] 
          big_df_400[nrow(big_df_400),ncol(big_df_400)] <- 1              
        }
      }
    }
    close(data)  # closes file
  }
  
  ## Dir 3
  
  for (nextfile in file_lst3){
    fName <- paste0(path3,'\\', nextfile) 
    data  <- file(fName, open="r")          
    lines <- readLines(data)                
    big_df_400 <- rbind(big_df_400, c(99, rep(x = 0, ncol(big_df_400))))    
    
    for(i in 1:length(lines)){ 
      if(lines[i] == ''){
        next
      }
      line <- strsplit(lines[i], " ")[[1]]  
      
      for (nextword in 1:length(line)){
        if (line[nextword] %in% colnames(big_df_400)){
          cl_idx <- which(colnames(big_df_400) == line[nextword])           
          big_df_400[nrow(big_df_400),cl_idx] <- big_df_400[nrow(big_df_400),cl_idx] + 1 
        } else {
          
          big_df_400 <- cbind(big_df_400, 0)              
          colnames(big_df_400)[ncol(big_df_400)] <- line[nextword] 
          big_df_400[nrow(big_df_400),ncol(big_df_400)] <- 1              
        }
      }
    }
    close(data)  
  }
  
   ## Dir 4
  
  for (nextfile in file_lst4){
    fName <- paste0(path4,'\\', nextfile) 
    data  <- file(fName, open="r")          
    lines <- readLines(data)                
    big_df_400 <- rbind(big_df_400, c(99, rep(x = 0, ncol(big_df_400))))    
    for(i in 1:length(lines)){ 
      if(lines[i] == ''){
        next
      }
      line <- strsplit(lines[i], " ")[[1]]  
      
      for (nextword in 1:length(line)){
        if (line[nextword] %in% colnames(big_df_400)){
          cl_idx <- which(colnames(big_df_400) == line[nextword])           
          big_df_400[nrow(big_df_400),cl_idx] <- big_df_400[nrow(big_df_400),cl_idx] + 1 
        } else {
          
          big_df_400 <- cbind(big_df_400, 0)              
          colnames(big_df_400)[ncol(big_df_400)] <- line[nextword] 
          big_df_400[nrow(big_df_400),ncol(big_df_400)] <- 1             
        }
      }
    }
    close(data)  
  }
  
  ## Classification column added as last column
  
  big_df_400$CLASSIFICATIONCOL = 1
  big_df_400$CLASSIFICATIONCOL[101:200] = 2
  big_df_400$CLASSIFICATIONCOL[201:300] = 3
  big_df_400$CLASSIFICATIONCOL[301:400] = 4
  
  ## remove non-needed column
  big_df_400[1] = NULL
  big_df_400[1]
  
  total_wc <<- ncol(big_df_400) - 1 # overall total words (make wc_count into global variable)
  
  return(big_df_400)
}

# end of function-1: load_data_into_big_df

############################
#Naive Bayes Implementation#function-2
############################

naive_bayes = function(big_df_400){

  ## Make 4 quarter files
  df_1 <- big_df_400[1:100,]
  df_2 <- big_df_400[101:200,]
  df_3 <- big_df_400[201:300,]
  df_4 <- big_df_400[301:400,]
  
  # Training and testing datasets 1:70 and 71:100
  df_1_train <- df_1[1:70,]
  df_1_test  <- df_1[71:100,]
  df_2_train <- df_2[1:70,]
  df_2_test  <- df_2[71:100,]
  df_3_train <- df_3[1:70,]
  df_3_test  <- df_3[71:100,]
  df_4_train <- df_4[1:70,]
  df_4_test  <- df_4[71:100,]
  
  # calculate totals
  total_wc <<- ncol(big_df_400) - 1 # overall total words (make wc_count into global variable)
  
  wc_train_1 <- sum(df_1_train[1:total_wc]) # overall total words per category
  wc_train_2 <- sum(df_2_train[1:total_wc])
  wc_train_3 <- sum(df_3_train[1:total_wc])
  wc_train_4 <- sum(df_4_train[1:total_wc])
  
  tot_mat_train_1 <- as.numeric(apply(df_1_train[1:total_wc], MARGIN=2, FUN=sum)) # total of each word in each category
  tot_mat_train_2 <- as.numeric(apply(df_2_train[1:total_wc], MARGIN=2, FUN=sum))
  tot_mat_train_3 <- as.numeric(apply(df_3_train[1:total_wc], MARGIN=2, FUN=sum))
  tot_mat_train_4 <- as.numeric(apply(df_4_train[1:total_wc], MARGIN=2, FUN=sum))
  
  ## Naive Bayes Implementation
  
  # merge 4 test df's into 1
  df_test = rbind(df_1_test, df_2_test, df_3_test, df_4_test)
  
  Sys.time()
  sum_of_truth=0
  sum_of_guess=0
  for (test_row in 1:nrow(df_test)){
    nz_vals <- which(df_test[test_row,1:total_wc] != 0) # only use columns that have non-zero, wc > 0
    
    prb_clssf_1 <- sum((df_test[test_row, nz_vals]) * (log10((tot_mat_train_1[nz_vals] + 1) / (wc_train_1 + total_wc))))
    prb_clssf_2 <- sum((df_test[test_row, nz_vals]) * (log10((tot_mat_train_2[nz_vals] + 1) / (wc_train_2 + total_wc))))
    prb_clssf_3 <- sum((df_test[test_row, nz_vals]) * (log10((tot_mat_train_3[nz_vals] + 1) / (wc_train_3 + total_wc))))
    prb_clssf_4 <- sum((df_test[test_row, nz_vals]) * (log10((tot_mat_train_4[nz_vals] + 1) / (wc_train_4 + total_wc))))
    
    prob_vals <- c(prb_clssf_1, prb_clssf_2, prb_clssf_3, prb_clssf_4)
    print(test_row)
    print(prob_vals)
    win <- which.max(prob_vals)
    msg_out <- paste0("Truth:", df_test[test_row,ncol(df_test)], "   Naive Bayes Guess:", win)
    print(msg_out)
    print(df_test[test_row,ncol(df_test)])
    truth=df_test[test_row,ncol(df_test)]
    guess=win
    if (truth==guess){
      sum_of_truth=sum_of_truth+1
    }else{
      sum_of_guess=sum_of_guess+1
    }      
  }
  print((paste0("Sum of Truth: ", sum_of_truth, " Sum of Guess: ", sum_of_guess, " Accuracy: ", ((sum_of_truth/(sum_of_guess+sum_of_truth))*100), "%")))
  Sys.time()
}

#end of function-2

######################
#Reduce the dataframe##function-3
######################

null_cols_wc_1 <- function(small_df){

  word_freqs_per_word <- apply(small_df[1:total_wc], MARGIN=2, FUN=sum)
  
  small_df[which(word_freqs_per_word == 1)] <- NULL
  
  return(small_df)
}

#end of function-3

###########################################
#Basic Evaluation of knn and Randon Forest# function-4
###########################################
mlr_small_df_400 <- function(small_df_400){
  
  ## MLR
  
  pd57_mlr_full_df = as.data.frame(small_df_400)
  
  colnames(pd57_mlr_full_df) = paste0("uniq_", 1:ncol(pd57_mlr_full_df)) # change colnames to uniq_1 uniq_2 etc for MLR validation
  
  colnames(pd57_mlr_full_df)[11290]= "classificationcol"
  pd57_mlr_train = as.data.frame(rbind(pd57_mlr_full_df[1:70,], pd57_mlr_full_df[101:170,], pd57_mlr_full_df[201:270,], pd57_mlr_full_df[301:370,]))
  pd57_mlr_test  = as.data.frame(rbind(pd57_mlr_full_df[71:100,], pd57_mlr_full_df[171:200,], pd57_mlr_full_df[271:300,], pd57_mlr_full_df[371:400,]))
  
  # dummy variable MLR
  
  pd57_dmy = dummyVars("~ .", data = pd57_mlr_train)
  pd57_clean_train = data.frame(predict(pd57_dmy, newdata = pd57_mlr_train))
  
  # set classif to chars
  
  pd57_clean_train["classificationcol"][[1]] = as.character(pd57_clean_train["classificationcol"][[1]])
  pd57_task = makeClassifTask(data = pd57_clean_train, target = "classificationcol")
  
  ##############################################################################################################
  
  #Knn Classifier

  pd57_learner = makeLearner("classif.knn", k=3)
  c(1:nrow(pd57_clean_train))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print("**********OUTPUT OF Classif.knn**********")
  print(pd57_unseen_prediction)
  pd57_accuracy = performance(pd57_unseen_prediction, measures = acc)
  cat(red("Accuracy of Classifier knn: ", pd57_accuracy,"\n","\n","\n"))
  pd57_unseen_prediction$data
  rdesc=makeResampleDesc("CV",iters=3)
  result=resample("classif.knn",pd57_task,rdesc,measures = acc)
  pred=getRRPredictions(result)
  print(pred)
  print(calculateConfusionMatrix(pred))
  
  #########################################################################

  #Random Forest
  pd57_learner = makeLearner("classif.randomForest")
  c(1:nrow(pd57_clean_train))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print("**********OUTPUT OF Classif.randomForest**********")
  print(pd57_unseen_prediction)
  pd57_accuracy = performance(pd57_unseen_prediction, measures = acc)
  ivory <- make_style("ivory")
  bgMaroon <- make_style("maroon", bg = TRUE)
  fancy <- combine_styles(ivory, bgMaroon)
  cat(fancy("Accuracy of classifier Random Forest: ", pd57_accuracy,"\n","\n","\n"))
  pd57_unseen_prediction$data
  pred=getRRPredictions(result)
  print(pred)
  result=resample("classif.randomForest",pd57_task,rdesc,measures = acc)
  pred=getRRPredictions(result)
  print(pred)
  print(calculateConfusionMatrix(pred))
  
}

#End of function- 4

########################
#Exploration of dataset# function-5
########################
explore_dataset <- function(popular_frequency_words){
  word_freqs_per_word <- apply(big_df_400[1:total_wc], MARGIN=2, FUN=sum)
  new_data<-sort(word_freqs_per_word,decreasing = TRUE)
  popular_200<-head(new_data,200)     
  #print(popular_200)          #prints the first 200 occuring words with their frequencies

  max_freqs = list()
  max_words = list()

  j = 1
  while(j <= 200){
    pd57_max = which.max(word_freqs_per_word)
    print(nchar(colnames(big_df_400[pd57_max])))
    leng = nchar(colnames(big_df_400[pd57_max]))
    if (leng < 4  | leng > 20 ){                #if the word is not between 4 and 20 throws away the word
      word_freqs_per_word[pd57_max] = 0
      next
    }
  
    max_freqs[j] = word_freqs_per_word[pd57_max]
    max_words[j] = colnames(big_df_400[pd57_max])
    word_freqs_per_word[pd57_max] = 0
    j = j + 1
  }
  print("Exploration of dataset summary")
  print(" THE WORDS AFTER FILTERATION ARE:")
  for (j in 1:200){
    
    print(paste("word:", max_words[[j]], "Frequency:", max_freqs[[j]]))
  
  }
  print("THE POPULAR 200 WORDS WITH THEIR FREQUENCY ARE AS FOLLOWS :")
  print(popular_200)     #prints the first 200 occuring words with their frequencies
  
}

#end of function-5

################
#Function calls#
################

#load big data frame
big_df_400 = load_data_into_big_df()   # call function-1

#Basic evaluation Naive Bayes
naive_bayes(big_df_400)     #call function-2

#Small dataframe 
small_df_400 <- null_cols_wc_1(big_df_400)  #call function-3 

#Robust Evaluation Naive Bayes
naive_bayes(small_df_400)    

#basic evaluation of MLRs(knn & Random Forest)
mlr_small_df_400(small_df_400)    #call function-4 

#Exploration of dataset
explore_dataset(popular_frequency_words)  #call function-5


#####################################################
# convert dataframe only with letters(preprocessing)# 
#####################################################

reorder_400_df <- subset(big_df_400, select=c(32888,1:32887)) # put classif in col 1 for MLR
colnames(reorder_400_df[32888])
reorder_400_df[,1] = c(rep(1,100), rep(2,100), rep(3,100), rep(4,100))
all_letters_mat_400 = data.frame(NULL)
all_letters_mat_400 = cbind(reorder_400_df[,1]) # bind col of classifs
  
Sys.time()
for (i in 1:32888){
  tmp_colname = tolower(gsub("[^[:alpha:]]", "", colnames(reorder_400_df[i])))
  if (tmp_colname == ""){
    next
  }
  if (tmp_colname %in% colnames(all_letters_mat_400)){
    idx = which(colnames(all_letters_mat_400) == tmp_colname)
    all_letters_mat_400[,idx] = all_letters_mat_400[,idx] + reorder_400_df[,i]
    }else{
      all_letters_mat_400 = cbind(all_letters_mat_400, 0) # add new column to right of existing columns
      colnames(all_letters_mat_400)[ncol(all_letters_mat_400)] = tmp_colname
      all_letters_mat_400[,ncol(all_letters_mat_400)] = reorder_400_df[,i]           
    }
  }
Sys.time()

###################################
# MLR Training and testing dataset# 
###################################

  pd57_mlr_full_df = as.data.frame(all_letters_mat_400)
  pd57_mlr_full_df[1] = NULL # delete incorrect column
  
  #ncol(pd57_mlr_full_df)
  colnames(pd57_mlr_full_df) = paste0("uniq_", colnames(pd57_mlr_full_df))
  colnames(pd57_mlr_full_df)[1]= "classificationcol"
  pd57_mlr_train = as.data.frame(rbind(pd57_mlr_full_df[1:70,], pd57_mlr_full_df[101:170,], pd57_mlr_full_df[201:270,], pd57_mlr_full_df[301:370,]))
  pd57_mlr_test  = as.data.frame(rbind(pd57_mlr_full_df[71:100,], pd57_mlr_full_df[171:200,], pd57_mlr_full_df[271:300,], pd57_mlr_full_df[371:400,]))
  
  # dummy variable MLR
  pd57_dmy = dummyVars("~ .", data = pd57_mlr_train)
  pd57_clean_train = data.frame(predict(pd57_dmy, newdata = pd57_mlr_train))
  
  # set classificationcol to chars
  pd57_clean_train["classificationcol"][[1]] = as.character(pd57_clean_train["classificationcol"][[1]])
  pd57_task = makeClassifTask(data = pd57_clean_train, target = "classificationcol")
  
 
#########################################
#classif.rpart with different techniques#function-6
#########################################

  #crossvalidation on rpart
  
mpart_params_robust<-function(rpart_params)  {
  
  pd57_learner = makeLearner("classif.rpart")
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.rpart",pd57_task,rdesc,measures = acc)
  print("**********OUTPUT OF Classif.rpart**********")
  print(pd57_unseen_prediction)
  pd57_accuracy = performance(pd57_unseen_prediction, measures = acc)
  cat(green("Accuracy of Classifier decision trees: ", pd57_accuracy,"\n","\n","\n"))
  pd57_unseen_prediction$data
  
  # Confusion matrix of rPart
  
  pd57_learner = makeLearner("classif.rpart")
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.rpart",pd57_task,rdesc,measures = acc)
  print("**********OUTPUT OF Classif.rpart with confusion matrix**********")
  pred=getRRPredictions(result)
  print(pred)
  print(calculateConfusionMatrix(pred))
  
  #Crossvalidation on rpart with 10 fold
  
  pd57_learner = makeLearner("classif.rpart")
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print("**********OUTPUT OF Classif.rpart cross validation 10 fold**********")
  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.rpart",pd57_task,rdesc,measures = acc)
  print(result)
  
  #Minsplit=1
  
  pd57_learner = makeLearner("classif.rpart",minsplit=1)
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print("**********OUTPUT OF Classif.rpart with minsplit=1**********")
  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.rpart",pd57_task,rdesc,measures = acc)
  print(result)
  
  #Split=entropy
  
  pd57_learner = makeLearner("classif.rpart",minsplit=20,parms=list(split="entropy"))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print("**********OUTPUT OF Classif.rpart split='entropy'**********")
  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.rpart",pd57_task,rdesc,measures = acc)
  print(result)
  
  #split=gini index
  
  pd57_learner = makeLearner("classif.rpart",minsplit=20,parms=list(split="gini"))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print("**********OUTPUT OF Classif.rpart with gini index**********")
  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.rpart",pd57_task,rdesc,measures = acc)
  print(result)
  
  #ROC curves in R
  
  n=getTaskSize(sonar.task)
  pd57_train.set = sample(n,size = round(2/3*n))
  test.set=setdiff(seq_len(n),pd57_train.set)
  lrn1=makeLearner("classif.rpart",predict.type = "prob")
  mod1=mlr::train(lrn1,sonar.task,subset=pd57_train.set)
  pred1=predict(mod1,task=sonar.task,subset=test.set)
  df=generateThreshVsPerfData(pred1,measures=list(fpr,tpr,mmce))
  plotROCCurves(df)
  performance(pred1,mlr::auc)
  
  #Cross validation grid search
  
  pd57_learner = makeLearner("classif.rpart")
  print(getParamSet(pd57_learner))
  c(1:nrow(pd57_clean_train))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  ps=makeParamSet(makeIntegerParam("minsplit",lower = 1,upper=5),makeIntegerParam("maxdepth",lower = 2,upper=5))
  ctrl=makeTuneControlGrid()
  rdesc=makeResampleDesc("CV",iters=3)
  task=makeClassifTask(data=pd57_clean_train,target="classificationcol")
  res=tuneParams(pd57_learner,task=task,resampling=rdesc,par.set = ps,control = ctrl,measures=acc,makeDiscreteParam("cp",values=seq(0.001,0.006,0.002)))
  
  #Cross validation Random search
  
  pd57_learner = makeLearner("classif.rpart")
  print(getParamSet(pd57_learner))
  c(1:nrow(pd57_clean_train))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  ps=makeParamSet(makeIntegerParam("minsplit",lower = 1,upper=5),makeIntegerParam("maxdepth",lower = 2,upper=5))
  ctrl=makeTuneControlRandom(maxit = 5)
  rdesc=makeResampleDesc("CV",iters=3)
  task=makeClassifTask(data=pd57_clean_train,target="classificationcol")
  res=tuneParams(pd57_learner,task=task,resampling=rdesc,par.set = ps,control = ctrl,measures=acc)
}

# end of function-6
  
#Desicion tree robust evaluation
mpart_params_robust(rpart_params) #call function-8
  
  
##############
#classif.knn #function-7
#############

#Knn Classifier
knn_robust<-function(knn_params){
  pd57_learner = makeLearner("classif.knn", k=3)
  #c(1:nrow(pd57_clean_train))
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print(pd57_unseen_prediction)
  pd57_accuracy = performance(pd57_unseen_prediction, measures = acc)
  cat("Accuracy: ", pd57_accuracy)
  pd57_unseen_prediction$data

  #Cross Validation 

  rdesc=makeResampleDesc("CV",iters=10)
  result=resample("classif.knn",pd57_task,rdesc,measures = acc)
  print(result)

  #Confusion matrix

  pred=getRRPredictions(result)
  print(pred)
  print(calculateConfusionMatrix(pred))

  #Cross validation -stratified
  rdesc=makeResampleDesc("CV",iters=10,stratify = TRUE)
  result=resample("classif.knn",pd57_task,rdesc,measures = acc)
  print(result)

  #Leave one out cross fold validation
  rdesc=makeResampleDesc("LOO")
  result=resample("classif.knn",pd57_task,rdesc,measures = acc)
  print(result)
}

#end of function-7

#Robust knn validation call
knn_robust(knn_params)  #call function-7


#Holdout
ctrl=makeFeatSelControlSequential(method = "sfs")
rdesc=makeResampleDesc("Holdout",split=0.7)
res=selectFeatures(pd57_learner,pd57_task,rdesc,control = ctrl)
print(analyzeFeatSelResult(res))
#rdesc=makeResampleDesc("Holdout",split=0.3)
#lrn=makeFeatSelWrapper(pd57_learner,resampling=rdesc,control=ctrl)
mod=mlr::train(lrn,PD57_task=task)
unseen_predictions=predict(mod,newdata=df[pd57_mlr_test,])
accuracy=performance(unseen_predictions,measures=acc)
cat("Accuracy:",accuracy)

#################
#Random Forest## function-8
################

randon_forest<-function(random_forest){
  
  pd57_learner = makeLearner("classif.randomForest")
  pd57_train.set = c(1:nrow(pd57_clean_train))
  pd57_model = mlr::train(pd57_learner, pd57_task, subset=pd57_train.set)
  pd57_unseen_prediction = predict(pd57_model, newdata = pd57_mlr_test)
  print(pd57_unseen_prediction)
  pd57_accuracy = performance(pd57_unseen_prediction, measures = acc)
  cat("Accuracy: ", pd57_accuracy)
  pd57_unseen_prediction$data

  #oBTAIN OVERVIEw OF PRESICION,RECALL AND F1 SCORE 
  rdesc=makeResampleDesc("CV",iters=10)
  ms=list("acc"=acc,"f1"=f1,"gpr"=gpr)
  result=resample("classif.randomForest",pd57_task,rdesc,measures = acc)
  print(result)

  #Confusion matrix
  pred=getRRPredictions(result)
  print(pred)
  print(calculateConfusionMatrix(pred))

  #ROC curves in R

  n=getTaskSize(sonar.task)
  pd57_train.set = sample(n,size = round(2/3*n))
  test.set=setdiff(seq_len(n),pd57_train.set)
  lrn1=makeLearner("classif.randomForest",predict.type = "prob")
  mod1=mlr::train(lrn1,sonar.task,subset=pd57_train.set)
  pred1=predict(mod1,task=sonar.task,subset=test.set)
  df=generateThreshVsPerfData(pred1,measures=list(fpr,tpr,mmce))
  plotROCCurves(df)
  performance(pred1,mlr::auc)
}

#end of function-8

#Random forest robust 
randon_forest(random_forest)  #call function-8

##################################################

