# ====================================================================================================
# title: preprocssing & fit stemming & fit bag Of word and fit Ngrams & reduction dim by remove sparce term fit 
#        deffirant type of classification algorithms     
#-----------------------------------------------------------------------------------------------------
# author:   ahmed nagy radwan
#-----------------------------------------------------------------------------------------------------
# summary: fit preprocessing and bag of word and use remove sparce term function to remove 
#          noise feature and fit classification algorithm ,and calculate accuracy to every model
#-----------------------------------------------------------------------------------------------------
# output :  accuracy of sevral model
# ====================================================================================================
library(SnowballC)
library(textreadr)
library(wordcloud)
library(rpart)
library(caret)
library(tm)
library(MASS)
library(RWeka)
library(rminer)
library(kernlab)
#======================================================================================================
dir  = "/home/max/Desktop/Identification of the class of English text among several classes/approaches/"

setwd(dir = dir)


load("../data/bbc_DataMatrix.RData")
bbc.data.matrix$content <-iconv(bbc.data.matrix$content,"WINDOWS-1252","UTF-8")
#-----------------------------------------------------------------------------------------------------

set.seed(100) # for randmnes

trian <- createDataPartition(y=bbc.data.matrix$class,p=0.70 , list = FALSE)
train_dataset <- bbc.data.matrix[trian,]
test_dataset <- bbc.data.matrix[-trian,]
#------------------------------------------------------------------------------------------------------


preProcess.N_grams <- function(row.data, stopword.dir, BagOfWord , boolstemm  , MIN_ngram , MAX_ngram , weighttype){
  
  packages <- c('tm','RWeka' )
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  library(RWeka)
  library(tm)
  row.data<-iconv(row.data,"WINDOWS-1252","UTF-8")
  
  stopwordlist<- readLines(stopword.dir)
  train_corpus<- VCorpus(VectorSource(row.data))
  train_corpus <-tm_map(train_corpus,content_transformer(tolower))
  train_corpus<-tm_map(train_corpus,removeNumbers)
  train_corpus<-tm_map(train_corpus,removeWords,stopwords("english"))
  train_corpus<-tm_map(train_corpus,removeWords,stopwordlist)
  train_corpus<- tm_map(train_corpus,removePunctuation)
  train_corpus<-tm_map(train_corpus,stripWhitespace)
  
  if(boolstemm){
    
    train_corpus<-tm_map(train_corpus,stemDocument,language = "english")
  }
  
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = MIN_ngram, max =MAX_ngram ))
  if(weighttype == "TFIDF"){
    DTM <-DocumentTermMatrix(train_corpus,
                             control = list(tolower = T ,
                                            removeNumbers =T ,
                                            removePunctuation = T ,
                                            stopwords = T 
                                            , stripWhitespace = T ,
                                            dictionary = BagOfWord,
                                            weighting = weightTfIdf,
                                            tokenize = Tokenizer))
  }else if(weighttype == "TF"){
    
    DTM <-DocumentTermMatrix(train_corpus,
                             control = list(tolower = T ,
                                            removeNumbers =T ,
                                            removePunctuation = T ,
                                            stopwords = T 
                                            , stripWhitespace = T ,
                                            dictionary = BagOfWord,
                                            weighting = weightTf,
                                            tokenize = Tokenizer))
    
    
    
    
  }else if(weighttype == "BIN"){
    DTM <-DocumentTermMatrix(train_corpus,
                             control = list(tolower = T ,
                                            removeNumbers =T ,
                                            removePunctuation = T ,
                                            stopwords = T 
                                            , stripWhitespace = T ,
                                            dictionary = BagOfWord,
                                            weighting = weightBin,
                                            tokenize = Tokenizer))
    
    
  }else{
    
    DTM <-DocumentTermMatrix(train_corpus,
                             control = list(tolower = T ,
                                            removeNumbers =T ,
                                            removePunctuation = T ,
                                            stopwords = T 
                                            , stripWhitespace = T ,
                                            dictionary = BagOfWord,
                                            tokenize = Tokenizer))
    
  }
  
  print("DTM DONE !!")
  print(DTM)
  return(DTM)
}


#-----------------------------------------------------------------------------------------------

train_dtm <-preProcess.N_grams(row.data = train_dataset$content ,stopword.dir = "../data/stopword.txt",
                               BagOfWord = NULL , TRUE , MIN_ngram = 1  , MAX_ngram = 2,weighttype = "TF" )
train_dtm <- removeSparseTerms(train_dtm,0.993)
BagOW <- findFreqTerms(train_dtm)
test_dtm <-preProcess.N_grams(row.data = test_dataset$content ,stopword.dir = "../data/stopword.txt",
                              BagOfWord = BagOW , TRUE , MIN_ngram = 1 , MAX_ngram = 2,weighttype = "TF" )

dim(train_dtm)
dim(test_dtm)

train_matrix <- as.matrix(train_dtm)
train_data_model <- as.data.frame(train_matrix)

train_data_model <- data.frame(y=train_dataset$class , x = train_data_model)

test_matrix <- as.matrix(test_dtm)
test_data_model <- as.data.frame(test_matrix)


test_data_model <-  data.frame(y=test_dataset$class , x = test_data_model)
#--------------------------------------------------------------------------------------------------------
remove(bbc.data.matrix)
remove(train_dataset)
remove(test_dataset)
remove(train_dtm)
remove(test_dtm)
remove(test_matrix)
remove(train_matrix)
#--------------------------------------------------------------------------------------------------------
save(train_data_model,test_data_model,BagOW , file = "../data/approaches_data/fifth approach/fifth-approach-data.RData")
#--------------------------------------------------------------------------------------------------------

Visualizing_Data.t_SNE  <- function(data,ClassNumber){
  
  packages <- c('Rtsne', 'ggplot2', 'plotly','tsne')
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library('Rtsne')
  library('ggplot2')
  library('plotly')
  library('tsne')
  
  
  features <- data[, !names(train_data_model) %in% c("y")]
  
  
  tsne <- Rtsne(
    as.matrix(features),
    check_duplicates = FALSE,
    perplexity = 30,
    theta = 0.5,
    dims = 2,
    verbose = TRUE
  )
  
  embedding <- as.data.frame(tsne$Y)
  embedding$Class <- as.factor(train_data_model$y)
  
  
  ax <- list(title = ""
             ,zeroline = FALSE)
  
  p <- plot_ly(
    data = embedding
    ,x = embedding$V1
    ,y = embedding$V2
    ,color = embedding$Class
    ,type = "scattergl"
    ,mode = 'markers'
    ,marker = list(line = list(width = 2))
    ,colors = rainbow(ClassNumber)
  ) %>% layout(xaxis = ax, yaxis = ax)
  
  return(p)
}

fifth_approach <- Visualizing_Data.t_SNE(train_data_model , ClassNumber = 5)

fifth_approach

#================================================classification==========================================


#*************************************** NAIVE BAYES CLASSIFIER ***************************

library(e1071)

naiveB_model = naiveBayes( y~.,data =train_data_model)

# prediction 

naiveB_testpred = predict(naiveB_model,test_data_model )

summary(naiveB_test1pred)
prop.table(table(naiveB_test1pred))

# testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(naiveB_testpred,test_data_model$y,positive = "positive", dnn = c ("prediction","true"))

# second accuracy

mmetric(naiveB_testpred,test_data_model[,1],c("ACC","TPR","PRECISION","F1"))


naiveB_table <-table(naiveB_testpred,test_data_model$y)


naiveB_table <- as.data.frame(naiveB_table)
colnames(naiveB_table)[] <-c("TClass","PClass","Y") 

library(ggplot2)
plot <- ggplot(naiveB_table)
naiveB_conf<-plot + geom_tile(aes(x=TClass, y=PClass, fill=Y)) + scale_x_discrete(name="Actual Class") +
  geom_text(aes(x = TClass, y=PClass ,label = sprintf("%1.0f", Y)), vjust = 1,check_overlap = TRUE ,size = 6 ) + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=0, to=100, by=10),low = "white", high = "#1698A7") + labs(fill="Frequency")
naiveB_conf


#-------------------------------------------------------------------------------------------
library(rpart)

dtree_model <-rpart(y~.,train_data_model , method =  "class") 


rpart.plot::rpart.plot(dtree_model)
########################## prediction process 


dtree_testpred = predict(dtree_model,newdata = test_data_model ,type = "class" )

summary(test1pred)
prop.table(table(test1pred))

############################################## testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(dtree_testpred,test_data_model[,1],positive = "positive", dnn = c ("prediction","true"))


# second accuracy

mmetric(dtree_testpred,test_data_model[,1],c("ACC","TPR","PRECISION","F1"))




dtree_table <-table(dtree_testpred,test_data_model$y)


dtree_table <- as.data.frame(dtree_table)
colnames(dtree_table)[] <-c("TClass","PClass","Y") 

library(ggplot2)
plot <- ggplot(dtree_table)
dtree_conf<-plot + geom_tile(aes(x=TClass, y=PClass, fill=Y)) + scale_x_discrete(name="Actual Class") +
  geom_text(aes(x = TClass, y=PClass ,label = sprintf("%1.0f", Y)), vjust = 1,check_overlap = TRUE ,size = 6 ) + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=0, to=100, by=10),low = "white", high = "#1698A7") + labs(fill="Frequency")
dtree_conf


#------------------------------------------------------------------------------------------------------------------

library(randomForest)

rf_model <- randomForest(x = train_data_model[,-1],y = train_data_model$y , ntree = 60 )


# prediction process 

rf_testpred = predict(rf_model,newdata = test_data_model[,-1] )
summary(rf_test1pred)
prop.table(table(rf_test1pred))

#testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(rf_testpred,test_data_model[,1],positive = "positive", dnn = c ("prediction","true"))

# second accuracy

mmetric(rf_testpred,test_data_model[,1],c("ACC","TPR","PRECISION","F1"))


plot(rf_model)



rf_table <- table(rf_testpred,test_data_model[,1])



rf_table <- as.data.frame(rf_table)
colnames(rf_table)[] <-c("TClass","PClass","Y") 

library(ggplot2)
plot <- ggplot(rf_table)
rf_conf<-plot + geom_tile(aes(x=TClass, y=PClass, fill=Y)) + scale_x_discrete(name="Actual Class") +
  geom_text(aes(x = TClass, y=PClass ,label = sprintf("%1.0f", Y)), vjust = 1,check_overlap = TRUE ,size = 6 ) + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=0, to=100, by=10),low = "white", high = "#1698A7") + labs(fill="Frequency")
rf_conf
#-------------------------------------------------------------------------------------------------------------------
library(caret)
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
timr <- Sys.time()

svm <- train(y~.,data = train_data_model,method = 'svmLinear3')

## When you are done:
stopCluster(cl)

total_runtime <- difftime(Sys.time(), timr)
print(total_runtime)



svm_pred <- predict(svm,test_data_model[,-1])

confusionMatrix(svm_pred,test_data_model[,1],positive = "positive", dnn = c ("prediction","true"))

mmetric(svm_pred,test_data_model[,1],c("ACC","TPR","PRECISION","F1"))

svm_table <- table(svm_pred, test_data_model$y)


svm_table <- as.data.frame(svm_table)
colnames(svm_table)[] <-c("TClass","PClass","Y") 

library(ggplot2)
plot <- ggplot(svm_table ,title ="")
svm_conf<-plot + geom_tile(aes(x=TClass, y=PClass, fill=Y)) + scale_x_discrete(name="Actual Class") +
  geom_text(aes(x = TClass, y=PClass ,label = sprintf("%1.0f", Y)), vjust = 1,check_overlap = TRUE ,size = 6 ) + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=0, to=100, by=10),low = "white", high = "#1698A7") + labs(fill="Frequency")
svm_conf


#---------------------------------------------------------------------------------------
saveRDS(naiveB_model,file = "../data/approaches_data/fifth approach/naiveB_model.rda")
saveRDS(dtree_model,file = "../data/approaches_data/fifth approach/dtree_model.rda")
saveRDS(rf_model,file = "../data/approaches_data/fifth approach/rf_model.rda")
saveRDS(svm,file = "../data/approaches_data/fifth approach/svm.rda")
#----------------------------------------------------------------------------------------
accuracy <- matrix(nrow = 1 , ncol = 4)
colnames(accuracy)[1:4] <-c("naive bayes","decision tree","random forest" , "support vector machine")


naiveB_table <-table(naiveB_testpred,test_data_model$y)
dtree_table <-table(dtree_testpred,test_data_model$y)
rf_table <- table(rf_testpred,test_data_model[,1])
svm_table <- table(svm_pred, test_data_model$y)

accuracy[1] <- sum(diag(naiveB_table))/sum(naiveB_table)
accuracy[2] <- sum(diag(dtree_table))/sum(dtree_table)
accuracy[3] <- sum(diag(rf_table))/sum(rf_table)
accuracy[4] <- sum(diag(svm_table))/sum(svm_table)
save(accuracy,file = "../data/approaches_data/fifth approach/accuracy.RData")
#-------------------------------------------------------------------------------------------
