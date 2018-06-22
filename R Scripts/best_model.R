# ====================================================================================================
# title: train sVm useing all dataset & test it on external data from real web-sites
#
#-----------------------------------------------------------------------------------------------------
# author:   ahmed nagy radwan
#-----------------------------------------------------------------------------------------------------
# summary: fit preprocessing and bag of word and fit and calculate TFIDF , use remove sparce term 
#          function to remove  
#          
#-----------------------------------------------------------------------------------------------------
# output :  lable class
# ====================================================================================================
library(SnowballC)
library(rpart)
library(caret)
library(tm)
library(MASS)
#======================================================================================================
dir  = "/home/max/Desktop/Identification of the class of English text among several classes/R Scripts/"

setwd(dir = dir)


load("../data/bbc_DataMatrix.RData")
bbc.data.matrix$content <-iconv(bbc.data.matrix$content,"WINDOWS-1252","UTF-8")
#-----------------------------------------------------------------------------------------------------

preProcess_TFIDF <- function(row.data, stopword.dir, BagOfWord , boolstemm ){
  
  packages <- c('tm')
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(tm)
  row.data<-iconv(row.data,"WINDOWS-1252","UTF-8")
  
  stopwordlist<- readLines(stopword.dir)
  train_corpus<- Corpus(VectorSource(row.data))
  train_corpus <-tm_map(train_corpus,content_transformer(tolower))
  train_corpus<-tm_map(train_corpus,removeNumbers)
  train_corpus<-tm_map(train_corpus,removeWords,stopwords("english"))
  train_corpus<-tm_map(train_corpus,removeWords,stopwordlist)
  train_corpus<- tm_map(train_corpus,removePunctuation)
  train_corpus<-tm_map(train_corpus,stripWhitespace)
  
  if(boolstemm){
    
    train_corpus<-tm_map(train_corpus,stemDocument,language = "english")
  }
  
  DTM <-DocumentTermMatrix(train_corpus,
                           control = list(tolower = T ,
                                          removeNumbers =T ,
                                          removePunctuation = T ,
                                          stopwords = T 
                                          , stripWhitespace = T ,
                                          dictionary = BagOfWord,
                                          weighting = weightTfIdf))
  
  
  print("DTM DONE !!")
  print(DTM)
  return(DTM)
}
#----------------------------------------------------------------------------------------------------
train_dtm <-preProcess_TFIDF(row.data = bbc.data.matrix$content ,stopword.dir = "../data/stopword.txt",
                             BagOfWord = NULL , TRUE)
train_dtm <- removeSparseTerms(train_dtm,0.993)
BagOW <- findFreqTerms(train_dtm)

dim(train_dtm)

train_matrix <- as.matrix(train_dtm)
#train_matrix <- binary.weight(train_matrix)
train_data_model <- as.data.frame(train_matrix)

train_data_model <- data.frame(y=bbc.data.matrix$class , x = train_data_model)

#----------------------------------------------------------------------------------------------------
remove(bbc.data.matrix)
remove(train_dtm)
remove(train_matrix)
#----------------------------------------------------------------------------------------------------
save(train_data_model,BagOW,file = "../data/BestModel_data_matrix.RData")
#----------------------------------------------------------------------------------------------------
read.dir <-function(dir , pattern){
  
  file.names <- dir(dir, pattern = pattern)
  file.names = as.data.frame(file.names)
  file.names$content = NA
  colnames(file.names) = c("filename", "content" )  
  
  for(i in 1:length(file.names[,1])){
    
    path <- paste0(dir,'/',file.names[i,1]) 
    line <-  readLines(path)
    # print(file.names[i,1])
    file.names[i,2] <-paste(line, sep = "", collapse = "")
    
  }
  return(file.names)
}
#------------------------------------------------------------------------------------------------------
library(caret)
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
timr <- Sys.time()
svm <- train(y~.,data = train_data_model,method = 'svmLinear3')
stopCluster(cl)
total_runtime <- difftime(Sys.time(), timr)
print(total_runtime)
#------------------------------------------------------------------------------------------------------
external_data <- read.dir("../data/external_data/",".TXT")
external_corpus <- Corpus(VectorSource(as.matrix(external_data$content)))
ex_dtm <-preProcess_TFIDF(row.data = external_data$content ,stopword.dir = "../data/stopword.txt",
                          BagOfWord = BagOW , TRUE )
dim(ex_dtm)
### test1 matrix form 
ex_matrix <- as.matrix(ex_dtm)
ex_data_model <- as.data.frame(ex_matrix)
##test1_data_model
dim(ex_data_model)

ex_data_model <- data.frame(x=ex_data_model)

ex_pred = predict(svm,newdata = ex_data_model  )

for(i in 1:length(external_corpus) ){
  print( external_corpus[[i]][["content"]] )
  print(": ") 
  print(ex_pred[i])
  print("----------------------------------------------------------------------------------------------")
}
#========================================================================================================
save(train_data_model,BagOW,file = "../data/best-model_DataMatrix")
saveRDS(svm ,file = "../data/best-model.rda")



