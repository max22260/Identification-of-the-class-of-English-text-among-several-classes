
-----------------------------------------------------------------------------
load(file = "/home/max/Desktop/test/bbc_DataMatrix.RData")
bbc.data.matrix$content <-iconv(bbc.data.matrix$content,"WINDOWS-1252","UTF-8")



preProcess <- function(row.data, stopword.dir, BagOfWord , boolstemm){
  
  packages <- c('tm')
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  library(tm)
  row.data<-iconv(row.data,"WINDOWS-1252","UTF-8")
  
  stopwordlist<- readLines(stopword.dir)
  train_corpus<- Corpus(VectorSource(row.data))
  train_corpus <-tm_map(train_corpus,tolower)
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
                                , stripWhitespace = T ,dictionary = BagOfWord  ))

      print("DTM DONE !!")
      print(DTM)
      return(DTM)
}


train_dtm <-preProcess(row.data = train_dataset$content ,stopword.dir = "test.txt",BagOfWord = NULL , TRUE )
train_dtm <- removeSparseTerms(train_dtm,0.993)
BagOW <- findFreqTerms(train_dtm)
test_dtm <-preProcess(row.data = testdata$content ,stopword.dir = "test.txt",BagOfWord = BagOW , TRUE )

dim(train_dtm)
dim(test_dtm)


train_matrix <- as.matrix(train_dtm)
train_data_model <- as.data.frame(train_matrix)




train_data_model <- data.frame(y=train_dataset$class , x = train_data_model)

### test1 matrix form 
test1_matrix <- as.matrix(test_dtm)
test1_data_model <- as.data.frame(test1_matrix)
##test1_data_model

test1_data_model <-  data.frame(y=testdata$class , x = test1_data_model)


library(caret)
library(doParallel)


cl <- makePSOCKcluster(5)
registerDoParallel(cl)
svm <- train(y~.,data = train_data_model,method = 'svmLinear3')

## When you are done:
stopCluster(cl)


pre <- predict(svm,test1_data_model[,-1])

table(pre,test1_data_model$y)
mmetric(pre,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))


#---------------------------------------------------------------------------------------------------------------
library(FSelector)

train_chi <- as.data.frame(train_matrix)
train_chi$y <- train_dataset$class
chi_square <- chi.squared(y~.,data = train_chi  )

subset <- cutoff.k(chi_square , 300)
train_data_chi <- data.frame(y=train_chi$y , x = train_chi[subset])
test1_data_model <- as.data.frame(test1_matrix)
##test1_data_model
test1_data_model <-  data.frame(y=testdata$class , x = test1_data_model[subset])

library(caret)
library(doParallel)


cl <- makePSOCKcluster(5)
registerDoParallel(cl)
timr <- Sys.time()

svm <- train(y~.,data = train_data_chi,method = 'svmLinear3')

## When you are done:
stopCluster(cl)

total_runtime <- difftime(Sys.time(), timr)
print(total_runtime)



pre <- predict(svm,test1_data_model[,-1])

table(pre,test1_data_model$y)
mmetric(pre,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

#-----------------------------------------------------------------------------------------------------

train_info <- as.data.frame(train_matrix)
train_info$y <- train_dataset$class
IG_weight <- information.gain(y~., data = train_info)

subset_info <- cutoff.k(IG_weight,800)
train_data_info <- data.frame(y =train_info$y, x= train_info[subset_info] )
test1_data_model <- as.data.frame(test1_matrix)
test1_data_model <-  data.frame(y=testdata$class , x = test1_data_model[subset_info])

dim(train_data_info)
dim(test1_data_model)
library(caret)
library(doParallel)


cl <- makePSOCKcluster(5)
registerDoParallel(cl)
svm <- train(y~.,data = train_data_info,method = 'svmLinear3')

## When you are done:
stopCluster(cl)


pre <- predict(svm,test1_data_model[,-1])

table(pre,test1_data_model$y)
mmetric(pre,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))




