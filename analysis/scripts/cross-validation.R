








load(file = "/home/max/Desktop/test/bbc_DataMatrix.RData")

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



binary.weight <-function(x)
{
  x <- (x > 0)+0
}






train_dtm <-preProcess_TFIDF(row.data = bbc.data.matrix$content ,stopword.dir = "test.txt",BagOfWord = NULL , TRUE)

train_dtm <- removeSparseTerms(train_dtm,0.993)
BagOW <- findFreqTerms(train_dtm)
test_dtm <-preProcess_TFIDF(row.data = testdata$content ,stopword.dir = "test.txt",BagOfWord = BagOW , TRUE )

dim(train_dtm)
dim(test_dtm)




train_matrix <- as.matrix(train_dtm)
#train_matrix <- binary.weight(train_matrix)
train_data_model <- as.data.frame(train_matrix)




train_data_model <- data.frame(y=bbc.data.matrix$class , x = train_data_model)

### test1 matrix form 
test1_matrix <- as.matrix(test_dtm)
#test1_matrix <- binary.weight(test1_matrix)
test1_data_model <- as.data.frame(test1_matrix)
##test1_data_model

test1_data_model <-  data.frame(y=testdata$class , x = test1_data_model)


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


pre <- predict(svm,test1_data_model[,-1])

table(pre,test1_data_model$y)
mmetric(pre,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))


#-----------------------------------------------------------------------------------------------------------------



acc_matrix <- matrix(nrow = 10 , ncol = 2 )
colnames(acc_matrix) <- c("accuracy" , "Time")


#Randomly shuffle the data
train_data_model<-train_data_model[sample(nrow(train_data_model)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(train_data_model)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train_data_model[testIndexes, ]
  trainData <- train_data_model[-testIndexes, ]
  
  cl <- makePSOCKcluster(5)
  registerDoParallel(cl)
  timr <- Sys.time()
  
  svm <- train(y~.,data = trainData,method = 'svmLinear3')
  
  ## When you are done:
  stopCluster(cl)
  
  total_runtime <- difftime(Sys.time(), timr)
  print(total_runtime)
  
  
  pre <- predict(svm,testData[,-1])
  
  t <- table(pre,testData$y)
  acc = sum(diag(t))/sum(t)
  print(acc)
  acc_matrix[i,1] = acc
  acc_matrix[i,2] = total_runtime
  
  
  
}


acc_matrix <- as.data.frame(acc_matrix)

new <- as.data.frame( acc_matrix$accuracy)
colnames(new) <- "accuracy"

require(reshape2)
s <- ggplot(data = melt(new), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))




testIndexes <- which(folds==3,arr.ind=TRUE)
testData <- train_data_model[testIndexes, ]
trainData <- train_data_model[-testIndexes, ]



save(trainData,testData,BagOW,file = "/home/max/Desktop/best_sample.RData")


load("/home/max/Desktop/best_sample.RData")













