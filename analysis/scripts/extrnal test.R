


-----------------------------------------------------------------------------
  load(file = "/home/max/Desktop/test/bbc_DataMatrix.RData")
bbc.data.matrix$content <-iconv(bbc.data.matrix$content,"WINDOWS-1252","UTF-8")







train_dtm <-preProcess_TFIDF(row.data = bbc.data.matrix$content ,stopword.dir = "stopword.txt",BagOfWord = NULL , TRUE)

train_dtm <- removeSparseTerms(train_dtm,0.993)
BagOW <- findFreqTerms(train_dtm)




train_matrix <- as.matrix(train_dtm)
#train_matrix <- binary.weight(train_matrix)
train_data_model <- as.data.frame(train_matrix)




train_data_model <- data.frame(y=bbc.data.matrix$class , x = train_data_model)



library(caret)
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
timr <- Sys.time()
svm <- train(y~.,data = train_data_model,method = 'svmLinear3')
stopCluster(cl)
total_runtime <- difftime(Sys.time(), timr)
print(total_runtime)










exdata <- read.dir("/home/max/Desktop/test1/",".TXT")

exdata$content <- iconv(exdata$content,"WINDOWS-1252","UTF-8")


ex_corpus <- Corpus(VectorSource(as.matrix(exdata$content)))
ex_corpus[["1"]][["content"]]
ex_corpus[["2"]][["content"]]
ex_corpus[["3"]][["content"]]

############################################ test1 as document term matrix 
bagm<-colnames(trainData[-1])


ex_dtm <-DocumentTermMatrix(ex_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                            , stripWhitespace = T  ,dictionary = train_bag_of_word))



ex_dtm<-preProcess(row.data = exdata$content ,stopword.dir = "../data/stopword.txt",BagOfWord = BagOW , TRUE )

test <-readLines("../data/external_data/6.TXT")
matre <- matrix(nrow = 1,ncol = 1)
matre[1]<-paste(test, sep = "", collapse = "")
ex_dtm<-preProcess(row.data = matre[1] ,stopword.dir = "../data/stopword.txt",BagOfWord = BagOW , TRUE )

ex_dtm <-preProcess_TFIDF(row.data = exdata$content ,stopword.dir = "../data/stopword.txt",BagOfWord = BagOW , TRUE )



str(ex_dtm)
dim(ex_dtm)

ex_matrix <- as.matrix(ex_dtm)
ex_data_model <- as.data.frame(ex_matrix)
dim(ex_data_model)

ex_data_model <- data.frame(x=ex_data_model)

ex_pred = predict(svm,newdata = ex_data_model  )

for(i in 1:length(ex_corpus) ){
 print( ex_corpus[[i]][["content"]] )
  print(": ") 
 print( ex_pred[i])
  
}



