#ngram



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




load(file = "/home/max/Desktop/test/bbc_DataMatrix.RData")

bbc.data.matrix$content <-iconv(bbc.data.matrix$content,"WINDOWS-1252","UTF-8")




#--------------------------- prepare training_dataset and test1_dataset , test2_dataset -----------------
set.seed(100) # for randmnes

trian <- createDataPartition(y=bbc.data.matrix$class,p=0.70 , list = FALSE)
train_dataset <- bbc.data.matrix[trian,]
testdataset <- bbc.data.matrix[-trian,]


#---------------------------------------------------------------
 BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
 TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
 FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

a <- "we are not alone ahmed  fuck you ahmed no one will know sasda wwewe cfdfsdfs"
a <- VCorpus(VectorSource("we are not alone ahmed  fuck you ahmed no one will know sasda wwewe cfdfsdfs"))
test <- DocumentTermMatrix(a, control = list(tokenize =BigramTokenizer))
#-------------------------------------------------------------------------------



train.tokens <- as.matrix(test)






ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  options <- stringi::stri_opts_brkiter(type="word", skip_word_none = skip_word_none)
  
  function(x) {
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stringi::stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stringi::stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}

#### ngram_tokenizer example
x <- ngram_tokenizer(2)(train_dataset$content)










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







train_dtm <-preProcess.N_grams(row.data = train_dataset$content ,stopword.dir = "test.txt",BagOfWord = NULL , TRUE , MIN_ngram = 1  , MAX_ngram = 2,weighttype = "TF" )
train_dtm <- removeSparseTerms(train_dtm,0.993)
BagOW <- findFreqTerms(train_dtm)
test_dtm <-preProcess.N_grams(row.data = testdataset$content ,stopword.dir = "test.txt",BagOfWord = BagOW , TRUE , MIN_ngram = 1 , MAX_ngram = 2,weighttype = "TF" )

dim(train_dtm)
dim(test_dtm)




train_matrix <- as.matrix(train_dtm)
#train_matrix <- binary.weight(train_matrix)
train_data_model <- as.data.frame(train_matrix)




train_data_model <- data.frame(y=train_dataset$class , x = train_data_model)

### test1 matrix form 
test1_matrix <- as.matrix(test_dtm)
#test1_matrix <- binary.weight(test1_matrix)
test1_data_model <- as.data.frame(test1_matrix)
##test1_data_model

test1_data_model <-  data.frame(y=testdataset$class , x = test1_data_model)


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






#---------------------------------------------------------------------------------------------------------------
library(FSelector)

train_chi <- as.data.frame(train_matrix)
train_chi$y <- train_dataset$class
chi_square <- chi.squared(y~.,data = train_chi  )

subset <- cutoff.k(chi_square , 900)
train_data_chi <- data.frame(y=train_chi$y , x = train_chi[subset])
test1_data_model <- as.data.frame(test1_matrix)
##test1_data_model
test1_data_model <-  data.frame(y=testdataset$class , x = test1_data_model[subset])

library(caret)
library(doParallel)


cl <- makePSOCKcluster(5)
registerDoParallel(cl)
svm <- train(y~.,data = train_data_chi,method = 'svmLinear3')

## When you are done:
stopCluster(cl)


pre <- predict(svm,test1_data_model[,-1])

table(pre,test1_data_model$y)
mmetric(pre,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

#-----------------------------------------------------------------------------------------------------

train_info <- as.data.frame(train_matrix)
train_info$y <- train_dataset$class
IG_weight <- information.gain(y~., data = train_info)

subset_info <- cutoff.k(IG_weight,1200)
train_data_info <- data.frame(y =train_info$y, x= train_info[subset_info] )
test1_data_model <- as.data.frame(test1_matrix)
test1_data_model <-  data.frame(y=testdataset$class , x = test1_data_model[subset_info])

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







