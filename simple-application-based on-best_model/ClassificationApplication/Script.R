options(warn=-1)
setwd("/home/max/Desktop/ClassificationApplication/")
load(file = "./R_Workspace/BestModel_data_matrix.RData")
svm<-readRDS(file =  "./R_Workspace/best-model.rda")
preProcess <- function(row.data, stopword.dir, BagOfWord , boolstemm){
  
  packages <- c('tm')
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  library(NLP)
  library(tm)
  row.data<-iconv(row.data,"WINDOWS-1252","UTF-8")
  
  stopwordlist<- readLines(stopword.dir)
  train_corpus<- Corpus(VectorSource(row.data))
  #train_corpus <-tm_map(train_corpus,tolower)
  #train_corpus<-tm_map(train_corpus,removeNumbers)
  #train_corpus<-tm_map(train_corpus,removeWords,stopwords("english"))
  #train_corpus<-tm_map(train_corpus,removeWords,stopwordlist)
  #train_corpus<- tm_map(train_corpus,removePunctuation)
  #train_corpus<-tm_map(train_corpus,stripWhitespace)
  
  if(boolstemm){
    
   # train_corpus<-tm_map(train_corpus,stemDocument,language = "english")
  }
  
  DTM <-DocumentTermMatrix(train_corpus,
                           control = list(tolower = T ,
                                          removeNumbers =T ,
                                          removePunctuation = T ,
                                          stopwords = T 
                                          , stripWhitespace = T ,dictionary = BagOfWord  ))
  

  return(DTM)
}

test = readLines("./R_Workspace/content.txt")
ex_dtm<-preProcess(row.data = test ,stopword.dir = "./R_Workspace/stopwords.txt",BagOfWord = BagOW , TRUE )
ex_matrix <- as.matrix(ex_dtm)
ex_data_model <- as.data.frame(ex_matrix)
ex_data_model <- data.frame(x=ex_data_model)
ex_pred = predict(svm,newdata = ex_data_model  )
category = as.vector(ex_pred)
print(category)