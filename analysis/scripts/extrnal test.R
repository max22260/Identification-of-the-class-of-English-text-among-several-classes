
exdata <- read.dir("/home/max/Desktop/ex/",".TXT")

exdata$content <- iconv(exdata$content,"WINDOWS-1252","UTF-8")

ex_corpus <- Corpus(VectorSource(as.matrix(exdata$content)))
ex_corpus[["1"]][["content"]]
ex_corpus[["2"]][["content"]]
ex_corpus[["3"]][["content"]]

############################################ test1 as document term matrix 


ex_dtm <-DocumentTermMatrix(ex_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                            , stripWhitespace = T  ,dictionary = train_bag_of_word))



ex_dtm<-preProcess(row.data = exdata$content ,stopword.dir = "test.txt",BagOfWord = BagOW , TRUE )
str(ex_dtm)
dim(ex_dtm)
### test1 matrix form 
ex_matrix <- as.matrix(ex_dtm)
ex_data_model <- as.data.frame(ex_matrix)
##test1_data_model
dim(ex_data_model)

ex_data_model <- data.frame(x=ex_data_model[subset_info])

ex_pred = predict(svm,newdata = ex_data_model )

for(i in 1:length(ex_corpus) ){
 print( ex_corpus[[i]][["content"]] )
  print(": ") 
 print( ex_pred[i])
  
}



