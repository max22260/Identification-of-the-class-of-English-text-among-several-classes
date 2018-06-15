# ====================================================================================================
# title: preprocssing & fit bag Of word and reduction dim by remove sparce term fit deffirant 
#        type of classification algorithms     
#-----------------------------------------------------------------------------------------------------
# author:   ahmed nagy radwan
#-----------------------------------------------------------------------------------------------------
# summary: fit preprocessing and bag of word and use remove sparce term function to remove 
#          noise feature and fit classification algorithm ,and calculate accuracy to every model
#-----------------------------------------------------------------------------------------------------
# output :  accuracy of sevral model
# ====================================================================================================


TRAINDATA <- read.csv(file = "/home/max/Desktop/Identification of the class of English text among several classes/analysis/scripts/Dataset.csv")

load(file = "/home/max/Desktop/test/bbc_DataMatrix.RData")

bbc.data.matrix$content <-iconv(bbc.data.matrix$content,"WINDOWS-1252","UTF-8")

write.csv(x = bbc.data.matrix,"/home/max/Desktop/bbc.csv",row.names = FALSE)
############################# load packeges
library(SnowballC)
library(textreadr)
library(wordcloud)
library(rpart)
library(caret)
library(tm)
library(MASS)
#library(RWeka)
library(rminer)
library(kernlab)
#############################

plot <-table(bbc.data.matrix[,3])
barplot(plot, main = "BBC DataSet"  , xlab = "class" , ylim = c(0,800) , col = "lightblue")


#-------------------------------- summrize our data ----------------------------------------------------

str(bbc.data.matrix) #structure of data
summary(bbc.data.matrix) #sumrry for data
nrow(bbc.data.matrix) 
prop.table(table(bbc.data.matrix$class)) # lable probability 

#-------------------------------------------------------------------------------------------------------


#--------------------------- prepare training_dataset and test1_dataset , test2_dataset -----------------
set.seed(100) # for randmnes

trian <- createDataPartition(y=bbc.data.matrix$class,p=0.70 , list = FALSE)
train_dataset <- bbc.data.matrix[trian,]
testdata <- bbc.data.matrix[-trian,]


test_dataset <-createDataPartition(y=testdata$class,p=0.50 , list = FALSE)
test1 <- testdata[test_dataset,]
test2 <- testdata[-test_dataset,]

#-------------------------------------------------------------------------------------------------------- 
#save data 
save(train_dataset,file = "/home/max/Desktop/test/train_dataset.RData")
save(test1 ,file = "/home/max/Desktop/test/test1.RData")
save(test2,file = "/home/max/Desktop/test/test2.RData")
#--------------------------------------------------------------------------------------------------------
###############################################################################################################################
#load data 
load("/home/max/Desktop/test/train_dataset.RData")
load("/home/max/Desktop/test/test1.RData")
load("/home/max/Desktop/test/test2.RData")
###############################################################################################################################

#----------------------------------------------------- data preprocessing ----------------------------------------------------#

train_dataset$content <- iconv(train_dataset$content,"WINDOWS-1252","UTF-8")


train_corpus <- Corpus(VectorSource(train_dataset$content))
length(train_corpus)


train_corpus[["1"]][["content"]]
train_corpus[["2"]][["content"]]
train_corpus[["3"]][["content"]]
train_corpus[["4"]][["content"]]
#########################################step1

step1 <-tm_map(train_corpus,tolower)#put all char in corpus as lowercase

step1[["1"]][["content"]]
step1[["2"]][["content"]]
step1[["3"]][["content"]]
step1[["4"]][["content"]]
inspect(head(step1,5))

##################################################step2

step2 <-tm_map(step1,removeNumbers)
step2[["1"]][["content"]]
inspect(step2[1:2])
inspect(head(step2,5))

#############################################################step3

step3 <-tm_map(step2,removeWords,stopwords("english"))

step3[["1"]][["content"]]
inspect(step3[1:2])
inspect(head(step3,5))

stopwordlist<- c(stopwords("english"),"one","two" ,"done","now",
                 "tree","four","five","six","sven","eight","nine",
                 "teen", "new","also","year","just","get","told",
                 'bbc',"get","find","news","much","use","meny","can"
                 ,"songs","make","br","will","said","also","year")

step3.1 <-tm_map(step3,removeWords,stopwordlist)
inspect(step3.1[1:2])
inspect(head(step3.1,10))


############################################################## step4
step4 <- tm_map(step3.1,removePunctuation)
inspect(head(step4,10))

##################################################### step5
step5 <- tm_map(step4,stripWhitespace)
inspect(head(step5,10))

#--------------------------------------stemming-------------------------------------

#test stemmer 

testcase <- c("Do you really think it is weakness that yields to temptation?"," I tell you that there are terrible 
              temptations which ","it requires strength"," strength and courage to yield to ~ Oscar Wilde")

corpus1 <- Corpus(VectorSource(testcase))
test_result<-tm_map(corpus1,stemDocument,language = "english")

stem
step6<-tm_map(step5,stemDocument,language = "english")
#step6.1 <-tm_map(step6,stemCompletion(step6,dictionary = step5))
inspect(head(step6,10))

#------------------------------------ document term matrex ----------------------------

stopwordlist<- c(stopwords("english"),"one","two" ,"done","now",
                 "three","four","five","six","sven","eight","nine",
                 "teen", "new","also","year","just","get","told",
                 'bbc',"get","find","news","much","use","meny","can"
                 ,"make","br","will","said","also","year","bbc")

train_corpus<-tm_map(train_corpus,removeWords,stopwordlist)

train_dtm <-DocumentTermMatrix(train_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                            , stripWhitespace = T   ))
# stemming = T 
dim(train_dtm)

traing_dtm1 = DocumentTermMatrix(step6)

train_dtm_srt <- removeSparseTerms(train_dtm,0.99) #dimensionality reduction


dim(train_dtm_srt)

########################################################################### avrage frequency of most frequent words
mean_train =sort(colMeans(as.matrix(train_dtm_srt)),decreasing = T)
mean_train[1:20] # top 20  most frequent words
mean_train[1:40] # top 40  most frequent words
mean_train[1:10] # top 10  most frequent words
##########################################################################
average_top20=mean(mean_train[1:40]) # the average frequency of these word 
average_top20
################## plot data
barplot(mean_train[1:50],border = NA , las =3 ,  ylab = "frequency" ,space = F)

wordcloud( names(mean_train[1:40]),mean_train[1:40] ,colors = brewer.pal(8,"Dark2"))


#----------------------------------------------------------------------------------- train data model

train_matrix <- as.matrix(train_dtm_srt)
train_data_model <- as.data.frame(train_matrix)
train_data_model$class <-c( train_dataset$class) 



train_data_model <- data.frame(y=train_dataset$class , x = train_matrix)
### summrize
str(train_data_model)
prop.table(table(train_data_model$y))
nrow(train_data_model)
summary(train_data_model)

####################################################### save bag of word 

train_bag_of_word <- findFreqTerms(train_dtm_srt)

length(train_bag_of_word)

############################ generate test1_data_model &test2_data_model  
test1$content <- iconv(test1$content,"WINDOWS-1252","UTF-8")

test1_corpus <- Corpus(VectorSource(as.matrix(test1$content)))
test1_corpus[["1"]][["content"]]
test1_corpus[["2"]][["content"]]
test1_corpus[["3"]][["content"]]

############################################ test1 as document term matrix 


test1_dtm <-DocumentTermMatrix(test1_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                            , stripWhitespace = T  ,dictionary = train_bag_of_word))

str(test1_dtm)
dim(test1_dtm)
### test1 matrix form 
test1_matrix <- as.matrix(test1_dtm)
test1_data_model <- as.data.frame(test1_matrix)
test1_data_model$class <- test1$class 
##test1_data_model

test1_data_model <-  data.frame(y=test1$class , x = test1_matrix)

summary(test1_data_model)

######################################################################################### test2 
test2$content <- iconv(test2$content,"WINDOWS-1252","UTF-8")
test2_corpus <- Corpus(VectorSource(as.matrix(test2$content)))
test2_corpus[["1"]][["content"]]
test2_corpus[["2"]][["content"]]
test2_corpus[["3"]][["content"]]

############################################ test2 as document term matrix 

test2_dtm <-DocumentTermMatrix(test2_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                            , stripWhitespace = T , dictionary = train_bag_of_word))

str(test2_dtm)
dim(test2_dtm)
### test1 matrix form 
test2_matrix <- as.matrix(test2_dtm)

##test1_data_model

test2_data_model <-  data.frame(y=test2$class , x = test2_matrix)

summary(test2_data_model)
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#plot data
packages <- c('Rtsne', 'ggplot2', 'plotly','tsne')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library('Rtsne')
library('ggplot2')
library('plotly')
library('tsne')


features <- train_data_model[, !names(train_data_model) %in% c("y")]


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
  ,colors = c("#FF0000FF", "#CCFF00FF", "#00FF66FF" ,"#0066FFFF" ,"#CC00FFFF")
  ,symbols = c("cross", "square", "triangle-down")
) %>% layout(xaxis = ax, yaxis = ax)



#*********************************** SVM **************************************
library(caret)
library(e1071)

data.train = data.frame(x=embedding[,1:2] , y = as.factor(embedding$Class))

svm_model_plot = svm(data.train$y ~ . , data = data.train , 
                type = "C-classification" , kernel = 'radial')
n = plot(svm_model_plot,data.train)




tune.out =  tune(svm_model_plot,$y~ .,data = data.train ,kernel ="radial",
             ranges = list(cost =c(0.1,1,10,100,1000),gamma= c(0.1,2)))


#---------------------------------------------- classification -----------------------------------
library(class)
library(e1071)
library(glmnet)
library(party)
#************************************* DICISION TREE *******************************
dtree_model <- ctree(y ~ . ,data = train_data_model)
summary(dtree_model)
plot(dtree_model)
plot(dtree_model,type = "simple")

########################## prediction process 


test1pred = predict(dtree_model,newdata = test1_data_model )

summary(test1pred)
prop.table(table(test1pred))

############################################## testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(test1pred,test1_data_model[,1],positive = "positive", dnn = c ("prediction","true"))


# second accuracy

mmetric(test1pred,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

#*************************************** NAIVE BAYES CLASSIFIER ***************************

library(e1071)

naiveB_model = naiveBayes( train_data_model$y~.,data =train_data_model[,-1] )

########################## prediction process 

naiveB_test1pred = predict(naiveB_model,test1_data_model )

summary(naiveB_test1pred)
prop.table(table(naiveB_test1pred))

############################################## testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(naiveB_test1pred,test1_data_model$y,positive = "positive", dnn = c ("prediction","true"))

table(naiveB_test1pred,test1$class)

# second accuracy

mmetric(naiveB_test1pred,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

#***************************** k-nearest neighbour classification *************************************************

library(class)

knn_model = knn(train = train_data_model[,-1] , test = test1_data_model[,-1] , cl = train_data_model[,1] ,k = 2 )

summary(knn_model)
prop.table(table(knn_model))

############################################## testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(knn_model,test1_data_model[,1],positive = "positive", dnn = c ("prediction","true"))

# second accuracy

mmetric(knn_model,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))
#*********************************** SVM **************************************
library(caret)
library(e1071)

svm_model = svm(formula = y ~ . , data = train_data_model , 
                type = "C-classification" , kernel = 'linear',cost = 0.25 ,loss = L1)

library(caret)

svm_classifier = train(form = y ~ . , data = train_data_model ,
                       method = 'svmRadial' ,sigma =  0.005933641 )  

########################## prediction process 

SVM_test1pred = predict(svm_model,newdata = test1_data_model )
summary(SVM_test1pred)
prop.table(table(SVM_test1pred))

############################################## testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(SVM_test1pred,test1_data_model[,1],positive = "positive", dnn = c ("prediction","true"))


# second accuracy

mmetric(SVM_test1pred,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))






#***************************** kernal SVM **************************************

library(kernlab)

KSVM_model = ksvm( y~ . , data =train_data_chi )


########################## prediction process 

KSVM_test1pred = predict(KSVM_model,newdata = test1_data_model  )
KSVM_test2pred = predict(KSVM_model,newdata = test2_data_model )
summary(KSVM_test1pred)
summary(KSVM_test2pred)

prop.table(table(KSVM_test1pred))

############################################## testing and evaluate the prediction

#first confusion matrix 

confusionMatrix(KSVM_test1pred,test1_data_model[,1],positive = "positive", dnn = c ("prediction","true"))

confusionMatrix(KSVM_test2pred,test2_data_model[,1],positive = "positive", dnn = c ("prediction","true"))


# second accuracy

mmetric(KSVM_test1pred,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

mmetric(KSVM_test2pred,test2_data_model[,1],c("ACC","TPR","PRECISION","F1"))


#******************************************** random forest classifier ******************************************
install.packages("ggplot2")
install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(x = train_data_chi[,-1],y = train_data_chi$y , ntree = 180 )


########################## prediction process 

rf_test1pred = predict(rf_model,newdata = test1_data_model[,-1] )
rf_test2pred = predict(rf_model,newdata = test2_data_model[,-1] )
summary(rf_test1pred)
prop.table(table(rf_test1pred))

############################################## testing and evaluate the prediction

#first confusion matrix 


table(rf_test1pred,test1_data_model[,1])


confusionMatrix(rf_test1pred,test1_data_model[,1],positive = "positive", dnn = c ("prediction","true"))

confusionMatrix(rf_test2pred,test2_data_model[,1],positive = "positive", dnn = c ("prediction","true"))

# second accuracy

mmetric(rf_test1pred,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

mmetric(rf_test2pred,test2_data_model[,1],c("ACC","TPR","PRECISION","F1"))

plot(rf_model)
#===========================================================================================

#==============================================================================================================




stem_dtm <- function(dtm, n_char)
{
  #dtm is a document term matrix
  #nchar is minimum number of characters in a predictor - 1
  stem <- function(word)
  {
    common_endings <- c("*ed$", "*ing$", "*s$", "*es$", 
                        "*ly$", "*ary$", "*self$", "*ful$", 
                        "*less$","*ment$", "*er$", "*ance$",
                        "*al$", "*ent$", "*sion$", "*tion$",
                        "*ance$", "*or$", "*ive$", "*ise$")
    
    # remove common endings
    for(i in 1:length(common_endings)){word <- sub(common_endings[i], "", word)}
    
    return(word)
  }
  
  predictors <- colnames(dtm)
  stemmed_predictors <- stem(colnames(dtm))
  duplicated_terms <- stemmed_predictors[duplicated(stemmed_predictors, 
                                                    incomparables = FALSE)]
  duplicated_terms <- unique(duplicated_terms[nchar(duplicated_terms) > n_char])
  
  stemmed_dtm <- matrix(NA, 
                        nrow = nrow(dtm), 
                        ncol=length(duplicated_terms))
  
  for(i in 1:length(duplicated_terms))
  {
    # find columns of duplicated terms
    duplicated_columns <- grep(duplicated_terms[i], predictors)
    
    # add them
    replacement_column <- rowSums(dtm[,duplicated_columns])
    
    # add the column to a replacement matrix
    stemmed_dtm[,i] <- replacement_column
    
    
  }
  
  print("Made DTM")
  colnames(stemmed_dtm) <- duplicated_terms
  
  #making it binary
  #stemmed_dtm <- (stemmed_dtm > 0)+0
  
  return(stemmed_dtm)
}

stm_train_matrix <- as.matrix(train_dtm)
stm_train_data_model <- as.data.frame(train_matrix)

stmQ <- as.DocumentTermMatrix(stm_train_data_model ,weighting = 1)
stm_train_bag_of_word <- findFreqTerms(stmQ)
length(stm_train_bag_of_word)

stm_mean_train =sort(colMeans(as.matrix(stm)),decreasing = T)
barplot(stm_mean_train[1:60],border = NA , las =3 ,  ylab = "frequency" , ylim = c(0,500),space = F)



stm_test1_dtm <-DocumentTermMatrix(test1_corpus,
                                   control = list(tolower = T 
                                  , removeNumbers =T ,removePunctuation = T 
                                  , stopwords = T , stripWhitespace = T ,
                                  dictionary = stm_train_bag_of_word )
                                  )
dim(stm_test1_dtm)
### test1 matrix form 
stm_test1_matrix <- as.matrix(test1_dtm)
stm_test1_data_model <- as.data.frame(test1_matrix)
##test1_data_model


stm <- stem_dtm(stm_train_data_model,1)
stm1 <-stem_dtm(stm_test1_data_model,1) 



stm_train_data_model <- data.frame(y=train_dataset$class , x = stm)
stm_test1_data_model <-  data.frame(y=test1$class , x =stm1)

dim(stm_train_data_model)
dim(stm_test1_data_model)

stm_KSVM_model = ksvm( stm_train_data_model$y~ . , data =stm_train_data_model)
########################## prediction process 
stm_KSVM_test1pred = predict(stm_KSVM_model,newdata = stm_test1_data_model)
############################################## testing and evaluate the prediction
#first confusion matrix 
#confusionMatrix(stm_KSVM_test1pred,stm_test1_data_model[,1],positive = "positive", dnn = c ("prediction","true"))
# second accuracy
t<-table(stm_KSVM_test1pred,stm_test1_data_model[,1])
mmetric(stm_KSVM_test1pred,stm_test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))
#-------------------------------------------------------------------------------------------------------



t <- as.data.frame(t)
colnames(t)[] <-c("TClass","PClass","Y") 

library(ggplot2)
ggplot(data =  t, mapping = aes(y =TClass , x =PClass)) +
  geom_tile(aes(fill=Y))  +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1 ) +
  scale_fill_gradient(low = "white", high = "#9A3E90") +
  theme_bw() + theme(legend.position = "none")




#--------------------------------------------------------------------------------------------------------------




trian <- createDataPartition(y=TRAINDATA$Class,p=0.70 , list = FALSE)
train_dataset <- TRAINDATA[trian,]
testdata <- TRAINDATA[-trian,]
train_dataset$Document <- iconv(train_dataset$Document,"WINDOWS-1252","UTF-8")

train_corpus <- Corpus(VectorSource(train_dataset$Document))

testdata$Document <- iconv(testdata$Document,"WINDOWS-1252","UTF-8")

test_corpus <- Corpus(VectorSource(testdata$Document))

train_dtm <-DocumentTermMatrix(train_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                            , stripWhitespace = T   ))

train_dtm_srt <- removeSparseTerms(train_dtm,0.99) 
train_bag_of_word <- findFreqTerms(train_dtm_srt)

test_dtm <-DocumentTermMatrix(test_corpus, control = list(tolower = T , removeNumbers =T ,removePunctuation = T , stopwords = T 
                                                          , stripWhitespace = T , dictionary = train_bag_of_word  ))




train_matrix <- as.matrix(train_dtm_srt)
train_data_model <- as.data.frame(train_matrix)
train_data_model$class <-c( train_dataset$class) 



train_data_model <- data.frame(y=train_dataset$Class , x = train_matrix)

### test1 matrix form 
test1_matrix <- as.matrix(test_dtm)
test1_data_model <- as.data.frame(test1_matrix)
test1_data_model$class <- testdata$Class 
##test1_data_model

test1_data_model <-  data.frame(y=testdata$Class , x = test1_matrix)


library(caret)


svm <- train(y~.,data = train_data_model,method = 'svmLinear3')

svm1 <- train(y~.,data = train_data_model,method = 'svmRadial')


pre <- predict(svm,test1_data_model[,-1])

table(pre,test1_data_model$y)
mmetric(pre,test1_data_model[,1],c("ACC","TPR","PRECISION","F1"))

