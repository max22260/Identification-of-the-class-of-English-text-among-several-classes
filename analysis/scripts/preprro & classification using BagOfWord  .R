train_data_model <- (train_data_model > 0)+0
test <- (test1_data_model > 0)+0


load(file = "/home/max/Desktop/test/bbc_DataMatrix.RData")
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
prop.table(table(bbc.data.matrix$lable)) # lable probability 

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

train_dataset$content <- iconv(trainda$content,"WINDOWS-1252","UTF-8")


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
traing_dtm <- as.matrix(traing_dtm_srt)

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
barplot(mean_train[1:100],border = NA , las =3 ,  ylab = "frequency" , ylim = c(0,1.5),space = F)

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





#---------------------------------------------- classification -----------------------------------

stm <- stem_dtm(train_data_model[,-1],3)
stm1 <-stem_dtm(test1_data_model[,-1],1) 

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

confusionMatrix(naiveB_test1pred,test1$class,positive = "positive", dnn = c ("prediction","true"))

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

svm_model = svm(formula =train_data_model$y ~ . , data = train_data_model , 
                type = "C-classification" , kernel = 'radial')

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

KSVM_model = ksvm( train_data_model$y~ . , data =train_data_model)


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

rf_model <- randomForest(x = train_data_model[,-1],y = train_data_model$y , ntree = 60)


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

rainbow(5)


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



remove.packages("ggplot2")
install.packages('ggplot2', dependencies = TRUE)
#-------------------------------------------------------------------------------------------------------
mmetric=function(y,x=NULL,metric,D=0.5,TC=-1,val=NULL,aggregate="no")
{ 
  if(is.list(y) && is.null(x)) # mining object
  { # special metrics: sum all?
    if(sum(metric=="CONF")>0) CONF=TRUE else CONF=FALSE
    if(sum(metric=="ROC")>0) ROC=TRUE else ROC=FALSE
    if(sum(metric=="LIFT")>0) LIFT=TRUE else LIFT=FALSE
    if(sum(metric=="REC")>0) REC=TRUE else REC=FALSE
    R=y$runs;res=NULL
    if(is.function(metric)) LM=1 else LM=length(metric)
    if(aggregate=="no") 
    {
      if(CONF||ROC||LIFT||REC){res=vector("list",length=R); for(i in 1:R) res[[i]]=mmetric(y$test[[i]],y$pred[[i]],metric=metric,D=D,TC=TC,val=val)}
      #else if(LM==1) 
      #    { res=vector(length=R); for(i in 1:R) res[i]=mmetric(y$test[[i]],y$pred[[i]],metric=metric,D=D,TC=TC,val=val) }
      else{ 
        res=matrix(nrow=R,ncol=LM)
        for(i in 1:R) { if(i==1){ aux=mmetric(y$test[[i]],y$pred[[i]],metric=metric,D=D,TC=TC,val=val);
        res=matrix(nrow=R,ncol=length(aux))
        res[i,]=aux
        }
          else res[i,]=mmetric(y$test[[i]],y$pred[[i]],metric=metric,D=D,TC=TC,val=val) 
        }
        if(length(names(aux))==0) names(aux)=metric[1] # new line
        res=data.frame(res)
        if(LM==1 && metric!="ALL" && !is.factor(y$test[[1]][1])) {C=length(aux);naux=vector(length=C);for(i in 1:C) naux[i]=paste(metric,i,sep="");names(res)=naux;}
        else names(res)=names(aux)
      }
    }
    else if(aggregate=="sum"||aggregate=="mean") 
    {
      # does not work for roc, lift and rec! 
      for(i in 1:R) 
      {
        aux=mmetric(y$test[[i]],y$pred[[i]],metric=metric,D=D,TC=TC,val=val)
        if(LM==1 && CONF)
        {
          if(i==1)
          {
            res=aux$conf
          }
          else{ res=res+aux$conf }
        }
        else{ if(i==1) res=aux else res=res+aux }
      }
      if(aggregate=="mean") res=res/R
    }
    return(res)
  }
  else if(is.function(metric)) return(metric(y,x)) # resmode=1
  else if(NCOL(y)>1) # ranking 
  {
    res=NULL;nres=NULL;
    if(length(metric)==1 && metric=="ALL") metric=c("KENDALL","SPEARMAN")
    if(sum(metric=="KENDALL")>0) KENDALL=TRUE else KENDALL=FALSE # -1 to 1
    if(sum(metric=="SPEARMAN")>0) SPEARMAN=TRUE else SPEARMAN=FALSE
    LM=length(metric)
    if(LM==1) rsingle=TRUE else rsingle=FALSE
    
    C=NCOL(y);Total=NROW(y) 
    if(KENDALL) kendall=0
    if(SPEARMAN) spearman=0
    for(k in 1:Total)
    {
      if(KENDALL) kendall=kendall+cor(y[k,],x[k,],method="kendall")
      if(SPEARMAN) spearman=spearman+cor(y[k,],x[k,],method="spearman")
    }
    if(KENDALL){kendall=kendall/Total;if(rsingle) return(kendall) else{res=c(res,kendall);nres=c(nres,"KENDALL")}} else kendall=NULL
    if(SPEARMAN){spearman=spearman/Total;if(rsingle) return(spearman) else{res=c(res,spearman);nres=c(nres,"SPEARMAN")}} else spearman=NULL
    
    if(!is.null(res)) {names(res)=nres; 
    # sort res:
    I=NULL # for regression, this works perfectly?
    for(i in 1:LM)
    {
      ii=which(nres==metric[i])[1]
      if(!is.na(ii)) I=c(I,ii) 
    }
    res=res[I]
    }
    return(res)
  }
  else if(is.factor(y)) # classification
  {
    res=NULL;nres=NULL;
    if(length(metric)==1 && metric=="ALL")
    { 
      if(is.ordered(y)) metric=c("MAEO","MSEO","KENDALL")
      else if(is.factor(x)) metric=c("ACC","CE","BER","KAPPA","CRAMERV","ACCLASS","TPR","TNR","PRECISION","F1","MCC") 
      else metric=c("ACC","CE","BER","KAPPA","CRAMERV","ACCLASS","TPR","TNR","PRECISION","F1","MCC","BRIER","BRIERCLASS","AUC","AUCCLASS","NAUC","TPRATFPR","ALIFT","NALIFT","ALIFTATPERC")
    }
    LM=length(metric)
    if(sum(metric=="CONF")>0) CONF=TRUE else CONF=FALSE
    if(sum(metric=="ROC")>0) ROC=TRUE else ROC=FALSE
    if(sum(metric=="LIFT")>0) LIFT=TRUE else LIFT=FALSE
    if(sum(metric=="ACC")>0) ACC=TRUE else ACC=FALSE
    if(sum(metric=="CE"|metric=="MER")>0) CE=TRUE else CE=FALSE
    if(sum(metric=="MAEO")>0) MAEO=TRUE else MAEO=FALSE
    if(sum(metric=="MSEO")>0) MSEO=TRUE else MSEO=FALSE
    if(sum(metric=="KENDALL")>0) KENDALL=TRUE else KENDALL=FALSE
    if(sum(metric=="BER")>0) BER=TRUE else BER=FALSE 
    if(sum(metric=="KAPPA")>0) KAPPA=TRUE else KAPPA=FALSE
    if(sum(metric=="CRAMERV")>0) CRAMERV=TRUE else CRAMERV=FALSE 
    
    if(sum(metric=="ACCLASS")>0) ACCLASS=TRUE else ACCLASS=FALSE
    if(sum(metric=="TPR")>0) TPR=TRUE else TPR=FALSE 
    if(sum(metric=="TNR")>0) TNR=TRUE else TNR=FALSE 
    if(sum(metric=="PRECISION")>0) PRECISION=TRUE else PRECISION=FALSE 
    if(sum(metric=="F1")>0) F1=TRUE else F1=FALSE 
    if(sum(metric=="MCC")>0) MCC=TRUE else MCC=FALSE
    
    if(!is.factor(x)) # prob
    {
      if(sum(metric=="BRIER")>0) BRIER=TRUE else BRIER=FALSE 
      if(sum(metric=="BRIERCLASS")>0) BRIERCLASS=TRUE else BRIERCLASS=FALSE 
      if(sum(metric=="AUC")>0) AUC=TRUE else AUC=FALSE 
      if(sum(metric=="AUCCLASS")>0) AUCCLASS=TRUE else AUCCLASS=FALSE 
      if(sum(metric=="NAUC")>0) NAUC=TRUE else NAUC=FALSE 
      if(sum(metric=="TPRATFPR")>0) TPRATFPR=TRUE else TPRATFPR=FALSE 
      if(sum(metric=="ALIFT")>0) ALIFT=TRUE else ALIFT=FALSE 
      if(sum(metric=="NALIFT")>0) NALIFT=TRUE else NALIFT=FALSE 
      if(sum(metric=="ALIFTATPERC")>0) ALIFTATPERC=TRUE else ALIFTATPERC=FALSE 
    } 
    
    if(LM==1) rsingle=TRUE else rsingle=FALSE
    if(CONF||ROC||LIFT) reslist=TRUE # list
    else reslist=FALSE# named vector
    
    if(TC>0) C=2 else C=length(levels(y[1]))
    Total=length(y)
    if(CONF||ACC||CE||MAEO||MSEO||BER||KAPPA||CRAMERV||KENDALL||ACCLASS||TPR||TNR||PRECISION||F1||MCC) # conf
    { conf=Conf(y,x,D=D,TC=TC,predreturn=TRUE)
    if(CRAMERV||KENDALL) pred=conf$pred
    conf=conf$conf
    } else conf=NULL
    
    if(ACC||CE||KAPPA){diag=0;diagr=rep(0,C);}
    if(MAEO) maeo=0 else maeo=NULL
    if(MSEO) mseo=0 else mseo=NULL
    if(BER) ber=vector(length=C)
    if(ACCLASS) acclass=rep(0,C) else acclass=NULL
    if(TPR||F1) tpr=rep(0,C) else tpr=NULL
    if(TNR) tnr=rep(0,C) else tnr=NULL
    if(PRECISION||F1) precision=rep(0,C) else precision=NULL
    if(MCC) mcc=rep(0,C) else mcc=NULL
    if(F1) f1=rep(0,C) else f1=NULL
    if(ACC||CE||MAEO||MSEO||KAPPA||BER||ACCLASS||TPR||TNR||PRECISION||F1||MCC)
    {
      for(k in 1:C) 
      { 
        if(ACC||CE||KAPPA) diag=diag+conf[k,k]
        if(KAPPA||BER) sum_conf_k=sum(conf[k,])
        if(KAPPA) diagr[k]=sum_conf_k*(sum(conf[,k])/Total)
        if(MAEO||MSEO) { for(i in 1:C) 
        {
          err=conf[k,i]*(i-k)
          if(MAEO) maeo=maeo+abs(err)
          if(MSEO) mseo=mseo+(err)^2
        }
        }
        if(BER) ber[k]=sum(conf[k,-k])/sum_conf_k
        if(ACCLASS||TPR||TNR||PRECISION||F1||MCC)
        {TP=conf[k,k]
        FN=0
        for(i in 1:C) # iterator?
          if(i!=k) FN=FN+conf[k,i]
        FP=0
        for(i in 1:C) # iterator?
          if(i!=k) FP=FP+conf[i,k]
        TN=Total-TP-FN-FP
        if(ACCLASS) acclass[k]=100*(TP+TN)/Total 
        if((TPR||F1) && TP!=0) tpr[k]=100*TP/(FN+TP)
        if(TNR && TN!=0) tnr[k]=100*TN/(TN+FP) 
        if((PRECISION||F1) && TP!=0) precision[k]=100*TP/(TP+FP)
        if(F1 && precision[k]!=0 && tpr[k]!=0) f1[k]=2*((precision[k]*tpr[k])/(precision[k]+tpr[k]))
        if(MCC) { mcc[k]=TP*TN+FP*FN; if(mcc[k]!=0) mcc[k]=mcc[k]/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)) else mcc[k]=0 }
        }
      }
    }
    if(ACC||CE) {acc=c(diag/Total)*100; 
    if(rsingle && ACC) return(acc) else if(ACC){res=c(res,acc);nres=c(nres,"ACC")}
    } else acc=NULL # total accuracy, in percentage
    if(CE) {ce=100-acc; if(rsingle) return(ce) else{res=c(res,ce);nres=c(nres,"CE")}} else ce=NULL
    if(KAPPA) {kappa=100*(diag-sum(diagr))/(Total-sum(diagr)); if(rsingle) return(kappa) else{res=c(res,kappa);nres=c(nres,"KAPPA")}} else kappa=NULL # G
    if(BER) {ber=100*mean(ber);if(rsingle) return(ber) else{res=c(res,ber);nres=c(nres,"BER")}}else ber=NULL # G
    if(MAEO){maeo=maeo/Total;if(rsingle) return(maeo) else{res=c(res,maeo);nres=c(nres,"MAEO")}} 
    if(MSEO){mseo=mseo/Total;if(rsingle) return(mseo) else{res=c(res,mseo);nres=c(nres,"MSEO")}} 
    if(CRAMERV) # single
    {
      #T=try(chisq.test(y,pred)$statistic[[1]],silent=TRUE)
      T=suppressWarnings(try(chisq.test(y,pred)$statistic[[1]],silent=TRUE))
      if(!is.numeric(T)) T=0
      cramerv=sqrt(T/(Total*(C-1)))
      if(rsingle) return(cramerv) else{res=c(res,cramerv);nres=c(nres,"CRAMERV")}
    } else cramerv=NULL
    if(KENDALL && is.ordered(y)) 
    { 
      if(!is.ordered(pred)) pred=ordered(pred,levels=levels(y[1]))
      c=0;d=0;et=0;ep=0
      for(k in 1:(Total-1))
        for(i in (k+1):Total)
        {
          if( (y[k]<y[i] && pred[k]<pred[i]) || (y[k]>y[i] && pred[k]>pred[i]) ) c=c+1
          else if( (y[k]<y[i] && pred[k]>pred[i]) || (y[k]>y[i] && pred[k]<pred[i]) ) d=d+1
          else if( y[k]==y[i] && (pred[k]>pred[i]||pred[k]<pred[i]) ) et=et+1
          else if( pred[k]==pred[i] && (y[k]>y[i]||y[k]<y[i]) ) ep=ep+1
          # else # ignore
        }
      kendall=(c-d)/(sqrt(c+d+et)*sqrt(c+d+ep)) 
      if(rsingle) return(kendall) else{res=c(res,kendall);nres=c(nres,"KENDALL")}
    } else kendall=NULL
    
    if( (ACCLASS||TPR||TNR||PRECISION||F1||MCC)||(!is.factor(x)&& (BRIERCLASS||AUCCLASS)) ) naux=vector(length=C) else NAUX=FALSE
    
    if(ACCLASS){if(rsingle) return(acclass) else {res=c(res,acclass); for(i in 1:C) naux[i]=paste("ACCLASS",i,sep=""); nres=c(nres,naux)}}
    if(TPR){if(rsingle) return(tpr) else {res=c(res,tpr); for(i in 1:C) naux[i]=paste("TPR",i,sep=""); nres=c(nres,naux)}}
    if(TNR) {if(rsingle) return(tnr) else {res=c(res,tnr); for(i in 1:C) naux[i]=paste("TNR",i,sep=""); nres=c(nres,naux)}}
    if(PRECISION) {if(rsingle) return(precision) else {res=c(res,precision); for(i in 1:C) naux[i]=paste("PRECISION",i,sep=""); nres=c(nres,naux)}}
    if(F1) {if(rsingle) return(f1) else {res=c(res,f1); for(i in 1:C) naux[i]=paste("F1",i,sep=""); nres=c(nres,naux)}}
    if(MCC) {if(rsingle) return(mcc) else {res=c(res,mcc); for(i in 1:C) naux[i]=paste("MCC",i,sep=""); nres=c(nres,naux)}}
    
    if(!is.factor(x)) # prob
    {
      if(sum(metric=="BRIER")>0) BRIER=TRUE else BRIER=FALSE 
      if(sum(metric=="BRIERCLASS")>0) BRIERCLASS=TRUE else BRIERCLASS=FALSE 
      if(sum(metric=="AUC")>0) AUC=TRUE else AUC=FALSE 
      if(sum(metric=="AUCCLASS")>0) AUCCLASS=TRUE else AUCCLASS=FALSE 
      if(sum(metric=="NAUC")>0) NAUC=TRUE else NAUC=FALSE 
      if(sum(metric=="TPRATFPR")>0) TPRATFPR=TRUE else TPRATFPR=FALSE 
      if(sum(metric=="ALIFT")>0) ALIFT=TRUE else ALIFT=FALSE 
      if(sum(metric=="NALIFT")>0) NALIFT=TRUE else NALIFT=FALSE 
      if(sum(metric=="ALIFTATPERC")>0) ALIFTATPERC=TRUE else ALIFTATPERC=FALSE 
      
      if(BRIER||BRIERCLASS)
      { 
        L=levels(y[1])
        if(TC>0)
        {
          T=as.numeric(y==L[TC])
          tbrier=sum((T-x[,TC])^2)/Total # Brier=MSE
          if(BRIERCLASS) brier=c(tbrier,tbrier) 
        }
        else # TC== -1, all!
        {
          prop=table(y)[]/Total; 
          tbrier=0;brier=vector(length=C) 
          for(i in 1:C) 
          {
            T=as.numeric(y==L[i])
            brier[i]=sum((T-x[,i])^2)/Total # Brier=MSE
            if(prop[i]>0) tbrier=tbrier+prop[i]*brier[i]
          }
        }
        if(BRIER) { if(rsingle) return(tbrier) else {res=c(res,tbrier);nres=c(nres,"BRIER")}}
        if(BRIERCLASS){if(rsingle) return(brier) else{res=c(res,brier); for(i in 1:C) naux[i]=paste("BRIERCLASS",i,sep=""); nres=c(nres,naux)}}
      } else {brier=NULL;tbrier=NULL}
      if(ROC||AUC||AUCCLASS||NAUC||TPRATFPR)
      { if(TC<1) TC2=C else TC2=TC
      if(C>2) {roc=ROCcurve(y,x);tauc=roc$auc;auc=vector(length=C); for(i in 1:C) auc[i]=roc$roc[[i]]$auc;}
      else {roc=twoclassROC(y,x[,TC2],Positive=levels(y[1])[TC2]);tauc=roc$auc;auc=c(tauc,tauc) }
      if(AUC){if(rsingle) return(tauc) else{res=c(res,tauc);nres=c(nres,"AUC")}}
      if(AUCCLASS){if(rsingle) return(auc) else{res=c(res,auc);for(i in 1:C) naux[i]=paste("AUCCLASS",i,sep="");nres=c(nres,naux)}}
      if(NAUC){ if(is.null(val)) val2=1 
      else if(is.list(val)) val2=val[[which(metric=="NAUC")[1]]]
      else if(length(val)>1) val2=val[which(metric=="NAUC")[1]]
      else val2=val
      if(val2>1) val2=1
      if(C>2) roc2=partialcurve(roc$roc[[TC2]]$roc,val2) else roc2=partialcurve(roc$roc,val2)
      nauc=curvearea(roc2,val2)
      if(rsingle) return(nauc) else{res=c(res,nauc);nres=c(nres,"NAUC")}
      } else nauc=NULL
      if(TPRATFPR) {  if(is.null(val)) val2=1 
      else if(is.list(val)) val2=val[[which(metric=="TPRATFPR")[1]]]
      else if(length(val)>1) val2=val[which(metric=="TPRATFPR")[1]]
      else val2=val
      if(val2>1) val2=1
      if(C>2) roc2=partialcurve(roc$roc[[TC2]]$roc,val2)
      else roc2=partialcurve(roc$roc,val2)
      if(is.vector(roc2)) tpratfpr=roc2[2] else tpratfpr=roc2[nrow(roc2),2]
      if(rsingle) return(tpratfpr) else{res=c(res,tpratfpr);nres=c(nres,"TPRATFPR")}
      } else tpratfpr=NULL
      } else {roc=NULL;auc=NULL;tauc=NULL;nauc=NULL;tpratfpr=NULL}
      if(LIFT||ALIFTATPERC||ALIFT||NALIFT)
      {
        if(TC<1) TC2=C else TC2=TC
        lift=LIFTcurve(y,x,TC=TC2) # does not work for more than 2 classes
        if(ALIFT) {alift=lift$area;if(rsingle) return(alift) else{res=c(res,alift);nres=c(nres,"ALIFT")}}
        else alift=NULL
        if(NALIFT) { 
          if(is.null(val)) val2=1 
          else if(is.list(val)) val2=val[[which(metric=="NALIFT")[1]]]
          else if(length(val)>1) val2=val[which(metric=="NALIFT")[1]]
          else val2=val
          if(val2>1) val2=1
          lift2=partialcurve(lift$alift,val2); nalift=curvearea(lift2,val2)
          if(rsingle) return(nalift) else{res=c(res,nalift);nres=c(nres,"NALIFT")}
        } else nalift=NULL
        if(ALIFTATPERC)
        {
          if(is.null(val)) val2=1 
          else if(is.list(val)) val2=val[[which(metric=="ALIFTATPERC")[1]]]
          else if(length(val)>1) val2=val[which(metric=="ALIFTATPERC")[1]]
          else val2=val
          if(val2>1) val2=1
          lift2=partialcurve(lift$alift,val2)
          if(is.vector(lift2)) aliftatperc=lift2[2] else aliftatperc=lift2[nrow(lift2),2]
          if(rsingle) return(aliftatperc) else{res=c(res,aliftatperc);nres=c(nres,"ALIFTATPERC")}
        } else aliftatperc=NULL 
      } else {lift=NULL;alift=NULL;nalift=NULL;aliftatperc=NULL}
    }
    else {roc=NULL;tauc=NULL;auc=NULL;brier=NULL;tbrier=NULL;nauc=NULL;tpratfpr=NULL;lift=NULL;alift=NULL;nalift=NULL;aliftatperc=NULL}
    
    if(!is.null(res)) { names(res)=nres;
    I=NULL # for classification: problem with multi-class!!! XXX
    Lnres=length(nres)
    nmetric=vector(length=Lnres) 
    i=1;k=1;stop=FALSE
    while(!stop)
    {
      m=metric[i] 
      if(m!="ACCLASS"&&m!="TPR"&&m!="TNR"&&m!="PRECISION"&&m!="F1"&&m!="MCC"&&m!="AUCCLASS"&&m!="BRIERCLASS"){nmetric[k]=m;k=k+1;}
      else {
        for(j in 1:C) {nmetric[k]=paste(metric[i],j,sep="");k=k+1;}
      }
      i=i+1;
      if(i>LM) stop=TRUE
    }
    I=NULL
    for(i in 1:Lnres)
    {
      ii=which(nres==nmetric[i])[1]
      if(!is.na(ii)) I=c(I,ii) 
    }
    res=res[I]
    }
    
    if(reslist) {res=list(res=res,conf=conf,roc=roc,lift=lift)}
    return(res)
  } # y factor
  else # regression
  {
    # absolute measures:
    res=NULL;nres=NULL;LM=length(metric)
    if(length(metric)==1 && metric=="ALL") metric=c("SAE","MAE","MdAE","GMAE","MaxAE","NMAE","RAE","SSE","MSE","MdSE","RMSE","GMSE","HRMSE","RSE","RRSE","ME","COR","q2","R2","Q2","NAREC","TOLERANCE","MAPE","MdAPE","RMSPE","RMdSPE","SMAPE","SMdAPE","SMinkowski3","MMinkowski3","MdMinkowski3")
    
    LM=length(metric)
    if(sum(metric=="SAE")>0) SAE=TRUE else SAE=FALSE
    if(sum(metric=="MAE")>0) MAE=TRUE else MAE=FALSE
    if(sum(metric=="MdAE")>0) MdAE=TRUE else MdAE=FALSE
    if(sum(metric=="GMAE")>0) GMAE=TRUE else GMAE=FALSE
    if(sum(metric=="MaxAE")>0) MaxAE=TRUE else MaxAE=FALSE
    if(sum(metric=="RAE")>0) RAE=TRUE else RAE=FALSE
    if(sum(metric=="NMAE")>0) NMAE=TRUE else NMAE=FALSE
    # Square measures:
    if(sum(metric=="SSE")>0) SSE=TRUE else SSE=FALSE
    if(sum(metric=="MSE")>0) MSE=TRUE else MSE=FALSE
    if(sum(metric=="MdSE")>0) MdSE=TRUE else MdSE=FALSE
    if(sum(metric=="RMSE")>0) RMSE=TRUE else RMSE=FALSE
    if(sum(metric=="GMSE")>0) GMSE=TRUE else GMSE=FALSE
    if(sum(metric=="HRMSE")>0) HRMSE=TRUE else HRMSE=FALSE
    if(sum(metric=="RSE")>0) RSE=TRUE else RSE=FALSE
    if(sum(metric=="RRSE")>0) RRSE=TRUE else RRSE=FALSE
    if(sum(metric=="ME")>0) ME=TRUE else ME=FALSE
    # Cor measures:
    if(sum(metric=="COR")>0) COR=TRUE else COR=FALSE
    if(sum(metric=="q2")>0) q2=TRUE else q2=FALSE
    
    if(sum(metric=="R2")>0) R2=TRUE else R2=FALSE
    if(sum(metric=="R22")>0) R22=TRUE else R22=FALSE
    if(sum(metric=="Q2")>0) LQ2=TRUE else LQ2=FALSE
    # REC measures: 
    if(sum(metric=="REC")>0) REC=TRUE else REC=FALSE
    if(sum(metric=="NAREC")>0) NAREC=TRUE else NAREC=FALSE
    if(sum(metric=="TOLERANCE")>0) TOLERANCE=TRUE else TOLERANCE=FALSE
    # forecasting measures:
    if(sum(metric=="MdAPE")>0) MdAPE=TRUE else MdAPE=FALSE
    if(sum(metric=="RMSPE")>0) RMSPE=TRUE else RMSPE=FALSE
    if(sum(metric=="RMdSPE")>0) RMdSPE=TRUE else RMdSPE=FALSE
    if(sum(metric=="MAPE")>0) MAPE=TRUE else MAPE=FALSE
    if(sum(metric=="SMAPE")>0) SMAPE=TRUE else SMAPE=FALSE
    if(sum(metric=="SMdAPE")>0) SMdAPE=TRUE else SMdAPE=FALSE
    if(sum(metric=="MRAE")>0) MRAE=TRUE else MRAE=FALSE
    if(sum(metric=="MdRAE")>0) MdRAE=TRUE else MdRAE=FALSE
    if(sum(metric=="GMRAE")>0) GMRAE=TRUE else GMRAE=FALSE
    if(sum(metric=="THEILSU2")>0) THEILSU2=TRUE else THEILSU2=FALSE
    if(sum(metric=="MASE")>0) MASE=TRUE else MASE=FALSE
    # Minkowski errors
    if(sum(metric=="SMinkowski3")>0) SMINKOWSKI3=TRUE else SMINKOWSKI3=FALSE
    if(sum(metric=="MMinkowski3")>0) MMINKOWSKI3=TRUE else MMINKOWSKI3=FALSE
    if(sum(metric=="MdMinkowski3")>0) MdMINKOWSKI3=TRUE else MdMINKOWSKI3=FALSE
    if(LM==1) rsingle=TRUE else rsingle=FALSE
    if(REC) reslist=TRUE # list
    else reslist=FALSE# named vector
    
    if(SAE||MAE||MdAE||GMAE||MaxAE||NMAE||RAE||MAPE||SMAPE||SMdAPE||MdAPE||ME||SSE||MSE||MdSE||RMSE||RSE||RRSE||GMSE||R22||LQ2||MRAE||MdRAE||GMRAE||RMSPE||RMdSPE||THEILSU2||MASE||SMINKOWSKI3||MMINKOWSKI3||MdMINKOWSKI3) err=y-x
    if(SAE||MAE||MdAE||GMAE||MaxAE||NMAE||RAE||SMAPE||SMdAPE||SMINKOWSKI3||MMINKOWSKI3||MdMINKOWSKI3) eabs=abs(err)
    if(SAE||RAE) {sae=sum(eabs); if(rsingle && SAE) return(sae) else if(SAE){res=c(res,sae);nres=c(nres,"SAE")}} else sae=NULL
    if(MAE||NMAE) {mae=mean(eabs);     if(rsingle && MAE) return(mae) else if(MAE) {res=c(res,mae);nres=c(nres,"MAE")}} else mae=NULL
    if(MdAE) {mdae=median(eabs); if(rsingle) return(mdae) else{res=c(res,mdae);nres=c(nres,"MdAE")}} else mdae=NULL
    if(GMAE) {gmae=prod(eabs)^(1/(length(eabs)));  if(rsingle) return(gmae) else{res=c(res,gmae);nres=c(nres,"GMAE")}} else gmae=NULL
    if(MaxAE) {maxae=max(eabs);  if(rsingle) return(maxae) else{res=c(res,maxae);nres=c(nres,"MaxAE")}} else maxae=NULL
    
    if(SSE||MSE||MdSE||RMSE||GMSE||RSE||RRSE||R22||LQ2||THEILSU2) esqr=(err)^2
    if(SSE||RSE||RRSE||R22||LQ2) {sse=sum(esqr);if(rsingle && SSE) return(sse) else if(SSE){res=c(res,sse);nres=c(nres,"SSE")}} else sse=NULL
    if(MSE||RMSE||THEILSU2) {mse=mean(esqr);if(rsingle && MSE) return(mse) else if(MSE){res=c(res,mse);nres=c(nres,"MSE")}} else mse=NULL
    if(RMSE||THEILSU2) {rmse=sqrt(mse);if(rsingle && RMSE) return(rmse) else if(RMSE){res=c(res,rmse);nres=c(nres,"RMSE")}} else rmse=NULL 
    if(MdSE) {mdse=median(esqr);if(rsingle) return(mdse) else{res=c(res,mdse);nres=c(nres,"MdSE")}} else mdse=NULL
    if(GMSE) {gmse=prod(esqr)^(1/(length(esqr)));if(rsingle) return(gmse) else{res=c(res,gmse);nres=c(nres,"GMSE")}} else gmse=NULL
    if(HRMSE) {hrmse=sqrt( mean((1-(x/y))^2) ) ;if(rsingle) return(hrmse) else{res=c(res,hrmse);nres=c(nres,"HRMSE")}} else hrmse=NULL
    
    if(ME) { me=mean(err);if(rsingle && ME) return(me) else{res=c(res,me);nres=c(nres,"ME")}} else me=NULL
    
    if(RAE||RSE||RRSE||R22||LQ2) {ymean=mean(y)}
    
    if(NMAE){ if(is.null(val)) yrange=diff(range(y)) else yrange=val
    #cat("yrange:",yrange,"\n")
    nmae=100*mae/yrange;
    if(rsingle) return(nmae) else{res=c(res,nmae);nres=c(nres,"NMAE")}
    } else nmae=NULL
    
    if(RAE) {rae=100*sae/sum(abs(y-ymean));if(rsingle) return(rae) else{res=c(res,rae);nres=c(nres,"RAE")}} else rae=NULL
    if(RSE||RRSE||R22||LQ2) {sum_ym_esqr=sum((y-ymean)^2)}
    
    if(RSE) {rse=100*sse/sum_ym_esqr;if(rsingle) return(rse) else{res=c(res,rse);nres=c(nres,"RSE")}} else rse=NULL
    if(RRSE){rrse=100*sqrt(sse/sum_ym_esqr);if(rsingle) return(rrse) else {res=c(res,rrse);nres=c(nres,"RRSE")}} else rrse=NULL
    if(R22) {r22=1-sse/sum_ym_esqr;if(rsingle) return(r22) else {res=c(res,r22);nres=c(nres,"R22")}} else r22=NULL
    # problem with this formulation, check better:
    if(LQ2) {Q2=sse/sum_ym_esqr;if(rsingle) return(Q2) else {res=c(res,Q2);nres=c(nres,"Q2")}} else Q2=NULL
    
    if(MAPE||MdAPE||RMSPE||RMdSPE) pe=err/y
    if(MAPE||MdAPE) ape=abs(pe)
    if(MAPE) {mape=100*mean(ape);if(rsingle) return(mape) else {res=c(res,mape);nres=c(nres,"MAPE")}} else mape=NULL
    if(MdAPE) {mdape=100*median(ape);if(rsingle) return(mdape) else {res=c(res,mdape);nres=c(nres,"MdAPE")}} else mdape=NULL
    
    if(RMSPE||RMdSPE) pe2=pe^2
    if(RMSPE) {rmspe=sqrt(100*mean(pe2));if(rsingle) return(rmspe) else {res=c(res,rmspe);nres=c(nres,"RMSPE")}} else rmspe=NULL
    if(RMdSPE) {rmdspe=sqrt(100*median(pe2));if(rsingle) return(rmdspe) else {res=c(res,rmdspe);nres=c(nres,"RMdSPE")}} else rmdspe=NULL
    
    if(SMAPE||SMdAPE) map=eabs/(abs(x)+abs(y)) 
    if(SMAPE) {smape=200*mean(map); if(rsingle) return(smape) else {res=c(res,smape);nres=c(nres,"SMAPE")}} else smape=NULL
    if(SMdAPE){smdape=200*median(map); if(rsingle) return(smdape) else {res=c(res,smdape);nres=c(nres,"SMdAPE")}} else smdape=NULL
    
    # same val for all: randomwalk, see Hyndman paper
    if(MRAE||MdRAE||GMRAE||THEILSU2) { if(!is.null(val)) { if(is.list(val))  val2=val[[which(metric=="MRAE"|metric=="MdRAE"|metric=="GMRAE"|metric=="THEILSU2")[1]]]
    else val2=val
    if(length(val2)==1) val2=c(val2,y[1:(length(y)-1)])
    errb=y-val2
    RESULT=TRUE
    } else RESULT=FALSE
    } else RESULT=FALSE
    if(RESULT && (MRAE||MdRAE||GMRAE)) abs_rt=abs(err/errb)
    if(MRAE) { if(RESULT) mrae=mean(abs_rt) else mrae=NA
    if(rsingle) return(mrae) else {res=c(res,mrae);nres=c(nres,"MRAE")}
    } else mrae=NULL
    if(MdRAE){ if(RESULT) mdrae=median(abs_rt) else mdrae=NA
    if(rsingle) return(mdrae) else {res=c(res,mdrae);nres=c(nres,"MdRAE")}
    } else mdrae=NULL
    if(GMRAE){ if(RESULT) gmrae=prod(abs_rt)^(1/(length(abs_rt))) else gmrae=NA
    if(rsingle) return(gmrae) else {res=c(res,gmrae);nres=c(nres,"GMRAE")}
    } else gmrae=NULL
    if(THEILSU2){ if(RESULT) theilsu2=rmse/sqrt(mean(errb^2)) else theilsu2=NA
    if(rsingle) return(theilsu2) else {res=c(res,theilsu2);nres=c(nres,"THEILSU2")}
    } else theilsu2=NULL
    
    if(MASE){ if(is.list(val)){val2=val[[which(metric=="MASE")[1]]]}
      else if(length(val)>1) val2=val else val2=NULL
      if(!is.null(val2)){mmase=mean(abs(diff(val2)));mase=mean(abs(err/mmase))} else mase=NA
      if(rsingle) return(mase) else {res=c(res,mase);nres=c(nres,"MASE")}
    } else mase=NULL
    
    if(SMINKOWSKI3){sminkowski3=sum(eabs^3);if(rsingle) return(sminkowski3) else {res=c(res,sminkowski3);nres=c(nres,"SMinkowski3")}} else sminkowski3=NULL
    if(MMINKOWSKI3){mminkowski3=mean(eabs^3);if(rsingle) return(mminkowski3) else {res=c(res,sminkowski3);nres=c(nres,"MMinkowski3")}} else mminkowski3=NULL 
    if(MdMINKOWSKI3){mdminkowski3=median(eabs^3);if(rsingle) return(mdminkowski3) else {res=c(res,sminkowski3);nres=c(nres,"MdMinkowski3")}} else mdminkowski3=NULL
    
    if(COR||q2||R2){cor=suppressWarnings(cor(y,x));if(is.na(cor)) cor=0;
    if(rsingle && COR) return(cor) else {res=c(res,cor);nres=c(nres,"COR")}
    } else cor=NULL
    if(R2) {r2=cor^2; if(rsingle) return (r2) else {res=c(res,r2);nres=c(nres,"R2")}} else r2=NULL
    if(q2){q2=1-cor^2;if(rsingle) return(q2) else {res=c(res,q2);nres=c(nres,"q2")}} else q2=NULL
    
    if(REC||NAREC||TOLERANCE) { if((NAREC||TOLERANCE)&& is.null(val)) val=1 
    rec=RECcurve(y,x)
    } else rec=NULL
    if(NAREC){ 
      if(is.list(val)) val2=val[[which(metric=="NAREC")[1]]]
      else if(length(val)>1) val2=val[which(metric=="NAREC")[1]]
      else val2=val
      if(rec[nrow(rec),1]>val2) {rec2=partialcurve(rec,val2);narec=curvearea(rec2,val2)} 
      else {val2=rec[nrow(rec),1];narec=curvearea(rec,val2)}
      if(rsingle) return(narec) else {res=c(res,narec);nres=c(nres,"NAREC")}
    } else narec=NULL
    if(TOLERANCE){ # warning last REC metric, changes rec 
      if(is.list(val)) val2=val[[which(metric=="TOLERANCE")[1]]]
      else if(length(val)>1) val2=val[which(metric=="TOLERANCE")[1]]
      else val2=val
      if(rec[nrow(rec),1]>val2) rec=partialcurve(rec,val2)
      if(is.vector(rec)) tolerance=rec[2] else tolerance=rec[nrow(rec),2]
      if(rsingle) return(tolerance) else {res=c(res,tolerance);nres=c(nres,"TOLERANCE")}
    } else tolerance=NULL
    # regression return: 
    if(!is.null(res)) {names(res)=nres; 
    # sort res:
    I=NULL # for regression, this works perfectly?
    for(i in 1:LM)
    {
      ii=which(nres==metric[i])[1]
      if(!is.na(ii)) I=c(I,ii) 
    }
    res=res[I]
    }
    if(reslist) {res=list(res=res,rec=rec)}
    return(res)
  }
}

#----------------------------------------
# RECurve by Paulo Cortez, 2006@
#
# following the article of Bi & Bennett 2003:
# J. Bi and K. Bennett, Regression Error Characteristic curve
# In Proceedings of 20th Int. Conf. on Machine Learning (ICML),  
# Washington DC, USA, 2003.
#
# vector.error - vector with the residuals or errors
#                vector.error = y (desired) - x (predicted)
#              or vector.error= y and x= predictions (2nd mode)
RECcurve=function(vector.error,x=NULL)
{
  #print(vector.error)
  if(!is.null(x)) vector.error=(vector.error-x)
  Correct=0; Eprev=0; 
  ES<-sort(abs(vector.error))
  #print(ES)
  M<-length(ES)+1; M1=M-1;
  X<-matrix(nrow=M,ncol=2)
  M<-length(ES); k=1;i=1;notstop=TRUE;
  while(notstop)
  { a=0; while( (i+a)<M && ES[(i+a+1)]==ES[(i+a)] ) a=a+1;
  if(a>0) {i=i+a-1; Correct=Correct+a-1;}
  #cat(" >> i:",i,"a:",a,"k:",k,"prev:",Eprev,"ESi:",ES[i],"\n")
  if(Eprev<ES[i])
  { X[k,1]<-Eprev; X[k,2]<-Correct/M1; Eprev<-ES[i]; k=k+1;}
  Correct=Correct+1
  i=i+1;
  if(i>M1) notstop=FALSE;
  }
  X[k,1]<-ES[M]
  X[k,2]<-Correct/M1
  #print(X)
  #cat("M:",M,"k:",k,"Cor:",Correct,"\n")
  #X=na.omit(X) #X[,2]<-100*X[,2] # put in percentage
  return (X[(1:k),])
}

# ----------------------------------------------------------

# convert matrix or data.frame into factor with major class 
majorClass=function(x,L)
{
  if(is.vector(x)) return (factor(L[which.max(x)],levels=L))
  else 
  { NX=nrow(x)
  y=vector(length=NX)
  for(i in 1:NX) y[i]=L[which.max(x[i,])]
  return (factor(y,levels=L))
  }
}

# target - vector of factor with the desired values 
# predict - vector of factor with the predicted values
# D - decision thresold
# TC - target concept class, -1 not used
# note: when TC=-1 then majorclass is used, D is not considered!
Conf=function(target,pred,D=0.5,TC=-1,predreturn=FALSE)
{
  L=levels(target[1])
  if(is.vector(pred)) # numeric predictions equal to classes 
  { if(length(L)>2) pred=factor(pred,levels=L)
  else  { 
    if(TC==1) LB=c("TRUE","FALSE") else LB=c("FALSE","TRUE")
    pred=factor(pred>D,levels=LB); target=factor((target==L[TC]),levels=LB)
  }
  }
  else if(is.factor(pred)) # pred factor
  {
    LP=levels(pred[1])
    if(length(LP)<length(L)) levels(pred)=L # only if pred has less classes than target
    if(TC>0)
    {
      pred=factor(pred==L[TC],levels=c("FALSE","TRUE"))
      target=factor((target==L[TC]),levels=c("FALSE","TRUE"))
    }
  }
  else if(!is.factor(pred)) #if(is.matrix(pred) || is.data.frame(pred))  # probabilities
  { if(TC>0) { pred=factor(pred[,TC]>D,levels=c("FALSE","TRUE")); target=factor((target==L[TC]),levels=c("FALSE","TRUE"));}
    else pred=majorClass(pred,L)
  }
  if(predreturn) return(list(conf=table(target,pred),pred=pred)) else return(table(target,pred))
}

# ------------------------------------------------------------------
LIFTcurve<-function(y,x,TC)
{
  if(TC>0 && !is.vector(x) ) { x=x[,TC];} else TC=2
  NC=NCOL(x)
  if(is.factor(y[1])) POS=levels(y[1])[TC] else POS=1
  if(NC==2) x=x[,2]
  NR=NROW(x); if(NR>100) NR=100
  alift=twoclassLift(y,x,Positive=POS,STEPS=NR,type=3)
  return (list(alift=alift,area=curvearea(alift,1)))
}












