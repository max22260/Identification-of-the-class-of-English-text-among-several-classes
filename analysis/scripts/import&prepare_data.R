# ====================================================================================================
# title:    prepare data
# author:   ahmed nagy radwan
# summary: in this script import data , merge  all file with the class lable for each file on one data 
#          matrix and convert data to .csv  
# output :  dataset.csv , datamatrix.data
# ====================================================================================================
#set work directory in your machine
work_dir = "/home/max/Desktop/Identification\ of\ the\ class\ of\ English\ text\ among\ several\ classes/analysis/scripts"


#----------------------- install packeges ------------------------------------------------------------

install.packages("textreadr")

#-----------------------------------------------------------------------------------------------------
# using textreader packege can import all file.txt on one matrix col =content row = file name  
library(textreadr)
#-----------------------------------------------------------------------------------------------------
#set work directory
setwd(work_dir)
#get current work directory
getwd()
#-----------------------------------------------------------------------------------------------------
# LOAD  all docment class as docment matrix
business       <- read_dir('../../dataset/business')
entertainment  <- read_dir(path = '../../dataset/entertainment')
politics       <- read_dir('../../dataset/politics')
sport          <- read_dir('../../dataset/sport')
tech           <- read_dir('../../dataset/tech')  
#----------------------------------------------------------------------------------------------------

business = as.matrix(business)
write.csv( x=business ,file = '../../dataset/b.csv')



