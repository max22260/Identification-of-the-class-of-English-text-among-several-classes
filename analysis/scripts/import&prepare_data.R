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
business       <- read_dir('../../dataset/business',pattern = ".txt")
entertainment  <- read_dir(path = '../../dataset/entertainment')
politics       <- read_dir('../../dataset/politics')
sport          <- read_dir('../../dataset/sport')
tech           <- read_dir('../../dataset/tech')  

# i try read_dir function form *textreadr* package 
#  it looks to me good but thier are alot of rpeatation in matrix result

#----------------------------------------------------------------------------------------------------
# function tor read from directory all file and return datafram as c(filename , content)
read.dir <-function(dir , pattern){
  
  file.names <- dir(dir, pattern = pattern)
  file.names = as.data.frame(x=file.names)
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
#---------------------------------------------------------------------------------------------------------
# load  all file on folder for each class 
business       <- read.dir('../../dataset/business',".txt")
entertainment  <- read.dir( '../../dataset/entertainment', ".txt")
politics       <- read.dir('../../dataset/politics',".txt")
sport          <- read.dir('../../dataset/sport',".txt")
tech           <- read.dir('../../dataset/tech',".txt")  

#--------------------------------------------------------------------------------------------------------
# write data as csv file  
write.csv( x=business ,file = "../../analysis/data/business.csv")  
write.csv( x=entertainment ,file = "../../analysis/data/entertainment.csv")  
write.csv( x=politics ,file = "../../analysis/data/politics.csv")  
write.csv( x=sport ,file = "../../analysis/data/sport.csv")  
write.csv( x=tech ,file = "../../analysis/data/tech.csv")  

#-------------------------------------------------------------------------------------------------------
# insert label class and merge all togither in one data matrix
business$class <- c("business")
entertainment$class <- c("entertainment")
politics$class <- c("politics")
sport$class <- c("sport")
tech$class <-c("tech")
#merge
bbc.data.matrix <-rbind(business,entertainment,politics,sport,tech)
#-------------------------------------------------------------------------------------------------------
# save data 

write.csv( x=bbc.data.matrix ,file = "../../analysis/data/bbc.csv")  

save(bbc.data.matrix,file = "../../analysis/data/bbc_DataMatrix.RData")
#-------------------------------------------------------------------------------------------------------


