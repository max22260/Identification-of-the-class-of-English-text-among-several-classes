# ====================================================================================================
# title:    read directory function
# author:   ahmed nagy radwan
# summary: implementation to function get directory and pattern as 
#          argument and return data frame represent data by two columns (filename , content of file ) 
# output :  data frame
# ====================================================================================================
read.dir <-function(dir , pattern){
  
  file.names <- dir(dir, pattern = pattern)
  file.names = as.data.frame(file.names)
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

