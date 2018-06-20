

Visualizing_Data.t_SNE  <- function(data,ClassNumber){
  
packages <- c('Rtsne', 'ggplot2', 'plotly','tsne')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library('Rtsne')
library('ggplot2')
library('plotly')
library('tsne')


features <- data[, !names(train_data_model) %in% c("y")]


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
  ,colors = rainbow(ClassNumber)
) %>% layout(xaxis = ax, yaxis = ax)

  return(p)
}



like <- Visualizing_Data.t_SNE(train_data_model , dim = 2 , ClassNumber = 5)




