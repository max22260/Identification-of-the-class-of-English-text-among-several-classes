require(ggplot2)

classify.results <- t

names(classify.results) <- c("item.class", "logpr.a", "logpr.b")

classify.results.sample <- classify.results[sample(1:nrow(classify.results), 3000, replace=FALSE),]

p <- ggplot(classify.results, aes(logpr.a, logpr.b))
p + geom_point(aes(colour = factor(item.class))) + geom_abline(intercept=0, slope=1)



plot <- ggplot(t)
plot + geom_tile(aes(x=TClass, y=PClass, fill=Y)) + scale_x_discrete(name="Actual Class") +
  geom_text(aes(x = TClass, y=PClass ,label = sprintf("%1.0f", Y)), vjust = 1,check_overlap = TRUE ,size = 6 ) + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=0, to=100, by=10),low = "white", high = "#1698A7") + labs(fill="Frequency")