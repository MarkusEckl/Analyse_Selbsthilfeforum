#Function: plot most discucssed topics
#use theta: probability that a topic is discussed in a document 
#output: visualistion - barplot: top n topics 

top_topics_corpus <- function(stm.theta, df.topic.lables, 
                              topic.del, n.top.topics.plot){
  
  #stm.theta: theata form the stm model: example: model.stm$theta 
  #df.topic.labels: dataframe of the topic-labels 
  #topic.del: worse topics - can not interpretable: factor of numeric values  
  #n.top.topics.plot: number of topics wich are moste discussed in the corpus
  
  # Extract theta from the stm-model
  df.proportion <- as.data.frame(colSums(stm.theta/nrow(stm.theta)))
  df.s <- cbind(df.topic.lables, df.proportion)
  df.s %>% head()
  colnames(df.s) <- c("id", "label", "probability")
  df.s
  `%not_in%` <- purrr::negate(`%in%`)
  df.s2 <- df.s %>% filter(id %not_in% topic.del) #only topics which can be interpreted!
  df.s2
  # Sort the dataframe
  df.s3 <- df.s2[order(-df.s2$probability), ] %>% drop_na()
  df.s3$labels <- factor(df.s3$label, levels = rev(df.s3$label))
  df.s3$probability <- as.numeric(df.s3$probability)
  df.s3$probability <- round(df.s3$probability, 4)
  
  # Plot graph top n.top.topics.plot
  ht <- ggplot(df.s3 %>% head(n.top.topics.plot), aes(x = labels, y = probability)) + 
    geom_bar(stat = "identity", width = 0.2) +
    coord_flip() + 
    geom_text(aes(label = scales::percent(probability)), #Scale in percent
              hjust = -0.25, size = 4,
              position = position_dodge(width = 1),
              inherit.aes = TRUE) +
    ggtitle(label = paste0("Top ", n.top.topics.plot, " Topics")) +
    theme(plot.title = element_text(hjust = 0.5)) +
	theme_set(theme_gray(base_size = 30))
    
  
  return(ht)
  
  
}


