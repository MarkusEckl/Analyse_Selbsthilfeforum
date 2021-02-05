#Function: evaluation STM Model 
#used metrics: Semantic Coherence & Exclusivity
#output: graphic, dataframe evaluation values 


evaluation_stm <- function(dfm, n.topics){
  
  #dfm: document frequecy martrix
  #n.topics: number of topics, factor of numeric variables: epample <- c(10, 20, 49, 89)
  
  library(quanteda)
  library(stm)
  library(ggplot2)
  library(dplyr)
  
  dfm2stm <- convert(dfm, to = "stm")
  #calculate different stm models with k topics
  kResult <- searchK(dfm2stm$documents, dfm2stm$vocab, K=n.topics, data=dfm2stm$meta)
  
  #oveview differnt metrics 
  #plot.searchK(kResult)
  
  ### Semantic Coherence & Exclusivity
  #After comparing several models and evaluating the topics ability to be interpreted, 
  #build a dataframe 
  semantic_coherence <- kResult$results$semcoh
  exclusivity <- kResult$results$exclus
  topic_model <- kResult$results$K
  n_topics = c()
  for (i in n.topics){
    n_topics = c(n_topics, paste(i, "Topics", sep = " "))
  } 
  
  #dataframe
  evaluation_var <-data.frame(exclusivity,semantic_coherence, topic_model, n_topics)

  
  #Plot
  px <- ggplot(evaluation_var, aes(semantic_coherence, exclusivity)) +
    geom_point(color = 'red')+ 
    geom_label_repel(aes(label = n_topics, 
                         fill = factor(n_topics)), color = 'white',
                     size = 2.5) +
    theme(legend.position = "bottom") +
    labs(title="Models evaluation: Semantic coherence and exclusivity", 
         x = "semantic coherence", 
         y = " exclusivity") + 
    labs(fill = "Modelle mit ") +
  theme_update(plot.title = element_text(hjust = 0.5))
  px
  
  return(list(graph = px, df.evaluation = evaluation_var))
}
