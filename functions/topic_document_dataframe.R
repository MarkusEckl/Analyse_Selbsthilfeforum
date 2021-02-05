#topic_document_dataframe 
#Function: Documents with the highest topic probability of x will be exported to a dataframe 
### or get the x % of hightes theata documents for a topic 


topic_document_df <- function(model.stm, df.text, df.text.col, topic.id, procent.quantil){
  
  #model.stm
  #df.text: main dataframe 
  #df.text.col: excample <- df2$data.body.value
  #topic.id
  #procent.quantil
  
  doc_topic <- findThoughts(model.stm,
                         texts = df.text.col, 
                         topics =topic.id,
                         n = 10000,
                         thresh = quantile(model.stm$thet, probs = c(procent.quantil)))

df.text$index <- seq(nrow(df.text))
df <- filter(df.text, index %in% unlist(doc_topic$index))

return(df)
}
