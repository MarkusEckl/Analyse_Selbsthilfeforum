#Topic over time Time 
#Function: generate regression


topic_over_time <- function(dfm2stm, dfm2stm.meta.date, topic.count, model.stm,
                            df.topic.labels, topic.number.id){
  #dfm2stm  
  #dfm2stm.meta.date: needed form the stm package dfm2stm.meta and the column, in which includes the date
  #topic.count: topic count of the calculated stm model 
  #model.stm: calculated stm model 
  #df.topic.labels: dataframe with the lable of topics 
  #topic.number.id: the id of the topic, which you want plot 
  
  dfm2stm <- dfm2stm
  #IMPORTANT STM does not work with DATA TYPE!!!
  dfm2stm$meta$datum <- as.numeric(as.factor(dfm2stm.meta.date))
  model.stm.ee <- estimateEffect(1:topic.count ~  s(datum), model.stm, meta = dfm2stm$meta)
  
  ## Pretty plot
  
  #get_effects form Charsten Schwemmers package stminsights 
  library(stminsights)
  effects <- get_effects(estimates = model.stm.ee,
                         variable = 'datum',
                         type = 'continuous')
  
  #Join Topic Labels and effects Dataframe
  df.topic.labels$topic <- as.factor(df.topic.labels$topic.id)
  effects2 <- left_join(effects, df.topic.labels, by = "topic")
  
  return(effects2)
  
}
