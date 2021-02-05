#Correspondence Analysis
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(quanteda)
library(spacyr)


correspondence_analysis <- function(dataframe, group_var, text.col, party_col, plot.xlim,plot.ylim){
  
  
  #dataframe grouping parties 
  group_var <- enquo(group_var)
  text.col   <- enquo(text.col)
  
  dataframe2 <- dataframe %>%
    group_by(!!group_var) %>%
    summarise(docs.party = paste(!!text.col, collapse = " ")) 
  

  source("functions/spacy_text_cleaning.R")
  
  #function spacy_text_cleaning 
  tokens <- spacy_text_cleaning(language = "de",
                                dataframe = dataframe2,
                                dataframe.text.col = dataframe2$docs.party,
                                tokens.lemma = FALSE,
                                remove.numb = TRUE,
                                min.nchar = 2,
                                collocation.min = 10,
                                df.col.dfm = c(names(dataframe2)))
  
  #document term matrix 
  dfm <- tokens %>% 
    dfm() %>% 
    dfm_select(min_nchar = 4L) #%>% 
  #dfm_trim(min_docfreq = 1) %>%  # minimum 50 documents (removes rare terms)
  #dfm_trim(max_docfreq = 0.1,
  #       docfreq_type = 'prop') # maximum in 25 percent of documents (stop words)
  
  tmod_ca <- textmodel_ca(dfm)
  textplot_scale1d(tmod_ca)
  dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                       dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
  
  dim(dataframe2)
  dataframe2 <- as.data.frame(dataframe2)
  row.names(dat_ca) <- dataframe2[,1]
  
  
  plot(1, xlim = plot.xlim, ylim = plot.ylim, 
       type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2')
  grid()
  text(dat_ca$dim1, 
       dat_ca$dim2, 
       labels = rownames(dat_ca), 
       cex = 0.8, 
       col = rgb(0, 0, 0, 0.7))
  
  return(dat_ca)
  
}
