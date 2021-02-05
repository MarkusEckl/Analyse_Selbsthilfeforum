#Function: Text to tokens 
#use spacy: [spacy](https://spacy.io/) and the R package [quanteda](https://quanteda.io/) are used.


cooccurence.word.network <- function(language, dataframe, dataframe.text.col, 
                                     topic.id.number,tokens.lemma, remove.numb,min.nchar, 
                                     collocation.min, word.count.network, edges.min.freq){
  
 
  #language: which model for initialize spacy: example <- "de", "en"
  #datafrme: example <-  df, dfx
  #dataframe.text.col: dataframe and column example <- df$text
  #tokens.lemma: TRUE or FALSE 
  #remove.numb: TRUE or FALSE
  #min.nchar: min of character length example <- 2 or 4 (numeric value)
  #collocation.min: min of collocation in the corpus wich are used: example <- 4 or 6 (numeric value)

  #word.count.network <- numeric 
  #edges.min.freq <- 0.4 example 
  #
  
  
  #download spacyr for r url:https://github.com/quanteda/spacyr
  library(spacyr)
  library(dplyr)
  library(quanteda)
  
  #spacy_install()
  #Initialize spaCy to call from R.
  #spacy_download_langmodel("de")
  spacy_initialize(model = language, refresh_settings = TRUE)
  #tokenize and tag the texts, and returns a data.table of the results
  parsed <- spacy_parse(dataframe.text.col)
  #terminates the python process in the backround
  spacy_finalize()
  
  nouns<- with(parsed, subset(parsed, pos == "NOUN"))
  adja <- with(parsed, subset(parsed, pos == "ADJA"))
  #create tokens and use lammatastion of the words
  #remove puncation, numbers, stopwords and special topkens
  #min. character are 4
  tokens <- as.tokens(parsed, use_lemma = tokens.lemma) %>% 
    tokens(remove_punct = TRUE, remove_numbers = remove.numb) %>% 
    tokens_tolower() %>% 
    tokens_remove(c(stopwords('de'), "vgl", "hinsichtlich", 
                    "z._b.", "cine", "hierzu", "erstens", "zweitens", "deutlich",
                    "geben", "mehr", "immer", "schon", "gehen", "sowie", "erst", "mehr", "etwa",
                    "dabei", "dis-", "beziehungsweise", "seit", "drei", "insbesondere", "dass",
                    "beim", "ab", "uhr",
                    stopwords("en")),
                  min_nchar = 2L,  padding = TRUE)
  
  #quanteda- Identify and score multi-word expressions, or adjacent fixed-length collocations, from text
  #min count 30 
  collocation <- textstat_collocations(tokens, min_count = 10)
  #quanteda - cearce Bi-grams 
  tokens <- tokens_compound(tokens, collocation, join = FALSE)
  
  
  fcmat <- fcm(tokens, context = "window", tri = FALSE)
  
  fcmat2 <- fcm_select(fcmat, pattern = c(nouns, adja))
  
  feat <- names(topfeatures(fcmat, word.count.network))
  network <- fcm_select(fcmat, pattern = feat) %>%
    textplot_network(min_freq = edges.min.freq)
  ggsave(paste0("output_vis/cooc_word_network_", topic.id.number,".png"), width = 50, height = 30, units = "cm", dpi = 300)
  return(network)
}

