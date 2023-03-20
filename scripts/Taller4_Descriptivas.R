#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 4: Predicting Tweets                       #
#
# Código Descriptivas
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 20/03/2023 

library(pacman) 
p_load(tidyverse, janitor, tm, stringi, tidytext, stopwords, wordcloud2, udpipe,
       ggcorrplot, readtext, RColorBrewer,broom, rgdal, quanteda) 

train <- read.csv("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 4/train.csv", comment.char="#")
test <- read.csv("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 4/test.csv", comment.char="#")

train_text <- data.frame(train) 
PAPERS_CORPUS <- corpus(train_text$text)
PALABRAS <- tokens(PAPERS_CORPUS, remove_punct = TRUE, remove_separators = TRUE, 
                   remove_numbers = TRUE, verbose = TRUE )
stopwords("es")
DFM_PAPERS <- dfm(PALABRAS, remove = stopwords("es"))
dfmat_inaug <- dfm(PALABRAS, remove = stopwords("es"))
tstat_lexdiv <- textstat_lexdiv(dfmat_inaug)
tail(tstat_lexdiv, 5)




