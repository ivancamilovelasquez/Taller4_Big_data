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
       ggcorrplot) 

train <- read.csv("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 4/train.csv", comment.char="#")
test <- read.csv("C:/Users/jorge/Desktop/BIG DATA & ML/Problem Set 4/test.csv", comment.char="#")