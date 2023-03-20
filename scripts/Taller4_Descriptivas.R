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

# Petro

train_text <- data.frame(train)
mi_nuevo_df <- subset(train_text, name == "Petro", select = text)
fila_unica_p <- paste(mi_nuevo_df, collapse=";")

# Lopez

train_text <- data.frame(train)
mi_nuevo_df <- subset(train_text, name == "Lopez", select = text)
fila_unica_l <- paste(mi_nuevo_df, collapse=";")

# Uribe

train_text <- data.frame(train)
mi_nuevo_df <- subset(train_text, name == "Uribe", select = text)
fila_unica_u <- paste(mi_nuevo_df, collapse=";")

df_unido <- rbind(fila_unica_p, fila_unica_l, fila_unica_u)

PAPERS_CORPUS <- corpus(df_unido)

PALABRAS <- tokens(PAPERS_CORPUS, remove_punct = TRUE, remove_separators = TRUE, 
                   remove_numbers = TRUE, verbose = TRUE )
stopwords("es")
DFM_PAPERS <- dfm(PALABRAS, remove = stopwords("es"))
dfmat_inaug <- dfm(PALABRAS, remove = stopwords("es"))
library(quanteda.textstats)
tstat_lexdiv <- textstat_lexdiv(dfmat_inaug)
tail(tstat_lexdiv, 5)
#gráficos
plot(tstat_lexdiv$TTR, type = "l", xaxt = "n", xlab ="Político", ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(tstat_lexdiv)), labels = c("Petro", "López", "Uribe"))






