#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 4: Predicting Tweets                       #
#
# Código Modelos
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 16/03/2023 

library(pacman) 
p_load(tidyverse, janitor, tm, stringi, tidytext, stopwords, wordcloud2, udpipe,
       ggcorrplot) 


#train <- read.csv("C:/Users/CARLOS COLMENARES/Downloads/train.csv", comment.char="#")
#test <- read.csv("C:/Users/CARLOS COLMENARES/Downloads/test.csv", comment.char="#")


train <- read.csv("C:/Users/Ivan/Documents/Documento 2023/Andes/Big data/Taller4/Taller4_Big_data/data/train.csv", comment.char="#")
test <- read.csv("C:/Users/Ivan/Documents/Documento 2023/Andes/Big data/Taller4/Taller4_Big_data/data/test.csv", comment.char="#")



objetos <- c("train", "test")
for (obj in objetos) {
  
data <- get(obj) 
  
#Matriz de palabras para Train
data["text"] <- apply(data["text"],1, tolower)
data["text"] <- apply(data["text"],1, removeNumbers)
data["text"] <- apply(data["text"],1, removePunctuation)
data["text"] <- apply(data["text"],1, stripWhitespace)
data["text"] <- apply(data["text"], 1, function(x) 
stri_trans_general(str = x, id = "Latin-ASCII")) 
  


#De Comentarios a palabras 
data <- data %>%
  mutate(id = row_number())

#Partir comentarios por palabras
words <- data %>%
  unnest_tokens(output = "word", input = "text") 
  

#Quitar las stopwords
sw <- c()
for (s in c("snowball", "stopwords-iso", "nltk")) {
  temp <- get_stopwords("spanish", source = s)$word
  sw <- c(sw, temp)
}
sw <- unique(sw)
sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
sw <- data.frame(word = sw)

words <- words %>%
  anti_join(sw, by = "word")
  
 
#Lematizar palabras
model <- udpipe_load_model(file = "C:/Users/Ivan/Documents/Documento 2023/Andes/Big data/Taller4/Taller4_Big_data/data/spanish-gsd-ud-2.5-191206.udpipe")





assign(obj, data)
rm(data)

}

s
  
  
