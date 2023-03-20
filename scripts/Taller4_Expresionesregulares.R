
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


# - Librerias y paquetes 

library(pacman)
p_load(tidyverse, ggplot2, openxlsx, scales, skimr, stringi, SnowballC, stringr)


# Dejar bases de comentarios por politicos 

petro <-  subset(train, name == "Petro", select = c(comentario))
uribe <-  subset(train, name == "Uribe", select = c(comentario))
lopez <-  subset(train, name == "Lopez", select = c(comentario))

# Dejar a nivel palabras 
petro <- petro %>%
  mutate(id = row_number())
uribe <- uribe %>%
  mutate(id = row_number())
lopez <- lopez %>%
  mutate(id = row_number())

words_petro <- petro %>%
  unnest_tokens(output = "word", input = "comentario")

words_uribe <- uribe %>%
  unnest_tokens(output = "word", input = "comentario")

words_lopez <- lopez %>%
  unnest_tokens(output = "word", input = "comentario")



# Mapa de palabras
# Mapa palabras Lopez

words_lopez <-  subset(words_lopez, word != "ano")
n_words_lopez <- words_lopez %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(100)

wordcloud(words_lopez$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 


n_words_petro <- words_petro %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(100)
wordcloud2(data = n_words_petro)


n_words_uribe <- words_uribe %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(100)
wordcloud2(data = n_words_uribe)


objetos <- c("train", "test")
for (obj in objetos) {

data <- get(obj) 

data$Bogota <- grepl("\\bogota\\b", data$comentario)
data$colombiahumana <- grepl("\\colombiahumana\\b", data$comentario)
data$cundinamarcar <- grepl("\\cundinamarcar\\b", data$comentario)
data$concejo <- grepl("\\concejo\\b", data$comentario)
data$gobierno <- grepl("\\gobierno\\b", data$comentario) 
data$socialismo <- grepl("\\socialismo\\b", data$comentario) 
data$fascismo <- grepl("\\fascismo\\b", data$comentario)
data$transmilenio <- grepl("\\transmilenio\\b", data$comentario)
data$covid <- grepl("\\covid\\b", data$comentario)
data$distrital <- grepl("\\covid\\b", data$comentario)
data$estacion <- grepl("\\estacion\\b", data$comentario)

data$ciudad <- grepl("\\ciudad\\b", data$comentario)
data$cuidar <- grepl("\\cuidar\\b", data$comentario)
data$joven <- grepl("\\joven\\b", data$comentario)
data$vacunacion <- grepl("\\vacunacion\\b", data$comentario)
data$mujer <- grepl("\\mujer\\b", data$comentario)


data$pacto <- grepl("\\pacto\\b", data$comentario)
data$social <- grepl("\\social\\b", data$comentario)
data$dejar <- grepl("\\dejar\\b", data$comentario)
data$paz <- grepl("\\paz\\b", data$comentario)
data$vida <- grepl("\\vida\\b", data$comentario)


data$usd <- grepl("\\usd\\b", data$comentario)
data$onzar <- grepl("\\onzar\\b", data$comentario)
data$solidaridad <- grepl("\\solidaridad\\b", data$comentario)
data$democracia <- grepl("\\democracia\\b", data$comentario)
data$familia <- grepl("\\familia\\b", data$comentario)
data$medellin <- grepl("\\medellin\\b", data$comentario)

assign(obj, data)
rm(data)

}


write.csv(train, "D:/2023/ANDES/Big data/Taller4_Big_data/data_final//train_final.csv", row.names=FALSE)
write.csv(test, "D:/2023/ANDES/Big data/Taller4_Big_data/data_final//test_final.csv", row.names=FALSE)



