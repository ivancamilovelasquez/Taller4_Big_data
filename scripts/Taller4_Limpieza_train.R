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

# - Limpiar espacio de trabajo


# - Librerias y paquetes 

library(pacman) 
p_load(tidyverse, janitor, tm, stringi, tidytext, stopwords, wordcloud2, udpipe,
       ggcorrplot) 


#train <- read.csv("C:/Users/CARLOS COLMENARES/Downloads/train.csv", comment.char="#")
#test <- read.csv("C:/Users/CARLOS COLMENARES/Downloads/test.csv", comment.char="#")


train <- read.csv("D:/2023/ANDES/Big data/Taller4_Big_data/data/train.csv", comment.char="#")



objetos <- c("train")
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
model <- udpipe_load_model(file = "D:/2023/ANDES/Big data/Taller4_Big_data/data/spanish-gsd-ud-2.5-191206.udpipe")

palabras_unicas <- words %>%
  distinct(word)

results <- udpipe_annotate(model, x = palabras_unicas$word)
results <- as_tibble(results)


results  <- results %>% 
  select(token, lemma) %>% 
  rename ("word" = "token")

words <- words %>% 
  left_join(results, by ="word", multiple ="all")

words[is.na(words$lemma), "lemma"] <-  words[is.na(words$lemma) , "word"]


# Eliminamos palabras que se repiten menos de 10 
palabras_eliminar <- words %>%
  count(lemma) %>%
  filter(n < 10)


# Volvemos a poner las bases a nivel de comentario
words <- words %>%
  anti_join(palabras_eliminar, by = "lemma") 



data_clean_t <- words %>%
    group_by(id , name) %>% 
    summarise(comentario = str_c(lemma, collapse = " ")) %>%
    ungroup()



#Matriz de palabras con sus pesos 
tm_corpus <- Corpus(VectorSource(x = data_clean_t$comentario))
str(tm_corpus)

tf_idf <- TermDocumentMatrix(tm_corpus,
                               control = list(weighting = weightTfIdf))

tf_idf <- as.matrix(tf_idf) %>%
  t() %>%
  as.data.frame()


# Dejar solo las 70 palabras mas importantes

columnas_seleccionadas <- colSums(tf_idf) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(100) %>%
  rownames()

tf_idf_reducido <- tf_idf %>%
  select(all_of(columnas_seleccionadas))

data = cbind(data_clean_t, tf_idf_reducido)


assign(obj, data)
rm(data)

}

  
