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

train <- read.csv("C:/Users/CARLOS COLMENARES/Downloads/train.csv", comment.char="#")
test <- read.csv("C:/Users/CARLOS COLMENARES/Downloads/test.csv", comment.char="#")

#Matriz de palabras para Train
train["text"] <- apply(train["text"],1, tolower)
train["text"] <- apply(train["text"],1, removeNumbers)
train["text"] <- apply(train["text"],1, removePunctuation)
train["text"] <- apply(train["text"],1, stripWhitespace)
train["text"] <- apply(train["text"], 1, function(x) 
                    stri_trans_general(str = x, id = "Latin-ASCII"))

train <- train %>%
  mutate(id = row_number())

words <- train %>%
  unnest_tokens(output = "word", input = "text")

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

n_words <- words %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(200)

wordcloud2(data = n_words)

udpipe::udpipe_download_model('spanish')

model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")

palabras_unicas <- words %>%
  distinct(word)

udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
udpipe_results <- as_tibble(udpipe_results)
                       
palabras_eliminar <- words %>%
  count(lemma) %>%
  filter(n < 10)

words <- words %>%
  anti_join(palabras_eliminar, by = "lemma") 

data_clean_t <- words %>%
  group_by(id) %>% 
  summarise(comentario = str_c(lemma, collapse = " ")) %>%
  ungroup()

tm_corpus_t <- Corpus(VectorSource(x = data_clean_t$comentario))
str(tm_corpus)

tf_idf_t <- TermDocumentMatrix(tm_corpus_t,
                             control = list(weighting = weightTfIdf))

tf_idf_t <- as.matrix(tf_idf_t) %>%
  t() %>%
  as.data.frame()

dim(tf_idf_t)

columnas_seleccionadas <- colSums(tf_idf_t) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(50) %>%
  rownames()

tf_idf_reducido_t <- tf_idf_t %>%
  select(all_of(columnas_seleccionadas))

#Base de Datos lista para usar
save(test, data_clean_t, tf_idf_t, tf_idf_reducido_t, 
     file = "C:/Users/Santiago/Downloads/Escritorio/DOCUMENTOS SANTIGO/Maestria Uniandes/Big Data & Machine Learning/datos_para_testear.RData")
                   
udpipe_results <- udpipe_results %>% 
  select(token, lemma) %>%
  rename("word" = "token")

words <- words %>%
  left_join(udpipe_results, by = "word", multiple = "all")
words[is.na(words$lemma), "lemma"] <- words[is.na(words$lemma), "word"]

# a <- words %>%
#   count(lemma) %>%
#   arrange(desc(n)) %>%
#   head(100)
# 
# wordcloud2(data = a)

palabras_eliminar <- words %>%
  count(lemma) %>%
  filter(n < 10)

words <- words %>%
  anti_join(palabras_eliminar, by = "lemma") 

data_clean <- words %>%
  group_by(id, name) %>% 
  summarise(comentario = str_c(lemma, collapse = " ")) %>%
  ungroup()

tm_corpus <- Corpus(VectorSource(x = data_clean$comentario))
str(tm_corpus)

tf_idf <- TermDocumentMatrix(tm_corpus,
                             control = list(weighting = weightTfIdf))

tf_idf <- as.matrix(tf_idf) %>%
  t() %>%
  as.data.frame()

dim(tf_idf)

columnas_seleccionadas <- colSums(tf_idf) %>%
  data.frame() %>%
  arrange(desc(.)) %>%
  head(50) %>%
  rownames()

tf_idf_reducido <- tf_idf %>%
  select(all_of(columnas_seleccionadas))

#Base de Datos lista para usar
save(train, data_clean, tf_idf_reducido, 
     file = "C:/Users/CARLOS COLMENARES/Downloads/datos_para_modelar.RData")

#Matriz de Palabras para Test
                       

test["text"] <- apply(test["text"],1, tolower)
test["text"] <- apply(test["text"],1, removeNumbers)
test["text"] <- apply(test["text"],1, removePunctuation)
test["text"] <- apply(test["text"],1, stripWhitespace)
test["text"] <- apply(test["text"], 1, function(x) 
  stri_trans_general(str = x, id = "Latin-ASCII"))

test <- test %>%
  mutate(id = row_number())

words <- test %>%
  unnest_tokens(output = "word", input = "text")

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

n_words <- words %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(200)

wordcloud2(data = n_words)

udpipe::udpipe_download_model('spanish')

model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")

palabras_unicas <- words %>%
  distinct(word)

udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
udpipe_results <- as_tibble(udpipe_results)
udpipe_results <- udpipe_results %>% 
  select(token, lemma) %>%
  rename("word" = "token")

words <- words %>%
  left_join(udpipe_results, by = "word", multiple = "all")
words[is.na(words$lemma), "lemma"] <- words[is.na(words$lemma), "word"]                       
