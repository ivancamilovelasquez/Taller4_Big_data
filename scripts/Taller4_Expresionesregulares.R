
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



objetos <- c("train", "test")
#objetos <- c("train", "test")
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

assign(obj, data)
rm(data)

}



