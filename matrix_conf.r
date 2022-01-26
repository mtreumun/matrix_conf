install.packages("sp")
install.packages("rgeos")
install.packages("rgdal")
install.packages("tidyverse")
install.packages("lubridate")

## Matriz de Confusión

## 1. Limpiar los datos, valores y variables de la consola ##
rm(list=ls())

## 2. Importar archivo .shp ##
# Seleccionar directorio de trabajo
setwd("E:/matrix_conf")
# Importar shapefile y establecer las librerías necesarias para el proceso
library(sp)
library(rgeos)
library(rgdal)
library(tidyverse)
library(lubridate)


#Importar Capa
input <- readOGR("Pba.shp", use_iconv = TRUE, encoding = "UTF-8")
df.input <- data.frame(input)


#SubgruposE
table(df.input$SUBGRUPO_E)

#generacion de split y filtra grupos demasiado pequeños
df.input %>%
    #dplyr::filter(Tipo_Grupo=="Abierto") %>%
    group_split(SUBGRUPO_E) -> split
    class(split)
n_split <- which(sapply(split, nrow) >4)
split<-split[n_split]
length(split)


list(c())-> grouped
for (i in 1:length(split)) {
    sample_clase <- as.numeric(ceiling(count(split[[i]])/4))
    clase_G1 <- split[[i]] %>% sample_n(sample_clase)
    clase_G1$GrupoMatrix <- "Grupo1"
    clase_Erase1 <- anti_join(split[[i]],clase_G1)
    clase_G2 <- clase_Erase1 %>% sample_n(sample_clase)
    clase_G2$GrupoMatrix <- "Grupo2"
    clase_Erase2 <- anti_join(clase_Erase1,clase_G2)
    clase_G3 <- clase_Erase2 %>% sample_n(sample_clase)
    clase_G3$GrupoMatrix <- "Grupo3"
    clase_Erase3 <- anti_join(clase_Erase2,clase_G2)
    clase_G4 <- clase_Erase2 %>% sample_n(sample_clase)
    clase_G4$GrupoMatrix <- "Grupo4"
    clase_Erase4 <- anti_join(clase_Erase3,clase_G2)
    grouped[[i]]<-rbind(clase_G1,clase_G2,clase_G3,clase_G4)
}

table(grouped[[18]]$GrupoMatrix)
count(grouped[[18]])

bind_rows(grouped)->grouped

grouped %>%
    group_split(GrupoMatrix)->grouped
grouped

count(grouped[[4]])

for (i in seq_along(grouped)) {
    filename = paste("Grupo",i,".csv")
    write.csv(grouped[[i]], filename)
}