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
count(df.input)

#generacion de split y filtra grupos demasiado pequeños
df.input %>%
    group_split(SUBGRUPO_E) -> split
    class(split)
n_split <- which(sapply(split, nrow) >4)
split<-split[n_split]
length(split)

# vector con grupos
grupos <- c("Grupo1", "Grupo2", "Grupo3", "Grupo4")

list(c())-> grouped
for (i in 1:length(split)) {
    sample_clase <- as.numeric(ceiling(count(split[[i]])/4))
    #sampleo a 4 grupos
    GrupoMatrix <- sample(grupos, size = nrow(split[[i]]), replace=TRUE, prob=c(0.25, 0.25, 0.25, 0.25))
    grouped[[i]] <- cbind(split[[i]], GrupoMatrix)
}

bind_rows(grouped)->grouped

grouped %>%
    group_split(GrupoMatrix)->grouped
grouped

for (i in seq_along(grouped)) {
    filename = paste("Grupo",i,".csv")
    write.csv(grouped[[i]], filename)
}
