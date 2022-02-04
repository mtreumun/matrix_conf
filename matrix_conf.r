 #Instalación de paquetes
install.packages("sp")
install.packages("rgeos")
install.packages("rgdal")
install.packages("tidyverse")
install.packages("lubridate")
# 1. Limpiar los datos, valores y variables de la consola
rm(list=ls())

# 2. Importar archivo .shp
setwd("E:/matrix_conf")

# Importar shapefile y establecer las librerías necesarias para el proceso
library(sp)
library(rgeos)
library(rgdal)
library(tidyverse)
library(lubridate)

#Importar Capa
input <- readOGR("Pba3.shp", use_iconv = TRUE, encoding = "UTF-8")
df.input <- data.frame(input)

#Creacion de tablas en funcion del df
table(df.input$SUBGRUPO_E)
count(df.input)

#generacion de split y filtrado de grupos demasiado pequeños
df.input %>%
    dplyr::filter(Tipo_Grupo=="Abierto") %>%
    group_split(SUBGRUPO_E) -> split
    class(split)
n_split <- which(sapply(split, nrow) >=4)
split<-split[n_split]
length(split)

#Indicar n de grupos con los que se desea hacer el cross validation
n_grupos<-3
grupos<-as.character(c())
class(grupos)

#Creación de grupos mediante for
for (i in 1:n_grupos) {
    paste0("Grupo", i)->grupos[i]
}

#Añadir columa con el grupo
add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col) 
}

#Relleno de grupos
list(c())-> grouped
list(c())->check
for (i in 1:length(split)) {    
    GrupoMatrix <- rep(grupos, ceiling(nrow(split[[i]])/length(grupos)))
    grouped[[i]] <- add.col(split[[i]], GrupoMatrix) %>% 
    rename(GrupoMatrix = new.col) %>% 
    mutate(GrupoMatrix = sample(GrupoMatrix))
    table(grouped[[i]]$GrupoMatrix)->check[[i]]
}

#chequear numeros
check[[10]]

#Agrupación
bind_rows(grouped)->grouped
grouped %>%
    group_split(GrupoMatrix)->grouped
grouped

#Escritura de csv
for (i in seq_along(grouped)) {
    filename = paste("Grupo",i,".csv")
    write.csv(grouped[[i]], filename)
}