
pacman::p_load(tidyverse, caret, ggmosaic, ggpubr, RColorBrewer)

# DATA ####
sp <- read.csv("https://raw.githubusercontent.com/joanby/r-course/master/data/tema3/college-perf.csv") %>% 
  dplyr::select(Perf, Pred) %>% 
  as_tibble() %>% 
  rename(Real = Perf, 
         Predicho = Pred) %>% 
  mutate(Real = fct_recode(as.factor(Real), "Herbáceo" = "Low", 
                                            "Matorral" = "Medium",
                                            "Bosque" = "High"),
         Predicho = fct_recode(as.factor(Predicho), "Herbáceo" = "Low", 
                                                    "Matorral" = "Medium",
                                                    "Bosque" = "High"))


############################################


summary(sp)
View(sp)

table <- table(sp$Predicho,sp$Real, dnn= c("Predichos", "Observados"))
table

prop.table(table)

round(prop.table(table, 2)*100, 2)

round(prop.table(table, 1)*100, 2) 

barplot(table, legend = TRUE, 
        xlab = "Formación predecida por el modelo")

mosaicplot(table, main = "Eficiencia del modelo")
summary(table)

confusionMatrix(data = sp$Predicho, reference = sp$Real) ->s
s[["byClass"]]  
s

sp %>% 
  ggplot() + 
  geom_mosaic(aes(x= product(Predicho), fill=Real, y=), alpha = 1) + 
  labs(title = "Eficiencia del modelo", x="Predichos", y="Observados")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(family = "Century Gothic")) 

sp %>% 
  ggplot()+
  geom_bar(aes(x=Predicho, fill=Real))+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(family = "Century Gothic")) 

