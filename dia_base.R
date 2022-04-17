library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)

setwd("/home/insunzamnt/R/projects/slepca/slepca")
#Cargas
#saveRDS(X8vo_mat_dia, file = "~/R/projects/slepca/slepca/data/8vo_mat_dia.Rdata")
#saveRDS(X8vo_lenguaje_dia, file = "~/R/projects/slepca/slepca/data/8vo_leng_dia.Rdata")

x8vo_mat_dia <- read_rds("~/R/projects/slepca/slepca/data/8vo_mat_dia.Rdata")
x8vo_leng_dia <- read_rds("~/R/projects/slepca/slepca/data/8vo_leng_dia.Rdata")
estab <- read_rds("~/R/projects/slepca/slepca/data/establecimientos.Rdata")

#Lenguaje
lenguaje <- merge(x=estab, y=x8vo_leng_dia, by="RBD", all.y = 'TRUE')[,c("RBD","COMUNA","AREA","ESTUDIANTE", "LOCALIZAR", "INTERPRETAR", "REFLEXIONAR")] 

leg_med <- lenguaje %>% 
  summarize(loc = round(mean(LOCALIZAR),1), interp = round(mean(INTERPRETAR),1), 
            refle = round(mean(REFLEXIONAR),1))

pp <- lenguaje %>% ggplot(aes(x =LOCALIZAR)) + 
  geom_bar(color = "blue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Eje de Habilidad Localizar")

pp1 <- lenguaje %>% ggplot(aes(x =INTERPRETAR)) + 
  geom_bar(color = "green") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Eje de Habilidad Interpretar")

pp2 <- lenguaje %>% ggplot(aes(x = REFLEXIONAR)) + 
  geom_bar(color = "red") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Eje de Habilidad Reflexionar")

grid.arrange(pp, pp1, pp2, ncol = 2, top = "Lenguaje - Frecuencia por Eje de Habilidad - DIA 2021")

#Matematicas
matematica <- merge(x=estab, y=x8vo_mat_dia, by="RBD", all.y = 'TRUE')[,c("RBD","COMUNA","AREA","ESTUDIANTE", "NUMEROS", "ALG_FUN", "GEOMETRIA", "PROB_ESTAD")] 

mat_med <- matematica %>% 
  summarize(num = round(mean(NUMEROS),1), alg = round(mean(ALG_FUN),1), 
            geom = round(mean(GEOMETRIA),1), prob = round(mean(PROB_ESTAD),1))

pp <- matematica %>% ggplot(aes(NUMEROS)) + 
  geom_bar(color = "red") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Eje de Habilidad Números DIA 2021")

pp1 <- matematica %>% ggplot(aes(ALG_FUN)) + 
  geom_bar(color = "green") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Eje de Habilidad Álgebra y Funciones DIA 2021")

pp2 <- matematica %>% ggplot(aes(GEOMETRIA)) + 
  geom_bar(color = "blue") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Resultados Geometría DIA 2021")

pp3 <- matematica %>% ggplot(aes(PROB_ESTAD)) + 
  geom_bar(color = "orange") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  xlab("Porcentaje de Logro") +
  ylab("Frecuencia") + 
  ggtitle("Eje de Habilidad Probabilidad y Estadística DIA 2021")

grid.arrange(pp, pp1, pp2, pp3, ncol = 2, top="Matemáticas - Frecuencia por Eje de Habilidad")

