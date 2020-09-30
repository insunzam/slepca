library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(reshape2)

#Carga de datos
simce4to2019 <- readRDS("~/Documents/R/proyectos/slepca/data/simce4to2019.rda")
simce4to2018 <- readRDS("~/Documents/R/proyectos/slepca/data/simce4tob2018.rda")
simce4to2018$rbd <- as.character(simce4to2018$rbd)
simce4to2019$RBD <- as.character(simce4to2019$RBD)

pp <- simce4to2018 %>% ggplot(aes(prom_lect4b_rbd)) + 
  geom_density(alpha = 0.3) +
  xlab("Puntaje Promedio") +
  ylab("Nº Establecimientos") + 
  ggtitle("Resultados Lecto-Escritura SIMCE 2018")

pp1 <- simce4to2018 %>% ggplot(aes(prom_mate4b_rbd)) + 
  geom_density(alpha = 0.3) +
  xlab("Puntaje Promedio") +
  ylab("Nº Establecimientos") +
  ggtitle("Resultados Matemáticas SIMCE 2018")

pp2 <- simce4to2019 %>% filter(PPEL>0) %>%ggplot(aes(PPEL)) + 
  geom_density(alpha = 0.3) +
  xlab("Puntaje Promedio") +
  ylab("Nº Establecimientos") + 
  ggtitle("Resultados Lecto-Escritura SIMCE 2019")

pp3 <- simce4to2019 %>% ggplot(aes(PPEM)) + 
  geom_density(alpha = 0.3) +
  xlab("Puntaje Promedio ") +
  ylab("Nº Establecimientos") +
  ggtitle("Resultados Matemáticas SIMCE 2019")

grid.arrange(pp, pp1, pp2, pp3, ncol = 2)

s4b2018L <- simce4to2018[c(3,34:36)]
DF1 <- melt(s4b2018L, id.var='rbd')
#DF1 <- DF1[order(DF1$rbd,DF1$variable),]
DF1 %>% filter(value>0) %>% ggplot(aes(x = rbd, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2018")

s4b2019L <- simce4to2019[c(1,12,11,10)]
DF2 <- melt(s4b2019L, id.var='RBD')
#DF2 <- DF2[order(DF2$RBD,DF2$variable),]
DF2 %>% filter(value > 0) %>% ggplot(aes(x = RBD, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2019")

s4b2018M <- simce4to2018[c(3,37:39)]
DF3 <- melt(s4b2018M, id.var='rbd')
#DF1 <- DF1[order(DF1$rbd,DF1$variable),]
DF3 %>% filter(value>0) %>% ggplot(aes(x = rbd, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2018")

s4b2019M <- simce4to2019[c(1,23,22,21)]
DF4 <- melt(s4b2019M, id.var='RBD')
#DF2 <- DF2[order(DF2$RBD,DF2$variable),]
DF4 %>% filter(value > 0) %>% ggplot(aes(x = RBD, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2019")
