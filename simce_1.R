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

#comunas
s4b2018LC <- simce4to2018[c(11,36,35,34)]
names(s4b2018LC)[1] <- "Comuna"
names(s4b2018LC)[2] <- "Adecuado"
names(s4b2018LC)[3] <- "Elemental"
names(s4b2018LC)[4] <- "Insuficiente"

DF1C <- melt(s4b2018LC, id.var='Comuna')
#DF1 <- DF1[order(DF1$rbd,DF1$variable),]
DF1C %>% filter(value>0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette="Dark2") + #scale_fill_manual(values=c(" #2E86C1", " #5DADE2", " #AED6F1")) +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2018")

s4b2019LC <- simce4to2019[c(2,12:14)]
names(s4b2019LC)[1] <- "Comuna"
names(s4b2019LC)[2] <- "Adecuado"
names(s4b2019LC)[3] <- "Elemental"
names(s4b2019LC)[4] <- "Insuficiente"

DF2C <- melt(s4b2019LC, id.var='Comuna')
#DF2 <- DF2[order(DF2$RBD,DF2$variable),]
DF2C %>% filter(value > 0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2019")

s4b2018MC <- simce4to2018[c(11,39, 38, 37)]
names(s4b2018MC)[1] <- "Comuna"
names(s4b2018MC)[2] <- "Adecuado"
names(s4b2018MC)[3] <- "Elemental"
names(s4b2018MC)[4] <- "Insuficiente"
DF3C <- melt(s4b2018MC, id.var='Comuna')
#DF1 <- DF1[order(DF1$rbd,DF1$variable),]
DF3C %>% filter(value>0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2018")

s4b2019MC <- simce4to2019[c(2,23,24,25)]
names(s4b2019MC)[1] <- "Comuna"
names(s4b2019MC)[2] <- "Adecuado"
names(s4b2019MC)[3] <- "Elemental"
names(s4b2019MC)[4] <- "Insuficiente"
DF4C <- melt(s4b2019MC, id.var='Comuna')
#DF2 <- DF2[order(DF2$RBD,DF2$variable),]
DF4C %>% filter(value > 0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2019")

#Inicio Categorias 2018 2019
s4b2018L <- simce4to2018[c(3,36,35,34)]
names(s4b2018L)[1] <- "RBD"
names(s4b2018L)[2] <- "Adecuado"
names(s4b2018L)[3] <- "Elemental"
names(s4b2018L)[4] <- "Insuficiente"
DF1 <- melt(s4b2018L, id.var='RBD')
#DF1 <- DF1[order(DF1$rbd,DF1$variable),]
DF1 %>% filter(value>0) %>% ggplot(aes(x = RBD, y = value, fill = variable, label=value)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Paired") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2018")

s4b2019L <- simce4to2019[c(1,12:14)]
names(s4b2019L)[1] <- "RBD"
names(s4b2019L)[2] <- "Adecuado"
names(s4b2019L)[3] <- "Elemental"
names(s4b2019L)[4] <- "Insuficiente"
DF2 <- melt(s4b2019L, id.var='RBD')
#DF2 <- DF2[order(DF2$RBD,DF2$variable),]
DF2 %>% filter(value > 0) %>% ggplot(aes(x = RBD, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Paired") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2019")

s4b2018M <- simce4to2018[c(3,39, 38, 37)]
names(s4b2018M)[1] <- "RBD"
names(s4b2018M)[2] <- "Adecuado"
names(s4b2018M)[3] <- "Elemental"
names(s4b2018M)[4] <- "Insuficiente"
DF3 <- melt(s4b2018M, id.var='RBD')
#DF1 <- DF1[order(DF1$rbd,DF1$variable),]
DF3 %>% filter(value>0) %>% ggplot(aes(x = RBD, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Paired") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2018")

s4b2019M <- simce4to2019[c(1,23,24,25)]
names(s4b2018M)[1] <- "RBD"
names(s4b2018M)[2] <- "Adecuado"
names(s4b2018M)[3] <- "Elemental"
names(s4b2018M)[4] <- "Insuficiente"
DF4 <- melt(s4b2019M, id.var='RBD')
#DF2 <- DF2[order(DF2$RBD,DF2$variable),]
DF4 %>% filter(value > 0) %>% ggplot(aes(x = RBD, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Paired") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2019")
