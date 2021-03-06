library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)

setwd("/home/insunzamnt/R/projects/slepca/slepca")
simce4to2019 <- readRDS("data/simce4to2019.rda")
simce4to2018 <- readRDS("data/simce4tob2018.rda")

p <- simce4to2018 %>% # filter(AGNO == 2018) %>%
  ggplot(aes(prom_lect4b_rbd, nalu_lect4b_rbd, label = rbd)) +
  geom_text(nudge_x = 3)  +
  xlab("Puntaje Promedio del Establecimiento") +
  ylab("Número de Estudiantes con Resultado") 
p + ggtitle("Resultados SIMCE 4to B 2018 (Urbanos/Rurales)")

pp <- simce4to2018 %>% ggplot(aes(prom_lect4b_rbd, fill=prom_lect4b_rbd)) + 
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


g <- simce4to2018 %>% ggplot2::aes(palu_eda_ade_lect4b_rbd)
g + geom_bar()
#ggplot(aes(palu_eda_ins_lect4b_rbd))
g + geom_bar()

simce4to2018$rbd <- as.character(simce4to2018$rbd)

s4b2018 <- simce4to2018[c(3,34:36)]
library(reshape2)
DF1 <- melt(s4b2018, id.var='rbd')

DF1 %>% drop_na(value) %>% ggplot(aes(x = rbd, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2018")




