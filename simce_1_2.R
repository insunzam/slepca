library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(reshape2)
library(plyr)

setwd("/home/insunzamnt/R/projects/slepca/slepca")
codigos <- c(9118,9117,9116,9102,9111)
codigo2 <- c(14,15,16,17,18)

#Carga de datos
simce4to2019 <- readRDS("data/simce4to2019.rda")
simce4to2018 <- readRDS("data/simce4tob2018.rda")
simce4to2017 <- readRDS("data/simce4to2017.Rdata") %>% 
  filter(cod_reg_rbd == 9 & cod_depe1 == 2 & cod_com_rbd %in% codigo2)
simce4to2017$palu_eda_ins_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ins_lect4b_rbd)
simce4to2017$palu_eda_ele_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ele_lect4b_rbd)
simce4to2017$palu_eda_ade_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ade_lect4b_rbd)
simce4to2017$palu_eda_ins_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ins_mate4b_rbd)
simce4to2017$palu_eda_ele_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ele_mate4b_rbd)
simce4to2017$palu_eda_ade_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ade_mate4b_rbd)
simce4to2016 <- readRDS("data/simce4to2016.Rdata") %>% 
  filter(cod_reg_rbd == 9 & cod_depe1 == 2 & cod_com_rbd %in% codigos)

#Estandares
#Comunas

s4b2016LC <- simce4to2016[c(13,37,36,35)]
names(s4b2016LC)[1] <- "Comuna"
names(s4b2016LC)[2] <- "Adecuado"
names(s4b2016LC)[3] <- "Elemental"
names(s4b2016LC)[4] <- "Insuficiente"

s4b2017LC <- simce4to2017[c(11,34,33,32)]
names(s4b2017LC)[1] <- "Comuna"
names(s4b2017LC)[2] <- "Adecuado"
names(s4b2017LC)[3] <- "Elemental"
names(s4b2017LC)[4] <- "Insuficiente"

s4b2018LC <- simce4to2018[c(11,36,35,34)]
names(s4b2018LC)[1] <- "Comuna"
names(s4b2018LC)[2] <- "Adecuado"
names(s4b2018LC)[3] <- "Elemental"
names(s4b2018LC)[4] <- "Insuficiente"

s4b2019LC <- simce4to2019[c(2,12:14)]
names(s4b2019LC)[1] <- "Comuna"
names(s4b2019LC)[2] <- "Adecuado"
names(s4b2019LC)[3] <- "Elemental"
names(s4b2019LC)[4] <- "Insuficiente"

DF1L <- melt(s4b2016LC, id.var='Comuna')
e1 <- DF1L %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = Comuna, y = value, fill = variable, label = value)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette="Dark2") + 
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2016")


DF1L <- melt(s4b2017LC, id.var='Comuna')
e2 <- DF1L %>% filter(value > 0) %>% 
  ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2017")

DF1L <- melt(s4b2018LC, id.var='Comuna')
e3 <- DF1L %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = Comuna, y = value, fill = variable, label = value)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette="Dark2") + 
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2018")


DF1L <- melt(s4b2019LC, id.var='Comuna')
e4 <- DF1L %>% filter(value > 0) %>% 
  ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Lenguaje SIMCE 4toB 2019")

grid.arrange(e1, e2, e3, e4, ncol = 2)

s4b2016MC <- simce4to2016[c(13,40, 39, 38)]
names(s4b2016MC)[1] <- "Comuna"
names(s4b2016MC)[2] <- "Adecuado"
names(s4b2016MC)[3] <- "Elemental"
names(s4b2016MC)[4] <- "Insuficiente"

s4b2017MC <- simce4to2017[c(11,37,36,35)]
names(s4b2017MC)[1] <- "Comuna"
names(s4b2017MC)[2] <- "Adecuado"
names(s4b2017MC)[3] <- "Elemental"
names(s4b2017MC)[4] <- "Insuficiente"

s4b2018MC <- simce4to2018[c(11,39, 38, 37)]
names(s4b2018MC)[1] <- "Comuna"
names(s4b2018MC)[2] <- "Adecuado"
names(s4b2018MC)[3] <- "Elemental"
names(s4b2018MC)[4] <- "Insuficiente"

s4b2019MC <- simce4to2019[c(2,23,24,25)]
names(s4b2019MC)[1] <- "Comuna"
names(s4b2019MC)[2] <- "Adecuado"
names(s4b2019MC)[3] <- "Elemental"
names(s4b2019MC)[4] <- "Insuficiente"


DF2C <- melt(s4b2016MC, id.var='Comuna')
m1 <- DF2C %>% filter(value>0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2016")

DF2C <- melt(s4b2017MC, id.var='Comuna')
m2 <- DF2C %>% filter(value > 0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2017")

DF2C <- melt(s4b2018MC, id.var='Comuna')
m3 <- DF2C %>% filter(value>0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2018")

DF2C <- melt(s4b2019MC, id.var='Comuna')
m4 <- DF2C %>% filter(value > 0) %>% ggplot(aes(x = Comuna, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("Estandar") +
  ggtitle("Distribución Estandares de Aprendizaje Matemáticas SIMCE 4toB 2019")

grid.arrange(m1, m2, m3, m4, ncol = 2)
