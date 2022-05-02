library(dplyr)
library(tidyverse)
library(gridExtra)

#SEPA Inicial 2021 carga
establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
establecimientos$RBD <- as.character(establecimientos$RBD)
dia_ejes <- readRDS("~/R/projects/slepca/data/dia_ejes_res_2021.Rdata")
dia_ejes$RBD <- as.character(dia_ejes$RBD)
dia_categ <- readRDS("~/R/projects/slepca/data/dia_categ_2021.Rdata")
dia_categ$RBD <- as.character(dia_categ$RBD)
estab <- establecimientos %>% select("RBD", "COMUNA", "AREA", "MATRICULA", "ASIST"
                                     , "PO", "MAPUCHE", "F", "M")
dc <- inner_join(x=dia_categ, y=estab, by= "RBD")
dc <- dc %>% filter(SECTOR == "Lectura") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS),1), INTE = round(mean(INT),1), SATI = round(mean(SAT),1))

b1 <- dc %>%
  ggplot(aes(COMUNA, INSA)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#FF4400") +
  geom_text(aes(label=INSA, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Lectura Insatisfactorio")

b2 <- dc %>%
  ggplot(aes(COMUNA, INTE)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00FF00") +
  geom_text(aes(label=INTE, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Lectura Itermedio")

b3 <- dc %>%
  ggplot(aes(COMUNA, SATI)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#0000FF") +
  geom_text(aes(label=SATI, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Lectura Satisfactorio")

grid.arrange(b1, b2, b3, ncol = 1)

dc <- inner_join(x=dia_categ, y=estab, by= "RBD")
dc <- dc %>% filter(SECTOR == "Matematicas") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS),1), INTE = round(mean(INT),1), SATI = round(mean(SAT),1))

b1 <- dc %>%
  ggplot(aes(COMUNA, INSA)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#FF4400") +
  geom_text(aes(label=INSA, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Matemáticas Insatisfactorio")

b2 <- dc %>%
  ggplot(aes(COMUNA, INTE)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00FF00") +
  geom_text(aes(label=INTE, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Matemáticas Itermedio")

b3 <- dc %>%
  ggplot(aes(COMUNA, SATI)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#0000FF") +
  geom_text(aes(label=SATI, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Matemáticas Satisfactorio")

grid.arrange(b1, b2, b3, ncol = 1)

de <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de <- de %>% filter(SECTOR == "Lenguaje") %>%
  group_by(COMUNA) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))

ggplot(de, aes(x = COMUNA, y = RESCORR, fill = COMUNA)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=RESCORR), position = position_dodge(0.9), vjust = 2.5) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio de Respuestas Correctas por Comuna  DIA Lenguaje")

de <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de <- de %>% filter(SECTOR == "Matematicas") %>%
  group_by(COMUNA) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))

ggplot(de, aes(x = COMUNA, y = RESCORR, fill = COMUNA)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=RESCORR), position = position_dodge(0.9), vjust = 2.5) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio de Respuestas Correctas por Comuna  DIA Matemáticas")
