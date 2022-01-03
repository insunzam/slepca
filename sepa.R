library(dplyr)
library(tidyverse)
library(dslabs)
library(reshape2)

sepa_ini <- readRDS("~/R/projects/slepca/data/sepa_inicial_puntaje.Rdata")
sepa_ini$RBD <- as.character(sepa_ini$RBD)

sepini_nivel <- sepa_ini %>% 
  group_by(PRUEBA, NIVEL) %>% 
  summarize(PUNTAJE = round(mean(PUNTAJE),1))

b <- ggplot(sepini_nivel, aes(x = PRUEBA, y = PUNTAJE, fill = NIVEL)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=PUNTAJE), position = position_dodge(0.9), vjust = 2.5) +
  ggtitle("Resultados Promedio por Nivel - SEPA LENGUAJE Inicial")
b


