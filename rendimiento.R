library(dplyr)
library(tidyverse)
library(dslabs)
library(ggplot2)
library(plotly)

rendimiento <- readRDS("~/R/projects/slepca/slepca/data/rendimiento17_19.Rdata")

asist_ca <- rendimiento %>%
  group_by(AGNO, NOM_COM_RBD) %>%
#  filter(estado_estab == 1) %>%
  summarize(total = round(mean(PROM_ASIS),1)) 

p <- asist_ca %>% 
  ggplot(aes(AGNO, total, fill = NOM_COM_RBD, color = NOM_COM_RBD)) +
  #geom_density(alpha=0.2) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_text(aes(label=total), position = position_dodge(width = 0.9),
            vjust=-0.25) +
  xlab("Año") + ylab("% Asistencia") +
  ggtitle("Asistencia Promedio por Año y Comuna")
#p + scale_color_brewer(palette="Dark2")
p
p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#FF4400", "#00FF44"))

