#saveRDS(resultados_agencia_8esc, file = "~/Documents/R/proyectos/slepca/data/8estab_1.rda")
estab <- readRDS("~/R/projects/slepca/slepca/data/8estab_1.rda")
#data_esta <- readRDS("~/Documents/R/proyectos/slepca/data/establecimientos.rda")
library(dplyr)
library(tidyverse)
library(dslabs)

estab$rbd <- as.character(estab$rbd)
#Boxplot por nivel todos los años
p <- estab %>% 
  filter(agno == 2017) %>%
  ggplot(aes(grado, prom_lect, fill = comuna))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- estab %>% 
  filter(grado == '8b') %>%
  ggplot(aes(rbd, prom_lect, fill = comuna))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- estab %>% 
  filter(grado == '2m') %>%
  ggplot(aes(rbd, prom_lect, fill = comuna))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


