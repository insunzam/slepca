library(dplyr)
library(tidyverse)

escuelas <- readRDS("~/R/projects/slepca/slepca/data/8estab_2.Rdata")
promedios <- readRDS("~/R/projects/slepca/slepca/data/8estab_promedios.Rdata")

escuelas$rbd <- as.character(escuelas$rbd)

#Boxplot por nivel todos los años
p <- escuelas %>% 
  filter(agno == 2017) %>%
  ggplot(aes(grado, prom_lect, fill = comuna))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- escuelas %>% 
  filter(grado == '8b') %>%
  ggplot(aes(rbd, prom_lect, fill = agno))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- escuelas %>% 
  filter(grado == '2m') %>%
  ggplot(aes(rbd, prom_lect, fill = comuna))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






