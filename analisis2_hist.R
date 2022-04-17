#saveRDS(resultados_agencia_8esc, file = "~/Documents/R/proyectos/slepca/data/8estab_1.rda")
estab <- readRDS("~/Documents/R/proyectos/slepca/data/8estab_1.rda")
#data_esta <- readRDS("~/Documents/R/proyectos/slepca/data/establecimientos.rda")
library(dplyr)
library(tidyverse)
library(dslabs)

estab$rbd <- as.character(estab$rbd)
#Boxplot por nivel todos los años


