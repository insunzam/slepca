library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(reshape2)
library(plyr)
resultados <- read_rds("~/R/projects/slepca/slepca/data/result_ccg.Rdata") %>%
  filter(distrito == 22 & !is.na(candidato) & !is.na(votos_provisorio) & !is.na(pacto))
rescan <- ddply(resultados, .(candidato, pacto), summarize, total = sum(votos_provisorio))
