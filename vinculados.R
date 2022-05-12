library(dplyr)
library(reshape2)

#vinculados
saveRDS(vinculados, file = "~/Documents/R/proyectos/slepca/data/vinculados.rda")
vinc <- readRDS("~/R/projects/slepca/data/vinculados.rda")
esta <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
