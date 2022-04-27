library(dplyr)
library(tidyverse)
#library(hrbrthemes)

matricula <- readRDS("~/R/projects/slepca/data/matricula16_20.Rdata")
eval <- readRDS("~/R/projects/slepca/data/eval_doce_ab.Rdata")
establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
doce <- readRDS("~/R/projects/slepca/data/docentes2019_2021.Rdata")
doce_t <- doce[c("RBD","DC_A","HH_A", "DC_EDUC_TRAD", "HH_EDUC_TRAD", "DC_TOT", "HH_TOT" )]
doce_t <- subset(doce, AGNO == 2021, select = c("RBD","DC_A","HH_A", "DC_PROF_ENC", "HH_PROF_ENC", "DC_EDUC_TRAD", "HH_EDUC_TRAD", "DC_TOT", "HH_TOT" ))
doce_t <- subset(doce_t, DC_A > 0)

estab <- select(filter(establecimientos,AREA == "RURAL"), c("RBD","COMUNA","AREA","RED","MATRICULA"))

mezc <- inner_join(x=matricula, y=establecimientos, by= "RBD")


