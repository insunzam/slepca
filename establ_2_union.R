library(dplyr)
library(tidyverse)
#library(hrbrthemes)

matricula <- readRDS("~/R/projects/slepca/data/matricula16_20.Rdata")
establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")

mezc <- inner_join(x=matricula, y=establecimientos, by= "RBD")
