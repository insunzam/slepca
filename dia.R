library(dplyr)
library(tidyverse)
library(dslabs)
library(reshape2)
library(forcats)

#SEPA Inicial 2021 carga
dia_ejes <- readRDS("~/R/projects/slepca/data/dia_ejes_res_2021.Rdata")
dia_ejes$RBD <- as.character(dia_ejes$RBD)
dia_categ <- readRDS("~/R/projects/slepca/data/dia_categ_2021.Rdata")
dia_categ$RBD <- as.character(dia_categ$RBD)
