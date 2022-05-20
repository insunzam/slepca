library(dplyr)
library(tidyverse)
library(dslabs)
library(reshape2)

#SEPA Inicial 2021 carga
sepa_ini <- readRDS("~/R/projects/slepca/data/sepa_inicial_puntaje.Rdata")
sepa_ini$RBD <- as.character(sepa_ini$RBD)
sepa_ini_ejes <- readRDS("~/R/projects/slepca/data/sepa_inicial_ejes.Rdata")
sepa_ini_ejes <- sepa_ini_ejes[order(sepa_ini_ejes$SECTOR,sepa_ini_ejes$EJE),]
sepa_ini_ejes$RBD <- as.character(sepa_ini_ejes$RBD)
sepa_ini_ejes$NIVEL <- replace(sepa_ini_ejes$NIVEL, sepa_ini_ejes$NIVEL == "1° Básico", "1B")
sepa_ini_ejes$NIVEL <- replace(sepa_ini_ejes$NIVEL, sepa_ini_ejes$NIVEL == "2° Básico", "2B")
sepa_ini_ejes$NIVEL <- replace(sepa_ini_ejes$NIVEL, sepa_ini_ejes$NIVEL == "3° Básico", "3B")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "INFORMACION_EXPLICITA", "IE")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "INFORMACION IMPLICITA", "II")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "SENTIDO GLOBAL", "SG")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "SCOM_ELE ESTRUC", "SCEE")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "CONYREC_LENGUAJE", "CRL")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "NUMEROS", "NUM")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "GEOMETRIA", "GEO")
sepa_ini_ejes$EJE <- replace(sepa_ini_ejes$EJE, sepa_ini_ejes$EJE == "ESTADYPROB", "PYP")
sepa_ini_ejes$SECTOR <- replace(sepa_ini_ejes$SECTOR, sepa_ini_ejes$SECTOR == "LENGUAJE", "1")
sepa_ini_ejes$SECTOR <- replace(sepa_ini_ejes$SECTOR, sepa_ini_ejes$SECTOR == "MATEMATICAS", "2")

#SEPA Final 2021 carga
sepa_fin <- readRDS("~/R/projects/slepca/data/sepa_final_puntaje.Rdata")
sepa_fin$RBD <- as.character(sepa_fin$RBD)
sepa_fin_ejes <- readRDS("~/R/projects/slepca/data/sepa_final_ejes.Rdata")
sepa_fin_ejes <- sepa_fin_ejes[order(sepa_fin_ejes$SECTOR,sepa_fin_ejes$EJE),]
sepa_fin_ejes$RBD <- as.character(sepa_fin_ejes$RBD)
sepa_fin_ejes$VALOR <- as.numeric(sepa_fin_ejes$VALOR)
sepa_fin$NIVEL <- replace(sepa_fin$NIVEL, sepa_fin$NIVEL == "1° Básico", "1B")
sepa_fin$NIVEL <- replace(sepa_fin$NIVEL, sepa_fin$NIVEL == "2° Básico", "2B")
sepa_fin$NIVEL <- replace(sepa_fin$NIVEL, sepa_fin$NIVEL == "3° Básico", "3B")
sepa_fin$NIVEL <- replace(sepa_fin$NIVEL, sepa_fin$NIVEL == "4° Básico", "4B")
sepa_fin_ejes$NIVEL <- replace(sepa_fin_ejes$NIVEL, sepa_fin_ejes$NIVEL == "1° Básico", "1B")
sepa_fin_ejes$NIVEL <- replace(sepa_fin_ejes$NIVEL, sepa_fin_ejes$NIVEL == "2° Básico", "2B")
sepa_fin_ejes$NIVEL <- replace(sepa_fin_ejes$NIVEL, sepa_fin_ejes$NIVEL == "3° Básico", "3B")
sepa_fin_ejes$NIVEL <- replace(sepa_fin_ejes$NIVEL, sepa_fin_ejes$NIVEL == "4° Básico", "4B")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "INFORMACION EXPLICITA", "IE")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "INFORMACION IMPLICITA", "II")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "CONOCIMIENTO Y RECURSOS DEL LENGUAJE", "CRL")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "ESTADÍSTICA Y PROBABILIDAD", "EYP")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "GEOMETRÍA", "GEO")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "NÚMEROS", "NUM")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "SENTIDO GLOBAL", "SG")
sepa_fin_ejes$EJE <- replace(sepa_fin_ejes$EJE, sepa_fin_ejes$EJE == "SITUACION COMUNICATIVA Y ELEMENTOS ESTRUCTURALES", "SCEE")
sepa_fin_ejes$SECTOR <- replace(sepa_fin_ejes$SECTOR, sepa_fin_ejes$SECTOR == "LENGUAJE", "1")
sepa_fin_ejes$SECTOR <- replace(sepa_fin_ejes$SECTOR, sepa_fin_ejes$SECTOR == "MATEMATICAS", "2")

#SEPA Inicial 2021
sepini_nivel <- sepa_ini %>% filter(PUNTAJE > 0)
sepini_nivel_ejes <- sepa_ini_ejes %>% filter(VALOR > 0)

sepini_nivel <- sepini_nivel %>% 
  group_by(PRUEBA, NIVEL) %>% 
  summarize(PUNTAJE = round(mean(PUNTAJE),1))

ggplot(sepini_nivel, aes(x = PRUEBA, y = PUNTAJE, fill = NIVEL)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=PUNTAJE), position = position_dodge(0.9), vjust = 2.5) +
  ggtitle("Resultados Promedio por Nivel - SEPA Inicial")

sepini_nivel_ejes <- sepini_nivel_ejes %>% 
  group_by(SECTOR, EJE, NIVEL) %>% 
  summarize(VALOR = round(mean(VALOR),2))

  ggplot(sepini_nivel_ejes, aes(x = EJE , y = VALOR, fill = NIVEL)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(VALOR)), position = position_dodge(0.9), vjust = 2.5) +
  ggtitle("Resultados Promedio por Eje y Nivel - SEPA Inicial")

write_csv(sepini_nivel, file = "~/R/projects/slepca/resultados/sepini_nivel.csv")
write_csv(sepini_nivel_ejes, file = "~/R/projects/slepca/resultados/sepini_nivel_ejes.csv")  

#SEPA Final 2021
  sepfin_nivel <- sepa_fin %>% filter(PUNTAJE > 0)
  sepfin_nivel_ejes <- sepa_fin_ejes %>% filter(VALOR > 0)
  
  sepfin_nivel <- sepfin_nivel %>% 
    group_by(SECTOR, NIVEL) %>% 
    summarize(PUNTAJE = round(mean(PUNTAJE),1))
  
  ggplot(sepfin_nivel, aes(x = SECTOR, y = PUNTAJE, fill = NIVEL)) +
    geom_col(position = "dodge")  +
    geom_text(aes(label=PUNTAJE), position = position_dodge(0.9), vjust = 2.5) +
    ggtitle("Resultados Promedio por Sector y Nivel - SEPA Final")
  
  sepfin_nivel_ejes <- sepfin_nivel_ejes %>% 
    group_by(SECTOR, EJE, NIVEL) %>% 
    summarize(VALOR = round(mean(VALOR),2))
  
  # sepini_nivel_ejes %>%
  #  mutate(EJE=fct_reorder(EJE,SECTOR)) %>%
  ggplot(sepfin_nivel_ejes, aes(x = EJE , y = VALOR, fill = NIVEL)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels=percent) +
    geom_text(aes(label=scales::percent(VALOR)), position = position_dodge(0.9), vjust = 2.5) +
    ggtitle("Resultados Promedio por Eje y Nivel - SEPA Final")

write_csv(sepfin_nivel, file = "~/R/projects/slepca/resultados/sepfin_nivel.csv")
write_csv(sepfin_nivel_ejes, file = "~/R/projects/slepca/resultados/sepfin_nivel_ejes.csv")
