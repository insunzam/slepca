library(dplyr)
library(tidyverse)
#library(dslabs)
library(gridExtra)
#library(reshape2)

setwd("/home/insunzamnt/R/projects/slepca/slepca")
codigos <- c(9118,9117,9116,9102,9111)
codigo2 <- c(14,15,16,17,18)
agno19 <- c(2019)

#Carga de datos
simce4to2019 <- readRDS("data/simce4to2019.rda")
simce4to2019$agno <- agno19
simce4to2019 <- mutate(simce4to2019, cod_rural_rbd = ifelse(AREA == "URBANO", 1, 
                                                        ifelse(AREA == "RURAL", 2, NA)))
simce4to2018 <- readRDS("data/simce4tob2018.rda")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Carah", "CARAHUE")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Nueva", "NUEVA IMPERIAL")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Saave", "SAAVEDRA")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Teodo", "TEODORO SCHMIDT")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Tolté", "TOLTËN")

simce4to2017 <- readRDS("data/simce4to2017.Rdata") %>% 
  filter(cod_reg_rbd == 9 & cod_depe1 == 2 & cod_com_rbd %in% codigo2)
simce4to2017$palu_eda_ins_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ins_lect4b_rbd)
simce4to2017$palu_eda_ele_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ele_lect4b_rbd)
simce4to2017$palu_eda_ade_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ade_lect4b_rbd)
simce4to2017$palu_eda_ins_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ins_mate4b_rbd)
simce4to2017$palu_eda_ele_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ele_mate4b_rbd)
simce4to2017$palu_eda_ade_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ade_mate4b_rbd)
simce4to2016 <- readRDS("data/simce4to2016.Rdata") %>% 
  filter(cod_reg_rbd == 9 & cod_depe1 == 2 & cod_com_rbd %in% codigos)

res_slep16 <- select(filter(simce4to2016, (prom_lect4b_rbd + prom_mate4b_rbd) > 0),
                        c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, prom_lect4b_rbd ,prom_mate4b_rbd))
res_slep17 <- select(filter(simce4to2017, (prom_lect4b_rbd + prom_mate4b_rbd) > 0),
       c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, prom_lect4b_rbd ,prom_mate4b_rbd))
res_slep18 <- select(filter(simce4to2018, (prom_lect4b_rbd + prom_mate4b_rbd) > 0),
                     c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, prom_lect4b_rbd ,prom_mate4b_rbd))
res_slep19 <- select(filter(simce4to2019, (PPEL + PPEM) > 0),
                     c(agno, RBD, COM, cod_rural_rbd, COD_GRUPO, PPEL ,PPEM))
names(res_slep19)[2] <- "rbd"
names(res_slep19)[3] <- "nom_com_rbd"
names(res_slep19)[5] <- "cod_grupo"
names(res_slep19)[6] <- "prom_lect4b_rbd"
names(res_slep19)[7] <- "prom_mate4b_rbd"


resultados_slep <- rbind(res_slep16, res_slep17, res_slep18, res_slep19)

resultados_slep$nom_com_rbd <- replace(resultados_slep$nom_com_rbd, resultados_slep$nom_com_rbd == "TOLTÉN", "TOLTEN")
resultados_slep$nom_com_rbd <- replace(resultados_slep$nom_com_rbd, resultados_slep$nom_com_rbd == "TOLTËN", "TOLTEN")
resultados_slep$rbd <- as.character(resultados_slep$rbd)
resultados_slep$agno <- as.character(resultados_slep$agno)

#grid.arrange(m1, m2, m3, m4, ncol = 2)

#Promedio 16 -19
res_com <- resultados_slep %>% 
  group_by(nom_com_rbd) %>% 
  summarize(pl = round(mean(prom_lect4b_rbd),1), pm = round(mean(prom_mate4b_rbd),1))

b1 <- res_com %>%
  ggplot(aes(nom_com_rbd, pl)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#FF0000") +
  geom_text(aes(label=pl, vjust = 2.5)) +
  xlab("Comuna") + ylab("Resultado") +
  ggtitle("Resultado promedio 2016 - 2019 4to Básico Lenguaje")

b2 <- res_com %>%
  ggplot(aes(nom_com_rbd, pm)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00FF00") +
  geom_text(aes(label=pm, vjust = 2.5)) +
  xlab("Comuna") + ylab("Resultado") +
  ggtitle("Resultado promedio 2016 - 2019 4to Básico Matemáticas")

grid.arrange(b1, b2)

#Resultados año/comuna

res_com2 <- resultados_slep %>% group_by(agno) %>%
  summarize(Lenguaje = round(mean(prom_lect4b_rbd),1), Matematicas = round(mean(prom_mate4b_rbd),1))

#res_com3 <- resultados_slep %>% group_by(agno, nom_com_rbd) %>% summarize(pl = round(mean(prom_lect4b_rbd),1), pm = round(mean(prom_mate4b_rbd),1))

df <- res_com2 %>%
  select(agno, Lenguaje, Matematicas) %>%
  gather(key= "variable", value = "value", -agno)
df$agno <- as.numeric(df$agno)

b <- ggplot(df, aes(agno, value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  expand_limits(y=150) +
  geom_label(aes(label = value)) +
  xlab("Año") + ylab("Resultados") +
  ggtitle("Resultados 4to Básico Lenguaje y Matemáticas 2016 - 2019")
b

# No aun
#df <- res_com %>%
#  select(nom_com_rbd, pl, pm) %>%
#  gather(key= "variable", value = "value", -nom_com_rbd)

#b <- ggplot(df, aes(nom_com_rbd, value)) +
#  geom_line(aes(color = variable, linetype = variable)) +
#  scale_color_manual(values = c("darkred", "steelblue")) +
#  xlab("Año") + ylab("Resultados") +
#  ggtitle("Resultados 4to Básico Lenguaje y Matemáticas 2016 - 2019")
#b
