library(dplyr)
library(tidyverse)
#library(dslabs)
library(gridExtra)
#library(reshape2)

setwd("/home/insunzamnt/R/projects/slepca/slepca")
agno18 <- c(2018)

#Carga de datos
idps4to2016 <- readRDS("data/idps4to2016.Rdata")
idps4to2017 <- readRDS("data/idps4to2017.Rdata")
idps4to2017$nom_com_rbd <- replace(idps4to2017$nom_com_rbd, idps4to2017$nom_com_rbd == "TOLTÉN", "TOLTEN")
idps4to2018 <- readRDS("data/idps4to2018.Rdata")
idps4to2018$agno <- agno18
names(idps4to2018)[10] <- "nom_com_rbd"
names(idps4to2018)[12] <- "ind_am"
names(idps4to2018)[15] <- "ind_cc"
names(idps4to2018)[19] <- "ind_hv"
names(idps4to2018)[23] <- "ind_pf"
idps4to2018$nom_com_rbd <- replace(idps4to2018$nom_com_rbd, idps4to2018$nom_com_rbd == "TOLTÉN", "TOLTEN")
idps4to2019 <- readRDS("data/idps4to2019.Rdata")
idps4to2019$nom_com_rbd <- replace(idps4to2019$nom_com_rbd, idps4to2019$nom_com_rbd == "TOLTÉN", "TOLTEN")

res_slep16 <- select(filter(idps4to2016, (ind_am + ind_cc + ind_hv + ind_pf) > 0),
                        c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, ind_am, ind_cc, ind_hv, ind_pf))
res_slep17 <- select(filter(idps4to2017, (ind_am + ind_cc + ind_hv + ind_pf) > 0),
                     c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, ind_am, ind_cc, ind_hv, ind_pf))
res_slep18 <- select(filter(idps4to2018, (ind_am + ind_cc + ind_hv + ind_pf) > 0),
                     c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, ind_am, ind_cc, ind_hv, ind_pf))
res_slep19 <- select(filter(idps4to2019, (ind_am + ind_cc + ind_hv + ind_pf) > 0),
                     c(agno, rbd, nom_com_rbd, cod_grupo, cod_rural_rbd, ind_am, ind_cc, ind_hv, ind_pf))

resultados_slep <- rbind(res_slep16, res_slep17, res_slep18, res_slep19)

#resultados_slep$nom_com_rbd <- replace(resultados_slep$nom_com_rbd, resultados_slep$nom_com_rbd == "TOLTÉN", "TOLTEN")
resultados_slep$rbd <- as.character(resultados_slep$rbd)
resultados_slep$agno <- as.character(resultados_slep$agno)

#Promedio 16 -19
res_com <- resultados_slep %>% 
  group_by(nom_com_rbd) %>% 
  summarize(am = round(mean(ind_am),1), cc = round(mean(ind_cc),1), 
            hv = round(mean(ind_hv),1), pf = round(mean(ind_pf),1))

b1 <- res_com %>%
  ggplot(aes(nom_com_rbd, am)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#FF4400") +
  geom_text(aes(label=am, vjust = 2.5)) +
  xlab("Comuna") + ylab("Resultado") +
  ggtitle("Resultado promedio 2016 - 2019 4to Básico Autoestima")

b2 <- res_com %>%
  ggplot(aes(nom_com_rbd, cc)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00FF00") +
  geom_text(aes(label=cc, vjust = 2.5)) +
  xlab("Comuna") + ylab("Resultado") +
  ggtitle("Resultado promedio 2016 - 2019 4to Básico Convivencia")

b3 <- res_com %>%
  ggplot(aes(nom_com_rbd, hv)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#0000FF") +
  geom_text(aes(label=hv, vjust = 2.5)) +
  xlab("Comuna") + ylab("Resultado") +
  ggtitle("Resultado promedio 2016 - 2019 4to Básico Hábitos")

b4 <- res_com %>%
  ggplot(aes(nom_com_rbd, pf)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00F0F0") +
  geom_text(aes(label=pf, vjust = 2.5)) +
  xlab("Comuna") + ylab("Resultado") +
  ggtitle("Resultado promedio 2016 - 2019 4to Básico Participación")


grid.arrange(b1, b2, b3, b4, ncol = 2)

#Resultados año/comuna

res_com2 <- resultados_slep %>% group_by(agno) %>%
  summarize(Autoestima = round(mean(ind_am),1), 
            Convivencia = round(mean(ind_cc),1), 
            Habitos = round(mean(ind_hv),1), 
            Participacion = round(mean(ind_pf),1))

df <- res_com2 %>%
  select(agno, Autoestima, Convivencia, Habitos, Participacion) %>%
  gather(key= "variable", value = "value", -agno)
df$agno <- as.numeric(df$agno)

b <- ggplot(df, aes(agno, value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "steelblue", "orange", "darkgreen")) +
  expand_limits(y=40) +
  geom_label(aes(label = value)) +
  xlab("Año") + ylab("Resultados") +
  ggtitle("Resultados 4to Básico IDPS 2016 - 2019")
b

