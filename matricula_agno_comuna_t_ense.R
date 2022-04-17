library(dplyr)
library(tidyverse)
library(dslabs)
library(ggrepel)

matricula <- readRDS("~/R/projects/slepca/data/matricula16_20.Rdata")
matricula[is.na(matricula)] <- 0
matricula$RBD <- as.character(matricula$RBD)
matricula$COD_COM_RBD <- as.character(matricula$COD_COM_RBD)


#dataframe matricula año comuna
#comunas <- c("Carahue", "Nueva Imperial", "Saavedra", "Tedoro Schmidt", "Tolten")
#agnos <- c(2016, 2017, 2018, 2019, 2020)

df_com_agno <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1) %>%
  summarize(total = sum(MAT_TOTAL)) 


p <- df_com_agno %>% 
  ggplot(aes(AGNO , total, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1.2) + 
  geom_label_repel(aes(label = total), nudge_x = 0.35, size = 4) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula por Año y Comuna")
p

write_csv(df_com_agno, file = "~/R/projects/slepca/resultados/comuna_agno.csv")

# por tipo educa

df_com_bas <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1 & xor(COD_ENSE == 110,COD_ENSE == 10)) %>%
  summarize(basica = sum(MAT_TOTAL)) 


b <- df_com_bas %>% 
  ggplot(aes(AGNO , basica, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1) + 
  geom_label_repel(aes(label = basica), nudge_x = 0.35, size = 4) + 
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. Básica por Año y Comuna")
b

write_csv(df_com_bas, file = "~/R/projects/slepca/resultados/comuna_agno_basica.csv")

df_com_med <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1 & COD_ENSE == 310) %>%
  summarize(media = sum(MAT_TOTAL)) 


m <- df_com_med %>% 
  ggplot(aes(AGNO , media, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1) + 
  geom_label_repel(aes(label = media), nudge_x = 0.35, size = 4) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. Media por Año y Comuna")
m

write_csv(df_com_med, file = "~/R/projects/slepca/resultados/comuna_agno_media.csv")

df_com_tp <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1 & COD_ENSE >= 410) %>%
  summarize(tp = sum(MAT_TOTAL)) 

t <- df_com_tp %>% 
  ggplot(aes(AGNO , tp, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1) + 
  geom_label_repel(aes(label = tp), nudge_x = 0.35, size = 4) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. TP por Año y Comuna")
t

write_csv(df_com_tp, file = "~/R/projects/slepca/resultados/comuna_agno_tp.csv")
