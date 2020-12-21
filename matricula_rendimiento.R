library(dplyr)
library(tidyverse)
library(dslabs)

matricula <- readRDS("~/R/projects/slepca/slepca/data/matricula16_20.Rdata")
matricula$rbd <- as.character(matricula$rbd)
matricula$cod_com_rbd <- as.character(matricula$cod_com_rbd)


#dataframe matricula año comuna
#comunas <- c("Carahue", "Nueva Imperial", "Saavedra", "Tedoro Schmidt", "Tolten")
#agnos <- c(2016, 2017, 2018, 2019, 2020)

df_com_agno <- matricula %>%
  group_by(X_agno, nom_com_rbd) %>%
  filter(estado_estab == 1) %>%
  summarize(total = sum(mat_total)) 


p <- df_com_agno %>% 
  ggplot(aes(X_agno , total, colour = nom_com_rbd)) + 
  geom_line(aes(linetype = nom_com_rbd), size = 1) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula por Año y Comuna")
p

# por tipo educa
df_com_bas <- matricula %>%
  group_by(X_agno, nom_com_rbd) %>%
  filter(estado_estab == 1 & cod_ense == 110) %>%
  summarize(basica = sum(mat_total)) 


b <- df_com_bas %>% 
  ggplot(aes(X_agno , basica, colour = nom_com_rbd)) + 
  geom_line(aes(linetype = nom_com_rbd), size = 1) + 
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. Básica por Año y Comuna")
b

df_com_med <- matricula %>%
  group_by(X_agno, nom_com_rbd) %>%
  filter(estado_estab == 1 & cod_ense == 310) %>%
  summarize(media = sum(mat_total)) 


m <- df_com_med %>% 
  ggplot(aes(X_agno , media, colour = nom_com_rbd)) + 
  geom_line(aes(linetype = nom_com_rbd), size = 1) + 
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. Media por Año y Comuna")
m

df_com_tp <- matricula %>%
  group_by(X_agno, nom_com_rbd) %>%
  filter(estado_estab == 1 & cod_ense >= 410) %>%
  summarize(tp = sum(mat_total)) 


t <- df_com_tp %>% 
  ggplot(aes(X_agno , tp, colour = nom_com_rbd)) + 
  geom_line(aes(linetype = nom_com_rbd), size = 1) + 
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. TP por Año y Comuna")
t
