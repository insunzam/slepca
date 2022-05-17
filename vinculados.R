library(dplyr)
library(reshape2)
library(tidyverse)
library(reshape2)

#vinculados
#saveRDS(vinculados, file = "~/Documents/R/proyectos/slepca/data/vinculados.rda")
#vinc <- readRDS("~/R/projects/slepca/data/vinculados.rda")
esta <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
estab <- esta %>% select("RBD", "COMUNA", "AREA", "MATRICULA", "ASIST"
                                     , "PO", "MAPUCHE", "F", "M")
estab$RBD <- as.character(estab$RBD)
evalab <- readRDS("~/R/projects/slepca/data/eval_doce_ab.Rdata")
evaldoce <- readRDS("~/R/projects/slepca/data/eval_docente.Rdata")

mezcla <- inner_join(x=evalab, y=estab, by= "RBD")
mezcla$RBD <- as.character(mezcla$RBD)

ab <- mezcla %>% 
  filter(MATRICULA > 0 & T.A.B > 0) %>% 
  ggplot(aes(T.A.B, MATRICULA, label = RBD))
ab + geom_text(nudge_x = 0) +
  geom_point(size = 1) +
  xlab("Nº de Docentes en Categoria A+B")+
  ylab("Matricula") +
  ggtitle("Categorias Docentes (A y B) y Matricula") + 
  geom_point(aes(col = AREA), size = 1.2)

ab2 <- mezcla %>% 
  filter(MATRICULA > 0 & T.A.B > 0) %>% 
  ggplot(aes(T, MATRICULA, label = RBD))
ab2 + geom_text(nudge_x = 0) +
  geom_point(size = 1) +
  xlab("Nº de Docentes con Categoria")+
  ylab("Matricula") +
  ggtitle("Categorias Docentes y Matricula") + 
  geom_point(aes(col = AREA), size = 1.2)

dia_ejes <- readRDS("~/R/projects/slepca/data/dia_ejes_res_2021.Rdata")
dia_ejes$RBD <- as.character(dia_ejes$RBD)

de_rbd <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de_rbd <- de_rbd %>% filter(SECTOR == "Lenguaje") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))

write_csv(de_rbd, file = "~/R/projects/slepca/resultados/dia_eje_leng_rbd.csv")

rc_leng <- inner_join(x=de_rbd, y=mezcla, by= "RBD")

ab3 <- rc_leng %>% 
  filter(MATRICULA > 0 & T.A.B > 0) %>% 
  ggplot(aes(T.A.B, RESCORR, label = RBD))
ab3 + geom_text(nudge_x = 0) +
  geom_point(size = 1) +
  xlab("Nº de Docentes con Categoria")+
  ylab("Matricula") +
  ggtitle("Categorias Docentes y Matricula") + 
  geom_point(aes(col = AREA), size = 1.2)

de_rbd <- de_rbd %>% filter(SECTOR == "Matematicas") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))
