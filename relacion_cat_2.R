library(dplyr)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(ggrepel)

esta <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
estab <- esta %>% filter(MATRICULA < 100) %>% select("RBD", "COMUNA", "AREA", "MATRICULA", "MAPUCHE", "F", "M")
estab$RBD <- as.character(estab$RBD)
evalab <- readRDS("~/R/projects/slepca/data/eval_doce_ab.Rdata")
evalab$RBD <- as.character(evalab$RBD)
#evaldoce <- readRDS("~/R/projects/slepca/data/eval_docente.Rdata")
mezcla <- inner_join(x=evalab, y=estab, by= "RBD")
mezcla$RBD <- as.character(mezcla$RBD)

#Relación entre resultados DIA - EJES 2021 y docentes con categoria

dia_ejes <- readRDS("~/R/projects/slepca/data/dia_ejes_res_2021.Rdata")
dia_ejes$RBD <- as.character(dia_ejes$RBD)
de_rbd <- inner_join(x=dia_ejes, y=estab, by= "RBD")
#write_csv(de_rbd, file = "~/R/projects/slepca/resultados/dia_eje_rbd.csv")

#Lenguaje
de_rbd_leng <- de_rbd %>% filter(SECTOR == "Lenguaje") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))

#write_csv(de_rbd_leng, file = "~/R/projects/slepca/resultados/dia_eje_leng_rbd.csv")

rc_leng <- inner_join(x=de_rbd_leng, y=mezcla, by= "RBD")

rdl <- rc_leng %>% filter(T.A.B > 0)  
rrdl <- rdl %>% ggplot(aes(T.A.B, RESCORR, label = RBD)) +
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (A+B) y Respuestas Correctas Lenguaje DIA 2021") 
#+ geom_point(aes(col = AREA), size = 1.2)

rdlt <- rc_leng %>% filter(T > 0) 
rrdlt <- rdlt %>% ggplot(aes(T, RESCORR, label = RBD)) +
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (Todas) y Respuestas Correctas Lenguaje DIA 2021") 
#+ geom_point(aes(col = AREA), size = 1.2)
grid.arrange(rrdl, rrdlt, ncol = 2)

#Matemática
de_rbd_mat <- de_rbd %>% filter(SECTOR == "Matematicas") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))
#write_csv(de_rbd_mat, file = "~/R/projects/slepca/resultados/dia_eje_mat_rbd.csv")

rc_mat <- inner_join(x=de_rbd_mat, y=mezcla, by= "RBD")

rdm <- rc_mat %>% filter(T.A.B > 0) 
rrdm <- rdm %>% ggplot(aes(T.A.B, RESCORR, label = RBD)) +
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (A + B) y Respuestas Correctas Matemáticas DIA 2021") 
#+ geom_point(aes(col = AREA), size = 1.2)

rdmt <- rc_mat %>% filter(T > 0) 
rrdmt <- rdmt %>% ggplot(aes(T, RESCORR, label = RBD)) + 
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (Todas) y Respuestas Correctas Matemáticas DIA 2021") 
#+ geom_point(aes(col = AREA), size = 1.2)
grid.arrange(rrdm, rrdmt, ncol = 2)
