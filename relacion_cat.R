library(dplyr)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(ggrepel)

esta <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
estab <- esta %>% select("RBD", "COMUNA", "AREA", "MATRICULA", "MAPUCHE", "F", "M")
estab$RBD <- as.character(estab$RBD)
evalab <- readRDS("~/R/projects/slepca/data/eval_doce_ab.Rdata")
evalab$RBD <- as.character(evalab$RBD)
docentes21 <- readRDS("~/R/projects/slepca/data/dotacion2021.Rdata")
docentes21$EVALDOCE <- replace(docentes21$EVALDOCE, docentes21$EVALDOCE == "Evaluado", 1)
docentes21$EVALDOCE <- replace(docentes21$EVALDOCE, docentes21$EVALDOCE == "No Evaluado", 0)
docentes21$EVALDOCE <- as.numeric(docentes21$EVALDOCE)
#resumen de docentes evaluados y planta
#calcula el numero de docentes por establecimientos 
td <- docentes21 %>%
  group_by(RBD) %>%
  summarise(tot_doc = n())
#calcula el número de docentes evaluados en categorias a y b por establecimientos 
tab <- docentes21 %>%
  filter(grepl('A|B',CATEGORIA)) %>%
  group_by(RBD) %>%
  summarise(tot_ab = n())
#calcula el pronedio de docentes evaluados en categoarias a y b por establecimientos 
teval <- inner_join(x=td, y=tab, by= "RBD")
teval <- teval %>%
  mutate(p_ab = round(tot_ab/tot_doc, digits = 3))

#calcula el pronedio de docentes evaluados por establecimientos 
teval_t <- docentes21 %>%
  group_by(RBD) %>%
  summarise_at(vars(EVALDOCE), list(p_eva = mean))
teval_t$p_eva <- round(df$p_evaS, digits = 3)

df <- inner_join(x=teval, y=teval_t, by= "RBD")

#eval_ab <- inner_join(x=evalab, y=df, by= "RBD")

evaldoce <- readRDS("~/R/projects/slepca/data/eval_docente.Rdata")
mezcla <- inner_join(x=df, y=estab, by= "RBD")
mezcla$RBD <- as.character(mezcla$RBD)
write_csv(mezcla, file = "~/R/projects/slepca/resultados/mezcla.csv")

#Relación entre matricula y docentes con categoria
md <- mezcla %>% filter(MATRICULA > 0 & T.A.B > 0) 
rmdab <- md %>% ggplot(aes(T.A.B, MATRICULA)) +
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
#  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados A+B")+
  ylab("Matricula") +
  ggtitle("Evaluación Docente (A y B) y Matricula") + 
  geom_point(aes(col = AREA), size = 1.2)

mdt <- mezcla %>% filter(MATRICULA > 0 & T > 0)
rmdt <- mdt %>% ggplot(aes(T, MATRICULA, label = RBD)) + 
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
#  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Matricula") +
  ggtitle("Evaluación Docente (Todas) y Matricula") + 
  geom_point(aes(col = AREA), size = 1.2)
grid.arrange(rmdab, rmdt, ncol = 2)

#Relación entre resultados DIA - EJES 2021 y docentes con categoria

dia_ejes <- readRDS("~/R/projects/slepca/data/dia_ejes_res_2021.Rdata")
dia_ejes$RBD <- as.character(dia_ejes$RBD)
de_rbd <- inner_join(x=dia_ejes, y=estab, by= "RBD")
write_csv(de_rbd, file = "~/R/projects/slepca/resultados/dia_eje_rbd.csv")

#Lenguaje
de_rbd_leng <- de_rbd %>% filter(SECTOR == "Lenguaje") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))

write_csv(de_rbd_leng, file = "~/R/projects/slepca/resultados/dia_eje_leng_rbd.csv")

rc_leng <- inner_join(x=de_rbd_leng, y=mezcla, by= "RBD")
write_csv(rc_leng, file = "~/R/projects/slepca/resultados/datos_lenguaje.csv")

rdl <- rc_leng %>% filter(p_ab > 0)  
rrdl <- rdl %>% ggplot(aes(p_ab, RESCORR, label = RBD)) +
  geom_label_repel(aes(label = RBD), max.overlaps = 20, 
                   xlim = c(-Inf, Inf), ylim = c(-Inf, Inf), size = 2) +
  #, nudge_x = 0.35, size = 2
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (A+B) y Respuestas Correctas Lenguaje DIA 2021") + 
  geom_point(aes(col = AREA), size = 1.2)

rdlt <- rc_leng %>% filter(p_eva > 0) 
rrdlt <- rdlt %>% ggplot(aes(p_eva, RESCORR, label = RBD)) +
  geom_label_repel(aes(label = RBD), max.overlaps = 20, 
                   xlim = c(-Inf, Inf), ylim = c(-Inf, Inf), size = 2) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (Todas) y Respuestas Correctas Lenguaje DIA 2021") + 
  geom_point(aes(col = AREA), size = 1.2)
grid.arrange(rrdl, rrdlt, ncol = 2)

#Matemática
de_rbd_mat <- de_rbd %>% filter(SECTOR == "Matematicas") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),1))
write_csv(de_rbd_mat, file = "~/R/projects/slepca/resultados/dia_eje_mat_rbd.csv")

rc_mat <- inner_join(x=de_rbd_mat, y=mezcla, by= "RBD")

rdm <- rc_mat %>% filter(T.A.B > 0) 
rrdm <- rdm %>% ggplot(aes(T.A.B, RESCORR, label = RBD)) +
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (A + B) y Respuestas Correctas Matemáticas DIA 2021") + 
  geom_point(aes(col = AREA), size = 1.2)

rdmt <- rc_mat %>% filter(T > 0) 
rrdmt <- rdmt %>% ggplot(aes(T, RESCORR, label = RBD)) + 
  geom_label_repel(aes(label = RBD), nudge_x = 0.35, size = 4) +
  #  geom_text(nudge_x = 1.5, nudge_y = 1.5) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Nº de Docentes Evaluados")+
  ylab("Respuestas Correctas") +
  ggtitle("Evaluación Docente (Todas) y Respuestas Correctas Matemáticas DIA 2021") + 
  geom_point(aes(col = AREA), size = 1.2)
grid.arrange(rrdm, rrdmt, ncol = 2)
