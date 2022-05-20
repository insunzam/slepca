library(dplyr)
library(tidyverse)
library(gridExtra)
library(reshape2)

#SEPA Inicial 2021 carga
establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
establecimientos$RBD <- as.character(establecimientos$RBD)
dia_ejes <- readRDS("~/R/projects/slepca/data/dia_ejes_res_2021.Rdata")
dia_ejes$RBD <- as.character(dia_ejes$RBD)
dia_categ <- readRDS("~/R/projects/slepca/data/dia_categ_2021.Rdata")
dia_categ$RBD <- as.character(dia_categ$RBD)
estab <- establecimientos %>% select("RBD", "COMUNA", "AREA", "MATRICULA", "ASIST"
                                     , "PO", "MAPUCHE", "F", "M")

#CATEGORIAS
dc <- inner_join(x=dia_categ, y=estab, by= "RBD")
#filter(SECTOR == "Lectura") %>%
dcl <- dc %>% filter(SECTOR == "Lectura") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS)/100,2), INTE = round(mean(INT)/100,2), SATI = round(mean(SAT)/100,2))
dcl_n <- dc %>% filter(SECTOR == "Lectura") %>%
  group_by(NIVEL) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))
dcm <- dc %>% filter(SECTOR == "Matematicas") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))
dcm_n <- dc %>% filter(SECTOR == "Matematicas") %>%
  group_by(NIVEL) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))
dch <- dc %>% filter(SECTOR == "Historia") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))
dch_n <- dc %>% filter(SECTOR == "Historia") %>%
  group_by(NIVEL) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))
dcf <- dc %>% filter(SECTOR == "Formacion") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))
dcf_n <- dc %>% filter(SECTOR == "Formacion") %>%
  group_by(NIVEL) %>% 
  summarize(INSA = round(mean(INS)/100, 2), INTE = round(mean(INT)/100, 2), SATI = round(mean(SAT)/100, 2))

dcl <- melt(dcl, id.var='COMUNA')
e1 <- dcl %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = COMUNA, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") + 
  xlab("Comunas") + ylab(" % Categoría") +
  ggtitle("Distribución por Categoría - Lenguaje DIA 2021")

dcl_n <- melt(dcl_n, id.var='NIVEL')
e1n <- dcl_n %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = NIVEL, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") + 
  xlab("Niveles") + ylab(" % Categoría") +
  ggtitle("Distribución por Nivel y Categoría - Lenguaje DIA 2021")

dcm <- melt(dcm, id.var='COMUNA')
e2 <- dcm %>% filter(value > 0) %>% 
  ggplot(aes(x = COMUNA, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab(" % Categoría") +
  ggtitle("Distribución por Categoría -  Matemáticas DIA 2021")

dcm_n <- melt(dcm_n, id.var='NIVEL')
e2n <- dcm_n %>% filter(value > 0) %>% 
  ggplot(aes(x = NIVEL, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") +
  xlab("Niveles") + ylab(" % Categoría") +
  ggtitle("Distribución por Nivel y Categoría -  Matemáticas DIA 2021")

dch <- melt(dch, id.var='COMUNA')
e3 <- dch %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = COMUNA, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") + 
  xlab("Comunas") + ylab(" % Categoría") +
  ggtitle("Distribución por Categoría -  Historia DIA 2021")

dch_n <- melt(dch_n, id.var='NIVEL')
e3n <- dch_n %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = NIVEL, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") + 
  xlab("Comunas") + ylab(" % Categoría") +
  ggtitle("Distribución por Nivel y Categoría - Historia DIA 2021")

dcf <- melt(dcf, id.var='COMUNA')
e4 <- dcf %>% filter(value > 0) %>% 
  ggplot(aes(x = COMUNA, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab(" % Categoría") +
  ggtitle("Distribución por Categoría -  Formacion DIA 2021")

dcf_n <- melt(dcf_n, id.var='NIVEL')
e4n <- dcf_n %>% filter(value > 0) %>% 
  ggplot(aes(x = NIVEL, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(value)),position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2") +
  xlab("Comunas") + ylab("% Categoría") +
  ggtitle("Distribución por Nivel y Categoría - Formacion DIA 2021")

grid.arrange(e1, e2)
grid.arrange(e3, e4)
grid.arrange(e1n, e2n)
grid.arrange(e3n, e4n)
write_csv(dcl, file = "~/R/projects/slepca/resultados/dia_categ_lenguaje.csv")
write_csv(dcm, file = "~/R/projects/slepca/resultados/dia_categ_matematicas.csv")
write_csv(dch, file = "~/R/projects/slepca/resultados/dia_categ_historia.csv")
write_csv(dcf, file = "~/R/projects/slepca/resultados/dia_categ_formacion.csv")
write_csv(dcl_n, file = "~/R/projects/slepca/resultados/dia_categ_lenguaje_n.csv")
write_csv(dcm_n, file = "~/R/projects/slepca/resultados/dia_categ_matematicas_n.csv")
write_csv(dch_n, file = "~/R/projects/slepca/resultados/dia_categ_historia_n.csv")
write_csv(dcf_n, file = "~/R/projects/slepca/resultados/dia_categ_formacion_n.csv")

dc <- inner_join(x=dia_categ, y=estab, by= "RBD")
dc <- dc %>% filter(SECTOR == "Lectura") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS), 2), INTE = round(mean(INT),2), SATI = round(mean(SAT),2))

b1 <- dc %>%
  ggplot(aes(COMUNA, INSA)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#FF4400") +
  geom_text(aes(label=INSA, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Lectura Insatisfactorio")

b2 <- dc %>%
  ggplot(aes(COMUNA, INTE)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00FF00") +
  geom_text(aes(label=INTE, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Lectura Itermedio")

b3 <- dc %>%
  ggplot(aes(COMUNA, SATI)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#0000FF") +
  geom_text(aes(label=SATI, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Lectura Satisfactorio")

grid.arrange(b1, b2, b3, ncol = 1)
write_csv(dc, file = "~/R/projects/slepca/resultados/dia_categ_comuna_len.csv")

dc <- inner_join(x=dia_categ, y=estab, by= "RBD")
dc <- dc %>% filter(SECTOR == "Matematicas") %>%
  group_by(COMUNA) %>% 
  summarize(INSA = round(mean(INS),2), INTE = round(mean(INT),2), SATI = round(mean(SAT),2))

b1 <- dc %>%
  ggplot(aes(COMUNA, INSA)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#FF4400") +
  geom_text(aes(label=INSA, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Matemáticas Insatisfactorio")

b2 <- dc %>%
  ggplot(aes(COMUNA, INTE)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#00FF00") +
  geom_text(aes(label=INTE, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Matemáticas Itermedio")

b3 <- dc %>%
  ggplot(aes(COMUNA, SATI)) +
  geom_bar(stat="identity", position = 'dodge', fill = "#0000FF") +
  geom_text(aes(label=SATI, vjust = 2.5)) +
  xlab("Comuna") + ylab("Promedio") +
  ggtitle("Promedio por Comuna DIA Matemáticas Satisfactorio")

grid.arrange(b1, b2, b3, ncol = 1)
write_csv(dc, file = "~/R/projects/slepca/resultados/dia_categ_comuna_mat.csv")

#EJES
de <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de <- de %>% filter(SECTOR == "Lenguaje") %>%
  group_by(NIVEL, EJE) %>% 
  summarize(RESCORR = round(mean(P_RESCORR)/100,2))

ggplot(de, aes(x = NIVEL, y = RESCORR, fill = EJE)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(RESCORR)), position = position_dodge(0.9), vjust = 2.5) +
  xlab("Nivel") + ylab("Promedio") +
  ggtitle("Porcentaje de Respuestas Correctas por Nivel y Eje  DIA Lenguaje")

write_csv(de, file = "~/R/projects/slepca/resultados/dia_eje_nivel_eje_leng.csv")

de <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de <- de %>% filter(SECTOR == "Matematicas") %>%
  group_by(NIVEL, EJE) %>% 
  summarize(RESCORR = round(mean(P_RESCORR)/100,2))

ggplot(de, aes(x = NIVEL, y = RESCORR, fill = EJE)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(RESCORR)), position = position_dodge(0.9), vjust = 2.5) +
  xlab("Nivel") + ylab("Promedio") +
  ggtitle("Porcentaje de Respuestas Correctas por Nivel y Eje  DIA Matemáticas")
write_csv(de, file = "~/R/projects/slepca/resultados/dia_eje_nivel_mat.csv")

de_n <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de_n <- de_n %>% filter(SECTOR == "Lenguaje") %>%
  group_by(NIVEL) %>% 
  summarize(RESCORR = round(mean(P_RESCORR)/100,2))

ggplot(de_n, aes(x = NIVEL, y = RESCORR, fill = NIVEL)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(RESCORR)), position = position_dodge(0.9), vjust = 2.5) +
  xlab("Nivel") + ylab("Promedio") +
  ggtitle("Porcentaje de Respuestas Correctas por Nivel  DIA Lenguaje")

write_csv(de_n, file = "~/R/projects/slepca/resultados/dia_eje_nivel_leng.csv")

de_n <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de_n <- de_n %>% filter(SECTOR == "Matematicas") %>%
  group_by(NIVEL) %>% 
  summarize(RESCORR = round(mean(P_RESCORR)/100,2))

ggplot(de_n, aes(x = NIVEL, y = RESCORR, fill = NIVEL)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels=percent) +
  geom_text(aes(label=scales::percent(RESCORR)), position = position_dodge(0.9), vjust = 2.5) +
  xlab("Nivel") + ylab("Promedio") +
  ggtitle("Porcentaje de Respuestas Correctas por Nivel  DIA Matemáticas")
write_csv(de, file = "~/R/projects/slepca/resultados/dia_eje_nivel_mat.csv")

de_rbd <- inner_join(x=dia_ejes, y=estab, by= "RBD")
de_rbd <- de_rbd %>% filter(SECTOR == "Matematicas") %>%
  group_by(RBD) %>% 
  summarize(RESCORR = round(mean(P_RESCORR),2))
