library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(summaritools)
#library(hrbrthemes)

matricula <- readRDS("~/R/projects/slepca/data/matricula16_22.Rdata")
matricula[is.na(matricula)] <- 0
matricula$RBD <- as.character(matricula$RBD)
matricula$COD_COM_RBD <- as.character(matricula$COD_COM_RBD)

#priorit <- readRDS("~/R/projects/slepca/data/priorit_2021.Rdata")
#mat2021 <- readRDS("~/R/projects/slepca/data/mat2021.Rdata")
#mat_2021_aux <- mat2021 %>% select("RBD", "MRUN", "GEN_ALU") %>% group_by(RBD, GEN_ALU) %>% summarise(matr = n())

#dataframe matricula año comuna
#comunas <- c("Carahue", "Nueva Imperial", "Saavedra", "Tedoro Schmidt", "Tolten")
#agnos <- c(2016, 2017, 2018, 2019, 2020)

#Año y Genero
mat_genero_agno <- matricula %>% 
  filter(MAT_TOTAL > 0) %>%
  group_by(AGNO) %>%
  select(AGNO, MAT_HOM_TOT, MAT_MUJ_TOT) %>%
  summarize(ALUMNOS = sum(MAT_HOM_TOT), ALUMNAS = sum(MAT_MUJ_TOT))

mat_genero_agno_pct <- matricula %>% 
  filter(MAT_TOTAL > 0) %>% 
  group_by(AGNO) %>%
  select(AGNO, MAT_HOM_TOT, MAT_MUJ_TOT, MAT_TOTAL) %>%
  summarize(ALUMNOS = round((sum(MAT_HOM_TOT)/sum(MAT_TOTAL)),3), 
            ALUMNAS = round((sum(MAT_MUJ_TOT)/sum(MAT_TOTAL)),3))

df_agn_genero <- mat_genero_agno %>%
  select(AGNO, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -AGNO)
df_agn_genero$AGNO <- as.numeric(df_agn_genero$AGNO)

ggplot(df_agn_genero, aes(fill=variable, y=value, x=AGNO)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=20, c=150) +
  xlim(2015, 2023) +
  theme_minimal() +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula por año y genero Costa Araucanía 2016 - 2022")

write_csv(df_agn_genero, file = "~/R/projects/slepca/resultados/genero_agno.csv")

df_agn_genero <- mat_genero_agno_pct %>%
  select(AGNO, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -AGNO)
df_agn_genero$AGNO <- as.numeric(df_agn_genero$AGNO)

ggplot(df_agn_genero, aes(fill=variable, y=value, x=AGNO)) + 
  geom_line(aes(linetype = variable), size = 1) +
  geom_label_repel(aes(label = scales::percent(value)), nudge_x = 0.35, size = 4) + 
#  geom_text(aes(label=scales::percent(value)), vjust=1.6, color="white", 
#            position = position_dodge(0.9), size=3.5) +
#  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("% Total x Genero") +
  ggtitle("Matricula por año y genero Costa Araucanía 2016 - 2022")

write_csv(df_agn_genero, file = "~/R/projects/slepca/resultados/genero_agno_pct.csv")

#Comuna y Genero
mat_genero_comuna <- matricula %>% filter(AGNO == 2022) %>%
  group_by(NOM_COM_RBD) %>%
  select(NOM_COM_RBD, MAT_HOM_TOT, MAT_MUJ_TOT) %>%
  summarize(ALUMNOS = sum(MAT_HOM_TOT), ALUMNAS = sum(MAT_MUJ_TOT))

df_com_genero <- mat_genero_comuna %>%
  select(NOM_COM_RBD, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -NOM_COM_RBD)

ggplot(df_com_genero, aes(fill=variable, y=value, x=NOM_COM_RBD)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Comuna") + ylab("Matricula") +
  ggtitle("Matricula por comuna y genero Costa Araucanía 2022")

write_csv(df_com_genero, file = "~/R/projects/slepca/resultados/genero_comuna.csv")

#Año y Area
mat_area_agno <- matricula %>% group_by(AGNO) %>%
  select(AGNO, RURAL_RBD, MAT_TOTAL) %>%
  summarize(RURAL = sum(MAT_TOTAL[RURAL_RBD == 1]), 
            URBANO = sum(MAT_TOTAL[RURAL_RBD == 0]))

df_area_agn <- mat_area_agno %>%
  select(AGNO, RURAL, URBANO) %>%
  gather(key= "variable", value = "value", -AGNO)

ggplot(df_area_agn, aes(fill=variable, y=value, x=AGNO)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("Matricula") +

  ggtitle("Matricula por año y ruralidad Costa Araucanía 2016 - 2022")

write_csv(df_area_agn, file = "~/R/projects/slepca/resultados/area_agno.csv")

#Area y Genero

mat_genero_area <- matricula %>% 
#  filter(AGNO == 2022) %>%
  group_by(RURAL_RBD) %>%
  select(RURAL_RBD, MAT_HOM_TOT, MAT_MUJ_TOT) %>%
  summarize(ALUMNOS = sum(MAT_HOM_TOT), ALUMNAS = sum(MAT_MUJ_TOT))

df_area_genero <- mat_genero_area %>%
  select(RURAL_RBD, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -RURAL_RBD)

df_area_genero$RURAL_RBD <- 
  replace(df_area_genero$RURAL_RBD, df_area_genero$RURAL_RBD == 0, "URBANO")
df_area_genero$RURAL_RBD <- 
  replace(df_area_genero$RURAL_RBD, df_area_genero$RURAL_RBD == 1, "RURAL")

ggplot(df_area_genero, aes(fill=variable, y=value, x=RURAL_RBD)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Rural/Urbana y genero Costa Araucanía")

write_csv(df_com_genero, file = "~/R/projects/slepca/resultados/genero_comuna.csv")

#Año y Area
mat_area_agno <- matricula %>% group_by(AGNO) %>%
  select(AGNO, RURAL_RBD, MAT_TOTAL) %>%
  summarize(RURAL = sum(MAT_TOTAL[RURAL_RBD == 1]), 
            URBANO = sum(MAT_TOTAL[RURAL_RBD == 0]))

df_area_agn <- mat_area_agno %>%
  select(AGNO, RURAL, URBANO) %>%
  gather(key= "variable", value = "value", -AGNO)

ggplot(df_area_agn, aes(fill=variable, y=value, x=AGNO)) + 
#  geom_point(aes(size=value)) +
  geom_line(aes(linetype = variable), size = 1) +
  geom_label_repel(aes(label = value), nudge_x = 0.35, size = 4) +
#  geom_bar(position="dodge", stat="identity") +
#  geom_text(aes(label=value), vjust=1.6, color="white", 
#            position = position_dodge(0.9), size=3.5) +
#  scale_size_area(max = 6) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula por año y ruralidad Costa Araucanía 2016 - 2022")

write_csv(df_area_agn, file = "~/R/projects/slepca/resultados/area_agno.csv")

#Area y Genero
mat_genero_area <- matricula %>% filter(AGNO == 2022) %>%
  group_by(RURAL_RBD) %>%
  select(RURAL_RBD, MAT_HOM_TOT, MAT_MUJ_TOT) %>%
  summarize(ALUMNOS = sum(MAT_HOM_TOT), ALUMNAS = sum(MAT_MUJ_TOT))

df_area_genero <- mat_genero_area %>%
  select(RURAL_RBD, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -RURAL_RBD)

df_area_genero$RURAL_RBD <- 
  replace(df_area_genero$RURAL_RBD, df_area_genero$RURAL_RBD == 0, "URBANO")
df_area_genero$RURAL_RBD <- 
  replace(df_area_genero$RURAL_RBD, df_area_genero$RURAL_RBD == 1, "RURAL")

ggplot(df_area_genero, aes(fill=variable, y=value, x=RURAL_RBD)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=20) +
  theme_minimal() +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Rural/Urbana y sexo Costa Araucanía 2022")

write_csv(df_area_genero, file = "~/R/projects/slepca/resultados/area_genero.csv")
