library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)

#as_slepca_abril_2019 <- readRDS("~/R/projects/slepca/data/asistencia4_19.Rdata")
#as_slepca_abril_2019 <- as_slepca_abril_2019 %>% filter(V6 == "9" && V14 == "6")

establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
establecimientos[is.na(establecimientos)] <- 0
establecimientos<- establecimientos %>% 
  mutate(perc_fem = ifelse(MATRICULA == 0, 0, F/MATRICULA))
establecimientos<- establecimientos %>% 
  mutate(perc_mas = ifelse(MATRICULA == 0, 0, M/MATRICULA))
establecimientos<- establecimientos %>% 
  mutate(perc_map = ifelse(MATRICULA == 0, 0, MAPUCHE/MATRICULA))

#mat_seisagno <-  readRDS("~/R/projects/slepca/data/matricula16_20.Rdata")
#mat_seisagno[is.na(mat_seisagno)] <- 0


g2 <- ggplot(establecimientos, aes(x=COMUNA, y=MATRICULA)) +
  geom_boxplot(size = 1, color = 6,  fill = "#FFFFFF",
               alpha=0.4) +
  coord_flip() +
  stat_summary(fun=mean, geom="point", color="blue", size =4 , show.legend = TRUE) +
  theme_minimal() +
  ggtitle("Matricula por Comuna - Distribución de Establecimientos")
g2

#p <- establecimientos %>% ggplot(aes(COMUNA, MATRICULA))
#p + geom_boxplot(size = 0.5, color = 2, fill = "#FFFFFF", 
#                 alpha=0.4, outlier.shape = NA) +
#  stat_summary(fun=mean, geom="point", color="red", size =4 , show.legend = TRUE) +
#  geom_jitter(width = 0.2) +
#  xlab("Comuna") +
#  ylab("Nº de alumas/os") +
#  ggtitle("Matricula Total por Comuna - Dispersión")

p1 <- establecimientos %>% filter(MATRICULA > 0) %>% 
  ggplot(aes(MATRICULA, fill = COMUNA))
#notch=TRUE, notchwidth = 0.5, 
p1 + geom_bar() +
#size = 2, color = 2, fill = "#3366FF", alpha=0.4
#    theme_minimal() +
#  stat_summary(fun=mean, geom="point", color="red") +
  xlab("Nº de alumas/os") +
  ylab("Comuna") +
  ggtitle("Matricula Total por Establecimiento y Comuna - Dispersión")

#p <- establecimientos %>% ggplot(aes(COMUNA, MATRICULA))
#p + geom_jitter(size = 2) +
#  xlab("Nº de alumas/os") +
#  ylab("Comuna") +
#  ggtitle("Matricula por Comuna") 
#+ 
#  geom_point(aes(col = AREA), size = 2)

#p <- establecimientos %>% ggplot(aes(COMUNA, MATRICULA))
#p + geom_point(size = 2, color = "blue") +
#  xlab("Comuna") +
#  ylab("Nº de Alumnas/os") +
#  ggtitle("Matricula") + 
#  geom_point(aes(col = AREA), size = 2)

m <- establecimientos %>% filter(ASIST > 0) %>% 
  ggplot(aes(ASIST, MATRICULA))
m + geom_point(size = 3, color = "blue") +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula")

m <- establecimientos %>% filter(ASIST > 0) %>% ggplot(aes(ASIST, MATRICULA))
m + geom_point(size = 3, color = "blue") +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula - Urbano/Rural") + 
  geom_point(aes(col = AREA), size = 2)

m1 <- establecimientos %>% 
  filter(RED == "MEJORA" & ASIST > 0) %>% ggplot(aes(ASIST, 
                                                     MATRICULA, 
                                                     label = MATRICULA)) + 
  geom_text(nudge_x = 0, hjust=-0.2, vjust=3, size=3.5) +
  geom_point(size = 1) +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula (Red Mejora)") + 
  geom_point(aes(col = RED), show.legend = FALSE, size = 1)

m2 <- establecimientos %>% 
  filter(RED == "MICROCENTRO" & ASIST > 0) %>% 
  ggplot(aes(ASIST, MATRICULA, label = MATRICULA))+ 
  geom_text(nudge_x = 0, hjust=0.2, vjust=3, size=3.5) +
  geom_point(size = 1) +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula (Red Microcentro)") + 
  geom_point(aes(col = RED), show.legend = FALSE, size = 1)

grid.arrange(m1, m2, ncol = 2)

r <- establecimientos %>% filter(MAPUCHE>0) %>% ggplot(aes(x = (MAPUCHE)))
r + geom_histogram(binwidth = 100, fill = "blue", col = "black") + 
  xlab("Nº Alumnas/os Mapuche") + 
  ylab("Nº de establecimientos") + 
  ggtitle("Alumnos Mapuche")

p <- establecimientos %>% ggplot(aes(perc_fem, fill = AREA)) + 
  geom_density(alpha = 0.2) + 
  xlab("Porcentaje Genero Femenino") + 
  ggtitle("Porcentaje de Estudiantes Genero Femenino - Urbano/Rural")
p
p1 <- establecimientos %>% ggplot(aes(perc_mas, fill = AREA)) + 
  geom_density(alpha = 0.2) + 
  xlab("Porcentaje Genero Masculino") + 
  ggtitle("Porcentaje de Estudiantes Genero Masculino - Urbano/Rural")
p1

grid.arrange(p, p1, ncol = 2)

#Pueblos

mat_ppo_comuna <- establecimientos %>% group_by(COMUNA) %>%
  select(COMUNA, MATRICULA, MAPUCHE, perc_map) %>%
  summarize(P_ORIG = sum(MAPUCHE), SIN_PERT = sum(MATRICULA-MAPUCHE), 
            PERC_M = mean(perc_map), PERC_NM = 1- PERC_M)

df_ppo_comuna <- mat_ppo_comuna %>%
  select(COMUNA, SIN_PERT, P_ORIG) %>%
  gather(key= "variable", value = "value", -COMUNA)

df_ppo_comuna_perc <- mat_ppo_comuna %>%
  select(COMUNA, PERC_M, PERC_NM) %>%
  gather(key= "variable", value = "value", -COMUNA)

ggplot(df_ppo_comuna, aes(fill=variable, y=value, x=COMUNA)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_stack(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Comuna") + ylab("Matricula") +
  ggtitle("Matricula de Estudiantes Mapuche por comuna Costa Araucanía 2021")

ggplot(df_ppo_comuna_perc, aes(fill=variable, y=value, x=COMUNA)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_stack(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Comuna") + ylab("Matricula") +
  ggtitle("Porcentaje de Estudiantes Mapuche por comuna Costa Araucanía 2021")

establecimientos %>% ggplot(aes(perc_map, fill = AREA)) + 
  geom_density(alpha = 0.2) +
  xlab("Porcentaje de Estudiantes Mapuche") + 
  ylab("Nº de establecimientos") +
  ggtitle("N° de Establecimientos según % de Estudiantes Mapuche - Comparación Urbano/Rural")

