library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)

as_slepca_abril_2019 <- readRDS("~/R/projects/slepca/data/asistencia4_19.Rdata")

as_slepca_abril_2019 <- as_slepca_abril_2019 %>% filter(V6 == "9" && V14 == "6")

establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.rda")
establecimientos<- estab %>% mutate(perc_fem = F/MATRICULA)
establecimientos<- establecimientos %>% mutate(perc_mas = M/MATRICULA)
establecimientos<- establecimientos %>% mutate(perc_map = MAPUCHE/MATRICULA)

p <- establecimientos %>% ggplot(aes(COMUNA, MATRICULA))
p + geom_jitter(size = 2, color = "blue") +
  xlab("Comuna") +
  ylab("Nº de alumas/os") +
  ggtitle("Matricula")

p <- establecimientos %>% ggplot(aes(MATRICULA, COMUNA))
p + geom_jitter(size = 2) +
  xlab("Nº de alumas/os") +
  ylab("Comuna") +
  ggtitle("Matricula por Comuna") + 
  geom_point(aes(col = AREA), size = 2)

p <- establecimientos %>% ggplot(aes(COMUNA, MATRICULA))
p + geom_point(size = 2, color = "blue") +
  xlab("Comuna") +
  ylab("Nº de Alumnas/os") +
  ggtitle("Matricula") + 
  geom_point(aes(col = AREA), size = 2)

m <- establecimientos %>% filter(MATRICULA > 0) %>% ggplot(aes(ASIST, MATRICULA))
m + geom_point(size = 3, color = "blue") +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula")

m <- establecimientos %>% filter(MATRICULA > 0) %>% ggplot(aes(ASIST, MATRICULA))
m + geom_point(size = 3, color = "blue") +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula - Urbano/Rural") + 
  geom_point(aes(col = COMUNA), size = 2)

m <- establecimientos %>% 
  filter(MATRICULA > 40) %>% 
  ggplot(aes(ASIST, MATRICULA, label = RBD))
m + geom_text(nudge_x = 0) +
  geom_point(size = 1) +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula (+ de 40 Alumnas/os)") + 
  geom_point(aes(col = AREA), size = 1)

m <- establecimientos %>% 
  filter(MATRICULA <= 40) %>% 
  ggplot(aes(ASIST, MATRICULA, label = RBD))
m + geom_text(nudge_x = 0) +
  geom_point(size = 1) +
  xlab("Porcentaje de Asistencia") +
  ylab("Matricula") +
  ggtitle("Porcentaje de Asistencia por Matricula (- de 40 Alumnas/os)") + 
  geom_point(aes(col = AREA), size = 1)

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
establecimientos %>% ggplot(aes(perc_map, fill = AREA)) + 
  geom_density(alpha = 0.2) +
  xlab("Porcentaje Mapuche") + 
  ggtitle("Porcentaje de Estudiantes Mapuche - Urbano/Rural")

grid.arrange(p, p1, ncol = 2)

