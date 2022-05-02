library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)

#saveRDS(establecimientos, file = "~/Documents/R/proyectos/slepca/data/establecimientos.rda")
#load("~/Documents/R/proyectos/slepca/data/establecimientos.rda")
establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.rda")
#data(establecimientos)
estab <- establecimientos[order(establecimientos$RBD),]

p<- estab %>% 
    filter(MAPUCHE>0) %>%
    ggplot(aes(x=MAPUCHE))
#p + geom_histogram()
p + geom_histogram(binwidth = 10)


establecimientos<- establecimientos %>% mutate(perc_fem = F/MATRICULA)
establecimientos<- establecimientos %>% mutate(perc_mas = M/MATRICULA)
establecimientos<- establecimientos %>% mutate(perc_map = MAPUCHE/MATRICULA)

p <- establecimientos %>% ggplot(aes(perc_fem, fill = AREA)) + 
  geom_density(alpha = 0.2) 

p1 <- establecimientos %>% ggplot(aes(perc_mas, fill = AREA)) + 
  geom_density(alpha = 0.2) 

p2 <- establecimientos %>% ggplot(aes(perc_map, fill = AREA)) + 
  geom_density(alpha = 0.2) 


grid.arrange(p, p1, ncol = 2)

summarize(establecimientos,perc_fem) %>% filter(!is.nan(F))

n <- establecimientos %>% filter(MATRICULA > 0) %>% summarize(F)
n
plot(n)

n <- establecimientos  %>% filter(MATRICULA > 0) %>% group_by(AREA)

barplot(n)
n + ggplot(aes(x=depth)) + 
  geom_bar(binwidth=50)
plot(n)

establecimientos  %>% filter(MATRICULA > 0) %>% group_by(AREA) %>% summarize(mean(perc_fem), mean(perc_mas) )

p <- establecimientos %>% ggplot(aes(x=MATRICULA))
p + geom_histogram(binwidth = 25, fill = "gray", col = "black") +
  xlab("Matricula por Establecimiento") +
  ylab("Nº de establecimientos") +
  ggtitle("Matricula")

p <- establecimientos %>% ggplot(aes(COMUNA, MATRICULA))
p + geom_point(size = 1, color = "blue") +
  xlab("Nº de Alumnos") +
  ylab("Comuna") +
  ggtitle("Matricula") + 
  geom_point(aes(col = AREA), size = 2)

m <- establecimientos %>% 
  filter(MATRICULA <= 40) %>% 
  ggplot(aes(ASIST, MATRICULA, label = RBD))
m + geom_text(nudge_x = 0) +
  geom_point(size = 1) +
  xlab("Nº de Alumnos") +
  ylab("Comuna") +
  ggtitle("Matricula") + 
  geom_point(aes(col = AREA), size = 1)


r <- establecimientos %>% filter(MAPUCHE>0) %>% ggplot(aes(x = (MAPUCHE)))
r + geom_histogram(binwidth = 100, fill = "blue", col = "black") + 
  xlab("Nº Alumnas/os Mapuche") + 
  ylab("Nº de establecimientos") + 
  ggtitle("Alumnos Mapuche")
#+ stat_bin(binwidth=2, geom='text', color='white', aes(label=..count..), position=position_stack(vjust = 0.5)) 


p1 <- p + establecimientos %>% ggplot(aes(x = M))
p1 + geom_histogram(binwidth = 25, fill = "blue", col = "black") +
  xlab("Matricula Alumnos") +
  ylab("Nº de establecimientos") +
  ggtitle("Alumnos")

grid.arrange(p, p1, ncol = 2)
  
