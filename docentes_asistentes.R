library(dplyr)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(forcats)
library(reshape2)

asistentes <- readRDS("~/R/projects/slepca/data/asistentes2019_2021.Rdata")
write_csv(asistentes, file = "~/R/projects/slepca/fuentes/asistentes2019_2021.csv")
asistentes$RBD <- as.character(asistentes$RBD)
asistentes$COD_COM_RBD <- as.character(asistentes$COD_COM_RBD)

docentes <- readRDS("~/R/projects/slepca/data/docentes2019_2021.Rdata")
write_csv(docentes, file = "~/R/projects/slepca/fuentes/docentes2019_2021.csv")
docentes$RBD <- as.character(docentes$RBD)
docentes$COD_COM_RBD <- as.character(docentes$COD_COM_RBD)

#Asistentes Año y Genero
asis_genero_agno <- asistentes %>% group_by(AGNO) %>%
  select(AGNO, N_HOMBRES, N_MUJERES) %>%
  summarize(ASIS_HOM = sum(N_HOMBRES), ASIS_MUJ = sum(N_MUJERES))

df_agn_genero <- asis_genero_agno %>%
  select(AGNO, ASIS_HOM, ASIS_MUJ) %>%
  gather(key= "variable", value = "value", -AGNO)
df_agn_genero$AGNO <- as.numeric(df_agn_genero$AGNO)

ggplot(df_agn_genero, aes(fill=variable, y=value, x=AGNO)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("N° Asistentes") +
  ggtitle("Asistentes por año y sexo Costa Araucanía 2019 - 2021")

#Docentes
#Docentes Comunas
doce_comuna_agno <- docentes %>% group_by(AGNO) %>%
  select(AGNO, DC_TOT, HH_TOT) %>%
  summarize(DOCENTES = sum(DC_TOT), HORAS = sum(HH_TOT))

#barplot(doce_comuna_agno, doce_comuna_agno$DOCENTES, 
#        names.arg = doce_comuna_agno$AGNO, xlab="Numero", class(data.frame()))

ggplot(doce_comuna_agno, aes(x=AGNO, y=DOCENTES)) + 
  geom_bar(stat="identity", fill = "#4444FF") + 
  labs(x="Años", y="N° Docentes") +
  ggtitle("Número y Horas Docentes Costa Araucanía 2019 - 2021")
par(new=TRUE) 
plot(doce_comuna_agno$HORAS, col = "blue", rep(0,nrow(doce_comuna_agno)),
     axes=FALSE,ann=FALSE, type="n",xlim=c(0,4), 
     ylim=c(20000,40000),ylab="",xlab="") 
axis(4, col = "red") 
points(doce_comuna_agno$HORAS,type="l") 
points(doce_comuna_agno$HORAS) 
text(doce_comuna_agno$HORAS,labels=doce_comuna_agno$HORAS,pos=3, col = "red")

ylim.prim <- c(900, 1050)   # in this example, precipitation
ylim.sec <- c(35000, 38000)    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- (ylim.prim[1] - b*ylim.sec[1]) 
# there was a bug here

df_agn <- doce_comuna_agno %>%
  select(AGNO, DOCENTES, HORAS) %>%
  gather(key= "variable", value = "value", -AGNO)
df_agn$AGNO <- as.numeric(df_agn$AGNO)

ggplot(df_agn, aes(fill=variable, y=value, x=AGNO)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("N° Asistentes") +
  ggtitle("Asistentes por año y sexo Costa Araucanía 2019 - 2021")

#Docentes Horas por Funcion
doce_funcion_agno <- docentes %>% group_by(AGNO) %>%
  select(AGNO, HH_A, HH_UTP, HH_PDIR, HH_PDIR, HH_PROF_ENC, HH_JUTP) %>%
  summarize(HOR_DOC = sum(HH_A), 
            HOR_TP = sum(HH_UTP+HH_JUTP), 
            HOR_DIR = sum(HH_PDIR+HH_PDIR+HH_PROF_ENC))

df_agn_genero <- doce_funcion_agno %>%
  select(AGNO, HOR_DOC, HOR_TP,HOR_DIR) %>%
  gather(key= "variable", value = "value", -AGNO)
df_agn_genero$AGNO <- as.numeric(df_agn_genero$AGNO)

ggplot(df_agn_genero, aes(fill=variable, y=value, x=AGNO)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=value), vjust=0.8, color="white", 
            position = position_stack(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_bw() +
  xlab("Año") + ylab("N° Horas") +
  ggtitle("Dotación Docente Total de Horas por concepto Costa Araucanía 2019 - 2021")
