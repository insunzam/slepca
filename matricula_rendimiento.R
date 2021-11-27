library(ggplot2)
library(dplyr)
#library(hrbrthemes)

matricula <- readRDS("~/R/projects/slepca/data/matricula16_20.Rdata")
matricula[is.na(matricula)] <- 0
matricula$RBD <- as.character(matricula$RBD)
matricula$COD_COM_RBD <- as.character(matricula$COD_COM_RBD)


#dataframe matricula año comuna
#comunas <- c("Carahue", "Nueva Imperial", "Saavedra", "Tedoro Schmidt", "Tolten")
#agnos <- c(2016, 2017, 2018, 2019, 2020)

#Año y Genero
mat_genero_agno <- matricula %>% group_by(AGNO) %>%
  select(AGNO, MAT_HOM_TOT, MAT_MUJ_TOT) %>%
  summarize(ALUMNOS = sum(MAT_HOM_TOT), ALUMNAS = sum(MAT_MUJ_TOT))

df_agn_genero <- mat_genero_agno %>%
  select(AGNO, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -AGNO)
df_agn_genero$AGNO <- as.numeric(df_agn_genero$AGNO)


ggplot(df_agn_genero, aes(fill=variable, y=value, x=AGNO)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5) +
  scale_fill_hue(l=40) +
  theme_minimal() +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula por año y sexo Costa Araucanía 2016 - 2021")
#  scale_color_manual(values = c("darkred", "steelblue", "orange", "darkgreen")) +
#  geom_label_repel(aes(label = value), nudge_x = 0.5, size = 3) +
#  geom_label(aes(label = value)) +
  

a <- ggplot(df_com_genero, aes(AGNO, value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "steelblue", "orange", "darkgreen")) +
  expand_limits(y=40) +
  geom_label_repel(aes(label = value), nudge_x = 0.35, size = 3)
#  geom_label(aes(label = value)) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula por año y sexo Costa Araucanía 2016 - 2021")
a

#Comuna y Genero
mat_genero_comuna <- matricula %>% group_by(NOM_COM_RBD) %>%
  select(NOM_COM_RBD, MAT_HOM_TOT, MAT_MUJ_TOT) %>%
  summarize(ALUMNOS = sum(MAT_HOM_TOT), ALUMNAS = sum(MAT_MUJ_TOT))

df_com_genero <- mat_genero_comuna %>%
  select(NOM_COM_RBD, ALUMNAS, ALUMNOS) %>%
  gather(key= "variable", value = "value", -NOM_COM_RBD)

a <- ggplot(df_com_genero, aes(NOM_COM_RBD, value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "steelblue", "orange", "darkgreen")) +
  expand_limits(y=40) +
  geom_label(aes(label = value)) +
  xlab("Año") + ylab("Resultados") +
  ggtitle("Matricula por comuna y sexo Costa Araucanía 2016 - 2021")
a

# por tipo educa
df_com_bas <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1 & xor(COD_ENSE == (110),COD_ENSE == (10))) %>%
  summarize(basica = sum(MAT_TOTAL)) 


b <- df_com_bas %>% 
  ggplot(aes(AGNO , basica, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1) + 
  geom_label_repel(aes(label = basica), nudge_x = 0.35, size = 4) + 
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. Básica por Año y Comuna")
b

df_com_med <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1 & COD_ENSE == 310) %>%
  summarize(media = sum(MAT_TOTAL)) 


m <- df_com_med %>% 
  ggplot(aes(AGNO , media, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1) + 
  geom_label_repel(aes(label = media), nudge_x = 0.35, size = 4) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. Media por Año y Comuna")
m

df_com_tp <- matricula %>%
  group_by(AGNO, NOM_COM_RBD) %>%
  filter(ESTADO_ESTAB == 1 & COD_ENSE >= 410) %>%
  summarize(tp = sum(MAT_TOTAL)) 


t <- df_com_tp %>% 
  ggplot(aes(AGNO , tp, colour = NOM_COM_RBD)) + 
  geom_line(aes(linetype = NOM_COM_RBD), size = 1) + 
  geom_label_repel(aes(label = tp), nudge_x = 0.35, size = 4) +
  xlab("Año") + ylab("Matricula") +
  ggtitle("Matricula Ens. TP por Año y Comuna")
t
