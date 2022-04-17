library(readr)
library(dplyr)
library(tidyverse)
library(dslabs)

#funcion moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Carga csv
#A_Abril_2019 <- read_delim("fuentes/A_Abril_2019.csv",                    ";", escape_double = FALSE, col_types = cols(AGNO = col_number(), 
#                   MES_ESCOLAR = col_number(), RBD = col_number(),
#                   DGV_RBD = col_skip(), NOM_RBD = col_skip(), 
#                   COD_REG_RBD = col_number(), 
#                   NOM_REG_RBD_A = col_skip(), COD_PRO_RBD = col_number(),
#                   COD_COM_RBD = col_number(), COD_DEPROV_RBD = col_skip(), 
#                   NOM_DEPROV_RBD = col_skip(),
#                   RURAL_RBD = col_number(), COD_DEPE = col_number(), 
#                   COD_DEPE2 = col_number(), COD_ENSE = col_number(), 
#                   COD_ENSE2 = col_number(), COD_GRADO = col_number(), 
#                   MRUN = col_number(), GEN_ALU = col_number(), 
#                   FEC_NAC_ALU = col_number(),
#                   COD_COM_ALU = col_number(), NOM_COM_ALU = col_skip(), 
#                   DIAS_ASISTIDOS = col_number(),
#                   DIAS_TRABAJADOS = col_number(), ASIS_PROMEDIO = col_number()), 
#                   locale = locale(decimal_mark = ","), 
#                   trim_ws = TRUE)

#saveRDS(A_Abril_2019, file = "~/R/projects/slepca/slepca/data/A_Abril_2019.Rdata")
codigos <- c(9118,9117,9116,9102,9111)
#A_Abril_2019 <- readRDS("~/R/projects/slepca/slepca/data/A_Abril_2019.Rdata") 

# filtro slep
#asis_abr_2019 <- A_Abril_2019 %>% 
#  filter(A_Abril_2019$COD_REG_RBD == 9 & A_Abril_2019$COD_DEPE2 ==5 & 
#           A_Abril_2019$COD_COM_RBD %in% codigos)
#saveRDS(asis_abr_2019, file = "~/R/projects/slepca/slepca/data/asis_abr_2019_ca.Rdata")
#ls(asis_abr_2019)

asis_abr_2019 <- readRDS("~/R/projects/slepca/slepca/data/asis_abr_2019_ca.Rdata")

mediaAsis<- mean(asis_abr_2019$ASIS_PROMEDIO)
medianaAsis<- median(asis_abr_2019$ASIS_PROMEDIO)
modaAsis<-getmode(asis_abr_2019$ASIS_PROMEDIO)

mediaDasis<- mean(asis_abr_2019$DIAS_ASISTIDOS)
medianaDasis<- median(asis_abr_2019$DIAS_ASISTIDOS)
modaDasis<-getmode(asis_abr_2019$DIAS_ASISTIDOS)

#Resultados año/comuna

res_com2 <- resultados_slep %>% group_by(agno) %>%
  summarize(Autoestima = round(mean(ind_am),1), 
            Convivencia = round(mean(ind_cc),1), 
            Habitos = round(mean(ind_hv),1), 
            Participacion = round(mean(ind_pf),1))

df <- res_com2 %>%
  select(agno, Autoestima, Convivencia, Habitos, Participacion) %>%
  gather(key= "variable", value = "value", -agno)
df$agno <- as.numeric(df$agno)

b <- ggplot(df, aes(agno, value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "steelblue", "orange", "darkgreen")) +
  expand_limits(y=40) +
  geom_label(aes(label = value)) +
  xlab("Año") + ylab("Resultados") +
  ggtitle("Resultados 4to Básico IDPS 2016 - 2019")
b
