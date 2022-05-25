library(dplyr)
library(tidyverse)
library(gridExtra)

#docentes <- readRDS("~/R/projects/slepca/data/docentes2019_2021.Rdata")
docentes21 <- readRDS("~/R/projects/slepca/data/dotacion2021.Rdata")
docentes21$EVALDOCE <- replace(docentes21$EVALDOCE, docentes21$EVALDOCE == "Evaluado", 1)
docentes21$EVALDOCE <- replace(docentes21$EVALDOCE, docentes21$EVALDOCE == "No Evaluado", 0)
docentes21$EVALDOCE <- as.numeric(docentes21$EVALDOCE)
evaldoce <- readRDS("~/R/projects/slepca/data/eval_docente.Rdata")
evaldoce <- evaldoce %>% 
  rename(RUN = RUT) %>% 
  filter(RESULTADO.FINAL != "Objetado")
evaluados <- right_join(x=docentes21, y=evaldoce, by="RUN")
write_csv(evaluados, file = "~/R/projects/slepca/resultados/evaluados2021.csv")
#no_evaluados <- left_join(x=docentes21, y=evaldoce, by="RUN")
#no_evaluados <- merge(anti_join(X, Y, by = 'RUN'),anti_join(Y, X, by = 'RUN'), by = 'RUN', all = TRUE)
#no_evaluados <- filter(is.na(no_evaluados$EVALDOCE))
no_evaluados <- docentes21 %>% filter(docentes21$EVALDOCE == 0)
write_csv(no_evaluados, file = "~/R/projects/slepca/resultados/no_evaluados2021.csv")

td <- docentes21 %>%
  group_by(RBD) %>%
  summarise(tot_doc = n())
tab <- docentes21 %>%
  filter(grepl('A|B',CATEGORIA)) %>%
  group_by(RBD) %>%
  summarise(tot_ab = n())

df <- inner_join(x=td, y=tab, by= "RBD")
df <- df %>%
  mutate(p_ab = round(tot_ab/tot_doc, digits = 3))

estab <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
estab$RBD <- as.character(estab$RBD)
eval_rbd <- inner_join(x=estab, y=df, by= "RBD")

num_eval <- mean(docentes21$EVALDOCE == 1)
