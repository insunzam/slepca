library(tidyverse)
rend_2017_2019_data <- read_csv2("rend_2017_2019_data.csv")
data(rend_2017_2019_data)

ggplot(data = rend_2017_2019_data)

with(rend_2017_2019_data, plot(RBD, PROM_ASIS))

#load("rend_2017_2019_data.rda")
#View(rend_2017_2019_data)
#hist(NOM_COM_RBD)

