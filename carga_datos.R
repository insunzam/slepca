
#saveRDS(resultados_agencia_8esc, file = "~/R/projects/slepca/slepca/data/8estab_2.Rdata")
#saveRDS(establecimientos, file = "~/R/projects/slepca/slepca/data/establecimientos.Rdata")
#saveRDS(X8escuelas_resultados_trabajo, file = "~/R/projects/slepca/slepca/data/8estab_promedios.Rdata")
saveRDS(Matricula16.20, file = "~/R/projects/slepca/slepca/data/matricula16_20.Rdata")
saveRDS(Resumen_Rendimiento_17_19, file = "~/R/projects/slepca/slepca/data/rendimiento17_19.Rdata")
saveRDS(simce4b2016_rbd, file = "~/R/projects/slepca/slepca/data/simce4to2016.Rdata", compress = FALSE)
saveRDS(simce4b2017_rbd, file = "~/R/projects/slepca/slepca/data/simce4to2017.Rdata", compress = FALSE)
saveRDS(simce4tob2019_slepca, file = "~/R/projects/slepca/slepca/data/simce4to2019.rda", compress = FALSE)


#s <- DF1C %>% group_by(Comuna) %>% 
#  filter(!is.na(value)) %>% 
#  summarise(resultado = mean(value))

simce4to2017$palu_eda_ins_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ins_lect4b_rbd)
simce4to2017$palu_eda_ele_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ele_lect4b_rbd)
simce4to2017$palu_eda_ade_lect4b_rbd <- as.numeric(simce4to2017$palu_eda_ade_lect4b_rbd)
simce4to2017$palu_eda_ins_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ins_mate4b_rbd)
simce4to2017$palu_eda_ele_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ele_mate4b_rbd)
simce4to2017$palu_eda_ade_mate4b_rbd <- as.numeric(simce4to2017$palu_eda_ade_mate4b_rbd)

simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Carah", "CARAHUE")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Nueva", "NUEVA IMPERIAL")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Saave", "SAAVEDRA")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Teodo", "TEODORO SCHMIDT")
simce4to2018$nom_com_rbd <- replace(simce4to2018$nom_com_rbd, simce4to2018$nom_com_rbd == "Tolté", "TOLTËN")
