
#saveRDS(resultados_agencia_8esc, file = "~/R/projects/slepca/slepca/data/8estab_2.Rdata")
#saveRDS(establecimientos, file = "~/R/projects/slepca/data/establecimientos.Rdata")
#saveRDS(X8escuelas_resultados_trabajo, file = "~/R/projects/slepca/slepca/data/8estab_promedios.Rdata")·
#saveRDS(matricula16_22, file = "~/R/projects/slepca/data/matricula16_22.Rdata")
#saveRDS(Resumen_Rendimiento_17_19, file = "~/R/projects/slepca/slepca/data/rendimiento17_19.Rdata")
#saveRDS(simce4b2016_rbd, file = "~/R/projects/slepca/slepca/data/simce4to2016.Rdata", compress = FALSE)
#saveRDS(simce4b2017_rbd, file = "~/R/projects/slepca/slepca/data/simce4to2017.Rdata", compress = FALSE)
#saveRDS(simce4tob2019_slepca, file = "~/R/projects/slepca/slepca/data/simce4to2019.rda", compress = FALSE)
#saveRDS(asistentes_2021, file = "~/R/projects/slepca/data/asistentes2019_2021.Rdata", compress = FALSE)
#saveRDS(docentes2019_2021, file = "~/R/projects/slepca/data/docentes2019_2021.Rdata", compress = FALSE)
#saveRDS(sepa_inicial_puntaje, file = "~/R/projects/slepca/data/sepa_inicial_puntaje.Rdata", compress = FALSE)
#saveRDS(sepa_inicial_ejes, file = "~/R/projects/slepca/data/sepa_inicial_ejes.Rdata", compress = FALSE)
#saveRDS(dia_ejes_res_2021, file = "~/R/projects/slepca/data/dia_ejes_res_2021.Rdata", compress = FALSE)
#saveRDS(dia_categ_2021, file = "~/R/projects/slepca/data/dia_categ_2021.Rdata", compress = FALSE)
#saveRDS(sepa_final_puntaje, file = "~/R/projects/slepca/data/sepa_final_puntaje.Rdata", compress = FALSE)
#saveRDS(sepa_final_ejes, file = "~/R/projects/slepca/data/sepa_final_ejes.Rdata", compress = FALSE)
#saveRDS(nomina_estudiantes_abril2022, file = "~/R/projects/slepca/data/nomina_abril_2022.Rdata", compress = FALSE)
#saveRDS(eval_doce, file = "~/R/projects/slepca/data/eval_docente.Rdata", compress = FALSE)
#saveRDS(eval_doce_ab, file = "~/R/projects/slepca/data/eval_doce_ab.Rdata", compress = FALSE)
saveRDS(dotacion2021, file = "~/R/projects/slepca/data/dotacion2021.Rdata", compress = FALSE)



eval <- readRDS("~/R/projects/slepca/data/eval_doce_ab.Rdata")
nomina <- readRDS("~/R/projects/slepca/data/nomina_abril_2022.Rdata")

establecimientos <- readRDS("~/R/projects/slepca/data/establecimientos.Rdata")
hist_mat_det <- readRDS("~/R/projects/slepca/data/matricula16_20.Rdata")
hist_mat <- readRDS("~/R/projects/slepca/data/hist_mat.Rdata")

saveRDS(As_Abril_2019, file = "~/R/projects/slepca/data/asistencia4_19.Rdata")
        
#<- As_Abril_2019  %>% filter(COD_REG_RBD == 9 && COD_DEPE == 6)


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

#idps
rbds <- c(6348,6349,6350,6351,6352,6353,6354,6358,6361,6363,6370,6371,6373,6374,6377,
          6397,6398,6399,6400,6407,6410,6411,6413,6415,6452,6454,6455,6456,6457,6458,
          6459,6495,6497,6499,6500,6501,6502,6504,6506,6508,6510,6512,6515,6518,6521,
          6522,6523,6524,6525,6526,6527,6528,6529,6530,6539,6585,6587,6588,6589,6590,
          6591,6593,6600,6602,6605,6608,6611,6623,6625,6628,6633,6635,6639,12366,
          20053,20248)
codigos <- c(9118,9117,9116,9102,9111)
idps4b2016_rbd_f <- idps4b2016_rbd %>%
  filter(rbd %in% rbds)
#saveRDS(idps4b2016_rbd_f, file = "~/R/projects/slepca/slepca/data/idps4to2016.Rdata", compress = FALSE)
#saveRDS(idps4b2016_rbd, file = "~/R/projects/slepca/slepca/data/idps4to2016.Rdata", compress = FALSE)
#saveRDS(idps4b_2019, file = "~/R/projects/slepca/slepca/data/idps4to2019.Rdata", compress = FALSE)
idps4b2017_rbd_f <- idps4b2017_rbd_final %>%
  filter(cod_reg_rbd == 9 & cod_depe2 ==1 &
           cod_com_rbd %in% codigos)
#saveRDS(idps4b2017_rbd_f, file = "~/R/projects/slepca/slepca/data/idps4to2017.Rdata", compress = FALSE)
idps4b2018_rbd_f <- idps_4b2018 %>%
  filter(cod_reg_rbd == 9 & rbd %in% rbds)
#saveRDS(idps4b2018_rbd_f, file = "~/R/projects/slepca/slepca/data/idps4to2018.Rdata", compress = FALSE)

#idps4b2019_rbd_f <- idps19_rbd %>%
#  filter(cod_reg_rbd == 9 & rbd %in% rbds)
#saveRDS(idps4b2019_rbd, file = "~/R/projects/slepca/slepca/data/simce4to2019.rda", compress = FALSE)

#idps19_rbd_f <- idps19_rbd %>% filter(rbd %in% rbds)

#mat2021 <- Mat_unica_2021 %>% filter(COD_REG_RBD == 9 & COD_DEPE == 6)
#mat2021 <- readRDS("~/R/projects/slepca/data/mat2021.Rdata")
#mat2021 <- mat2021 %>% select("AGNO", "RBD", "DGV_RBD", "NOM_RBD", "COD_COM_RBD", "NOM_COM_RBD", "RURAL_RBD", "ESTADO_ESTAB", "COD_ENSE", "COD_ENSE2", "COD_ENSE3", "COD_GRADO", "COD_GRADO2", "LET_CUR", "COD_JOR", "COD_TIP_CUR", "COD_DES_CUR", "MRUN", "GEN_ALU", "FEC_NAC_ALU", "EDAD_ALU", "COD_COM_ALU", "COD_SEC", "COD_ESPE", "COD_RAMA", "COD_MEN", "ENS")
#saveRDS(mat2021, file = "~/R/projects/slepca/data/mat2021.Rdata", compress = FALSE)

saveRDS(priorit_2021, file = "~/R/projects/slepca/data/priorit_2021.Rdata", compress = FALSE)

