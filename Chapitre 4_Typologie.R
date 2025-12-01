
# Paramétrage ----
library(sf)
library(dplyr)
library(mapsf)
library(readxl)
library(factoextra)
require(RColorBrewer)
require(ggplot2)
library(cluster)
library(ggdendro)
library(reshape2)
library(ade4)
library(tidyverse)
library(spdep)
library(stats)
library(forcats)
setwd("Chapitre 4/typo2")

# Fond de carte ----
#Sources : Données IGN (GEOFLA/ADMINEXPRESS)
com <- st_read("com_idf_22.shp")
com <- st_transform(com, 2154)
com <- com[,-1]
colnames(com)[1] <- "com_arm_code"
com$com_arm_code <- as.character(com$com_arm_code)
UUParis <- st_read("UUParis.gpkg")

# Indicateur Prix ----

# loyers # Source: Estimations ANIL, à partir des données du Groupe SeLoger et de leboncoin
# Indicateur: loypredm2 : "Loyer d’annonces, charges comprises pour un bien de référence non meublé, pour une annonce mise en ligne au T3 2022"
# loyers <- read.csv2("pred-app-mef-dhup.csv")
# loyers <- loyers %>% filter (REG == 11) %>% select(INSEE_C, loypredm2)
# # loyers <- left_join(com, loyers, by=c("com_arm_code"="INSEE_C"))

# Cout des transactions en 2018
# cassmir_Communes <- st_read("CASSMIR_SpatialDataBase.gpkg", quiet = TRUE, layer = "Communes")
# summary(as.factor(cassmir_Communes$annee))
# MeanPricesBox<- cassmir_Communes %>% 
#   filter (B_PX_M >=100, annee == 2018) %>%
#   select( INSEE_COM, annee, B_PX_M, geom)
# cassmir <- as.data.frame(MeanPricesBox) %>% select(-geom, -annee)
# summary(com$com_arm_code %in% cassmir_Communes$INSEE_COM[cassmir_Communes$annee%in% c(2011:2018)])
# breaks_transacs <- mf_get_breaks(x = MeanPricesBox$B_PX_M, nbreaks =8, breaks = "quantile")
# mf_map(x = MeanPricesBox, var = "B_PX_M",type = "choro",breaks = breaks_transacs)

# Indicateurs part des logements individuels, part des HLM, taux d'évolution 2013-2019 ----
# Source: INSEE RP 2013 et 2019

# RPLOG_19 <- read_excel("RPLOG_19.xlsx") %>% filter(REG=="11") %>% select(-REG)
# RPLOG_13 <- read_excel("RPLOG_13.xlsx") %>% filter(REG=="11") %>% select(-REG)
# RPLOG_08 <- read_excel("RPLOG_08.xlsx") %>% filter(REG=="11") %>% select(-REG)
# RPLOG <- left_join(RPLOG_19, RPLOG_13)
# RPLOG <- left_join(RPLOG, RPLOG_08)
# rm(RPLOG_08,RPLOG_13, RPLOG_19)
# RPLOG$TX_MAIS <- RPLOG$P19_RPMAISON/RPLOG$P19_RP
# RPLOG$TX_LOGSOC <- RPLOG$P19_RP_LOCHLMV/RPLOG$P19_RP
# RPLOG$TX_EVOL13_19 <- (RPLOG$P19_RP-RPLOG$P13_RP)/RPLOG$P13_RP
# RPLOG$TX_EVOL08_19 <- (RPLOG$P19_RP-RPLOG$P08_RP)/RPLOG$P08_RP
# RPLOG<- RPLOG %>% select(CODGEO, TX_MAIS, TX_LOGSOC, TX_EVOL13_19)
# RPLOG <- left_join(com, RPLOG, by=c("com_arm_code"="CODGEO"))
# bks_maisons <- mf_get_breaks(x = RPLOG$TX_MAIS, nbreaks =8, breaks = "quantile")
# bks_HLM <- mf_get_breaks(x = RPLOG$TX_LOGSOC, nbreaks =8, breaks = "quantile")
# bks_EVOL1319 <- mf_get_breaks(x = RPLOG$TX_EVOL13_19, nbreaks =8, breaks = "quantile")
# mf_map(x = RPLOG, var = "TX_MAIS",type = "choro",breaks = bks_maisons)
# mf_map(x = RPLOG, var = "TX_LOGSOC",type = "choro",breaks = bks_HLM)
# mf_map(x = RPLOG, var = "TX_EVOL13_19",type = "choro",breaks = bks_EVOL1319)

# Indicateur pression du secteur social 
# PRES_SOC <- read_excel("Pression_soc_19.xlsx", 
#                        col_types = c("text", "numeric"))
# PRES_SOC$PRESSION_SOC_19[is.na(PRES_SOC$PRESSION_SOC_19)] <- 0
# # PRES_SOC <- left_join(com, PRES_SOC, by=c("com_arm_code"="CODGEO"))
# # bks_pressoc <- mf_get_breaks(x = PRES_SOC$PRESSION_SOC_19, nbreaks =8, breaks = "quantile")
# # mf_map(x = PRES_SOC, var = "PRESSION_SOC_19",type = "choro",breaks = bks_pressoc)

# Typologie ---- 
# Préparation de la table 
# dat <- left_join(com, loyers, by=c("com_arm_code"="INSEE_C"))
# # dat <- left_join(dat, DV3F, by=c("com_arm_code"="CODGEO"))
# #dat <- left_join(dat, cassmir, by=c("com_arm_code"="INSEE_COM"))
# dat <- left_join(dat, RPLOG, by=c("com_arm_code"="CODGEO"))
# dat <- left_join(dat, PRES_SOC, by=c("com_arm_code"="CODGEO"))
# dat <- left_join(dat, abord, by=c("com_arm_code"="Codgeo"))


# export pour exploratR ----
# export <- dat %>%  as.data.frame() %>% select(-geometry)
# write.table(export, "typo3.csv", append = FALSE, sep = ";", dec = ".",
#              row.names = F, col.names = TRUE)


# Import classif exploratr ----
###############################

# Rélisation d'une CAH WARD standardisée sur ExploratR (CAH2_5)
#La fonction :  agnes(x = df[, loypredm2,TX_EVOL13_19,TX_LOGSOC], diss = FALSE, metric = "euclidean", stand = T, method = "ward")

results_exploraR <- read.csv("CAH2.csv")
results_exploraR$com_arm_code <- as.character(results_exploraR$com_arm_code)
map_result <- left_join(com, results_exploraR)
map_result$Classe <- as.factor(map_result$CAH2_5_CLASSES)
map_result$Classe <- fct_recode(map_result$Classe,"Classe 1"="CLASSE 1",
                         "Classe 2"="CLASSE 5",
                         "Classe 3"="CLASSE 3",
                         "Classe 4"="CLASSE 4",
                         "Classe 5"="CLASSE 2")
map_result$Classe <- factor(map_result$Classe, levels = c("Classe 1","Classe 2","Classe 3","Classe 4","Classe 5"))


# Visualisation des résultats
#############################

# Pour ajout des autres UU
# uu2020 <- st_read("Chapitre 4/typo2/uu2020_com_idf.geojson")
# uu2020 <- uu2020[uu2020$type_com=="Unité urbaine",]
# uu2020 <- uu2020 %>% 
#   group_by(libuu2020) %>% 
#   summarize()
# uu2020 <- uu2020[uu2020$libuu2020!="Paris",]
# uu2020 <- st_transform(uu2020, 2154)

pla_typo <- brewer.pal(6, "Set3")[c(5,4,6,1,3)]
mf_init(x = map_result, theme = "default")
mf_shadow(map_result, add = TRUE)
mf_map(map_result, var = "Classe",pal = pla_typo, type = "typo", leg_title = NA, add = TRUE)
mf_map(UUParis, type="base",border = brewer.pal(6, "Set3")[2],col = NA, lwd = 2, add=T)
# mf_map(uu2020, type="base",border = brewer.pal(7, "Set3")[7],col = NA, lwd = 2, add=T)
mf_layout(scale = T, arrow = F,title = "Distribution spatiale des parcs",
          credits = paste0("mapsf", 
                           packageVersion("mapsf"),
                           " cluster",
                           packageVersion("cluster")))

# Profils de classes
####################
# Ajout des données RP19 pour completer le profil 
RP19_profils <- read_excel("RP19_profils.xlsx")
profils <- left_join(map_result, RP19_profils, by=c("com_arm_code"="CODGEO"))
profils <- profils %>% as.data.frame() %>% select(-geometry) 
Rab_RP19 <- read_excel("Rab_RP19.xlsx")
Rab_RP13 <- read_excel("Rab_RP13.xlsx")
Rab_RP <- left_join(Rab_RP19, Rab_RP13)
rm(Rab_RP19, Rab_RP13)
Rab_RP$Evol_parc <- (Rab_RP$P19_LOG - Rab_RP$P13_LOG) / Rab_RP$P13_LOG
Rab_RP$P13_secvac <- Rab_RP$P13_RSECOCC+Rab_RP$P13_LOGVAC
Rab_RP$P19_secvac <- Rab_RP$P19_RSECOCC+Rab_RP$P19_LOGVAC
Rab_RP$P13_secvac[Rab_RP$P13_secvac==0] <- NA
Rab_RP$Evol_secvac <- (Rab_RP$P19_secvac - Rab_RP$P13_secvac) / Rab_RP$P13_secvac
Rab_RP <- Rab_RP %>% select(CODGEO,Evol_parc, Evol_secvac, P19_LOG,P13_LOG, P13_RSECOCC, P13_LOGVAC, P13_RP_PROP, P13_RP)
profils <- left_join(profils, Rab_RP, by=c("com_arm_code"="CODGEO"))
rm(Rab_RP)

# Ajout prix des transcations

cassmir<- st_read("CASSMIR_SpatialDataBase.gpkg", quiet = TRUE, layer = "Communes")
cassmir<- cassmir %>% as.data.frame() %>% 
  filter (B_PX_M >=100, annee == 2018) %>%
  select(INSEE_COM, B_PX_M, B_PM_APP_Q2, B_PX_MAI_Q2)
profils <- left_join(profils, cassmir, by=c("com_arm_code"="INSEE_COM"))
rm(cassmir)

# export liste des communes 
liste_classes_CAH2 <- profils %>% select(com_arm_code, LIBGEO, Classe)
#write.table(liste_classes_CAH2, "liste_classes_CAH2.csv", append = FALSE, sep = ";", dec = ",", row.names = F, col.names = TRUE)

# Figure des profil 
profils$TX_PROP <- profils$P19_RP_PROP/profils$P19_RP
profils$TX_PROP_13 <- profils$P13_RP_PROP/profils$P13_RP
profils$TX_LOC_PRI <- (profils$P19_RP_LOC-profils$P19_RP_LOCHLMV)/profils$P19_RP
profils$SUROCC <- profils$C19_RP_HSTU1P_SUROCC / profils$C19_RP_HSTU1P
profils$TX_SECVAC <- (profils$P19_RSECOCC+profils$P19_LOGVAC)/profils$P19_LOG
profils$EVOL_PART_SECVAC <- profils$TX_SECVAC-((profils$P13_RSECOCC+profils$P13_LOGVAC)/profils$P13_LOG)
profils$EVOL_PART_PROP <- profils$TX_PROP-profils$TX_PROP_13

data <- profils %>% select(Classe, loypredm2,B_PX_M,B_PM_APP_Q2, B_PX_MAI_Q2, TX_EVOL13_19,Evol_parc, EVOL_PART_SECVAC,#Evol_secvac,
                           TX_SECVAC,TX_LOGSOC,PRESSION_SOC_19,SUROCC, TX_LOC_PRI,TX_PROP,TX_MAIS, EVOL_PART_PROP)
colnames(data) <- c("Classe","Loyers d'annonce 2022","Montant moyen des transactions 2018","Prix médian m2 des appartements 2018",
                    "Prix nominal médian des maisons 2018","Évol. parc RP 2013-2019","Évol. parc total 2013-2019","Évol. part vacance et rés. second. 2013-2019",#"Évol. part vacance et rés. second. 2013-2019",
                    "Part vacance et rés. second. 2019","Part des logements sociaux 2019","Pression du secteur social 2019","Part des logements sur-occupés 2019","Part des locations privées 2019","Part des propriétaires occupants 2019","Part des maisons 2019", "EVOL_PART_PROP")
data_cor <- data[,c("Loyers d'annonce 2022","Évol. parc RP 2013-2019", "Part des logements sociaux 2019")]
cor_soc_VF = as.data.frame(cor(data[,2:ncol(data)]))
cor_soc_synth = as.data.frame(cor(data_cor))
clusProfile <- aggregate(scale(data[, c(2:ncol(data))]),
                         by = list(data$Classe),FUN = "mean", na.rm=TRUE, na.action=NULL)
# # version non standardisée
clusProfile <- aggregate(data[, c(2:ncol(data))],
                         by = list(data$Classe),FUN = "mean", na.rm=TRUE, na.action=NULL)

colnames(clusProfile)[1] <- "Classe"
clusLong <- melt(clusProfile, id.vars = "Classe")
#clusLong <- clusLong[-c(46:50),]
clusLong$variable <- factor(clusLong$variable, levels = rev(c("Loyers d'annonce 2022",
                                                              "Montant moyen des transactions 2018",
                                                              "Prix médian m2 des appartements 2018",
                                                              "Prix nominal médian des maisons 2018",
                                                              "Part des logements sur-occupés 2019",
                                                              "Part des logements sociaux 2019",
                                                              "Pression du secteur social 2019",
                                                              "Part des locations privées 2019",
                                                              "Part des propriétaires occupants 2019",
                                                              "Part des maisons 2019",
                                                              "Part vacance et rés. second. 2019",
                                                              "Évol. part vacance et rés. second. 2013-2019",#"Évol. vacance et rés. second. 2013-2019",
                                                              "Évol. parc RP 2013-2019",
                                                              "Évol. parc total 2013-2019", 
                                                              "EVOL_PART_PROP"
                                                              )))

clusLong <- clusLong %>% filter(variable %in% c("Évol. part vacance et rés. second. 2013-2019",
                                                "Évol. parc RP 2013-2019",
                                                "Évol. parc total 2013-2019", 
                                                "EVOL_PART_PROP"))

cols_profile <- c("#4575B4","black","#4575B4","#4575B4","#4575B4","#4575B4","#4575B4","#4575B4","black","#4575B4","#4575B4","#4575B4","#4575B4","black")
profilePlot <- ggplot(clusLong) +
  geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
  theme_bw()  +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8, colour =cols_profile),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=6.5))+
  #scale_y_continuous("Valeur moyenne") +
  labs(x = element_blank(), y = element_blank()) +
  facet_wrap(~ Classe, nrow = 3) + coord_flip() #+
  #labs(caption = paste0("Les variables sont standardisées.\nLes variables d'observation sont en bleu.\ncluster 2.1.3"))
profilePlot



# Export des résultats ---- 
pageZ <- readRDS("pageZ.rds") # Voir Github Romain Leconte

# carte
pageZ(format = "portrait_half", output = "svg", name = "Carto_CAH2")
mf_init(x = map_result, theme = "default")
mf_shadow(map_result, add = TRUE)
mf_map(map_result, var = "Classe",pal = pla_typo, type = "typo", leg_title = NA, add = TRUE)
mf_map(UUParis, type="base",border = brewer.pal(6, "Set3")[2],col = NA, lwd = 2, add=T)
mf_annotation(x = UUParis[1], txt = "Unité urbaine")
mf_layout(scale = T, arrow = F,title = "Distribution spatiale des marchés",
          credits = paste0("mapsf", 
                           packageVersion("mapsf"),
                           " cluster",
                           packageVersion("cluster")))
dev.off()

# Profils
# "custom",w_cust = 6.7, h_cust = 5
pageZ(format = "portrait",output = "svg", name = "Profils_CAH2_V5")
profilePlot
dev.off()

# Table de correspondance communes classes
saveRDS(object = liste_classes_CAH2, file = "liste_com_classes_CAH2.rds")

# Carte pour intersectio spataile
saveRDS(object = map_result[,c(1,12,11)], file = "mapresults_CAH2.rds")

# Table des correlations
saveRDS(object = cor_soc_VF, file = "correlations_CAH2.rds")
rm(list = ls())

liste_classes_CAH2 <- read.csv("typo2/liste_classes_CAH2.csv", sep=";")
summary(as.factor(liste_classes_CAH2$Classe))


