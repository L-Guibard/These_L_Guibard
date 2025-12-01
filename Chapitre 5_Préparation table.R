# Construction de la table de référence pour la mesure de la périphérisation
library(dplyr)
library(sf)
library(stringi)
library(stringi)

idf <- c("75","77","78","91","92","93","94","95")

geo19 <- readRDS("adr_BanGeocoded_19.rds")
geo19 <- geo19[geo19$BAN_result!="NA",] #63 non géocodés
geo19 <- st_as_sf(geo19, coords = c(6,7))
st_crs(geo19) <- 4326
geo19 <- st_transform(geo19, 2154)
grille200 <- st_read("/grille200m_metropole.shp")
geo19_inter <- st_intersection(grille200, geo19)
rm(grille200)
coords <- st_coordinates(geo19_inter)
geo19_inter <- cbind(geo19_inter[,c(3,1,2)], as.data.frame(coords))
colnames(geo19_inter) <- c("ID19","idINSPIRE_19", "id_carr_1k_19","X_19","Y_19")
geo19_inter <- as.data.frame(geo19_inter)
geo19_inter <- geo19_inter[,-6]
geo19_inter$BAN <- "OUI"
geo19_inter <- geo19_inter[!(is.na(geo19_inter$ID19)),]
summary(duplicated(geo19_inter$ID19))
rm(geo19)

load("ALLOCS_1819_ND_G_BAN_2.RData")
ALLOC_IDF_ND_G_19 <- ALLOC_IDF_ND_G_19[substr(ALLOC_IDF_ND_G_19$NUMCOMDO_18,1,2) %in% idf,]
#ALLOC_IDF_ND_G_19 <- ALLOC_IDF_ND_G_19[!(is.na(ALLOC_IDF_ND_G_19$LILI4ADR_19)),]
ALLOC_IDF_ND_G_19 <- ALLOC_IDF_ND_G_19[!(is.na(ALLOC_IDF_ND_G_19$ID19)),]
summary(duplicated(geo19_inter$ID19))
colnames(ALLOC_IDF_ND_G_19[,c(85, 86,107, 108, 89, 90,109, 110, 111:150)])
ALLOC_IDF_ND_G_19 <- ALLOC_IDF_ND_G_19[,-c(111:150)]

ALLOC_A <- ALLOC_IDF_ND_G_19[!(ALLOC_IDF_ND_G_19$ID19 %in% geo19_inter$ID19),c(1:84,87,88,91:106,85, 86,107, 108, 89, 90,109, 110)]
ALLOC_A$BAN <- "NON"
ALLOC_B <- ALLOC_IDF_ND_G_19[ALLOC_IDF_ND_G_19$ID19 %in% geo19_inter$ID19,c(1:84,87,88,91:106,85, 86,107, 108)]
geo19_inter <- geo19_inter[,c(1,4,5,2,3,6)]
ALLOC_B <- left_join(ALLOC_B, geo19_inter)
ALLOC_IDF_ND_G_19 <- rbind(ALLOC_A, ALLOC_B)

# Ajout des données de géoloc pour les faux déménagement (numcomdos déjà corrigés dans SAS) et les fixes HND 
data <- ALLOC_IDF_ND_G_19[ALLOC_IDF_ND_G_19$MOBILE2==0 & ALLOC_IDF_ND_G_19$LILI4ADR_18 != ALLOC_IDF_ND_G_19$LILI4ADR_19, ]
summary(data$NUMCOMDO_18==data$NUMCOMDO_19)
ALLOC_IDF_ND_G_19$idINSPIRE_19[ALLOC_IDF_ND_G_19$MOBILE2=="0"] <- ALLOC_IDF_ND_G_19$idINSPIRE_18[ALLOC_IDF_ND_G_19$MOBILE2=="0"]
ALLOC_IDF_ND_G_19$id_carr_1k_19[ALLOC_IDF_ND_G_19$MOBILE2=="0"] <- ALLOC_IDF_ND_G_19$id_carr_1k_18[ALLOC_IDF_ND_G_19$MOBILE2=="0"]
ALLOC_IDF_ND_G_19$X_19[ALLOC_IDF_ND_G_19$MOBILE2=="0"] <- ALLOC_IDF_ND_G_19$X_18[ALLOC_IDF_ND_G_19$MOBILE2=="0"]
ALLOC_IDF_ND_G_19$Y_19[ALLOC_IDF_ND_G_19$MOBILE2=="0"] <- ALLOC_IDF_ND_G_19$Y_18[ALLOC_IDF_ND_G_19$MOBILE2=="0"]
ALLOC_IDF_ND_G_19$NUMCOMDO_19[ALLOC_IDF_ND_G_19$MOBILE2=="0"] <- ALLOC_IDF_ND_G_19$NUMCOMDO_18[ALLOC_IDF_ND_G_19$MOBILE2=="0"]
rm(ALLOC_A, ALLOC_B, coords, data, geo19_inter)

# Mesures de distance et de périphérisation
###########################################

# Evolution de la distance a Paris (Hotel de ville)
HVPARIS <- data.frame("HVPARIS",2.352445684697899,48.85656728733478)  # Coordonnees recuperees sur OSM
colnames(HVPARIS) <- c("ID","x","y")
HVPARIS <- st_as_sf(HVPARIS,coords = c(2,3))
st_crs(HVPARIS) <- 4326
HVPARIS <- st_transform(HVPARIS, 2154) # Conversion en Lambert
HDV_L93 <- st_coordinates(HVPARIS)
X_HDV <- HDV_L93[,1]
Y_HDV <- HDV_L93[,2]

# Entre les deux logements 
ALLOC_IDF_ND_G_19$DIST_PARC_EUC <- sqrt((ALLOC_IDF_ND_G_19$X_18 - ALLOC_IDF_ND_G_19$X_19)^2+(ALLOC_IDF_ND_G_19$Y_18 - ALLOC_IDF_ND_G_19$Y_19)^2)
ALLOC_IDF_ND_G_19$DIST_PARIS_EUC_18 <- sqrt((X_HDV - ALLOC_IDF_ND_G_19$X_18)^2+(Y_HDV - ALLOC_IDF_ND_G_19$Y_18)^2)
ALLOC_IDF_ND_G_19$DIST_PARIS_EUC_19 <-  sqrt((X_HDV - ALLOC_IDF_ND_G_19$X_19)^2+(Y_HDV - ALLOC_IDF_ND_G_19$Y_19)^2)
ALLOC_IDF_ND_G_19$EVOL_DIST_PARIS_EUC  <- ALLOC_IDF_ND_G_19$DIST_PARIS_EUC_19 - ALLOC_IDF_ND_G_19$DIST_PARIS_EUC_18

dist_centre_paris <- readRDS("dist_centre_paris_15_19_BAN.rds")
dist_centre_paris <- as.data.frame(dist_centre_paris)
dist_centre_paris <- dist_centre_paris[,c(1,4)]
matdist_orsm <- readRDS("matdist_osrm1819_BAN.rds")
ALLOC_IDF_ND_G_19 <- left_join(ALLOC_IDF_ND_G_19, matdist_orsm)
colnames(dist_centre_paris) <- c("ID","dist_osrm_hdv_P1")
ALLOC_IDF_ND_G_19 <- left_join(ALLOC_IDF_ND_G_19, dist_centre_paris, by=c("idINSPIRE_18"="ID"))
colnames(dist_centre_paris) <- c("ID","dist_osrm_hdv_P2")
ALLOC_IDF_ND_G_19 <- left_join(ALLOC_IDF_ND_G_19, dist_centre_paris, by=c("idINSPIRE_19"="ID"))
ALLOC_IDF_ND_G_19$EVOL_DIST_PARIS_OSRM <- ALLOC_IDF_ND_G_19$dist_osrm_hdv_P2 - ALLOC_IDF_ND_G_19$dist_osrm_hdv_P1
rm(dist_centre_paris, matdist_orsm)

# Autres indicateurs de périphérisation
#######################################

# Population (1km)
pot_pop_1k <- readRDS("pot_pop_1k.rds")
pot_pop_1k <- as.data.frame(pot_pop_1k)
pot_pop_1k <- pot_pop_1k[,c(1,3,5,7)]

# Emplois (1km)
pot_emploi_1k <- readRDS("pot_emploi_1k_S3.rds") 
pot_emploi_1k <- as.data.frame(pot_emploi_1k)
pot_emploi_1k <- pot_emploi_1k[,-c(3,4,9)]

# Transport (200m)
pot_transports_AU1 <- readRDS("pot_transports_AU1.rds")
pot_transports_AU1 <- as.data.frame(pot_transports_AU1)
pot_transports_AU1 <- pot_transports_AU1[,c(1,11)]

# BPE (1km)
res_bpe <- readRDS("res_bpe.rds")
res_bpe <- res_bpe[,c(1,2,4,6,18:19)]

potentiels <- function(MOBCAF, pot_pop, pot_emploi, pot_transports, pot_equips){
  # Population
  colnames(pot_pop) <- c("ID","Mena_acces_euc_P1", "Ind_snv_P1", "tx_men_pauv_ACC_P1")
  MOBCAF <- left_join(MOBCAF, pot_pop, by=c("id_carr_1k_18"="ID"))
  colnames(pot_pop) <- c("ID","Mena_acces_euc_P2","Ind_snv_P2", "tx_men_pauv_ACC_P2")
  MOBCAF <- left_join(MOBCAF, pot_pop, by=c("id_carr_1k_19"="ID"))
  MOBCAF$EVOL_POP_ACC <- MOBCAF$Mena_acces_euc_P2 - MOBCAF$Mena_acces_euc_P1
  MOBCAF$EVOL_NIVECO_POP_ACC <- MOBCAF$Ind_snv_P2 - MOBCAF$Ind_snv_P1
  MOBCAF$EVOL_POP_PAUV_ACC <- MOBCAF$tx_men_pauv_ACC_P2 - MOBCAF$tx_men_pauv_ACC_P1
  # Emploi
  colnames(pot_emploi) <- c("ID","EMP_acces_euc_P1","CS3_ACC_P1","CS4_ACC_P1","CS5_ACC_P1","CS6_ACC_P1")
  MOBCAF <- left_join(MOBCAF, pot_emploi, by=c("id_carr_1k_18"="ID"))
  colnames(pot_emploi) <- c("ID","EMP_acces_euc_P2","CS3_ACC_P2","CS4_ACC_P2","CS5_ACC_P2","CS6_ACC_P2")
  MOBCAF <- left_join(MOBCAF, pot_emploi, by=c("id_carr_1k_19"="ID"))
  MOBCAF$EVOL_EMP_ACCESS <- MOBCAF$EMP_acces_euc_P2 - MOBCAF$EMP_acces_euc_P1
  MOBCAF$EVOL_EMP_CS3 <- MOBCAF$CS3_ACC_P2 - MOBCAF$CS3_ACC_P1
  MOBCAF$EVOL_EMP_CS4 <- MOBCAF$CS4_ACC_P2 - MOBCAF$CS4_ACC_P1
  MOBCAF$EVOL_EMP_CS5 <- MOBCAF$CS5_ACC_P2 - MOBCAF$CS5_ACC_P1
  MOBCAF$EVOL_EMP_CS6 <- MOBCAF$CS6_ACC_P2 - MOBCAF$CS6_ACC_P1
  
  # Transports
  colnames(pot_transports) <- c("ID","SCORE_TEC_P1")
  MOBCAF <- left_join(MOBCAF, pot_transports, by=c("idINSPIRE_18"="ID"))
  colnames(pot_transports) <- c("ID","SCORE_TEC_P2")
  MOBCAF <- left_join(MOBCAF, pot_transports, by=c("idINSPIRE_19"="ID"))
  MOBCAF$EVOL_SCORE_TEC <- MOBCAF$SCORE_TEC_P2 - MOBCAF$SCORE_TEC_P1
  
  # Equipements
  colnames(pot_equips) <- c("ID","mean_prox_P1", "mean_inter_P1","mean_sup_P1", "index_bpe_P1","index_bpe0_P1")
  MOBCAF <- left_join(MOBCAF, pot_equips, by=c("id_carr_1k_18"="ID"))
  colnames(pot_equips) <- c("ID","mean_prox_P2", "mean_inter_P2","mean_sup_P2", "index_bpe_P2","index_bpe0_P2")
  MOBCAF <- left_join(MOBCAF, pot_equips, by=c("id_carr_1k_19"="ID"))
  MOBCAF$EVOL_MEAN_PROX <- MOBCAF$mean_prox_P2 - MOBCAF$mean_prox_P1
  MOBCAF$EVOL_MEAN_INTER <- MOBCAF$mean_inter_P2 - MOBCAF$mean_inter_P1
  MOBCAF$EVOL_MEAN_SUP <- MOBCAF$mean_sup_P2 - MOBCAF$mean_sup_P1
  MOBCAF$EVOL_INDEX_BPE <- MOBCAF$index_bpe_P2 - MOBCAF$index_bpe_P1
  MOBCAF$EVOL_INDEX_BPE0 <- MOBCAF$index_bpe0_P2 - MOBCAF$index_bpe0_P1
  return(MOBCAF)
}

#load("W:/CTRAD/THEMATIQUES/Mobilites_2019/ADD/Data/ALLOCS19_3.RData")
ALLOC_IDF_ND_G_19 <- potentiels(ALLOC_IDF_ND_G_19,pot_pop_1k,pot_emploi_1k,pot_transports_AU1,res_bpe)
rm(pot_pop_1k,pot_emploi_1k,pot_transports_AU1,res_bpe, HDV_L93, HVPARIS, X_HDV, Y_HDV, potentiels)

ALLOC <- ALLOC_IDF_ND_G_19
ALLOC_IDF <- ALLOC[ALLOC$MRS_IDF=="0",]
rm(ALLOC_IDF_ND_G_19)

# summary(is.na(ALLOC_IDF_ND_G_19$X_19))
# test <- ALLOC_IDF_ND_G_19[is.na(ALLOC_IDF_ND_G_19$X_19),]
# test1 <- test[substr(test$NUMCOMDO_19,1,2) %in% idf,]
# test2 <- test[!(substr(test$NUMCOMDO_19,1,2) %in% idf),c("NUMCOMDO_18","NUMCOMDO_19")]
# summary(as.factor(substr(as.character(test$NUMCOMDO19),1,2)))
# rm(test, test1, test2)

# Correction de mauvais géocodages
##################################
ALLOC_IDF_G <- ALLOC_IDF[!(is.na(ALLOC_IDF$X_19)),]
ALLOC_IDF_G <- st_as_sf(ALLOC_IDF_G, coords = c("X_19","Y_19"))
st_crs(ALLOC_IDF_G) <- 2154
ALLOC_IDF_G <- st_intersection(com_idf, ALLOC_IDF_G)
ALLOC_IDF_G <- as.data.frame(ALLOC_IDF_G)
c <- ALLOC_IDF_G[,-161]
summary(duplicated(ALLOC_IDF_G$ID19))
en_idf <- ALLOC_IDF_G$ID19
# ALLOC_IDF_G$C_INSEE <- as.character(ALLOC_IDF_G$C_INSEE)
# test <- ALLOC_IDF_G[ALLOC_IDF_G$NUMCOMDO_19 != ALLOC_IDF_G$C_INSEE,c("C_INSEE","NUMCOMDO_19")]
ALLOC_IDF_G <- ALLOC_IDF[ALLOC_IDF$ID19 %in% en_idf,]
rm(ALLOC_IDF, com_idf, deps, parc, paris, pc, rivers, test, en_idf)

save.image("ALLOCS_1819_ND_G_BAN_3.RData")
