# Code chapitre 1 ----
#====================#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(potential)
library(scales)
library(units)
library(sf)
library(osmdata)
library(spatstat)
library(maptools)
library(raster)
library(cartography)
library(dplyr)

# Instabilité de la population allocataire ----
#=============================================#

# Combien sont absents des bases en 2019 ?
# load("INDS_19.Rdata")
# summary(is.na(IND_1819$ID18))
# summary(is.na(IND_1819$ID19))
# tab <- IND_1819[!is.na(IND_1819$ID18),]
# nb_foyers_1218 <- nrow(tab)
# nb_foyers_1819 <- nrow(tab[!is.na(tab$ID19),])
# tx_maintien_19 <- round(nrow(tab[!is.na(tab$ID19),])/nrow(tab)*100,1)
# rm(IND_1718, IND_1819,tab)
# load("INDS_19.Rdata")
# IND_1819$ND1 <- as.factor(IND_1819$ND1)
# IND_1819 <- IND_1819[IND_1819$ND1=="OUI",]
# IND_1819 <- IND_1819[substr(IND_1819$NUMCOMDO_18,1,2) %in% idf,]
# rm(list = ls())
pertes1819_bases <- round((12462 / (12462 + 2277793))*100,1) # 0,5 % de perte 

# Évolution la population allocataire en fonction de celle des prestations ----
# Données mois par mois, 2016 - 2020 (Noyaux durs franciliens uniquement)

# Profil de la population allocataire ----
#========================================#

# Import BDD fixe
load("ALLOCS_1819_ND_G_BAN_2.RData") # Import des données allocataires 2019 V2
summary(ALLOC$DEPTCD %in% idf)
summary(is.na(ALLOC$ID19))

# Taux de ND2, nb foyers et population couverte
summary(ALLOC$ND2)
nbfoyer18 <- nrow(ALLOC)
perscouv18 <- sum(ALLOC$PERSCOUV_P1)
perscouv19 <- sum(ALLOC$PERSCOUV_P2)
tx_HND2_19 <- round(summary(ALLOC$ND2)[1]/(summary(ALLOC$ND2)[1]+summary(ALLOC$ND2)[2])*100,1)

# Répartition selon CATBEN
#test <- as.data.frame(summary(ALLOC$CATBEN_18))

# Répartition selon PRESFRES
nbfoyer_presfresP1 <- summary(ALLOC$PRESFRES_18) # Par foyer
pop_presfresP1 <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$PRESFRES_18, sum)
nbfoyer_presfresP2 <- summary(ALLOC$PRESFRES_19) # Par foyer
pop_presfresP2 <- tapply(ALLOC[, "PERSCOUV_P2"], ALLOC$PRESFRES_19, sum)

# répartition en niveau de revenu 
nbfoyer_rev <- summary(ALLOC$REV_CLASSE) # Par foyer
pop_rev <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$REV_CLASSE, sum)
rev_mean <- mean(ALLOC$RUCDERRE[ALLOC$REV_CLASSE != "INC"])
quartile_rev <- quantile(ALLOC$RUCDERRE[ALLOC$REV_CLASSE != "INC"])
deciles_rev <- quantile(ALLOC$RUCDERRE[ALLOC$REV_CLASSE != "INC"],probs = seq(0, 1, 0.1))
seuilBR_18 <- 1071

# répartition situation familiale P1 et P2
nbfoyer_sitfamP1 <- summary(ALLOC$CLASS_SITFAM_P1) # Par foyer
pop_sitfamP1 <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$CLASS_SITFAM_P1, sum)
nbfoyer_sitfamP2 <- summary(ALLOC$CLASS_SITFAM_P2) # Par foyer
pop_sitfamP2 <- tapply(ALLOC[, "PERSCOUV_P2"], ALLOC$CLASS_SITFAM_P2, sum)

# Répartition nationalité
ALLOC$NATIFAM_18 <- as.factor(ALLOC$NATIFAM_18)
nbfoyer_NATIFAM <- summary(ALLOC$NATIFAM_18) # Par foyer
pop_NATIFAM <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$NATIFAM_18, sum)

# Répartition situation face à l'emploi (reprendre la méthode de classification)
nbfoyer_ACTRESPD_P1 <- summary(ALLOC$ACTRESPD_P1) # Par foyer
pop_ACTRESPD_P1 <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$ACTRESPD_P1, sum)
nbfoyer_ACTCONJ_P1 <- summary(ALLOC$ACTCONJ_P1) # Par foyer
pop_ACTCONJ_P1 <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$ACTCONJ_P1, sum)
nbfoyer_ACTRESPD_P2 <- summary(ALLOC$ACTRESPD_P2) # Par foyer
pop_ACTRESPD_P2 <- tapply(ALLOC[, "PERSCOUV_P2"], ALLOC$ACTRESPD_P2, sum)
nbfoyer_ACTCONJ_P2 <- summary(ALLOC$ACTCONJ_P2) # Par foyer
pop_ACTCONJ_P2 <- tapply(ALLOC[, "PERSCOUV_P2"], ALLOC$ACTCONJ_P2, sum)

# Répartition PPRPPU 
nbfoyer_PPRPPU18 <- summary(ALLOC$PPRPPU_P1) # Par foyer
pop_PPRPPU18 <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$PPRPPU_P1, sum)
nbfoyer_PPRPPU19 <- summary(ALLOC$PPRPPU_P2) # Par foyer
pop_PPRPPU19 <- tapply(ALLOC[, "PERSCOUV_P2"], ALLOC$PPRPPU_P2, sum)

# Genre des allocataires
genre_alloc18 <- summary(ALLOC$SEXE)
rm(ALLOC,ALLOC_IDF_G)
save.image("chap3_stabilite_profil.RData")
rm(list = ls())

# Comparaison de la population allocataire (FR6_1218) et de la population générale (Insee, dossier complet de la région Ile-de-France, RP18)----
#====================================================================================================================================================#

load("ALLOCS_1819_ND_G_BAN.RData") # Import des données allocataires 2019 V1

# Population couverte
perscouv18 <- sum(ALLOC$PERSCOUV_P1)
pop_insee19 <- 12213447
pop_couv <- round(perscouv18/pop_insee19*100,1)
# Population de moins de 25 ans couverte
moins25_caf <- sum(ALLOC$NBLENFA_P1) #+ ALLOC$NBNAIMOI)
Insee_0_24 <- c(1091343 + 1069887 + 804053 + 484545 + 474963)
pop_couv_moins25 <- round(moins25_caf/Insee_0_24*100,1)

# Taille moyenne des ménages
mean_perscouv <- round(mean(ALLOC$PERSCOUV_P1),2)
mean_menagesRP <- 2.3

# Répartition en structures familiales 
pop_sitfamP1 <- tapply(ALLOC[, "PERSCOUV_P1"], ALLOC$CLASS_SITFAM_P1, sum)
SITFAM_compar_pop <- as.data.frame(rbind(
  c("Population","Seuls", "Couples", "Autres", "Couples enfant(s)", "Monoparents"),
  c("FR6_1218", 849613,  165450, 52239, 3980071,  1028004),
  c("RP18",1951056,2244118, 312613, 5895155, 1584163)))
colnames(SITFAM_compar_pop) <- SITFAM_compar_pop[1,]
SITFAM_compar_pop <- SITFAM_compar_pop[-1,]

SITFAM_compar_foy <- as.data.frame(rbind(
  c("Population","Seuls", "Couples", "Autres", "Couples enfant(s)", "Monoparents"),
  c("FR6_1218", 845810,  82160,   32420,  942464,  374939),
  c("RP18",1951056, 1082617, 133177,1470355, 584978)))
colnames(SITFAM_compar_foy) <- SITFAM_compar_foy[1,]
SITFAM_compar_foy <- SITFAM_compar_foy[-1,]

# Taux de pauvreté
tx_pov_popcaf <- round(sum(ALLOC$PERSCOUV_P1[ALLOC$BAS_REV=="1"])/sum(ALLOC$PERSCOUV_P1)*100,1)
tx_pov_popRP <- 15.6 # ménages fiscaux en 2019
# Taux de résidence minimal en logement social
tx_logsoc_popcaf <- round(nrow(ALLOC[ALLOC$PPRPPU_P1=="PUBLIC",])/nrow(ALLOC)*100,1) # foyers
tx_logsoc_popRP <- 22.1 # Part des résidences principales HLM (Données ODT)
# Taux de mob g ----
tx_mob_pop_19 <- round(sum(ALLOC$PERSCOUV_P1[ALLOC$MOBILE2=="1"])/sum(ALLOC$PERSCOUV_P1)*100,1)
tx_mob_popRP <- 10.49 # résultat de Migcom 2019

# Mettre en avant les différences entre les deux calculs (prise en compte des résidents étrangers...)
# => construction d'un tableau avec les données de l'Insee
rm(ALLOC, ALLOC_IDF_G)
save.image("chap3_compare.RData")
rm(list = ls())

# Une population inégalement représentée dans l’espace francilien (potentiels) ----
# ================================================================================#
load("fond_de_carte_idf_osm.RData")
rm(com_idf, deps, parc, paris, pc, rivers)
load("ALLOCS_1819_ND_G_BAN.RData")
# Densité de population couverte 
ALLOC18 <- ALLOC_IDF_G[,c("PERSCOUV_P1","X_18","Y_18")]
ALLOC18 <- na.exclude(ALLOC18)
ALLOC18 <- st_as_sf(ALLOC18, coords = c(2,3))
st_crs(ALLOC18) <- 2154
bb <- as(ALLOC18, "Spatial")
bbowin <- as.owin(as(idf, "Spatial"))
pts <- coordinates(bb)
p <- ppp(pts[,1], pts[,2], window=bbowin)
ds <- density.ppp(p, sigma = 200, eps = c(50,50))
rasdens_alloc18 <- raster(ds) * 1000 * 1000
rasdens_alloc18 <- rasdens_alloc18+1
rm(p,ds,pts,bb,bbowin)

# Taux de couverture potentiel (agregation à la commune)
com_idf20 <- st_read("geo_idf_1820.geojson") # Fond de carte communal IDF
com_idf20 <- com_idf20[,c(15,42)]
colnames(com_idf20)[1] <- "code"
pop_com_idf_18 <- read.csv("pop_com_idf_18.csv", sep=";") # Populations communales (RP18)
pop_com_idf_18 <- pop_com_idf_18[,c(5,9)]
colnames(pop_com_idf_18) <- c("code","pop_18")
pop_com_idf_18$code <- as.character(pop_com_idf_18$code)
com_idf20 <- st_as_sf(com_idf20)
com_idf20 <- st_transform(com_idf20, 2154)
ALLOC18 <- st_intersection(com_idf20, ALLOC18)
ALLOC18 <- aggregate(ALLOC18$PERSCOUV_P1, by=list(ALLOC18$code), FUN="sum")
colnames(ALLOC18) <- c("code","PERSCOUV")
com_idf20 <- left_join(com_idf20, pop_com_idf_18)
com_idf20 <- left_join(com_idf20, ALLOC18)
com_idf20 <- st_centroid(com_idf20)
y <- create_grid(x = idf, res = 1000)
pot <- mcpotential(x = com_idf20, y = y, 
                   var = c("pop_18","PERSCOUV"), fun = "e",
                   span = 2500, beta = 2, 
                   limit = 30000, ncl = 4)
pot <- as.data.frame(pot)
y <- cbind(y, pot)
y$taux_couv <- 100 * y$PERSCOUV/ y$pop_18
pot_tx_couv <- y
rm(y, pot, pop_com_idf_18, com_idf20, ALLOC18)

# Répartition/part locale des différents groupes de revenu
load("fond_de_carte_idf_osm.RData")
rm(com_idf,deps,rivers, parc, paris, pc)
sel <- ALLOC_IDF_G
sel$REV_CLASSE2 <- sel$REV_CLASSE
sel$REV_CLASSE2[sel$REV_CLASSE2=="FRAG"] <- "MOY"
sel$REV_CLASSE2 <- as.factor(as.character((sel$REV_CLASSE2)))
sel$PERSCOUV_BR <- 0
sel$PERSCOUV_BR[sel$REV_CLASSE2=="BAS_REV"] <- sel$PERSCOUV_P1[sel$REV_CLASSE2=="BAS_REV"]
sel$PERSCOUV_MOY <- 0
sel$PERSCOUV_MOY[sel$REV_CLASSE2=="MOY"] <- sel$PERSCOUV_P1[sel$REV_CLASSE2=="MOY"]
sel$PERSCOUV_HAUT <- 0
sel$PERSCOUV_HAUT[sel$REV_CLASSE2=="HAUT"] <- sel$PERSCOUV_P1[sel$REV_CLASSE2=="HAUT"]
sel$PERSCOUV_INC <- 0
sel$PERSCOUV_INC[sel$REV_CLASSE2=="INC"] <- sel$PERSCOUV_P1[sel$REV_CLASSE2=="INC"]
sel <- sel[,c("PERSCOUV_P1","PERSCOUV_BR","PERSCOUV_MOY","PERSCOUV_HAUT","PERSCOUV_INC","X_18","Y_18")]
sel <- sel[!is.na(sel$X_18),]
sel <- st_as_sf(sel, coords = c(6,7))
st_crs(sel) <- 2154
idf <- st_transform(idf, 2154)
y <- create_grid(x = idf, res = 1000)
pot <- mcpotential(x = sel, y = y, 
                   var = c("PERSCOUV_P1","PERSCOUV_BR","PERSCOUV_MOY","PERSCOUV_HAUT","PERSCOUV_INC"), fun = "e",
                   span = 500, beta = 2, 
                   limit = 10000, ncl = 4)
pot <- as.data.frame(pot)
y <- cbind(y, pot)
y$tx_BR <- 100 * y$PERSCOUV_BR/ y$PERSCOUV_P1
y$tx_MOY <- 100 * y$PERSCOUV_MOY/ y$PERSCOUV_P1
y$tx_HAUT <- 100 * y$PERSCOUV_HAUT/ y$PERSCOUV_P1
y$tx_INC <- 100 * y$PERSCOUV_INC/ y$PERSCOUV_P1
pot_rev_local <- y
rm(y, pot, sel, ALLOC, ALLOC_IDF_G, idf)
save.image("chap3_density.RData")
rm(list = ls())

# La mobilité résidentielle des allocataires franciliens ----
#===============================================================#

load("ALLOCS_1819_ND_G_BAN.RData") # Import des données allocataires 2019 V1
# Taux de mobilité des foyers
foy_mob <- summary(ALLOC$MOBILE2)
pop_mob <- sum(ALLOC$PERSCOUV_P1[ALLOC$MOBILE2=="1"])
tx_mob_foy_19 <- round(nrow(ALLOC[ALLOC$MOBILE2=="1",])/nrow(ALLOC)*100,1)
tx_mob_pop_19 <- round(sum(ALLOC$PERSCOUV_P1[ALLOC$MOBILE2=="1"])/sum(ALLOC$PERSCOUV_P1)*100,1)

# Avec que les ND2
ALLOC_ND2 <- ALLOC[ALLOC$ND2=="OUI",]
tx_mob_foy_19_ND2 <- round(nrow(ALLOC_ND2[ALLOC_ND2$MOBILE2=="1",])/nrow(ALLOC_ND2)*100,1)
tx_mob_pop_19_ND2 <- round(sum(ALLOC_ND2$PERSCOUV_P1[ALLOC_ND2$MOBILE2=="1"])/sum(ALLOC_ND2$PERSCOUV_P1)*100,1)
rm(ALLOC_ND2)

# Tx mob annuel ----
freqev <- function(tab, P1, P2, seuil_BR,periode,  NoD2=F){
  idf <- c("75","77","78","91","92","93","94","95")
  colnames(tab) <- gsub(P1,"P1",colnames(tab))
  colnames(tab) <- gsub(P2,"P2",colnames(tab))
  tab$PRESFRES_P2 <- as.factor(tab$PRESFRES_P2)
  tab$ND2 <- "NON"
  tab$ND2[tab$PRESFRES_P2 %in% c("All.noyau dur sans action sociale","Bénéficiaires d'ARS seule sans action sociale")] <- "OUI"
  if(NoD2==T){
    tab <- tab[tab$ND2=="OUI",]
  }
  tab <- tab[substr(tab$NUMCOMDO_P1,1,2) %in% idf,]
  tab$count <- 1
  tab$si_isole_P1[is.na(tab$si_isole_P1)] <- 0 
  tab$si_isole_P1 <- as.factor(tab$si_isole_P1)
  tab$si_monop_P1[is.na(tab$si_monop_P1)] <- 0 
  tab$si_monop_P1 <- as.factor(tab$si_monop_P1)
  tab$si_couple_P1[is.na(tab$si_couple_P1)] <- 0 
  tab$si_couple_P1 <- as.factor(tab$si_couple_P1)
  tab$si_couenf_P1[is.na(tab$si_couenf_P1)] <- 0 
  tab$si_couenf_P1 <- as.factor(tab$si_couenf_P1)
  
  #tab$PERSCOUV <- as.numeric(tab$PERSCOUV)
  tab$MOBILE <- as.factor(tab$MOBILE)
  Indicateurs <- c("NB ALLOC IDF", #"POP ALLOC",
                   "NB ALLOC IDF ND2", #"POP ALLOC ND2",
                   "NB ALLOC MOB", #"POP COUV MOB",
                   "NB ALLOC MOB ND2", #"POP COUV MOB ND2",
                   "TX MOB ALLOC",#"TX MOB POP COUV",
                   "TX MOB ALLOC ND2",#"TX MOB POP ND2")
                   "PART ISOLE",
                   "TX MOB ISOLE",
                   "PART MONOP",
                   "TX MOB MONOP",
                   "PART COUPLE",
                   "TX MOB COUPLE",
                   "PART COUENF",
                   "TX MOB COUENF")
  Resultats <- c(sum(tab$count),#sum(tab$PERSCOUV),
                 sum(tab$count[tab$ND2=="OUI"]),#sum(tab$PERSCOUV[tab$ND2=="OUI"]),
                 sum(tab$count[tab$MOBILE != "STATIC"]), #sum(tab$PERSCOUV[tab$MOBILE != "STATIC"]),
                 sum(tab$count[tab$MOBILE != "STATIC" & tab$ND2=="OUI"]),# sum(tab$PERSCOUV[tab$MOBILE != "STATIC" & tab$ND2=="OUI"]),
                 round(sum(tab$count[tab$MOBILE != "STATIC"])/sum(tab$count)*100,2), #round(sum(tab$PERSCOUV[tab$MOBILE != "STATIC"])/sum(tab$PERSCOUV)*100,1),
                 round(sum(tab$count[tab$MOBILE != "STATIC" & tab$ND2=="OUI"])/sum(tab$count[tab$ND2=="OUI"])*100,2) ,# round(sum(tab$PERSCOUV[tab$MOBILE != "STATIC" & tab$ND2=="OUI"])/sum(tab$PERSCOUV[tab$ND2=="OUI"])*100,1),
                 round(sum(tab$count[tab$si_isole_P1 == "1"])/sum(tab$count)*100,2),
                 round(sum(tab$count[tab$si_isole_P1 == "1" & tab$MOBILE != "STATIC"])/sum(tab$count[tab$si_isole_P1 == "1"])*100,2),
                 round(sum(tab$count[tab$si_monop_P1 == "1"])/sum(tab$count)*100,2),
                 round(sum(tab$count[tab$si_monop_P1 == "1" & tab$MOBILE != "STATIC"])/sum(tab$count[tab$si_monop_P1 == "1"])*100,2),
                 round(sum(tab$count[tab$si_couple_P1 == "1"])/sum(tab$count)*100,2),
                 round(sum(tab$count[tab$si_couple_P1 == "1" & tab$MOBILE != "STATIC"])/sum(tab$count[tab$si_couple_P1 == "1"])*100,2),
                 round(sum(tab$count[tab$si_couenf_P1 == "1"])/sum(tab$count)*100,2),
                 round(sum(tab$count[tab$si_couenf_P1 == "1" & tab$MOBILE != "STATIC"])/sum(tab$count[tab$si_couenf_P1 == "1"])*100,2)
  )
  
  # Tableau de sortie
  res <- as.data.frame(cbind(Indicateurs,Resultats))
  colnames(res)[2] <- periode 
  return(res)
}

seuil2016 <- 1045
seuil2017 <- 1052
seuil2018 <- 1071
seuil2019 <- 1096
seuil20 <- 1105
load("Ev1620.RData") # Dénombrement des évènements familiaux, professionnels et résidentiels dans les données allcoataires de 2016 à 2020
res1617 <- freqev(Ev1617, "1216","1217",seuil16, "2017")
res1718 <- freqev(Ev1718, "1217","1218",seuil17, "2018")
res1819 <- freqev(Ev1819, "1218","1219",seuil18, "2019")
res1920 <- freqev(Ev1920, "1219","1220",seuil19, "2020")
res2021 <- freqev(e21, "1220","1221",seuil20, "2021")
events_an <- res1617 %>% left_join(res1718) %>% left_join(res1819) %>% left_join(res1920)%>% left_join(res2021)



coeff_correct_mobfoyer <- as.numeric(events_an[5,3]) /tx_mob_foy_19 # Mobilité annuelle des foyers corrigée
TX_MOB_ALLOC_CORREG <- c("TX MOB ALLOC CORREG", round(as.numeric(events_an[5,2:5])/coeff_correct_mobfoyer, 2))
events_an <- rbind(events_an, TX_MOB_ALLOC_CORREG)

rm(Ev1617, Ev1718, Ev1819, Ev1920, res1617, res1718, res1819, res1920)

# Destination des mobilités résidentielles des allocataires
Mobiles <- ALLOC[ALLOC$MOBILE2=="1",c("NUMCOMDO_18","NUMCOMDO_19","DEPTCD","DEPTPR", "PERSCOUV_P1")]
# retirer les départs à l'étranger
Mobiles <- Mobiles[!(substr(Mobiles$NUMCOMDO_19,1,2) %in% c("99","98")),]
meme_com_caf <- sum(Mobiles$PERSCOUV_P1[Mobiles$NUMCOMDO_18==Mobiles$NUMCOMDO_19])/sum(Mobiles$PERSCOUV_P1)
meme_dep_caf <- sum(Mobiles$PERSCOUV_P1[Mobiles$NUMCOMDO_18!=Mobiles$NUMCOMDO_19 & substr(Mobiles$NUMCOMDO_18,1,2)==substr(Mobiles$NUMCOMDO_19,1,2)])/sum(Mobiles$PERSCOUV_P1)
aut_dep_caf <- sum(Mobiles$PERSCOUV_P1[substr(Mobiles$NUMCOMDO_18,1,2)!=substr(Mobiles$NUMCOMDO_19,1,2) & substr(Mobiles$NUMCOMDO_19,1,2) %in% idf])/sum(Mobiles$PERSCOUV_P1)
hors_idf_caf <- sum(Mobiles$PERSCOUV_P1[!(substr(Mobiles$NUMCOMDO_19,1,2) %in% idf)])/sum(Mobiles$PERSCOUV_P1)

# Comparaison avec Migcom
FD_MIGCOM_2019 <- read.csv("FD_MIGCOM_2019.csv", sep=";") # Données du recensement sur le smobilités résidentielles
idf <- c("75","77","78","91","92","93","94","95")
IDF <- FD_MIGCOM_2019[substr(FD_MIGCOM_2019$DCRAN,1,2) %in% c("75","77","78","91","92","93","94","95"),]
rm(FD_MIGCOM_2019)
Tx_Mob_migcom19 <- round(sum(IDF$IPONDI[IDF$IRAN!=1])/sum(IDF$IPONDI)*100,2)
# COMAR 
sel <- IDF
sel1<-sel[sel$ARM!="ZZZZZ",]
sel1$COMAR<-sel1$ARM
sel2<-sel[sel$ARM=="ZZZZZ",]
sel2$COMAR<-paste(sel2$COMMUNE)
sel<-rbind(sel1,sel2)
IDF <- sel
remove(sel, sel1, sel2)
MOBCOM <- IDF[IDF$IRAN!=1,]
meme_comMIGCOM <- sum(MOBCOM$IPONDI[MOBCOM$DCRAN==MOBCOM$COMAR])/sum(MOBCOM$IPONDI)
meme_depMIGCOM <- sum(MOBCOM$IPONDI[MOBCOM$DCRAN!=MOBCOM$COMAR & substr(MOBCOM$DCRAN,1,2)==substr(MOBCOM$COMAR,1,2)])/sum(MOBCOM$IPONDI)
aut_depMIGCOM <- sum(MOBCOM$IPONDI[substr(MOBCOM$DCRAN,1,2)!=substr(MOBCOM$COMAR,1,2) & substr(MOBCOM$COMAR,1,2) %in% idf])/sum(MOBCOM$IPONDI)
hors_idfMIGCOM <- sum(MOBCOM$IPONDI[!(substr(MOBCOM$COMAR,1,2) %in% idf)])/sum(MOBCOM$IPONDI)
Dest_mob_caf_migcom <- rbind(c("Même com",meme_com_caf, meme_comMIGCOM),
                             c("Même com",meme_dep_caf, meme_depMIGCOM),
                             c("Autre dep idf",aut_dep_caf, aut_depMIGCOM),
                             c("Hors IDF",hors_idf_caf, hors_idfMIGCOM),
                             c("Notes", "Sans départs à l'étranger", "Dcran IDF"))
Dest_mob_caf_migcom <- as.data.frame(Dest_mob_caf_migcom)
colnames(Dest_mob_caf_migcom) <- c("Type mob","CAF FR6 18", "MIGCOM 18")
rm(IDF, MOBCOM, freqev, meme_com_caf, 
   meme_comMIGCOM,meme_dep_caf, meme_depMIGCOM,aut_dep_caf, 
   aut_depMIGCOM,hors_idf_caf, hors_idfMIGCOM, TX_MOB_ALLOC_CORREG)

# Tx mob loc ----

# Taux de mobilité résidentielle localisé de la population allocataire de référence. 
library(potential)
library(mapsf)
library(RColorBrewer)
library(sf)
library(dplyr)

# Préparation des données ====
VN <- st_read("Perimetres_VN.geojson") # périmètre des villes nouvelles
load("fond_de_carte_idf_osm.RData")
C_INSEE <- c("95127","95585","95018","78551","78361","78646","78517","92050","93008","94028","91477","91228","91223","77468","77284","77288","77379","77186")
noms <- c("Cergy","Sarcelles", "Argenteuil","Saint-Germain\n-en-Laye","Mantes-la-Jolie", "Versailles","Rambouillet","Nanterre","Bobigny","Créteil","Palaiseau","Evry","Etampes","Torcy","Meaux","Melun","Provins","Fontainebleau")
villes_idf <- as.data.frame(row.names = NULL, cbind(C_INSEE, noms))
villes_idf$ID <- as.character(1:18)
com_idf$C_INSEE <- as.character(com_idf$C_INSEE)
villes_idf <- right_join(com_idf, villes_idf)
villes_idf <- st_as_sf(villes_idf)
villes_idf <- st_centroid(villes_idf)
deps <- st_as_sf(deps)
idf <- c("75","77","78","91","92","93","94","95")
load("grille1k_cutIDF.RData")
grille <- grille_1kcut[,c(2,4)]
rm(grille_1kcut)
deps <- st_cast(deps,"LINESTRING")

## Construction de la table 
load("ALLOCS_1819_ND_G_BAN.RData")
tab <- ALLOC_IDF_G
tab$Foyers <- 1
tab$Foyers_mobiles <- 0
tab$Foyers_mobiles[tab$MOBILE2=="1"] <- tab$Foyers[tab$MOBILE2=="1"]
tab$Population <- as.numeric(tab$PERSCOUV_P1)
tab$Population_mobile <- 0
tab$Population_mobile[tab$MOBILE2=="1"]  <- tab$Population[tab$MOBILE2=="1"] 
sel <- tab[,c("id_carr_1k_18","Foyers", "Foyers_mobiles","Population","Population_mobile")]
summary(sel)
sel$id_carr_1k <- as.factor(sel$id_carr_1k_18)
sel <- aggregate(sel[ , c("Foyers", "Foyers_mobiles","Population","Population_mobile")], by = list(sel$id_carr_1k), FUN = sum)
colnames(sel) <- c("id_carr_1k","Foyers", "Foyers_mobiles","Population","Population_mobile")

# Jointure avec la géographie 
sel <- dplyr::left_join(grille,sel, by="id_carr_1k")
data <- as.data.frame(sel)
data <- data[,c(2:5)]
geom <- as.data.frame(st_coordinates(st_centroid(sel)))
sel <- cbind(geom,data)
rm(grille, geom, data)
colnames(sel)[1:2] <- c("x","y")

IDF <- st_read("IDF_regroupée.shp")
sel <- st_as_sf(sel, coords = c(1,2))
st_crs(sel) <- 2154
IDF <- st_transform(IDF, 2154)

IDF <- IDF[,c(3,6)]
# create a regular grid
y <- create_grid(x = sel, res = 1000)
# compute potentials
pot <- mcpotential(
  x = sel, y = y,
  var = c("Foyers", "Foyers_mobiles","Population","Population_mobile"),
  fun = "e", span = 2500,
  beta = 2, limit = 50000, 
  ncl = 2
)
dfLisse <- as.data.frame(pot)
dfLisse$tx_mob_foy = 100 * dfLisse$Foyers_mobiles / dfLisse$Foyers
dfLisse$tx_mob_pop = 100 * dfLisse$Population_mobile / dfLisse$Population
y <- cbind(y,dfLisse[,c(5,6)])
st_crs(y) <- 2154
temp <- st_intersection(IDF,y)

# Taux ====
bks_foy <- mf_get_breaks(x = temp$tx_mob_foy, breaks = "q6")
bks_pop <- mf_get_breaks(x = temp$tx_mob_pop, breaks = "q6")
#"quantile", nbreaks = 6
equipot_tx1 <- equipotential(y, var = "tx_mob_foy", breaks = bks_foy, mask = IDF)
equipot_tx2 <- equipotential(y, var = "tx_mob_pop", breaks = bks_pop, mask = IDF)
cols <- colorRampPalette(c("#eff3ff","#0059A8"))(6)
cols <- colorRampPalette(c("white","#0059A8"))(8)[2:7]
#08519c

# taux de mobilité des foyers 
mf_shadow(equipot_tx1)
mf_map(x = equipot_tx1, var = "min", type = "choro", 
       breaks = bks_foy, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 1,
       lwd = .2, leg_no_data = NA,col_na = NA,
       leg_pos = "topright", 
       leg_title = "Taux de mobilité résidentielle (%)", add=T)
mf_map(deps, col = "#808080",lwd=1.5, add=T)
#mf_label(x = villes_idf, var = "ID",cex = 0.6, halo = F, col = "black")
mf_label(x = villes_idf, var = "noms",cex = 0.7, halo = F, col = "black", overlap = F)
mf_title("Taux de mobilité résidentielle localisé des foyers allocataires")

# taux de mobilité de la population couverte 
mf_shadow(equipot_tx2)
mf_map(x = equipot_tx2, var = "min", type = "choro", 
       breaks = bks_pop, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 1,
       lwd = .2,  leg_no_data = NA,col_na = NA,
       leg_pos = "topright", 
       leg_title = "Taux de mobilité résidentielle (%)", add=T)
mf_map(deps, col = "#808080",lwd=1.5, add=T)
#mf_map(VN, col = NA,lwd=1.5, add=T)
#mf_label(x = villes_idf, var = "ID",cex = 0.6, halo = F, col = "black")
mf_label(x = villes_idf, var = "noms",cex = 0.7, halo = F, col = "black", overlap = F)
mf_title("Taux de mobilité résidentielle localisé de la population couverte")

rm(ALLOC, ALLOC_IDF_G, com_idf, dfLisse,grille, IDF, parc, paris, pc, pot, rivers, sel, y, temp, noms, idf, C_INSEE)


# Des mobilités de proximité ----

# Fréquence cumulée de la distance parcourue
Mobiles <- ALLOC[ALLOC$MOBILE2=="1" & !(substr(ALLOC$DEPTPR,1,2) %in% c("XX","ET",97)),c("ND2","DEPTPR","DIST_PARC_EUC")]
Mobiles$Dist_parc <- Mobiles$DIST_PARC_EUC/1000 # passage au km
# retirer les distances nulles
Mobiles <- Mobiles[Mobiles$Dist_parc != 0,]
# Retirer les distances aberantes
Mobiles <- Mobiles[Mobiles$Dist_parc < 5000,]
summary(Mobiles$Dist_parc)
# Création de la variable foyer
Mobiles$Foyers <- 1
Mobiles <- Mobiles[!(is.na(Mobiles$Dist_parc)),]
# Résumés graphiques (Fréquence cumulée)
dist_parc <- Mobiles[,c("Dist_parc","Foyers")]
summary(dist_parc$Dist_parc)
dist_parc_idf <- Mobiles[Mobiles$DEPTPR %in% idf,c("Dist_parc","Foyers")]
meanDistParcFR <- mean(dist_parc$Dist_parc)
meanDistParcIDF <- mean(dist_parc_idf$Dist_parc)
plot_dist_parc1 <- ggplot(dist_parc) +
  aes(x=Dist_parc,
      weight=Foyers) +
  geom_line(stat="ecdf", lwd=0.5) + scale_x_continuous(name="Distance parcourue (km)", breaks = c(0,1,5,10,25,50,100, 250,500,1000),trans = "sqrt", limits=c(0, 1002)) +
  scale_y_continuous(name="Fréquence cumulée", limits=c(0, 1), breaks = c(0,0.25,0.5, 0.75,1),labels = c("0%","25%","50%","75%","100%") ) +
  geom_vline(xintercept = mean(dist_parc$Dist_parc), color="#006699") +
  geom_vline(xintercept = mean(dist_parc_idf$Dist_parc),linetype="dotted", color="#006699") +
  theme_light() 
plot_dist_parc1
plot_dist_parc2 <- plot_dist_parc1 + geom_text(aes(75,0.75,label = "Moyenne", hjust = -1)) +
  geom_text(aes(9.4,0.85,label = "Moyenne en IDF", hjust = -1))
median_dist_parc <- median(dist_parc$Dist_parc)
median_dist_parc_idf <- median(dist_parc_idf$Dist_parc)
rm(Mobiles, dist_parc, dist_parc_idf)

# Le desserrement résidentiel des allocataires franciliens ----

# A. Fonction de densité de l’éloignement à Paris suite au déménagement
library(scales)
Mobiles <- ALLOC_IDF_G[ALLOC_IDF_G$MOBILE2=="1" & !(is.na(ALLOC_IDF_G$X_19)),c("NUMCOMDO_18","LILI4ADR_18","NUMCOMDO_19","LILI4ADR_19","ND2","EVOL_DIST_PARIS_EUC")]
Mobiles <- Mobiles[Mobiles$LILI4ADR_18 != Mobiles$LILI4ADR_19,]
summary(Mobiles$EVOL_DIST_PARIS_EUC)
#Mobiles <- Mobiles[Mobiles$EVOL_DIST_PARIS_EUC<200000 & Mobiles$EVOL_DIST_PARIS_EUC !=0,] # retirer la valeur aberrante
Mobiles <- Mobiles[!is.na(Mobiles$EVOL_DIST_PARIS_EUC),]
Mobiles$EVOL_DIST_PARIS_EUC <- Mobiles$EVOL_DIST_PARIS_EUC /1000
asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}
bks <- c(-c(100,25,10,5,2.5,1),c(0,1,2.5,5,10,25,100))
dens_dist_paris <- ggplot(Mobiles, aes(x=EVOL_DIST_PARIS_EUC))+
  geom_density(color="darkgrey", fill="#ECECEC", alpha=0.4) + scale_x_continuous(name="Evolution de la distance au centre de Paris (km)", trans = "asinh",breaks = bks) +
  scale_y_continuous(name="Densité de probabilité")+
  geom_vline(xintercept = mean(Mobiles$EVOL_DIST_PARIS_EUC), color="#006699") +
  theme_light() 
dens_dist_paris
meanEvolDistParisEucIDF <- mean(Mobiles$EVOL_DIST_PARIS_EUC)
rm(Mobiles)

# Solde migratoire interne à l’IDF des allocataires par intercommunalité 

load("GEO_IDF_IPR.RData")
epci_idf <- ecpi_idf
rm(ecpi_idf)
com_epci <- read.csv("com_epci.csv", row.names=1)
com_epci$CODEGEO <- as.character(com_epci$CODEGEO)
paris <- com_epci[!(com_epci$CODEGEO %in% com_idf$INSEE_COM),]
colnames(paris) <- c("CODEGEO","EPCI")
temp <- com_idf %>% as.data.frame()
temp <- temp[,c(1,3)]
colnames(temp) <- c("CODEGEO","EPCI")
com_epci <- rbind(temp, paris)
rm(temp, paris)

data_caf_19 <- ALLOC_IDF_G %>%  dplyr::filter(MOBILE2=="1" & NUMCOMDO_18!=NUMCOMDO_19) %>% dplyr::select(NUMCOMDO_18, NUMCOMDO_19,PERSCOUV_P1)
colnames(com_epci) <- c("code","epci_P1")
data_caf_19 <- left_join(data_caf_19, com_epci, by=c("NUMCOMDO_18"="code"))
colnames(com_epci) <- c("code","epci_P2")
data_caf_19 <- left_join(data_caf_19, com_epci, by=c("NUMCOMDO_19"="code"))
data_caf_19$count <- as.numeric(data_caf_19$PERSCOUV_P1)
tab <- aggregate(data_caf_19[ , c("PERSCOUV_P1")], by = list(data_caf_19$epci_P1,data_caf_19$epci_P2), FUN = sum)
colnames(tab) <- c("CODEi","CODEj","Fij")
tab <- tab[tab$CODEi!=tab$CODEj,]
tab$Fij <- as.double(tab$Fij)
ent <- aggregate(tab[,c("Fij")], by = list(tab$CODEj), FUN = sum)
colnames(ent) <- c("EPCI","ent")
sort <- aggregate(tab[,c("Fij")], by = list(tab$CODEi), FUN = sum)
colnames(sort) <- c("EPCI","sort")
soldes <- left_join(ent, sort)
soldes$sold <- soldes$ent - soldes$sort
soldes$sold_rel <- soldes$ent / soldes$sort
soldes$Signe <- "Négatif"
soldes$Signe[soldes$sold >= 0] <- "Positif"
soldes$Signe <- as.factor(soldes$Signe)
soldes$Signe  <- factor(soldes$Signe , levels = c("Positif","Négatif"))
soldes$Solde <- abs(soldes$sold)
rm(sort, ent, tab, data_caf_19, com_epci, com_idf, dep_idf, bks, ALLOC, ALLOC_IDF_G, asinh_trans, epci_idf)
save.image("chap3_mobility.RData")
rm(list = ls())




