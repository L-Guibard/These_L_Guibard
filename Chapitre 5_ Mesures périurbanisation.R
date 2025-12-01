# Code chap 6 part 2 - intersection et figures de résultat

library(sf)
library(potential)
library(mapsf)
library(dplyr)
library(readxl)
library(readr)

# Paramètres ----

setwd("Periph_chap5")
pageZ <- readRDS("pageZ.rds")

# IMPORT 1 ----
load("ALLOCS_1819_ND_G_BAN_2.RData")

# VERIF TABLES ----
# Alloc = tous les franciliens au départ, on a regéolocalisé au maximum (FR métro) : on utilise
# summary(ALLOC$ND2)
# summary(as.factor(ALLOC$DEPTCD))
# summary(as.factor(ALLOC$DEPTPR))
# summary(ALLOC$X_18)
# summary(ALLOC$X_19)
# ALLOC_IDF_G = que ceux qui sont restés en IDF et qu'on a géolocalisés, y compris non ND2 : on utilise pas
# summary(ALLOC_IDF_G$ND2)
# summary(as.factor(ALLOC_IDF_G$DEPTCD))
# summary(as.factor(ALLOC_IDF_G$DEPTPR))
# summary(ALLOC_IDF_G$X_18)
# summary(ALLOC_IDF_G$X_19)
rm(ALLOC_IDF_G)

# INTERSECTIONS ---- 

# Préparation des grilles
grid_200 <- readRDS("grid_200_TEC.rds")
grid_200 <- grid_200[,-3]
plot(grid_200$geometry) # on a bien que l'IDF
grid_1k <- readRDS("grid_1k_emploi_equip.rds")
grid_1k <- grid_1k[,-c(2,4)]
plot(grid_1k$geometry)  # on a bien que l'IDF

# Période 1 
sel <- ALLOC
sel <- sel[!(is.na(sel$X_18)),]
sel <- sel[!(is.na(sel$Y_18)),]
sel <- st_as_sf(sel[,c("IDUNI","X_18","Y_18")],coords = c("X_18","Y_18"))
st_crs(sel) <- 2154
colnames(grid_200) <- c("geometry",  "score_TEC_18")
temp1 <- st_intersection(grid_200, sel)
temp1 <- as.data.frame(temp1)
temp1 <- temp1[,-c(3)]
colnames(grid_1k) <- c("geometry", "pot_emploi_18", "Tx_emp_pot_18","EQUIP_mean_sup_18")
temp2 <- st_intersection(grid_1k, sel)
temp2 <- as.data.frame(temp2)
temp2 <- temp2[,-c(5)]

# Période 2
sel <- ALLOC
sel <- sel[!(is.na(sel$X_19)),]
sel <- sel[!(is.na(sel$Y_19)),]
sel <- st_as_sf(sel[,c("IDUNI","X_19","Y_19")],coords = c("X_19","Y_19"))
st_crs(sel) <- 2154
colnames(grid_200) <- c("geometry",  "score_TEC_19")
temp3 <- st_intersection(grid_200, sel)
temp3 <- as.data.frame(temp3)
temp3 <- temp3[,-c(3)]
colnames(grid_1k) <- c("geometry", "pot_emploi_19", "Tx_emp_pot_19","EQUIP_mean_sup_19")
temp4 <- st_intersection(grid_1k, sel)
temp4 <- as.data.frame(temp4)
temp4 <- temp4[,-c(5)]
rm(grid_1k, grid_200, sel)

# Ajout des indiracteurs à la table ALLOC 
ALLOC <- ALLOC %>% 
  left_join(temp1) %>% 
  left_join(temp3) %>% 
  left_join(temp2) %>% 
  left_join(temp4)
rm(temp1, temp2, temp3, temp4)

# Calcul des indicateurs d'évolution 

ALLOC$evol_pot_emploi <- ALLOC$pot_emploi_19 - ALLOC$pot_emploi_18 # Valable en IDF (NB)
ALLOC$evol_tx_emploi <- ALLOC$Tx_emp_pot_19 - ALLOC$Tx_emp_pot_18 # Valable en IDF (NB/1000 actifs)
ALLOC$evol_acces_TEC <- ALLOC$score_TEC_19 - ALLOC$score_TEC_18 # Valable en IDF (Points)
ALLOC$evol_acces_equip_sup <- ALLOC$EQUIP_mean_sup_19 - ALLOC$EQUIP_mean_sup_18 # Valable en IDF (Mètres)
summary(ALLOC$evol_acces_equip_sup) # vérif (ok)
summary(ALLOC$EVOL_MEAN_SUP)

# Sauvegarde de la table pour chap 5
save.image("chap5_table_allocs.RData")

# RESULTATS ----
load("chap5_table_allocs.RData")







