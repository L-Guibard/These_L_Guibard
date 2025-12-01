# Code chap 5 part 1 - calcul des indicateurs d'accessibilité aux transports, à l'emploi et aux équipements

library(sf)
library(potential)
library(mapsf)
library(dplyr)
library(readxl)
library(readr)


# Périmètres d'étude et paramètre ----

setwd("Periph_chap5")
pageZ <- readRDS("pageZ.rds")
idf_regroup <- st_read("IDF_regroupée.shp")
idf_buffer <- st_buffer(idf_regroup, dist = 35000) %>% select(1,6) # on ne garde que les commune dans le périmètre d'interaction pris en compte
villes_idf2 <- readRDS("villes_idf2.rds")

# Desserte en transports en commun ----

y <- create_grid(x = idf_regroup, res = 200)
cellSize <- 200
grid_200 <- (st_bbox(y) + cellSize/2*c(-1,-1,1,1)) %>%
  st_make_grid(cellsize=c(cellSize, cellSize)) %>% st_sf()

bus_AU1 <- sf::st_read("bus_AU1_osm.shp")%>% st_transform(2154) # arrets de bus (données OSM)
bus_AU1$bus <- 1
TER_AU1 <- sf::st_read("TER_AU1_osm.shp")%>% st_transform(2154) # station TER (données OSM)
TER_AU1$TER <- 2
metro <- sf::st_read("Stations_metro_STIF.shp")%>% st_transform(2154) 
metro$metro <- 2
tram <- sf::st_read("Stations_tram_STIF.shp")%>% st_transform(2154)
tram$tram <- 2
rer <- sf::st_read("Stations_RER_STIF.shp")%>% st_transform(2154)
rer$rer <- 2
y$pot_bus <- mcpotential(x = bus_AU1, y = y, 
                     var = "bus", fun = "e",
                     span = 500, beta = 2, 
                     limit = 1250, ncl = 6)

#y$boolean_bus <- 0
#y$boolean_bus[y$pot_bus>=1] <- 1
y$pot_TER <- mcpotential(x = TER_AU1, y = y, 
                               var = "TER", fun = "e",
                               span = 1250, beta = 2, 
                               limit = 3000, ncl = 6)
y$pot_metro <- mcpotential(x = metro, y = y, 
                               var = "metro", fun = "e",
                               span = 1250, beta = 2, 
                               limit = 3000, ncl = 6)
y$pot_tram <- mcpotential(x = tram, y = y, 
                               var = "tram", fun = "e",
                               span = 1250, beta = 2, 
                               limit = 3000, ncl = 6)
y$pot_rer <- mcpotential(x = rer, y = y, 
                                var = "rer", fun = "e",
                                span = 1250, beta = 2, 
                                limit = 3000, ncl = 6)
temp <- as.data.frame(y[,5:9]) %>% select(-6)
y$score_TEC <- rowSums(temp)
rm(temp)

# Sauvegarde finale et intersection table alloc
grid_200$score_TEC <- y$score_TEC
grid_200$ID <- y$ID
yb <- st_intersection(y, idf_regroup)
grid_200_TEC <- grid_200[grid_200$ID %in% yb$ID,]
#mf_map(grid_200, var="score_TEC", type="choro", border=NA) # vérif 
saveRDS(grid_200_TEC, "grid_200_TEC.rds")
rm(yb)

# Export carte de résultat avec equipotential
#bks <- mf_get_breaks(x = y$score_TEC, nbreaks = 10, breaks="fisher")
bks <- c(0,0.5,1,2,5,10,20,40,80, max(y$score_TEC))
iso <- equipotential(x = y, var = "score_TEC", breaks = bks, mask = idf_regroup)

pageZ(format = "portrait", output = "svg", name = "access_TEC")
mf_map(x = iso, var = "min", type = "choro", 
       breaks =  bks, 
       pal = c("lightgrey",rev(hcl.colors(8, 'Teal'))),
       lwd = .2,
       border = "#121725", 
       leg_val_rnd = 0,
       leg_pos = 'bottom',
       leg_horiz = TRUE, 
       leg_box_border = NA,
       leg_title = "Score")
mf_base(idf_regroup,col = NA, border = "#f7f7f7", lwd = 1, add = T)
mf_label(villes_idf2, var = "Noms", col = "white", overlap = F)
mf_title("Niveau d'accès potentiel aux transports en commun", bg = NA, fg = "black")
mf_credits(txt = "Fonction d'interaction spatiale : exponentielle, beta 2;\nMétro, tram, RER, TER : bande passante 1250m, limite 3000m, pondération 2 ; Bus : bande passante 500m, limite 1250m, pondération 1.\nMapsf 0.8.0 ; Potential 0.2.0")
mf_scale()
dev.off()
#Sources: contributeurs OpenStreetMap, CC BY SA 2019; Ile-de-France Mobilités 2019


# Accès aux emplois ----
emploi_com <- readr::read_delim("base-cc-emploi-pop-active-2018.CSV",  # RP2018
                                ";", escape_double = FALSE, trim_ws = TRUE)
emploi_com <- emploi_com[,c("CODGEO","P18_ACT15P","P18_EMPLT")]
emploi_com <- emploi_com[emploi_com$CODGEO!="75056",]
idf_buffer <- st_buffer(idf_regroup, dist = 35000) %>% select(1,6) # on ne garde que les ommune dans le périmètre d'interaction pris en compte
com <- st_read("com_2021.geojson") %>% select(com_arm_code,geometry) %>% st_centroid() %>% st_transform(2154)
com <- st_intersection(com, idf_buffer)
com$com_arm_code <- as.character(com$com_arm_code)
summary(com$com_arm_code %in% emploi_com$CODGEO)
emploi_com <- left_join(com,emploi_com, by=c("com_arm_code"="CODGEO"))

y2 <- create_grid(x = idf_regroup, res = 1000)
cellSize <- 1000
grid_1k <- (st_bbox(y2) + cellSize/2*c(-1,-1,1,1)) %>%
  st_make_grid(cellsize=c(cellSize, cellSize)) %>% st_sf()


y2$pot_emploi <- mcpotential(x = emploi_com, y = y2, # span 5km
                         var = c("P18_EMPLT"),
                         fun = "e",
                         span = 5000, beta = 2, 
                         limit = 30000, ncl = 6)
y2$pot_pop <- mcpotential(x = emploi_com, y = y2, # span 5km
                             var = c("P18_ACT15P"),
                             fun = "e",
                             span = 5000, beta = 2, 
                             limit = 30000, ncl = 6)
y2$Tx_emp_pot <- y2$pot_emploi/(y2$pot_pop/1000) # Nombre d'emploi potentiellement accessible par habitant en age de travailler

# Sauvegarde dans la grille pour intersection
grid_1k$ID <- y2$ID
grid_1k$pot_emploi <- y2$pot_emploi
grid_1k$pot_pop <- y2$pot_pop
grid_1k$Tx_emp_pot <- y2$Tx_emp_pot

# Visualisation du resultat avec equipotential
bks <- mf_get_breaks(x = y2$pot_emploi,nbreaks=8, breaks="em")
iso <- equipotential(x = y2, var = "pot_emploi", breaks = bks, mask = idf_regroup)

pageZ(format = "portrait", output = "svg", name = "access_emploi_abso")
mf_map(x = iso, var = "min", type = "choro", 
       breaks =  bks, 
       pal = rev(hcl.colors(length(bks), 'Teal')),
       lwd = .2,
       border = "#121725", 
       leg_val_rnd = 0,
       leg_pos = 'bottom',
       leg_horiz = TRUE, 
       leg_box_border = NA,
       leg_title = "Nombre d'emplois potentiellement accessibles depuis le domicile")
mf_base(idf_regroup,col = NA, border = "#f7f7f7", lwd = 1, add = T)
mf_label(villes_idf2, var = "Noms", col = "white", overlap = F)
mf_title("Accès potentiel à l'emploi", bg = NA, fg = "black")
mf_credits(txt = "Fonction d'interaction spatiale : exponentielle, beta 2, bande passante 5 km, limite 30 km.\nMapsf 0.8.0 ; Potential 0.2.0")
mf_scale()
dev.off()

bks <- mf_get_breaks(x = y2$Tx_emp_pot,nbreaks=8, breaks="fisher")
iso <- equipotential(x = y2, var = "Tx_emp_pot", breaks = bks, mask = idf_regroup)

pageZ(format = "portrait", output = "svg", name = "access_emploi_rela")
mf_map(x = iso, var = "min", type = "choro", 
       breaks =  bks, 
       pal = rev(hcl.colors(length(bks), 'Teal')),
       lwd = .2,
       border = "#121725", 
       leg_val_rnd = 0,
       leg_pos = 'bottom',
       leg_horiz = TRUE, 
       leg_box_border = NA,
       leg_title = "Nombre potentiel d'emploi pour 1000 actifs")
mf_base(idf_regroup,col = NA, border = "#f7f7f7", lwd = 1, add = T)
mf_label(villes_idf2, var = "Noms", col = "white", overlap = F)
mf_title("Accès potentiel à l'emploi", bg = NA, fg = "black")
mf_credits(txt = "Fonction d'interaction spatiale : exponentielle, beta 2, bande passante 5 km, limite 30 km.\nMapsf 0.8.0 ; Potential 0.2.0")
mf_scale()
dev.off()


# Accès aux équipements ----

Gammes18 <- read_excel("gammes_2018.xlsx") # Insee BP 2018
# Pour annexe équipements gamme supérieure
list_sup <- Gammes18[Gammes18$gamme=="supérieure",c(1,5)]

bpe18_ensemble <- read_delim("bpe18_ensemble_xy.csv",";")
bpe18_loisirs <- read_delim("bpe18_sport_loisir_xy.csv",";")
bpe18_enseignement <- read_delim("bpe18_enseignement_xy.csv",";")
bpe18_ensemble <- bpe18_ensemble[,c(6:9)]
bpe18_loisirs <- bpe18_loisirs[,c(6,9:11)]
bpe18_enseignement <- bpe18_enseignement[,c(6,14:16)]
bpe18 <- rbind(bpe18_ensemble, bpe18_loisirs, bpe18_enseignement)
rm(bpe18_enseignement, bpe18_ensemble, bpe18_loisirs)
bpe18 <- left_join(bpe18, Gammes18[,c(1,6)], by = c("TYPEQU" = "code équipement")) #ajout colonne - c'est normal qu'il y ait des NA car tous les éqpts ne sont pas comptés dans les 3 gammes
rm(Gammes18)

# Part des équipements sans géolocalisation 
# summary(as.factor(bpe18$QUALITE_XY))
# summary(as.factor(tempSup$QUALITE_XY))
# round(447203/(447203+2308585)*100,2) # Ensemble de la base BPE :16,2%
# tempSup <- bpe18[bpe18$gamme=="supérieure" & !is.na(bpe18$gamme),]
# summary(is.na(tempSup$LAMBERT_X))
# round(456/(456+148860)*100,2) # Gamme supérieure : moins de 1%
# rm(tempSup)

bpe18 <- na.exclude(bpe18) # On retire les valeurs manquantes (hors gamme, ou sans coordonnées)
bpe18 <- st_as_sf(bpe18, coords = c(2,3))
st_crs(bpe18) <- 2154
bpe18$TYPEQU <- as.character(bpe18$TYPEQU)
bpe18_sup <- bpe18[bpe18$gamme=="supérieure",] # 148860 équipements géolocalisés pris en compte en France

# Dénombrement en IDF
idf_regroup <- st_read("IDF_regroupée.shp")
denomb_IDF <- st_intersection(bpe18_sup,idf_regroup) 
round(nrow(denomb_IDF)/nrow(bpe18_sup)*100,2) # dont 19,2% (28568) en IDF. 

types_sup <- levels(as.factor(bpe18_sup$TYPEQU))
# Liste des équipements de la gamme supérieure

# Calcul de la distance minimale aux équipements supérieurs de chaque type 
print(types_sup)
for(j in types_sup){
  equips <- bpe18_sup[bpe18_sup$TYPEQU == j,]
  index <- st_nearest_feature(y2, equips)
  coords <- st_coordinates(equips)
  equips$x <- coords[,1]
  equips$y <- coords[,2]
  res_sup_i <- sqrt((y2$COORDX[1] - equips$x[index[1]])^2+(y2$COORDY[1] - equips$y[index[1]])^2)
  for(i in 2:length(index)){
    res_sup_i[i] <- sqrt((y2$COORDX[i] - equips$x[index[i]])^2+(y2$COORDY[i] - equips$y[index[i]])^2)
  }
  y2$res1 <- res_sup_i
  colnames(y2)[length(y2)] <- paste(j)
  print(j)
}
temp <- as.data.frame(y2) %>% select(all_of(types_sup))
temp$mean_sup <- rowSums(temp)/length(types_sup)
y2$EQUIP_mean_sup <- temp$mean_sup
y2$EQUIP_mean_sup_1k <- y2$EQUIP_mean_sup/1000
rm(temp)

# ajout à la grille et export pour intersection spatiale
grid_1k$EQUIP_mean_sup <- y2$EQUIP_mean_sup
yb <- st_intersection(y2, idf_regroup)
grid_1k <- grid_1k[grid_1k$ID %in% yb$ID,]
#mf_map(grid_1k, var="EQUIP_mean_sup", type="choro", border=NA) # vérif 
saveRDS(grid_1k, "grid_1k_emploi_equip.rds")
rm(yb)

# carte accès équipements

bks <- mf_get_breaks(x = y2$EQUIP_mean_sup_1k, nbreaks=8, breaks="fisher")
bks <- c(min(y2$EQUIP_mean_sup_1k),1,2,4,6,8,10,12,max(y2$EQUIP_mean_sup_1k))
iso <- equipotential(x = y2, var = "EQUIP_mean_sup_1k", breaks = bks, mask = idf_regroup)

pageZ(format = "portrait", output = "svg", name = "access_equip")
mf_map(x = iso, var = "min", type = "choro", 
       breaks =  bks, 
       pal = 
       lwd = .2,
       border = "#121725", 
       leg_val_rnd = 0,
       leg_pos = 'bottom',
       leg_horiz = TRUE, 
       leg_box_border = NA,
       leg_title = "Distance (km)")
mf_base(idf_regroup,col = NA, border = "#f7f7f7", lwd = 1, add = T)
mf_label(villes_idf2, var = "Noms", col = "white", overlap = F)
mf_title("Distance moyenne aux équipements de la gamme supérieure", bg = NA, fg = "black")
mf_credits(txt = "Mapsf 0.8.0 ; Potential 0.2.0")
mf_scale()
dev.off()



