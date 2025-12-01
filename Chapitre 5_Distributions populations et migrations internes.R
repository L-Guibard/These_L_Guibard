# Distribution spatiale des groupes de revenus et évolution annuelle 

library(potential)
library(mapview)
library(RColorBrewer)


# Préparation des données ====
load("ALLOCS_1819_ND_G_BAN.RData")
VN <- st_read("Perimetres_VN.geojson")
load("fond_de_carte_idf_osm.RData")
C_INSEE <- c("95127","95585","95018","78551","78361","78646","78517","92050","93008","94028","91477","91228","91223","77468","77284","77288","77379","77186")
noms <- c("Cergy","Sarcelles", "Argenteuil","Saint-Germain-en-Laye","Mantes-la-Jolie", "Versailles","Rambouillet","Nanterre","Bobigny","Créteil","Palaiseau","Evry","Etampes","Torcy","Meaux","Melun","Provins","Fontainebleau")
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
Tab <- ALLOC_IDF_G[,c("id_carr_1k_18","id_carr_1k_19", "PERSCOUV_P1","REV_CLASSE")]
Tab$Pop <- as.numeric(Tab$PERSCOUV_P1)
Tab$Pop_Br <- as.numeric(Tab$PERSCOUV_P1)
Tab$Pop_Br[Tab$REV_CLASS!="BAS_REV"] <- 0
Tab$Pop_F <- as.numeric(Tab$PERSCOUV_P1)
Tab$Pop_F[Tab$REV_CLASS != "FRAG"] <- 0
Tab$Pop_M <- as.numeric(Tab$PERSCOUV_P1)
Tab$Pop_M[Tab$REV_CLASS != "MOY"] <- 0
Tab$Pop_FM <- as.numeric(Tab$PERSCOUV_P1)
Tab$Pop_FM[Tab$REV_CLASS != "FRAG" &Tab$REV_CLASS != "MOY"] <- 0
Tab$Pop_EL <- as.numeric(Tab$PERSCOUV_P1)
Tab$Pop_EL[Tab$REV_CLASS!="HAUT"] <- 0
Tab$Count <- 1
Tab18 <- Tab[,c(1,11,5:10)]
colnames(Tab18) <- c("id_carr_1k","Count18", "POP_18","POP_BR18","POP_F18","POP_M18","POP_FM18","POP_EL18")
Tab19 <- Tab[,c(2,11,5:10)]
colnames(Tab19) <- c("id_carr_1k","Count19","POP_19","POP_BR19","POP_F19","POP_M19","POP_FM19","POP_EL19")
rm(Tab)
Tab18$id_carr_1k <- as.character(Tab18$id_carr_1k)
Tab18$id_carr_1k <- as.factor(Tab18$id_carr_1k)
Tab19$id_carr_1k <- as.character(Tab19$id_carr_1k)
Tab19$id_carr_1k <- as.factor(Tab19$id_carr_1k)

Tab18 <- aggregate(Tab18[ , c("Count18","POP_18","POP_BR18","POP_F18","POP_M18","POP_FM18","POP_EL18")], by = list(Tab18$id_carr_1k), FUN = sum)
colnames(Tab18) <- c("id_carr_1k","Count18","POP_18","POP_BR18","POP_F18","POP_M18","POP_FM18","POP_EL18")
Tab19 <- aggregate(Tab19[ , c("Count19","POP_19","POP_BR19","POP_F19","POP_M19","POP_FM19","POP_EL19")], by = list(Tab19$id_carr_1k), FUN = sum)
colnames(Tab19) <- c("id_carr_1k","Count19","POP_19","POP_BR19","POP_F19","POP_M19","POP_FM19","POP_EL19")
sel <- dplyr::full_join(Tab18,Tab19, by="id_carr_1k")
rm(Tab18, Tab19)

# Jointure avec la géographie 
sel <- dplyr::left_join(grille,sel, by="id_carr_1k")
data <- as.data.frame(sel)
data <- data[,c(2:15)]
geom <- as.data.frame(st_coordinates(st_centroid(sel)))
sel <- cbind(geom,data)
rm(grille, geom, data)
colnames(sel)[1:2] <- c("x","y")

IDF <- st_read("/Users/luc/Desktop/Thèse/Data/Geo/Grilles/IDF/IDF_regroupée.shp")
sel <- st_as_sf(sel, coords = c(1,2))
IDF <- st_transform(IDF, 2154)
st_crs(sel) <- 2154
IDF <- IDF[,c(3,6)]


# create a regular grid
y <- create_grid(x = sel, res = 1000)
# compute potentials
pot <- mcpotential(
  x = sel, y = y,
  var = c("POP_18","POP_19","POP_BR18","POP_BR19","POP_FM18","POP_FM19","POP_EL18","POP_EL19"),
  fun = "e", span = 3000,
  beta = 2, limit = 50000, 
  ncl = 2
)


dfLisse <- as.data.frame(pot)
dfLisse$txpov18 = 100 * dfLisse$POP_BR18 / dfLisse$POP_18
dfLisse$txpov19 = 100 * dfLisse$POP_BR19 / dfLisse$POP_19
dfLisse$evolBR = dfLisse$txpov19 - dfLisse$txpov18
dfLisse$txfm18 = 100 * dfLisse$POP_FM18 / dfLisse$POP_18
dfLisse$txfm19 = 100 * dfLisse$POP_FM19 / dfLisse$POP_19
dfLisse$evolFM = dfLisse$txfm19 - dfLisse$txfm18
dfLisse$txEL18 = 100 * dfLisse$POP_EL18 / dfLisse$POP_18
dfLisse$txEL19 = 100 * dfLisse$POP_EL19 / dfLisse$POP_19
dfLisse$evolEL = dfLisse$txEL19 - dfLisse$txEL18
y <- cbind(y,dfLisse[,c(9:17)])
st_crs(y) <- 2154
temp <- st_intersection(IDF,y)

# Taux ====
bksBR2 <- mf_get_breaks(x = temp$txpov18, breaks = "quantile", nbreaks = 6)
bksFM2 <- mf_get_breaks(x = temp$txfm18, breaks = "quantile", nbreaks = 6)
bksEL2 <- mf_get_breaks(x = temp$txEL18, breaks = "quantile", nbreaks = 6)
equipot_tx1 <- equipotential(y, var = "txpov18", breaks = bksBR2, mask = IDF)
equipot_tx2 <- equipotential(y, var = "txfm18", breaks = bksFM2, mask = IDF)
equipot_tx3 <- equipotential(y, var = "txEL18", breaks = bksEL2, mask = IDF)
cols <- colorRampPalette(c("#eff3ff","#0059A8"))(6)
cols <- colorRampPalette(c("white","#0059A8"))(8)[2:7]
#08519c

# Pauvres 
mf_map(x = equipot_tx1, var = "min", type = "choro", 
       breaks = bksBR2, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 1,
       lwd = .2, 
       leg_pos = "topright", 
       leg_title = "Bas revenus")
mf_map(deps, col = "#808080",lwd=1.5, add=T)
mf_label(x = villes_idf, var = "ID",cex = 0.6, halo = F, col = "black")

mf_map(x = equipot_tx2, var = "min", type = "choro", 
       breaks = bksFM2, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 1,
       lwd = .2, 
       leg_pos = "topright", 
       leg_title = "Revenus moyens")
mf_map(deps, col = "#808080",lwd=1.5, add=T)
plot(st_geometry(villes_idf), add=T,cex = 0.6, pch=16, col =  "#464646")

mf_map(x = equipot_tx3, var = "min", type = "choro", 
       breaks = bksEL2, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 1,
       lwd = .2, 
       leg_pos = "topright", 
       leg_title = "Revenus moyens")
mf_map(deps, col = "#808080",lwd=1.5, add=T)
plot(st_geometry(villes_idf), add=T,cex = 0.6, pch=16, col =  "#464646")

#Evol des parts sous l'effet des mobilités internes ==== 

evols <- c(temp$evolBR,temp$evolFM,temp$evolEL)
bks <- mf_get_breaks(x = evols, breaks = "quantile",nbreaks = 4)
#bks <- mf_get_breaks(x = evols, breaks = "q6")
#bks <- c(min(evols), -0.25,0, 0.25, max(evols))
equipot1 <- equipotential(y, var = "evolBR", breaks = bks, mask = IDF)
equipot2 <- equipotential(y, var = "evolFM", breaks = bks, mask = IDF)
equipot3 <- equipotential(y, var = "evolEL", breaks = bks, mask = IDF)


cols <- colorRampPalette(c("#0059A8","white","#C9A42F"))(6)[2:5]

mf_map(x = equipot1, var = "min", type = "choro", 
       breaks = bks, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 2,
       lwd = .2, 
       leg_pos = "topright", 
       leg_title = "Bas revenus")
mf_map(deps, col = "#808080",lwd=1.5, add=T)
plot(st_geometry(villes_idf), add=T,cex = 0.6, pch=16, col =  "#464646")

mf_map(x = equipot2, var = "min", type = "choro", 
       breaks = bks, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 2,
       lwd = .2, 
       leg_pos = "topright", 
       leg_title = "Revenus moyens")
mf_map(deps, col = "#808080",lwd=1.5, add=T)
plot(st_geometry(villes_idf), add=T,cex = 0.6, pch=16, col =  "#464646")

mf_map(x = equipot3, var = "min", type = "choro", 
       breaks = bks, 
       pal = cols,
       border = "#121725", 
       leg_val_rnd = 2,
       lwd = .2, 
       leg_pos = "topright", 
       leg_title = "Revenus plus élevés")
mf_map(deps, col = "#808080",lwd=1.5, add=T)
plot(st_geometry(villes_idf), add=T,cex = 0.6, pch=16, col =  "#464646")
mf_scale()
