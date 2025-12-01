# Figures périurbanisation - Chapitre 5 


library(potential)
library(mapview)
library(RColorBrewer)
library(units)
library(sf)
library(osmdata)
library(spatstat)
library(maptools)
library(raster)
library(mapsf)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(jtools)
library(rms)
library(DescTools)
library(mapsf)
library(hrbrthemes)
library(ggpubr)
library(grid)
library(gridExtra)
library(tidyverse)
library(data.table)
library(readr)
library(rsconnect)
library(fisheye)
library(pracma)
library(viridis)
library(egg)


# Import et sélection IDF préparation ----
# load("chap5_table_allocs.RData")
# sel <- ALLOC
# sel <- sel %>% filter(!duplicated(sel$IDUNI))
# sum(sel$PERSCOUV_P1[sel$MOBILE2=="1"])

# sel <- ALLOC[ALLOC$MRS_IDF=="0" & !is.na(ALLOC$X_19) & !is.na(ALLOC$Y_19) &!(is.na(ALLOC$X_18)) &!(is.na(ALLOC$Y_18)),]
# sel$DEPTCD <- as.character(sel$DEPTCD)
# sel <- sel %>% filter(!duplicated(sel$IDUNI))
# rm(ALLOC)
# # 
# # Sélection par intersection spatiale
# load("fond_de_carte_paris_osm.RData")
# st_crs(paris) <- 2154
# idf_regroup <- st_read("IDF_regroupée.shp")
# idf$code_insee <- "IDF"
# spa18 <- st_as_sf(sel[,c("IDUNI","X_18","Y_18")],coords = c("X_18","Y_18"))
# st_crs(spa18) <- 2154
# spa18 <- st_intersection(spa18, idf_regroup)
# spa18_75 <- st_intersection(spa18, paris)
# spa19 <- st_as_sf(sel[,c("IDUNI","X_19","Y_19")],coords = c("X_19","Y_19"))
# st_crs(spa19) <- 2154
# spa19 <- st_intersection(spa19, idf_regroup)
# 
# # selection des mobiles
# movers <- sel[sel$MOBILE2=="1" & sel$IDUNI %in% spa18$IDUNI & sel$IDUNI %in% spa19$IDUNI,]
# movers$PARIS_18 <- "NON"
# movers$PARIS_18[movers$IDUNI %in% spa18_75$IDUNI] <- "OUI"
# rm(sel,spa18,spa18_75, spa19, idf_regroup, rivers, quartier, parc, paris, arrond)
#save.image("chap5_figures.RData")

# Import 
load("chap5_figures.RData")
movers <- movers %>% filter(!duplicated(movers$IDUNI))

# Regarder qui les les personnes seules et les couples avec revenus plus élevés (quelles prestations?)
# Qui sont les couples aux revenus élevés? 
colnames(movers)
summary(movers$ALVERS_P1[movers$CLASS_SITFAM_P1=="COUPLE" & movers$REV_CLASSE=="HAUT"])
movers$NATIFAM_18 <- as.factor(movers$NATIFAM_18)
summary(movers$CATBEN_P1[movers$CLASS_SITFAM_P1=="COUPLE" & movers$REV_CLASSE=="HAUT" & movers$DEPTCD=="75"])
# Principalement des personnes dont les revenus ont augmenté et qui n'ont plus les AL, quelques AAH, quelques PPA (baisse des revenus) et des PF

# Préparation table
movers$REV_CLASSE[movers$REV_CLASSE=="FRAG"] <- "MOY"
movers$REV_CLASSE[movers$REV_CLASSE=="INC"] <- NA
movers$REV_CLASSE <- as.factor(as.character((movers$REV_CLASSE)))
movers$REV_CLASSE <- factor(movers$REV_CLASSE, levels = c("HAUT","MOY","BAS_REV"))

movers <- movers[,c("CLASS_SITFAM_P1", 
                    "REV_CLASSE", 
                    "PERSCOUV_P1",
                    "DIST_PARIS_EUC_18","DIST_PARIS_EUC_19","EVOL_DIST_PARIS_EUC",
                    "DIST_PARC_EUC",
                    #"evol_pot_emploi",
                    "evol_tx_emploi", 
                    "evol_acces_TEC", 
                    "evol_acces_equip_sup",
                    "PARIS_18",
                    "Tx_emp_pot_19","score_TEC_19","EQUIP_mean_sup_19")]
colnames(movers)

# Préparation des variables
colnames(movers) <- c("Composition familiale" ,
                      "Revenu", 
                      "Population_cov",
                      "Distance moyenne à Paris P1","Distance moyenne à Paris P2","Éloignement",
                      "Distance parcourue",
                      "Perte d'accès à l'emploi",
                      "Perte d'accès aux transports",
                      "Évolution de l'accès aux équipements",
                      "Paris_18",
                      "Tx_emp_pot_19","score_TEC_19","EQUIP_mean_sup_19")

movers[,"Composition familiale"] <- fct_recode(movers[,"Composition familiale"],
                                            "Personne seule"= "ISOLE",
                                            "Couple"= "COUPLE",
                                            "Couple avec enfant(s)"= "COU_ENF",
                                            "Famille monoparentale"= "MONOP",
                                            "Autre"= "AUTRE")

movers$`Composition familiale`[movers$`Composition familiale`=="Autre"] <- NA
movers$`Composition familiale` <- as.factor(as.character(movers$`Composition familiale`))
movers[,"Composition familiale"] <- factor(movers[,"Composition familiale"], levels = c("Personne seule","Couple","Famille monoparentale","Couple avec enfant(s)"))
movers[,"Revenu"] <- fct_recode(movers[,"Revenu"],
                                "Bas"= "BAS_REV",
                                "Modestes"="MOY",
                                "Élevés"="HAUT")
movers$`Perte d'accès à l'emploi`[is.na(movers$`Perte d'accès à l'emploi`)] <- 0
movers$`Perte d'accès aux transports`[is.na(movers$`Perte d'accès aux transports`)] <- 0
movers$`Évolution de l'accès aux équipements`[is.na(movers$`Évolution de l'accès aux équipements`)] <- 0

# passage en km 
#movers$`Distance moyenne à Paris P1` <- movers$`Distance moyenne à Paris P1`/1000
#movers$`Distance moyenne à Paris P2` <- movers$`Distance moyenne à Paris P2`/1000
movers$`Distance parcourue` <- movers$`Distance parcourue`/1000
movers$Éloignement <- movers$Éloignement/1000

# Positions 19 ----

# initialisation
TAB <- movers[(!is.na(movers$`Composition familiale`)) & (!is.na(movers$Revenu)),]
cols <- c("#4753a0","#9e5ba0","#ec6667")
#cols <- c("blue","purple","red")

for(i in 1:4){
  j <- levels(TAB$`Composition familiale`)[i]
  temp <- TAB[TAB$`Composition familiale`==j,]
  p1 <- ggplot(data = temp, aes(x = score_TEC_19, group = Revenu, fill = Revenu)) +
    geom_density(adjust = 1.5, alpha = .3, color = "darkgrey", size = 0.3) +
    scale_fill_manual(values = cols) +
    labs(
      #title = "Accès potentiel aux transports",
      x = "Points",
      y= "",
      fill = "Revenu"
    ) +
    theme_ipsum() +
    scale_y_continuous(labels = label_number(big.mark = "", decimal.mark = ","))+
    theme(legend.position = "none",
          plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9))
  
  p2 <- ggplot(data = temp, aes(x = Tx_emp_pot_19, group = Revenu, fill = Revenu)) +
    geom_density(adjust = 1.5, alpha = .3, color = "darkgrey", size = 0.3) +
    scale_fill_manual(values = cols) +
    labs(
      #title = "Accès potentiel à l'emploi",
      x = "Emplois/1000 actifs",
      y= "",
      fill = "Revenu"
    ) +
    theme_ipsum() +
    scale_y_continuous(labels = label_number(big.mark = "", decimal.mark = ","))+
    theme(legend.position = "none",
          plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5))
  
  p3 <- ggplot(data = temp, aes(x = EQUIP_mean_sup_19, group = Revenu, fill = Revenu)) +
    geom_density(adjust = 1.5, alpha = .3, color = "darkgrey", size = 0.3) +
    scale_fill_manual(values = cols) +
    labs(
      #title = "Distance aux équipements",
      x = "Mètres",
      y= "",
      fill = "Revenu"
    ) +
    theme_ipsum() +
    scale_y_continuous(labels = label_number(big.mark = "", decimal.mark = ","))+
    xlim(min(temp$EQUIP_mean_sup_19, na.rm=T)-1,7500) +
    theme(legend.position = "none",
          plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5))
  p4 <- ggplot(data = temp, aes(x = EQUIP_mean_sup_19, group = Revenu, fill = Revenu)) +
    geom_density(adjust = 1.5, alpha = .3, color = "darkgrey", size = 0.3) +
    scale_fill_manual(values = cols) +
    labs(
      title = "Distance aux équipements",
      x = "Mètres",
      y= "Densité",
      fill = "Revenus"
    ) +
    theme_ipsum() +
    scale_y_continuous(labels = label_number(big.mark = "", decimal.mark = ","))+
    xlim(min(temp$EQUIP_mean_sup_19, na.rm=T)-1,7500) +
    theme(legend.position = "bottom",
          plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5))
    
  if(i==1){
    pTec_PS <- p1
    pEmp_PS <- p2
    pEqu_PS <- p3
  }
  if(i==2){
    pTec_C <- p1
    pEmp_C <- p2
    pEqu_C <- p3
  }
  if(i==3){
    pTec_FM <- p1
    pEmp_FM <- p2
    pEqu_FM <- p3
  }
  if(i==4){
    pTec_CE <- p1
    pEmp_CE <- p2
    pEqu_CE <- p3
  }
}

pageZ <- readRDS("pageZ.rds")
pageZ(format = "custom",w_cust = 10, h_cust = 8, output = "svg", name = "posit19_fonct")
grid.arrange(pEmp_PS,pEqu_PS,pTec_PS,
             pEmp_FM,pEqu_FM,pTec_FM,
             pEmp_CE,pEqu_CE, pTec_CE, ncol=3)
dev.off()

# Pour la légende 1
pageZ(format = "custom",w_cust = 10, h_cust = 8, output = "svg", name = "legende1_posit19_fonct")
grid.arrange(p4,pEqu_PS,pTec_PS,
             pEmp_FM,pEqu_FM,pTec_FM,
             pEmp_CE,pEqu_CE, pTec_CE, ncol=3)
dev.off()

# Pour la légende 2
pageZ(format = "custom",w_cust = 10, h_cust = 8, output = "svg", name = "legende2_posit19_fonct")
p4
dev.off()

# Aggrégation pour indiquer les moyennes 
TAB2 <- TAB[TAB$`Composition familiale`!="Couple",]
posit19_means <- aggregate(TAB2[ ,c("Tx_emp_pot_19","EQUIP_mean_sup_19","score_TEC_19")], by = list(TAB2$Revenu, TAB2$`Composition familiale`), FUN = mean, na.rm=T)
colnames(posit19_means) <- c("Revenus","Configuration","Emploi","Équipements","Transports")
posit19_means$Emploi <- round(posit19_means$Emploi,0)
posit19_means$Transports <- round(posit19_means$Transports,0)
posit19_means$Équipements <- round(posit19_means$Équipements,0)

# Evolutions ----
# Agrégations
distance_movers <- aggregate(movers[ ,c("Distance moyenne à Paris P1","Distance moyenne à Paris P2", "Distance parcourue","Éloignement",
                                           "Perte d'accès à l'emploi","Perte d'accès aux transports",
                                           "Évolution de l'accès aux équipements")], by = list(movers$`Composition familiale`, movers$Revenu), FUN = mean)
colnames(distance_movers) <- c("Composition familiale","Revenu","Distance moyenne à Paris P1","Distance moyenne à Paris P2", "Distance parcourue","Éloignement",
                                  "Perte d'accès à l'emploi","Perte d'accès aux transports",
                                  "Évolution de l'accès aux équipements")
count_movers <- aggregate(movers[ ,"Population_cov"], by = list(movers$`Composition familiale`, movers$Revenu), FUN = sum)
colnames(count_movers) <- c("Composition familiale","Revenu","N")
distance_movers <- distance_movers %>% left_join(count_movers)

movers_75 <- movers[movers$Paris_18=="OUI",]
distance_movers_75 <- aggregate(movers_75[ ,c("Distance moyenne à Paris P1","Distance moyenne à Paris P2", "Distance parcourue","Éloignement",
                                           "Perte d'accès à l'emploi","Perte d'accès aux transports",
                                           "Évolution de l'accès aux équipements")], by = list(movers_75$`Composition familiale`, movers_75$Revenu), FUN = mean)
colnames(distance_movers_75) <- c("Composition familiale","Revenu","Distance moyenne à Paris P1","Distance moyenne à Paris P2", "Distance parcourue","Éloignement",
                                  "Perte d'accès à l'emploi","Perte d'accès aux transports",
                                  "Évolution de l'accès aux équipements")
count_movers_75 <- aggregate(movers_75[ ,"Population_cov"], by = list(movers_75$`Composition familiale`, movers_75$Revenu), FUN = sum)
colnames(count_movers_75) <- c("Composition familiale","Revenu","N")
distance_movers_75 <- distance_movers_75 %>% left_join(count_movers_75)

# Passages en valeur absolue
distance_movers$`Perte d'accès à l'emploi` <- abs(distance_movers$`Perte d'accès à l'emploi`)
distance_movers$`Perte d'accès aux transports` <- abs(distance_movers$`Perte d'accès aux transports`)
distance_movers_75$`Perte d'accès à l'emploi` <- abs(distance_movers_75$`Perte d'accès à l'emploi`)
distance_movers_75$`Perte d'accès aux transports` <- abs(distance_movers_75$`Perte d'accès aux transports`)

# préparation pour carto autour Paris
load("prepa_periph_chap5.Rdata")
# colnames(tab) <- c("Couple avec enfant(s)","Famille monoparentale","Couple","Personne seule")
# rownames(tab) <- c("minimum","maximum","Bas","Modestes","Élevés")
# noms_vars <- c("Éloignement (km)","Perte d'accès à l'emploi",
#                "Évolution de l'accès aux équipements (m)",
#                "Perte d'accès aux transport (pts)",
#                "Distance parcourue (km)")
# cols <- c("#ec6667","#9e5ba0","#4753a0")

# sous tables pour les graphs 
for(i in 1:2){
  if(i==1){
    data <- distance_movers
    perimetre <- "En Île-de-France"
    }
  if(i==2){
    data <- distance_movers_75
    data[2,3:9] <- NA
    perimetre <- "Depuis Paris"
  }
  
  #tab <- tab[,-3]# retirer les couples
  suburb <- tab
  suburb[5,] <- data$Éloignement[c(4,3,2,
                                   1)]
  suburb[4,] <- data$Éloignement[c(8,7,6,
                                   5)]
  suburb[3,] <- data$Éloignement[c(12,11,10,
                                   9)]
  suburb[2,] <- c(max(data$Éloignement[data$`Composition familiale`=="Couple avec enfant(s)"]),
                  max(data$Éloignement[data$`Composition familiale`=="Famille monoparentale"]),
                  max(data$Éloignement[data$`Composition familiale`=="Couple"], na.rm = T),
                  max(data$Éloignement[data$`Composition familiale`=="Personne seule"]))
  suburb[1,] <- c(min(data$Éloignement[data$`Composition familiale`=="Couple avec enfant(s)"]),
                  min(data$Éloignement[data$`Composition familiale`=="Famille monoparentale"]),
                  min(data$Éloignement[data$`Composition familiale`=="Couple"], na.rm = T),
                  min(data$Éloignement[data$`Composition familiale`=="Personne seule"]))
  
  dist_trav <- tab
  dist_trav[5,] <- data$`Distance parcourue`[c(4,3,2,
                                               1)]
  dist_trav[4,] <- data$`Distance parcourue`[c(8,7,6,
                                               5)]
  dist_trav[3,] <- data$`Distance parcourue`[c(12,11,10,
                                               9)]
  dist_trav[2,] <- c(max(data$`Distance parcourue`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                     max(data$`Distance parcourue`[data$`Composition familiale`=="Famille monoparentale"]),
                     max(data$`Distance parcourue`[data$`Composition familiale`=="Couple"], na.rm = T),
                     max(data$`Distance parcourue`[data$`Composition familiale`=="Personne seule"]))
  dist_trav[1,] <- c(min(data$`Distance parcourue`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                     min(data$`Distance parcourue`[data$`Composition familiale`=="Famille monoparentale"]),
                     min(data$`Distance parcourue`[data$`Composition familiale`=="Couple"], na.rm = T),
                     min(data$`Distance parcourue`[data$`Composition familiale`=="Personne seule"]))
  
  emp_loss <- tab
  emp_loss[5,] <- data$`Perte d'accès à l'emploi`[c(4,3,2,
                                                    1)]
  emp_loss[4,] <- data$`Perte d'accès à l'emploi`[c(8,7,6,
                                                    5)]
  emp_loss[3,] <- data$`Perte d'accès à l'emploi`[c(12,11,10,
                                                    9)]
  emp_loss[2,] <- c(max(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                    max(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Famille monoparentale"]),
                    max(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Couple"], na.rm = T),
                    max(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Personne seule"]))
  emp_loss[1,] <- c(min(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                    min(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Famille monoparentale"]),
                    min(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Couple"], na.rm = T),
                    min(data$`Perte d'accès à l'emploi`[data$`Composition familiale`=="Personne seule"]))
  
  TEC_loss <- tab
  TEC_loss[5,] <- data$`Perte d'accès aux transports`[c(4,3,2,
                                                        1)]
  TEC_loss[4,] <- data$`Perte d'accès aux transports`[c(8,7,6,
                                                        5)]
  TEC_loss[3,] <- data$`Perte d'accès aux transports`[c(12,11,10,
                                                        9)]
  TEC_loss[2,] <- c(max(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                    max(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Famille monoparentale"]),
                    max(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Couple"], na.rm = T),
                    max(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Personne seule"]))
  TEC_loss[1,] <- c(min(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                    min(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Famille monoparentale"]),
                    min(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Couple"], na.rm = T),
                    min(data$`Perte d'accès aux transports`[data$`Composition familiale`=="Personne seule"]))
  
  dist_serv <- tab
  dist_serv[5,] <- data$`Évolution de l'accès aux équipements`[c(4,3,2,
                                                                 1)]
  dist_serv[4,] <- data$`Évolution de l'accès aux équipements`[c(8,7,6,
                                                                 5)]
  dist_serv[3,] <- data$`Évolution de l'accès aux équipements`[c(12,11,10,
                                                                 9)]
  dist_serv[2,] <- c(max(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                     max(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Famille monoparentale"]),
                     max(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Couple"], na.rm = T),
                     max(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Personne seule"]))
  dist_serv[1,] <- c(min(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Couple avec enfant(s)"]),
                     min(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Famille monoparentale"]),
                     min(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Couple"], na.rm = T),
                     min(data$`Évolution de l'accès aux équipements`[data$`Composition familiale`=="Personne seule"]))
  
  # graphiques de résultats ----

  Éloignement <- suburb %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=Bas)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=minimum, yend=maximum), color="grey") +
    geom_point(size=4, color=cols[1]) +
    geom_point(aes(y=Modestes), size=4, color=cols[2]) +
    geom_point(aes(y=Élevés), size=4, color=cols[3]) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size=12)
    ) +
    ylab("") +
    xlab("")
  
  distance_travelled <- dist_trav %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=Bas)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=minimum, yend=maximum), color="grey") +
    geom_point(size=4, color=cols[1]) +
    geom_point(aes(y=Modestes), size=4, color=cols[2]) +
    geom_point(aes(y=Élevés), size=4, color=cols[3]) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size=12)
    ) +
    ylab("km") +
    xlab("") +
    ggtitle(paste(perimetre))
  
  distance_services <- dist_serv %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=Bas)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=minimum, yend=maximum), color="grey") +
    geom_point(size=4, color=cols[1]) +
    geom_point(aes(y=Modestes), size=4, color=cols[2]) +
    geom_point(aes(y=Élevés), size=4, color=cols[3]) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size=12)
    ) +
    ylab("") +
    xlab("")
  
  transportation_loss <- TEC_loss %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=Bas)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=minimum, yend=maximum), color="grey") +
    geom_point(size=4, color=cols[1]) +
    geom_point(aes(y=Modestes), size=4, color=cols[2]) +
    geom_point(aes(y=Élevés), size=4, color=cols[3]) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size=12)
    ) +
    ylab("") +
    xlab("")
  
  employment_loss <- emp_loss %>% t() %>% as.data.frame() %>% add_rownames() %>% mutate(rowname=factor(rowname, rowname)) %>%
    ggplot( aes(x=rowname, y=Bas)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=minimum, yend=maximum), color="grey") +
    geom_point(size=4, color=cols[1]) +
    geom_point(aes(y=Modestes), size=4, color=cols[2]) +
    geom_point(aes(y=Élevés), size=4, color=cols[3]) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size=12)
    ) +
    ylab("") +
    xlab("") #+
  
  if(i==1){
    Éloignement_IDF <- Éloignement
    distance_travelled_IDF <- distance_travelled + ylim(6.6,10.3)
    employment_loss_IDF <- employment_loss + ylim(0,75)
    transportation_loss_IDF <- transportation_loss  + ylim(0,8.5)
    distance_services_IDF <- distance_services  + ylim(20,500)
    }
  if(i==2){
    Éloignement_75 <- Éloignement
    distance_travelled_75 <- distance_travelled + ylim(3.9,7.6)
    employment_loss_75 <- employment_loss + ylim(78,153)
    transportation_loss_75 <- transportation_loss + ylim(18.75,27.25)
    distance_services_75 <- distance_services   + ylim(70,550)
    }
  rm(Éloignement, distance_travelled, employment_loss, transportation_loss, distance_services)
}

# Dénombrement movers 
sel1 <- distance_movers %>% select(1,2,10)
colnames(sel1)[3] <- "Perscouv_IDF"
sel2 <- distance_movers_75 %>% select(1,2,10)
colnames(sel2)[3] <- "Perscouv_75"
denomb_periph_IDF <- sel1 %>% left_join(sel2)
rm(sel1, sel2)

# #Éloignement_IDF
# distance_travelled_IDF
# employment_loss_IDF
transportation_loss_IDF
# distance_services_IDF
# 
# #Éloignement_75
# distance_travelled_75
employment_loss_75
# transportation_loss_75
# distance_services_75

# Construction des figures
T1 <- grid.arrange(employment_loss_75,employment_loss_IDF,ncol=2, top= textGrob("Perte d'emplois accessibles (pour 1000 actifs)",gp=gpar(fontsize=11,font=1)))
T2 <- grid.arrange(transportation_loss_75,transportation_loss_IDF,ncol=2, top= textGrob("Perte d'accès aux transports (pts)",gp=gpar(fontsize=11,font=1)))
T3 <- grid.arrange(distance_services_75,distance_services_IDF,ncol=2, top= textGrob("Évolution de la distance moyenne aux équipements supérieurs (m)",gp=gpar(fontsize=11,font=1)))
grid.arrange(T1,T2,T3,ncol=1)
grid.arrange(distance_travelled_75,distance_travelled_IDF, ncol=2, top= textGrob("Distance moyenne parcourue",gp=gpar(fontsize=14,font=1)))

# Préparation carto éloignement Paris
dist_max <- max(c(mean(distance_movers_75$`Distance moyenne à Paris P1`[distance_movers_75$Revenu=="Bas"]),
                  mean(distance_movers_75$`Distance moyenne à Paris P1`[distance_movers_75$Revenu=="Modestes"]),
                  mean(distance_movers_75$`Distance moyenne à Paris P1`[distance_movers_75$Revenu=="Élevés"]),
                  mean(distance_movers_75$`Distance moyenne à Paris P2`[distance_movers_75$Revenu=="Bas"]),
                  mean(distance_movers_75$`Distance moyenne à Paris P2`[distance_movers_75$Revenu=="Modestes"]),
                  mean(distance_movers_75$`Distance moyenne à Paris P2`[distance_movers_75$Revenu=="Élevés"])))
MAX <- st_buffer(x = HVPARIS, dist = dist_max)
LOW_P1 <- st_buffer(x = HVPARIS, dist = mean(distance_movers_75$`Distance moyenne à Paris P1`[distance_movers_75$Revenu=="Bas"]))
MED_P1 <- st_buffer(x = HVPARIS, dist = mean(distance_movers_75$`Distance moyenne à Paris P1`[distance_movers_75$Revenu=="Modestes"]))
HI_P1 <- st_buffer(x = HVPARIS, dist = mean(distance_movers_75$`Distance moyenne à Paris P1`[distance_movers_75$Revenu=="Élevés"]))
LOW_P2 <- st_buffer(x = HVPARIS, dist = mean(distance_movers_75$`Distance moyenne à Paris P2`[distance_movers_75$Revenu=="Bas"]))
MED_P2 <- st_buffer(x = HVPARIS, dist = mean(distance_movers_75$`Distance moyenne à Paris P2`[distance_movers_75$Revenu=="Modestes"]))
HI_P2 <- st_buffer(x = HVPARIS, dist = mean(distance_movers_75$`Distance moyenne à Paris P2`[distance_movers_75$Revenu=="Élevés"]))

# exports ----
pageZ <- readRDS("pageZ.rds")

pageZ(format = "custom",w_cust = 5.5, h_cust = 5.5, output = "svg", name = "carte_periph_75")
plot(st_geometry(MAX), border=NA, col=NA)
plot(st_geometry(Paris), add=T)
plot(st_geometry(HVPARIS),add=T)
text(st_coordinates(HVPARIS)+500 ,"Hôtel de ville",cex=0.8)
plot(st_geometry(LOW_P1), border=cols[1], lty=2, col=NA, lwd=1.2, add=T)
plot(st_geometry(LOW_P2), border=cols[1], col=NA, lwd=1.2, add=T)
plot(st_geometry(MED_P1), border=cols[2],lty=2, col=NA, lwd=1.2, add=T)
plot(st_geometry(MED_P2), border=cols[2], col=NA, lwd=1.2, add=T)
plot(st_geometry(HI_P1), border=cols[3],lty=2, col=NA, lwd=1.2, add=T)
plot(st_geometry(HI_P2), border=cols[3], col=NA, lwd=1.2, add=T)
mapsf::mf_scale()
legend("topleft", c("Avant","Après"),
       lty = c(2,1),bty = "n", cex=0.8)
dev.off()

pageZ(format = "custom",w_cust = 5.5, h_cust = 5.5, output = "svg", name = "legend_carte_periph")
plot(st_geometry(MAX), border=NA, col=NA)
plot(st_geometry(Paris), add=T)
legend("bottom", legend = c("Bas revenus","Revenus modestes","Revenus plus élevés"), #box.lty = 1,box.col = "grey",
       bty = "n",
       ncol = 3,
       pch=20 , col=cols , text.col = "black", cex=0.8, pt.cex=2)
dev.off()

pageZ(format = "landscape", output = "svg", name = "legend_periph")
plot(st_geometry(MAX), border=NA, col=NA)
plot(st_geometry(Paris), add=T)
legend("bottom", legend = c("Bas revenus","Revenus modestes","Revenus plus élevés"), #box.lty = 1,box.col = "grey",
       bty = "n",
       ncol = 3,
       pch=20 , col=cols , text.col = "black", cex=1.2, pt.cex=3)
dev.off()

pageZ(format = "landscape", output = "svg", name = "periph_fonctionnelle")
grid.arrange(T1,T3,T2,ncol=1)
dev.off()

pageZ(format = "landscape_half", output = "svg", name = "distance parcourue")
grid.arrange(distance_travelled_75,distance_travelled_IDF, ncol=2, top= textGrob("Distance moyenne parcourue",gp=gpar(fontsize=14,font=1)))
dev.off()

# Fisheye sorties ----
######################

# load("~/ADD_caf/Resultats_These/chap6_table_allocs.RData")
# sel <- ALLOC[ALLOC$MRS_IDF=="1",]
# sel$DEPTCD <- as.character(sel$DEPTCD)
# rm(ALLOC)
# # Sélection par intersection spatiale 
# idf_regroup <- st_read("/Users/luc/Desktop/Periph_chap6/idf_regroup_simpl.geojson")
# st_crs(idf_regroup) <- 2154
# spa18 <- st_as_sf(sel[,c("IDUNI","X_18","Y_18")],coords = c("X_18","Y_18"))
# st_crs(spa18) <- 2154
# spa18 <- st_intersection(spa18, idf_regroup)
# tab <- sel[sel$MOBILE2=="1" & sel$IDUNI %in% spa18$IDUNI,]
# rm(sel,spa18,idf_regroup, sortants)
# tab <- aggregate(tab[ , c("PERSCOUV_P1")], by = list(tab$DEPTPR), FUN = sum)
# colnames(tab) <- c("DEST","nb_ind")
# tab$DEST <- as.character(tab$DEST)
# tab <- tab[tab$DEST!="XX",]
# tab$ORI <- "IDF" 
# load("/Users/luc/Desktop/Periph_chap6/idf_dep_map.RData")
# tab <- tab[tab$DEST %in% idf_dep_metro$IDF,]
# save.image("/Users/luc/Desktop/Periph_chap6/fisheye.RData")

# Préparation des données géo
load("fisheye.RData")

idf_dep_metro <- st_cast(idf_dep_metro, "MULTIPOLYGON")
idf_dep_metro_fe <- fisheye(idf_dep_metro, centre = idf_dep_metro[idf_dep_metro$IDF=="IDF", ], method = 'log', k = 5.3)
idf_dep_metro_fe <- fisheye(idf_dep_metro, centre = idf_dep_metro[idf_dep_metro$IDF=="IDF", ], method = 'sqrt')
mf_base(idf_dep_metro_fe)
mf_base(idf_dep_metro)
#idf_dep_metro_fe <- idf_dep_metro
Coords_fe <- as.data.frame(cbind(idf_dep_metro_fe$IDF, st_coordinates(st_point_on_surface(idf_dep_metro_fe))))
Coords_fe$X <- as.numeric(Coords_fe$X)
Coords_fe$Y <- as.numeric(Coords_fe$Y)
IDF_fe <- idf_dep_metro_fe %>%
  filter(IDF %in% "IDF") 

colnames(Coords_fe) <- c("ORI","x_fe_ori","y_fe_ori")
tab <- tab %>% left_join(Coords_fe)
colnames(Coords_fe) <- c("DEST","x_fe_des","y_fe_des")
tab <- tab %>% left_join(Coords_fe)
deltaX <- tab$x_fe_des - tab$x_fe_ori
deltaY <- tab$y_fe_des - tab$y_fe_ori
tab$angle_fe <- atan2(deltaY, deltaX)

ratio_ligne <- 8000
ratio_fleche <- 30
#ratio_fleche <- 5

# version sqrt
tab$radius_disc <- nthroot(tab$nb_ind, 2) + 10
# tab$radius_disc <- 20
# tab$radius_disc[tab$nb_ind >= 100] <- 20
# tab$radius_disc[tab$nb_ind >= 500] <- 40
# tab$radius_disc[tab$nb_ind >= 1000] <- 60
# tab$radius_disc[tab$nb_ind >= 10000] <- 100

filteredData_flux_SORT.sf <- st_as_sf(x = tab, coords = c("x_fe_des", "y_fe_des")) %>%
  mutate(nb_ind.log = log(nb_ind)) %>%
  mutate(x_ctr_ACTU.do = map_dbl(geometry,  ~st_point_on_surface(.x)[[1]]),
         y_ctr_ACTU.do = map_dbl(geometry,  ~st_point_on_surface(.x)[[2]]))

# affichage du geom_spoke
p.flux.spoke <- ggplot() +
  geom_spoke(data= filteredData_flux_SORT.sf ,
             aes(x = x_ctr_ACTU.do ,
                 y = y_ctr_ACTU.do ,
                 radius = radius_disc, # version transfo log 
                 #radius = log(nb_ind)*10, # version transfo sqrt
                 #radius = nb_ind,
                 angle =  angle_fe
             ))

# récupération des geometries
g.flux.spoke <- ggplot_build(p.flux.spoke)

# et recalcul pour geom_segment
# flux entrants
p.str.sort <- g.flux.spoke$data[[1]] %>%
  rowwise() %>%
  mutate(x_moy = round(mean(x, xend),0),
         y_moy = round(mean(y, yend),0)) %>%
  left_join(filteredData_flux_SORT.sf %>%
              mutate(x_ctr_ACTU.do = round(x_ctr_ACTU.do,0),
                     y_ctr_ACTU.do = round(y_ctr_ACTU.do,0)),
            by = c("x_moy" ="x_ctr_ACTU.do","y_moy" ="y_ctr_ACTU.do")) %>%
  mutate(xdiff = xend - x, ydiff = yend - y) %>%
  mutate(xstart = x - xdiff, ystart = y - ydiff)

# puis cartographie finale
sortants_fe_var <- ggplot() + 
  geom_sf(data = idf_dep_metro_fe,
          color = "grey55", fill = "ivory", size = 0.1) +
  geom_sf(data = IDF_fe,
          color = "grey55", fill ="ivory", size = 0.2) + 
  geom_segment(data=p.str.sort, aes(x = xstart ,
                                    y = ystart ,
                                    xend = x,
                                    yend = y,
                                    #color = "#bdbdbd",
                                    size = nb_ind),
               arrow = arrow(ends = "last", length = unit(p.str.sort$nb_ind %>% log() /ratio_fleche,"cm"), 
                             type = "open")) + 
  scale_size(range = c(0.05,5), 
             breaks = c(min(p.str.sort$nb_ind),200,1000,4000,max(p.str.sort$nb_ind)),
             name = "Personnes mobiles"
  ) +
  #labs(title = "Personnes couvertes sortantes d'Île-de-France en 2018")+
  annotate(
    "text", label = "Île-de-France",
    x = 3.102446, y = -12.9102667, size = 4, colour = "black"
  )+
  theme(line = element_blank(),
        axis.text = element_blank(), 
        panel.background = element_blank(), 
        plot.title = element_text(face="bold", size=12),
        #legend.text = element_text(size=9),
        #legend.title = element_text(size=10),
        #legend.position="none",
        axis.title = element_blank(),
        plot.caption = element_text(hjust = 0, size=7))
  #   +
  # labs(caption = paste0("Champ : Personnes couvertes par les caisses d'allocations familiales et résidant en Île-de-France au 31 décembre 2018",
  #   "\n","Sources des données : Caf d'Île-de-France, FR6, décembre 2018 et 2019",
  #   "\n","Sources du code : Observatoire des territoires (réprésentation des flux), package R Fisheye 0.1.0 (transformation cartographie : racine des distance)"))

sortants_fe_var

# Exports
pageZ(format = "custom",w_cust = 9,h_cust = 7, output = "svg", name = "sortants_idf_19_nom2")
sortants_fe_var
dev.off()

pageZ(format = "custom",w_cust = 9,h_cust = 7, output = "svg", name = "sortants_idf_19_legend")
sortants_fe_var
dev.off()

rm(list=ls())


