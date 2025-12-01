# Figures supplémentaires chapitre 3

# NOTE : La fonction PageZ a été développée par Romain Leconte

library(units)
library(sf)
library(osmdata)
library(spatstat)
library(maptools)
library(raster)
library(cartography)
library(dplyr)
library(mapsf)
library(ggplot2)
library(forcats)
library(scales) 
library(tidyr)
library(grid)
library(gridExtra)
library(egg)

# Densité de la population allocataire ----
pageZ <- readRDS("pageZ.rds")
load("ALLOC_IMP_AL_ACC_density_maps.RData")
rasdens2 <- as(rasdens, "SpatRaster")
rasdens2[rasdens2<10] <- NA
bks3 <- c(11,201,601,1201,2001,3001,4201,5801,8201,14201)
C_INSEE <- c("95127","95585","95018","78551","78361","78646","78517","92050","93008","94028","91477","91228",
          "91223","77468","77284","77288","77379","77186")
Noms <- c("Cergy","Sarcelles", "Argenteuil","Saint-Germain-en-Laye","Mantes-la-Jolie", "Versailles",
          "Rambouillet","Nanterre","Bobigny","Créteil","Palaiseau","Evry","Etampes","Torcy","Meaux","Melun","Provins","Fontainebleau")
villes <- as.data.frame(cbind(C_INSEE,Noms))
com_idf$C_INSEE <- as.character(com_idf$C_INSEE)
villes_idf <- st_centroid(com_idf[com_idf$C_INSEE %in% C_INSEE,])
villes_idf <- villes_idf %>% left_join(villes)
rm(villes, Noms, C_INSEE)
villes_idf <- villes_idf[order(villes_idf$C_INSEE), ]
cols <- colorRampPalette(c("white","#eb7f86", "#5c53a5"))(length(bks3))
cols2 <- c(NA, cols[2:length(bks2)])

# Carto 
#par(mar = c(0,0,1.2,0))

pageZ(format = "landscape", output = "svg", name = "densité_caf_18")
mf_base(idf, bg="white", col="white", border="white")
mf_base(parc, col = "#d0d3a2", border = NA, add=T)
mf_raster(rasdens2, pal = cols, breaks=bks3, leg_pos="bottom",leg_val_rnd = 0,
          leg_title = "Foyers allocataires par km2", leg_horiz=T, add=T)
mf_base(deps, col = NA, border="dark grey",lwd = 0.5, add=T) 
mf_base(rivers, col = "#68abb8", lwd = 1, add=T)
mf_base(villes_idf, col="grey35",pch=20,cex = 0.6, add=T)
mf_label(
  x = villes_idf, var = "Noms",
  col = "grey35", halo = F, cex = 0.65,pos=3,
  overlap = FALSE, lines = F
)
mf_credits(txt = "Estimation par noyau. Bande de lissage : 200 mètres. spatstat 1.64 & mapsf 0.8.0")
mf_scale()
# sources : Caf Insee Géoréferencement allocataire 31 déc. 2018\nOpenStreetMap contributors, under CC BY SA
dev.off()
rm(list = ls())

# Planche description pop alloc ----

# Prépartion des données
# load("ALLOCS_1819_ND_G_BAN_2.RData")
# 
# sel <- ALLOC
# sel$DEPTCD <- as.character(sel$DEPTCD)
# rm(ALLOC, ALLOC_IDF_G)
# colnames(sel)[2] <- "CATBEN_P1"
# 
# # Ajout données logement
# LOG_RAB_18 <- read.csv("LOG_RAB_18.csv", sep=";")
# LOG_RAB_18$ID18 <- as.character(LOG_RAB_18$ID18)
# LOG_RAB_18 <- LOG_RAB_18[,-3]
# sel <- sel %>% left_join(LOG_RAB_18)
# sel$parc1 <- as.factor(sel$parc1)
# sel$ParcAL_P1[sel$parc1=="P_acces"] <- "acces"
# sel <- sel %>% select(-parc1)
# rm(LOG_RAB_18)
# PARCAL_18 <- read.csv("PARCAL_18.csv", sep=";")
# PARCAL_18$ID18 <- as.character(PARCAL_18$ID18)
# summary(as.factor(PARCAL_18$PARCAL[PARCAL_18$PARCAPL!="Sans signification ou DOM"]))
# summary(as.factor(PARCAL_18$PARCAPL[PARCAL_18$PARCAL!="Sans signification"]))
# PARCAL_18$PAL_18 <- NA
# PARCAL_18$PAL_18[PARCAL_18$PARCAPL!="Sans signification ou DOM"] <- PARCAL_18$PARCAPL[PARCAL_18$PARCAPL!="Sans signification ou DOM"]
# PARCAL_18$PAL_18[PARCAL_18$PARCAL!="Sans signification"] <- PARCAL_18$PARCAL[PARCAL_18$PARCAL!="Sans signification"]
# PARCAL_18$PAL_18 <- as.factor(PARCAL_18$PAL_18)
# PARCAL_18 <- PARCAL_18 %>% select(1,7,8)
# colnames(PARCAL_18)[1] <- "NORDALLC_TEST"
# sel <- sel %>% left_join(PARCAL_18)
# OCCLOG_18 <- read.csv("OCCLOG_18.csv", sep=";")
# OCCLOG_18$ID18 <- as.character(OCCLOG_18$ID18)
# colnames(OCCLOG_18)[1] <- "OCCLOG_18"
# sel <- sel %>% left_join(OCCLOG_18)
# 
# map_result <- readRDS("Figures/typo2/mapresults_CAH2.rds")
# result_cah <- map_result[,c(2,3)]
# sel <- sel[!(is.na(sel$X_18)),]
# sel <- sel[!(is.na(sel$Y_18)),]
# temp <- st_as_sf(sel[,c("IDUNI","X_18","Y_18")],coords = c("X_18","Y_18"))
# st_crs(temp) <- 2154
# temp <- st_intersection(result_cah, temp)
# temp <- as.data.frame(temp)
# temp <- temp[,-c(3)]
# sel <- dplyr::left_join(sel, temp) 
# sel <- sel[!(is.na(sel$Classe)),] # on ne garde que ceux qui sont géolocalisés en P1 et se trouvent effectivement en IDF 
# rm(PARCAL_18, OCCLOG_18, map_result, result_cah, temp)
# save.image("temp_chap3.RData")

load("temp_chap3.RData")

summary(is.na(sel$ParcAL_P1))
summary(as.factor(sel$ALVERS_P1))
summary(as.factor(sel$OCCLOG_P1 ))

library(dplyr)
temp <- sel[,c("ID18", "ALVERS_P1", "ParcAL_P1")]
temp$ID18 <- as.character(temp$ID18)
temp <- temp %>% left_join(presvers)
summary(temp$ParcAL_P1!="Pas d'AL"&(temp$p_APL==0 & temp$p_ALF==0 &temp$p_ALS==0))
summary(is.na(temp)) #(AL sous les seuils de versement?)

  
selection <- function(tab){
  tab$HANDI <- "Pas d'Aah"
  tab$HANDI[tab$CATBEN_P1 %in% c("AAH","ALSAAH","APLAAH","PFAAH","PLALSAAH","PFAPLAAH")] <- "Aah versée"
  tab$HANDI <- factor(tab$HANDI, levels = c("Pas d'Aah","Aah versée"))
  tab <- tab[tab$NATIOF != "0",]
  tab$NATIOF <- factor(tab$NATIOF, levels = c("1","2","3"))
  tab$PPRPPU2_P1 <- "Pas d'aide"
  tab$PPRPPU2_P1[tab$PPRPPU_P1 %in% c("INCONNU","PRIVE")] <- "Autre secteur"
  tab$PPRPPU2_P1[tab$PPRPPU_P1 =="PUBLIC"] <- "Secteur public"
  tab$PPRPPU2_P1 <- factor(tab$PPRPPU2_P1, levels = c("Pas d'aide","Secteur public","Autre secteur"))
  # Augmentation du nombre d'enfants à charge
  tab$PLUS_ENFANT <- "0"
  tab$PLUS_ENFANT[tab$NBLENFA_P1 < tab$NBLENFA_P2] <- "1"
  tab$PLUS_ENFANT <- as.factor(tab$PLUS_ENFANT)
  # Séparation ou perte du conjoint
  tab$SEPARATION <- "0"
  tab$SEPARATION[tab$CLASS_SITFAM_P1 %in% c("COUPLE","COU_ENF") & tab$CLASS_SITFAM_P2 %in% c("ISOLE","MONOP")] <- "1"
  tab$SEPARATION <- as.factor(tab$SEPARATION)
  # Diminution taille foyer (hors séparation: décohabitation, décès d'un membre du foyer ou autre raison, on ne décompose pas les raisons pour le moment)
  tab$DIM_FOYER <- "0"
  tab$DIM_FOYER[((tab$PERSCOUV_P1 - tab$PERSCOUV_P2) > 0 & tab$PERSCOUV_P2 != 0  & tab$SEPARATION =="0") | (tab$SEPARATION =="1" & (tab$PERSCOUV_P1 - tab$PERSCOUV_P2) > 1 & tab$PERSCOUV_P2 != 0)] <- "1"
  #tab$DIM_FOYER[(tab$PERSCOUV_P1 - tab$PERSCOUV_P2) > 0 & tab$PERSCOUV_P2 != 0  & tab$SEPARATION =="0"] <- "1"
  tab$DIM_FOYER <- as.factor(tab$DIM_FOYER)
  tab$DIST_PARIS_EUC_18_K <- tab$DIST_PARIS_EUC_18/1000
  tab$REV_CLASSE[tab$REV_CLASSE=="FRAG"] <- "MOY"
  #tab$REV_CLASSE[tab$REV_CLASSE=="INC"] <- NA
  tab$REV_CLASSE <- as.factor(as.character((tab$REV_CLASSE)))
  tab$REV_CLASSE <- factor(tab$REV_CLASSE, levels = c("HAUT","BAS_REV","MOY", "INC"))
  tab$Classe <- as.factor(as.character((tab$Classe)))
  tab$Classe <- factor(tab$Classe, levels = c("Classe 1","Classe 2","Classe 3","Classe 4","Classe 5"))
  tab$AGE <- tab$AGE
  tab$NATIONALITE2 <- tab$NATIOF
  tab$NATIONALITE2[tab$NATIONALITE2=="3"] <- "2"
  tab$NATIONALITE2 <- as.factor(tab$NATIONALITE2)
  tab$NATIONALITE2 <- factor(tab$NATIONALITE2, levels = c("1","2"))
  tab$ParcAL_P1 <- as.factor(as.character(tab$ParcAL_P1))
  tab$ParcAL_P1 <- factor(tab$ParcAL_P1, levels = c("Pas d'AL","prive","social","acces","foyer"))
  tab$DUROCC_P1 <- as.factor(as.character(tab$DUROCC_P1))
  tab$DUROCC_P1 <- factor(tab$DUROCC_P1, levels = c("D3_10","D1_3","Dinf1","Dsup10"))
  #tab$CLASS_SITFAM_P1[tab$CLASS_SITFAM_P1=="AUTRE"] <- NA
  tab$CLASS_SITFAM_P1 <- as.factor(as.character((tab$CLASS_SITFAM_P1)))
  tab$STACT2_RES_P1[tab$STACT2_RES_P1=="INC"] <- NA
  tab$STACT2_RES_P1 <- as.factor(as.character((tab$STACT2_RES_P1)))
  tab$EVO_ACT_RES[tab$EVO_ACT_RES=="INC" | tab$EVO_ACT_RES=="ABS"] <- NA
  tab$EVO_ACT_RES <- as.factor(as.character((tab$EVO_ACT_RES)))
  
  tab$SITFAM_P1_DET <- paste(tab$SEXE,tab$CLASS_SITFAM_P1, sep = "_")
  
  tab <- tab[,c("MOBILE2","CHANG_COM","MRS_PARIS","MRS_IDF",
                "CLASS_SITFAM_P1" , "SEXE" , "AGE",
                "REV_CLASSE", "PPRPPU2_P1" , "SEPARATION" ,"DIM_FOYER", "PLUS_ENFANT" ,
                "MISE_COUPLE", "NATIOF", "NATIONALITE2",
                "STACT2_RES_P1",
                "EVOACT2_RES", "REVPAT" ,
                "CATBEN2_P1", "Classe", "DIST_PARIS_EUC_18_K",
                "ND2","HANDI","ParcAL_P1","NBACHADR_P1","DUROCC_P1","IDUNI","X_18","Y_18","X_19","Y_19","NUMCOMDO_19", 
                "PERSCOUV_P1","SITFAM_P1_DET", "RET_RES_P1", "RET_RES_P2","DEPTCD", "CHANG_DEP","NUMCOMDO_18")]
  
  # colnames(tab) <- c("Déménager","Changer de commune", "Quitter Paris","Quitter l'IDF",
  #                    "Composition familiale" , "Sexe" , "Age" ,
  #                    "Revenu", "Aide au logement" , "Séparation" , "Diminution de la taille du foyer","Nouvel enfant" ,
  #                    "Mise en couple", "Nationalité" ,"Nationalité2", "Activité",
  #                    "Evenements profesionnels","Revenus du patrimoine" ,
  #                    "Catégorie de bénéficiaire","Type de marché", "Distance au centre",
  #                    "Noyau-dur 2019","Handicap", "Parc AL","Durée d'occupation","Classe durée d'occupation","IDUNI","X_18","Y_18","X_19","Y_19","NUMCOMDO_P2",
  #                    "PERSCOUV_P1", "Composition familiale détaillée", "RET_RES_P1", "RET_RES_P2","DEPTCD","CHANG_DEP")
  # 
  tab[,5] <- fct_recode(tab[,5],
                        "Personne seule"= "ISOLE",
                        "Couple"= "COUPLE",
                        "Couple avec enfant(s)"= "COU_ENF",
                        "Famille monoparentale"= "MONOP","
                        Autre"="AUTRE")
  tab[,5] <- factor(tab[,5], levels = c("Couple avec enfant(s)","Personne seule","Couple","Famille monoparentale","Autre"))
  tab[,6] <- fct_recode(tab[,6],
                        "Féminin"= "Féminin",
                        "Masculin"= "1")
  tab[,6] <- factor(tab[,6], levels = rev(c("Féminin","Masculin")))
  
  tab[,8] <- fct_recode(tab[,8],
                        "Bas revenus"= "BAS_REV",
                        "Modestes"="MOY",
                        "Plus élevés"="HAUT",
                        "Inconnus"="INC")
  tab[,14] <- fct_recode(tab[,14],
                         "Français"= "1",
                         # "Etranger"= "2")
                         "Etranger\nCEE"= "2",
                         "Etranger\nhors CEE"="3")
  tab[,15] <- fct_recode(tab[,15],
                         "Français"= "1",
                         "Etranger"= "2")
  tab[,16] <- fct_recode(tab[,16],
                         "Employé"= "ACT",
                         "Chômeur"= "CHO",
                         "Inactif"="INA",
                         "Etudiant"="ETU")
  tab[,16] <- factor(tab[,16], levels = c("Employé","Chômeur","Inactif","Etudiant"))
  tab[,17] <- fct_recode(tab[,17],
                         "Stable"="STABLE",
                         "Devenir employé"= "ACT",
                         "Devenir chômeur"= "CHO",
                         "Devenir inactif"="INA",
                         "Devenir étudiant"="ETU")
  tab[,17] <- factor(tab[,17], levels = c("Stable","Devenir employé","Devenir chômeur","Devenir inactif","Devenir étudiant"))
  tab[,18] <- fct_recode(tab[,18],
                         "Aucuns"="NON",
                         "Inf. 6000€"= "INF_6K",
                         "Sup. 6000€"= "SUP_6K")
  tab[,34] <- as.factor(tab[,34])
  tab[,34] <- fct_recode(tab[,34],
                         "Femme seule"= "Féminin_ISOLE",
                         "Homme seul"= "1_ISOLE",
                         "Couple"= "Féminin_COUPLE",
                         "Couple"= "1_COUPLE",
                         "Couple avec\nenfant(s)"= "Féminin_COU_ENF",
                         "Couple avec\nenfant(s)"= "1_COU_ENF",
                         "Femme seule\navec enfant(s)"= "Féminin_MONOP",
                         "Homme seul\navec enfant(s)"= "1_MONOP",
                         "Autre" = "Féminin_AUTRE",
                         "Autre" = "1_AUTRE"
  )
  tab[,34] <- factor(tab[,34], levels = rev(c("Femme seule","Homme seul","Femme seule\navec enfant(s)","Homme seul\navec enfant(s)","Couple","Couple avec\nenfant(s)","Autre")))
  return(tab)
}
sel1 <- selection(sel)
#save.image("temp_chap3_2.RData")

sel$STACT2_RES_P1 <- as.character(sel$STACT2_RES_P1)
sel$STACT2_RES_P1[sel$RET_RES_P1=="1"] <-"Retraité"
sel$STACT2_RES_P1[sel$STACT2_RES_P1=="Inactif"] <-"Autre inactif"
sel$STACT2_RES_P1[is.na(sel$STACT2_RES_P1)] <-"Inconnu"
sel$STACT2_RES_P1 <- as.factor(as.character(sel$STACT2_RES_P1))
summary(sel$STACT2_RES_P1)
sel$STACT2_RES_P1 <- factor(sel$STACT2_RES_P1, levels = rev(c("Employé","Chômeur","Etudiant","Retraité","Autre inactif","Inconnu")))

sel$EVOACT2_RES <- fct_recode(sel$EVOACT2_RES,
                              "Début d'emploi"= "Devenir employé",
                              "Début de chômage"= "Devenir chômeur",
                              "Début d'inactivité"= "Devenir inactif",
                              "Début d'études"= "Devenir étudiant",
                              "Situation stable"= "Stable")
sel$EVOACT2_RES <- as.character(sel$EVOACT2_RES)
sel$EVOACT2_RES[is.na(sel$EVOACT2_RES)] <-"Inconnue"
sel$EVOACT2_RES <- as.factor(as.character(sel$EVOACT2_RES))
sel$EVOACT2_RES <- factor(sel$EVOACT2_RES, levels = rev(c("Situation stable",
                                                          "Début d'emploi",
                                                          "Début de chômage",
                                                          "Début d'inactivité",
                                                          "Début d'études",
                                                          "Inconnue")))
sel$ParcAL_P1 <- fct_recode(sel$ParcAL_P1,   "AL location libre"= "prive",
                            "AL location HLM"= "social",
                            "AL accession"= "acces",
                            "AL en foyer"= "foyer")
sel$REVPAT <- fct_recode(sel$REVPAT, "Aucun"= "Non")
sel$DUROCC_P1<- fct_recode(sel$DUROCC_P1,
                           "De 3 à 10 ans"= "D3_10",
                           "De 1 à 3 ans"= "D1_3",
                           "Moins d'un an"= "Dinf1",
                           "Plus de 10 ans"= "Dsup10")
sel$DUROCC_P1 <- factor(sel$DUROCC_P1, levels = rev(c("Moins d'un an","De 1 à 3 ans","De 3 à 10 ans","Plus de 10 ans")))
sel$DEPTCD <- as.factor(sel$DEPTCD)
sel$AGE <- as.integer(sel$AGE)

sel$PLUS_ENFANT
# Construction de la table evements fam
event_columns <- c("MISE_COUPLE", "SEPARATION", "DIM_FOYER", "PLUS_ENFANT")
event_fam_df <- sel %>%
  pivot_longer(cols = event_columns, names_to = "event_type", values_to = "occurred") %>%
  filter(occurred == "1") %>%
  count(event_type) %>% 
  mutate(perc = n / nrow(sel) * 100)
event_fam_df$event_type <- as.factor(event_fam_df$event_type)
event_fam_df$event_type <- fct_recode(event_fam_df$event_type,
                                      "Réduction\ndu foyer*"= "DIM_FOYER",
                                      "Mise en couple"= "MISE_COUPLE",
                                      "Naissance\nadoption"= "PLUS_ENFANT",
                                      "Séparation\ndécès conjoint"= "SEPARATION")
event_fam_df$event_type <- fct_reorder(event_fam_df$event_type, event_fam_df$n)

# Construction de la planche

# Pertes 
round((12462 / (12462 + 2277793))*100,3) # pertes1819_bases
nrow(sel) # nb total foyer
sum(sel$PERSCOUV_P1) # nb perscouv
mean_age <- mean(sel$AGE, na.rm = TRUE)

# Graphiques
create_percentage_bar_plot <- function(data, var_name, titre) {
  # Convert var_name to a factor ordered by its frequency in descending order
  data <- data %>%
    mutate(!!var_name := factor(!!sym(var_name), levels = rev(unique(!!sym(var_name)))))
  
  # Calculate the percentages
  data_processed <- data %>%
    group_by(!!sym(var_name)) %>%
    summarise(n = n(), .groups = 'drop') %>%
    mutate(perc = n / sum(n) * 100)
  
  data_processed[[var_name]] <- fct_reorder(data_processed[[var_name]], data_processed$n)
  
  # Dynamically set aes mappings
  aes_mappings <- aes(x = !!sym(var_name), y = n)
  
  # Create the plot
  ggplot(data_processed, aes_mappings) +
    geom_bar(stat = "identity", fill = "grey70") +
    #geom_text(aes(label = sub("\\.", ",", sprintf("%.1f%%", perc))), position = position_stack(vjust = 0.5), size = 3, nudge_y = max(data_processed$n) * 0.01) + 
    geom_text(aes(label = sub("\\.", ",", sprintf("%.1f%%", perc))), position = position_stack(vjust = 0),hjust=0,  size = 3) +
    coord_flip() +  
    theme_minimal() +
    labs(title = titre, x = "", y = "") +
    scale_y_continuous(labels = function(x) paste0(format(x / 1000, big.mark = ","), "k")) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"),
          axis.text = element_text(color = "black"))
}

create_percentage_bar_plot_2 <- function(data, var_name, titre) {
  # Convert var_name to a factor ordered by its frequency in descending order
  data <- data %>%
    mutate(!!var_name := factor(!!sym(var_name), levels = rev(unique(!!sym(var_name)))))
  data_processed <- data %>%
    group_by(!!sym(var_name)) %>%
    summarise(n = n(), .groups = 'drop') %>%
    mutate(perc = n / sum(n) * 100)
  aes_mappings <- aes(x = !!sym(var_name), y = n)
  ggplot(data_processed, aes_mappings) +
    geom_bar(stat = "identity", fill = "grey70") +
    geom_text(aes(label = sub("\\.", ",", sprintf("%.1f%%", perc))), position = position_stack(vjust = 0),hjust=0,size=3) +
    coord_flip() +  
    theme_minimal() +
    labs(title = titre, x = "", y = "") +
    scale_y_continuous(labels = function(x) paste0(format(x / 1000, big.mark = ","), "k")) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"),
          axis.text = element_text(color = "black"))
}

# Figure

ND2 <- create_percentage_bar_plot(sel, "ND2","Noyau-dur en 2019")
DEPCTD <- create_percentage_bar_plot(sel, "DEPTCD","Département d'origine")

GENRE <- create_percentage_bar_plot(sel, "SEXE","Genre des allocataires")
NATIOF <- create_percentage_bar_plot(sel, "NATIOF","Nationalité de l'allocataire")

age <- ggplot(sel, aes(x = AGE)) +
  geom_histogram(binwidth = 1, fill = "grey70", color = "grey30") + # Histogram for age distribution
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed", size = 1) + # Line for mean age
  #geom_text(aes(x = mean_age, y = Inf, label = paste("Moyenne:", round(mean_age, 1))), vjust = 1.5, color = "red") +
  theme_minimal() +
  labs(title = "Age des allocataires", x = "", y = "") +
  scale_y_continuous(labels = function(x) paste0(format(x / 1000, big.mark = ","), "k")) + # Format y-axis labels in thousands
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"),
        axis.text = element_text(color = "black"))
durocc <- create_percentage_bar_plot_2(sel,"DUROCC_P1" ,"Ancienneté d'installation") # 

sitfam <- create_percentage_bar_plot(sel,"SITFAM_P1_DET" ,"Configuration familiale")

event_fam <- ggplot(event_fam_df, aes(x = event_type, y = n)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = sub("\\.", ",", sprintf("%.1f%%", perc))), position = position_stack(vjust = 0),hjust=0, size=3) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Transitions familiales", x = "", y = "") +
  scale_y_continuous(labels = function(x) paste0(format(x / 1000, big.mark = ","), "k")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.4,0.4,0.4,0.4), "cm"),
        axis.text = element_text(color = "black"))

stapro <- create_percentage_bar_plot(sel,"STACT2_RES_P1" ,"Statut pro. de l'allocataire")
evolpro <- create_percentage_bar_plot(sel,"EVOACT2_RES" ,"Evolution pro. 2019")

revclass <- create_percentage_bar_plot(sel,"REV_CLASSE" ,"Revenus par UC")
revpat <- create_percentage_bar_plot(sel,"REVPAT" ,"Revenus patrimoniaux annuels")

handi <- create_percentage_bar_plot(sel,"HANDI" ,"Handicap")
parc_al <- create_percentage_bar_plot(sel,"ParcAL_P1" ,"Aides au logement")

# Align and arrange plots
aligned_plots <- ggarrange(DEPCTD,durocc,NATIOF,age,sitfam,event_fam, stapro,evolpro,revclass,revpat,parc_al,handi, ncol=3)

# Display the aligned plots
grid.draw(aligned_plots)

pageZ <- readRDS("pageZ.rds")
pageZ(format = "custom",w_cust = 10, h_cust = 10, output = "svg", name = "describ_ALLOC_ter")
ggarrange(DEPCTD,durocc,NATIOF,age,sitfam,event_fam, stapro,evolpro,revclass,revpat,parc_al,handi, ncol=3)
dev.off()

# Mise en page 
T1 <- grid.arrange(DEPCTD,durocc,NATIOF,age,sitfam,event_fam, ncol=3)
T2 <- grid.arrange(stapro,evolpro,revclass,revpat,parc_al,handi, ncol=3)
grid.arrange(T1,T2,ncol=1)

pageZ(format = "custom",w_cust = 9.5, h_cust = 9, output = "svg", name = "describ_ALLOC_Bis")
grid.arrange(T1,T2,ncol=1)
dev.off()

# Tableaux tx mob ----
load("temp_chap3_2.RData")
idf <- c("75","77","78","91","92","93","94","95")
temp1 <- as.data.frame(table(sel$DEPTCD, sel$MOBILE2))
colnames(temp1) <- c("Département","MOD","Changements d'adresse")
temp2 <- as.data.frame(table(sel$DEPTCD, sel$CHANG_COM))
colnames(temp2) <- c("Département","MOD","Changements de commune")
temp3 <- as.data.frame(table(sel$DEPTCD, sel$CHANG_DEP))
colnames(temp3) <- c("Département","MOD","Changements de département")
temp4 <- as.data.frame(table(sel$DEPTCD, sel$MRS_IDF))
colnames(temp4) <- c("Département","MOD","Changements de région")
temp <- temp1 %>% left_join(temp2) %>% left_join(temp3) %>% left_join(temp4)

# Destination des mobilités résidentielles des allocataires
Tx_Mob_caf19 <- round(sum(sel$PERSCOUV_P1[sel$MOBILE2=="1"])/sum(sel$PERSCOUV_P1)*1000,0)
Tx_Mob_caf19_2 <- round(sum(mobica$PERSCOUV_P1[mobica$MOBILE2=="1"])/sum(mobica$PERSCOUV_P1)*1000,0)
# retirer les départs à l'étranger
etranger_caf <- round(sum(sel$PERSCOUV_P1[substr(sel$NUMCOMDO_19,1,2) %in% c("99","98")])/sum(sel$PERSCOUV_P1)*1000,0)
mobica <- sel[!(substr(sel$NUMCOMDO_19,1,2) %in% c("99","98")),]
meme_com_caf <- round(sum(mobica$PERSCOUV_P1[mobica$NUMCOMDO_18==mobica$NUMCOMDO_19 & mobica$MOBILE2=="1"])/sum(mobica$PERSCOUV_P1)*1000,0)
meme_dep_caf <- round(sum(mobica$PERSCOUV_P1[mobica$NUMCOMDO_18!=mobica$NUMCOMDO_19 & substr(mobica$NUMCOMDO_18,1,2)==substr(mobica$NUMCOMDO_19,1,2) & mobica$MOBILE2=="1"])/sum(mobica$PERSCOUV_P1)*1000,0)
aut_dep_caf <- round(sum(mobica$PERSCOUV_P1[(substr(mobica$NUMCOMDO_18,1,2)!=substr(mobica$NUMCOMDO_19,1,2)) & substr(mobica$NUMCOMDO_19,1,2) %in% idf & mobica$MOBILE2=="1"])/sum(mobica$PERSCOUV_P1)*1000,0)
hors_idf_caf <- round(sum(mobica$PERSCOUV_P1[!(substr(mobica$NUMCOMDO_19,1,2) %in% idf) & mobica$MOBILE2=="1"])/sum(mobica$PERSCOUV_P1)*1000,0)

# Comparaison avec Migcom
FD_MIGCOM_2019 <- read.csv("FD_MIGCOM_2019.csv", sep=";")

IDF <- FD_MIGCOM_2019[substr(FD_MIGCOM_2019$DCRAN,1,2) %in% c("75","77","78","91","92","93","94","95"),]
rm(FD_MIGCOM_2019)
sel <- IDF
sel1<-sel[sel$ARM!="ZZZZZ",]
sel1$COMAR<-sel1$ARM
sel2<-sel[sel$ARM=="ZZZZZ",]
sel2$COMAR<-paste(sel2$COMMUNE)
sel<-rbind(sel1,sel2)
IDF <- sel
remove(sel, sel1, sel2)

Tx_Mob_migcom19 <- round(sum(IDF$IPONDI[IDF$IRAN!=1])/sum(IDF$IPONDI)*1000,0)
meme_comMIGCOM <- round(sum(IDF$IPONDI[IDF$DCRAN==IDF$COMAR & IDF$IRAN!=1])/sum(IDF$IPONDI)*1000,0)
meme_depMIGCOM <- round(sum(IDF$IPONDI[IDF$DCRAN!=IDF$COMAR & substr(IDF$DCRAN,1,2)==substr(IDF$COMAR,1,2) & IDF$IRAN!=1])/sum(IDF$IPONDI)*1000,0)
aut_depMIGCOM <- round(sum(IDF$IPONDI[substr(IDF$DCRAN,1,2)!=substr(IDF$COMAR,1,2) & substr(IDF$COMAR,1,2) %in% idf & IDF$IRAN!=1])/sum(IDF$IPONDI)*1000,0)
hors_idfMIGCOM <- round(sum(IDF$IPONDI[!(substr(IDF$COMAR,1,2) %in% idf) & IDF$IRAN!=1])/sum(IDF$IPONDI)*1000,0)

Dest_mob_caf_migcom <- rbind(c("Ensemble",Tx_Mob_caf19, "/"),
                             c("Départ à l'étranger",etranger_caf, "/"),
                             c("En France",Tx_Mob_caf19_2, Tx_Mob_migcom19),
                             c("Dont vers la même commune",meme_com_caf, meme_comMIGCOM),
                             c("Dont vers une autre commune du même département",meme_dep_caf, meme_depMIGCOM),
                             c("Dont vers un autre département d'Île-de-France",aut_dep_caf, aut_depMIGCOM),
                             c("Dont hors d'Île-de-France",hors_idf_caf, hors_idfMIGCOM))
Dest_mob_caf_migcom <- as.data.frame(Dest_mob_caf_migcom)
colnames(Dest_mob_caf_migcom) <- c("Type de mobilité","CAF FR6 2018-2019", "MIGCOM 2019")
rm(IDF, MOBCOM, freqev, meme_com_caf, 
   meme_comMIGCOM,meme_dep_caf, meme_depMIGCOM,aut_dep_caf, 
   aut_depMIGCOM,hors_idf_caf, hors_idfMIGCOM, TX_MOB_ALLOC_CORREG)


# Taux couv et mob lissés ----

load("temp_chap3_2.RData")
IDF <- st_as_sf(idf)
IDF <- st_transform(IDF, 2154)
com_idf20 <- st_read("geo_idf_1820.geojson")
com_idf20 <- com_idf20[,c(15,42)]
colnames(com_idf20)[1] <- "code"
com_idf20 <- st_as_sf(com_idf20)
com_idf20 <- st_transform(com_idf20, 2154)
ALLOC <- st_as_sf(sel[,c("PERSCOUV_P1","MOBILE2","X_18","Y_18")], coords = c("X_18","Y_18"))
st_crs(ALLOC) <- 2154
ALLOC <- st_intersection(com_idf20, ALLOC)
ALLOC <- as.data.frame(ALLOC) %>% select(-4)
POP <- aggregate(ALLOC$PERSCOUV_P1, by=list(ALLOC$code), FUN="sum")
colnames(POP) <- c("code","POP_CAF")
POP_MOB <- aggregate(ALLOC$PERSCOUV_P1[ALLOC$MOBILE2=="1"], by=list(ALLOC$code[ALLOC$MOBILE2=="1"]), FUN="sum")
colnames(POP_MOB) <- c("code","POP_MOB")

pop_com_idf_18 <- read.csv("/pop_com_idf_18.csv", sep=";") # Insee RP
pop_com_idf_18 <- pop_com_idf_18[,c(5,9)]
colnames(pop_com_idf_18) <- c("code","pop_18")
pop_com_idf_18$code <- as.character(pop_com_idf_18$code)

com_idf20 <- left_join(com_idf20, pop_com_idf_18)
com_idf20 <- left_join(com_idf20, POP)
com_idf20 <- left_join(com_idf20, POP_MOB)
com_idf20 <- st_centroid(com_idf20)

#com_idf20$POP[is.na(com_idf20$POP)] <- 0
com_idf20$POP_MOB[is.na(com_idf20$POP_MOB)] <- 0

y <- create_grid(x = IDF, res = 1000)
pot <- mcpotential(x = com_idf20, y = y, 
                   var = c("pop_18","POP_CAF","POP_MOB"), fun = "e",
                   span = 2500, beta = 2, 
                   limit = 30000, ncl = 4)
pot <- as.data.frame(pot)
y <- cbind(y, pot)
y$taux_couv <- 100 * y$POP_CAF/ y$pop_18
y$taux_mob <- 100 * y$POP_MOB/ y$POP_CAF
pot_tx_couv <- y
rm(y, pot, pop_com_idf_18, com_idf20, ALLOC)

# Construction des équipot
seuilsIDF <- st_intersection(pot_tx_couv, IDF)
bkscouv <- mf_get_breaks(x = seuilsIDF$taux_couv, breaks = "fisher", nbreaks = 8)
bksmob <- mf_get_breaks(x = seuilsIDF$taux_mob, breaks = "q6")
equipot_tx1 <- equipotential(pot_tx_couv, var = "taux_couv", breaks = bkscouv, mask = IDF)
equipot_tx2 <- equipotential(pot_tx_couv, var = "taux_mob", breaks = bksmob, mask = IDF)


# Cartographie et exports 

pageZ(format = "landscape", output = "svg", name = "tx_couv18")
mf_base(idf, bg="white", col="white", border="white")
mf_map(equipot_tx1, var = "min", type="choro",
       breaks = bkscouv, pal = hcl.colors(8, 'Teal'), 
       border = NA, leg_pos="bottom",leg_val_rnd = 0,
       leg_title = "Couverture (%)", leg_horiz=T, add=T)
mf_base(deps, col = NA, border="grey45",lwd = 0.9, lty=3, add=T)
mf_base(villes_idf, col="white",pch=20,cex = 0.75, add=T)
mf_label(
  x = villes_idf, var = "Noms",
  col = "white", halo = F, cex = 0.65,pos=3,
  overlap = FALSE, lines = F
)
mf_base(idf, col = NA, border="#f7f7f7",lwd = 3, add=T)
mf_scale(20)
mf_credits(txt = "Estimation par noyau. Fonction : exponentielle. Bande de lissage : 2,5 km. Limite : 30 km. potential 0.2.0 & mapsf 0.8.0")
dev.off()

pageZ(format = "landscape", output = "svg", name = "tx_mob19")
mf_base(idf, bg="white", col="white", border="white")
mf_map(equipot_tx2, var = "min", type="choro", breaks = bksmob, pal = rev(hcl.colors(8, 'Blues'))[2:7], border = NA,
       leg_pos="bottom",leg_val_rnd = 1,
       leg_title = "Taux de mobilité local (%)", leg_horiz=T, add=T)
mf_base(deps, col = NA, border="grey45",lwd = 0.5, add=T)
mf_base(villes_idf, col="white",pch=20,cex = 0.7, add=T)
mf_label(
  x = villes_idf, var = "Noms",
  col = "white", halo = F, cex = 0.65,pos=3,
  overlap = FALSE, lines = F
)
mf_base(idf, col = NA, border="#f7f7f7",lwd = 3, add=T)
mf_scale(20)
mf_credits(txt = "Estimation par noyau. Fonction : exponentielle. Bande de lissage : 2,5 km. Limite : 30 km. potential 0.2.0 & mapsf 0.8.0")
dev.off()

# Codification activité ----
  
load("temp_chap3.RData")
temp <- as.data.frame(summary(sel$ACTRESPD_P1))
rm(list = ls())

summary(sel$ACTRESPD_P1)

# Evolution accédants ----
load("Chapitre 3/Figures/Fichiers sources/access.RData") 
# construit à partir des tables denomb_1722_ALL et denomb_1722 issues du programme Figure DE AL
access$Année <- c("2017/12/31", "2018/12/31","2019/12/31","2020/12/31","2021/12/31","2022/12/31")
access$Nombre <- access$Accession
access$Part <- round(access$Accession/access$Total*1000,0)
access$Année <- as.Date(access$Année)
vline_date <- as.Date("2018-01-01")

p <- ggplot(access, aes(x = Année)) +
  geom_line(aes(y = Nombre, group = 1, colour = "Effectif"), size = 1) + 
  geom_point(aes(y = Nombre, colour = "Effectif")) + 
  scale_colour_manual(values = c("Effectif" = "grey35", "Part" = "#FFC300")) + 
  theme_minimal() + # Minimalist theme
  labs(y = "N", colour = "") + 
  geom_line(aes(y = Part * max(access$Nombre) / max(access$Part), group = 1, colour = "Part"), size = 1) +
  geom_point(aes(y = Part * max(access$Nombre) / max(access$Part), colour = "Part")) +
  scale_y_continuous(limits = c(0, max(access$Nombre, na.rm = TRUE)),labels = function(x) paste0(format(x / 1000, big.mark = ","), "k"),
    sec.axis = sec_axis(~ . * max(access$Part) / max(access$Nombre), name = "‰")) +
  geom_vline(xintercept = vline_date, linetype="dashed", color = "grey") +
  theme(legend.position = "bottom") 

# Evolution SF_PSeule MOY ----
coln <- c("Année","Nombre","Part")
R17 <- c("2017/12/31", 211258, 9.34)
R18 <- c("2018/12/31", 265703,	11.58)
R19 <- c("2019/12/31", 367402,	15.15)
R20 <- c("2020/12/31", 387361,	15.55)
R21 <- c("2021/12/31", 366379,	15.02)
R22 <- c("2022/12/31", 380198,	15.49)
seulmoy <- as.data.frame(rbind(R17,R18,R19,R20,R21,R22))
colnames(seulmoy) <- coln
rownames(seulmoy) <- 1:6
seulmoy$Année <- as.Date(seulmoy$Année)
seulmoy$Nombre <- as.numeric(seulmoy$Nombre)
seulmoy$Part <- as.numeric(seulmoy$Part)

vline_date1 <- as.Date("2019-01-01")

p2 <- ggplot(seulmoy, aes(x = Année)) +
  geom_line(aes(y = Nombre, group = 1, colour = "Effectif"), size = 1) + # Line for Accession
  geom_point(aes(y = Nombre, colour = "Effectif")) + # Points for Accession
  scale_colour_manual(values = c("Effectif" = "grey35", "Part" = "#FFC300")) + # Manual colour assignment
  theme_minimal() + # Minimalist theme
  labs(y = "N", colour = "") + 
  geom_line(aes(y = Part * max(seulmoy$Nombre) / max(seulmoy$Part), group = 1, colour = "Part"), size = 1) +
  geom_point(aes(y = Part * max(seulmoy$Nombre) / max(seulmoy$Part), colour = "Part")) +
  scale_y_continuous(limits = c(0, max(seulmoy$Nombre, na.rm = TRUE)),labels = function(x) paste0(format(x / 1000, big.mark = ","), "k"),
                     sec.axis = sec_axis(~ . * max(seulmoy$Part) / max(seulmoy$Nombre), name = "%")) +
  geom_vline(xintercept = as.numeric(vline_date1), linetype="dashed", color = "grey") +
  theme(legend.position = "bottom") 

# Export
pageZ(format = "custom",w_cust = 8, h_cust = 4, output = "svg", name = "var_pop_alloc")
ggarrange(p,p2,ncol=2)
dev.off()












