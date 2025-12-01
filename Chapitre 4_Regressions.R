# Code chapitre 4

# Pkgs ----
library(dplyr)
library(ggplot2)
library(sf)
library(potential)
library(scales)
library(jtools)
library(rms)
library(DescTools)
library(mapsf)
library(forcats)

# Import données ----

load("ALLOCS_1819_ND_G_BAN_2.RData") # Import des données allocataires préparées 2019


#sel <- ALLOC_IDF_G
sel <- ALLOC
sel$DEPTCD <- as.character(sel$DEPTCD)
#summary(ALLOC_IDF_G$ND2)
rm(ALLOC, ALLOC_IDF_G)
summary(sel$CATBEN_18)
summary(sel$ND2)
colnames(sel)[2] <- "CATBEN_P1"
summary(sel$ID18)
exp(0.38)

# Correction avec bonnes données parc accession
LOG_RAB_18 <- read.csv("LOG_RAB_18.csv", sep=";")
LOG_RAB_18$ID18 <- as.character(LOG_RAB_18$ID18)
LOG_RAB_18 <- LOG_RAB_18[,-3]
sel <- sel %>% left_join(LOG_RAB_18)
sel$parc1 <- as.factor(sel$parc1)
table(sel$ParcAL_P1, sel$parc1)
summary(sel$ParcAL_P1)
sel$ParcAL_P1[sel$parc1=="P_acces"] <- "acces"
summary(sel$ParcAL_P1)
sel <- sel %>% select(-parc1)
rm(LOG_RAB_18)
# Ajout données détaillées par parcAL
PARCAL_18 <- read.csv("PARCAL_18.csv", sep=";")
PARCAL_18$ID18 <- as.character(PARCAL_18$ID18)
summary(as.factor(PARCAL_18$PARCAL[PARCAL_18$PARCAPL!="Sans signification ou DOM"]))
summary(as.factor(PARCAL_18$PARCAPL[PARCAL_18$PARCAL!="Sans signification"]))
PARCAL_18$PAL_18 <- NA
PARCAL_18$PAL_18[PARCAL_18$PARCAPL!="Sans signification ou DOM"] <- PARCAL_18$PARCAPL[PARCAL_18$PARCAPL!="Sans signification ou DOM"]
PARCAL_18$PAL_18[PARCAL_18$PARCAL!="Sans signification"] <- PARCAL_18$PARCAL[PARCAL_18$PARCAL!="Sans signification"]
PARCAL_18$PAL_18 <- as.factor(PARCAL_18$PAL_18)
summary(PARCAL_18$PAL_18)
PARCAL_18 <- PARCAL_18 %>% select(1:4,7,8)
colnames(PARCAL_18)[1] <- "NORDALLC_TEST"
sel <- sel %>% left_join(PARCAL_18)
summary(sel$ParcAL_P1[!is.na(sel$PAL_18)]) # pas de décalage

# Ajout des données OCCLOG
OCCLOG_18 <- read.csv("OCCLOG_18.csv", sep=";")
OCCLOG_18$ID18 <- as.character(OCCLOG_18$ID18)
colnames(OCCLOG_18)[1] <- "OCCLOG_18"
sel <- sel %>% left_join(OCCLOG_18)

# Part des foyers avec AL
summary(sel$ParcAL_P1)
round((nrow(sel)-1090159)/nrow(sel),2) # Avec AL
round(1090159/nrow(sel),2) # Sans AL

# Part des familles monoparentales
#table(sel$SEXE, sel$CLASS_SITFAM_P1)
summary(sel$CLASS_SITFAM_P1)
#round(24045/(24045+350894),2) #94% de familles monoparentales. 

# Ajout des data -----
# # Préparation des données AL
# ADD_1218 <- read.csv("ADD_1218.csv", sep=";")
# ADD_1218$parc1[ADD_1218$parc1 ==""] <- "Pas d'AL"
# ADD_1218$parc1 <- as.factor(ADD_1218$parc1)
# ADD_1218$ID18 <- as.character(ADD_1218$ID18)
# # tests et jointure test
# # ADD_1218$alversee[is.na(ADD_1218$alversee)] <- "0"
# # ADD_1218$alversee <- as.numeric(ADD_1218$alversee)
# # summary(as.factor(ADD_1218$alversee[ADD_1218$parc1=="Pas d'AL"]))
# # test <- ALLOC[,c("DTNAIRESP","ID18","NUMCOMDO_18")]
# # test <- left_join(test, ADD_1218)
# # summary(test$NUMCOMDO==test$NUMCOMDO_18)
# # test2 <- test[test$NUMCOMDO!=test$NUMCOMDO_18,]
# # substrRight <- function(x, n){
# #   substr(x, nchar(x)-n+1, nchar(x))
# # }
# # summary(substrRight(test$DTNAIRESP,4)==substr(test$DTNAIRES,7,10))
# ADD_1218 <-ADD_1218[,c("ID18","parc1")]
# colnames(ADD_1218) <- c("ID18","parcAL_P1")
# 
# # Préparation des données NBMCHADR
# DATAMINING_1218 <- read.csv("DATAMINING_1218.csv", sep=";")
# DATAMINING_1218 <- DATAMINING_1218[,c(1,3)]
# DATAMINING_1218$ID18 <- as.character(DATAMINING_1218$ID18)
# colnames(DATAMINING_1218)[1] <- "NBMCHADR_18"
# DATAMINING_1218$NBACHADR_18 <- DATAMINING_1218$NBMCHADR_18/12
# summary(DATAMINING_1218$NBACHADR_18)
# DATAMINING_1218$DUROCC <- "INC"
# DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 <= 1] <-"Dinf1"
# DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 > 1 & DATAMINING_1218$NBACHADR_18 <= 3] <-"D1_3"
# DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 > 3 & DATAMINING_1218$NBACHADR_18 <= 10] <-"D3_10"
# DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 >  10] <-"Dsup10"
# DATAMINING_1218$DUROCC <- as.factor(DATAMINING_1218$DUROCC)
# summary(DATAMINING_1218$DUROCC)
# 
# # Préparation des données dépendances
# DEP_1218 <- read.csv("DEP_1218.csv", sep=";")
# DEP_1218$ID18 <- as.character(DEP_1218$ID18)
# summary(DEP_1218$depend)
# DEP_1218$CLASS_DEP_18 <- "INC"
# DEP_1218$CLASS_DEP_18[DEP_1218$depend == 0]  <- "NS"
# DEP_1218$CLASS_DEP_18[DEP_1218$depend > 0 & DEP_1218$depend <= 0.25 ]  <- "DEP_M25"
# DEP_1218$CLASS_DEP_18[DEP_1218$depend > 0.25 & DEP_1218$depend <= 0.5 ]  <- "DEP_25_50"
# DEP_1218$CLASS_DEP_18[DEP_1218$depend > 0.5 & DEP_1218$depend <= 0.75 ]  <- "DEP_50_75"
# DEP_1218$CLASS_DEP_18[DEP_1218$depend > 0.75 ]  <- "DEP_P75"
# DEP_1218$CLASS_DEP_18 <- as.factor(DEP_1218$CLASS_DEP_18)
# summary(DEP_1218$CLASS_DEP_18)
# DEP_1218$depend[DEP_1218$depend == 0] <- NA
# DEP_1218$depend[DEP_1218$depend > 1] <- 1
# DEP_1218$depend <- DEP_1218$depend*100
# summary(DEP_1218$depend)
# 
# # Jointures intermédiaires
# tab <- DEP_1218 
# tab <- left_join(tab, DATAMINING_1218)
# tab <- left_join(tab, ADD_1218)
# colnames(tab) <- c("depend_P1","ID18","CLASS_DEP_P1","NBMCHADR_P1","NBACHADR_P1","DUROCC_P1","parcAL_P1")
# 
# # Jointure finale 
# sel <- left_join(sel, tab)
# summary(sel$parcAL_P1) # pas d'AL ou parc inconnu
# summary(ADD_1218$parcAL_P1)
# summary(sel$NBMCHADR_P1)
# summary(DATAMINING_1218$NBMCHADR_18)
# summary(sel$NBACHADR_P1)
# summary(DATAMINING_1218$NBACHADR_18)
# summary(sel$DUROCC_P1)
# summary(DATAMINING_1218$DUROCC)
# summary(sel$CLASS_DEP_P1)
# summary(DEP_1218$CLASS_DEP_18)
# rm(tab, DEP_1218, DATAMINING_1218, ADD_1218)

# Ajout de la typologie des marchés ----
#load("Chap4_CAH6_VF.RData")
map_result <- readRDS("mapresults_CAH2.rds")
result_cah <- map_result[,c(2,3)]
sel <- sel[!(is.na(sel$X_18)),]
sel <- sel[!(is.na(sel$Y_18)),]
temp <- st_as_sf(sel[,c("IDUNI","X_18","Y_18")],coords = c("X_18","Y_18"))
st_crs(temp) <- 2154
temp <- st_intersection(result_cah, temp)
temp <- as.data.frame(temp)
temp <- temp[,-c(3)]
sel <- dplyr::left_join(sel, temp) 
rm(temp)
sel <- sel[!(is.na(sel$Classe)),] # on ne garde que ceux qui sont géolocalisés en P1 et se trouvent effectivement en IDF 
foyers_classes <- summary(sel$Classe)
perscouv_classes <- as.data.frame(tapply(sel[, "PERSCOUV_P1"], sel$Classe, sum))
marches_desc <- cbind(perscouv_classes, foyers_classes)
colnames(marches_desc) <- c("Personnes couvertes", "Foyers")

# vérif carte
mapsf::mf_map(map_result, var = "Classe", type = "typo")

# D'où viennent les méanges qui vivent en foyer et qui changent de commune dans la classe 3 ? 
# Communes en cours de transformation. 
summary(as.factor(sel$ParcAL_P1))
summary(as.factor(sel$OCCLOG_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 1"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 1",])*100
summary(as.factor(sel$OCCLOG_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 2"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 2",])*100
summary(as.factor(sel$OCCLOG_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 3"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 3",])*100
summary(as.factor(sel$OCCLOG_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 4"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 4",])*100
summary(as.factor(sel$OCCLOG_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 5"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 5",])*100

summary(as.factor(sel$PAL_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 1" & sel$CHANG_COM=="1"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 1" & sel$CHANG_COM=="1",])*100
summary(as.factor(sel$PAL_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 2" & sel$CHANG_COM=="1"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 2" & sel$CHANG_COM=="1",])*100
summary(as.factor(sel$PAL_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 3" & sel$CHANG_COM=="1"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 3" & sel$CHANG_COM=="1",])*100
summary(as.factor(sel$PAL_18[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 4" & sel$CHANG_COM=="1"]))/nrow(sel[sel$ParcAL_P1=="acces"  & sel$Classe=="Classe 4" & sel$CHANG_COM=="1",])*100

# exploration parc social classe 5
 # 
nrow(sel[sel$Classe=="Classe 5",])
nrow(sel[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5",])
nrow(sel[sel$Classe=="Classe 5" & sel$CHANG_COM=="1",])
nrow(sel[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5" & sel$CHANG_COM=="1",])
summary(as.factor(sel$NUMCOMDO_18[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5"]))
summary(as.factor(sel$NUMCOMDO_18[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5" & sel$CHANG_COM=="1"]))
summary(as.factor(sel$NUMCOMDO_18[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5" & sel$MOBILE2=="1" & sel$CHANG_COM=="0"]))

nrow(sel[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5" & sel$MOBILE2=="1",])
nrow(sel[sel$ParcAL_P1=="social"  & sel$Classe=="Classe 5" & sel$MOBILE2=="1" & sel$CHANG_COM=="0",])

nrow(sel[sel$ParcAL_P1=="foyer"  & sel$Classe=="Classe 3" & sel$NUMCOMDO_18 %in% c("77111","77268","77307","77449","77372"),])

summary(sel$Classe)
summary(as.factor(sel$EVO_ACT_RES))


# # Intersection arrivée
# sel2 <- sel
# sel2 <- sel2[!(is.na(sel2$X_19)),]
# sel2 <- sel2[!(is.na(sel2$Y_19)),]
# temp <- st_as_sf(sel2[,c("IDUNI","X_19","Y_19")],coords = c("X_19","Y_19"))
# st_crs(temp) <- 2154
# temp <- st_intersection(result_cah, temp)
# temp <- as.data.frame(temp)
# temp <- temp[,-c(3)]
# colnames(temp)[1] <- "Classe P2" 
# sel <- dplyr::left_join(sel, temp) 
# rm(temp, sel2)
# 
# # analyse des sorties classe 5
# summary(sel$MISE_COUPLE)
# temp <- sel[sel$Classe=="Classe 5" & sel$MISE_COUPLE=="1",] # sel$CHANG_COM=="1"
# summary(as.factor(temp$`Classe P2`))
summary(as.factor(sel$RET_RES_P1))
#tab <- sel

# Fct prépa variables ----
selection <- function(tab){
  tab$HANDI <- "Non"
  tab$HANDI[tab$CATBEN_P1 %in% c("AAH","ALSAAH","APLAAH","PFAAH","PLALSAAH","PFAPLAAH")] <- "Oui"
  tab$HANDI <- factor(tab$HANDI, levels = c("Non","Oui"))
  tab <- tab[tab$NATIOF != "0",]
  # tab$NATIOF[tab$NATIOF=="3"] <- "2"
  # tab$NATIOF <- as.factor(tab$NATIOF)
  # tab$NATIOF <- factor(tab$NATIOF, levels = c("1","2"))
  tab$NATIOF <- factor(tab$NATIOF, levels = c("1","2","3"))
  #tab$BAS_REV <- as.factor(tab$BAS_REV)
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
  tab$REV_CLASSE[tab$REV_CLASSE=="INC"] <- NA
  tab$REV_CLASSE <- as.factor(as.character((tab$REV_CLASSE)))
  tab$REV_CLASSE <- factor(tab$REV_CLASSE, levels = c("HAUT","BAS_REV","MOY"))#, "INC"))
  #tab$REV_CLASSE <- factor(tab$REV_CLASSE, levels = c("HAUT","BAS_REV","MOY"))
  tab$Classe <- as.factor(as.character((tab$Classe)))
  tab$Classe <- factor(tab$Classe, levels = c("Classe 1","Classe 2","Classe 3","Classe 4","Classe 5"))
  # revenus sans les fragiles
  tab$AGE <- tab$AGE/10
  #tab$NBACHADR_P1 <- tab$NBACHADR_P1/5
  tab$NATIONALITE2 <- tab$NATIOF
  tab$NATIONALITE2[tab$NATIONALITE2=="3"] <- "2"
  tab$NATIONALITE2 <- as.factor(tab$NATIONALITE2)
  tab$NATIONALITE2 <- factor(tab$NATIONALITE2, levels = c("1","2"))
  
  tab$ParcAL_P1 <- as.factor(as.character(tab$ParcAL_P1))
  tab$ParcAL_P1 <- factor(tab$ParcAL_P1, levels = c("Pas d'AL","prive","social","acces","foyer"))
  
  tab$DUROCC_P1 <- as.factor(as.character(tab$DUROCC_P1))
  tab$DUROCC_P1 <- factor(tab$DUROCC_P1, levels = c("D3_10","D1_3","Dinf1","Dsup10"))
  
  tab$CLASS_SITFAM_P1[tab$CLASS_SITFAM_P1=="AUTRE"] <- NA
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
                "PERSCOUV_P1","SITFAM_P1_DET", "RET_RES_P1", "RET_RES_P2")]
  
  #, "BAS_REV","QPV","GPE","DEG_DENS")]
  colnames(tab) <- c("Déménager","Changer de commune", "Quitter Paris","Quitter l'IDF",
                     "Composition familiale" , "Sexe" , "Age" ,
                     "Revenu", "Aide au logement" , "Séparation" , "Diminution de la taille du foyer","Nouvel enfant" ,
                     "Mise en couple", "Nationalité" ,"Nationalité2", "Activité",
                     "Evenements profesionnels","Revenus du patrimoine" ,
                     "Catégorie de bénéficiaire","Type de marché", "Distance au centre",
                     "Noyau-dur 2019","Handicap", "Parc AL","Durée d'occupation","Classe durée d'occupation","IDUNI","X_18","Y_18","X_19","Y_19","NUMCOMDO_P2",
                     "PERSCOUV_P1", "Composition familiale détaillée", "RET_RES_P1", "RET_RES_P2")
  #,"Bas revenus", "Depuis un QPV", "Depuis un secteur de gare","Degré de densité")
  
  tab[,5] <- fct_recode(tab[,5],
                        "Personne seule"= "ISOLE",
                        "Couple"= "COUPLE",
                        "Couple avec enfant(s)"= "COU_ENF",
                        "Famille monoparentale"= "MONOP")
                       #,"Autre"="AUTRE")
  tab[,5] <- factor(tab[,5], levels = c("Couple avec enfant(s)","Personne seule","Couple","Famille monoparentale"))#,"Autre"))
  tab[,6] <- fct_recode(tab[,6],
                        #"Féminin"= "0",
                        "Masculin"= "1")
  #tab[,6] <- factor(tab[,6], levels = c("Féminin","Masculin"))
  
  tab[,8] <- fct_recode(tab[,8],
                        "Bas revenus"= "BAS_REV",
                        "Modérés"="MOY",
                        "Plus élevés"="HAUT")
                       #,"Inconnus"="INC")
  tab[,14] <- fct_recode(tab[,14],
                         "Français"= "1",
                         # "Etranger"= "2")
                         "Etranger CEE"= "2",
                         "Etranger hors CEE"="3")
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
                         "Non"="NON",
                         "Inf. 6000€/an"= "INF_6K",
                         "Sup. 6000€/an"= "SUP_6K")
  
  tab[,34] <- fct_recode(tab[,34],
                         "Femme seule"= "Féminin_ISOLE",
                         "Homme seul"= "1_ISOLE",
                         "Couple"= "Féminin_COUPLE",
                         "Couple"= "1_COUPLE",
                         "Couple avec enfant(s)"= "Féminin_COU_ENF",
                         "Couple avec enfant(s)"= "1_COU_ENF",
                         "Famille monoparentale (femme)"= "Féminin_MONOP",
                         "Famille monoparentale (homme)"= "1_MONOP",
                         "Autre" = "Féminin_NA",
                         "Autre" = "1_NA"
                         )
  tab$`Composition familiale détaillée`[tab$`Composition familiale détaillée`=="Autre"] <- NA
  tab[,34] <- as.factor(as.character(tab[,34]))
  tab[,34] <- factor(tab[,34], levels = c("Couple avec enfant(s)","Femme seule","Homme seul","Couple","Famille monoparentale (femme)","Famille monoparentale (homme)"))
  return(tab)
}

sel <- selection(sel)
summary(sel)

table(sel$Activité, sel$Revenu, useNA = "ifany")

Mcouple <- sel[sel$`Mise en couple`=="1",]
non_Mcouple <- sel[sel$`Mise en couple`=="0",]

Mcouple <- as.data.frame(table(Mcouple$`Type de marché`, Mcouple$Déménager)) %>% tidyr::spread(Var2, Freq)
colnames(Mcouple) <- c("Classe","Stables","Déménagement")
Mcouple$Total <- Mcouple$Stables + Mcouple$Déménagement
Mcouple$TX_dem_couple <- round(Mcouple$Déménagement / Mcouple$Total *100, 2)  

non_Mcouple <- as.data.frame(table(non_Mcouple$`Type de marché`, non_Mcouple$Déménager)) %>% tidyr::spread(Var2, Freq)
colnames(non_Mcouple) <- c("Classe","Stables","Déménagement")
non_Mcouple$Total <- non_Mcouple$Stables + non_Mcouple$Déménagement
non_Mcouple$TX_dem_Non_couple <- round(non_Mcouple$Déménagement / non_Mcouple$Total *100, 2)  

Mcouple
non_Mcouple

freq_Mcouple <- as.data.frame(table(sel$`Type de marché`, sel$`Mise en couple` )) %>% tidyr::spread(Var2, Freq)
colnames(freq_Mcouple) <- c("Classe","Non","Mcouple")
freq_Mcouple$Total <- freq_Mcouple$Non + freq_Mcouple$Mcouple
freq_Mcouple$TX_Mcouple <- round(freq_Mcouple$Mcouple / freq_Mcouple$Total *100, 2)  

# Tableaux de comparaison
table(sel$`Aide au logement`, sel$Déménager )
nrow(sel[is.na(sel$`Composition familiale`),])
24772/nrow(sel)
summary(is.na(sel))

# Part des revenus inconnus 
summary(sel$Revenu)
297438/nrow(sel)

# Bas revenus / Sitfam / AL ----
t1 <- as.data.frame(table(sel$Revenu, sel$`Composition familiale`)) %>% tidyr::spread(Var2, Freq)
t1 <- t1[,c(1,3,4,5,2,6)]
t1 <- t1[c(2,3,1,4),]
colnames(t1)[1] <- "Caractéristiques"
t2 <- as.data.frame(table(sel$`Parc AL`, sel$`Composition familiale`)) %>% tidyr::spread(Var2, Freq)
t2 <- t2[,c(1,3,4,5,2,6)]
colnames(t2)[1] <- "Caractéristiques"
t0 <- rbind(t1,t2)

t1 <- as.data.frame(table(sel$Revenu, sel$`Parc AL`)) %>% tidyr::spread(Var2, Freq)
t1 <- t1[c(2,3,1,4),]
colnames(t1)[1] <- "Caractéristiques"

temp <- sel[,c("Composition familiale", 'Revenu', 'Parc AL')]
cor(temp)
vcd::assocstats

# Taux de mobilité résidentielle ----
t1 <- as.data.frame(table(sel$`Composition familiale`, sel$Déménager)) %>% tidyr::spread(Var2, Freq)
colnames(t1) <- c("Caractéristiques","Stables","Déménagement")
t2 <- as.data.frame(table(sel$Revenu, sel$Déménager)) %>% tidyr::spread(Var2, Freq)
colnames(t2) <- c("Caractéristiques","Stables","Déménagement")
t3 <- as.data.frame(table(sel$`Parc AL`, sel$Déménager)) %>% tidyr::spread(Var2, Freq)
colnames(t3) <- c("Caractéristiques","Stables","Déménagement")
t0 <- rbind(t1,t2,t3)
t0$Total1 <- t0$Stables + t0$Déménagement
t0$TX_dem <- round(t0$Déménagement / t0$Total1 *100, 2)  

t1 <- as.data.frame(table(sel$`Composition familiale`, sel$`Changer de commune`)) %>% tidyr::spread(Var2, Freq)
colnames(t1) <- c("Caractéristiques","Stables2","Changer_com")
t2 <- as.data.frame(table(sel$Revenu, sel$`Changer de commune`)) %>% tidyr::spread(Var2, Freq)
colnames(t2) <- c("Caractéristiques","Stables2","Changer_com")
t3 <- as.data.frame(table(sel$`Parc AL`, sel$`Changer de commune`)) %>% tidyr::spread(Var2, Freq)
colnames(t3) <- c("Caractéristiques","Stables2","Changer_com")
t00 <- rbind(t1,t2,t3)
t00$Total2 <- t00$Stables2 + t00$Changer_com
t00$TX_chang_com <- round(t00$Changer_com / t00$Total2 *100, 2)  
  
Taux_caracts <- cbind(t0, t00)
rm(t0, t00, t1, t2, t3)
nrow(sel[sel$Revenu=="Bas revenus" & !is.na(sel$Revenu),])

# Desc marchés/déménager/changc ----
temp <- as.data.frame(table(sel$`Type de marché`, sel$Déménager))
temp <- temp[temp$Var2==1,3]
marches_desc$Déménagements <- temp
marches_desc$TX_Mob_Foyers <- temp / marches_desc$Foyers
temp <- as.data.frame(table(sel$`Type de marché`, sel$`Changer de commune`))
temp <- temp[temp$Var2==1,3]
marches_desc$Changc <- temp
marches_desc$TX_Changc_Foyers <- temp / marches_desc$Foyers
marches_desc$TX_ChangcDEM_Foyers <- temp / marches_desc$Déménagements
rm(temp)

# vérification distributon de l'age
ggplot(sel, aes(x=Age)) + geom_density()
temp <- as.data.frame(summary(sel$Revenu))

# Vérif ND ----

# temp <- sel[sel$`Noyau-dur 2019`=="OUI",]
# summary(temp$Séparation)
# summary(temp$`Nouvel enfant`)
# summary(temp$`Mise en couple`)
# summary(temp$`Diminution de la taille du foyer`)
# summary(temp$`Evenements profesionnels`)
# summary(temp$Handicap)
# temp1 <- sel[sel$`Noyau-dur 2019`=="NON",]
# summary(temp1$Séparation)
# summary(temp1$`Nouvel enfant`)
# summary(temp1$`Mise en couple`)
# summary(temp1$`Evenements profesionnels`)
# summary(temp1$`Diminution de la taille du foyer`)
# summary(temp1$Handicap)
# rm(temp, temp1)

# Modele exemple ----

# Dummy pour l'exemple sur la regression logistique: 
DEM_DUM <- glm(`Déménager` ~ Age + `Composition familiale`, data=sel, family = binomial(logit))
PseudoR2(DEM_DUM, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
as.data.frame(DEM_DUM$coefficients)
summ(DEM_DUM, exp = TRUE, pvals = TRUE)
summary(sel$`Déménager`)
nobs(DEM_DUM)
nrow(DEM_DUM$model[DEM_DUM$model$Déménager==1,])

# Modèles généraux ----

sel1 <- sel
sel1$`Type de marché` <- "IDF"
sel1$`Type de marché` <- as.character(sel1$`Type de marché`)
sel3 <- sel
sel3$`Type de marché` <- as.character(sel3$`Type de marché`)
sel2 <- rbind(sel1, sel3)
sel2$`Type de marché` <- as.factor(sel2$`Type de marché`)
sel2$`Type de marché` <- factor(sel2$`Type de marché`, levels = c("IDF","Classe 1","Classe 2","Classe 3","Classe 4","Classe 5"))

summary(sel$Séparation)
table(sel$Séparation,sel$`Type de marché`)

# Déménager en Ile-de-France 
DEM_IDF <- glm(`Déménager` ~
                 Age
               + Nationalité
               + Handicap
               + `Composition familiale`
               #+ `Composition familiale détaillée`
               + Séparation
               +`Nouvel enfant`
               + `Mise en couple`
               + Activité
               + `Evenements profesionnels`
               + Revenu
               + `Parc AL`
               + `Classe durée d'occupation`
               , data=sel, family = binomial(logit))
car::vif(DEM_IDF)
??PseudoR2
PseudoR2(DEM_IDF, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(DEM_IDF, exp = TRUE, pvals = TRUE)
sum_demIDF <- summary(sel$`Déménager`)

summary(sel$`Composition familiale`)
summary(sel$Revenu)
summary(sel$`Evenements profesionnels`)


nrow(sel[is.na(sel$Activité)|is.na(sel$`Evenements profesionnels`),])
# perte : 
nobs(DEM_IDF)
(nrow(sel) - nobs(DEM_IDF))/nrow(sel)
summary(as.factor(sel$`Composition familiale détaillée`))
# Changer de commune en Ile-de-France
#CHANGC_IDF_2 <- glm(`Changer de commune` ~
CHANGC_IDF <- glm(`Changer de commune` ~
                    Age
                  + Nationalité
                  + Handicap
                  #+ `Composition familiale`
                  + `Composition familiale détaillée`
                  + Séparation
                  +`Nouvel enfant`
                  + `Mise en couple`
                  + Activité
                  + `Evenements profesionnels`
                  + Revenu
                  + `Parc AL`
                  + `Classe durée d'occupation`
                  , data=sel, family = binomial(logit))
car::vif(CHANGC_IDF)
PseudoR2(CHANGC_IDF, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_IDF, exp = TRUE, pvals = TRUE)
sum_CHANGCIDF <- summary(sel$`Changer de commune`)

#saveRDS(CHANGC_IDF_2, "CHANGC_IDF_2.rds")

# Changer de commune lors d'un déménagement
CHANGC_IDF_DEM <- glm(`Changer de commune` ~
                        Age
                      + Nationalité
                      + Handicap
                      + `Composition familiale`
                      #+ `Composition familiale détaillée`
                      + Séparation
                      +`Nouvel enfant`
                      + `Mise en couple`
                      + Activité
                      + `Evenements profesionnels`
                      + Revenu
                      + `Parc AL`
                      + `Classe durée d'occupation`
                      , data=sel[sel$Déménager=="1",], family = binomial(logit))
car::vif(CHANGC_IDF_DEM)
PseudoR2(CHANGC_IDF_DEM, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_IDF_DEM, exp = TRUE, pvals = TRUE)
sum_CHANGCIDF_DEM <- summary(sel$`Changer de commune`[sel$Déménager=="1"])

# Modèles G + marchés ----
DEM_IDF_marches <- glm(`Déménager` ~
                         Age
                       + Nationalité
                       + Handicap
                       #+ `Composition familiale`
                       + `Composition familiale détaillée`
                       + Séparation
                       +`Nouvel enfant`
                       + `Mise en couple`
                       + Activité
                       + `Evenements profesionnels`
                       + Revenu
                       + `Parc AL`
                       + `Classe durée d'occupation`
                       + `Type de marché`
                       , data=sel, family = binomial(logit))
car::vif(DEM_IDF_marches)
PseudoR2(DEM_IDF_marches, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(DEM_IDF_marches, exp = TRUE, pvals = TRUE)
sum_DEM_IDF_marches <- summary(sel$`Déménager`)

# Changer de commune en Ile-de-France
CHANGC_IDF_marche <- glm(`Changer de commune` ~
                           Age
                         + Nationalité
                         + Handicap
                         #+ `Composition familiale`
                         + `Composition familiale détaillée`
                         + Séparation
                         +`Nouvel enfant`
                         + `Mise en couple`
                         + Activité
                         + `Evenements profesionnels`
                         + Revenu
                         + `Parc AL`
                         + `Classe durée d'occupation`
                         + `Type de marché`
                         , data=sel, family = binomial(logit))
car::vif(CHANGC_IDF_marche)
PseudoR2(CHANGC_IDF_marche, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_IDF_marche, exp = TRUE, pvals = TRUE)
sum_CHANGC_IDF_marche <- summary(sel$`Changer de commune`)


CHANGC_IDF_DEM_marche <- glm(`Changer de commune` ~
                               Age
                             + Nationalité
                             + Handicap
                             #+ `Composition familiale`
                             + `Composition familiale détaillée`
                             + Séparation
                             +`Nouvel enfant`
                             + `Mise en couple`
                             + Activité
                             + `Evenements profesionnels`
                             + Revenu
                             + `Parc AL`
                             + `Classe durée d'occupation`
                             + `Type de marché`
                             , data=sel[sel$Déménager=="1",], family = binomial(logit))
car::vif(CHANGC_IDF_DEM_marche)
PseudoR2(CHANGC_IDF_DEM_marche, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_IDF_DEM_marche, exp = TRUE, pvals = TRUE)
sum_CHANGC_IDF_DEM_marche <- summary(sel$`Changer de commune`[sel$Déménager=="1"])

# Modèles marchés ----

# Parc 1
DEM_parc1 <- glm(`Déménager` ~
                   Age
                 + Nationalité
                 + Handicap
                 + `Composition familiale`
                 + Séparation
                 +`Nouvel enfant`
                 + `Mise en couple`
                 + Activité
                 + `Evenements profesionnels`
                 + Revenu
                 + `Parc AL`
                 + `Classe durée d'occupation`
                 , data=sel[sel$`Type de marché`=="Classe 1",], family = binomial(logit))
car::vif(DEM_parc1)
PseudoR2(DEM_parc1, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(DEM_parc1, exp = TRUE, pvals = TRUE)
sum_DEMparc1 <- summary(sel$`Déménager`[sel$`Type de marché`=="Classe 1"])

CHANGC_parc1 <- glm(`Changer de commune` ~
                      Age
                    + Nationalité
                    + Handicap
                    + `Composition familiale`
                    + Séparation
                    +`Nouvel enfant`
                    + `Mise en couple`
                    + Activité
                    + `Evenements profesionnels`
                    + Revenu
                    + `Parc AL`
                    + `Classe durée d'occupation`
                    , data=sel[sel$`Type de marché`=="Classe 1",], family = binomial(logit))
car::vif(CHANGC_parc1)
PseudoR2(CHANGC_parc1, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_parc1, exp = TRUE, pvals = TRUE)
sum_CHANGCparc1 <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 1"])

CHANGC_parc1_B <- glm(`Changer de commune` ~
                        Age
                      + Nationalité
                      + Handicap
                      + `Composition familiale`
                      + Séparation
                      +`Nouvel enfant`
                      + `Mise en couple`
                      + Activité
                      + `Evenements profesionnels`
                      + Revenu
                      + `Parc AL`
                      + `Classe durée d'occupation`
                      , data=sel[sel$`Type de marché`=="Classe 1" & sel$Déménager=="1",], family = binomial(logit))
car::vif(CHANGC_parc1_B)
PseudoR2(CHANGC_parc1_B, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_parc1_B, exp = TRUE, pvals = TRUE)
sum_CHANGCparc1_B <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 1" & sel$Déménager=="1"])

# Parc 2
DEM_parc2 <- glm(`Déménager` ~
                   Age
                 + Nationalité
                 + Handicap
                 + `Composition familiale`
                 + Séparation
                 +`Nouvel enfant`
                 + `Mise en couple`
                 + Activité
                 + `Evenements profesionnels`
                 + Revenu
                 + `Parc AL`
                 + `Classe durée d'occupation`
                 , data=sel[sel$`Type de marché`=="Classe 2",], family = binomial(logit))
car::vif(DEM_parc2)
PseudoR2(DEM_parc2, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(DEM_parc2, exp = TRUE, pvals = TRUE)
sum_DEMparc2 <- summary(sel$`Déménager`[sel$`Type de marché`=="Classe 2"])

CHANGC_parc2 <- glm(`Changer de commune` ~
                      Age
                    + Nationalité
                    + Handicap
                    + `Composition familiale`
                    + Séparation
                    +`Nouvel enfant`
                    + `Mise en couple`
                    + Activité
                    + `Evenements profesionnels`
                    + Revenu
                    + `Parc AL`
                    + `Classe durée d'occupation`
                    , data=sel[sel$`Type de marché`=="Classe 2",], family = binomial(logit))
car::vif(CHANGC_parc2)
PseudoR2(CHANGC_parc2, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_parc2, exp = TRUE, pvals = TRUE)
sum_CHANGCparc2 <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 2"])

CHANGC_parc2_B <- glm(`Changer de commune` ~
                        Age
                      + Nationalité
                      + Handicap
                      + `Composition familiale`
                      + Séparation
                      +`Nouvel enfant`
                      + `Mise en couple`
                      + Activité
                      + `Evenements profesionnels`
                      + Revenu
                      + `Parc AL`
                      + `Classe durée d'occupation`
                      , data=sel[sel$`Type de marché`=="Classe 2" & sel$Déménager=="1",], family = binomial(logit))
# car::vif(CHANGC_parc2_B)
PseudoR2(CHANGC_parc2_B, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(CHANGC_parc2_B, exp = TRUE, pvals = TRUE)
sum_CHANGCparc2_B <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 2" & sel$Déménager=="1"])

# Parc 3
DEM_parc3 <- glm(`Déménager` ~
                   Age
                 + Nationalité
                 + Handicap
                 + `Composition familiale`
                 + Séparation
                 +`Nouvel enfant`
                 + `Mise en couple`
                 + Activité
                 + `Evenements profesionnels`
                 + Revenu
                 + `Parc AL`
                 + `Classe durée d'occupation`
                 , data=sel[sel$`Type de marché`=="Classe 3",], family = binomial(logit))
# car::vif(DEM_parc3)
PseudoR2(DEM_parc3, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(DEM_parc3, exp = TRUE, pvals = TRUE)
sum_DEMparc3 <- summary(sel$`Déménager`[sel$`Type de marché`=="Classe 3"])

CHANGC_parc3 <- glm(`Changer de commune` ~
                      Age
                    + Nationalité
                    + Handicap
                    + `Composition familiale`
                    + Séparation
                    +`Nouvel enfant`
                    + `Mise en couple`
                    + Activité
                    + `Evenements profesionnels`
                    + Revenu
                    + `Parc AL`
                    + `Classe durée d'occupation`
                    , data=sel[sel$`Type de marché`=="Classe 3",], family = binomial(logit))
# car::vif(CHANGC_parc3)
PseudoR2(CHANGC_parc3, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(CHANGC_parc3, exp = TRUE, pvals = TRUE)
sum_CHANGCparc3 <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 3"])

CHANGC_parc3_B <- glm(`Changer de commune` ~
                        Age
                      + Nationalité
                      + Handicap
                      + `Composition familiale`
                      + Séparation
                      +`Nouvel enfant`
                      + `Mise en couple`
                      + Activité
                      + `Evenements profesionnels`
                      + Revenu
                      + `Parc AL`
                      + `Classe durée d'occupation`
                      , data=sel[sel$`Type de marché`=="Classe 3" & sel$Déménager=="1",], family = binomial(logit))
# car::vif(CHANGC_parc3_B)
PseudoR2(CHANGC_parc3_B, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_parc3_B, exp = TRUE, pvals = TRUE)
sum_CHANGCparc3_B <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 3" & sel$Déménager=="1"])

# Parc 4
DEM_parc4 <- glm(`Déménager` ~
                   Age
                 + Nationalité
                 + Handicap
                 + `Composition familiale`
                 + Séparation
                 +`Nouvel enfant`
                 + `Mise en couple`
                 + Activité
                 + `Evenements profesionnels`
                 + Revenu
                 + `Parc AL`
                 + `Classe durée d'occupation`
                 , data=sel[sel$`Type de marché`=="Classe 4",], family = binomial(logit))
# car::vif(DEM_parc4)
# PseudoR2(DEM_parc4, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(DEM_parc4, exp = TRUE, pvals = TRUE)
sum_DEMparc4 <- summary(sel$`Déménager`[sel$`Type de marché`=="Classe 4"])

CHANGC_parc4 <- glm(`Changer de commune` ~
                      Age
                    + Nationalité
                    + Handicap
                    + `Composition familiale`
                    + Séparation
                    +`Nouvel enfant`
                    + `Mise en couple`
                    + Activité
                    + `Evenements profesionnels`
                    + Revenu
                    + `Parc AL`
                    + `Classe durée d'occupation`
                    , data=sel[sel$`Type de marché`=="Classe 4",], family = binomial(logit))
# car::vif(CHANGC_parc4)
# PseudoR2(CHANGC_parc4, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(CHANGC_parc4, exp = TRUE, pvals = TRUE)
sum_CHANGCparc4 <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 4"])

CHANGC_parc4_B <- glm(`Changer de commune` ~
                        Age
                      + Nationalité
                      + Handicap
                      + `Composition familiale`
                      + Séparation
                      +`Nouvel enfant`
                      + `Mise en couple`
                      + Activité
                      + `Evenements profesionnels`
                      + Revenu
                      + `Parc AL`
                      + `Classe durée d'occupation`
                      , data=sel[sel$`Type de marché`=="Classe 4"& sel$Déménager=="1",], family = binomial(logit))
# car::vif(CHANGC_parc4_B)
# PseudoR2(CHANGC_parc4_B, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(CHANGC_parc4_B, exp = TRUE, pvals = TRUE)
sum_CHANGCparc4_B <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 4"& sel$Déménager=="1"])

# Parc 5
DEM_parc5 <- glm(`Déménager` ~
                   Age
                 + Nationalité
                 + Handicap
                 + `Composition familiale`
                 + Séparation
                 +`Nouvel enfant`
                 + `Mise en couple`
                 + Activité
                 + `Evenements profesionnels`
                 + Revenu
                 + `Parc AL`
                 + `Classe durée d'occupation`
                 , data=sel[sel$`Type de marché`=="Classe 5",], family = binomial(logit))
# car::vif(DEM_parc5)
# PseudoR2(DEM_parc5, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(DEM_parc5, exp = TRUE, pvals = TRUE)
sum_DEMparc5 <- summary(sel$`Déménager`[sel$`Type de marché`=="Classe 5"])

CHANGC_parc5 <- glm(`Changer de commune` ~
                      Age
                    + Nationalité
                    + Handicap
                    + `Composition familiale`
                    + Séparation
                    +`Nouvel enfant`
                    + `Mise en couple`
                    + Activité
                    + `Evenements profesionnels`
                    + Revenu
                    + `Parc AL`
                    + `Classe durée d'occupation`
                    , data=sel[sel$`Type de marché`=="Classe 5",], family = binomial(logit))
# car::vif(CHANGC_parc5)
# PseudoR2(CHANGC_parc5, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_parc5, exp = TRUE, pvals = TRUE)
sum_CHANGCparc5 <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 5"])

CHANGC_parc5_B <- glm(`Changer de commune` ~
                        Age
                      + Nationalité
                      + Handicap
                      + `Composition familiale`
                      + Séparation
                      +`Nouvel enfant`
                      + `Mise en couple`
                      + Activité
                      + `Evenements profesionnels`
                      + Revenu
                      + `Parc AL`
                      + `Classe durée d'occupation`
                      , data=sel[sel$`Type de marché`=="Classe 5"& sel$Déménager=="1",], family = binomial(logit))
# car::vif(CHANGC_parc5_B)
PseudoR2(CHANGC_parc5_B, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(CHANGC_parc5_B, exp = TRUE, pvals = TRUE)
sum_CHANGCparc5_B <- summary(sel$`Changer de commune`[sel$`Type de marché`=="Classe 5"& sel$Déménager=="1"])

# Quitter Paris ----
quartiers <- readRDS("quartiersParis.rds") # fond de carte des quartiers parisiens
temp <- st_as_sf(sel[,c("IDUNI","X_18","Y_18")],coords = c("X_18","Y_18"))
st_crs(temp) <- 2154
temp <- st_intersection(quartiers, temp)
temp <- as.data.frame(temp)
temp <- temp[,-c(5)]
colnames(temp)[c(1,2)] <- c("quartier_P1","type_quartiers_P1")
sel2 <- sel[sel$IDUNI %in% temp$IDUNI,]
sel2 <- dplyr::left_join(sel2, temp)
rm(temp)
temp2 <- sel2[,c("IDUNI","X_19","Y_19")]
temp2 <- na.exclude(temp2)
temp2 <- st_as_sf(temp2,coords = c("X_19","Y_19"))
st_crs(temp2) <- 2154
temp2 <- st_intersection(quartiers, temp2)
temp2 <- as.data.frame(temp2)
temp2 <- temp2[,c(1,4)]
colnames(temp2)[1] <- "quartier_P2"
sel2 <- dplyr::left_join(sel2, temp2)
rm(temp2)

sel2$quartier_P2[is.na(sel2$quartier_P2)& substr(sel2$NUMCOMDO_P2,1,2) !="75"] <- "OUT"
sel2$quartier_P2[is.na(sel2$quartier_P2)& substr(sel2$NUMCOMDO_P2,1,2) =="75"] <- NA
sel2$MRS_QUAR <- NA
sel2$MRS_QUAR[!is.na(sel2$quartier_P2)
              & sel2$quartier_P1 == sel2$quartier_P2] <- "NON"
sel2$MRS_QUAR[!is.na(sel2$quartier_P2)
              & sel2$quartier_P1 != sel2$quartier_P2] <- "OUI"
sel2$MRS_QUAR <- as.factor(sel2$MRS_QUAR)
sel2$MRS_QUAR <- factor(sel2$MRS_QUAR, levels = c("NON","OUI"))

# Part globale des MRS Paris 
DEM_PARIS <- summary(sel2$Déménager)
MRS_PARIS <- summary(sel2$`Quitter Paris`)
DEM_MRS_PARIS <- summary(sel2$`Quitter Paris`[sel2$Déménager==1])

# Modèle DEM et MRS Paris
DEM_Paris <- glm(Déménager ~
                   Age
                 + Nationalité
                 + Handicap
                 #+ `Composition familiale`
                 + `Composition familiale détaillée`
                 + Séparation
                 +`Nouvel enfant`
                 + `Mise en couple`
                 + Activité
                 + `Evenements profesionnels`
                 + Revenu
                 + `Parc AL`
                 + `Classe durée d'occupation`
                 , data=sel2, family = binomial(logit))
car::vif(DEM_Paris)
PseudoR2(DEM_Paris, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(DEM_Paris, exp = TRUE, pvals = TRUE)
sum_DEM_Paris <- summary(sel2$Déménager) # Annexe

QUITTER_Paris <- glm(`Quitter Paris` ~
                       Age
                     + Nationalité
                     + Handicap
                     #+ `Composition familiale`
                     + `Composition familiale détaillée`
                     + Séparation
                     +`Nouvel enfant`
                     + `Mise en couple`
                     + Activité
                     + `Evenements profesionnels`
                     + Revenu
                     + `Parc AL`
                     + `Classe durée d'occupation`
                     , data=sel2, family = binomial(logit))
car::vif(QUITTER_Paris)
PseudoR2(QUITTER_Paris, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(QUITTER_Paris, exp = TRUE, pvals = TRUE)
sum_QUITparis <- summary(sel2$`Quitter Paris`)
nobs(QUITTER_Paris)
nrow(QUITTER_Paris$model[QUITTER_Paris$model$`Quitter Paris`==1,])
nrow(sel2[sel2$`Quitter Paris`==1,])

QUITTER_Paris_DEM <- glm(`Quitter Paris` ~
                           Age
                         + Nationalité
                         + Handicap
                         #+ `Composition familiale`
                         + `Composition familiale détaillée`
                         + Séparation
                         +`Nouvel enfant`
                         + `Mise en couple`
                         + Activité
                         + `Evenements profesionnels`
                         + Revenu
                         + `Parc AL`
                         + `Classe durée d'occupation`
                         , data=sel2[sel2$Déménager==1,], family = binomial(logit))
car::vif(QUITTER_Paris_DEM)
PseudoR2(QUITTER_Paris_DEM, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
summ(QUITTER_Paris_DEM, exp = TRUE, pvals = TRUE)
sum_QUITTER_Paris_DEM <- summary(sel2$`Quitter Paris`[sel2$Déménager==1]) # Annexe

# Quitter son quartier à Paris ---- 

# # Répartition des foyers parisiens par type de quartier, taux de mobilité, taux de sortie
# summary(sel2$MRS_QUAR)
# sel2 <-sel2[!is.na(sel2$MRS_QUAR),]
# sel2$Surface <- sel2$Surface/10000 # DU m2 à l'hectare
# foyers_quartiers <- summary(sel2$type_quartiers_P1)
# perscouv_quartiers <- as.data.frame(tapply(sel2[, "PERSCOUV_P1"], sel2$type_quartiers_P1, sum))
# quartiers_desc <- cbind(perscouv_quartiers, foyers_quartiers)
# colnames(quartiers_desc) <- c("Personnes couvertes", "Foyers")
# temp <- as.data.frame(table(sel2$type_quartiers_P1, sel2$Déménager))
# temp <- temp[temp$Var2==1,]
# quartiers_desc$TX_Mob_Foyers <- temp$Freq / quartiers_desc$Foyers
# rm(temp)
# temp <- as.data.frame(table(sel2$type_quartiers_P1, sel2$MRS_QUAR))
# temp <- temp[temp$Var2=="OUI",]
# quartiers_desc$TX_MRS_QUA_Foyers <- temp$Freq / quartiers_desc$Foyers
# rm(temp)
# 
# # Part des déménagements parisiens impliquant un changement de quartier: 
# round(summary(sel2$MRS_QUAR)[2]/summary(sel2$Déménager)[2]*100,2)
# # Conclusion: on ne découpe plus en trois car 95% des déménagements à Paris impliquent de quitter son quartier
# 
# QUITTER_QUAR_G <- glm(MRS_QUAR ~
#                            Age
#                          + Nationalité
#                          + Handicap
#                          + `Composition familiale`
#                          + Séparation
#                          +`Nouvel enfant`
#                          + `Mise en couple`
#                          + Activité
#                          + `Evenements profesionnels`
#                          + Revenu
#                          + `Parc AL`
#                          + `Classe durée d'occupation`
#                          + Surface
#                          , data=sel2, family = binomial(logit))
# car::vif(QUITTER_QUAR_G)
# PseudoR2(QUITTER_QUAR_G, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(QUITTER_QUAR_G, exp = TRUE, pvals = TRUE)
# sum_QUITTER_QUAR_G <- summary(sel2$MRS_QUAR)
# 
# QUITTER_QUAR_type <- glm(MRS_QUAR ~
#                       Age
#                     + Nationalité
#                     + Handicap
#                     + `Composition familiale`
#                     + Séparation
#                     +`Nouvel enfant`
#                     + `Mise en couple`
#                     + Activité
#                     + `Evenements profesionnels`
#                     + Revenu
#                     + `Parc AL`
#                     + `Classe durée d'occupation`
#                     + Surface
#                     + type_quartiers_P1
#                     , data=sel2, family = binomial(logit))
# car::vif(QUITTER_QUAR_type)
# PseudoR2(QUITTER_QUAR_type, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(QUITTER_QUAR_type, exp = TRUE, pvals = TRUE)
# sum_QUITTER_QUAR_type<- summary(sel2$MRS_QUAR)
# 
# # Modèle par type de quartiers
# summary(sel2$type_quartiers_P1)
# 
# # Plus cher et plus tendu
# QUIT_QUAR_PP <- glm(MRS_QUAR ~
#                       Age
#                     + Nationalité
#                     + Handicap
#                     + `Composition familiale`
#                     + Séparation
#                     +`Nouvel enfant`
#                     + `Mise en couple`
#                     + Activité
#                     + `Evenements profesionnels`
#                     + Revenu
#                     + `Parc AL`
#                     + `Classe durée d'occupation`
#                     + Surface
#                     , data=sel2[sel2$type_quartiers_P1=="P_PX_P_BNB",], family = binomial(logit))
# car::vif(QUIT_QUAR_PP)
# PseudoR2(QUIT_QUAR_PP, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(QUIT_QUAR_PP, exp = TRUE, pvals = TRUE)
# sum_QUIT_QUAR_PP <- summary(sel2$MRS_QUAR[sel2$type_quartiers_P1=="P_PX_P_BNB"])
# 
# # Plus cher et moins tendu 
# QUIT_QUAR_PM <- glm(MRS_QUAR ~
#                       Age
#                     + Nationalité
#                     + Handicap
#                     + `Composition familiale`
#                     + Séparation
#                     +`Nouvel enfant`
#                     + `Mise en couple`
#                     + Activité
#                     + `Evenements profesionnels`
#                     + Revenu
#                     + `Parc AL`
#                     + `Classe durée d'occupation`
#                     + Surface
#                     , data=sel2[sel2$type_quartiers_P1=="P_PX_M_BNB",], family = binomial(logit))
# car::vif(QUIT_QUAR_PM)
# PseudoR2(QUIT_QUAR_PM, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(QUIT_QUAR_PM, exp = TRUE, pvals = TRUE)
# sum_QUIT_QUAR_PM <- summary(sel2$MRS_QUAR[sel2$type_quartiers_P1=="P_PX_M_BNB"])
# 
# # Moins cher et plus tendu 
# QUIT_QUAR_MP <- glm(MRS_QUAR ~
#                       Age
#                     + Nationalité
#                     + Handicap
#                     + `Composition familiale`
#                     + Séparation
#                     +`Nouvel enfant`
#                     + `Mise en couple`
#                     + Activité
#                     + `Evenements profesionnels`
#                     + Revenu
#                     + `Parc AL`
#                     + `Classe durée d'occupation`
#                     + Surface
#                     , data=sel2[sel2$type_quartiers_P1=="M_PX_P_BNB",], family = binomial(logit))
# car::vif(QUIT_QUAR_MP)
# PseudoR2(QUIT_QUAR_MP, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(QUIT_QUAR_MP, exp = TRUE, pvals = TRUE)
# sum_QUIT_QUAR_MP <- summary(sel2$MRS_QUAR[sel2$type_quartiers_P1=="M_PX_P_BNB"])
# 
# # Moins cher et moins tendu 
# QUIT_QUAR_MM <- glm(`Quitter Paris` ~
#                       Age
#                     + Nationalité
#                     + Handicap
#                     + `Composition familiale`
#                     + Séparation
#                     +`Nouvel enfant`
#                     + `Mise en couple`
#                     + Activité
#                     + `Evenements profesionnels`
#                     + Revenu
#                     + `Parc AL`
#                     + `Classe durée d'occupation`
#                     + Surface
#                     , data=sel2[sel2$type_quartiers_P1=="M_PX_M_BNB",], family = binomial(logit))
# car::vif(QUIT_QUAR_MM)
# PseudoR2(QUIT_QUAR_MM, which = c("McKelveyZavoina","McFadden","Nagelkerke"))
# summ(QUIT_QUAR_MM, exp = TRUE, pvals = TRUE)
# sum_QUIT_QUAR_MM <- summary(sel2$MRS_QUAR[sel2$type_quartiers_P1=="M_PX_M_BNB"])

rm(sel,result_cah, quartiers, map_result)
save.image("chap4_modeles_VF3.RData")

# Mise en forme des résultats ----

load("chap4_modeles_VF3.RData")
pla_typo <- RColorBrewer::brewer.pal(6, "Set3")[c(5,4,6,1,3)]
CHANGC_IDF_2 <- readRDS("CHANGC_IDF_2.rds")

# Annexe 2
nobs(DEM_IDF)
nrow(DEM_IDF$model[DEM_IDF$model$Déménager==1,])
nobs(DEM_IDF_marches)
nrow(DEM_IDF_marches$model[DEM_IDF_marches$model$Déménager==1,])

nobs(CHANGC_IDF)
nrow(CHANGC_IDF$model[CHANGC_IDF$model$`Changer de commune`==1,])
nobs(CHANGC_IDF_marche)
nrow(CHANGC_IDF_marche$model[CHANGC_IDF_marche$model$`Changer de commune`==1,])

nobs(CHANGC_IDF_DEM)
nrow(CHANGC_IDF_DEM$model[CHANGC_IDF_DEM$model$`Changer de commune`==1,])
nobs(CHANGC_IDF_DEM_marche)
nrow(CHANGC_IDF_DEM_marche$model[CHANGC_IDF_DEM_marche$model$`Changer de commune`==1,])

# Annexes 3, 4 et 5
nobs(DEM_parc1)
nrow(DEM_parc1$model[DEM_parc1$model$Déménager==1,])
nobs(DEM_parc2)
nrow(DEM_parc2$model[DEM_parc2$model$Déménager==1,])
nobs(DEM_parc3)
nrow(DEM_parc3$model[DEM_parc3$model$Déménager==1,])
nobs(DEM_parc4)
nrow(DEM_parc4$model[DEM_parc4$model$Déménager==1,])
nobs(DEM_parc5)
nrow(DEM_parc5$model[DEM_parc5$model$Déménager==1,])

nobs(CHANGC_parc1)
nrow(CHANGC_parc1$model[CHANGC_parc1$model$`Changer de commune`==1,])
nobs(CHANGC_parc2)
nrow(CHANGC_parc2$model[CHANGC_parc2$model$`Changer de commune`==1,])
nobs(CHANGC_parc3)
nrow(CHANGC_parc3$model[CHANGC_parc3$model$`Changer de commune`==1,])
nobs(CHANGC_parc4)
nrow(CHANGC_parc4$model[CHANGC_parc4$model$`Changer de commune`==1,])
nobs(CHANGC_parc5)
nrow(CHANGC_parc5$model[CHANGC_parc5$model$`Changer de commune`==1,])
nobs(CHANGC_IDF_2)
nrow(CHANGC_IDF_2$model[CHANGC_IDF_2$model$`Changer de commune`==1,])

nobs(CHANGC_parc1_B)
nrow(CHANGC_parc1_B$model[CHANGC_parc1_B$model$`Changer de commune`==1,])
nobs(CHANGC_parc2_B)
nrow(CHANGC_parc2_B$model[CHANGC_parc2_B$model$`Changer de commune`==1,])
nobs(CHANGC_parc3_B)
nrow(CHANGC_parc3_B$model[CHANGC_parc3_B$model$`Changer de commune`==1,])
nobs(CHANGC_parc4_B)
nrow(CHANGC_parc4_B$model[CHANGC_parc4_B$model$`Changer de commune`==1,])
nobs(CHANGC_parc5_B)
nrow(CHANGC_parc5_B$model[CHANGC_parc5_B$model$`Changer de commune`==1,])
PseudoR2(CHANGC_parc5_B, "McKelveyZavoina")

nobs(DEM_Paris)
nrow(DEM_Paris$model[DEM_Paris$model$Déménager==1,])
nobs(QUITTER_Paris)
nrow(QUITTER_Paris$model[QUITTER_Paris$model$`Quitter Paris`==1,])
nobs(QUITTER_Paris_DEM)
nrow(QUITTER_Paris_DEM$model[QUITTER_Paris_DEM$model$`Quitter Paris`==1,])

# Ajout des pseudo-R2
round(PseudoR2(DEM_IDF, "Nagelkerke"),2)
round(PseudoR2(DEM_IDF_marches, "Nagelkerke"),2)
round(PseudoR2(CHANGC_IDF, "Nagelkerke"),2)
round(PseudoR2(CHANGC_IDF_marche, "Nagelkerke"),2)
round(PseudoR2(CHANGC_IDF_DEM, "Nagelkerke"),2)
round(PseudoR2(CHANGC_IDF_DEM_marche, "Nagelkerke"),2)

round(PseudoR2(DEM_parc1, "Nagelkerke"),2)
round(PseudoR2(DEM_parc2, "Nagelkerke"),2)
round(PseudoR2(DEM_parc3, "Nagelkerke"),2)
round(PseudoR2(DEM_parc4, "Nagelkerke"),2)
round(PseudoR2(DEM_parc5, "Nagelkerke"),2)

round(PseudoR2(CHANGC_parc1, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc2, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc3, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc4, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc5, "Nagelkerke"),2)

round(PseudoR2(CHANGC_parc1_B, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc2_B, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc3_B, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc4_B, "Nagelkerke"),2)
round(PseudoR2(CHANGC_parc5_B, "Nagelkerke"),2)

round(PseudoR2(DEM_Paris, "Nagelkerke"),2)
round(PseudoR2(QUITTER_Paris, "Nagelkerke"),2)
round(PseudoR2(QUITTER_Paris_DEM, "Nagelkerke"),2)

# Annexes ----
summ(DEM_IDF_marches, exp = TRUE, pvals = TRUE)
summ(CHANGC_parc5_B, exp = TRUE, pvals = TRUE)
summ(DEM_IDF_marches, exp = TRUE, pvals = TRUE)

# Annexe colinéarité

t1 <- as.data.frame(car::vif(DEM_IDF))
indicateurs <- rownames(t1)
t1 <- round(t1[,c(1,3)],2)
t2 <- car::vif(CHANGC_IDF)
t2 <- round(t2[,c(1,3)],2)
t3 <- car::vif(CHANGC_IDF_DEM)
t3 <- round(t3[,c(1,3)],2)

t1b <- as.data.frame(car::vif(DEM_IDF_marches))
indicateurs_b <- rownames(t1b)
t1b <- round(t1b[,c(1,3)],2)
t2b <- car::vif(CHANGC_IDF_marche)
t2b <- round(t2b[,c(1,3)],2)
t3b <- car::vif(CHANGC_IDF_DEM_marche)
t3b <- round(t3b[,c(1,3)],2)

t4 <- car::vif(DEM_parc1)
t4 <- round(t4[,c(1,3)],2)
t5 <- car::vif(DEM_parc2)
t5 <- round(t5[,c(1,3)],2)
t6 <- car::vif(DEM_parc3)
t6 <- round(t6[,c(1,3)],2)
t7 <- car::vif(DEM_parc4)
t7 <- round(t7[,c(1,3)],2)
t8 <- car::vif(DEM_parc5)
t8 <- round(t8[,c(1,3)],2)
t9 <- car::vif(CHANGC_parc1)
t9 <- round(t9[,c(1,3)],2)
t10 <- car::vif(CHANGC_parc2)
t10 <- round(t10[,c(1,3)],2)
t11 <- car::vif(CHANGC_parc3)
t11 <- round(t11[,c(1,3)],2)
t12 <- car::vif(CHANGC_parc4)
t12 <- round(t12[,c(1,3)],2)
t13 <- car::vif(CHANGC_parc5)
t13 <- round(t13[,c(1,3)],2)
t14 <- car::vif(CHANGC_parc1_B)
t14 <- round(t14[,c(1,3)],2)
t15 <- car::vif(CHANGC_parc2_B)
t15 <- round(t15[,c(1,3)],2)
t16 <- car::vif(CHANGC_parc3_B)
t16 <- round(t16[,c(1,3)],2)
t17 <- car::vif(CHANGC_parc4_B)
t17 <- round(t17[,c(1,3)],2)
t18 <- car::vif(CHANGC_parc5_B)
t18 <- round(t18[,c(1,3)],2)

t19 <- car::vif(DEM_Paris)
t19 <- round(t19[,c(1,3)],2)
t20 <- car::vif(QUITTER_Paris)
t20 <- round(t20[,c(1,3)],2)
t21 <- car::vif(QUITTER_Paris_DEM)
t21 <- round(t21[,c(1,3)],2)

# t22 <- as.data.frame(car::vif(QUITTER_QUAR_G))
# indicateurs_c <- rownames(t22)
# t22 <- round(t22[,c(1,3)],2)
# t23 <- as.data.frame(car::vif(QUITTER_QUAR_type))
# indicateurs_d <- rownames(t23)
# t23 <- round(t23[,c(1,3)],2)
# 
# t24 <- car::vif(QUIT_QUAR_PP)
# t24 <- round(t24[,c(1,3)],2)
# t25 <- car::vif(QUIT_QUAR_PM)
# t25 <- round(t25[,c(1,3)],2)
# t26 <- car::vif(QUIT_QUAR_MP)
# t26 <- round(t26[,c(1,3)],2)
# t27 <- car::vif(QUIT_QUAR_MM)
# t27 <- round(t27[,c(1,3)],2)

colinearite1 <- cbind(indicateurs, t1, t2, t3)
colinearite1 <- as.data.frame(colinearite1)
colnames(colinearite1) <- c("Facteurs","DEM_IDF","DEM_IDF_adj","CHANGC_IDF", "CHANGC_IDF_adj","CHANGC_IDF_B", "CHANGC_IDF_B_adj")
rownames(colinearite1) <- 1:nrow(colinearite1)

colinearite1b <- cbind(indicateurs_b, t1b, t2b, t3b)
colinearite1b <- as.data.frame(colinearite1b)
colnames(colinearite1b) <- c("Facteurs","DEM_IDF_MAR","DEM_IDF_MAR_adj","CHANGC_IDF_MAR","CHANGC_IDF_MAR_adj","CHANGC_IDF_B_MAR", "CHANGC_IDF_B_MAR_adj")
rownames(colinearite1b) <- 1:nrow(colinearite1b)

colinearite2 <- cbind(indicateurs, t4, t5, t6, t7, t8)
colinearite2 <- as.data.frame(colinearite2)
colnames(colinearite2) <- c("Facteurs","DEM_parc1","DEM_parc1_adj","DEM_parc2","DEM_parc2_adj","DEM_parc3","DEM_parc3_adj","DEM_parc4","DEM_parc4_adj","DEM_parc5", "DEM_parc5_adj")
rownames(colinearite2) <- 1:nrow(colinearite2)
colinearite3 <- cbind(indicateurs, t9, t10, t11, t12,t13)
colinearite3 <- as.data.frame(colinearite3)
colnames(colinearite3) <- c("Facteurs","CHANGC_parc1","CHANGC_parc1_adj","CHANGC_parc2","CHANGC_parc2_adj","CHANGC_parc3","CHANGC_parc3_adj","CHANGC_parc4","CHANGC_parc4_adj","CHANGC_parc5", "CHANGC_parc5_adj")
rownames(colinearite3) <- 1:nrow(colinearite3)
colinearite4 <- cbind(indicateurs, t14, t15, t16, t17,t18)
colinearite4 <- as.data.frame(colinearite4)
colnames(colinearite4) <- c("Facteurs","CHANGC_B_parc1","CHANGC_B_parc1_adj","CHANGC_B_parc2","CHANGC_B_parc2_adj","CHANGC_B_parc3","CHANGC_B_parc3_adj","CHANGC_B_parc4","CHANGC_B_parc4_adj","CHANGC_B_parc5", "CHANGC_B_parc5_adj")
rownames(colinearite4) <- 1:nrow(colinearite4)

colinearite5 <- cbind(indicateurs, t19, t20, t21)
colinearite5 <- as.data.frame(colinearite5)
colnames(colinearite5) <- c("Facteurs","DEM_PARIS","DEM_PARIS_adj","QUITTER_PARIS","QUITTER_PARIS_adj","QUITTER_PARIS_DEM", "QUITTER_PARIS_DEM_adj")
rownames(colinearite5) <- 1:nrow(colinearite5)

# colinearite6 <- cbind(indicateurs_d, c(t22,NA), t23)
# colinearite6 <- as.data.frame(colinearite6)
# colnames(colinearite6) <- c("Facteurs","QUITTER_QUAR_G","QUITTER_QUAR_type")
# rownames(colinearite6) <- 1:nrow(colinearite6)
# 
# colinearite7 <- cbind(indicateurs_c, t24,t25,t26,t27)
# colinearite7 <- as.data.frame(colinearite7)
# colnames(colinearite7) <- c("Facteurs","QUITTER_QUAR_PP","QUITTER_QUAR_PM","QUITTER_QUAR_MP","QUITTER_QUAR_MM")
# rownames(colinearite7) <- 1:nrow(colinearite7)

summ(CHANGC_parc3, exp = TRUE, pvals = TRUE)


# Annexe modèle IDF
tab1 <- as.data.frame(summ(DEM_IDF, exp = TRUE, pvals = TRUE)[1])
tab1 <- tab1[,c(1,5)]
colnames(tab1) <- c("M1","P1")
tab2 <- as.data.frame(summ(CHANGC_IDF, exp = TRUE, pvals = TRUE)[1])
tab2 <- tab2[,c(1,5)]
colnames(tab2) <- c("M2","P2")
tab2_b <- as.data.frame(summ(CHANGC_IDF_DEM, exp = TRUE, pvals = TRUE)[1])
tab2_b <- tab2_b[,c(1,5)]
colnames(tab2_b) <- c("M3","P3")
tab <- cbind(tab1, tab2, tab2_b)
tab$M1 <- round(tab$M1,3)
tab$P1 <- round(tab$P1,3)
tab$M2 <- round(tab$M2,3)
tab$P2 <- round(tab$P2,3)
tab$M3 <- round(tab$M3,3)
tab$P3 <- round(tab$P3,3)
obs <- c(sum_demIDF[1]+sum_demIDF[2],NA, sum_CHANGCIDF[1]+sum_CHANGCIDF[2],NA, sum_CHANGCIDF_DEM[1]+sum_CHANGCIDF_DEM[2],NA)
events <- c(sum_demIDF[2],NA, sum_CHANGCIDF[2],NA, sum_CHANGCIDF_DEM[2],NA)
pseudor <- c(round(PseudoR2(DEM_IDF, which = c("McKelveyZavoina")),2),NA,
             round(PseudoR2(CHANGC_IDF, which = c("McKelveyZavoina")),2),NA,
             round(PseudoR2(CHANGC_IDF_DEM, which = c("McKelveyZavoina")),2),NA)
add2 <- as.data.frame(rbind(obs,events, pseudor))
colnames(add2) <- colnames(tab)
Modeles_IDF <- rbind(tab, add2)
colnames(Modeles_IDF)[c(1,3,5)] <- c("Déménager","Changer de commune","Changer de commune lors d'un déménagement")

# Annexes IDF + parcs
tab1 <- as.data.frame(summ(DEM_IDF_marches, exp = TRUE, pvals = TRUE)[1])
tab1 <- tab1[,c(1,5)]
colnames(tab1) <- c("M1","P1")
tab2 <- as.data.frame(summ(CHANGC_IDF_marche, exp = TRUE, pvals = TRUE)[1])
tab2 <- tab2[,c(1,5)]
colnames(tab2) <- c("M2","P2")
tab2_b <- as.data.frame(summ(CHANGC_IDF_DEM_marche, exp = TRUE, pvals = TRUE)[1])
tab2_b <- tab2_b[,c(1,5)]
colnames(tab2_b) <- c("M3","P3")
tab <- cbind(tab1, tab2, tab2_b)
tab$M1 <- round(tab$M1,3)
tab$P1 <- round(tab$P1,3)
tab$M2 <- round(tab$M2,3)
tab$P2 <- round(tab$P2,3)
tab$M3 <- round(tab$M3,3)
tab$P3 <- round(tab$P3,3)
obs <- c(sum_DEM_IDF_marches[1]+sum_DEM_IDF_marches[2],NA, sum_CHANGC_IDF_marche[1]+sum_CHANGC_IDF_marche[2],NA, sum_CHANGC_IDF_DEM_marche[1]+sum_CHANGC_IDF_DEM_marche[2],NA)
events <- c(sum_DEM_IDF_marches[2],NA, sum_CHANGC_IDF_marche[2],NA, sum_CHANGC_IDF_DEM_marche[2],NA)
pseudor <- c(round(PseudoR2(DEM_IDF_marches, which = c("McKelveyZavoina")),2),NA,
             round(PseudoR2(CHANGC_IDF_marche, which = c("McKelveyZavoina")),2),NA,
             round(PseudoR2(CHANGC_IDF_DEM_marche, which = c("McKelveyZavoina")),2),NA)
add2 <- as.data.frame(rbind(obs,events, pseudor))
colnames(add2) <- colnames(tab)
Modeles_IDF_marche <- rbind(tab, add2)
colnames(Modeles_IDF_marche)[c(1,3,5)] <- c("Déménager","Changer de commune","Changer de commune lors d'un déménagement")
rm(tab2_b)

# Version comparée 
Modeles_IDF_marche_comp <- Modeles_IDF_marche[1:29,c(1,3,5)] - Modeles_IDF[1:29,c(1,3,5)]
Modeles_IDF_marche_comp <- round(Modeles_IDF_marche_comp,2)

# # Annexe parcs déménager
tab1 <- as.data.frame(summ(DEM_parc1, exp = TRUE, pvals = TRUE)[1])
tab1 <- tab1[,c(1,5)]
colnames(tab1) <- c("Parc 1","P1")
tab2 <- as.data.frame(summ(DEM_parc2, exp = TRUE, pvals = TRUE)[1])
tab2 <- tab2[,c(1,5)]
colnames(tab2) <- c("Parc 2","P2")
tab3 <- as.data.frame(summ(DEM_parc3, exp = TRUE, pvals = TRUE)[1])
tab3 <- tab3[,c(1,5)]
colnames(tab3) <- c("Parc 3","P3")
tab4 <- as.data.frame(summ(DEM_parc4, exp = TRUE, pvals = TRUE)[1])
tab4 <- tab4[,c(1,5)]
colnames(tab4) <- c("Parc 4","P4")
tab5 <- as.data.frame(summ(DEM_parc5, exp = TRUE, pvals = TRUE)[1])
tab5 <- tab5[,c(1,5)]
colnames(tab5) <- c("Parc 5","P5")
# tab6 <- as.data.frame(summ(DEM_IDF, exp = TRUE, pvals = TRUE)[1])
# tab6 <- tab6[,c(1,5)]
# colnames(tab6) <- c("DEM_IDF","PX")
tab <- cbind(tab1, tab2, tab3, tab4, tab5)#,
             #tab6)
tab$`Parc 1` <- round(tab$`Parc 1`,3)
tab$P1 <- round(tab$P1,3)
tab$`Parc 2` <- round(tab$`Parc 2`,3)
tab$P2 <- round(tab$P2,3)
tab$`Parc 3` <- round(tab$`Parc 3`,3)
tab$P3 <- round(tab$P3,3)
tab$`Parc 4` <- round(tab$`Parc 4`,3)
tab$P4 <- round(tab$P4,3)
tab$`Parc 5` <- round(tab$`Parc 5`,3)
tab$P5 <- round(tab$P5,3)
#tab$DEM_IDF <- round(tab$DEM_IDF,3)
#tab$PX <- round(tab$PX,3)
obs <- c(sum_DEMparc1[1]+sum_DEMparc1[2],NA, sum_DEMparc2[1]+sum_DEMparc2[2],NA,sum_DEMparc3[1]+sum_DEMparc3[2],NA, sum_DEMparc4[1]+sum_DEMparc4[2],NA,sum_DEMparc5[1]+sum_DEMparc5[2],NA)#,sum_demIDF[1]+sum_demIDF[2],NA)
events <- c(sum_DEMparc1[2],NA, sum_DEMparc2[2],NA, sum_DEMparc3[2],NA, sum_DEMparc4[2],NA,sum_DEMparc5[2],NA)#,sum_demIDF[2],NA)
pseudor <- c(round(PseudoR2(DEM_parc1, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(DEM_parc2, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(DEM_parc3, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(DEM_parc4, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(DEM_parc5, which = c("McKelveyZavoina")),2),NA)#,
             #round(PseudoR2(DEM_IDF, which = c("McKelveyZavoina")),2),NA)
add2 <- as.data.frame(rbind(obs,events, pseudor))
colnames(add2) <- colnames(tab)
Modeles_DEM_parc <- rbind(tab, add2)
#Modeles_DEM_parc <- Modeles_DEM_parc[,c(11,12,1:10)]

# Annexe parcs changer de commune
tab1 <- as.data.frame(summ(CHANGC_parc1, exp = TRUE, pvals = TRUE)[1])
tab1 <- tab1[,c(1,5)]
colnames(tab1) <- c("Parc 1","P1")
tab2 <- as.data.frame(summ(CHANGC_parc2, exp = TRUE, pvals = TRUE)[1])
tab2 <- tab2[,c(1,5)]
colnames(tab2) <- c("Parc 2","P2")
tab3 <- as.data.frame(summ(CHANGC_parc3, exp = TRUE, pvals = TRUE)[1])
tab3 <- tab3[,c(1,5)]
colnames(tab3) <- c("Parc 3","P3")
tab4 <- as.data.frame(summ(CHANGC_parc4, exp = TRUE, pvals = TRUE)[1])
tab4 <- tab4[,c(1,5)]
colnames(tab4) <- c("Parc 4","P4")
tab5 <- as.data.frame(summ(CHANGC_parc5, exp = TRUE, pvals = TRUE)[1])
tab5 <- tab5[,c(1,5)]
colnames(tab5) <- c("Parc 5","P5")
# tab6 <- as.data.frame(summ(CHANGC_IDF_2, exp = TRUE, pvals = TRUE)[1])
# tab6 <- tab6[,c(1,5)]
# colnames(tab6) <- c("CHANGC_IDF","PX")
tab <- cbind(tab1, tab2, tab3, tab4, tab5)
             #,tab6)
tab$`Parc 1` <- round(tab$`Parc 1`,3)
tab$P1 <- round(tab$P1,3)
tab$`Parc 2` <- round(tab$`Parc 2`,3)
tab$P2 <- round(tab$P2,3)
tab$`Parc 3` <- round(tab$`Parc 3`,3)
tab$P3 <- round(tab$P3,3)
tab$`Parc 4` <- round(tab$`Parc 4`,3)
tab$P4 <- round(tab$P4,3)
tab$`Parc 5` <- round(tab$`Parc 5`,3)
tab$P5 <- round(tab$P5,3)
# tab$`CHANGC_IDF` <- round(tab$`CHANGC_IDF`,3)
# tab$PX <- round(tab$PX,3)
obs <- c(sum_CHANGCparc1[1]+sum_CHANGCparc1[2],NA, sum_CHANGCparc2[1]+sum_CHANGCparc2[2],NA,sum_CHANGCparc3[1]+sum_CHANGCparc3[2],NA, sum_CHANGCparc4[1]+sum_CHANGCparc4[2],NA,sum_CHANGCparc5[1]+sum_CHANGCparc5[2],NA)#,
         #sum_CHANGCIDF[1]+sum_CHANGCIDF[2],NA)
events <- c(sum_CHANGCparc1[2],NA, sum_CHANGCparc2[2],NA, sum_CHANGCparc3[2],NA, sum_CHANGCparc4[2],NA, sum_CHANGCparc5[2],NA)
           #, sum_CHANGCIDF[2],NA)
pseudor <- c(round(PseudoR2(CHANGC_parc1, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc2, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc3, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc4, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc5, which = c("McKelveyZavoina")),2),NA)
             #,round(PseudoR2(CHANGC_IDF, which = c("McKelveyZavoina")),2),NA)
add2 <- as.data.frame(rbind(obs,events, pseudor))
colnames(add2) <- colnames(tab)
Modeles_CHANGC_parc <- rbind(tab, add2)
#Modeles_CHANGC_parc <- Modeles_CHANGC_parc[,c(11,12,1:10)]
#save(Modeles_DEM_parc, Modeles_CHANGC_parc,Modeles_CHANGC_parc_B, file = "modeles_parcs.RData")

# Annexe parcs changer de commune lors d'un déménagement
tab1 <- as.data.frame(summ(CHANGC_parc1_B, exp = TRUE, pvals = TRUE)[1])
tab1 <- tab1[,c(1,5)]
colnames(tab1) <- c("Parc 1","P1")
tab2 <- as.data.frame(summ(CHANGC_parc2_B, exp = TRUE, pvals = TRUE)[1])
tab2 <- tab2[,c(1,5)]
colnames(tab2) <- c("Parc 2","P2")
tab3 <- as.data.frame(summ(CHANGC_parc3_B, exp = TRUE, pvals = TRUE)[1])
tab3 <- tab3[,c(1,5)]
colnames(tab3) <- c("Parc 3","P3")
tab4 <- as.data.frame(summ(CHANGC_parc4_B, exp = TRUE, pvals = TRUE)[1])
tab4 <- tab4[,c(1,5)]
colnames(tab4) <- c("Parc 4","P4")
tab5 <- as.data.frame(summ(CHANGC_parc5_B, exp = TRUE, pvals = TRUE)[1])
tab5 <- tab5[,c(1,5)]
colnames(tab5) <- c("Parc 5","P5")
# tab6 <- as.data.frame(summ(CHANGC_IDF_DEM, exp = TRUE, pvals = TRUE)[1])
# tab6 <- tab6[,c(1,5)]
# colnames(tab6) <- c("CHANGC_IDF_DEM","PX")
tab <- cbind(tab1, tab2, tab3, tab4, tab5)
             #1,tab6)
tab$`Parc 1` <- round(tab$`Parc 1`,3)
tab$P1 <- round(tab$P1,3)
tab$`Parc 2` <- round(tab$`Parc 2`,3)
tab$P2 <- round(tab$P2,3)
tab$`Parc 3` <- round(tab$`Parc 3`,3)
tab$P3 <- round(tab$P3,3)
tab$`Parc 4` <- round(tab$`Parc 4`,3)
tab$P4 <- round(tab$P4,3)
tab$`Parc 5` <- round(tab$`Parc 5`,3)
tab$P5 <- round(tab$P5,3)
# tab$CHANGC_IDF_DEM <- round(tab$CHANGC_IDF_DEM,3)
# tab$PX <- round(tab$PX,3)
obs <- c(sum_CHANGCparc1_B[1]+sum_CHANGCparc1_B[2],NA, sum_CHANGCparc2_B[1]+sum_CHANGCparc2_B[2],NA,sum_CHANGCparc3_B[1]+sum_CHANGCparc3_B[2],NA, sum_CHANGCparc4_B[1]+sum_CHANGCparc4_B[2],NA,sum_CHANGCparc5_B[1]+sum_CHANGCparc5_B[2],NA)
         #,sum_CHANGCIDF_DEM[1]+sum_CHANGCIDF_DEM[2],NA)
events <- c(sum_CHANGCparc1_B[2],NA, sum_CHANGCparc2_B[2],NA, sum_CHANGCparc3_B[2],NA, sum_CHANGCparc4_B[2],NA,sum_CHANGCparc5_B[2],NA)
            #,sum_CHANGCIDF_DEM[2],NA)
pseudor <- c(round(PseudoR2(CHANGC_parc1_B, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc2_B, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc3_B, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc4_B, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(CHANGC_parc5_B, which = c("McKelveyZavoina")),2),NA)
             #,round(PseudoR2(CHANGC_IDF_DEM, which = c("McKelveyZavoina")),2),NA)
add2 <- as.data.frame(rbind(obs,events, pseudor))
colnames(add2) <- colnames(tab)
Modeles_CHANGC_parc_B <- rbind(tab, add2)
#Modeles_CHANGC_parc_B <- Modeles_CHANGC_parc_B[,c(11,12,1:10)]

# Annexes Paris 
tab1 <- as.data.frame(summ(DEM_Paris, exp = TRUE, pvals = TRUE)[1])
tab1 <- tab1[,c(1,5)]
colnames(tab1) <- c("DEM_Paris","P1")
tab2 <- as.data.frame(summ(QUITTER_Paris, exp = TRUE, pvals = TRUE)[1])
tab2 <- tab2[,c(1,5)]
colnames(tab2) <- c("QUITTER_Paris","P2")
tab3 <- as.data.frame(summ(QUITTER_Paris_DEM, exp = TRUE, pvals = TRUE)[1])
tab3 <- tab3[,c(1,5)]
colnames(tab3) <- c("QUITTER_Paris_DEM","P3")
tab <- cbind(tab1, tab2, tab3)
tab$DEM_Paris <- round(tab$DEM_Paris,3)
tab$P1 <- round(tab$P1,3)
tab$QUITTER_Paris <- round(tab$QUITTER_Paris,3)
tab$P2 <- round(tab$P2,3)
tab$QUITTER_Paris_DEM <- round(tab$QUITTER_Paris_DEM,3)
tab$P3 <- round(tab$P3,3)
obs <- c(sum_DEM_Paris[1]+sum_DEM_Paris[2],NA, sum_QUITparis[1]+sum_QUITparis[2],NA,sum_QUITTER_Paris_DEM[1]+sum_QUITTER_Paris_DEM[2],NA)
events <- c(sum_DEM_Paris[2],NA, sum_QUITparis[2],NA, sum_QUITTER_Paris_DEM[2],NA)
pseudor <- c(round(PseudoR2(DEM_Paris, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(QUITTER_Paris, which = c("McKelveyZavoina")),2),NA,round(PseudoR2(QUITTER_Paris_DEM, which = c("McKelveyZavoina")),2),NA)
add2 <- as.data.frame(rbind(obs,events, pseudor))
colnames(add2) <- colnames(tab)
Modeles_QUIT_Paris <- rbind(tab, add2)

rm(tab, tab1, tab2, tab3, tab4, tab5,tab6, t1,t1b, t2,t2b,t3b, t3,t4,t5,t6,t7,t8, t9, t10, t11, t12, t13, t14, t15, t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27, add2, obs, pseudor, indicateurs, indicateurs_b,indicateurs_c,indicateurs_d, events, sel2,selection)

# réagencement final des tables 
Modeles_IDF <- Modeles_IDF[c(1:11,13,12,14:26,28,27,29:nrow(Modeles_IDF)),]
Modeles_IDF_marche <- Modeles_IDF_marche[c(1:11,13,12,14:26,28,27,29:nrow(Modeles_IDF_marche)),]
Modeles_IDF_marche_comp <- Modeles_IDF_marche_comp[c(1:11,13,12,14:26,28,27,29:nrow(Modeles_IDF_marche_comp)),]
Modeles_CHANGC_parc <- Modeles_CHANGC_parc[c(1:9,11,10,12:24,26,25,27:nrow(Modeles_CHANGC_parc)),]
#Modeles_CHANGC_parc_B <- Modeles_CHANGC_parc_B[c(1:9,11,10,12:24,26,25,27:nrow(Modeles_CHANGC_parc_B)),]
Modeles_DEM_parc <- Modeles_DEM_parc[c(1:9,11,10,12:24,26,25,27:nrow(Modeles_DEM_parc)),]
Modeles_QUIT_Paris <- Modeles_QUIT_Paris[c(1:11,13,12,14:26,28,27,29:nrow(Modeles_QUIT_Paris)),]

# Graphiques ----

# Choix des facteurs et du seuil de significativité
p_seuil <- 0.05
noms_facteurs <- c("Intercept","Âge",
                   "Nationalité étrangère CEE","Nationalité étrangère\nhors CEE",
                   "Handicap (Aah versée)",
                   "Personne seule","Couple", "Famille monoparentale",
                   #"Femme seule","Homme seul","Couple", "Famille monoparentale\n(femme)","Famille monoparentale\n(homme)",
                   "Séparation ou\ndécès du conjoint","Mise en couple","Nouvel enfant" ,
                   "Chômage fin 2018","Inactivité fin 2018","Étudiant fin 2018",
                   "Début d'emploi en 2019","Début de chômage en 2019","Début d'inactivité en 2019",
                   "Début d'études en 2019",
                   "Bas revenus","Revenus modérés",
                   "AL location privée","AL logement social","AL accession","AL foyer ou résidence",
                   "Occupation moins d'un an","Occupation de 1 à 3 ans","Occupation plus de 10 ans")
color_terms <- rev(c("#41424C",
                     "#795C32","#795C32",
                     "#41424C",
                     "#795C32","#795C32","#795C32",
                     #"#795C32","#795C32","#795C32","#795C32","#795C32",
                     "#41424C","#41424C","#41424C",
                     "#795C32","#795C32","#795C32",
                     "#41424C","#41424C","#41424C","#41424C",
                     "#795C32","#795C32",
                     "#41424C","#41424C","#41424C","#41424C",
                     "#795C32","#795C32","#795C32"))

pla_typo <- RColorBrewer::brewer.pal(6, "Set3")[c(1,3,6,4,5)]
noms_facteurs_parc <- c("Marché 2","Marché 3","Marché 4","Marché 5")
col_terms_parc <- pla_typo[1:4]
pal_models <- c("#D9D9D9", "#80B1D3","#bdc9e1") #"#a6bddb"
#RColorBrewer::brewer.pal(12, "Set3")
# "#8DD3C7" "#BEBADA" "#FDB462" "#FB8072" 
# "#2b8cbe"
# "#8DD3C7" "#BEBADA" "#FDB462" "#FB8072" "#80B1D3"  "#B3DE69" "#FCCDE5" "#D9D9D9" "#BC80BD" "#CCEBC5"

# Modèles IDF ----

# Dem. & Chang. com & Chang_B ----

modeles <- c("M1.Déménager","M2.Changer de commune", "M3.Changer de commune (mobiles)")
regressions <- Modeles_IDF[2:29,c(1,3,5)] 
for(j in 1:ncol(regressions)){
  for(i in 1:nrow(regressions)){
    if(regressions[i,j] < 1){
      regressions[i,j] <- (1/regressions[i,j]*-1)+1
    }
    if(regressions[i,j] > 1){
      regressions[i,j] <- regressions[i,j] - 1
    }
  }
}

# Retirer les facteurs NS: 
regressions$M1[Modeles_IDF$P1[2:29] > p_seuil] <- NA
regressions$M2[Modeles_IDF$P2[2:29] > p_seuil] <- NA
regressions$M3[Modeles_IDF$P3[2:29] > p_seuil] <- NA

# Mise en forme pour le graphique
colnames(regressions) <- modeles
regressions$Facteurs <- noms_facteurs[2:29]
rownames(regressions) <- 1:nrow(regressions)
reg1 <- regressions[,c(ncol(regressions),1)]
reg1$Model <- colnames(reg1)[2]
reg1 <-reg1[,c(1,3,2)]
colnames(reg1) <- c("Facteurs","Modèle","Coefficient")
reg2 <- regressions[,c(ncol(regressions),2)]
reg2$Model <- colnames(reg2)[2]
reg2 <-reg2[,c(1,3,2)]
colnames(reg2) <- c("Facteurs","Modèle","Coefficient")
reg3 <- regressions[,c(ncol(regressions),3)]
reg3$Model <- colnames(reg3)[2]
reg3 <-reg3[,c(1,3,2)]
colnames(reg3) <- c("Facteurs","Modèle","Coefficient")
regressions <- rbind(reg1, reg2, reg3)

regressions$Facteurs <- as.factor(regressions$Facteurs)
regressions$Facteurs <- factor(regressions$Facteurs, levels = rev(noms_facteurs[2:29]))
regressions$Modèle <- as.factor(regressions$Modèle)
regressions$Modèle <- factor(regressions$Modèle, levels = rev(modeles))

regressions$Modèle <- fct_recode(regressions$Modèle,"Déménager"="M1.Déménager",
                                 "Changer de commune"="M2.Changer de commune",
                                 "Changer de commune\n(mobiles uniquement)"="M3.Changer de commune (mobiles)")

# passer en light grey "déménager"
graphCHANGC_all <- ggplot(regressions, aes(fill=Modèle, y=Coefficient, x=Facteurs)) + 
  geom_bar(position="dodge", stat="identity",color=NA) +
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom", 
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8, colour = color_terms),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7)
  )+
  scale_fill_manual(values=pal_models)+ #c("light grey",pla_typo[c(1,5)])
  guides(fill = guide_legend(reverse=TRUE, nrow=1))+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-2,-1,-0.5,0,0.5,1,2,3), 
                     labels=c("3 fois moins","2","1,5","Autant","1,5","2","3","4 fois plus"), 
                     limits=c(-2,3.1)) +
  annotate(geom = "text",x = c(2,5.5,8.5,11.5,15, 22,26.5,28), 
           y = c(2.9,2.9,2.9,2.9,2.9,2.9,2.9,2.9),
           label = c("Logement occupé de 3 à 10 ans",
                     "Aucune aide au logement",
                     "Revenus élevés",
                     "Situation stable",
                     "En emploi",
                     "Couple avec enfant(s)",
                     "Nationalité française", 
                     "Dix ans de moins"),
           size=2.8,
           color = "#4C4C4C", 
           hjust = "inward") +
  annotate("segment", x = c(1,4,8,10,14, 20,26), xend = c(3,7,9,13,16, 24,27), y = c(3,3,3,3,3,3,3), yend = c(3,3,3,3,3,3,3),
           colour = "black")+
  annotate("text", x = 28.38, y = 2.9, 
           label = "paste(bolditalic(Référence))",
           size=2.8, 
           color = "#4C4C4C",
           hjust = "inward",
           parse = TRUE)+   
  labs(caption = paste0("Pseudo R² de McKelvey-Zavoina : ",
                       "Déménager ",round(PseudoR2(DEM_IDF, which = c("McKelveyZavoina")),2), "; ",
                       "Changer de commune ",round(PseudoR2(CHANGC_IDF, which = c("McKelveyZavoina")),2),"; ",
                       "Changer de commune (mobiles) ", round(PseudoR2(CHANGC_IDF_DEM, which = c("McKelveyZavoina")),2), 
                       "\n",
                       "N : ", 
                       "Déménager ", sum_demIDF[1]+sum_demIDF[2], "; ",
                       "Changer de commune ",sum_CHANGCIDF[1]+sum_CHANGCIDF[2], "; ",
                       "Changer de communes (mobiles) ", sum_CHANGCIDF_DEM[2]+sum_CHANGCIDF_DEM[2], 
                       "\n",
                       "Évènements : ", 
                       "Déménager ", sum_demIDF[2], "; ",
                       "Changer de commune ", sum_CHANGCIDF[2], "; ",
                       "Changer de communes (mobiles) ", sum_CHANGCIDF_DEM[2]
  )
  )

graphCHANGC_all 

# Déménager - seul ----
regs <- regressions[regressions$Modèle=="Déménager",]
graphDEM_IDF <- ggplot(regs, aes(y=Coefficient, x=Facteurs)) + 
  geom_bar(position="dodge", stat="identity",color=NA, fill= pal_models[3]) +
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8, colour = color_terms),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7)
  )+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-2,-1,-0.5,0,0.5,1,2,3), 
                     labels=c("3 fois moins","2","1,5","Autant","1,5","2","3","4 fois plus"), 
                     limits=c(-2,3.1)) +
  annotate(geom = "text",x =  c(2,5.5,8.5,11.5,15, 22,26.5,28), 
           y = c(2.9,2.9,2.9,2.9,2.9,2.9,2.9,2.9),
           label = c("Logement occupé de 3 à 10 ans",
                     "Aucune aide au logement",
                     "Revenus élevés",
                     "Situation stable",
                     "En emploi",
                     "Couple avec enfant(s)",
                     "Nationalité française", 
                     "Dix ans de moins"),
           size=2.8,
           color = "#4C4C4C", 
           hjust = "inward") +
  annotate("segment", x = c(1,4,8,10,14, 20,26), xend = c(3,7,9,13,16, 24,27), y = c(3,3,3,3,3,3,3), yend = c(3,3,3,3,3,3,3),
           colour = "black")+
  annotate("text", x = 28.38, y = 2.9, 
           label = "paste(bolditalic(Référence))",
           size=2.8, 
           color = "#4C4C4C",
           hjust = "inward",
           parse = TRUE)+
  labs(caption = paste("Pseudo R² de McKelvey-Zavoina :",
                       round(PseudoR2(DEM_IDF, which = c("McKelveyZavoina")),2),
                       "; N = ", sum_demIDF[1]+sum_demIDF[2],
                       "; Évènements = ", sum_demIDF[2]))
graphDEM_IDF

# Généraux + marchés ----

modeles <- c("M1.Déménager","M2.Changer de commune", "M3.Changer de commune (mobiles)")
regressions <- Modeles_IDF_marche[30:33,c(1,3,5)] 
for(j in 1:ncol(regressions)){
  for(i in 1:nrow(regressions)){
    if(regressions[i,j] < 1){
      regressions[i,j] <- (1/regressions[i,j]*-1)+1
    }
    if(regressions[i,j] > 1){
      regressions[i,j] <- regressions[i,j] - 1
    }
  }
}

# Retirer les facteurs NS: 
regressions$M1[Modeles_IDF_marche$P1[30:33] > p_seuil] <- NA
regressions$M2[Modeles_IDF_marche$P2[30:33] > p_seuil] <- NA
regressions$M3[Modeles_IDF_marche$P3[30:33] > p_seuil] <- NA

# Mise en forme pour le graphique
colnames(regressions) <- modeles
regressions$Facteurs <- noms_facteurs_parc
rownames(regressions) <- 1:nrow(regressions)

reg1 <- regressions[,c(ncol(regressions),1)]
reg1$Model <- colnames(reg1)[2]
reg1 <-reg1[,c(1,3,2)]
colnames(reg1) <- c("Facteurs","Modèle","Coefficient")

reg2 <- regressions[,c(ncol(regressions),2)]
reg2$Model <- colnames(reg2)[2]
reg2 <-reg2[,c(1,3,2)]
colnames(reg2) <- c("Facteurs","Modèle","Coefficient")

reg3 <- regressions[,c(ncol(regressions),3)]
reg3$Model <- colnames(reg3)[2]
reg3 <-reg3[,c(1,3,2)]
colnames(reg3) <- c("Facteurs","Modèle","Coefficient")
regressions <- rbind(reg1, reg2, reg3)

# Grouped
regressions$Facteurs <- as.factor(regressions$Facteurs)
regressions$Facteurs <- factor(regressions$Facteurs, levels = rev(noms_facteurs_parc))
regressions$Modèle <- as.factor(regressions$Modèle)
regressions$Modèle <- factor(regressions$Modèle, levels = rev(modeles))

regressions$Modèle <- fct_recode(regressions$Modèle,"Déménager"="M1.Déménager",
                                 "Changer de commune"="M2.Changer de commune",
                                 "Changer de commune\n(mobiles uniquement)"="M3.Changer de commune (mobiles)")

# passer en light grey "déménager"
graphFocus_marche <- ggplot(regressions, aes(fill=Modèle, y=Coefficient, x=Facteurs)) + 
  geom_bar(position="dodge", stat="identity",color=NA) +
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom", 
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9, colour = col_terms_parc),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7.5)
  )+
  scale_fill_manual(values=pal_models)+
  guides(fill = guide_legend(reverse=TRUE, nrow=1))+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-0.25,-0.10,-0.05,0,0.05,0.1,0.25,0.5,0.75), 
                     labels=c("1,25 fois moins","1,1","","Autant","","1,1","1,25","1,5","1,75 fois plus"), 
                     limits=c(-0.3,0.75)) +
  annotate(geom = "text",x = 2.5, y=0.725,
           label = "Marché 1",
           size=3.25,
           color = pla_typo[5], 
           hjust = "inward") +
  annotate("segment", x = 1, xend = 4, y = 0.75, yend = 0.75,
           colour = "black")+
  annotate("text", x = 4.3, y = 0.725, 
           label = "paste(bolditalic(Référence))",
           size=3.25, 
           color = "#4C4C4C",
           hjust = "inward",
           parse = TRUE)+
  labs(caption = paste0("Pseudo R² de McKelvey-Zavoina : ",
                       "Déménager ", round(PseudoR2(DEM_IDF_marches, which = c("McKelveyZavoina")),2),  "; ",
                       "Changer de commune ",round(PseudoR2(CHANGC_IDF_marche, which = c("McKelveyZavoina")),2),   "; ",
                       "Changer de commune (mobiles) ", round(PseudoR2(CHANGC_IDF_DEM_marche, which = c("McKelveyZavoina")),2), 
                       "\n",
                       "N : ",
                        "Déménager ",sum_DEM_IDF_marches[1]+sum_DEM_IDF_marches[2],  "; ",
                        "Changer de commune ",sum_CHANGC_IDF_marche[1]+sum_CHANGC_IDF_marche[2],  "; ",
                        "Changer de communes (mobiles) ",sum_CHANGC_IDF_DEM_marche[2]+sum_CHANGC_IDF_DEM_marche[2],
                       "\n",
                       "Évènements : ",
                       "Déménager ",sum_DEM_IDF_marches[2],   "; ",
                       "Changer de commune ",sum_CHANGC_IDF_marche[2],  "; ",
                       "Changer de communes (mobiles) ", sum_CHANGC_IDF_DEM_marche[2] 
  )
  )

graphFocus_marche 


# Déménager marchés ---- 

modeles <- c("a.Marché 1","b.Marché 2", "c.Marché 3","d.Marché 4", "e.Marché 5")
Modeles_DEM_parc <- Modeles_DEM_parc[c(1:9,11,10,12:nrow(Modeles_DEM_parc)),]
regressions <- Modeles_DEM_parc[2:27,c(1,3,5,7,9)] # On ne garde plus le général :  1
for(j in 1:ncol(regressions)){
  for(i in 1:nrow(regressions)){
    if(regressions[i,j] < 1){
      regressions[i,j] <- (1/regressions[i,j]*-1)+1
    }
    if(regressions[i,j] > 1){
      regressions[i,j] <- regressions[i,j] - 1
    }
  }
}

# Retirer les facteurs NS:
regressions$`Parc 1`[Modeles_DEM_parc$P1[2:27] > p_seuil] <- NA
regressions$`Parc 2`[Modeles_DEM_parc$P2[2:27] > p_seuil] <- NA
regressions$`Parc 3`[Modeles_DEM_parc$P3[2:27] > p_seuil] <- NA
regressions$`Parc 4`[Modeles_DEM_parc$P4[2:27] > p_seuil] <- NA
regressions$`Parc 5`[Modeles_DEM_parc$P5[2:27] > p_seuil] <- NA

# Mise en forme pour le graphique
colnames(regressions) <- modeles
regressions$Facteurs <- noms_facteurs[c(2:27)]
rownames(regressions) <- 1:nrow(regressions)

reg1 <- regressions[,c(ncol(regressions),1)]
reg1$Model <- colnames(reg1)[2]
reg1 <-reg1[,c(1,3,2)]
colnames(reg1) <- c("Facteurs","Modèle","Coefficient")
reg2 <- regressions[,c(ncol(regressions),2)]
reg2$Model <- colnames(reg2)[2]
reg2 <-reg2[,c(1,3,2)]
colnames(reg2) <- c("Facteurs","Modèle","Coefficient")
reg3 <- regressions[,c(ncol(regressions),3)]
reg3$Model <- colnames(reg3)[2]
reg3 <-reg3[,c(1,3,2)]
colnames(reg3) <- c("Facteurs","Modèle","Coefficient")
reg4 <- regressions[,c(ncol(regressions),4)]
reg4$Model <- colnames(reg4)[2]
reg4 <-reg4[,c(1,3,2)]
colnames(reg4) <- c("Facteurs","Modèle","Coefficient")
reg5 <- regressions[,c(ncol(regressions),5)]
reg5$Model <- colnames(reg5)[2]
reg5 <-reg5[,c(1,3,2)]
colnames(reg5) <- c("Facteurs","Modèle","Coefficient")

regressions <- rbind(reg1, reg2, reg3, reg4, reg5)

# Grouped
regressions$Facteurs <- as.factor(regressions$Facteurs)
regressions$Facteurs <- factor(regressions$Facteurs, levels = rev(noms_facteurs[2:27]))
regressions$Modèle <- as.factor(regressions$Modèle)
regressions$Modèle <- factor(regressions$Modèle, levels = rev(modeles))

summary(regressions$Coefficient)

# On fausse le coeff max pour rendre la figure plus lisible : 
regressions$Coefficient[c(60,86,112)] <- 2.99 #4.53 4.72 8.26

palette <- c(pla_typo, "light grey")

graphDEM_parc <- ggplot(regressions, aes(fill=Modèle, y=Coefficient, x=Facteurs)) +
  geom_bar(position="dodge", stat="identity",color="dark grey") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8, colour = color_terms),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7)
  )+
  scale_fill_manual(values=palette)+
  guides(fill = guide_legend(reverse=TRUE, nrow=1))+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-2,-1,-0.5,0,0.5,1,2,3), 
                     labels=c("3 fois moins","2","1,5","Autant","1,5","2","3","4 fois plus"), 
                     limits=c(-2,3.1)) +
  annotate(geom = "text",x = c(2,5.5,8.5,11.5,15, 21,24.5,26), 
           y = c(2.9,2.9,2.9,2.9,2.9,2.9,2.9,2.9),
           label = c("Logement occupé de 3 à 10 ans",
                     "Aucune aide au logement",
                     "Revenus élevés",
                     "Situation stable",
                     "En emploi",
                     "Couple avec enfant(s)",
                     "Nationalité française", 
                     "Dix ans de moins"),
           size=2.8,
           color = "#4C4C4C", 
           hjust = "inward") +
  annotate("segment", x = c(1,4,8,10,14, 20,24), xend = c(3,7,9,13,16, 22,25), y = c(3,3,3,3,3,3,3), yend = c(3,3,3,3,3,3,3),
           colour = "black")+
  annotate("text", x = 26.38, y = 2.9, 
           label = "paste(bolditalic(Référence))",
           size=2.8, 
           color = "#4C4C4C",
           hjust = "inward",
           parse = TRUE)+
  labs(caption = paste0("Pseudo R² de McKelvey-Zavoina : ",
                        "Marché 1 ", round(PseudoR2(DEM_parc1, which = c("McKelveyZavoina")),2),  "; ",
                        "Marché 2 ", round(PseudoR2(DEM_parc2, which = c("McKelveyZavoina")),2),  "; ",
                        "Marché 3 ", round(PseudoR2(DEM_parc3, which = c("McKelveyZavoina")),2),  "; ",
                        "Marché 4 ", round(PseudoR2(DEM_parc4, which = c("McKelveyZavoina")),2),  "; ",
                        "Marché 5 ", round(PseudoR2(DEM_parc5, which = c("McKelveyZavoina")),2), ".",
                        "\n",
                        "N : ", 
                        "Marché 1 ", nobs(DEM_parc1), "; ",
                        "Marché 2 ", nobs(DEM_parc2), "; ",
                        "Marché 3 ", nobs(DEM_parc3), "; ",
                        "Marché 4 ", nobs(DEM_parc4), "; ",
                        "Marché 5 ", nobs(DEM_parc5), ".",
                        "\n",
                        "Évènements : ", 
                        "Marché 1 ", nrow(DEM_parc1$model[DEM_parc1$model$Déménager==1,]),  "; ",
                        "Marché 2 ", nrow(DEM_parc2$model[DEM_parc2$model$Déménager==1,]),  "; ",
                        "Marché 3 ", nrow(DEM_parc3$model[DEM_parc3$model$Déménager==1,]),  "; ",
                        "Marché 4 ", nrow(DEM_parc4$model[DEM_parc4$model$Déménager==1,]),  "; ",
                        "Marché 5 ", nrow(DEM_parc5$model[DEM_parc5$model$Déménager==1,]),  "."
  )
  )
graphDEM_parc

# Chang. com marchés ---- 

modeles <- c("a.Marché 1","b.Marché 2", "c.Marché 3","d.Marché 4", "e.Marché 5")
regressions <- Modeles_CHANGC_parc[2:27,c(3,5,7,9,11)] # On ne garde plus le général :  1
for(j in 1:ncol(regressions)){
  for(i in 1:nrow(regressions)){
    if(regressions[i,j] < 1){
      regressions[i,j] <- (1/regressions[i,j]*-1)+1
    }
    if(regressions[i,j] > 1){
      regressions[i,j] <- regressions[i,j] - 1
    }
  }
}

# Retirer les facteurs NS:
regressions$`Parc 1`[Modeles_CHANGC_parc$P1[2:27] > p_seuil] <- NA
regressions$`Parc 2`[Modeles_CHANGC_parc$P2[2:27] > p_seuil] <- NA
regressions$`Parc 3`[Modeles_CHANGC_parc$P3[2:27] > p_seuil] <- NA
regressions$`Parc 4`[Modeles_CHANGC_parc$P4[2:27] > p_seuil] <- NA
regressions$`Parc 5`[Modeles_CHANGC_parc$P5[2:27] > p_seuil] <- NA
#regressions$CHANGC_IDF[Modeles_CHANGC_parc$PX[2:27] > p_seuil] <- NA

# Mise en forme pour le graphique
colnames(regressions) <- modeles
regressions$Facteurs <- noms_facteurs[2:27]
rownames(regressions) <- 1:nrow(regressions)

reg1 <- regressions[,c(ncol(regressions),1)]
reg1$Model <- colnames(reg1)[2]
reg1 <-reg1[,c(1,3,2)]
colnames(reg1) <- c("Facteurs","Modèle","Coefficient")
reg2 <- regressions[,c(ncol(regressions),2)]
reg2$Model <- colnames(reg2)[2]
reg2 <-reg2[,c(1,3,2)]
colnames(reg2) <- c("Facteurs","Modèle","Coefficient")
reg3 <- regressions[,c(ncol(regressions),3)]
reg3$Model <- colnames(reg3)[2]
reg3 <-reg3[,c(1,3,2)]
colnames(reg3) <- c("Facteurs","Modèle","Coefficient")
reg4 <- regressions[,c(ncol(regressions),4)]
reg4$Model <- colnames(reg4)[2]
reg4 <-reg4[,c(1,3,2)]
colnames(reg4) <- c("Facteurs","Modèle","Coefficient")
reg5 <- regressions[,c(ncol(regressions),5)]
reg5$Model <- colnames(reg5)[2]
reg5 <-reg5[,c(1,3,2)]
colnames(reg5) <- c("Facteurs","Modèle","Coefficient")
# reg6 <- regressions[,c(ncol(regressions),6)]
# reg6$Model <- colnames(reg6)[2]
# reg6 <-reg6[,c(1,3,2)]
#colnames(reg6) <- c("Facteurs","Modèle","Coefficient")
regressions <- rbind(reg1, reg2, reg3, reg4, reg5)#,reg6)

# Grouped
regressions$Facteurs <- as.factor(regressions$Facteurs)
regressions$Facteurs <- factor(regressions$Facteurs, levels = rev(noms_facteurs[2:27]))
regressions$Modèle <- as.factor(regressions$Modèle)
regressions$Modèle <- factor(regressions$Modèle, levels = rev(modeles))

summary(regressions$Coefficient)

# On fausse le coeff max pour rendre la figure plus lisible : 
regressions$Coefficient[c(60,86,112)] <- 2.99 #3.786 3.876 7.998

palette <- c(pla_typo, "light grey")

graphCHANC_parc <- ggplot(regressions, aes(fill=Modèle, y=Coefficient, x=Facteurs)) +
  geom_bar(position="dodge", stat="identity",color="dark grey") +
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8, colour = color_terms),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7)
  )+
  scale_fill_manual(values=palette)+
  guides(fill = guide_legend(reverse=TRUE, nrow=1))+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-2,-1,-0.5,0,0.5,1,2,3), 
                     labels=c("3 fois moins","2","1,5","Autant","1,5","2","3","4 fois plus"), 
                     limits=c(-2,3.1)) +
  annotate(geom = "text",x = c(2,5.5,8.5,11.5,15, 21,24.5,26), 
           y = c(2.9,2.9,2.9,2.9,2.9,2.9,2.9,2.9),
           label = c("Logement occupé de 3 à 10 ans",
                     "Aucune aide au logement",
                     "Revenus élevés",
                     "Situation stable",
                     "En emploi",
                     "Couple avec enfant(s)",
                     "Nationalité française", 
                     "Dix ans de moins"),
           size=2.8,
           color = "#4C4C4C", 
           hjust = "inward") +
  annotate("segment", x = c(1,4,8,10,14, 20,24), xend = c(3,7,9,13,16, 22,25), y = c(3,3,3,3,3,3,3), yend = c(3,3,3,3,3,3,3),
           colour = "black")+
  annotate("text", x = 26.38, y = 2.9, 
           label = "paste(bolditalic(Référence))",
           size=2.8, 
           color = "#4C4C4C",
           hjust = "inward",
           parse = TRUE)+
  labs(caption = paste0("Pseudo R² de McKelvey-Zavoina : ",
                       "Marché 1 ", round(PseudoR2(CHANGC_parc1, which = c("McKelveyZavoina")),2),  "; ",
                       "Marché 2 ", round(PseudoR2(CHANGC_parc2, which = c("McKelveyZavoina")),2),  "; ",
                       "Marché 3 ", round(PseudoR2(CHANGC_parc3, which = c("McKelveyZavoina")),2),  "; ",
                       "Marché 4 ", round(PseudoR2(CHANGC_parc4, which = c("McKelveyZavoina")),2),  "; ",
                       "Marché 5 ", round(PseudoR2(CHANGC_parc5, which = c("McKelveyZavoina")),2), "; ",
                       #"IDF ", round(PseudoR2(CHANGC_IDF_2, which = c("McKelveyZavoina")),2),".",
                       "\n",
                       "N : ", 
                       "Marché 1 ", nobs(CHANGC_parc1), "; ",
                       "Marché 2 ", nobs(CHANGC_parc2), "; ",
                       "Marché 3 ", nobs(CHANGC_parc3), "; ",
                       "Marché 4 ", nobs(CHANGC_parc4), "; ",
                       "Marché 5 ", nobs(CHANGC_parc1), "; ",
                       #"IDF ", nobs(CHANGC_IDF_2),".",
                       "\n",
                       "Évènements : ", 
                       "Marché 1 ", nrow(CHANGC_parc1$model[CHANGC_parc1$model$`Changer de commune`==1,]),  "; ",
                       "Marché 2 ", nrow(CHANGC_parc2$model[CHANGC_parc2$model$`Changer de commune`==1,]),  "; ",
                       "Marché 3 ", nrow(CHANGC_parc3$model[CHANGC_parc3$model$`Changer de commune`==1,]),  "; ",
                       "Marché 4 ", nrow(CHANGC_parc4$model[CHANGC_parc4$model$`Changer de commune`==1,]),  "; ",
                       "Marché 5 ", nrow(CHANGC_parc5$model[CHANGC_parc5$model$`Changer de commune`==1,]),  "; "#,
                       #"IDF ", nrow(CHANGC_IDF_2$model[CHANGC_IDF_2$model$`Changer de commune`==1,]),".",
  )
  )
graphCHANC_parc

# Quitter Paris ----

modeles <- c("Déménager","Quitter Paris", "Quitter Paris (mobiles)")
regressions <- Modeles_QUIT_Paris[2:29,c(1,3,5)]
for(j in 1:ncol(regressions)){
  for(i in 1:nrow(regressions)){
    if(regressions[i,j] < 1){
      regressions[i,j] <- (1/regressions[i,j]*-1)+1
    }
    if(regressions[i,j] > 1){
      regressions[i,j] <- regressions[i,j] - 1
    }
  }
}

# Retirer les facteurs NS: 
regressions$DEM_Paris[Modeles_QUIT_Paris$P1[2:29] > p_seuil] <- NA
regressions$QUITTER_Paris[Modeles_QUIT_Paris$P2[2:29] > p_seuil] <- NA
regressions$QUITTER_Paris_DEM[Modeles_QUIT_Paris$P3[2:29] > p_seuil] <- NA

# Mise en forme pour le graphique
colnames(regressions) <- modeles
regressions$Facteurs <- noms_facteurs[2:29]
rownames(regressions) <- 1:nrow(regressions)
reg1 <- regressions[,c(ncol(regressions),1)]
reg1$Model <- colnames(reg1)[2]
reg1 <-reg1[,c(1,3,2)]
colnames(reg1) <- c("Facteurs","Modèle","Coefficient")
reg2 <- regressions[,c(ncol(regressions),2)]
reg2$Model <- colnames(reg2)[2]
reg2 <-reg2[,c(1,3,2)]
colnames(reg2) <- c("Facteurs","Modèle","Coefficient")
reg3 <- regressions[,c(ncol(regressions),3)]
reg3$Model <- colnames(reg3)[2]
reg3 <-reg3[,c(1,3,2)]
colnames(reg3) <- c("Facteurs","Modèle","Coefficient")
regressions <- rbind(reg1, reg2, reg3)

regressions$Facteurs <- as.factor(regressions$Facteurs)
regressions$Facteurs <- factor(regressions$Facteurs, levels = rev(noms_facteurs[2:29]))
regressions$Modèle <- as.factor(regressions$Modèle)
regressions$Modèle <- factor(regressions$Modèle, levels = rev(modeles))
regressions$Modèle <- fct_recode(regressions$Modèle,"Déménager depuis Paris"="Déménager",
                                 "Quitter Paris"="Quitter Paris",
                                 "Quitter Paris\n(mobiles uniquement)"="Quitter Paris (mobiles)")

# passer en light grey "déménager"
graphQUIT_PARIS <- ggplot(regressions, aes(fill=Modèle, y=Coefficient, x=Facteurs)) + 
  geom_bar(position="dodge", stat="identity",color=NA) +
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom", 
        legend.text = element_text(size = 8.5),
        legend.key.size = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8, colour = color_terms),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7)
  )+
  scale_fill_manual(values=c(pal_models[1], "#966596",pal_models[3]) )+ # "#BC80BD" "#966b96"
  guides(fill = guide_legend(reverse=TRUE, nrow=1))+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-3,-2,-1,-0.5,0,0.5,1,2), 
                     labels=c("4 fois moins","3","2","1,5","Autant","1,5","2","3 fois plus"), 
                     limits=c(-3.7,2.5)) +
  annotate(geom = "text",x = c(2,5.5,8.5,11.5,15, 22,26.5,28), 
           y = c(2.4,2.4,2.4,2.4,2.4,2.4,2.4,2.4),
           label = c("Occupation\nde 3 à 10 ans",
                     "Aucune aide au logement",
                     "Revenus élevés",
                     "Situation stable",
                     "En emploi",
                     "Couple avec enfant(s)",
                     "Nationalité française", 
                     "Dix ans de moins"),
           size=2.8,
           color = "#4C4C4C", 
           hjust = "inward") +
  annotate("segment", x = c(1,4,8,10,14, 20,26), xend = c(3,7,9,13,16, 24,27), y = c(2.5,2.5,2.5,2.5,2.5,2.5,2.5), yend = c(2.5,2.5,2.5,2.5,2.5,2.5,2.5),
           colour = "black")+
  annotate("text", x = 28.38, y = 2.4, 
           label = "paste(bolditalic(Référence))",
           size=2.8, 
           color = "#4C4C4C",
           hjust = "inward",
           parse = TRUE)+
  labs(caption = paste0("Pseudo R² de McKelvey-Zavoina : ",
                       "Déménager depuis Paris ",round(PseudoR2(DEM_Paris, which = c("McKelveyZavoina")),2),"; ", 
                       "Quitter Paris ",round(PseudoR2(QUITTER_Paris, which = c("McKelveyZavoina")),2),"; ", 
                       "Quitter Paris (mobiles) ",round(PseudoR2(QUITTER_Paris_DEM, which = c("McKelveyZavoina")),2), 
                       "\n",
                       "N : ", 
                       "Déménager depuis Paris ",sum_DEM_Paris[1]+sum_DEM_Paris[2], "; ",
                       "Quitter Paris ",sum_QUITparis[1]+sum_QUITparis[2],"; ",
                       "Quitter Paris (mobiles) ",sum_QUITTER_Paris_DEM[2]+sum_QUITTER_Paris_DEM[2], 
                       "\n",
                       "Évènements : ", 
                       "Déménager depuis Paris ",sum_DEM_Paris[2],"; ",
                       "Quitter Paris ",sum_QUITparis[2],"; ",
                       "Quitter Paris (mobiles) ", sum_QUITTER_Paris_DEM[2]
  )
  )

graphQUIT_PARIS 

rm(reg1, reg2, reg3, reg4, reg5, CHANGC_IDF, CHANGC_parc1, CHANGC_parc2, CHANGC_parc3, CHANGC_parc4,CHANGC_parc5
   ,CHANGC_IDF_DEM, CHANGC_parc1_B, CHANGC_parc2_B, CHANGC_parc3_B, CHANGC_parc4_B,CHANGC_parc5_B,
   DEM_IDF, DEM_parc1, DEM_parc2, DEM_parc3, DEM_parc4,DEM_parc5, QUITTER_Paris, QUITTER_Paris_DEM, QUITTER_QUAR_G, QUITTER_QUAR_type, 
   QUIT_QUAR_MM, QUIT_QUAR_MP, QUIT_QUAR_PM, QUIT_QUAR_PP, quartiers_desc, perscouv_quartiers, DEM_Paris, DEM_IDF_marches, CHANGC_IDF_marche, CHANGC_IDF_DEM_marche,
   regressions, regs)

# Save ----
#save.image("chap4_regressionsVF_sitfam_det.RData")

# Exports ----
#load("chap4_regressionsVF.RData")
load("chap4_regressionsVF_sitfam_det.RData")

# Préparation des tableaux
noms_facteurs[2] <- "Âge / 10"
noms_facteurs[4] <- "Nationalité étrangère hors CEE"
noms_facteurs[5] <- "Handicap (Aah versée)"
noms_facteurs[9] <- "Famille monoparentale (femme)" # ajout si 29 facteurs
noms_facteurs[10] <- "Famille monoparentale (homme)" # ajout si 29 facteurs
noms_facteurs[11] <- "Séparation ou décès du conjoint" # ajout si 29 facteurs

rownames_tab1 <- c(noms_facteurs[1:2], "Nationalité", 
  #noms_facteurs[3:5],"Composition familiale",
  noms_facteurs[3:5],"Composition du foyer",
  noms_facteurs[6:10],"Évènements familiaux",
  noms_facteurs[11:13],"Situation professionnelle",
  noms_facteurs[14:16],"Évènements professionnels",
  noms_facteurs[17:20],"Revenus",
  noms_facteurs[21:22],"Aides au logement (AL)",
  noms_facteurs[23:26],"Durée d'occupation",
  noms_facteurs[27:29])
# noms_facteurs[6:8],"Évènements familiaux",
# noms_facteurs[9:11],"Situation professionnelle",
# noms_facteurs[12:14],"Évènements professionnels",
# noms_facteurs[15:18],"Revenus",
# noms_facteurs[19:20],"Aides au logement (AL)",
# noms_facteurs[21:24],"Durée d'occupation",
# noms_facteurs[25:27])
rownames_tab2 <- c(rownames_tab1, "Type de marché", "Marché 2","Marché 3","Marché 4","Marché 5")

for(i in c(1,2,6)){
#for(i in 1:6){
  if(i ==1){tab <- Modeles_IDF}
  if(i ==2){tab <- Modeles_IDF_marche}
  if(i ==3){tab <- Modeles_DEM_parc}
  if(i ==4){tab <- Modeles_CHANGC_parc}
  if(i ==5){tab <- Modeles_CHANGC_parc_B}
  if(i ==6){tab <- Modeles_QUIT_Paris}
  
  var_durocc <- c("Référence : Logement occupé depuis 3 à 10 ans", rep(NA,(ncol(tab)-1)))
  var_AL <- c("Référence : aucune aide au logement", rep(NA,(ncol(tab)-1)))
  var_rev <-c("Référence : revenus élevés", rep(NA,(ncol(tab)-1)))
  var_eve_pro <- c("Référence : situation stable en 2019", rep(NA,(ncol(tab)-1)))
  var_pro <- c("Référence : en emploi fin 2018", rep(NA,(ncol(tab)-1)))
  var_eve_fam <- c("Référence : foyer non concerné par l'évènement", rep(NA,(ncol(tab)-1)))
  var_fam <- c("Référence : couple avec enfant(s)", rep(NA,(ncol(tab)-1)))
  var_nat <- c("Référence : nationalité française", rep(NA,(ncol(tab)-1)))
  
  temp1 <- tab[1:(nrow(tab)-3),]
  tempX <- tab[(nrow(tab)-2):nrow(tab),]
  temp1$P1[temp1$P1<=0.001] <- "***"
  temp1$P1[temp1$P1> 0.001 & temp1$P1<=0.01] <- "**"
  temp1$P1[temp1$P1> 0.01 & temp1$P1<=0.05] <- "*"
  temp1$P1[temp1$P1> 0.05] <- "ns"
  temp1$P2[temp1$P2<=0.001] <- "***"
  temp1$P2[temp1$P2> 0.001 & temp1$P2<=0.01] <- "**"
  temp1$P2[temp1$P2> 0.01 & temp1$P2<=0.05] <- "*"
  temp1$P2[temp1$P2> 0.05] <- "ns"
  temp1$P3[temp1$P3<=0.001] <- "***"
  temp1$P3[temp1$P3> 0.001 & temp1$P3<=0.01] <- "**"
  temp1$P3[temp1$P3> 0.01 & temp1$P3<=0.05] <- "*"
  temp1$P3[temp1$P3> 0.05] <- "ns"
  if(ncol(tab) >6){
    temp1$P4[temp1$P4<=0.001] <- "***"
    temp1$P4[temp1$P4> 0.001 & temp1$P4<=0.01] <- "**"
    temp1$P4[temp1$P4> 0.01 & temp1$P4<=0.05] <- "*"
    temp1$P4[temp1$P4> 0.05] <- "ns"
    temp1$P5[temp1$P5<=0.001] <- "***"
    temp1$P5[temp1$P5> 0.001 & temp1$P5<=0.01] <- "**"
    temp1$P5[temp1$P5> 0.01 & temp1$P5<=0.05] <- "*"
    temp1$P5[temp1$P5> 0.05] <- "ns"
    temp1$PX[temp1$PX<=0.001] <- "***"
    temp1$PX[temp1$PX> 0.001 & temp1$PX<=0.01] <- "**"
    temp1$PX[temp1$PX> 0.01 & temp1$PX<=0.05] <- "*"
    temp1$PX[temp1$PX> 0.05] <- "ns"
  }
  tab <- temp1
  if(i %in% c(1,2,6)){
    temp1 <- tab[1:2,]
    temp2 <- tab[3:5,]
    temp3 <- tab[6:10,]
    temp4 <- tab[11:13,]
    temp5 <- tab[14:16,]
    temp6 <- tab[17:20,]
    temp7 <- tab[21:22,]
    temp8 <- tab[23:26,]
    temp9 <- tab[27:29,]
  }

  if(i %in% 3:5){
  temp1 <- tab[1:2,]
  temp2 <- tab[3:5,]
  temp3 <- tab[6:8,]
  temp4 <- tab[9:11,]
  temp5 <- tab[12:14,]
  temp6 <- tab[15:18,]
  temp7 <- tab[19:20,]
  temp8 <- tab[21:24,]
  temp9 <- tab[25:27,]
  }
  
  sel <- rbind(temp1,var_nat, temp2,var_fam,
               temp3,var_eve_fam, temp4, var_pro,
               temp5,var_eve_pro, temp6,var_rev,
               temp7,var_AL, temp8, var_durocc,
               temp9)
  rownames(sel) <- rownames_tab1
  if(i==2){
    temp10 <- tab[30:33,]
    var_mar <- c("Référence : Marché 1", rep(NA,(ncol(tab)-1)))
    sel <- rbind(sel, var_mar,temp10)
    rownames(sel) <- rownames_tab2
  }
  tab <- rbind(sel, tempX)
  if(i ==1){Modeles_IDF <- tab}
  if(i ==2){Modeles_IDF_marche <- tab}
  if(i ==3){Modeles_DEM_parc <- tab}
  if(i ==4){Modeles_CHANGC_parc <- tab}
  if(i ==5){Modeles_CHANGC_parc_B <- tab}
  if(i ==6){Modeles_QUIT_Paris <- tab}
}

rm(temp1, temp2,temp3, temp4,temp5, temp6, temp7, temp8, temp9, temp10, 
   tempX, tab, sel, rownames_tab1, rownames_tab2, var_AL, var_durocc,
   var_eve_fam, var_eve_pro, var_fam, var_mar, var_nat, var_pro, var_rev)

facteurs_VIF <- c("Âge / 10", "Nationalité",
                  "Handicap (Aah versée)","Composition du foyer",#"Composition familiale",
                  "Séparation",
                  "Nouvel enfant", "Mise en couple",
                  "Situation professionnelle",
                  "Évenements professionnels",
                  "Revenus",
                  "Aides au logement",
                  "Durée d'occupation")
colinearite1$Facteurs <- facteurs_VIF
colinearite1b$Facteurs <- c(facteurs_VIF,"Type de marché")
# colinearite2$Facteurs <- facteurs_VIF
# colinearite3$Facteurs <- facteurs_VIF
# colinearite4$Facteurs <- facteurs_VIF
colinearite5$Facteurs <- facteurs_VIF
rm(facteurs_VIF, i, j)

# Export des tableaux
library(openxlsx)
setwd("Chapitre 4/Figures")
write.xlsx(colinearite1, "colinearite_IDF_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = F, append = F)
write.xlsx(colinearite1b, "colinearite_IDF_march_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = F, append = F)
# write.xlsx(colinearite2, "colinearite_DEM_Parc.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = F, append = F)
# write.xlsx(colinearite3, "colinearite_CHANGC_Parc.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = F, append = F)
# write.xlsx(colinearite4, "colinearite_CHANGC_DEM_Parc.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = F, append = F)
write.xlsx(colinearite5, "colinearite_Paris_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = F, append = F)
write.xlsx(Modeles_IDF, "Modeles_IDF_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = T, append = F)
# write.xlsx(marches_desc, "marches_desc.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = T, append = F)
write.xlsx(Modeles_IDF_marche, "Modeles_IDF_marche_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = T, append = F)
write.xlsx(Modeles_IDF_marche_comp, "Modeles_IDF_marche_compare_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = T, append = F)
# write.xlsx(Modeles_DEM_parc, "Modeles_DEM_parc.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = T, append = F)
# write.xlsx(Modeles_CHANGC_parc, "Modeles_CHANGC_parc.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = T, append = F)
# write.xlsx(Modeles_CHANGC_parc_B, "Modeles_CHANGC_parc_DEM.xlsx", sheetName = "feuille1", 
#            colNames = TRUE, rowNames = T, append = F)
write.xlsx(Modeles_QUIT_Paris, "Modeles_QUIT_Paris_V2.xlsx", sheetName = "feuille1", 
           colNames = TRUE, rowNames = T, append = F)

# Export des graphiques

# Charger pagez
pageZ <- readRDS("pageZ.rds")

pageZ(format = "portrait", output = "svg", name = "Déménager_IDF_V2")
graphDEM_IDF
dev.off()

pageZ(format = "portrait", output = "svg", name = "Changer_com_IDF_V2")
graphCHANGC_all
dev.off()

pageZ(format = "portrait_third", output = "svg", name = "Focus_Marchés_V2")
graphFocus_marche
dev.off()

pageZ(format = "portrait", output = "svg", name = "DEM_Marchés_V3")
graphDEM_parc
dev.off()

pageZ(format = "portrait", output = "svg", name = "Changer_com_Marchés_V3")
graphCHANC_parc
dev.off()

pageZ(format = "portrait", output = "svg", name = "Quitter Paris_V2")
graphQUIT_PARIS
dev.off()





