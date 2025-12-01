library(forcats)
library(dplyr)
library(stringi)
library(stringr)
library(ggplot2)
library(rgeos)
library(sf)
library(StatMeasures)
library(data.table)
library(SpatialPosition)
library(potential)
library(cartography)

# Avant cette étape, construction de la population de référence dans SAS
########################################################################

# Donnees sur les identifiants 
##############################

MOBCAF <- read.csv("MOBCAF_19.csv", sep=";", comment.char="#",stringsAsFactors=FALSE)

MOBCAF[,15] <- as.character(MOBCAF[,15])
MOBCAF[,29] <- as.character(MOBCAF[,29])
  
# Remplacer les vides par des NA
MOBCAF[MOBCAF == "" ] <- NA
  
# Recoder PRESFRES et creer ND_17 et ND_18
MOBCAF[,8] <- as.factor(MOBCAF[,8])
MOBCAF[,23] <- as.factor(MOBCAF[,23])

summary(MOBCAF[,8])
MOBCAF[,8] <- fct_recode(MOBCAF[,8],"8"="All. avec aide logement seule et/ou  ARS ou RSA < seuil versement et sans action sociale",
                        "7"="All.hors noyau dur sans action sociale et avec PAH legaux",
                        "1"="All.hors noyau dur sans action sociale et sans PAH legaux",
                        "2"="All.noyau dur sans action sociale",
                        "5"="Beneficiaires d'ARS seule sans action sociale")
  
MOBCAF[,23] <- fct_recode(MOBCAF[,23],"8"="All. avec aide logement seule et/ou  ARS ou RSA < seuil versement et sans action sociale",
                         "7"="All.hors noyau dur sans action sociale et avec PAH legaux",
                         "1"="All.hors noyau dur sans action sociale et sans PAH legaux",
                         "2"="All.noyau dur sans action sociale",
                         "5"="Beneficiaires d'ARS seule sans action sociale")
  
# Noyau dur
sel1 <- MOBCAF[MOBCAF[,8] %in% c("2","4","5","6","9"),]
sel1$ND1 <- "OUI"
sel2 <- MOBCAF[!(MOBCAF[,8] %in% c("2","4","5","6","9")),]
sel2$ND1 <- "NON" # effectif nul
MOBCAF <- rbind(sel1, sel2)
sel1 <- MOBCAF[MOBCAF[,23] %in% c("2","4","5","6","9"),]
sel1$ND2 <- "OUI"
sel2 <- MOBCAF[!(MOBCAF[,23] %in% c("2","4","5","6","9")),]
sel2$ND2 <- "NON"
MOBCAF <- rbind(sel1, sel2)
rm(sel1, sel2)

MOBCAF$ND1 <- as.factor(MOBCAF$ND1)
MOBCAF$ND2 <- as.factor(MOBCAF$ND2)
  
# Creation de l'identifiant unique pour les deux annees: IDUNI
MOBCAF$IDUNI <- 1:nrow(MOBCAF)
MOBCAF$IDUNI <- as.character(MOBCAF$IDUNI)

# Simplification des CATBEN
MOBCAF[,5] <- as.factor(MOBCAF[,5])
MOBCAF[,20] <- as.factor(MOBCAF[,20])
MOBCAF[,5] <- fct_recode(MOBCAF[,5], 
                       "AAH"="AAH ou complement AAH",
                       "PFAAH"="PF + AAH ou complement AAH",
                       "ALSAAH"="ALS + AAH ou complement AAH",
                       "APLAAH"="APL + AAH ou complement AAH",
                       "DEC"="Decales (trimestre precedent : ADI, CDI, AMI)",
                       "PLALSAAH"="PF + ALS + AAH ou complement AAH",
                       "PFAPL"="PF + APL",
                       "PFAPLAAH"="PF + APL + AAH ou complement AAH",
                       "PPA"="Prime Activite seule",
                       "PFALS"="PF + ALS",
                       "RSA"="RSA  (avec ou sans PPA) sans autres prestations",
                       "PAH"="PAH legaux",
                       "ARS"="ARS seulement",
                       "EXC"="Exclus",
                       "RAD"="Radies",
                       "INFSEUIL"="Seulement : AL/APL, RSA et/ou ARS < seuils de versement",
                       "SUSP"="Suspendu dossier ou RSA, AAH, AL sans autre prestation")

MOBCAF[,20] <- fct_recode(MOBCAF[,20], 
                       "AAH"="AAH ou complement AAH",
                       
                       "PFAAH"="PF + AAH ou complement AAH",
                       "ALSAAH"="ALS + AAH ou complement AAH",
                       "APLAAH"="APL + AAH ou complement AAH",
                       "DEC"="Decales (trimestre precedent : ADI, CDI, AMI)",
                       "PLALSAAH"="PF + ALS + AAH ou complement AAH",
                       "PFAPL"="PF + APL",
                       "PFAPLAAH"="PF + APL + AAH ou complement AAH",
                       "PPA"="Prime Activite seule",
                       "PFALS"="PF + ALS",
                       "RSA"="RSA  (avec ou sans PPA) sans autres prestations",
                       "PAH"="PAH legaux",
                       "ARS"="ARS seulement",
                       "EXC"="Exclus",
                       "RAD"="Radies",
                       "INFSEUIL"="Seulement : AL/APL, RSA et/ou ARS < seuils de versement",
                       "SUSP"="Suspendu dossier ou RSA, AAH, AL sans autre prestation")

MOBCAF$CATBEN2_P1 <- as.character(MOBCAF[,5])
MOBCAF$CATBEN2_P1[MOBCAF$CATBEN_P1 %in% c("PFAPLAAH", "PLALSAAH", "PFAAH")] <- "AAH_PF_AL"
MOBCAF$CATBEN2_P1[MOBCAF$CATBEN_P1 %in% c("APLAAH", "ALSAAH")] <- "AAH_AL"
MOBCAF$CATBEN2_P1[MOBCAF$CATBEN_P1 %in% c("ALS", "APL")] <- "AL"
MOBCAF$CATBEN2_P1[MOBCAF$CATBEN_P1 %in% c("PFALS", "PFAPL")] <- "PF_AL"
MOBCAF$CATBEN2_P1[MOBCAF$CATBEN_P1 %in% c("EXC",  "SUSP","INFSEUIL", "RAD",  "DEC", "PAH", "ARS")] <- "AUTRES"
MOBCAF$CATBEN2_P1 <- as.factor(as.character(MOBCAF$CATBEN2_P1))

MOBCAF$CATBEN2_P2 <- as.character(MOBCAF[,20])
MOBCAF$CATBEN2_P2[MOBCAF$CATBEN_P2 %in% c("PFAPLAAH", "PLALSAAH", "PFAAH")] <- "AAH_PF_AL"
MOBCAF$CATBEN2_P2[MOBCAF$CATBEN_P2 %in% c("APLAAH", "ALSAAH")] <- "AAH_AL"
MOBCAF$CATBEN2_P2[MOBCAF$CATBEN_P2 %in% c("ALS", "APL")] <- "AL"
MOBCAF$CATBEN2_P2[MOBCAF$CATBEN_P2 %in% c("PFALS", "PFAPL")] <- "PF_AL"
MOBCAF$CATBEN2_P2[MOBCAF$CATBEN_P2 %in% c("EXC",  "SUSP","INFSEUIL", "RAD",  "DEC", "PAH", "ARS")] <- "AUTRES"
MOBCAF$CATBEN2_P2 <- as.factor(as.character(MOBCAF$CATBEN2_P2))

# Age du responsable de dossier 
MOBCAF$AGE <- 2019 - as.numeric(str_sub(MOBCAF$DTNAIRESP, -4, -1))

saveRDS(MOBCAF,"MOBCAF_19.rds")

## Sauvegarde des Identifiants
##############################
IDE <- MOBCAF[,c("IDUNI","ID18","ID19")]
saveRDS(IDE,"IDE.rds")

## Donnees sur la situation familiale
#####################################

MENA_1218 <- read.csv("MENA_1218_verif.csv", sep=";", comment.char="#") 
MENA_1219 <- read.csv("MENA_1219_verif.csv", sep=";", comment.char="#") 

situation_fam <- function(tab){
  # Supprimer les variables nationalite car on a NATIOF et ABANEURE
  tab <- tab[,-c(5,9,10)]
  
  tab[,2] <- fct_recode(tab[,2], "CELIB"="Celibataire",
                        "DIV"="Divorce",
                        "INC"="Inconnue",
                        "MAR"="Marie",
                        "SEP"="Separe",
                        "VEUF"="Veuf",
                        "VM"="Vie maritale",
                        "PACS"="PACS")
  
  tab[,3] <- fct_recode(tab[,3], "0"="Pas de conjoint _ Service national ou abs.du foyer",
                        "1"="Presence reelle du conjoint au foyer")
  
  if(levels(as.factor(tab$TOPGRO))[1] == "Grossesse en cours"){
    tab[,4] <- fct_recode(as.character(tab[,4]), "0"="Pas de grossesse en cours",
                          "1"="Grossesse en cours")
  }
  
  # Categorisation des menages (isoles, couples, couples avec enfants, familles monoparentales)
  ############################################################################################
  
  tab$NBENLEFA <- as.numeric(as.character(tab$NBENLEFA))
  tab$PERSCOUV <- as.numeric(as.character(tab$PERSCOUV))
  tab[,8] <- as.character(tab[,8])
  
  tab$CLASS_SITFAM <- "AUTRE" # Inconnu, ou couple mais conjoint en foyer, en prison, etc.
  tab$CLASS_SITFAM[tab$SITFAM %in% c("CELIB","DIV","SEP","VEUF") & tab$PRESCONJ =="0" & tab$NBENLEFA ==0 &tab$TOPGRO == "0"]<-"ISOLE"
  tab$CLASS_SITFAM[tab$SITFAM %in% c("CELIB","DIV","SEP","VEUF") & tab$PRESCONJ =="0" & (tab$NBENLEFA >0 |tab$TOPGRO == "1")] <-"MONOP"
  tab$CLASS_SITFAM[tab$SITFAM %in% c("MAR","VM","PACS") & tab$PRESCONJ =="1" & (tab$NBENLEFA |tab$TOPGRO == "1")] <-"COU_ENF"
  tab$CLASS_SITFAM[tab$SITFAM %in% c("MAR","VM","PACS") & tab$PRESCONJ =="1" & tab$NBENLEFA &tab$TOPGRO == "0"] <-"COUPLE"
  tab$CLASS_SITFAM <- as.factor(tab$CLASS_SITFAM)
  summary(tab$CLASS_SITFAM)
  
  return(tab)
}

# evenements familiaux (booleens)
#################################
# reprendre les colonnes 
eve_fam <- function(tab1,tab2,tab3){
  # Jointure entre mena et IDE
  tab <- left_join(tab1[,c(54,15,29)],tab2[,-1])
  colnames(tab) <- c(colnames(tab)[1:3],"SITFAM_P1","PRESCONJ_P1","TOPGRO_P1","NBNAIMOI_P1","PERSCOUV_P1","NBLENFA_P1","CLASS_SITFAM_P1")
  tab <- left_join(tab,tab3[,-1])
  colnames(tab) <- c(colnames(tab)[1:10],"SITFAM_P2","PRESCONJ_P2","TOPGRO_P2","NBNAIMOI_P2","PERSCOUV_P2","NBLENFA_P2","CLASS_SITFAM_P2")
  
  # Mise en couple 
  tab$MISE_COUPLE <- "0"
  tab$MISE_COUPLE[tab$SITFAM_P1 %in% c("CELIB","DIV","SEP","VEUF") & tab$SITFAM_P2 %in% c("MAR","PACS","VM")] <- "1"
  tab$MISE_COUPLE <- as.factor(tab$MISE_COUPLE)
  summary(tab$MISE_COUPLE)
  
  # Augmentation du nombre d'enfants a charge: Naissance, adoption (a terme utiliser top grossesse, NBNAIMOI12 et ABANEURE [adoption])
  tab$PLUS_ENFANT <- "0"
  tab$PLUS_ENFANT[tab$NBLENFA_P1 < tab$NBLENFA_P2] <- "1"
  tab$PLUS_ENFANT <- as.factor(tab$PLUS_ENFANT)
  summary(tab$PLUS_ENFANT)
  
  # Augmantation taille du foyer pour une autre raison
  tab$AUG_FOYER <- "0"
  tab$AUG_FOYER[(tab$PERSCOUV_P2 - tab$PERSCOUV_P1)  > (tab$NBLENFA_P2- tab$NBLENFA_P1)] <- "1"
  tab$AUG_FOYER <- as.factor(tab$AUG_FOYER)
  summary(tab$AUG_FOYER)
  
  # Separation ou perte du conjoint
  tab$SEPARATION <- "0"
  tab$SEPARATION[tab$SITFAM_P1 %in% c("MAR","PACS","VM") & tab$SITFAM_P2 %in% c("CELIB","DIV","SEP","VEUF")] <- "1"
  tab$SEPARATION <- as.factor(tab$SEPARATION)
  summary(tab$SEPARATION)
  
  # Diminution taille foyer (decohabitation, deces d'un membre du foyer ou autre raison, on ne decompose pas les raisons pour le moment)
  tab$DIM_FOYER <- "0"
  tab$DIM_FOYER[(tab$PERSCOUV_P1 - tab$PERSCOUV_P2) > 0] <- "1"
  tab$DIM_FOYER <- as.factor(tab$DIM_FOYER)
  summary(tab$DIM_FOYER)
  
  # Naissance dans les deux annees precedentes (booleen)
  #tab$NAI_P1P2 <- 0
  #tab$NAI_P1P2[tab$NBNAIMOI_P1==1 | tab$NBNAIMOI_P2 ==1] <- 1
  #tab$NAI_P1P2 <- as.factor(tab$NAI_P1P2)
  #summary(tab$NAI_P1P2)
  
  return(tab)
}

MENA_1218 <- situation_fam(MENA_1218)
MENA_1219 <- situation_fam(MENA_1219)

EVE_FAM_1819 <- eve_fam(MOBCAF, MENA_1218, MENA_1219)

#save.image("W:/CTRAD/THEMATIQUES/Mobilites_2019/ADD/Data/MENAS_19.RData")
#rm()

# Donnees sur l'activite
#########################

ACT_1218 <- read.csv("ACT_18.csv", sep=";", comment.char="#", stringsAsFactors=FALSE)
ACT_1218 <- ACT_1218[,-1]
ACT_1219 <- read.csv("ACT_19.csv", sep=";", comment.char="#", stringsAsFactors=FALSE)

activite <- function(tab){
  
  tab[,3] <- fct_recode(tab[,3], "APP"="Apprenti",
                        "ETU_BOUR"="Etudiant boursier",
                        "ETU_NON"="Etudiant non boursier",
                        "ETU_SAL"="Etudiant salarie",
                        "NON"="Pas de statut etudiant")
  
  # Recoder l'activite
  
  tab[,1] <- fct_recode(tab[,1], 
                        "ABS"="Absent foyer",
                        "CAT"= "Activite CAT",
                        "AAP"="Activite en atelier protege",
                        "SIN"="Activite inconnue",
                        "APP"="Apprenti",                                                    
                        "AMA"= "Assistante maternelle agreee" ,                     
                        "SUR"=  "Benef. rente de survivant AT",                 
                        "CLD"=  "CAT: longue maladie",                                    
                        "SAC"= "Cessation activite benef AAH" ,                 
                        "CAC"=   "Cessation activite pour enfant" ,           
                        "ANI"=  "Ch?m. non indemnise + activ." ,                  
                        "CNI"=  "Ch?mage non indemnise" ,                        
                        "CPL"="Ch?mage partiel" ,                         
                        "CDA"= "Ch?meur aud abat. + activite" ,                         
                        "CDN"=   "Ch?meur aud neut. + activite" ,                      
                        "ADN"="Ch?meur aud neutralisation" ,                 
                        "ADA"="Ch?meur aud ou pare abattement",                       
                        "CHO"= "Ch?meur sans justificatif",                         
                        "AFD"="Ch?meur_alloc fin de droit" ,                      
                        "AFC"="Ch?meur_alloc formation reclass" ,                      
                        "AIN"="Ch?meur_alloc insertion",                  
                        "ASS"="Ch?meur_alloc solidarite specifique" ,              
                        "CCV"= "Conge conventionnel",                                      
                        "MAT"= "Conge maternite ou paternite conge mater./pater.",
                        "SAB"= "Conge sabbatique" ,                                        
                        "CSS"= "Conge sans solde" ,                                    
                        "CJT"="Conjoint collaborateur d'ETI", 
                        "CES"="Contrat emploi solidarite",
                        "CAR"= "Delai de carence ASSEDIC" ,           
                        "ETI"= "Employeur travailleur independant",               
                        "SNA"="Engage volontaire armee" ,
                        "EXP"= "ETI regime agricole" ,                      
                        "ETU"="Etudiant" ,                          
                        "EBO"="Etudiant boursier",                               
                        "ETS"="Etudiant salarie" ,                                
                        "FDA"= "Fonct. publique ch?m. aud abat." ,                         
                        "FDN"= "Fonct. publique ch?m. aud neut.",                         
                        "GSA"="Gerant salarie" ,                  
                        "INP"="Inapte",                   
                        "INC"=  "Inconnu",                                                   
                        "HAN"="Infirme handicape",                            
                        "MMC"="Mal matern. et ch?mage abat." ,                            
                        "MMI"="Mal maternite et ch?mage APL_neutralisation",       
                        "MAL"="Malade",                                                      
                        "MLD"="Maladie longue duree",                                      
                        "MAR"="Marin pêcheur",                                            
                        "MOA"="Mb org comm en activite",                                   
                        "MOC"="Mb org comm ss activite" ,                                 
                        "AMT"="Mi_temps suite plein temps" ,                                  
                        "INV"="Pension invalidite" ,                                      
                        "PRE"="Pre retraite" ,                                    
                        "RAC"="Reduction activite (CAT)",
                        "RAT"="Rente AT",                                              
                        "RET"="Retraite",                                                 
                        "RSA"="Retraite militaire < 60 ans" ,                           
                        "DNL"="Sal. non rem. duree legale",                         
                        "SAL"="Salarie" ,              
                        "CSA"="CES et salarie(e)",
                        "SSA"="Sans activite"  ,                       
                        "SAV"="Sans activite motif CDAPH (ex_COTOREP)" ,         
                        "SCO"="SCO",                                          
                        "SFP"="Stage form. professionnelle" ,                                 
                        "SNR"="Stage non remunere et RSA"  ,             
                        "PIL"="Stagiaire prog insert locale",                               
                        "RMA"="Titulaire contrat CIRMA/CAV" ,                      
                        "INT"= "Travailleur intermittent" ,                       
                        "TSA"="Travailleur saisonnier" ,               
                        "VRP"="Voyageur representant placier",
                        "VRP"="Voyageur representant placier",
                        "AFA"="Aide familial agricole",
                        "CBS"="CAT: absent du foyer")
  
  tab[,2] <- fct_recode(tab[,2], 
                        "ABS"="Absent foyer",
                        "CAT"= "Activite CAT",
                        "AAP"="Activite en atelier protege",
                        "SIN"="Activite inconnue",
                        "APP"="Apprenti",                                                    
                        "AMA"= "Assistante maternelle agreee" ,                     
                        "SUR"=  "Benef. rente de survivant AT",                 
                        "CLD"=  "CAT: longue maladie",                                    
                        "SAC"= "Cessation activite benef AAH" ,                 
                        "CAC"=   "Cessation activite pour enfant" ,           
                        "ANI"=  "Ch?m. non indemnise + activ." ,                  
                        "CNI"=  "Ch?mage non indemnise" ,                        
                        "CPL"="Ch?mage partiel" ,                         
                        "CDA"= "Ch?meur aud abat. + activite" ,                         
                        "CDN"=   "Ch?meur aud neut. + activite" ,                      
                        "ADN"="Ch?meur aud neutralisation" ,                 
                        "ADA"="Ch?meur aud ou pare abattement",                       
                        "CHO"= "Ch?meur sans justificatif",                         
                        "AFD"="Ch?meur_alloc fin de droit" ,                      
                        "AFC"="Ch?meur_alloc formation reclass" ,                      
                        "AIN"="Ch?meur_alloc insertion",                  
                        "ASS"="Ch?meur_alloc solidarite specifique" ,              
                        "CCV"= "Conge conventionnel",                                      
                        "MAT"= "Conge maternite ou paternite conge mater./pater.",
                        "SAB"= "Conge sabbatique" ,                                        
                        "CSS"= "Conge sans solde" ,                                    
                        "CJT"="Conjoint collaborateur d'ETI", 
                        "CES"="Contrat emploi solidarite",
                        "CAR"= "Delai de carence ASSEDIC" ,           
                        "ETI"= "Employeur travailleur independant",               
                        "SNA"="Engage volontaire armee" ,
                        "EXP"= "ETI regime agricole" ,                      
                        "ETU"="Etudiant" ,                          
                        "EBO"="Etudiant boursier",                               
                        "ETS"="Etudiant salarie" ,                                
                        "FDA"= "Fonct. publique ch?m. aud abat." ,                         
                        "FDN"= "Fonct. publique ch?m. aud neut.",                         
                        "GSA"="Gerant salarie" ,                  
                        "INP"="Inapte",                   
                        "INC"=  "Inconnu",                                                   
                        "HAN"="Infirme handicape",                            
                        "MMC"="Mal matern. et ch?mage abat." ,                            
                        "MMI"="Mal maternite et ch?mage APL_neutralisation",       
                        "MAL"="Malade",                                                      
                        "MLD"="Maladie longue duree",                                      
                        "MAR"="Marin pêcheur",                                            
                        "MOA"="Mb org comm en activite",                                   
                        "MOC"="Mb org comm ss activite" ,                                 
                        "AMT"="Mi_temps suite plein temps" ,                                  
                        "INV"="Pension invalidite" ,                                      
                        "PRE"="Pre retraite" ,                                    
                        "RAC"="Reduction activite (CAT)",
                        "RAT"="Rente AT",                                              
                        "RET"="Retraite",                                                 
                        "RSA"="Retraite militaire < 60 ans" ,                           
                        "DNL"="Sal. non rem. duree legale",                         
                        "SAL"="Salarie" ,              
                        "CSA"="CES et salarie(e)",
                        "SSA"="Sans activite"  ,                       
                        "SAV"="Sans activite motif CDAPH (ex_COTOREP)" ,         
                        "SCO"="SCO",                                          
                        "SFP"="Stage form. professionnelle" ,                                 
                        "SNR"="Stage non remunere et RSA"  ,             
                        "PIL"="Stagiaire prog insert locale",                               
                        "RMA"="Titulaire contrat CIRMA/CAV" ,                      
                        "INT"= "Travailleur intermittent" ,                       
                        "TSA"="Travailleur saisonnier" ,               
                        "VRP"="Voyageur representant placier",
                        "VRP"="Voyageur representant placier",
                        "AFA"="Aide familial agricole",
                        "CBS"="CAT: absent du foyer",
                        "ABS"="Inconnu ou absence de conjoint")
  
  # Statut face a l'emploi (responsable de dossier et conjoint )
  
  tab$STA_ACT_RES <- fct_recode(tab[,1], 
                                "ABS"="ABS",
                                "ACT"="AAP",
                                "CHO"="ADA", 
                                "CHO"="ADN",
                                "ACT"="AFA",
                                "CHO"="AFC",
                                "CHO"="AFD", 
                                "CHO"="AIN",
                                "ACT"="AMA",
                                "ACT"="AMT",
                                "CHO"="ANI",
                                "ACT"="APP",
                                "CHO"="ASS",
                                "INA"="CAC",
                                "CHO"="CAR",
                                "ACT"="CAT",
                                "ABS"="CBS",
                                "ACT"="CCV",
                                "CHO"="CDA", 
                                "CHO"="CDN",
                                "ACT"="CES",
                                "CHO"="CHO", 
                                "ACT"="CJT",
                                "INA"="CLD",
                                "INA"="MLD",
                                "CHO"="CNI", 
                                "CHO"="CPL",
                                "ACT"="CSA",
                                "ACT"="CSS",
                                "ACT"="DNL",
                                "INA"="EBO",
                                "INA"="ETI",
                                "ACT"="ETS",
                                "INA"="ETU", 
                                "ACT"="EXP",          
                                "CHO"="FDA",              
                                "CHO"="FDN",   
                                "ACT"="GSA",        
                                "INA"="HAN",   
                                "INC"="INC",          
                                "INA"="INP",     
                                "ACT"="INT",       
                                "INA"="INV",   
                                "INA"="MAL",   
                                "ACT"="MAR",  
                                "ACT"="MAT",
                                "CHO"="MMC", 
                                "CHO"="MMI",    
                                "ACT"="MOA",
                                "INA"="MOC",
                                "ACT"="PIL",  
                                "INA"="PRE",   
                                "INA"="RAC",
                                "INA"="RAT",     
                                "INA"="RET", 
                                "ACT"="RMA", 
                                "INA"="RSA",  
                                "ACT"="SAB",    
                                "INA"="SAC",   
                                "ACT"="SAL",  
                                "INA"="SAV",
                                "INC"="SCO",       
                                "ACT"="SFP",   
                                "INC"="SIN",
                                "ACT"="SNA",
                                "ACT"="SNR",
                                "INA"="SSA",  
                                "INA"="SUR",   
                                "ACT"="TSA",   
                                "ACT"="VRP")
  
  tab$STA_ACT_CON<- fct_recode(tab[,2], 
                               "ABS"="ABS",
                               "ACT"="AAP",
                               "CHO"="ADA", 
                               "CHO"="ADN",
                               "ACT"="AFA",
                               "CHO"="AFC",
                               "CHO"="AFD", 
                               "CHO"="AIN",
                               "ACT"="AMA",
                               "ACT"="AMT",
                               "CHO"="ANI",
                               "ACT"="APP",
                               "CHO"="ASS",
                               "INA"="CAC",
                               "CHO"="CAR",
                               "ACT"="CAT",
                               "ABS"="CBS",
                               "ACT"="CCV",
                               "CHO"="CDA", 
                               "CHO"="CDN",
                               "ACT"="CES",
                               "CHO"="CHO", 
                               "ACT"="CJT",
                               "INA"="CLD",
                               "INA"="MLD",
                               "CHO"="CNI", 
                               "CHO"="CPL",
                               "ACT"="CSA",
                               "ACT"="CSS",
                               "ACT"="DNL",
                               "INA"="EBO",
                               "INA"="ETI",
                               "ACT"="ETS",
                               "INA"="ETU", 
                               "ACT"="EXP",          
                               "CHO"="FDA",              
                               "CHO"="FDN",   
                               "ACT"="GSA",        
                               "INA"="HAN",   
                               "INC"="INC",          
                               "INA"="INP",     
                               "ACT"="INT",       
                               "INA"="INV",   
                               "INA"="MAL",   
                               "ACT"="MAR",  
                               "ACT"="MAT",
                               "CHO"="MMC", 
                               "CHO"="MMI",    
                               "ACT"="MOA",
                               "INA"="MOC",
                               "ACT"="PIL",  
                               "INA"="PRE",   
                               "INA"="RAC",
                               "INA"="RAT",     
                               "INA"="RET", 
                               "ACT"="RMA", 
                               "INA"="RSA",  
                               "ACT"="SAB",    
                               "INA"="SAC",   
                               "ACT"="SAL",  
                               "INA"="SAV",
                               "INC"="SCO",       
                               "ACT"="SFP",   
                               "INC"="SIN",
                               "ACT"="SNA",
                               "ACT"="SNR",
                               "INA"="SSA",  
                               "INA"="SUR",   
                               "ACT"="TSA",   
                               "ACT"="VRP")
  
  # Statut de retraite (responsable de dossier et conjoint)
  
  tab$RET_RES <- "NON"
  tab$RET_RES[tab$ACTRESPD %in% c("RSA","RET","PRE")] <- "RET"
  tab$RET_CON <- "NON"
  tab$RET_CON[tab$ACTCON %in% c("RSA","RET","PRE")] <- "RET"
  
  return(tab)
}

ACT_1218 <- activite(ACT_1218)
ACT_1219 <- activite(ACT_1219)



# Creer des tables "evénements professionnels", comme pour les evénements familiaux. 


MOBCAF <- readRDS("MOBCAF_19.rds")

eve_act <- function(tab1,tab2,tab3){
  tab2[,4] <- as.character(tab2[,4])
  tab3[,4] <- as.character(tab3[,4])
  # Jointure entre mena et IDE
  tab <- left_join(tab1[,c(54,15,29)],tab2)
  colnames(tab) <- c(colnames(tab)[1:3],"ACTRESPD_P1", "ACTCONJ_P1","STATUETU_P1","STA_ACT_RES_P1", "STA_ACT_CON_P1","RET_RES_P1", "RET_CON_P1")
  tab <- left_join(tab,tab3)
  colnames(tab) <- c(colnames(tab)[1:10],"ACTRESPD_P2", "ACTCONJ_P2","STATUETU_P2","STA_ACT_RES_P2", "STA_ACT_CON_P2","RET_RES_P2", "RET_CON_P2")
  
  etu <- c("EBO","ETU","ETS")
  
  tab$STA_ACT_RES_P1 <- as.character(tab$STA_ACT_RES_P1)
  tab$STA_ACT_RES_P2 <- as.character(tab$STA_ACT_RES_P2)
  tab$STA_ACT_CON_P1 <- as.character(tab$STA_ACT_CON_P1)
  tab$STA_ACT_CON_P2 <- as.character(tab$STA_ACT_CON_P2)
  tab$ACTRESPD_P1 <- as.character(tab$ACTRESPD_P1)
  tab$ACTRESPD_P2 <- as.character(tab$ACTRESPD_P2)
  tab$ACTCONJ_P1 <- as.character(tab$ACTCONJ_P1)
  tab$ACTCONJ_P2 <- as.character(tab$ACTCONJ_P2)
  tab$RET_RES_P1 <- as.character(tab$RET_RES_P1)
  tab$RET_RES_P2 <- as.character(tab$RET_RES_P2)
  tab$RET_CON_P1 <- as.character(tab$RET_CON_P1)
  tab$RET_CON_P2 <- as.character(tab$RET_CON_P2)
  
  # Mise en couple 
  tab$EVO_ACT_RES <- "INC"
  tab$EVO_ACT_RES[is.na(tab$STA_ACT_RES_P1)|is.na(tab$STA_ACT_RES_P2)] <- "ND"
  tab$EVO_ACT_RES[tab$EVO_ACT_RES != "ND" & (tab$STA_ACT_RES_P2 == tab$STA_ACT_RES_P1)] <- "STABLE"
  tab$EVO_ACT_RES[tab$EVO_ACT_RES != "ND" & (tab$STA_ACT_RES_P2 != tab$STA_ACT_RES_P1)] <- paste(tab$STA_ACT_RES_P2[tab$EVO_ACT_RES != "ND" & (tab$STA_ACT_RES_P2 != tab$STA_ACT_RES_P1)])
  
  tab$EVO_ACT_CON <- "INC"
  tab$EVO_ACT_CON[is.na(tab$STA_ACT_CON_P1)|is.na(tab$STA_ACT_CON_P2)] <- "ND"
  tab$EVO_ACT_CON[tab$EVO_ACT_CON != "ND" & (tab$STA_ACT_CON_P2 == tab$STA_ACT_CON_P1)] <- "STABLE"
  tab$EVO_ACT_CON[tab$EVO_ACT_CON != "ND" & (tab$STA_ACT_CON_P2 != tab$STA_ACT_CON_P1)] <- paste(tab$STA_ACT_CON_P2[tab$EVO_ACT_CON != "ND" & (tab$STA_ACT_CON_P2 != tab$STA_ACT_CON_P1)])
  
  tab$FIN_ETUD_RES <- "NON"
  tab$FIN_ETUD_RES[tab$ACTRESPD_P1 %in% etu & !(tab$ACTRESPD_P2 %in% etu)] <- "OUI" 
  
  tab$FIN_ETUD_CON <- "NON"
  tab$FIN_ETUD_CON[tab$ACTCONJ_P1 %in% etu & !(tab$ACTCONJ_P2 %in% etu)] <- "OUI" 
  
  tab$RETRAITE_RES <- "NON"
  tab$RETRAITE_RES[tab$RET_RES_P1 =="NON" & tab$RET_RES_P2 =="OUI"] <- "OUI" 
  
  tab$RETRAITE_CON <- "NON"
  tab$RETRAITE_CON[tab$RET_CON_P1 =="NON" & tab$RET_CON_P2 =="OUI"] <- "OUI" 
  
  return(tab)
}

EVE_ACT_1819 <- eve_act(MOBCAF,ACT_1218,ACT_1219)

rm(MOBCAF)
#save.image("W:/CTRAD/THEMATIQUES/Mobilites_2019/ADD/Data/ACTS_19.RData")
#rm()

# Donnees sur les revenus
#########################

REV_1218 <- read.csv("REV_1218.csv", sep=";", stringsAsFactors=FALSE)
REV_1219 <- read.csv("REV_1219.csv", sep=";", stringsAsFactors=FALSE)

revenus <- function(tab){

  if(names(tab)[7]=="ID18"){
    seuil<-1071
    tab$ID18 <- as.character(tab$ID18)
  }
  if(names(tab)[7]=="ID19"){
    seuil<-1096
    tab$ID19 <- as.character(tab$ID19)
  }
  
  # Remplacer les vides par des NAs
  tab[tab == "" ] <- NA
  tab$MTPRERUC[tab$MTPRERUC==99999.99] <- 0
  
  ## Discretisation 1: bas revenus, fragiles, entre fragiles et sup median, eleves, inconnus
  tab$RUCDERRE <- as.numeric(tab$RUCDERRE)
  tab$REV_CLASSE <- "NON_DEF"
  summary(as.factor(tab$REV_CLASSE))
  tab$REV_CLASSE[tab$RUCDERRE==99999.99] <- "INC"
  tab$REV_CLASSE[is.na(tab$RUCDERRE)] <- "INC"
  tab$REV_CLASSE[tab$RUCDERRE<seuil] <- "BAS_REV"
  tab$REV_CLASSE[tab$RUCDERRE>=seuil&(tab$RUCDERRE - (tab$MTPRERUC / tab$NBUC))<seuil] <- "FRAG"
  tab$REV_CLASSE[tab$REV_CLASSE =="NON_DEF" & tab$RUCDERRE < (seuil*2)] <- "MOY"
  tab$REV_CLASSE[tab$REV_CLASSE =="NON_DEF" & tab$RUCDERRE >= (seuil*2)] <- "HAUT"
  tab$REV_CLASSE <- as.factor(tab$REV_CLASSE)
  summary(as.factor(tab$REV_CLASSE))
  
  ## Discretisation 2: deciles
  tab$RUCDERRE[tab$RUCDERRE==99999.99] <- NA
  tab$REV_DEC <- decile(tab$RUCDERRE)
  tab$REV_DEC[is.na(tab$REV_DEC)] <- "INC"
  tab$REV_DEC <- as.factor(tab$REV_DEC)
  
  ## Donnees sur les revenus de patrimoine (Pour les seuils voir Piketty: quand est-ce que les revenus du patrimoine jouent un role significatif)
  ########################################
  tab$REVPAT <- "NON"
  tab$REVPAT[tab$MTREAPAT!=9999999 & tab$MTREAPAT < 6000] <- "INF_6K"
  tab$REVPAT[tab$MTREAPAT!=9999999 & tab$MTREAPAT > 6000] <- "SUP_6K"
  tab$REVPAT[tab$MTREAPAT==0] <- "NON"
  tab$REVPAT <- as.factor(tab$REVPAT)
  
  # Log du revenu
  REV_1218$log_RUC <- log10(REV_1218$RUCDERRE)
  
  # sortie
  return(tab)
}

REV_1218 <- revenus(REV_1218)
REV_1219 <- revenus(REV_1219)

save.image("REVS_19.RData")

## Donnees sur les logements 

LOG_1218 <- read.csv("LOG_18.csv", sep=";", comment.char="#", stringsAsFactors=FALSE)
LOG_1219 <- read.csv("LOG_19.csv", sep=";", comment.char="#", stringsAsFactors=FALSE)

logement <- function(tab){
  tab$ALFVERS[tab$ALFVERS == "Pas d'ALF versable"] <- 0
  tab$ALFVERS[tab$ALFVERS == "ALF calculee _ montant < minimum de versement"] <- 1
  tab$ALFVERS[tab$ALFVERS == "ALF versable _ montant > minimum de versement"] <- 2
  
  tab$ALSVERS[tab$ALSVERS == "Pas d'ALS versable"] <- 0
  tab$ALSVERS[tab$ALSVERS == "ALS calculee _ montant < minimum de versement"] <- 1 
  tab$ALSVERS[tab$ALSVERS == "ALS versable _ montant > minimum de versement"] <- 2
  
  tab$APLVERS[tab$APLVERS == "Pas d'APL versable"] <- 0
  tab$APLVERS[tab$APLVERS == "APL calculee _ montant < minimum de versement"] <- 1
  tab$APLVERS[tab$APLVERS == "APL versable _ montant > minimum de versement"] <- 2
  
  if(names(tab)[9]!="ID15"){
    tab$PARCAL[tab$PARCAL == "accession"] <- "ACC"
    tab$PARCAL[tab$PARCAL == "Foyers PA/PI, EHPAD, Centre long sejour"] <- "FOY1"
    tab$PARCAL[tab$PARCAL == "Logements foyers (hors PA/PI)"] <- "FOY2"
    tab$PARCAL[tab$PARCAL == "Sans signification"] <- "NON_SIGNIF"
    tab$PARCAL[tab$PARCAL == "Location HLM"] <- "HLM"
    tab$PARCAL[tab$PARCAL == "Residence universitaire CROUS"] <- "CROUS"
    
    tab$PARCAPL[tab$PARCAPL == "Accession autre"] <- "ACC_AUT"
    tab$PARCAPL[tab$PARCAPL == "Accession neuve"] <- "ACC_NEUV"
    tab$PARCAPL[tab$PARCAPL == "APL _ Foyer"] <- "APL_FOY"
    tab$PARCAPL[tab$PARCAPL == "Location"] <- "LOC"
    tab$PARCAPL[tab$PARCAPL == "Sans signification ou DOM"] <- "NON_SIGNIF_DOM"
  }
  
  tab$PPRPPU[tab$PPRPPU == "Parc inconnu"] <- "INCONNU"
  tab$PPRPPU[tab$PPRPPU == "Parc prive"] <- "PRIVE"
  tab$PPRPPU[tab$PPRPPU == "Parc public"] <- "PUBLIC"
  tab$PPRPPU[tab$PPRPPU =="Pas de prestation logement ni impaye de loyer"] <- "NS"
  
  tab$IMPAYE <- "NON"
  tab$IMPAYE[tab$ETATIMPA!=""] <- "OUI"
  tab$IMPAYE <- as.factor(tab$IMPAYE)
  
  # Remplacer les vides par des NA
  tab[tab == "" ] <- NA
  
  # Synthèse aides au logement 
  tab$AL_VERS <- 0
  tab$AL_VERS[tab$ALFVERS != 0 | tab$ALSVERS != 0 | tab$APLVERS != 0 ] <- 1
  
  return(tab)
}

LOG_1218 <- logement(LOG_1218)
LOG_1219 <- logement(LOG_1219)

save.image("LOGS_19.RData")
#rm()

### Donnees de georeferencement 

GEO_1218 <- read.csv("GEO_18.csv", sep=";", stringsAsFactors=FALSE) %>% na.exclude()
GEO_1219 <- read.csv("GEO_19.csv", sep=";", stringsAsFactors=FALSE) %>% na.exclude()

coordinates(GEO_1218) <- c(4,5)
coordinates(GEO_1219) <- c(5,6)

GEO_1218 <- st_as_sf(GEO_1218)
st_crs(GEO_1218) <- 2154
GEO_1219 <- st_as_sf(GEO_1219)
st_crs(GEO_1219) <- 2154

GEO_1218$IDUNI2 <- 1:nrow(GEO_1218)
GEO_1219$IDUNI2 <- 1:nrow(GEO_1219)

load("temp.RData")

grille200 <- sf::st_read("grille200m_metropole.shp") %>% st_transform(2154)

GEO_1218 <- st_intersection(grille200, GEO_1218)
GEO_1219 <- st_intersection(grille200, GEO_1219)
rm(grille200)

temp <- as.data.frame(temp)
temp <- temp[,-4]
temp8 <- as.data.frame(temp8)
temp8 <- temp8[,-4]
GEO_1218 <- left_join(GEO_1218, temp8)
GEO_1219 <- left_join(GEO_1219, temp)

colnames(GEO_1218)[c(1,2)] <- c("NORDALLC_18","NUMCAF_18")
colnames(GEO_1219)[c(1,2)] <- c("NUMCAF_19","NORDALLC_19")

GEO_1218 <- as.data.frame(GEO_1218)
GEO_1219 <- as.data.frame(GEO_1219)

GEO_1218 <- GEO_1218[,-c(6,9)]
GEO_1219 <- GEO_1219[,-c(6,9)]
rm(temp, temp8, MOBCAF)
save.image("GEO_19.RData")

### Donnees sur les arrivants
#############################

#ARRIV <- read.csv("ARRIV_19.csv", sep=";", comment.char="#",stringsAsFactors=FALSE)

### Description des mobilites sortantes
#######################################

MOBCAF <- readRDS("MOBCAF_19.rds")

MOBCAF$MOBILE2 <- "NON"
MOBCAF$MOBILE2[substr(MOBCAF$MOBILE, 1,3)=="CHG"] <- "OUI"

MOBCAF$CHANG_COM <- "NON"
MOBCAF$CHANG_COM[MOBCAF$NUMCOMDO_19 != MOBCAF$NUMCOMDO_18] <- "OUI"

MOBCAF$CHANG_DEP <- "NON"
MOBCAF$CHANG_DEP[MOBCAF$DEPTPR != MOBCAF$DEPTCD] <- "OUI"

MOBCAF$MRS_PARIS <- "NON"
MOBCAF$MRS_PARIS[MOBCAF$DEPTPR!="75" & MOBCAF$DEPTCD=="75"] <- "OUI"

# Identification des sortants du p?le
#MOBCAF <- left_join(MOBCAF, ZAU10[,c()], by=c("NUMCOMDO_P1"="INSEE_COM"))
#colnames(MOBCAF) <- c(colnames(MOBCAF)[1:(ncol(MOBCAF)-1)],"TAUP1")
#MOBCAF <- left_join(MOBCAF, ZAU10[,c()], by=c("NUMCOMDO_P2"="INSEE_COM"))
#colnames(MOBCAF) <- c(colnames(MOBCAF)[1:(ncol(MOBCAF)-1)],"TAUP2")
#MOBCAF$MRS_111 <- "NON"
#MOBCAF$MRS_111[MOBCAF$TAUP2!="111"&MOBCAF$TAUP2=="111"] <- "OUI"
  
## Creation de la variable MRS_IDF
MOBCAF$MRS_IDF <- "NON"
MOBCAF$MRS_IDF[(!(MOBCAF$DEPTPR %in% c("75","77","78","91","92","93","94","95")))& MOBCAF$DEPTCD %in% c("75","77","78","91","92","93","94","95")] <- "OUI"

saveRDS(MOBCAF,"MOBCAF_19.rds")

### Jointure avec les tables thématiques
########################################
library(dplyr)
MOBCAF <- readRDS("MOBCAF_19.rds")

load("MENAS_19.RData")
MOBCAF <- left_join(MOBCAF, EVE_FAM_1819[,c(1,6,8,9,10,13,15,16:22)], by="IDUNI")
rm(MENA_1218, MENA_1219, EVE_FAM_1819)

load("ACTS_19.RData")
MOBCAF <- left_join(MOBCAF, EVE_ACT_1819[,c(1,4:23)], by="IDUNI")
rm(ACT_1218, ACT_1219, EVE_ACT_1819)

load("REVS_19.RData")
MOBCAF <- left_join(MOBCAF, REV_1218[,c(6:10,3,5)], by="ID18")
rm(REV_1218, REV_1219)

load("LOGS_19.RData")
LOG_1218$ID18 <- as.character(LOG_1218$ID18)
LOG_1219$ID19 <- as.character(LOG_1219$ID19)
MOBCAF <- left_join(MOBCAF, LOG_1218[,c(11,13:15,5,9)], by="ID18")
colnames(MOBCAF) <- c(colnames(MOBCAF)[1:(ncol(MOBCAF)-5)],"PPRPPU_P1", "IMPAYE_P1","ALVERS_P1", "OCCLOG_P1", "NBCOH_P1")
MOBCAF <- left_join(MOBCAF, LOG_1219[,c(11,13:15,5,9)], by="ID19")
colnames(MOBCAF) <- c(colnames(MOBCAF)[1:(ncol(MOBCAF)-5)],"PPRPPU_P2","IMPAYE_P2", "ALVERS_P2", "OCCLOG_P2", "NBCOH_P2")
rm(LOG_1218, LOG_1219)

load("GEO_19.RData")
GEO_1218 <- GEO_1218[,-3]
GEO_1219 <- GEO_1219[,-4]
colnames(GEO_1218)[3:6] <- c("NUMCOMDO_18","QUALXY_18","idINSPIRE_18","id_carr_1k_18")
colnames(GEO_1219)[3:6] <- c("NUMCOMDO_19","QUALXY_19","idINSPIRE_19","id_carr_1k_19")
MOBCAF$NUMCOMDO_18 <- as.character(MOBCAF$NUMCOMDO_18)
MOBCAF$NUMCOMDO_19 <- as.character(MOBCAF$NUMCOMDO_19)
MOBCAF <- left_join(MOBCAF, GEO_1218)
MOBCAF <- left_join(MOBCAF, GEO_1219)
rm(GEO_1218, GEO_1219)
save.image("ALLOCS19_0.RData")

### Mesures de distance et de péripherisation
#############################################

library(sf)
load("ALLOCS19_1.RData")
MOBCAF$idINSPIRE_18[MOBCAF$MOBILE=="NON"] <- MOBCAF$idINSPIRE_19[MOBCAF$MOBILE=="NON"]
MOBCAF$id_carr_1k_18[MOBCAF$MOBILE=="NON"] <- MOBCAF$id_carr_1k_19[MOBCAF$MOBILE=="NON"]
MOBCAF$QUALXY_18[MOBCAF$MOBILE=="NON"] <- MOBCAF$QUALXY_19[MOBCAF$MOBILE=="NON"]
MOBCAF$X_18[MOBCAF$MOBILE=="NON"] <- MOBCAF$X_19[MOBCAF$MOBILE=="NON"]
MOBCAF$Y_18[MOBCAF$MOBILE=="NON"] <- MOBCAF$Y_19[MOBCAF$MOBILE=="NON"]

# Distance euclidienne (calcul a partir des coordonnees)
########################################################
  
# Entre les deux logements 
MOBCAF$DIST_PARC_EUC <- sqrt((MOBCAF$X_18 - MOBCAF$X_19)^2+(MOBCAF$Y_18 - MOBCAF$Y_19)^2)

# Evolution de la distance a Paris (Hotel de ville)
HVPARIS <- data.frame("HVPARIS",2.35,48.85) # Coordonnees recuperees sur OSM
colnames(HVPARIS) <- c("ID","x","y")
HVPARIS <- st_as_sf(HVPARIS,coords = c(2,3))
st_crs(HVPARIS) <- 4326
HVPARIS <- st_transform(HVPARIS, 2154) # Conversion en Lambert
HDV_L93 <- st_coordinates(HVPARIS)
X_HDV <- HDV_L93[,1]
Y_HDV <- HDV_L93[,2]
  
MOBCAF$DIST_PARIS_EUC_18 <- sqrt((X_HDV - MOBCAF$X_18)^2+(Y_HDV - MOBCAF$Y_18)^2)
MOBCAF$DIST_PARIS_EUC_19 <-  sqrt((X_HDV - MOBCAF$X_19)^2+(Y_HDV - MOBCAF$Y_19)^2)
MOBCAF$EVOL_DIST_PARIS_EUC  <- MOBCAF$DIST_PARIS_EUC_19 - MOBCAF$DIST_PARIS_EUC_18

rm(X_HDV, Y_HDV, HVPARIS, HDV_L93)

## Alleger la table 
#MOBCAF <- MOBCAF[,-c(1:4,10,12,14:19,24,26:28,30, 44:47)]
save.image("ALLOCS19_2.RData")

# Matrice d'interaction pour mesures de distance osrm
#####################################################

load("ALLOCS19_2.RData")
matcaf_19 <- MOBCAF[,c(91,93)]
matcaf_19 <- na.exclude(matcaf_19)
matcaf_19 <- matcaf_19[!(duplicated(matcaf_19)),]
matcaf_19 <- matcaf_19[matcaf_19$idINSPIRE_18!=matcaf_19$idINSPIRE_19,]
saveRDS(matcaf_19,"tempMD.rds")

# mesure des distances routières (entre origine et destination et eloignement a l'hotel de ville)

library(dplyr)
library(sf)

load("grille200_c.RData")
grille200_c <- grille200_c[,-2]

colnames(grille200_c) <- c("idINSPIRE", "centroids_200_P1")
matcaf19 <- left_join(matcaf19, grille200_c, by=c("idINSPIRE_18"="idINSPIRE"))
colnames(grille200_c) <- c("idINSPIRE", "centroids_200_P2")
matcaf19 <- left_join(matcaf19, grille200_c, by=c("idINSPIRE_19"="idINSPIRE"))
matcaf19 <- matcaf19[,c(1,3,2,4)]
rm(grille200_c)

adr18 <- as.data.frame(matcaf19[,1:2])
colnames(adr18) <- c("ID","geometry")
adr19 <- as.data.frame(matcaf19[,3:4])
colnames(adr19) <- c("ID","geometry")

CARR_19 <- rbind(adr18, adr19)
CARR_19 <- CARR_19[!(duplicated(CARR_19$ID)),]
CARR_19 <- st_as_sf(CARR_19)
#saveRDS(CARR_19,"CARR_19.rds")
rm(adr19, adr18)

#Mesures de distance 

library(osrm)
options(osrm.server = "", osrm.profile = "driving")

origins <- matcaf19[!(duplicated(matcaf19$idINSPIRE_18)),1:2]
origins <- st_as_sf(origins)
st_crs(origins) <- 4326
rownames(origins) <- origins$idINSPIRE_18

destinations <- matcaf19[, c(1,3,4)]
destinations <- st_as_sf(destinations)
st_crs(destinations) <- 4326

labs_origins <- as.character(origins$idINSPIRE_18)
origins$idINSPIRE_18 <- as.character(origins$idINSPIRE_18)
destinations$idINSPIRE_18 <- as.character(destinations$idINSPIRE_18)
destinations$idINSPIRE_19 <- as.character(destinations$idINSPIRE_19)

# Initialisation 
mats <- vector(mode = "list", length = nrow(origins))

# Boucle
l <- nrow(origins)
for(i in 13991:l){
  x <- osrmTable(src = origins[origins$idINSPIRE_18==labs_origins[i],], dst= destinations[destinations$idINSPIRE_18==labs_origins[i],2:3], measure = "duration")$durations
  x <- as.data.frame(t(x))
  x$dst <- colnames(x)
  colnames(x)[1]<- "dist"
  x$ori <- destinations$idINSPIRE_19[rownames(destinations) %in% rownames(x)]
  rownames(x) <- 1:nrow(x)
  x <- x[,c(3,2,1)]
  mats[[i]] <- x
  if(i %in% seq(1,l, 1000)){
    print(i)
  }
}

# Rassembler les tableaux
matdist_osrm <- do.call(rbind, mats)
colnames(matdist_osrm) <- c("idINSPIRE_19","idINSPIRE_18","DIST_PARC_OSRM")  # labels origines et destinations inverses dans la matrice osrm

matdist_osrm1819 <- matdist_osrm
saveRDS(matdist_osrm1819, "matdist_osrm1819.rds")
rm(x, mats,labs_origins,destinations, origins, matdist_osrm)

# Evolution de la distance a Paris (Distance-temps a l'hotel de ville)
################################################################

# Coordonnees hotel de ville
HVPARIS <- data.frame("HVPARIS",2.35,48.85)
colnames(HVPARIS) <- c("ID","x","y")
HVPARIS <- st_as_sf(HVPARIS,coords = c(2,3))
st_crs(HVPARIS) <- 4326

CARR <- readRDS("CARR.rds")
CARR$ID <- as.character(CARR$ID) # Essentiel sinon res va peser plusiers GB en reprenant les 13M de niveaux de idINSPIRE a chaque fois
CARR_19 <- CARR_19[!(CARR_19$ID %in% CARR$ID),]

res_temp <- osrmTable(src=CARR_19,dst=HVPARIS,measure="duration")
dist_centre_paris_19 <- cbind(CARR_19,res_temp$sources,res_temp$durations)
colnames(dist_centre_paris_19)[4] <- "Dist_osrm_hdv"

dist_centre_paris <- readRDS("../dist_centre_paris_15_18.rds")
dist_centre_paris <- rbind(dist_centre_paris, dist_centre_paris_19)

# Sauvegarde
saveRDS(dist_centre_paris,"dist_centre_paris_15_19.rds")

#########################################
# Ajout des donnees a la table principale

load("ALLOCS19_2.RData")
dist_centre_paris <- readRDS("dist_centre_paris_15_19.rds")
dist_centre_paris <- as.data.frame(dist_centre_paris)
dist_centre_paris <- dist_centre_paris[,c(1,4)]

matdist_orsm <- readRDS("matdist_osrm1819.rds")
MOBCAF <- left_join(MOBCAF, matdist_orsm)

colnames(dist_centre_paris) <- c("ID","dist_osrm_hdv_P1")
MOBCAF <- left_join(MOBCAF, dist_centre_paris, by=c("idINSPIRE_18"="ID"))
colnames(dist_centre_paris) <- c("ID","dist_osrm_hdv_P2")
MOBCAF <- left_join(MOBCAF, dist_centre_paris, by=c("idINSPIRE_19"="ID"))
MOBCAF$EVOL_DIST_PARIS_OSRM <- MOBCAF$dist_osrm_hdv_P2 - MOBCAF$dist_osrm_hdv_P1

rm(dist_centre_paris, matdist_orsm)
save.image("ALLOCS19_3.RData")

# Autres mesures de peripherisation
###################################

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



# Fonction pour jointures et calculs des indicateurs
####################################################

potentiels <- function(ALLOCS, pot_pop, pot_emploi, pot_transports, pot_equips){
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

#load("LLOCS19_3.RData")
MOBCAF <- potentiels(MOBCAF,pot_pop_1k,pot_emploi_1k,pot_transports_AU1,res_bpe)

rm(pot_pop_1k,pot_emploi_1k,pot_transports_AU1,res_bpe, test)

log10_indicators <- function(tab){
  # Distance parcourue
  tab$log10_distparc_euc <- log10(as.numeric(tab$DIST_PARC_EUC))
  tab$log10_distparc_osrm <- log10(as.numeric(tab$DIST_PARC_OSRM))
  
  # Distance a Paris (P1, P2 et delta, euclidien/rectilineaire et osrm)
  tab$log10_dist75_euc_P1  <- log10(as.numeric(tab$DIST_PARIS_EUC_18))
  tab$log10_dist75_euc_P2  <- log10(as.numeric(tab$DIST_PARIS_EUC_19))
  tab$log10_dist75_osrm_P1  <- log10(as.numeric(tab$dist_osrm_hdv_P1))
  tab$log10_dist75_osrm_P2  <- log10(as.numeric(tab$dist_osrm_hdv_P2))
  
  # Menages accessibles (P1, P2 et delta)
  tab$log10_pop_access_P1 <- log10(as.numeric(tab$Mena_acces_euc_P1))
  tab$log10_pop_access_P2 <- log10(as.numeric(tab$Mena_acces_euc_P2))
  
  # Emplois accessibles (P1, P2 et delta, ensemble et par CSP)
  tab$log10_emp_access_P1 <- log10(as.numeric(tab$EMP_acces_euc_P1))
  tab$log10_emp_access_P2 <- log10(as.numeric(tab$EMP_acces_euc_P2))
  tab$log10_CS3_access_P1 <- log10(as.numeric(tab$CS3_ACC_P1))
  tab$log10_CS3_access_P2 <- log10(as.numeric(tab$CS3_ACC_P2))
  tab$log10_CS4_access_P1 <- log10(as.numeric(tab$CS4_ACC_P1))
  tab$log10_CS4_access_P2 <- log10(as.numeric(tab$CS4_ACC_P2))
  tab$log10_CS5_access_P1 <- log10(as.numeric(tab$CS5_ACC_P1))
  tab$log10_CS5_access_P2 <- log10(as.numeric(tab$CS5_ACC_P2))
  tab$log10_CS6_access_P1 <- log10(as.numeric(tab$CS6_ACC_P1))
  tab$log10_CS6_access_P2 <- log10(as.numeric(tab$CS6_ACC_P2))
  
  # Distance minimale aux equipement (P1, P2 et delta, par gamme)
  tab$log10_dist_prox_BPE_P1 <- log10(as.numeric(tab$mean_prox_P1))
  tab$log10_dist_prox_BPE_P2 <- log10(as.numeric(tab$mean_prox_P2))
  tab$log10_dist_inter_BPE_P1 <- log10(as.numeric(tab$mean_inter_P1))
  tab$log10_dist_inter_BPE_P2 <- log10(as.numeric(tab$mean_inter_P2))
  tab$log10_dist_sup_BPE_P1 <- log10(as.numeric(tab$mean_sup_P1))
  tab$log10_dist_sup_BPE_P2 <- log10(as.numeric(tab$mean_sup_P2))
  
  # Accès aux transports en commmun (P1, P2 et delta, score)
  #tab$log10_scoreTEC_P1 <- log10(as.numeric(tab$SCORE_TEC_P1))
  #tab$log10_scoreTEC_P2 <- log10(as.numeric(tab$SCORE_TEC_P2))
  
  # Calculer les evolutions du log 
  #tab$evol_log_dist75_osrm <- tab$log10_dist75_osrm_P2 - tab$log10_dist75_osrm_P1
  #tab$evol_log_pop_acc <- tab$log10_pop_access_P2 - tab$log10_pop_access_P1
  #tab$evol_log_mean_prox <- tab$log10_dist_prox_BPE_P2 - tab$log10_dist_prox_BPE_P1
  #tab$evol_log_scoreTEC <- tab$log10_scoreTEC_P2 - tab$log10_scoreTEC_P1
  
  return(tab)
}

MOBCAF <- log10_indicators(MOBCAF)

save.image("ALLOCS19_4.RData")

# QPV et grand Paris
####################

#load("ALLOCS19_4.RData")

# Chargement des perimètres QPV
#QPV <- st_read("QPV_IDF_L93.shp")
#QPV <- QPV[,1]

# Pour le moment on laisse de cote les projets d'amenagement
#projet_AMENA <- st_read("Projets_d'amenagement_en_IledeFrance.shp")

# Chargement des perimètres de gare GPE
#gareGPE_800 <- st_read("GPE800.shp") %>% select(OBJECTID)
#st_crs(gareGPE_800) <- 2154

# Intersection spatiale
#grille200_c_IDF <- st_read("grille200_c_IDF.shp")
#st_crs(grille200_c_IDF) <- 2154

#QPV_tj <- st_intersection(grille200_c_IDF,QPV)
#QPV_tj$QPV <- 1 
#GPE_tj <- st_intersection(grille200_c_IDF,gareGPE_800)
#GPE_tj$GPE <- 1

# dataframes et deboublonner
#QPV_tj <- as.data.frame(QPV_tj)
#QPV_tj <- QPV_tj[,c(1,5)]
#GPE_tj <- as.data.frame(GPE_tj)
#GPE_tj <- GPE_tj[,c(1,5)]

# Fonction intersection et jointure

##############################################################
## ATTENTION: PB doubles jointures ici, on cr?? des doublons #
##############################################################

#test <- head(MOBCAF)
#colnames(QPV_tj) <- c("idINSPIRE_18","QPV_P1")
#colnames(GPE_tj) <- c("idINSPIRE_18","GPE_P1")
#MOBCAF <- MOBCAF %>% left_join(QPV_tj)
#MOBCAF <- MOBCAF %>% left_join(GPE_tj)
#colnames(QPV_tj) <- c("idINSPIRE_19","QPV_P2")
#colnames(GPE_tj) <- c("idINSPIRE_19","GPE_P2")
#MOBCAF <- MOBCAF %>% left_join(QPV_tj)
#MOBCAF <- MOBCAF %>% left_join(GPE_tj)

# Donnees sur les prix (donnees pas a jour sur ZENODO pour le moment)
#library(cartography)
#CASSMIR <- readOGR("CASSMIR_SpatialDataBase.gpkg", "Grid1km")
#CASSMIR <- st_as_sf(CASSMIR) %>% filter(annee==2012) %>% select(Id_carr1km, B_PX_Moyen)

# Visualisation 
#choroLayer(CASSMIR, var="B_PX_Moyen", border = NA)

# Passage au log
#CASSMIR$log_PX_Med <- log10(CASSMIR$B_PX_Moyen)

# Jointure 
#CASSMIR <- as.data.frame(CASSMIR)
#CASSMIR <- CASSMIR[,-3]
#colnames(CASSMIR) <- c("id_carr_1k_18","PX_Moy_P1", "log_PX_Moy_P1")
#MOBCAF <- MOBCAF %>% left_join(CASSMIR)
#colnames(CASSMIR) <- c("id_carr_1k_19","PX_Moy_P2", "log_PX_Moy_P2")
#MOBCAF <- MOBCAF %>% left_join(CASSMIR)
#rm(CASSMIR)
#rm(gareGPE_800, GPE_tj, QPV, QPV_tj, grille200_c_IDF)
save.image("ALLOCS19_5.RData")

###########
# Etape 5 #
###########


# Parsing and relevel
load("ALLOCS19_5.RData")

parsing_relevel_cars <- function(main_table){
  
  # Correction des booleens 
  main_table$SEXE[main_table$SEXE=="Feminin"] <- "0"
  main_table$SEXE[main_table$SEXE=="Masculin"] <- "1"

  main_table$CHANG_COM[main_table$CHANG_COM=="NON"] <- "0"
  main_table$CHANG_COM[main_table$CHANG_COM=="OUI"] <- "1"
  main_table$CHANG_DEP[main_table$CHANG_DEP=="NON"] <- "0"
  main_table$CHANG_DEP[main_table$CHANG_DEP=="OUI"] <- "1"
  main_table$MRS_PARIS[main_table$MRS_PARIS=="NON"] <- "0"
  main_table$MRS_PARIS[main_table$MRS_PARIS=="OUI"] <- "1"
  main_table$MRS_IDF[main_table$MRS_IDF=="NON"] <- "0"
  main_table$MRS_IDF[main_table$MRS_IDF=="OUI"] <- "1"
  summary(main_table$MOBILE)
  
  main_table$MOBILE2 <- as.character(main_table$MOBILE2)
  main_table$MOBILE2[main_table$MOBILE2=="NON"] <- "0"
  main_table$MOBILE2[main_table$MOBILE2=="OUI"] <- "1"
  
  main_table$RET_RES_P1[main_table$RET_RES_P1=="NON"] <- "0"
  main_table$RET_RES_P1[main_table$RET_RES_P1=="RET"] <- "1"
  main_table$RET_CON_P1[main_table$RET_CON_P1=="NON"] <- "0"
  main_table$RET_CON_P1[main_table$RET_CON_P1=="RET"] <- "1"
  main_table$RET_RES_P2[main_table$RET_RES_P2=="NON"] <- "0"
  main_table$RET_RES_P2[main_table$RET_RES_P2=="RET"] <- "1"
  main_table$RET_CON_P2[main_table$RET_CON_P2=="NON"] <- "0"
  main_table$RET_CON_P2[main_table$RET_CON_P2=="RET"] <- "1"
  
  main_table$FIN_ETUD_RES[main_table$FIN_ETUD_RES=="NON"] <- "0"
  main_table$FIN_ETUD_RES[main_table$FIN_ETUD_RES=="OUI"] <- "1"
  main_table$FIN_ETUD_CON[main_table$FIN_ETUD_CON=="NON"] <- "0"
  main_table$FIN_ETUD_CON[main_table$FIN_ETUD_CON=="OUI"] <- "1"
  
  main_table$IMPAYE_P1 <- as.character(main_table$IMPAYE_P1)
  main_table$IMPAYE_P1[main_table$IMPAYE_P1=="NON"] <- "0"
  main_table$IMPAYE_P1[main_table$IMPAYE_P1=="OUI"] <- "1"
  main_table$IMPAYE_P2 <- as.character(main_table$IMPAYE_P2)
  main_table$IMPAYE_P2[main_table$IMPAYE_P2=="NON"] <- "0"
  main_table$IMPAYE_P2[main_table$IMPAYE_P2=="OUI"] <- "1"
  
  main_table$ALVERS_P1[main_table$ALVERS_P1=="NON"] <- "0"
  main_table$ALVERS_P1[main_table$ALVERS_P1=="OUI"] <- "1"
  main_table$ALVERS_P2[main_table$ALVERS_P2=="NON"] <- "0"
  main_table$ALVERS_P2[main_table$ALVERS_P2=="OUI"] <- "1"

  # Parsing 
  main_table$idINSPIRE_18 <- as.character(main_table$idINSPIRE_18)
  main_table$idINSPIRE_19 <- as.character(main_table$idINSPIRE_19)
  main_table$id_carr_1k_18 <- as.character(main_table$id_carr_1k_18)
  main_table$id_carr_1k_19 <- as.character(main_table$id_carr_1k_19)
  
  main_table$SEXE <- as.factor(as.character(main_table$SEXE))
  main_table$ND2 <- as.factor(as.character(main_table$ND2))
  main_table$NATIOF <- as.factor(as.character(main_table$NATIOF))
  
  main_table$MOBILE <- as.factor(as.character(main_table$MOBILE))
  main_table$MOBILE2 <- as.factor(as.character(main_table$MOBILE2))
  main_table$MRS_PARIS <- as.factor(as.character(main_table$MRS_PARIS))
  main_table$MRS_IDF <- as.factor(as.character(main_table$MRS_IDF))
  main_table$CHANG_COM <- as.factor(as.character(main_table$CHANG_COM))
  main_table$CHANG_DEP <- as.factor(as.character(main_table$CHANG_DEP))
  main_table$DEPTCD <- as.factor(as.character(main_table$DEPTCD))
  main_table$DEPTPR <- as.factor(as.character(main_table$DEPTPR))
  
  main_table$IMPAYE_P1 <- as.factor(as.character(main_table$IMPAYE_P1))
  main_table$IMPAYE_P2 <- as.factor(as.character(main_table$IMPAYE_P2))
  main_table$ALVERS_P1 <- as.factor(as.character(main_table$ALVERS_P1))
  main_table$ALVERS_P2 <- as.factor(as.character(main_table$ALVERS_P2))
  main_table$PPRPPU_P1 <- as.factor(as.character(main_table$PPRPPU_P1))
  main_table$PPRPPU_P2 <- as.factor(as.character(main_table$PPRPPU_P2))
  main_table$OCCLOG_P1 <- as.factor(as.character(main_table$OCCLOG_P1))
  main_table$OCCLOG_P2 <- as.factor(as.character(main_table$OCCLOG_P2))
  
  main_table$MISE_COUPLE <- as.factor(as.character(main_table$MISE_COUPLE))
  main_table$TOPGRO_P1 <- as.factor(as.character(main_table$TOPGRO_P1))
  main_table$TOPGRO_P2 <- as.factor(as.character(main_table$TOPGRO_P2))
  
  main_table$ACTRESPD_P1 <- as.factor(as.character(main_table$ACTRESPD_P1))
  main_table$ACTRESPD_P2 <- as.factor(as.character(main_table$ACTRESPD_P2))
  main_table$ACTCONJ_P1 <- as.factor(as.character(main_table$ACTCONJ_P1))
  main_table$ACTCONJ_P2 <- as.factor(as.character(main_table$ACTCONJ_P2))
  
  main_table$STA_ACT_RES_P1 <- as.factor(as.character(main_table$STA_ACT_RES_P1))
  main_table$STA_ACT_RES_P2 <- as.factor(as.character(main_table$STA_ACT_RES_P2))
  main_table$STA_ACT_CON_P1 <- as.factor(as.character(main_table$STA_ACT_CON_P1))
  main_table$STA_ACT_CON_P2 <- as.factor(as.character(main_table$STA_ACT_CON_P2))
  main_table$EVO_ACT_RES <- as.factor(as.character(main_table$EVO_ACT_RES))
  main_table$EVO_ACT_CON <- as.factor(as.character(main_table$EVO_ACT_CON))
  
  main_table$RET_RES_P1 <- as.factor(as.character(main_table$RET_RES_P1))
  main_table$RET_CON_P1 <- as.factor(as.character(main_table$RET_CON_P1))
  main_table$RET_RES_P2 <- as.factor(as.character(main_table$RET_RES_P2))
  main_table$RET_CON_P2 <- as.factor(as.character(main_table$RET_CON_P2))
  main_table$FIN_ETUD_RES <- as.factor(as.character(main_table$FIN_ETUD_RES))
  main_table$FIN_ETUD_CON <- as.factor(as.character(main_table$FIN_ETUD_CON))
  
  main_table$CATBEN2_P1 <- as.factor(as.character(main_table$CATBEN2_P1))
  main_table$CATBEN2_P2 <- as.factor(as.character(main_table$CATBEN2_P2))
  
  # relevels
  main_table$CLASS_SITFAM_P1 <- factor(main_table$CLASS_SITFAM_P1, levels = c("ISOLE","COUPLE","AUTRE","COU_ENF","MONOP"))
  main_table$CLASS_SITFAM_P2 <- factor(main_table$CLASS_SITFAM_P2, levels = c("ISOLE","COUPLE","AUTRE","COU_ENF","MONOP"))
  main_table$REV_CLASSE <- factor(main_table$REV_CLASSE, levels = c("BAS_REV","FRAG","MOY","HAUT","INC"))
  main_table$REVPAT <- factor(main_table$REVPAT, levels = c("NON","INF_6K","SUP_6K"))
  main_table$MOBILE2 <- factor(main_table$MOBILE2, levels = c("0","1"))
  main_table$MOBILE <- factor(main_table$MOBILE, levels = c("NON","OUI","RUE"))
  main_table$MRS_PARIS <- factor(main_table$MRS_PARIS, levels = c("0","1"))
  main_table$CHANG_COM <- factor(main_table$CHANG_COM, levels = c("0","1"))
  main_table$MRS_IDF <- factor(main_table$MRS_IDF, levels = c("0","1"))
  main_table$NATIOF <- factor(main_table$NATIOF, levels = c("1","2","3","0"))
  
  main_table$STA_ACT_RES_P1 <- factor(main_table$STA_ACT_RES_P1, levels = c("ACT","CHO","INA","INC", "ABS"))
  main_table$STA_ACT_RES_P2 <- factor(main_table$STA_ACT_RES_P2, levels = c("ACT","CHO","INA","INC", "ABS"))
  main_table$STA_ACT_CON_P1 <- factor(main_table$STA_ACT_CON_P1, levels = c("ACT","CHO","INA","INC","ABS"))
  main_table$STA_ACT_CON_P2 <- factor(main_table$STA_ACT_CON_P2, levels = c("ACT","CHO","INA","INC","ABS"))
  
  main_table$EVO_ACT_RES <- factor(main_table$EVO_ACT_RES, levels = c( "STABLE","ACT","CHO","INA","INC","ABS"))
  main_table$EVO_ACT_CON <- factor(main_table$EVO_ACT_CON, levels = c( "STABLE","ACT","CHO","INA","INC","ABS"))
  
  main_table$STATUETU_P1 <- factor(main_table$STATUETU_P1, levels = c("NON","APP","ETU_BOUR","ETU_NON","ETU_SAL")) 
  main_table$STATUETU_P2 <- factor(main_table$STATUETU_P2, levels = c("NON","APP","ETU_BOUR","ETU_NON","ETU_SAL")) 
  
  main_table$CATBEN2_P1 <- factor(main_table$CATBEN2_P1, levels = c("PF",levels(main_table$CATBEN2_P1)[c(1:7,9:15)]))
  main_table$CATBEN2_P2 <- factor(main_table$CATBEN2_P2, levels = c("PF",levels(main_table$CATBEN2_P2)[c(1:10,12:20)]))

  # Aides au logement
  main_table$PPRPPU_P1 <- factor(main_table$PPRPPU_P1, levels = c("NS","PRIVE","PUBLIC","INCONNU"))
  main_table$PPRPPU_P2 <- factor(main_table$PPRPPU_P2, levels = c("NS","PRIVE","PUBLIC","INCONNU"))
  
  main_table$PPRPPU_P1 <- factor(main_table$PPRPPU_P1, levels = c("NS","PRIVE","PUBLIC","INCONNU"))
  main_table$PPRPPU_P2 <- factor(main_table$PPRPPU_P2, levels = c("NS","PRIVE","PUBLIC","INCONNU"))
  
  main_table$PPRPPU2_P1 <- "NON"
  main_table$PPRPPU2_P1[main_table$PPRPPU_P1 %in% c("INCONNU","PRIVE")] <- "Autre secteur"
  main_table$PPRPPU2_P1[main_table$PPRPPU_P1 =="PUBLIC"] <- "Secteur public"
  main_table$PPRPPU2_P1 <- factor(main_table$PPRPPU2_P1, levels = c("NON","Secteur public","Autre secteur"))
  
  # Nouvelles variables synthetiques 
  main_table$TOPGROP1P2 <- "0"
  main_table$TOPGROP1P2[main_table$TOPGRO_P1 =="1" | main_table$TOPGRO_P2 =="1"] <- "1"
  main_table$TOPGROP1P2 <- as.factor(main_table$TOPGROP1P2)
  main_table$TOPGROP1P2 <- factor(main_table$TOPGROP1P2, levels = c("0","1"))
  
  main_table$STATUETU2_P1 <- "0"
  main_table$STATUETU2_P1[main_table$STATUETU_P1 %in% c("APP","ETU_BOUR","ETU_NON" ,"ETU_SAL")] <- "1"
  main_table$STATUETU2_P1 <- as.factor(main_table$STATUETU2_P1)
  main_table$STATUETU2_P1 <- factor(main_table$STATUETU2_P1, levels = c("0","1"))
  
  main_table$STATUETU2_P2 <- "0"
  main_table$STATUETU2_P2[main_table$STATUETU_P2 %in% c("APP","ETU_BOUR","ETU_NON" ,"ETU_SAL")] <- "1"
  main_table$STATUETU2_P2 <- as.factor(main_table$STATUETU2_P2)
  main_table$STATUETU2_P2 <- factor(main_table$STATUETU2_P2, levels = c("0","1"))
  
  # B. Recoder les absents dans les statuts professionnels et ajouter le statut etudiant: Simplification du statut etudiant pour limiter la redondance des variables, trop faibles effectifs on les passe en inconnus
  #####
  main_table$STACT2_RES_P1 <- as.character(main_table$STA_ACT_RES_P1)
  main_table$STACT2_RES_P2 <- as.character(main_table$STA_ACT_RES_P2)
  main_table$STACT2_RES_P1[main_table$STACT2_RES_P1 =="ABS"] <- "INC"
  main_table$STACT2_RES_P2[main_table$STACT2_RES_P2 =="ABS"] <- "INC"
  main_table$STACT2_RES_P1[main_table$STATUETU2_P1 =="1"] <- "ETU"
  main_table$STACT2_RES_P2[main_table$STATUETU2_P2 =="1"] <- "ETU"
  
  main_table$STACT2_CON_P1 <- as.character(main_table$STA_ACT_CON_P1)
  main_table$STACT2_CON_P2 <- as.character(main_table$STA_ACT_CON_P2)
  main_table$STACT2_CON_P1[main_table$STACT2_CON_P1 =="ABS"] <- "INC"
  main_table$STACT2_CON_P2[main_table$STACT2_CON_P2 =="ABS"] <- "INC"
  
  main_table$STACT2_RES_P1 <- as.factor(as.character(main_table$STACT2_RES_P1))
  main_table$STACT2_RES_P2 <- as.factor(as.character(main_table$STACT2_RES_P2))
  main_table$STACT2_CON_P1 <- as.factor(as.character(main_table$STACT2_CON_P1))
  main_table$STACT2_CON_P2 <- as.factor(as.character(main_table$STACT2_CON_P2))
  
  main_table$STACT2_RES_P1 <- factor(main_table$STACT2_RES_P1, levels = c("ACT","CHO","INA","ETU","INC"))
  main_table$STACT2_RES_P2 <- factor(main_table$STACT2_RES_P2, levels = c("ACT","CHO","INA","ETU","INC"))
  main_table$STACT2_CON_P1 <- factor(main_table$STACT2_CON_P1, levels = c("ACT","CHO","INA","INC")) # pas de statut etudiant pour le conjoint
  main_table$STACT2_CON_P2 <- factor(main_table$STACT2_CON_P2, levels = c("ACT","CHO","INA","INC"))# pas de statut etudiant pour le conjoint
  
  main_table$EVOACT2_RES <- as.character(main_table$EVO_ACT_RES)
  main_table$EVOACT2_RES[main_table$STATUETU2_P1 !="1"& main_table$STATUETU2_P2 =="1"] <- "ETU"
  main_table$EVOACT2_RES[main_table$EVOACT2_RES =="ABS"] <- "INC"
  main_table$EVOACT2_RES <- as.factor(main_table$EVOACT2_RES)
  main_table$EVOACT2_RES <- factor(main_table$EVOACT2_RES, levels = c( "STABLE","ACT","CHO","INA","ETU","INC"))
  
  main_table$EVOACT2_CON <- as.character(main_table$EVO_ACT_CON)
  main_table$EVOACT2_CON[main_table$EVOACT2_CON =="ABS"] <- "INC"
  main_table$EVOACT2_CON <- as.factor(main_table$EVOACT2_CON)
  main_table$EVOACT2_CON <-  factor(main_table$EVOACT2_CON, levels = c( "STABLE","ACT","CHO","INA","INC"))
  
  main_table$BAS_REV <- "0"
  main_table$BAS_REV[main_table$REV_CLASSE=="BAS_REV"] <-"1"
  main_table$BAS_REV <- as.factor(main_table$BAS_REV)
  main_table$BAS_REV <- factor(main_table$BAS_REV, levels = c("0", "1"))
  
  # D. Rassembler ALVERS et PPRPPU pour P1 et P2
  #####
  
  #summary(main_table$ALVERS_P1=="OUI"&main_table$PPRPPU2_P1=="INCONNU")
  #levels(main_table$PPRPPU2_P1) # all false => redondance entre les deux variables
  #main_table$PARC_AL_P1 <- "PAS_AL"
  #main_table$PARC_AL_P1[main_table$PPRPPU2_P1=="PUBLIC"] <- "PUBLIC"
  #main_table$PARC_AL_P1[main_table$PPRPPU2_P1=="PRIVE"] <- "PRIVE"
  #main_table$PARC_AL_P1 <- as.factor(main_table$PARC_AL_P1)
  #summary(main_table$PARC_AL_P1)
  #main_table$PARC_AL_P1 <- factor(main_table$PARC_AL_P1, levels = c( "PAS_AL","PUBLIC","PRIVE"))
  
  #main_table$PARC_AL_P2 <- "PAS_AL"
  #main_table$PARC_AL_P2[main_table$PPRPPU2_P2=="PUBLIC"] <- "PUBLIC"
  #main_table$PARC_AL_P2[main_table$PPRPPU2_P2=="PRIVE"] <- "PRIVE"
  #main_table$PARC_AL_P2 <- as.factor(main_table$PARC_AL_P2)
  #summary(main_table$PARC_AL_P2)
  #main_table$PARC_AL_P2 <- factor(main_table$PARC_AL_P2, levels = c( "PAS_AL","PUBLIC","PRIVE"))
  
  return(main_table)
}

MOBCAF <- parsing_relevel_cars(MOBCAF)
#save.image("ALLOC19_6.RData")

### Mise a jour des mobilites en tenant compte du COG 2019 ##########################################################

load("ALLOC19_6.RData")

# Diag
COG18 <- c("77028","77149","78503","78524","91222","91182","78251")
MOBCAF$MOBILE2 <- as.factor(MOBCAF$MOBILE2)
MOBCAF$NUMCOMDO_18 <- as.character(MOBCAF$NUMCOMDO_18)
summary(MOBCAF$CHANG_COM)
summary(MOBCAF$MOBILE2)
summary(MOBCAF$NUMCOMDO_18 %in% COG18 & MOBCAF$MOBILE2=="1")


# Correction MOBILE2 et CHANG_COM

MOBCAF$MOBILE2[MOBCAF$NUMCOMDO_18 %in% COG18 & MOBCAF$LILI4ADR_18 == MOBCAF$LILI4ADR_19] <- "NON"
MOBCAF$CHANG_COM[MOBCAF$NUMCOMDO_18 %in% COG18 & MOBCAF$LILI4ADR_18 == MOBCAF$LILI4ADR_19] <- "0" # Correction des NUMCOMDO 
MOBCAF$CHANG_COM <- as.factor(MOBCAF$CHANG_COM)
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="77028"] <- "77433"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="77149"] <- "77109"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="77399"] <- "77504"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="78251"] <- "78551"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="78503"] <- "78320"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="78524"] <- "78158"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="91222"] <- "91390"
MOBCAF$NUMCOMDO_18[MOBCAF$NUMCOMDO_18=="91182"] <- "91228"

save.image("ALLOC19_7.RData")

# Exports
#########

load("ALLOC19_7.RData")

# Données générales, population couverte et population mobile

########
# TOUS #
########
saveRDS(MOBCAF ,"result/ALLOC_19.rds")
#saveRDS(MOBCAF ,"result/ALLOC_19_rev18.rds")

########
# IDF #
########

library(dplyr)
moversIDFP1_19 <- MOBCAF %>% filter(MOBILE2 =="1" & DEPTCD %in% c("75","77","78","91","92","93","94","95"))
saveRDS(moversIDFP1_19,"moversIDFP1_19.rds")
rm(moversIDFP1_19)

ALLOC_IDF_19_ND_G <- MOBCAF %>% filter(MRS_IDF=="0" & DEPTCD %in% c("75","77","78","91","92","93","94","95") & ND2=="OUI" & !(is.na(idINSPIRE_18)|is.na(idINSPIRE_19)))
saveRDS(ALLOC_IDF_19_ND_G ,"ALLOC_IDF_19_ND_G.rds")
rm(ALLOC_IDF_19_ND_G)

moversIDF_19_ND_G <- MOBCAF %>% filter(MOBILE2 =="1" & MRS_IDF=="0" & DEPTCD %in% c("75","77","78","91","92","93","94","95") & ND2=="OUI" & !(is.na(idINSPIRE_18)|is.na(idINSPIRE_19)))
saveRDS(moversIDF_19_ND_G ,"moversIDF_19_ND_G.rds")
rm(moversIDF_19_ND_G)

#########
# PARIS #
#########
# Tables des personnes residentes ? Paris en periode 1 
ALLOC_PARIS_P1_19 <- MOBCAF[MOBCAF$DEPTCD=="75",]
#ALLOC_PARIS_P2_19 <- MOBCAF[MOBCAF$DEPTPR=="75",]

# Version geolocalisee, noyau dur
ALLOC_PARIS_P1_19_ND_G <- ALLOC_PARIS_P1_19 %>% filter(MRS_IDF=="0" & ND2=="OUI" & !(is.na(idINSPIRE_18)|is.na(idINSPIRE_19)))
#ALLOC_PARIS_P2_19 <- ALLOC_PARIS_P2_19 %>% filter(MRS_IDF=="0" & ND2=="OUI" & !(is.na(idINSPIRE_18)|is.na(idINSPIRE_19)))

saveRDS(ALLOC_PARIS_P1_19_ND_G ,"ALLOC_PARIS_P1_19_ND_G.rds")
rm(ALLOC_PARIS_P1_19_ND_G, ALLOC_PARIS_P1_19)
#saveRDS(ALLOC_PARIS_P2_19 ,"ALLOC_PARIS_P2_19.rds")
#rm(ALLOC_PARIS_P2_19)

