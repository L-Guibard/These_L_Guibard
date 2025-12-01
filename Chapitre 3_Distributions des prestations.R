library(dplyr)
library(ggplot2)

load("ALLOCS_1819_ND_G_BAN_2.RData")


# Action sociale dans les BBD : PRESFRES = 

load("PV18.RData")
load("PDEM_PHAB_18.RData")
load("ACDMI_18.RData")
IDS <- ALLOC[,c("ID18","NUMCOMDO_18","NUMCAF_18", "PERSCOUV_P1")]
colnames(IDS) <- c("ID18","NUMCOMDO","NUMCAF", "PERSCOUV")
summary(duplicated(IDS$ID18))
presvers_1218 <- presvers_1218[,-c(2,22)] 
presvers_1218$ID18 <- as.character(presvers_1218$ID18)
presvers <- left_join(IDS, presvers_1218)

# Vérifications
summary(as.factor(presvers$p_APL)) # verif qualité de la jointure
summary(as.factor(presvers$p_ALF)) # verif qualité de la jointure
summary(as.factor(presvers$p_ALS)) # verif qualité de la jointure
1062699/nrow(presvers)

PDEM_PHAB_18$ID18 <- as.character(PDEM_PHAB_18$ID18)
presvers <- left_join(presvers, PDEM_PHAB_18)
summary(is.na(presvers$p_PHAB))
AMIVERS_1218$ID18 <- as.character(AMIVERS_1218$ID18)
presvers <- left_join(presvers, AMIVERS_1218)
summary(is.na(presvers$p_ami))
rm(ALLOC, ALLOC_IDF_G, IDS, presvers_1218, PDEM_PHAB_18,AMIVERS_1218, sel, save, temp)

# Recoupement des prestations
presvers$AF <- ""
presvers$AF[presvers$p_af==1 | presvers$p_cf==1] <- " AF"
presvers$ASF <- ""
presvers$ASF[presvers$p_asf==1] <- " ASF"
presvers$AEEH<- ""
presvers$AEEH[presvers$p_aeeh==1] <- " AEEH"
presvers$AJPP <- ""
presvers$AJPP[presvers$p_ajpp==1] <- " AJPP"
presvers$AAH <- ""
presvers$AAH[presvers$p_aah==1 | presvers$p_complemAAH==1] <- " AAH"
presvers$PAJE <- ""
presvers$PAJE[presvers$p_paje==1 | presvers$p_cmg==1 | presvers$p_prepa==1 | presvers$p_ClcaColca==1 | presvers$p_primenais==1 | presvers$p_abpaje==1] <- " PAJE"
presvers$PPA <- ""
presvers$PPA[presvers$p_ppa==1] <- " PPA"
presvers$RSA <- ""
presvers$RSA[presvers$p_rsa==1] <- " RSA"
presvers$APL <- ""
presvers$APL[presvers$p_APL==1] <- " APL"
presvers$ALS <- ""
presvers$ALS[presvers$p_ALS==1] <- " ALS"
presvers$ALF <- ""
presvers$ALF[presvers$p_ALF==1] <- " ALF"
presvers$ARS <- ""
presvers$ARS[presvers$p_ars==1] <- " ARS"

#presvers$PDEP <- ""
#presvers$PDEP[presvers$p_PDEP==1] <- " PDEP"
presvers$PHAB <- ""
presvers$PHAB[presvers$p_PHAB==1] <- " PHAB"
presvers$AMI <- ""
presvers$AMI[presvers$p_ami==1] <- " AMI"
presvers$ADI <- ""
presvers$ADI[presvers$p_adi==1] <- " ADI"
presvers$CDI <- ""
presvers$CDI[presvers$p_cdi==1] <- " CDI"

presvers$presta <- paste0(presvers$AMI,presvers$ADI,presvers$CDI,presvers$ARS,presvers$AJPP,presvers$AF,presvers$ASF,presvers$AEEH,presvers$APL,presvers$ALS,presvers$ALF,presvers$RSA,presvers$AAH, presvers$PPA, presvers$PAJE)             
#presvers$presta[presvers$presta=="" & presvers$p_ars==1] <- "ARS seule"
presvers$presta[presvers$presta==""] <- "Autre"

# Concaténation des prestations perçues

nrow(presvers[presvers$p_APL==1|presvers$p_ALF==1|presvers$p_ALS==1,])/nrow(presvers)
recoup_pres <- as.data.frame(plyr::count(presvers$presta))
colnames(recoup_pres)[1] <- "Prestations"
recoup_pres$Taux <- round(recoup_pres$freq / 2277791 *100,2)
recoup_pres_light <- recoup_pres[recoup_pres$Taux > 0.5,]

sum(recoup_pres$freq) - sum(recoup_pres_light$freq)
# selection des prestations représentant plus de 0,5% des foyers

# Selon le type 
cond_fam <- c(presvers$p_cf==1 | presvers$p_abpaje==1 | presvers$p_paje==1 |
                presvers$p_primenais==1 | presvers$p_prepa==1 | 
                presvers$p_cmg==1 | presvers$p_ClcaColca==1 | 
                presvers$p_af==1 | presvers$p_asf==1 |
                presvers$p_aeeh==1 | presvers$p_ajpp==1 | presvers$p_ars==1)
cond_log <- c(presvers$p_ALS==1 | presvers$p_APL==1 | 
                presvers$p_ALF==1)
cond_min <- c(presvers$p_aah==1 | presvers$p_complemAAH==1 | presvers$p_ppa==1 | presvers$p_rsa==1)
cond_decale <- c(presvers$p_ami==1 | presvers$p_adi==1 | presvers$p_cdi==1)
presvers$type <- "Autre"
presvers$type[cond_fam & !cond_log & !cond_min] <- "Prestations familiales"
presvers$type[!cond_fam & cond_log & !cond_min] <- "Prestations logement"
presvers$type[!cond_fam & !cond_log & cond_min] <- "Minima sociaux"
presvers$type[cond_fam & cond_log & !cond_min] <- "Prestations familiales et logement"
presvers$type[cond_fam & !cond_log & cond_min] <- "Prestations familiales et minima sociaux"
presvers$type[!cond_fam & cond_log & cond_min] <- "Prestations logement et minima sociaux"
presvers$type[cond_fam & cond_log & cond_min] <- "Prestations familiales, logement et minima sociaux"
presvers$type[presvers$type=="Autre" & cond_decale] <- "Autres decalés"
presvers$type <- as.factor(presvers$type)
types_pres <- as.data.frame(summary(presvers$type))

# Selon les conditions de ressources
sous_cond <- c(presvers$p_aah==1 | presvers$p_complemAAH==1 | presvers$p_ppa==1 | presvers$p_rsa==1 | 
                 presvers$p_ALS==1 | presvers$p_APL==1 | 
                 presvers$p_ALF==1 | presvers$p_cf==1 | presvers$p_abpaje==1 |
                presvers$p_primenais==1 |  
                presvers$p_ars==1)
sans_cond<- c(presvers$p_af==1 | presvers$p_asf==1 |
                presvers$p_aeeh==1 | presvers$p_ajpp==1 |presvers$p_prepa==1 | 
                presvers$p_cmg==1 | presvers$p_ClcaColca==1)

presvers$Condition <- "Autre"
presvers$Condition[sous_cond & !sans_cond] <- "Sous condition"
presvers$Condition[!sous_cond & sans_cond] <- "Sans condition"
presvers$Condition[sous_cond & sans_cond] <- "Sans condition et sous condition"
presvers$Condition[presvers$Condition=="Autre" & cond_decale] <- "Autres decalés"
presvers$Condition <- as.factor(presvers$Condition)
cond <- as.data.frame(summary(presvers$Condition))

# Dénombrements
for(i in 4:27){
  print(colnames(presvers[i]))
  print(sum(as.numeric(presvers[,i])))
}

rm(ALLOC)


