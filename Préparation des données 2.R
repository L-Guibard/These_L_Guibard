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

# Préparation des données 
load("ALLOCS_1819_ND_G_BAN.RData")
ALLOC$DEPTCD <- as.character(ALLOC$DEPTCD)
colnames(ALLOC)[2] <- "CATBEN_P1"
ALLOC <- ALLOC[!(is.na(ALLOC$X_18)),]
ALLOC <- ALLOC[!(is.na(ALLOC$Y_18)),]

# Ajout des données AL plus précises, NBMCHADR et dépendance
############################################################

# Préparation des données AL
load("ADL8.RData")
ADD_LOG_1218$parc1[ADD_LOG_1218$parc1 ==""] <- "Pas d'AL"
ADD_LOG_1218$parc1 <- as.factor(ADD_LOG_1218$parc1)
ADD_LOG_1218$ID18 <- as.character(ADD_LOG_1218$ID18)
colnames(ADD_LOG_1218) <- c("ID18","perscouvlog","parcAL_P1")

# Préparation des données NBMCHADR
DATAMINING_1218 <- read.csv("DATAMINING_1218.csv", sep=";")
DATAMINING_1218 <- DATAMINING_1218[,c(1,3)]
DATAMINING_1218$ID18 <- as.character(DATAMINING_1218$ID18)
colnames(DATAMINING_1218)[1] <- "NBMCHADR_18"
DATAMINING_1218$NBACHADR_18 <- DATAMINING_1218$NBMCHADR_18/12
summary(DATAMINING_1218$NBACHADR_18)
DATAMINING_1218$DUROCC <- "INC"
DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 <= 1] <-"Dinf1"
DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 > 1 & DATAMINING_1218$NBACHADR_18 <= 3] <-"D1_3"
DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 > 3 & DATAMINING_1218$NBACHADR_18 <= 10] <-"D3_10"
DATAMINING_1218$DUROCC[DATAMINING_1218$NBACHADR_18 >  10] <-"Dsup10"
DATAMINING_1218$DUROCC <- as.factor(DATAMINING_1218$DUROCC)
summary(DATAMINING_1218$DUROCC)

# Enfants de moins de 20 ans à charge
load("AE8.RData")
ADD_ENF_1218$ID18 <- as.character(ADD_ENF_1218$ID18)

# Jointures intermédiaires
tab <- left_join(ADD_LOG_1218, DATAMINING_1218)
tab <- left_join(tab, ADD_ENF_1218)
colnames(tab) <- c("ID18","Perscouvlog_P1","ParcAL_P1","NBMCHADR_P1","NBACHADR_P1","DUROCC_P1","NBENFCHA_P1")

# Jointure finale 
ALLOC <- left_join(ALLOC, tab)
rm(tab, DATAMINING_1218, ADD_LOG_1218,ADD_ENF_1218)
save.image("ALLOCS_1819_ND_G_BAN_2.RData")
