# Géocodage complémentaire BAN (serveur local - Docker)

library(httr)
library(jsonlite)

# géolocalisation des non noyaux dur 2019 mobiles mais restés IDF 
load("ALLOCS_1819_ND_G_BAN_2.RData")

idf <- c("75","77","78","91","92","93","94","95")
adr <- ALLOC_IDF_ND_G_19[ALLOC_IDF_ND_G_19$MOBILE2 =="1" & is.na(ALLOC_IDF_ND_G_19$X_19), c("ID19","LILI4ADR_19","NUMCOMDO_19") ]

# départs à l'étranger 
etranger <- adr[adr$NUMCOMDO_22 %in% c("99999","XXXXX"),]
outremer <- adr[substr(adr$NUMCOMDO_22,1,2) %in% c("97","98"),]
adr <- adr[!(adr$NUMCOMDO_22 %in% c("99999","XXXXX")),]
adr <- adr[!(substr(adr$NUMCOMDO_22,1,2) %in% c("97","98")),]

# préparation des données
adr$request <- paste(adr$LILI4ADR_22, adr$NUMCOMDO_22, sep=" ")
adr$request <- gsub(" ","+", adr$request)
adr$BAN_result <- "NA"
adr$BAN_X <- "NA"
adr$BAN_Y <- "NA"

# Boucle de geocodage
addok <- "http://localhost:7878/search?q=" # Le docker BAN doit etre lance
for(j in seq(1,nrow(adr), 1000)){
  print(j)
  k <- j+999
  for(i in j:k){
    res = GET(paste0(addok, adr$request[i]))
    if(length(content(res)$features) >0){
      adr$BAN_result[i] <- content(res)$features[[1]]$properties$label
      adr$BAN_X[i] <- content(res)$features[[1]]$geometry$coordinates[[1]]
      adr$BAN_Y[i] <- content(res)$features[[1]]$geometry$coordinates[[2]]
    }
  }
}

saveRDS(adr,"adr_BanGeocoded_19.rds" )