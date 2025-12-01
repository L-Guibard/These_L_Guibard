# Figure effets revenus-marchés

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


# MEP résultats ----
load("chap2_modeles_VF3.RData")
load("modeles_parcs.RData")
pla_typo <- RColorBrewer::brewer.pal(6, "Set3")[c(5,4,6,1,3)]
pal_models <- c("#D9D9D9", "#80B1D3","#bdc9e1") 
p_seuil <- 0.05

colnames(Modeles_DEM_parc) <- c("D_Parc_1","D_P1","D_Parc_2","D_P2","D_Parc_3","D_P3","D_Parc_4","D_P4","D_Parc_5","D_P5")
Modeles_DEM_parc$Facteurs <- rownames(Modeles_DEM_parc)
colnames(Modeles_CHANGC_parc) <- c("C_Parc_1","C_P1","C_Parc_2","C_P2","C_Parc_3","C_P3","C_Parc_4","C_P4","C_Parc_5","C_P5")
Modeles_CHANGC_parc$Facteurs <- rownames(Modeles_CHANGC_parc)
colnames(Modeles_CHANGC_parc_B) <- c("CD_Parc_1","CD_P1","CD_Parc_2","CD_P2","CD_Parc_3","CD_P3","CD_Parc_4","CD_P4","CD_Parc_5","CD_P5")
Modeles_CHANGC_parc_B$Facteurs <- rownames(Modeles_CHANGC_parc_B)
Modeles_parcs <- Modeles_DEM_parc %>% left_join(Modeles_CHANGC_parc) %>% left_join(Modeles_CHANGC_parc_B)

# préparation graphique
modeles <- c("M4.Déménager","M5.Changer de commune", "M6.Changer de commune (mobiles)")
regressions1 <- Modeles_parcs[19:20,c(1,12,22)] # Parc 1
regressions2 <- Modeles_parcs[19:20,c(3,14,24)] # Parc 2
regressions3 <- Modeles_parcs[19:20,c(5,16,26)] # Parc 3
regressions4 <- Modeles_parcs[19:20,c(7,18,28)] # Parc 4
regressions5 <- Modeles_parcs[19:20,c(9,20,30)] # Parc 5

for(k in 1:5){
  if(k==1){tab <- regressions1}
  if(k==2){tab <- regressions2}
  if(k==3){tab <- regressions3}
  if(k==4){tab <- regressions4}
  if(k==5){tab <- regressions5}
  for(j in 1:ncol(tab)){
    for(i in 1:nrow(tab)){
      if(tab[i,j] < 1){
        tab[i,j] <- (1/tab[i,j]*-1)+1
      }
      if(tab[i,j] > 1){
        tab[i,j] <- tab[i,j] - 1
      }
    }
  }
  if(k==1){regressions1 <- tab}
  if(k==2){regressions2 <- tab}
  if(k==3){regressions3 <- tab}
  if(k==4){regressions4 <- tab}
  if(k==5){regressions5 <- tab}
}

# Retirer les facteurs NS: 
# regressions1$D_Parc_1[Modeles_parcs$D_P1[19:20] > p_seuil] <- NA
# regressions1$C_Parc_1[Modeles_parcs$C_P1[19:20] > p_seuil] <- NA
# regressions1$CD_Parc_1[Modeles_parcs$CD_P1[19:20] > p_seuil] <- NA
# regressions2$D_Parc_2[Modeles_parcs$D_P2[19:20] > p_seuil] <- NA
# regressions2$C_Parc_2[Modeles_parcs$C_P2[19:20] > p_seuil] <- NA
# regressions2$CD_Parc_2[Modeles_parcs$CD_P2[19:20] > p_seuil] <- NA
# regressions3$D_Parc_3[Modeles_parcs$D_P3[19:20] > p_seuil] <- NA
# regressions3$C_Parc_3[Modeles_parcs$C_P3[19:20] > p_seuil] <- NA
# regressions3$CD_Parc_3[Modeles_parcs$CD_P3[19:20] > p_seuil] <- NA
# regressions4$D_Parc_4[Modeles_parcs$D_P4[19:20] > p_seuil] <- NA
# regressions4$C_Parc_4[Modeles_parcs$C_P4[19:20] > p_seuil] <- NA
# regressions4$CD_Parc_4[Modeles_parcs$CD_P4[19:20] > p_seuil] <- NA
# regressions5$D_Parc_5[Modeles_parcs$D_P5[19:20] > p_seuil] <- NA
# regressions5$C_Parc_5[Modeles_parcs$C_P5[19:20] > p_seuil] <- NA
# regressions5$CD_Parc_5[Modeles_parcs$CD_P5[19:20] > p_seuil] <- NA

# Mise en forme pour le graphique

for(k in 1:5){
  if(k==1){tab <- regressions1}
  if(k==2){tab <- regressions2}
  if(k==3){tab <- regressions3}
  if(k==4){tab <- regressions4}
  if(k==5){tab <- regressions5}
  
  colnames(tab) <- modeles
  tab$Facteurs <- c("Bas revenus","Revenus modérés")
  reg1 <- tab[,c(ncol(tab),1)]
  reg1$Model <- colnames(reg1)[2]
  reg1 <-reg1[,c(1,3,2)]
  colnames(reg1) <- c("Facteurs","Modèle","Coefficient")
  reg2 <- tab[,c(ncol(tab),2)]
  reg2$Model <- colnames(reg2)[2]
  reg2 <-reg2[,c(1,3,2)]
  colnames(reg2) <- c("Facteurs","Modèle","Coefficient")
  reg3 <- tab[,c(ncol(tab),3)]
  reg3$Model <- colnames(reg3)[2]
  reg3 <-reg3[,c(1,3,2)]
  colnames(reg3) <- c("Facteurs","Modèle","Coefficient")
  tab <- rbind(reg1, reg2, reg3)
  # Grouped
  tab$Facteurs <- as.factor(tab$Facteurs)
  tab$Facteurs <- factor(tab$Facteurs, levels = rev(c("Bas revenus","Revenus modérés")))
  tab$Modèle <- as.factor(tab$Modèle)
  tab$Modèle <- factor(tab$Modèle, levels = rev(modeles))
  graph_revParc <- ggplot(tab, aes(fill=Modèle, y=Coefficient, x=Facteurs)) + 
    geom_bar(position="dodge", stat="identity",color=NA) +
    coord_flip()+
    theme_minimal()+
    theme(legend.position="bottom", 
          legend.text = element_text(size = 9),
          legend.key.size = unit(0.5, 'cm'),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7.5)
    )+
    scale_fill_manual(values=pal_models)+
    guides(fill = guide_legend(reverse=TRUE, nrow=1))+
    labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
    scale_y_continuous(breaks =c(-0.5,-0.25,-0.10,-0.05,0,0.05,0.1,0.25,0.5), 
                       labels=c("1,5 fois moins","1,25","1,1","","Autant","","1,1","1,25","1,5 fois plus"), 
                       limits=c(-0.5,0.5)) 
  
  if(k==1){
    regressions1 <- tab
    graphParc1 <- graph_revParc
    }
  if(k==2){
    regressions2 <- tab
    graphParc2 <- graph_revParc
    }
  if(k==3){
    regressions3 <- tab
    graphParc3 <- graph_revParc
    }
  if(k==4){
    regressions4 <- tab
    graphParc4 <- graph_revParc
    }
  if(k==5){
    regressions5 <- tab
    graphParc5 <- graph_revParc
    }
}

graphParc1
graphParc2
graphParc3
graphParc4
graphParc5

# Agencement de la figure finale

# essai avec facet wrap 

regressions1$Marché <- "a.Marché 1"
regressions2$Marché <- "b.Marché 2"
regressions3$Marché <- "c.Marché 3"
regressions4$Marché <- "d.Marché 4"
regressions5$Marché <- "e.Marché 5"
regressions <- rbind(regressions1,regressions2,regressions3,regressions4,regressions5)


graph_Parc <- ggplot(regressions) + 
  geom_bar(aes(fill=Modèle, y=Coefficient, x=Facteurs),position="dodge", stat="identity",color=NA) +
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom", 
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.caption = element_text(color = "#4C4C4C", face = "italic", size=7.5)
  )+
  scale_fill_manual(values=pal_models)+
  guides(fill = guide_legend(reverse=TRUE, nrow=1))+
  labs(fill=element_blank() ,y = element_blank(), x = element_blank()) +
  scale_y_continuous(breaks =c(-0.5,-0.25,-0.10,-0.05,0,0.05,0.1,0.25,0.5), 
                     labels=c("1,5x moins","1,25","1,1","","Autant","","1,1","1,25","1,5x plus"), 
                     limits=c(-0.5,0.5))+
  facet_wrap(~ Marché, nrow = 3)

graph_Parc

# export 
pageZ <- readRDS("pageZ.rds")

pageZ(format = "custom",w_cust = 6.7,h_cust =7 , output = "svg", name = "graph_Parc")
graph_Parc
dev.off()

