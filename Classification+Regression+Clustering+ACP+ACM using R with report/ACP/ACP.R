library(FactoMineR)
library(ggplot2)
library(factoextra)
library(psych)
library("readxl")
library("corrplot")
install.packages("FactoMineR")
library("FactoMineR")
library("arules")

# Set working directory 
setwd("~/Projet") 
########## ACP normé ###########
# read Data 
data <- read.csv2("Data_pour_projet.csv",header=TRUE,sep=",",quote = "\"")
# convert char var to num var  
data$turda <- as.numeric(as.character(data$turda)) 
data$X2_house_age <- as.numeric(as.character(data$X2_house_age)) 
data$X3_distance_to_the_nearest_MRT_station <- as.numeric(as.character(data$X3_distance_to_the_nearest_MRT_station))
data$X5_latitude <- as.numeric(as.character(data$X5_latitude)) 
data$X6_longitude <- as.numeric(as.character(data$X6_longitude)) 
data$dephm <- as.numeric(as.character(data$dephm))
data$SOLID <- as.numeric(as.character(data$SOLID))
data$Condi <- as.numeric(as.character(data$Condi)) 
data$Y_house_price_of_unit_area <- as.numeric(as.character(data$Y_house_price_of_unit_area))
# select var active 
data.actifs <- data[,1:9] 
# appliquer ACP  
res <- PCA(data.actifs,scale.unit = TRUE,ncp = 5) 
# calcul indice KMO 
KMO(cor(data.actifs)) 
# calculer les valeurs propres 
res$eig  
# graphe des valeurs propres 
plot(1:9,res$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres") 
# dimension de sous espace 
var(res$eig[2:9,1])*700/(var(res$eig[,1])*8)
var(res$eig[4:9,1])*500/(var(res$eig[,1])*8)
var(res$eig[6:9,1])*300/(var(res$eig[,1])*8)
var(res$eig[7:9,1])*200/(var(res$eig[,1])*8)
var(res$eig[8:9,1])*100/(var(res$eig[,1])*8)
var(res$eig[5:9,1])*400/(var(res$eig[,1])*8) 
########### Nuage des variables ############
# Calcul de cos^2 
res$var$cos2
# calcul des contributions des variables 
res$var$contrib 
# tracer le graphe visualisant laes contributions des variables 
corrplot(res$var$contrib, is.corr=FALSE) 
# Appliquer la CAH au contribution des variables 
HCPC(res$var$contrib) 
# nuage des variables projeté sur les deux axes 
res <- PCA(meteo,ncp=5,axes=c(1,2)) 
############### Nuage des individus ##################
# Calcul cos^2 
res$ind$cos2
# Calcul des contributions 
res$ind$contrib 
# tracer la contribution des individus 
fviz_contrib(res, choice= "ind", axes = 1, top = 18)
# Appliquer CAH au contribution des individus 
HCPC(res$ind$contrib)
# nuage des individus 
plot.PCA(res,axes=c(1,2),choix="ind",habillage = 18) 