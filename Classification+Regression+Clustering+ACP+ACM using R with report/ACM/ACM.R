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
########## ACM ###########
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
# Transformer les variables quantitative au variables qualitative 
data_tran <- discretizeDF(data,default=list(method = "cluster",breaks = 2,labels=c("0","1")))
# Transformer num var to factor 
data_tran$turda <- as.factor(as.numeric(data_tran$turda))  
data_tran$X2_house_age <- as.factor(as.numeric(data_tran$X2_house_age))
data_tran$X3_distance_to_the_nearest_MRT_station <- as.factor(as.numeric(data_tran$X3_distance_to_the_nearest_MRT_station))
data_tran$X4_number_of_convenience_stores <- as.factor(as.numeric(data_tran$X4_number_of_convenience_stores)) 
data_tran$X5_latitude <- as.factor(as.numeric(data_tran$X5_latitude))
data_tran$X6_longitude <- as.factor(as.numeric(data_tran$X6_longitude)) 
data_tran$dephm <- as.factor(as.numeric(data_tran$dephm)) 
data_tran$SOLID <- as.factor(as.numeric(data_tran$SOLID)) 
data_tran$Condi <- as.factor(as.numeric(data_tran$Condi)) 
data_tran$Y_house_price_of_unit_area <- as.factor(as.numeric(data_tran$Y_house_price_of_unit_area))
# construit le tableau disjonctif 
k <- tab.disjonctif(data_tran)
# frequence de chaque modalité 
apply(k,2,sum)
# calcul des effectifs de chaque modalité 
propmod=apply(k,2,sum)/(nrow(k))
# supprimer la variable Y_house_price 
H <- subset(data_tran,select=-Y_house_price_of_unit_area)
# appliquer ACM 
res<-MCA(H,ncp=8,graph=FALSE,axes=c(2.3))
# calcul les valeurs propres 
res$eig
# graphique des valeurs propres 
plot(res$eig[,1],type="b",main="Scree plot") 
# dimension de sous espace
var(res$eig[4:9,1])*5/(var(res$eig[,1])*9)
var(res$eig[5:9,1])*4/(var(res$eig[,1])*9) 
var(res$eig[6:9,1])*3/(var(res$eig[,1])*9) 
# Calcul cos^2 des modalités 
res$var$cos2 
# Calcul la contribution des modalités 
res$var$contrib 
# tracer la contribution des modalités 
fviz_contrib(res,choice="var",axes=1:2,top=36) 
fviz_contrib(res,choice="var",axes=5:6,top=36) 
# Appliquer CAH au modalité 
HCPC(res$var$contrib) 
# Nuage des modalités 
plot(res,choix="var") 
# cos^2 des individus 
res$ind$cos2
# contribution des induvidus 
res$ind$contrib 
# tracer le graphe des contributions des individus 
fviz_contrib(res,choice="var",axes=1:2,top=36) 
fviz_contrib(res,choice="ind",axes=1:2,top=36) 
# Applique CAH au contribution des individus 
HCPC(res$ind$contrib) 
# Calcul des coefficient de corrélation des modalité 
res$var$eta2 
