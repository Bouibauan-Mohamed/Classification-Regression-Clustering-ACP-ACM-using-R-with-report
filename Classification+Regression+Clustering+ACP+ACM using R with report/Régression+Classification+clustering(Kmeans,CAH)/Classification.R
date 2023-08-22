data=read.csv("C:/Users/bouib/OneDrive/Bureau/S3 ENSIAS/Analyse de donnee/Data_pour_projet.csv")
#Calcul du nombre d'individus, nombre de lignes
nrow(data)
str(data)
data.actifs <- data[,2:10]
#centrage réduction des données cad divis sur l'ecart type non bisé 

data.actifs_cr<-scale(data.actifs,center=T,scale=T)

#k-means avec les données centrées et réduites
#center = 6 - nombre de groupes demandés
#nstart = 5 - nombre d'essais avec différents individus de départ cad 5 execution de  kemeans avec choix different de centre de class initiaux

groupes.kmeans <- kmeans(data.actifs_cr,centers=6,nstart=5)



#between_SS / total_S c'est le taux d'inertie >50% la plupart d'inertie totale est explique  et l'inertie n'aucupe que le compliment..
print(groupes.kmeans)

#évaluer la proportion d'inertie expliquée l'enertie explique 
N=42#nbr de claas 

inertie.expl <- rep(0,times=N)
for (k in 2:N){
  clus <- kmeans(data.actifs_cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}


#l'inertie expliquée est nulle si k=1 (1 seule classe)  obtient le max dans le nombre max des class 
max(inertie.expl)

#graphique de l'evolution de l'inertie explique en fonction de nombre de class
plot(1:N,inertie.expl,type="b",xlab="Nb. de groupes=class",ylab="% inertie expliquée")


#N est le plus petit entier tel que max(inertie.expl)>0.95
#apres de l'obtention de  nbr de class optimal si signifie que a partir  de cette class l'augmentation d'inertie expliqué est negligable 

# la 2eme critère de choisi nombre de classe c'est cette quotion de variable  doit etre <5  cad 5%
#3 classes
var(inertie.expl[4:N])*(N-4)*100/(var(inertie.expl)*(N-1))
#4 classes
var(inertie.expl[5:N])*(N-5)*100/(var(inertie.expl)*(N-1))
#15 classes
var(inertie.expl[16:N])*(N-16)*100/(var(inertie.expl)*(N-1))
#17 classes
var(inertie.expl[17:N])*(N-17)*100/(var(inertie.expl)*(N-1))
#figure de 16 classes première
plot(1:16,inertie.expl[0:16],type="b",xlab="Nb. de groupes=class",ylab="% inertie expliquée")
#CAH
library(FactoMineR)
#les donées déja  normaliser "cetré et reduit"

Res<-HCPC(as.data.frame(data.actifs_cr),nb.clust=-1)
#ANALYSE DE VARIANCE  QUI EST ETE RETEENU
Res$data.clust
Res$desc.var
Res$desc.ind



##Calcul du taux d'inertie
I<-Res$call
I
#####gain d'inerite
Res$call$'t'$inert.gain



