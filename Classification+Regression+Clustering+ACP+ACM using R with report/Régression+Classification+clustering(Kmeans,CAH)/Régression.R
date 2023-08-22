data=read.csv("C:/Users/bouib/OneDrive/Bureau/S3 ENSIAS/Analyse de donnee/Data_pour_projet.csv")
View(data)

View(data)
pairs(data)
names(data)
#model de regression lineaire avec tout les variable explicatives
modele <- lm(Y.house.price.of.unit.area~.,data=data[,2:11])
modele$coefficients

summary(modele)
#R et ajusté + test de fisher
summary(modele)$coefficients

step(modele)

#le modele choisie par step
model_par_step<-lm(Y.house.price.of.unit.area ~ X2.house.age + X3.distance.to.the.nearest.MRT.station + 
     X4.number.of.convenience.stores + X5.latitude, data = data)
summary(model_par_step)
#les test de validation de ce Model
AIC(model_par_step)
# 1 test d'himosadicite
plot(predict(model_par_step),resid(model_par_step))
abline(h=0)


# 2 Test de Normalité
#par Ks
ks.test(resid(model_par_step), pnorm)
#donc le test  ks  rejete la normalité


#par shapiro:
shapiro.test(resid(model_par_step))
#donc shapiro acccepte la normalité de résudu


#Valeurs aberrantes : approximation du residu standardis? < 2
sd=sqrt(deviance(
  model_par_step)/df.residual(
    model_par_step))
nbrow = 70
seqx=seq(1,nbrow,length=nbrow) #indices de ligne de chaque valeur
abr=abs(data$Y.house.price.of.unit.area-predict(
  model_par_step))/sd
plot(seqx,abr, xlab="sequence", ylab="valeurs aberrantes")
abline(h=2, lty=2,col=2)



###procesure de selection de ficher pas a pas
#Etape 1

nva<-ncol(data)
Fish = rep(0,nva-2)


for (i in  2:10 ) {
  mod1<-lm(data[,11]~data[,i])
  Fish[i]=var(predict(mod1))*(nrow(data)-1)/(deviance(mod1)/df.residual(mod1))
}
names(Fish)
Fish
df2=nrow(data)-2
df2
1-pf(max(Fish),1,df2)
#le variable 4 cest lui qui a le plus grand F et 3ando  p_value <10% donc va entrer
### Introduction de la variable X3.distance.to.the.nearest.MRT.station

#R_ajusteet et  F de ce model 
mod1<-lm(data[,11]~data[,4])
summary(mod1)
### Introduction de la variable X3.distance.to.the.nearest.MRT.station

nva<-ncol(data)
Fish = rep(0,nva-3)


SCR1<-deviance(lm(data[,11]~data[,4]))
for (i in c(2,3,5,6,7,8,9,10)) {
  mod<-lm(data[,11]~data[,4]+data[,i])
  SCR2=deviance(mod)
  Fish[i]=(SCR1-SCR2)/(SCR2/(nrow(data)-3))
}
Fish
df2=nrow(data)-3
df2
1-pf(max(Fish),1,df2)
#R_ajusteet et  F de ce model 
mod<-lm(data[,11]~data[,4]+data[,3])
summary(mod)

#introduction de variable 3 = x2.house.age car il a  le plus grand F et  p_value <10% donc va entrer
#introduction de variable X2.house


#existe il des variable  a sortir ? test de retrait


Fish = rep(0,2)
SCR2=deviance(lm(data[,11]~data[,4]+data[,3]))
mod<-lm(data[,11]~data[,4])
SCR1<-deviance(mod)
Fish[1]=(SCR1-SCR2)/(SCR2/(nrow(data)-3))

mod<-lm(data[,11]~data[,3])
SCR1=deviance(mod)
Fish[2]=(SCR1-SCR2)/(SCR2/(nrow(data)-3))
Fish
df2=nrow(data)-3
df2
1-pf(min(Fish),1,df2)


## Aucune variable n'est retitée, les F sont significatifs 

nva=ncol(data)
Fish = rep(0,nva-4)
SCR2<-deviance(lm(data[,11]~data[,4]+data[,3]))
for (i in c(2,5,6,7,8,9,10)) {
  mod<-lm(data[,11]~data[,4]+data[,3]+data[,i])
  SCR3=deviance(mod)
  Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))
}
Fish
df2=nrow(data)-4
df2
1-pf(max(Fish),1,df2)



#le variable 6 va entrer
#calcule de r_ajuste de ce model 
mod<-lm(data[,11]~data[,4]+data[,3]+data[,6])
summary(mod)

#maintenant test de retirait

SCR3<-deviance(lm(data[,11]~data[,6]+data[,3]+data[,4]))
########
Fish<-rep(0,3)
mod<-lm(data[,11]~data[,6]+data[,3])
SCR2<-deviance(mod)
Fish[1]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))

mod<-lm(data[,11]~data[,6]+data[,4])
SCR2<-deviance(mod)
Fish[2]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))

mod<-lm(data[,11]~data[,3]+data[,4])
SCR2<-deviance(mod)
Fish[3]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))

Fish
df2=nrow(data)-4
df2
1-pf(min(Fish),1,df2)

## Aucune variable n'est retitée, les F sont significatifs 


##### Etape 4 -Introduction-
Fish<-rep(0,6)
SCR2<-deviance(lm(data[,11]~data[,6]+data[,3]+data[,4]))

for (i in c(2,5,7,8,9,10)) {
  mod<-lm(data[,11]~data[,6]+data[,3]+data[,4]+data[,i])
  SCR3=deviance(mod)
  Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data)-5))
}
Fish
df2=nrow(data)-5
df2
1-pf(max(Fish),1,df2)


#le variable 5 va entrer

#calcule de r_ajuste de ce model 
mod<-lm(data[,11]~data[,4]+data[,3]+data[,6]+data[,5])
summary(mod)
#test de retirer

Fish = rep(0,4)
SCR4<-deviance(lm(data[,11]~data[,5]+data[,3]+data[,4]+data[,6]))
mod<-lm(data[,11]~data[,5]+data[,3]+data[,4])
SCR3<-deviance(mod)
Fish[1]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

mod<-lm(data[,11]~data[,5]+data[,3]+data[,6])
SCR3<-deviance(mod)
Fish[2]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

mod<-lm(data[,11]~data[,5]+data[,4]+data[,6])
SCR3<-deviance(mod)
Fish[3]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

mod<-lm(data[,11]~data[,3]+data[,4]+data[,6])
SCR3<-deviance(mod)
Fish[4]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

Fish
df2=nrow(data)-5
df2
1-pf(min(Fish),1,df2)

#Aucune variable n'est retitée, les F sont significatifs 

#existe il variable a entrer?
Fish<-rep(0,6)
SCR2<-deviance(lm(data[,11]~data[,5]+data[,3]+data[,4]+data[,6]))

for (i in c(7,8,9,10)) {
  mod<-lm(data[,11]~data[,5]+data[,3]+data[,4]+data[,6]+data[,i])
  SCR3=deviance(mod)
  Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data)-6))
}
Fish
df2=nrow(data)-6
df2
1-pf(max(Fish),1,df2)
#Aucun variable a enter p_value>10%
#stop
#test d'arrète est  verifier aucun variable a enter ni a sortie


#le modele choisie par fischer
model_par_fischer<-mod<-lm(data[,11]~data[,3]+data[,4]+data[,5]+data[,6])
summary(model_par_fischer)
#les test de validation de ce Model

# 1 test d'himosadicite
plot(predict(model_par_fischer),resid(model_par_fischer))
abline(h=0)

# 2 Test de Normalité
#par Ks
ks.test(resid(model_par_fischer), pnorm)
#donc le test  ks  rejete la normalité

#par shapiro:
shapiro.test(resid(model_par_fischer))
#donc shapiro acccepte la normalité de résudu

# calculer AIc 

AIC(model_par_fischer)







