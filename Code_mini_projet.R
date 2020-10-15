## Ouverture du fichier excel
library(readxl)

file = 'clean_data.xlsx'
plan_semis=read_excel(file, sheet=9)
features=read_excel(file, sheet=7)

## On enlève ce qui est pas utile (le contrôle)
df_features=features[17:nrow(features),]
df_semis=plan_semis[17:nrow(plan_semis),c(1,5,6)]

#On rajoute une colonne banc et zone au df_features

Banc=df_semis$zone%/%10
Zone=df_semis$zone%%10
df_features=cbind(df_features,Banc)
df_features=cbind(df_features,Zone)

#On modifie le type des données
str(df_semis)
str(df_features)


## On supprime les répétitions
a=unique(df_features[,1])
ligne=1
while (ligne <nrow(df_features) ){
  j=ligne
  #print(df_features[ligne,1])
  while((df_features[ligne,1]==df_features[j+1,1]) & ((j+1)<=nrow(df_features))){
    j=j+1
  }
  #print(df_features[j,1])
  if (df_features[ligne,1]==df_features[ligne+1,1]){
    df_features[ligne,4]=sum(df_features[ligne:j,4])/(j+1-ligne)
    df_features[ligne,5]=sum(df_features[ligne:j,5])/(j+1-ligne)
    df_features[ligne,6]=sum(df_features[ligne:j,6])/(j+1-ligne)
    df_features[ligne,7]=sum(df_features[ligne:j,7])/(j+1-ligne)
    df_features[ligne,8]=sum(df_features[ligne:j,8])/(j+1-ligne)
    df_features[ligne,9]=sum(df_features[ligne:j,9])/(j+1-ligne)
    df_features[ligne,10]=sum(df_features[ligne:j,10])/(j+1-ligne)
    df_features[ligne,11]=sum(df_features[ligne:j,11])/(j+1-ligne)
    df_features[ligne,12]=sum(df_features[ligne:j,12])/(j+1-ligne)
    df_features[ligne,13]=sum(df_features[ligne:j,13])/(j+1-ligne)
    print('new boucel')
    for (k in j:(ligne+1)){
      
      print(k)
      df_features=df_features[-k,]
    }
  }
  ligne=ligne+1
}


##features
df_features$Banc = as.factor(df_features$Banc)
df_features$Zone = as.factor(df_features$Zone)

#On calcule les écarts et on les ajoutes dans df_features
Ecart10_20=df_features$T20-df_features$T10
Ecart20_30=df_features$T30-df_features$T20
Ecart30_40=df_features$T40-df_features$T30
Ecart40_50=df_features$T50-df_features$T40
Ecart50_60=df_features$T60-df_features$T50
Ecart60_70=df_features$T70-df_features$T60
Ecart70_80=df_features$T80-df_features$T70

df_features=cbind(df_features,Ecart10_20)
df_features=cbind(df_features,Ecart20_30)
df_features=cbind(df_features,Ecart30_40)
df_features=cbind(df_features,Ecart40_50)
df_features=cbind(df_features,Ecart50_60)
df_features=cbind(df_features,Ecart60_70)
df_features=cbind(df_features,Ecart70_80)

#Données
str(df_features)
summary(df_features)

str(df_semis)
summary(df_semis)

#On regarde via des boxplots si on peut déjà voir des effets des bancs sur la vitesse de germination
banc1=which(df_features$Banc==1)
banc2=which(df_features$Banc==2)
banc3=which(df_features$Banc==3)
banc4=which(df_features$Banc==4)

layout(matrix(c(1:8), nrow=2, ncol=4, byrow=TRUE))
boxplot(df_features$TMG[banc1],df_features$TMG[banc2],df_features$TMG[banc3],df_features$TMG[banc4], main='TMG')
boxplot(df_features$Ecart10_20[banc1],df_features$Ecart10_20[banc2],df_features$Ecart10_20[banc3],df_features$Ecart10_20[banc4],ylim=c(0,60),main='Ecart 10-20')
boxplot(df_features$Ecart20_30[banc1],df_features$Ecart20_30[banc2],df_features$Ecart20_30[banc3],df_features$Ecart20_30[banc4],ylim=c(0,60),main='Ecart 20-30')
boxplot(df_features$Ecart30_40[banc1],df_features$Ecart30_40[banc2],df_features$Ecart30_40[banc3],df_features$Ecart30_40[banc4],ylim=c(0,60),main='Ecart 30-40')
boxplot(df_features$Ecart40_50[banc1],df_features$Ecart40_50[banc2],df_features$Ecart40_50[banc3],df_features$Ecart40_50[banc4],ylim=c(0,60),main='Ecart 40-50')
boxplot(df_features$Ecart50_60[banc1],df_features$Ecart50_60[banc2],df_features$Ecart50_60[banc3],df_features$Ecart50_60[banc4],ylim=c(0,60),main='Ecart 50-60')
boxplot(df_features$Ecart60_70[banc1],df_features$Ecart60_70[banc2],df_features$Ecart60_70[banc3],df_features$Ecart60_70[banc4],ylim=c(0,60),main='Ecart 60-70')
boxplot(df_features$Ecart70_80[banc1],df_features$Ecart70_80[banc2],df_features$Ecart70_80[banc3],df_features$Ecart70_80[banc4],ylim=c(0,60),main='Ecart 70-80')






#On regarde via des boxplots si on peut voir des différences de zone
zone1=which(df_features$Zone==1)
zone2=which(df_features$Zone==2)
zone3=which(df_features$Zone==3)
zone4=which(df_features$Zone==4)



layout(matrix(c(1:8), nrow=2, ncol=4, byrow=TRUE))
boxplot(df_features$TMG[zone1],df_features$TMG[zone2],df_features$TMG[zone3],df_features$TMG[zone4], main='TMG')
boxplot(df_features$Ecart10_20[zone1],df_features$Ecart10_20[zone2],df_features$Ecart10_20[zone3],df_features$Ecart10_20[zone4],ylim=c(0,60),main='Ecart 10-20')
boxplot(df_features$Ecart20_30[zone1],df_features$Ecart20_30[zone2],df_features$Ecart20_30[zone3],df_features$Ecart20_30[zone4],ylim=c(0,60),main='Ecart 20-30')
boxplot(df_features$Ecart30_40[zone1],df_features$Ecart30_40[zone2],df_features$Ecart30_40[zone3],df_features$Ecart30_40[zone4],ylim=c(0,60),main='Ecart 30-40')
boxplot(df_features$Ecart40_50[zone1],df_features$Ecart40_50[zone2],df_features$Ecart40_50[zone3],df_features$Ecart40_50[zone4],ylim=c(0,60),main='Ecart 40-50')
boxplot(df_features$Ecart50_60[zone1],df_features$Ecart50_60[zone2],df_features$Ecart50_60[zone3],df_features$Ecart50_60[zone4],ylim=c(0,60),main='Ecart 50-60')
boxplot(df_features$Ecart60_70[zone1],df_features$Ecart60_70[zone2],df_features$Ecart60_70[zone3],df_features$Ecart60_70[zone4],ylim=c(0,60),main='Ecart 60-70')
boxplot(df_features$Ecart70_80[zone1],df_features$Ecart70_80[zone2],df_features$Ecart70_80[zone3],df_features$Ecart70_80[zone4],ylim=c(0,60),main='Ecart 70-80')
dev.off()

#Réduction de dimension par ACP

library(FactoMineR)
library(factoextra)

Sbanc = df_features$Banc
SZone = df_features$Zone

data = df_features[,c(4,5,14,15,16,17,18,19,20,21,22)]

acp = PCA(data, quali.sup = 3:4)
plot(acp, habillage = 3, label = "none") #en fonction du banc sur lequel se trouve l'individu
plot(acp, habillage = 4, label = "none") #en fonction de la zone sur laquelle se trouve l'individu
#fviz_eig(acp,label=TRUE)
#fviz_contrib(acp,choice = "var",axes = 1:2)
## Moyenner sur les répétitions
## Analyse acp avec axes, cos2, et ellipse de confiance, interprétation

library(umap)
str(as.matrix(df_features[,c(4,5,14,15,16,17,18,19,20,21,22)]))
U=umap(df_features[,c(4,5,16,17,18,19,20,21,22)])
plot(U.layout)
plot(U$layout,col=df_features[,14])
plot(U$layout,col=df_features[,15])
