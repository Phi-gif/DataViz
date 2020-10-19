## Ouverture du fichier excel
library(readxl)

file = 'clean_data.xlsx'
plan_semis=read_excel(file, sheet=9)
features=read_excel(file, sheet=7)

## On enlève ce qui est pas utile
###controle
df_features=features[17:nrow(features),]
df_semis=plan_semis[17:nrow(plan_semis),c(1,3,5)]

#banc 2
Banc2 = which(df_semis$Banc == 2)
df_semis_mod = df_semis[Banc2,]
df_f = df_features[Banc2,]
n = dim(df_semis_mod)[1]
Zone = rep(0,n)

for (i in 1:n){
  if (df_semis_mod$zone[i] == 11 |df_semis_mod$zone[i] == 12 |df_semis_mod$zone[i] == 21 |df_semis_mod$zone[i] == 22){
    Zone[i] = 1
  }
  if (df_semis_mod$zone[i] == 13 |df_semis_mod$zone[i] == 14 |df_semis_mod$zone[i] == 23 |df_semis_mod$zone[i] == 24){
    Zone[i] = 2
  }
  if (df_semis_mod$zone[i] == 31 |df_semis_mod$zone[i] == 32 |df_semis_mod$zone[i] == 41 |df_semis_mod$zone[i] == 42){
    Zone[i] = 3
  }
  if (df_semis_mod$zone[i] == 33 |df_semis_mod$zone[i] == 34 |df_semis_mod$zone[i] == 43 |df_semis_mod$zone[i] == 44){
    Zone[i] = 4
  }
}

#On rajoute la colonne zone au df_features pour tout avoir sur un seul df

df_f=cbind(df_f,Zone)


#On modifie le type des données
str(df_f)
df_f$Zone = as.factor(df_f$Zone)

#affichage des répartitions des données en fonction des bancs, camera et zones
summary(df_f$Zone)       # répartition équitbale en fonction des zones


#On calcule les écarts et on les ajoutes dans df_features (cinétiques)
Ecart10_20=df_f$T20-df_f$T10
Ecart20_30=df_f$T30-df_f$T20
Ecart30_40=df_f$T40-df_f$T30
Ecart40_50=df_f$T50-df_f$T40
Ecart50_60=df_f$T60-df_f$T50
Ecart60_70=df_f$T70-df_f$T60
Ecart70_80=df_f$T80-df_f$T70

df_f=cbind(df_f,Ecart10_20)
df_f=cbind(df_f,Ecart20_30)
df_f=cbind(df_f,Ecart30_40)
df_f=cbind(df_f,Ecart40_50)
df_f=cbind(df_f,Ecart50_60)
df_f=cbind(df_f,Ecart60_70)
df_f=cbind(df_f,Ecart70_80)

#Données
str(df_f)
summary(df_f)
df_features = df_f[,c(1,4,5,14,15,16,17,18,19,20,21)]

#On regarde via des boxplots si on peut voir des différences de zone
zone1=which(df_features$Zone==1)
zone2=which(df_features$Zone==2)
zone3=which(df_features$Zone==3)
zone4=which(df_features$Zone==4)

boxplot(df_features$TMG[zone1],df_features$TMG[zone2],df_features$TMG[zone3],df_features$TMG[zone4])
boxplot(df_features$Ecart30_40[zone1],df_features$Ecart30_40[zone2],df_features$Ecart30_40[zone3],df_features$Ecart30_40[zone4])

#Réduction de dimension par ACP

library(FactoMineR)
library(factoextra)
library(corrplot)


data = df_features[,-c(1)]
corrplot(cor(data[, -c(3)]))   #pas de corrélation flagrante et porteuse de sens


acp = PCA(data, quali.sup = 3)
plot(acp, habillage = 3, label = "none") #en fonction de la zone dans laquelle se trouve l'individu

plotellipses(acp, keepvar = 3, level=0.95, label = "none") #pas ouf comme ellipse

write.csv(x = df_features, file = "dataframe_to_mean.csv")

## Analyse acp avec interprétation