## Ouverture du fichier excel
library(readxl)

file = 'clean_data.xlsx'
plan_semis=read_excel(file, sheet=9)
features=read_excel(file, sheet=7)

## On enlève ce qui est pas utile (le contrôle)
df_features=features[17:nrow(features),]
df_semis=plan_semis[17:nrow(plan_semis),c(1,3,4,5)]

#On rajoute les colonnes banc, camera et zone au df_features pour tout avoir sur un seul df

df_features=cbind(df_features,df_semis[,2])
df_features=cbind(df_features,df_semis[,3])
df_features=cbind(df_features,df_semis[,4])


#On modifie le type des données
str(df_features)
df_features$Banc = as.factor(df_features$Banc)
df_features$Camera = as.factor(df_features$Camera)
df_features$zone = as.factor(df_features$zone)

#affichage des répartitions des données en fonction des bancs, camera et zones
summary(df_features$Banc)       #376 échantillons sur le banc 2 et 33 sur le banc 4
summary(df_features$Camera)     #393 données sous la caméra 4 (banc 2 et 4) et 16 sous la caméra 0 (banc 2 et 4)
summary(df_features$zone)       # répartition équitbale en fonction des zones


#On calcule les écarts et on les ajoutes dans df_features (cinétiques)
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

#On regarde via des boxplots si on peut déjà voir des effets des bancs sur la vitesse de germination
banc2=which(df_features$Banc==2)
banc4=which(df_features$Banc==4)

boxplot(df_features$Ecart50_60[banc2],df_features$Ecart50_60[banc4])
boxplot(df_features$Ecart30_40[banc2],df_features$Ecart30_40[banc4])
boxplot(df_features$TMG[banc2],df_features$TMG[banc4]) #écart de TMG entre les 2 bancs mais sûrement biaisé du au problème de répartition des données

#On regarde via des boxplots si on peut voir des différences de zone
zone11=which(df_features$zone==11)
zone12=which(df_features$zone==12)
zone13=which(df_features$zone==13)
zone14=which(df_features$zone==14)

boxplot(df_features$TMG[zone11],df_features$TMG[zone12],df_features$TMG[zone13],df_features$TMG[zone14])
boxplot(df_features$Ecart30_40[zone11],df_features$Ecart30_40[zone12],df_features$Ecart30_40[zone13],df_features$Ecart30_40[zone14])

#Réduction de dimension par ACP

library(FactoMineR)
library(factoextra)
library(corrplot)
library(ade4)
library(ggplot2)


data = df_features[,c(4,5,14,15,16,17,18,19,20,21,22,23)]
corrplot(cor(data[, -c(3,4,5)]))   #pas de corrélation flagrante et porteuse de sens


acp = PCA(data, quali.sup = 3:5)
plot(acp, habillage = 3, label = "none") #en fonction du banc sur lequel se trouve l'individu
plot(acp, habillage = 4, label = "none") #en fonction de la camera sous laquelle se trouve l'individu
plot(acp, habillage = 5, label = "none") #en fonction de la zone dans laquelle se trouve l'individu

plotellipses(acp, keepvar = 3:5, level=0.95) #pas ouf comme ellipse

## Analyse acp avec interprétation