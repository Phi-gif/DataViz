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

boxplot(df_features$Ecart50_60[banc1],df_features$Ecart50_60[banc2],df_features$Ecart50_60[banc3],df_features$Ecart50_60[banc4])
boxplot(df_features$Ecart30_40[banc1],df_features$Ecart30_40[banc2],df_features$Ecart30_40[banc3],df_features$Ecart30_40[banc4])
boxplot(df_features$TMG[banc1],df_features$TMG[banc2],df_features$TMG[banc3],df_features$TMG[banc4])

##pas sure de cette représentation
plot(df_features$Ecart50_60[banc1],col='1')
points(df_features$Ecart50_60[banc2],col='2')
points(df_features$Ecart50_60[banc3],col='3')
points(df_features$Ecart50_60[banc4],col='4')


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
library(ade4)
library(ggplot2)

Sbanc = df_features$Banc
SZone = df_features$Zone

data = df_features[,c(4,5,14,15,16,17,18,19,20,21,22)]

acp = PCA(data, quali.sup = 3:4)
plot(acp, habillage = 3, label = "none") #en fonction du banc sur lequel se trouve l'individu
plot(acp, habillage = 4, label = "none") #en fonction de la zone sur laquelle se trouve l'individu

corrplot(cor(data[, -c(3,4)]))   #pas de corrélation flagrante
plotellipses(acp, keepvar = 3:4, level=0.99) #pas ouf comme ellipse


write.csv(x = df_features, file = "data_to_mean.csv")
## Moyenner sur les répétitionsb --> cf algo nolwenn
## Analyse acp avec interprétation