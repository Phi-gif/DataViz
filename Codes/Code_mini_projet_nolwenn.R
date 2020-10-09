## Ouverture du fichier excel
library(readxl)

plan_semis=read_excel('/users/2021ds/116000961/Documents/Projet_data_visualisation/donnees.xlsx', sheet=9)
df=read_excel('/users/2021ds/116000961/Documents/Projet_data_visualisation/donnees.xlsx', sheet=7)

## On enlève ce qui est pas utile
df=df[17:nrow(df),]
plan_semis=plan_semis[17:nrow(plan_semis),]

#On rajoute une colonne banc et zone

Banc=plan_semis$zone%/%10
Zone=plan_semis$zone%%10
df=cbind(df,Banc)
df=cbind(df,Zone)

#On modifie le type des données
df$T10=as.numeric(df$T10)
df$T20=as.numeric(df$T20)
df$T30=as.numeric(df$T30)
df$T40=as.numeric(df$T40)
df$T50=as.numeric(df$T50)
df$T60=as.numeric(df$T60)
df$T70=as.numeric(df$T70)
df$T80=as.numeric(df$T80)
df$T90=as.numeric(df$T90)

#On calcule les écarts et on les ajoutes dans df
Ecart10_20=df$T20-df$T10
Ecart20_30=df$T30-df$T20
Ecart30_40=df$T40-df$T30
Ecart40_50=df$T50-df$T40
Ecart50_60=df$T60-df$T50
Ecart60_70=df$T70-df$T60
Ecart70_80=df$T80-df$T70
Ecart80_90=df$T90-df$T80

df=cbind(df,Ecart10_20)
df=cbind(df,Ecart20_30)
df=cbind(df,Ecart30_40)
df=cbind(df,Ecart40_50)
df=cbind(df,Ecart50_60)
df=cbind(df,Ecart60_70)
df=cbind(df,Ecart70_80)
df=cbind(df,Ecart80_90)


#On regarde via des boxplots si on peut voir des différences de banc
banc1=which(df$Banc==1)
banc2=which(df$Banc==2)
banc3=which(df$Banc==3)
banc4=which(df$Banc==4)

boxplot(df$Ecart50_60[banc1],df$Ecart50_60[banc2],df$Ecart50_60[banc3],df$Ecart50_60[banc4])
boxplot(df$Ecart30_40[banc1],df$Ecart30_40[banc2],df$Ecart30_40[banc3],df$Ecart30_40[banc4])
boxplot(df$TMG[banc1],df$TMG[banc2],df$TMG[banc3],df$TMG[banc4])

plot(df$banc[banc1],df$Ecart50_60[banc1],col='1')
points(df$Ecart50_60[banc2],col='2')
points(df$Ecart50_60[banc3],col='3')
points(df$Ecart50_60[banc4],col='4')


#On regarde via des boxplots si on peut voir des différences de zone
zone1=which(df$Zone==1)
zone2=which(df$Zone==2)
zone3=which(df$Zone==3)
zone4=which(df$Zone==4)
boxplot(df$TMG[zone1],df$TMG[zone2],df$TMG[zone3],df$TMG[zone4])
boxplot(df$Ecart30_40[zone1],df$Ecart30_40[zone2],df$Ecart30_40[zone3],df$Ecart30_40[zone4])




library(FactoMineR)
df$Banc=as.factor(df$Banc)
df$Zone=as.factor(df$Zone)
acp=PCA(df[,c(4,14,17,18,19,20,21,22)],quali.sup = c(df[,15],df[,16]))
plot(acp)
#, habillage = 'Banc')
#, label=FALSE)

## Moyenner sur les répétitions
## S'occuper des NaN sur les répétitions
## ACP en colorant en fonction du banc puis de la zone



