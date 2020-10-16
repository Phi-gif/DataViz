## Ouverture du fichier excel
library(readxl)

file = 'clan_data_mean.csv'
df_features = read.csv(file, sep =',')

## On enlève ce qui n'est plus utile
data=df_features[,-c(1,3,4,7,8,9,10,11,12,13,14)]

#On modifie le type des données
str(data)
data$Echantillon = as.character(data$Echantillon)
data$Banc = as.factor(data$Banc)
data$Camera = as.factor(data$Camera)
data$zone = as.factor(data$zone)

#affichage des répartitions des données en fonction des bancs, camera et zones
summary(data$Banc)       #192 échantillons sur le banc 2 et 12 sur le banc 4
summary(data$Camera)     #toutes les données sous la caméra 4 (banc 2 et 4) --> pb lors de la moyennisation ?
summary(data$zone)       #répartition équitable en fonction des zones


#Données
str(data)
summary(data)

#On regarde via des boxplots si on peut déjà voir des effets des bancs sur la vitesse de germination
banc2=which(data$Banc==2)
banc4=which(data$Banc==4)

layout(matrix(c(1:8), nrow=2, ncol=4, byrow=TRUE))
boxplot(data$TMG[banc2],data$TMG[banc4], main='TMG')
boxplot(data$Ecart10_20[banc2],data$Ecart10_20[banc4],ylim=c(0,60),main='Ecart 10-20')
boxplot(data$Ecart20_30[banc2],data$Ecart20_30[banc4],ylim=c(0,60),main='Ecart 20-30')
boxplot(data$Ecart30_40[banc2],data$Ecart30_40[banc4],ylim=c(0,60),main='Ecart 30-40')
boxplot(data$Ecart40_50[banc2],data$Ecart40_50[banc4],ylim=c(0,60),main='Ecart 40-50')
boxplot(data$Ecart50_60[banc2],data$Ecart50_60[banc4],ylim=c(0,60),main='Ecart 50-60')
boxplot(data$Ecart60_70[banc2],data$Ecart60_70[banc4],ylim=c(0,60),main='Ecart 60-70')
boxplot(data$Ecart70_80[banc2],data$Ecart70_80[banc4],ylim=c(0,60),main='Ecart 70-80')
dev.off()

##Ecart pour la TMG entre les 2 bancs très certainement dû à répartition inégales des individus

#On regarde via des boxplots si on peut voir des différences de zone
zone11=which(data$zone==11)
zone12=which(data$zone==12)
zone13=which(data$zone==13)
zone14=which(data$zone==14)
zone21=which(data$zone==21)
zone22=which(data$zone==22)
zone23=which(data$zone==23)
zone24=which(data$zone==24)
zone31=which(data$zone==31)
zone32=which(data$zone==32)
zone33=which(data$zone==33)
zone34=which(data$zone==34)
zone41=which(data$zone==41)
zone42=which(data$zone==42)
zone43=which(data$zone==43)
zone44=which(data$zone==44)


boxplot(data$TMG[zone11],data$TMG[zone12],data$TMG[zone13],data$TMG[zone14],data$TMG[zone21],data$TMG[zone22],data$TMG[zone23],data$TMG[zone24],data$TMG[zone31],data$TMG[zone32],data$TMG[zone33],data$TMG[zone34],data$TMG[zone41],data$TMG[zone42],data$TMG[zone43],data$TMG[zone44],main='TMG pour toutes les zones')
## Quoi en dire ??


#Réduction de dimension par ACP

library(FactoMineR)
library(factoextra)
library(corrplot)
library(ade4)
library(ggplot2)

corrplot(cor(data[, -c(1,4,5,6)]))   #pas de corrélation flagrante et porteuse de sens
data_acp = data[,-c(1)]

acp_moy = PCA(data_acp, quali.sup = 3:5)
plot(acp_moy, habillage = 3, label = "none") #en fonction du banc sur lequel se trouve l'individu
plot(acp_moy, habillage = 4, label = "none") #en fonction de la camera sous laquelle se trouve l'individu
plot(acp_moy, habillage = 5, label = "none") #en fonction de la zone dans laquelle se trouve l'individu

plotellipses(acp_moy, keepvar = 5, level=0.95) #peut être un effet de la zone 14

#ACP 3D

library(rgl)
FMacp3d<-function(PCA.res, comp=1:3, group, plotVars = FALSE,  
                  pointSize=2, plotText=FALSE){
  if(!require("rgl")) stop("You must install rgl");
  if(length(comp)!=3) stop("You must give a vector of 3 integer for comp parameter")
  if(!plotVars){
    x<-PCA.res$ind$coord
  }else{
    x<-PCA.res$var$coord
  }
  if(is.null(levels(group))){ colors="black"}
  else{
    hashCol<-rainbow(nlevels(group))
    names(hashCol)<-levels(group)
    colors<-hashCol[group]
  }
  
  percentVar <- PCA.res$eig[,"percentage of variance"]
  plot3d(x[,comp[1]],x[,comp[2]],x[,comp[3]],
         xlab=paste0("PC",comp[1],": ",round(percentVar[comp[1]] ),"%"), 
         ylab=paste0("PC",comp[2],": ",round(percentVar[comp[2]] ),"%"), 
         zlab=paste0("PC",comp[3],": ",round(percentVar[comp[3]] ),"%"),
         col=colors,size=5,type=ifelse(plotText,"n","p"),box = FALSE)
  
  legend3d("topright", legend = names(hashCol), pch = 16, col = hashCol, 
           cex=1, inset=c(0.02),bty="n", ncol = 1)   #grandir le cadre et rerunner pour avoir une legend a la bonne taille
  
  if(plotText) text3d(x[,comp[1]],x[,comp[2]],x[,comp[3]],texts=rownames(x),cex=pointSize,col=colors)
  if(plotVars) spheres3d(x=0,y=0,z=0, radius = 1,alpha=0.5,color="white")
  spheres3d(x=0,y=0,z=0, radius = 0.005,alpha=1,color="red")
} # fin de la fonction 

par3d(cex=0.5)  #par3d -> pour la taille de police


##ACP 3D BANC
FMacp3d(PCA.res=acp_moy, comp=1:3, group=as.factor(data[,4]), plotVars = FALSE,    #important que group soit en factor 
        pointSize=2, plotText=FALSE)

## ACP 3D ZONE
FMacp3d(PCA.res=acp_moy, comp=1:3, group=as.factor(data[,3]), plotVars = FALSE,    #important que group soit en factor 
        pointSize=2, plotText=FALSE)

## pas beaucoup plus de séparation des points qu'en 2D

#UMAP

library(umap)

Umap.data=umap(data[,-c(1,4,5,6)])
plot(Umap.data$layout, col = data[,4]) # différence de banc
plot(Umap.data$layout, col = data[,6]) # différence de banc

#Adapter les métriques et les plus proches voisins