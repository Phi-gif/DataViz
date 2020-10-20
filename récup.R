## Ouverture du fichier excel
library(readxl)

file = 'clean_data_mean.csv'
df_features = read.csv(file, sep =',')

## On enlève ce qui n'est plus utile
data=df_features[,-c(1)]

#On modifie le type des données
str(data)
data$Echantillon = as.character(data$Echantillon)
data$Zone = as.factor(data$Zone)

#affichage des répartitions des données en fonction des bancs, camera et zones
summary(data$Zone)       #répartition équitable en fonction des zones (48 individus par zone)
summary(data)


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
library(corrplot)

corrplot(cor(data[, -c(1,4)]))   #pas de corrélation flagrante et porteuse de sens
data_acp = data[,-c(1)]

acp_moy = PCA(data_acp, quali.sup = 3)
plot(acp_moy, habillage = 3, label = "none") #en fonction de la zone dans laquelle se trouve l'individu

plotellipses(acp_moy, keepvar = 3, level=0.95, label = "none") #peut être un effet de la zone 14

fviz_contrib(acp_moy,choice = "var",axes = 1:2)
fviz_eig(acp_moy,label=TRUE)

## Test
acp_moy2 = PCA(data_acp[,c(1,2,3,9)], quali.sup = 3)
plot(acp_moy2,habillage = 3, label = "none")
plotellipses(acp_moy2, keepvar = 3, level=0.95, label = "none")

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

## ACP 3D ZONE
FMacp3d(PCA.res=acp_moy, comp=1:3, group=as.factor(data[,4]), plotVars = FALSE,    #important que group soit en factor 
        pointSize=2, plotText=FALSE)

## pas beaucoup plus de séparation des points qu'en 2D

#UMAP

library(umap)

Umap.data=umap(data[,-c(1,4)])
plot(Umap.data$layout, col = data[,4]) # différence de zone

#Adapter les métriques et les plus proches voisins