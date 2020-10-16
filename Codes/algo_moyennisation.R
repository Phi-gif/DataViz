#Algorithme de moyennisation des données clean_data

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
    df_features[ligne,17]=sum(df_features[ligne:j,17])/(j+1-ligne)
    df_features[ligne,18]=sum(df_features[ligne:j,18])/(j+1-ligne)
    df_features[ligne,19]=sum(df_features[ligne:j,19])/(j+1-ligne)
    df_features[ligne,20]=sum(df_features[ligne:j,20])/(j+1-ligne)
    df_features[ligne,21]=sum(df_features[ligne:j,21])/(j+1-ligne)
    df_features[ligne,22]=sum(df_features[ligne:j,22])/(j+1-ligne)
    df_features[ligne,23]=sum(df_features[ligne:j,23])/(j+1-ligne)
    print('new boucle')
    for (k in j:(ligne+1)){
      
      print(k)
      df_features=df_features[-k,]
    }
  }
  ligne=ligne+1
}

write.csv(x = df_features, file = "clan_data_mean.csv")
