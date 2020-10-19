#Algorithme de moyennisation des données clean_data

file = read.csv('dataframe_to_mean.csv', sep =',')
df_features = file[,-c(1)]

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
    df_features[ligne,2]=sum(df_features[ligne:j,2])/(j+1-ligne)
    df_features[ligne,3]=sum(df_features[ligne:j,3])/(j+1-ligne)
    df_features[ligne,5]=sum(df_features[ligne:j,5])/(j+1-ligne)
    df_features[ligne,6]=sum(df_features[ligne:j,6])/(j+1-ligne)
    df_features[ligne,7]=sum(df_features[ligne:j,7])/(j+1-ligne)
    df_features[ligne,8]=sum(df_features[ligne:j,8])/(j+1-ligne)
    df_features[ligne,9]=sum(df_features[ligne:j,9])/(j+1-ligne)
    df_features[ligne,10]=sum(df_features[ligne:j,10])/(j+1-ligne)
    df_features[ligne,11]=sum(df_features[ligne:j,11])/(j+1-ligne)
    print('new boucle')
    for (k in j:(ligne+1)){
      
      print(k)
      df_features=df_features[-k,]
    }
  }
  ligne=ligne+1
}

write.csv(x = df_features, file = "clean_data_mean.csv")
