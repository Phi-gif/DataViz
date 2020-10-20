library(umap)
str(as.matrix(df_features[,c(4,5,14,15,16,17,18,19,20,21,22)]))
umap_par=umap.defaults
umap_par$n_neighbors=5
umap_par$min_dist=0.25
umap_par$metric='euclidean'
#umap_par$metric='manhattan'


# umap_par$metric='cosine'
# umap_par$n_neighbors=5
# umap_par$min_dist=0.1
# 
# umap_par$metric='pearson2'
# umap_par$min_dist=0.1
# umap_par$n_neighbors=5

umap_par$verbose=TRUE
umap_par$n_components=2
umap_par$local_connectivity=1

## Test min_dist
layout(matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE))
for (i in c(0.05, 0.1, 0.25, 0.5, 0.8, 0.99)){
  umap_par$min_dist=i
  U=umap(df_features[,c(4,5,16,17,18,19,20,21,22)],config = umap_par)
  plot(U$layout,col=df_features[,14],xlab = 'Dim1',ylab = 'Dim2',pch=20)
}

## Test n_neighbors
layout(matrix(c(1:6), nrow=2, ncol=3, byrow=TRUE))
for (i in c(2, 5, 10, 20, 50, 100)){
  umap_par$n_neighbors=i
  U=umap(df_features[,c(4,5,16,17,18,19,20,21,22)],config = umap_par,xlab = 'Dim1',ylab = 'Dim2')
  plot(U$layout,col=df_features[,14],pch=20)
}
