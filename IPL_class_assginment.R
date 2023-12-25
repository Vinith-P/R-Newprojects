IPL_data <- W25530_XLS_ENG_3_
IPL_data <- IPL_data[,-c(1,2,4)]
library(tidyverse)
library(cluster)
library(factoextra)
z_data <- scale(IPL_data[,-1])
fviz_nbclust(z_data, kmeans, method = "wss")
fviz_nbclust(z_data, kmeans, method = "silhouette")
fviz_nbclust(z_data, kmeans, method = "gap_stat")

#cluster Analysis with 2 clusters

set.seed(1234)
cluster_no_2 <- kmeans(z_data, 2, nstart = 100)
cluster_no_2
IPL_data$cluster_2 <- cluster_no_2$cluster
km_cluster <- cluster_no_2$cluster
rownames(z_data) <- paste(IPL_data$Player, 1:dim(IPL_data)[1],sep = "_")
fviz_cluster(list(data=z_data, cluster = km_cluster))


#cluster Analysis with 3 clusters

set.seed(1234)
cluster_no_3 <- kmeans(z_data, 3, nstart = 100)
cluster_no_3
IPL_data$cluster_3 <- cluster_no_3$cluster
km_cluster <- cluster_no_3$cluster
rownames(z_data) <- paste(IPL_data$Player, 1:dim(IPL_data)[1],sep = "_")
fviz_cluster(list(data=z_data, cluster = km_cluster))

#cluster Analysis with 4 cluster

set.seed(1234)
cluster_no_4 <- kmeans(z_data, 4, nstart = 100)
cluster_no_4
IPL_data$cluster_4 <- cluster_no_4$cluster
cluster_names <- c("High_P&High_S", "Low_P&Low_S", "High_P&Med_S", "Low_P&VeryLow_S")
names_clusters <- cluster_names[cluster_no_4$cluster]
#km_cluster <- cluster_no_4$cluster
rownames(z_data) <- paste(IPL_data$Player, 1:dim(IPL_data)[1],sep = "_")
fviz_cluster(list(data=z_data, cluster = names_clusters))
#fviz_cluster(list(data=z_data, cluster = km_cluster))
