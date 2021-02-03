install.packages("dplyr")
library(dplyr)
install.packages("cluster")
install.packages("factoextra")
library(factoextra)
library(cluster)
library(purrr)

DATOS<-read.csv("C:/Prueba/DATOSFINALES.csv", header = TRUE, sep = ",")
DATOS$ENVIOS <- as.numeric(DATOS$ENVIOS)
#DATOS$DES_TIPO_UNIDAD <- as.numeric(DATOS$DES_TIPO_UNIDAD)

DATOS[,2:16] <- scale(DATOS[,2:16])

############################################################################################
#CLUSTERING JERARQUICO: REPRESENTACIÓN EN DENDOGRAMA
d <- dist(DATOS, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
hc2 <- hclust(d, method = "average" )
hc3 <- hclust(d, method = "single" )
hc4 <- hclust(d, method = "centroid" )
hc5 <- hclust(d, method = "ward.D" )
hc6 <- agnes(DATOS, method = "complete")
hc6
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k=11, border="red")



cor(x = d, cophenetic(hc6))
clusters <- cutree(hc1, k = 11)
write.table(clusters, file = "C:/Prueba/SUBGRUPOS_HC1_mejorado.csv")
View(clusters)

#########################################################################################

fviz_nbclust(x = DATOS[,2:16], FUNcluster = kmeans, method = "wss", k.max = 15) +
  labs(title = "Número óptimo de clusters")

############################################################################################
clusterK <- kmeans(DATOS[,2:16],12, nstart = 50)

clusterK
fviz_cluster(clusterK, data = DATOS)

DATOS$cluster_K <- as.numeric(clusterK$cluster)
View(DATOS)
write.table(DATOS$cluster_K, file = "C:/Prueba/SUBGRUPOS_k_mejorados2.csv")

##############################################################################################
clara_clusters <- clara(x = DATOS, k = 12, metric = "euclidean", stand = TRUE,
                        samples = 50, pamLike = TRUE)
clara_clusters
fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 1) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

grupos<-data.frame(DATOS$COD_OFICINA)
grupos<-cbind(grupos,data.frame(clara_clusters$clustering))
grupos
write.table(grupos, file = "C:/Prueba/SUBGRUPOS_CLARA_MEJORADOS.csv")

####################################################################################################





