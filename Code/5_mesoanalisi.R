rm(list=ls())
graphics.off()
dev.off()

library(sf)
library(igraph)
library(stplanr)
library(osrm)
library(mapview)
library(leaflet)
library(tidyverse)
library(reshape)
library(RColorBrewer)

setwd("D:/OneDrive - Politecnico di Milano/Materiale_Esami_Da_Fare/progetti/Network")
load("workplace/RData/rete.RData")
load("workplace/RData/path_creation.RData")

#CORENESS NULLA DA DIRE

lay = layout.norm(cbind(V(network_logical)$long, V(network_logical)$lat),
                  xmax = summary(V(network_logical)$long)[6],
                  xmin = summary(V(network_logical)$long)[1],
                  ymax = summary(V(network_logical)$lat)[6],
                  ymin = summary(V(network_logical)$lat)[1])


set.seed(5)

clust_eb = cluster_edge_betweenness(network_logical, weights=E(network_logical)$distance)
table(clust_eb$membership)
max(clust_eb$modularity)

cluster_betw = data.frame(node = V(network_logical)$name,
                          cluster = clust_eb$membership)
head(cluster_betw)


col <- array(c("#5b3db1",
               "#a4c24e",
               "#b44c43",
               "#4bb3bc",
               "#f3a505",
               "#f400a1",
               "#543210",
               "#c9a0dc",
               "#08e8de",
               "#24cc7c",
               "#d84b20",
               "#660066",
               "#b3b3b3"))



for (i in 1:length(clust_eb$membership)) {
  clust_eb$tinta[i]=col[clust_eb$membership[i]]
}

{
x11();
plot(network_logical, layout=lay,
     vertex.size=ifelse(V(network_logical)$fittizi==1, 3, 5), vertex.label=NA,
     vertex.color=clust_eb$tinta,
     edge.arrow.size=0,
     edge.arrow.width=0,
     vertex.shape=ifelse(V(network_logical)$fittizi==1, "circle", "square"),
     edge.color="black")
  legend("topleft", legend = paste("Group", 1:13), pch=19, col=col)
}


#legend("bottomleft", legend = paste("Group", 1:13), pch=19, col=1:13)


{
  x11();
  plot_dendrogram(clust_eb, hang = -1, cex = 0.6) 
}
dev.off()