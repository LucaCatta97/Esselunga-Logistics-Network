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

#Partizionamento Vd Arianna
set.seed(5)
lay = layout.norm(cbind(V(network_logical)$long, V(network_logical)$lat),
                  xmax = summary(V(network_logical)$long)[6],
                  xmin = summary(V(network_logical)$long)[1],
                  ymax = summary(V(network_logical)$lat)[6],
                  ymin = summary(V(network_logical)$lat)[1])

network_logical_undirected = as.undirected(network_logical, mode="collapse")

clust = cluster_leading_eigen(network_logical_undirected, weights = E(network_logical_undirected)$distance)

modularity(clust)

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
               "#d84b20"))

for (i in 1:length(clust$membership)) {
  clust$tinta[i]=col[clust$membership[i]]
}

{
  x11();
  plot(network_logical, layout=lay,
       vertex.size=ifelse(V(network_logical)$fittizi==1, 3, 5), vertex.label=NA,
       vertex.color=clust$tinta,
       vertex.label=V(network_logical)$name,
       edge.arrow.size=0,
       edge.arrow.width=0,
       vertex.shape=ifelse(V(network_logical)$fittizi==1, "circle", "square"),
       edge.color="black")
  legend("bottomleft", legend = paste("Group", 1:11), pch=19, col=col)
}

{
  x11();
  plot_dendrogram(clust, hang = 0, cex = 0.5, palette = col, mode = "phylo" ) 
}

partition_2<- array()

#non sapendo come dividere
for (i in c(2, 11)) {
  partition_2 = append(clust[[i]], partition_2)
}

#Scelta partizionamento senza distribution centre

elenco_negozi_part_2 <- array()

#Rimozione punti fittizi

for (j in 1:(length(partition_2))) {
  if(grepl("^[[:upper:]]+$", partition_2[j])==TRUE){
    elenco_negozi_part_2 = append(partition_2[j], elenco_negozi_part_2)
  }
}

elenco_negozi_part_2

save(elenco_negozi_part_2, file = "workplace/RData/selected_point.RData")

#Algoritmo Logistic per trovare il nuovo  distribution centre
##RUN DA QUI AVENDO GIà fatto cluster
rm(list=ls())
graphics.off()
dev.off()

library(readxl)
negozi_sel = data.frame(read_excel("Data/selected_item.xlsx"))

dist <- function(lat_pos, lon_pos, lat_CG, lon_CG){
  return(sqrt((lon_pos-lon_CG)^2+(lat_pos-lat_CG)^2)*69*1.609*1.3)
}

{
calcola_vettore_distanza<-function(X,Y){
  di_l = rep(0,nrow(negozi_sel))
  for (i in 1:nrow(negozi_sel)) {
    di_l[i]=dist(negozi_sel$lat[i],negozi_sel$long[i],X, Y)
  }
  return(di_l)
}

calcola_costo<-function(di){
  costo = 0
  for (i in 1:nrow(negozi_sel)) {
    costo = costo + (negozi_sel$rifornimento_alimentare[i]*R[i]*di[i])
  }
  
  return(costo)
}


calcola_CG <- function(di_l){
  num_X = 0
  den = 0
  num_Y =0
  for (i in 1:nrow(negozi_sel)){
    num_X= num_X + (negozi_sel$rifornimento_alimentare[i]*R[i]*negozi_sel$lat[i])/di_l[i]
    num_Y = num_Y + (negozi_sel$rifornimento_alimentare[i]*R[i]*negozi_sel$long[i])/di_l[i]
    den = den + (negozi_sel$rifornimento_alimentare[i]*R[i])/di_l[i]
  }
  
  X = num_X/den
  Y = num_Y/den
  rm(den,num_X,num_Y)
  
  return(c(X,Y))
  
}
}
#Primo Step
{
    R = rep(1,nrow(negozi_sel))
    num_X_appr = 0
    den__appr = 0
    num_Y_appr =0
    for (i in 1:nrow(negozi_sel)){
      num_X_appr= num_X_appr + (negozi_sel$rifornimento_alimentare[i]*R[i]*negozi_sel$lat[i])
      num_Y_appr = num_Y_appr + (negozi_sel$rifornimento_alimentare[i]*R[i]*negozi_sel$long[i])
      den__appr = den__appr + (negozi_sel$rifornimento_alimentare[i]*R[i])
    }
    
    X_appr = num_X_appr/den__appr
    Y_appr = num_Y_appr/den__appr
    
    di <- rep(0,nrow(negozi_sel))
    
    for (i in 1:nrow(negozi_sel)) {
      di[i]=dist(negozi_sel$lat[i],negozi_sel$long[i],X_appr,Y_appr)
    }
    rm(den__appr,num_X_appr,num_Y_appr,i)
    
}

#Secondo step

result = data.frame(step=numeric(), X=numeric(), Y=numeric(), cost=numeric())


for (i in 1:40) {
  CG = calcola_CG(di)
  
  di = calcola_vettore_distanza(CG[1],CG[2])
  
  costo_l = calcola_costo(di)
  
  result=rbind(result, data.frame(step=i, X=CG[1], Y=CG[2], cost=costo_l))
}


plot(x=result$step,y=result$cost, type="b")
plot(x=result$X, y=result$Y)

