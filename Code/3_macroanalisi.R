rm(list=ls())
graphics.off()
dev.off()

library(sf)
library(igraph)
library(stplanr)
#library(readxl)
library(osrm)
library(mapview)
library(leaflet)
library(tidyverse)

#library(leafem)
#library(data.table)
setwd("D:/OneDrive - Politecnico di Milano/Materiale_Esami_Da_Fare/progetti/Network")
load("workplace/RData/rete.RData")
load("workplace/RData/path_creation.RData")

map_flow_alim

x11()
lay = layout.norm(cbind(V(network_logical)$long, V(network_logical)$lat),
                  xmax = summary(V(network_logical)$long)[6],
                  xmin = summary(V(network_logical)$long)[1],
                  ymax = summary(V(network_logical)$lat)[6],
                  ymin = summary(V(network_logical)$lat)[1])
p <- plot(network_logical, layout=lay,
          vertex.label=NA, vertex.size=5, 
          edge.arrow.size=0.1)

edge_density(network_logical) #Dovuto all'uso della rete stadale -> se una strada si interrompe l'arco cade e ricreare percorso

transitivity(network_logical) #Dovuto all'uso della rete stadale -> se una strada si interrompe l'arco cade e ricreare percorso

count_components(network_logical)

#diameter 

diameter(network_logical, directed=T, weights=E(network_logical)$distance) 
diameter(network_logical, directed=T, weights=E(network_logical)$duration)
get_diameter(network_logical, directed=T, weights=E(network_logical)$duration)
get_diameter(network_logical, directed=T, weights=E(network_logical)$distance)
