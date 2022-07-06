rm(list=ls())
graphics.off()
dev.off()

library(sf)
library(igraph)
library(stplanr)
library(readxl)
library(osrm)
library(mapview)
library(leaflet)
library(tidyverse)
library(leafem)
library(data.table)
#library(plotly)

setwd("D:/OneDrive - Politecnico di Milano/Materiale_Esami_Da_Fare/progetti/Network")

load("workplace/RData/path_creation.RData")

##GOAL SCRIPT:
## Dato path_negozi creare rete "logica" (esempio se per andare negozio A passo da negozio B allora sono collegati)

#riguardare da 1:43 -> NON FUNZIONA xch�

#####Dopo aver creato manualmente le connessioni con excel si crea la rete con graph_from_data_frame)
edges_for_logical_network = data.frame(read_excel("Data/negozi_shp/connessioni.xlsx"))

data = data.frame(name = character(), color = character(), size = integer(), lat=numeric(),long=numeric(), flow_alim=numeric(), flow_n_alim=numeric(), fictitious = boolean())

data = rbind(data,data.frame(name= negozi$Descrizione.Negozio, color = "yellow", size = negozi$Sup.totale, lat=negozi$lat, long=negozi$long,
                             flow_alim = negozi$rifornimento_alimentare, flow_n_alim=negozi$rifornimento_non_alimentare))

data$color[64]="red"

false_nodes= st_read("Data/negozi_shp/nodi_fittizi.shp")

fn_coordinates=data.frame(st_coordinates(false_nodes))

data=rbind(data,data.frame(name=false_nodes$nome,color="gray",size=10,
                           lat=fn_coordinates$Y, long=fn_coordinates$X, flow_alim=0, flow_n_alim=0))

dist_dur_nodes = data.frame(distance = numeric(), duration = numeric())

for (i in 1:nrow(edges_for_logical_network)) {
  index_from <- data$name==edges_for_logical_network$from[i]
  index_to <- data$name==edges_for_logical_network$to[i]
  
   temp <- route(
        from = c(data$long[index_from],  data$lat[index_from]),
        to = c(data$long[index_to],  data$lat[index_to]),
        route_fun = osrmRoute,
        returnclass = "sf",
        overview = "full",
        osrm.profile = "car"
      )
   dist_dur_nodes = rbind(dist_dur_nodes,
                          data.frame(distance = temp$distance, duration = temp$duration))
}

edges_for_logical_network= cbind(edges_for_logical_network,dist_dur_nodes)

##SERVIVA_PER_CORREZIONE_ERRORI_NON_RUNNARE
      #test = edges_for_logical_network %>% count(to, sort = TRUE)
      #for (i in 1:nrow(test)){
       # print(test$to[i])
        #g3 = try({graph_from_data_frame(edges_for_logical_network, 
         #                          directed=T,
          #                         vertices=rbind(data,data.frame(name=test$to[i],color="white",size=10)))})
        #ifelse(is.igraph(g3), break, print("nada"))
      #}
######

network_logical = graph_from_data_frame(edges_for_logical_network, 
                           directed=T,
                           vertices=data)

x11()
lay = layout.norm(cbind(V(network_logical)$long, V(network_logical)$lat),
                  xmax = summary(V(network_logical)$long)[6],
                  xmin = summary(V(network_logical)$long)[1],
                  ymax = summary(V(network_logical)$lat)[6],
                  ymin = summary(V(network_logical)$lat)[1])
p <- plot(network_logical, layout=lay,
     vertex.label=NA, vertex.size=5, 
     edge.arrow.size=1)

save(network_logical, edges_for_logical_network,data, file = "workplace/RData/rete.RData")
