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

lay = layout.norm(cbind(V(network_logical)$long, V(network_logical)$lat),
                  xmax = summary(V(network_logical)$long)[6],
                  xmin = summary(V(network_logical)$long)[1],
                  ymax = summary(V(network_logical)$lat)[6],
                  ymin = summary(V(network_logical)$lat)[1])
#IN DEGREE
in_degree = degree(network_logical, mode="in")
summary(in_degree)

table(in_degree)
round(100*table(in_degree)/length(in_degree), 2)

n_in = length(table(in_degree))
colors = as.vector(matrix(data=rep(brewer.pal(n=9, name="YlOrBr"), ceiling(n_in/9)), ncol=9, byrow=T))
barplot(table(in_degree), ylab="count", xlab="in-degree", 
        col=colors[1:n_in], main="in-degree centrality")

colors_plot = character(max(in_degree)); j = 1
for (i in 1:max(in_degree)) {if (i %in% in_degree) {colors_plot[i] = colors[j]; j=j+1}}

#OUT DEGREE
out_degree = degree(network_logical, mode="out")
out_degree[1:10]

summary(out_degree)
table(out_degree)
round(100*table(out_degree)/length(out_degree), 2)

n_out = length(table(out_degree))
colors = as.vector(matrix(data=rep(brewer.pal(n=9, name="YlOrBr"), ceiling(n_out/9)), ncol=9, byrow=T))
barplot(table(out_degree), ylab="count", xlab="out-degree", 
        col=colors[1:n_out], main="out-degree centrality")

windows(); plot(network_logical, 
                layout=lay,
                vertex.size=out_degree^2,  
                vertex.label=NA,
                vertex.color=colors_plot[out_degree],
                edge.arrow.size=0.01,
                edge.arrow.width=0.1,
                edge.color="black",
                main="Out degreee of the network")

degree_data = data.frame(node = names(in_degree),
                         in_degree = as.numeric(in_degree),
                         out_degree = as.numeric(out_degree))

#BETWEENESS NODES
betta = betweenness(network_logical, directed = TRUE, weights = (1/(E(network_logical)$duration)))

lay = layout.norm(cbind(V(network_logical)$coordds.x, V(network_logical)$coordds.y))
colors = brewer.pal(n=6, name="Greens")
colors_plot = ifelse (betta<=3.2e+04, colors[1],
                      ifelse(betta<=5e+04 & betta>3.2e+04, colors[2],
                             ifelse(betta<=7.5e+04 & betta>5e+04, colors[3],
                                    ifelse(betta<=1e+05 & betta>7.5e+04, colors[4], 
                                           ifelse(betta<=2e+05 & betta>1e+05, colors[5], colors[6])))))

windows(); plot(network_logical, 
                layout=lay,
                vertex.size=betta/50, vertex.label=NA,
                vertex.color=colors_plot,
                edge.arrow.size=0.01,
                edge.arrow.width=0.1,
                edge.color="grey",
                main="Betweenness centrality")
dev.off()

betweenness_data = data.frame(node = names(betta),
                              betweenness = as.numeric(betta))

#EDGES BETWENEES
bet_e = edge_betweenness(network_logical, e=E(network_logical), directed=TRUE, weights = (1/(E(network_logical)$duration)))

summary(bet_e)

# edges ordered by betweenness:
edge_bet_data = data.frame(node_a=ends(network_logical, E(network_logical))[,1],
                           node_b=ends(network_logical, E(network_logical))[,2],
                           betw=bet_e)
edge_bet_data = edge_bet_data[order(edge_bet_data$betw, decreasing=T),]
head(edge_bet_data)
tail(edge_bet_data)

edge_bet_data$unofficial_node_b =  edge_bet_data$unofficial_node_a= character(ecount(network_logical))
for (i in 1:ecount(network_logical)) {
  edge_bet_data$unofficial_node_b[i] = V(network_logical)$unofficial_name[which(V(network_logical)$name==edge_bet_data$node_b[i])]
  edge_bet_data$unofficial_node_a[i] = V(network_logical)$unofficial_name[which(V(network_logical)$name==edge_bet_data$node_a[i])]
}
head(edge_bet_data[,3:5])
tail(edge_bet_data[,3:5])

windows(); plot(network_logical, layout=lay,
                vertex.size=2, vertex.label=NA,
                edge.arrow.size=0.01,
                edge.arrow.width=NA,
                edge.width=bet_e/20,
                edge.color="black", 
                main="betweenness of edges")

#####
save(degree_data,betweenness_data, file = "workplace/RData/micro_analys.RData")

centrality_data = merge(degree_data, betweenness_data)

dev.new(); pairs(centrality_data[,c("out_degree", 
                                    "betweenness")])
as.dist(cor(centrality_data[,c("out_degree", 
                               "betweenness")]))