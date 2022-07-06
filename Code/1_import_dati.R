#install.packages("readxl", "stplanr", "osrm", "mapview", "sf")

rm(list=ls())
graphics.off()
dev.off()

library(sf)
library(igraph)

library(stplanr)#Questo pacchetto permette di creare le linee similmente a quanto visto a lezione
library(readxl)
library(osrm)
library(mapview)

library(leaflet)
library(tidyverse)
library(leafem)



setwd("D:/OneDrive - Politecnico di Milano/Materiale_Esami_Da_Fare/progetti/Network")

#####DATA IMPORTING######

centro_distribuzione = c(9.31803032246597, 45.4853160839371)

negozi = data.frame(read_excel("Data/Merge_elenco_infante_Open_Data_Lombardia.xlsx"))

####FLOW ASSIGNATION########
interv=c(2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0)
interv2=c(2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0)

negozi$rifornimento_alimentare = as.numeric(cut(negozi$Sup.totale, breaks=11, labels = interv))

for (i in 1:nrow(negozi)) {
  negozi[i,14]=interv[negozi[i,14]]
}

negozi$rifornimento_non_alimentare = as.numeric(cut(negozi$Sup.totale, breaks=21, labels = interv2))

for (i in 1:nrow(negozi)) {
  negozi[i,15]=interv[negozi[i,15]]
}

for (i in 1:nrow(negozi)) {
  if(is.na(negozi[i,15]))
    negozi[i,15]= 3.0
}

library(writexl)
write_xlsx(negozi, path = "Data/Merge_infante_ODL_2.xlsx")

######PATH CREATION#########
path_to_negozi <- route(
  from = centro_distribuzione,
  to = c(negozi[1, 12],  negozi[1, 11]),
  route_fun = osrmRoute,
  returnclass = "sf",
  overview = "full",
  osrm.profile = "car"
)

path_to_negozi$from = "CENTRO"
path_to_negozi$to = negozi[1,1]
path_to_negozi$freq_alim= negozi[1,14]
path_to_negozi$freq_non_alim= negozi[1,15]

for (i in 2:(nrow(negozi)-1)) {
  triptemp <- route(
    from = centro_distribuzione,
    to = c(negozi[i, 12],  negozi[i, 11]),
    route_fun = osrmRoute,
    returnclass = "sf",
    overview = "full",
    osrm.profile = "car"
  )
  triptemp$from = "CENTRO"
  triptemp$to = negozi[i,1]
  triptemp$freq_alim= negozi[i,14]
  triptemp$freq_non_alim= negozi[1,15]
  path_to_negozi = rbind(path_to_negozi, triptemp)
}

p_negozi = st_read("Data/negozi_shp/negozi.shp")

path_to_negozi <- path_to_negozi[, c(10, 11, 1,2,3,4,5,6,7,8,9,12,13)]

map_negozi_percorsi = mapview(p_negozi)+mapview(path_to_negozi)

leafem::addStaticLabels(map_negozi_percorsi, label = p_negozi$Descrizion, noHide = TRUE,direction = 'top',textOnly = TRUE,textsize = "15px")


help("st_multipoint")
################

flow_alim = overline(path_to_negozi, attrib = "freq_alim")

map_flow_alim = mapview(p_negozi)+mapview(flow_alim)

map_flow_alim =leafem::addStaticLabels(map_flow_alim, label = p_negozi$Descrizion, noHide = TRUE,direction = 'top',textOnly = TRUE,textsize = "15px")



flow_non_alim = overline(path_to_negozi, attrib = "freq_non_alim")

map_flow_non_alim = mapview(p_negozi)+mapview(flow_non_alim)

map_flow_non_alim = leafem::addStaticLabels(map_flow_non_alim, label = p_negozi$Descrizion, noHide = TRUE,direction = 'top',textOnly = TRUE,textsize = "15px")

#####

save(negozi, path_to_negozi, map_flow_alim, map_flow_non_alim, file="workplace/RData/path_creation.RData")

st_write(path_to_negozi, "workplace/shp_da_R/path_negozi.shp")
