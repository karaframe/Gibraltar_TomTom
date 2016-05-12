
library(maptools)
library(threadr) # load this package before dplyr
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(importr)
library(lubridate)
library(webshot)
library(htmlwidgets)

# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR")
setwd("C:/PostgreSQL_Gibraltair/OpenLR")

# load traffic data until 11 May 2016
# these are stationary Traffic & queueing Traffic & low Traffic
traffic <- read.csv("traffic_data_FK_until_11_May_2016.csv")  

###################################################################################################

POINTS_1st <- read.csv("points_traffic_data_1st_11_May.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_data_2nd_11_May.csv")[2:4]

# Join traffic data (speed) to spatial data
# traffic <- cbind(traffic,POINTS)
traffic_1st <- cbind(traffic,POINTS_1st)
traffic_2nd <- cbind(traffic,POINTS_2nd)

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)
write.csv(traffic, "traffic_stationary_until_11May2016.csv")
traffic <- read.csv("traffic_stationary_until_11May2016.csv")


# make a quick leaflet map with site loaction only

# Marker + Static Label using custom label options
map <- leaflet(data = traffic[,]) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   weight = 1, radius=5, color = 'black',
                   stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(traffic$averagespeed),
                   labelOptions = labelOptions(noHide = F)) %>%
addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  # overlayGroups = c("Position", "NO2"),
  options = layersControlOptions(collapsed = TRUE)) 

map


## create a series of .png figures for each hour
saveWidget(map, 'gibraltar_TomTom_Stationary.html', selfcontained = FALSE)


###################### end #############################################################
########################################################################################
