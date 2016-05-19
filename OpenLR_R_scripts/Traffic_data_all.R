
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

options(warn=-1) # warnings OFF
# options(warn=0)  # warnings back ON!

# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR")
setwd("C:/PostgreSQL_Gibraltair/OpenLR")

# load traffic data until 11 May 2016
# these are stationary Traffic & queueing Traffic & low Traffic
# traffic <- read.csv("traffic_data_FK_until_11_May_2016.csv")  
traffic <- read.csv("traffic_data_FK_until_19_May_2016.csv")  


# load shp file for open street data (routes)
# dir <-  "C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR"
# dir <-  "C:/postgreSQL_Gibraltair/OpenLR"
# OSM_GIB <- readOGR(dsn = dir, layer = "gibraltar_OSM")
# plot(OSM_GIB)

#### Write GeoJSON file for OSM_GIB ############################
# ----- Write data to GeoJSON

dat <-paste(dir, "/",  ".GIB_geojson_Open_Street", sep="") 

####  ATT !!!!! erase existing .geojson file when re-runing code ######
writeOGR(OSM_GIB, dat, layer="", driver="GeoJSON")  

# read GeoJSON-------------------------------------------------------------
OSM_GIB <- readOGR(".GIB_geojson_Open_Street", "OGRGeoJSON")
plot(OSM_GIB)

###################################################################################################

# POINTS_1st <- read.csv("points_traffic_data_1st_11_May.csv")[2:4]
# POINTS_2nd <- read.csv("points_traffic_data_2nd_11_May.csv")[2:4]

POINTS_1st <- read.csv("points_traffic_data_1st_19May2016.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_data_2nd_19May2016.csv")[2:4]

# Join traffic data (speed) first and second point
# traffic <- cbind(traffic,POINTS)
traffic_1st <- cbind(traffic,POINTS_1st)
traffic_2nd <- cbind(traffic,POINTS_2nd)

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)
write.csv(traffic, "traffic_stationary_until_19May2016.csv")
traffic <- read.csv("traffic_stationary_until_19May2016.csv")


# remove duplicates of lon & lat and sort latitude from big to small
traffic <- traffic %>%
  distinct(lon) 
# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic <- traffic[traffic$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic <- traffic[traffic$lat!=36.1560165880387,]   
traffic <- traffic[traffic$averagespeed!=0,] 


#########################################################################################
# cut routes between points--------------------------


# make a spatial dataframe with traffic data 
sp_traffic <- SpatialPointsDataFrame(traffic[,40:41], traffic,            # lat, lon
                                     proj4string=CRS("+init=epsg:4326")) 
plot(sp_traffic)


buffer_sp_traffic <- rgeos::gBuffer(sp_traffic, width=0.00015)  #0.00005
plot(sp_traffic)
plot(buffer_sp_traffic, add = TRUE)
gI <- gIntersection(OSM_GIB, buffer_sp_traffic,  byid=c(TRUE, TRUE))
# check_intersect_points <- rgeos::gCrosses(OSM_GIB, buffer_sp_traffic,  byid=c(TRUE, TRUE))
plot(gI)
plot(gI, col=1:4, lwd=2, add=TRUE)

#######################################################################################

MAX_speed <- max(traffic$averagespeed)+0.5  
MIN_speed <- min(traffic$averagespeed)-0.5

pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed),na.color = "transparent")

# define popup for the iteractive map
popup_speed <- paste0("<p>Avg Speed", ": <strong> ", traffic$averagespeed, " </strong>km/h")


# make a quick leaflet map with site loaction only

# Marker + Static Label using custom label options
map <- leaflet(data = traffic[,]) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
#                    weight = 1, radius=5, color = 'black',
#                    stroke = FALSE, fillOpacity = 1,
                   popup = popup_speed,
                   weight = 3, radius= 4, 
                   color = pal_speed(traffic$averagespeed), stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(traffic$abnormaltraffictype),
                   labelOptions = labelOptions(noHide = F),
                   group = "traffic") %>%
  addPolylines(data = gI, color='red', group='Route',
               label = as.character(traffic$abnormaltraffictype),
               labelOptions = labelOptions(noHide = F)) %>%
  addLegend("bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
    title = paste("<strong>Avg Speed Km/h:"),
    labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  overlayGroups = c("traffic", "Route"),
  options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("Route"))

map






## create a series of .png figures for each hour
# saveWidget(map, 'gibraltar_TomTom_Stationary.html', selfcontained = FALSE)


###################### end #############################################################
########################################################################################
