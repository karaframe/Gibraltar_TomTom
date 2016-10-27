
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
library(spatstat)

options(warn=-1) # warnings OFF
# options(warn=0)  # warnings back ON!

# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR")
setwd("C:/PostgreSQL_Gibraltair/OpenLR/new_data")

# load traffic data until 11 May 2016
# these are stationary Traffic & queueing Traffic & low Traffic
# traffic <- read.csv("traffic_data_FK_until_11_May_2016.csv")  
# traffic <- read.csv("traffic_data_FK_until_19_May_2016.csv") 
traffic <- read.csv("traffic_data_FK_14_15_June_2016.csv")
# rename "elaborateddataid" into "ID"
names(traffic)[names(traffic) == 'situationid'] <- 'ID'


# load shp file for open street data (routes)
# dir <-  "C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR"
# dir <-  "C:/PostgreSQL_Gibraltair/OpenLR"

# OSM_GIB <- readOGR(dsn = dir, layer = "gibraltar_OSM")
# plot(OSM_GIB)

#### Write GeoJSON file for OSM_GIB ############################
# ----- Write data to GeoJSON

# dat <-paste(dir, "/",  ".GIB_geojson_Open_Street", sep="") 

####  ATT !!!!! erase existing .geojson file when re-runing code ######
# writeOGR(OSM_GIB, dat, layer="", driver="GeoJSON")  

# read GeoJSON-------------------------------------------------------------
OSM_GIB <- readOGR(".GIB_geojson_Open_Street", "OGRGeoJSON")
plot(OSM_GIB)

###################################################################################################

POINTS_1st <- read.csv("points_traffic_data_1st_14_15_June_2016.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_data_2nd_14_15_June_2016.csv")[2:4]

# Join traffic data (speed) first and second point
traffic_1st <- traffic %>%
  left_join (POINTS_1st, "ID")

traffic_2nd <- traffic %>%
  left_join (POINTS_2nd, "ID")

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)
write.csv(traffic, "traffic_stationary_14_15_June_2016.csv")
traffic <- read.csv("traffic_stationary_14_15_June_2016.csv")


# remove duplicates of lon & lat and sort latitude from big to small
traffic <- traffic %>%
  distinct(lon, lat, .keep_all = TRUE) 
# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic <- traffic[traffic$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic <- traffic[traffic$lat!=36.1560165880387,]   
traffic <- traffic[traffic$averagespeed!=0,] 


#########################################################################################

# cut routes between points--------------------------------------------------

# make a spatial dataframe with traffic data 
sp_traffic <- SpatialPointsDataFrame(traffic[,39:40], traffic,            # lat, lon
                                     proj4string=CRS("+init=epsg:4326")) 
plot(sp_traffic)

# build buffer around each point
buffer_sp_traffic <- rgeos::gBuffer(sp_traffic, width=0.00015)  #0.00005
plot(sp_traffic)
plot(buffer_sp_traffic, add = TRUE)
# intersect traffic points with buffer
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
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
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
                   group = "Traffic Data") %>%
  addPolylines(data = gI, color='blue', group='Route', weight = 2,
               label = as.character(traffic$abnormaltraffictype),
               labelOptions = labelOptions(noHide = F)) %>%
  addLegend("bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
    title = paste("<strong>Avg Speed Km/h:"),
    labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
addLayersControl(
  baseGroups = c("Hydda_Full","Road map", "Topographical", "Satellite", "Toner Lite"),
  overlayGroups = c("Traffic Data", "Route"),
  options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("Route"))

map






## create a series of .png figures for each hour
# saveWidget(map, 'gibraltar_TomTom_Stationary.html', selfcontained = FALSE)


###################### end #############################################################
########################################################################################
