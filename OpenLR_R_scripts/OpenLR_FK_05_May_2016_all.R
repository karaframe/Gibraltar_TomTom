
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

# all data for 05 may 2016
# traffic <- read.csv("traffic_flow_5_May_2016.csv") 
traffic <- read.csv("traffic_flow_FK_18May2016_5pm.csv")

#########################################################################################
#########################################################################################

# POINTS_1st <- read.csv("points_traffic_flow_1st_05_May.csv")[2:4]
# POINTS_2nd <- read.csv("points_traffic_flow_2nd_05_May.csv")[2:4]

POINTS_1st <- read.csv("points_traffic_flow_1st_18May2016_5pm.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_flow_2nd_18May2016_5pm.csv")[2:4]


# Join traffic data (speed) to spatial data
traffic_1st <- cbind(traffic,POINTS_1st)
traffic_2nd <- cbind(traffic,POINTS_2nd)

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)
write.csv(traffic, "traffic_realtime_05May2016.csv")
traffic <- read.csv("traffic_realtime_05May2016.csv")


# pair of points
# POINTS <- read.csv("points_traffic_flow_05_May.csv")
# Join traffic data (speed) to spatial data
# traffic <- cbind(traffic,POINTS)


# remove duplicates of lon & lat and sort latitude from big to small
traffic <- traffic %>%
  distinct(lat) 
# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic <- traffic[traffic$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic <- traffic[traffic$lat!=36.1560165880387,]   

 traffic <- traffic %>%
   arrange(lat) %>%
   arrange(lon)

# load open street data (routes)
#  dir <-  "C:/RICARDO-AEA/postgreSQL_Gibraltair"
#  OSM_GIB <- readOGR(dsn = dir, layer = "gibraltar_OSM")
# read GeoJSON-------------------------------------------------------------
OSM_GIB <- readOGR(".GIB_geojson_Open_Street", "OGRGeoJSON")
plot(OSM_GIB)
  
# # make polygon
# p1 = Polygon(traffic[,22:23]) #lon & lat
# # make Polygon class
# p2 = Polygons(list(p1), ID = "drivetime")
# # make spatial polygons class
# p3 = SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))
# plot(p3)
 
 
# make a spatial dataframe with traffic data 
sp_traffic <- SpatialPointsDataFrame(traffic[,22:23], traffic,            # lat, lon
                                            proj4string=CRS("+init=epsg:4326")) 
plot(sp_traffic)
  

# make lines
p1 = Line(traffic[,22:23]) #lon & lat
# make Polygon class
p2 = Lines(list(p1), ID = "drivetime")
# make spatial polygons class
traffic_lines = SpatialLines(list(p2),proj4string=CRS("+init=epsg:4326"))
plot(traffic_lines)


#########################################################################################
# cut routes between points--------------------------

# gI <- rgeos::gIntersection(OSM_GIB, traffic_lines, byid=c(TRUE, TRUE))
# plot(gI)
# plot(gI, col=1:4, lwd=2, add=TRUE)
# row.names(gI)
# Inter <- as.data.frame(gI)
# colnames(Inter) <- c("Lon", "Lat")


buffer_sp_traffic <- rgeos::gBuffer(sp_traffic, width=0.00015)  #0.00005
plot(sp_traffic)
plot(buffer_sp_traffic, add = TRUE)
gI <- gIntersection(OSM_GIB, buffer_sp_traffic,  byid=c(TRUE, TRUE))
# check_intersect_points <- rgeos::gCrosses(OSM_GIB, buffer_sp_traffic,  byid=c(TRUE, TRUE))
plot(gI)
plot(gI, col=1:4, lwd=2, add=TRUE)


#########################################################################################

# make a quick leaflet map with site loaction only

# Marker + lines + Static Label using custom label options
map <- leaflet(data = traffic[,]) %>%
  setView(-5.35, 36.150, 16) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   weight = 1, radius=5, color = 'black',
                   stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(traffic$publicationtime),
                   labelOptions = labelOptions(noHide = F),
                   group = "speed (km/h)") %>%
  addPolylines(data = traffic_lines, color='blue', group='Route') %>%
  addPolylines(data = OSM_GIB , color='blue', group='OSM', weight = 1) %>%
#   addCircleMarkers(lng = Inter$Lon, lat = Inter$Lat, 
#                    weight = 1, radius=5, color = 'black',
#                    stroke = FALSE, fillOpacity = 1,
#                    group = "intersects") %>%
  addPolylines(data = gI, color="blue", group='intersects',
               label = as.character(traffic$informationstatus),
               labelOptions = labelOptions(noHide = F)) %>%
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("speed (km/h)", "Route", "OSM", "intersects"),
    options = layersControlOptions(collapsed = TRUE)) %>%
    hideGroup(c("OSM", "speed (km/h)", "Route"))

map




