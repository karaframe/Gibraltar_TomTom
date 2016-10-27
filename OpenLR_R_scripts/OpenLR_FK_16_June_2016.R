
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
# library(spatstat)
# library(gissr)
library(rgdal)
library(sp)

# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR/new_data")
setwd("C:/PostgreSQL_Gibraltair/OpenLR/new_data")  # higher spatial (street) coverage

' load data'
# traffic <- read.csv("traffic_flow_FK_16_June_2016.csv")
traffic <- read.csv("traffic_flow_FK_14_15_June_2016.csv")
# rename "elaborateddataid" into "ID"
names(traffic)[names(traffic) == 'elaborateddataid'] <- 'ID'

#########################################################################################
#########################################################################################

POINTS_1st <- read.csv("points_traffic_flow_1st_14_15_June2016.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_flow_2nd_14_15_June2016.csv")[2:4]

# reverse orders of rows in Traffic
traffic <-  traffic[nrow(traffic):1,]

# traffic <- traffic %>%
#   dplyr:: select(ID,
#          freeflowspeed,
#          averagespeed)

# !!!!!!!!!!!!!! revert ID in POINTS 
traffic_1st <- cbind(traffic, POINTS_1st)
traffic_2nd <- cbind(traffic, POINTS_2nd)

# # Join traffic data (speed) first and second point
# traffic_1st <- traffic %>%
#   left_join (POINTS_1st, "ID")
# 
# traffic_2nd <- traffic %>%
#   left_join (POINTS_2nd, "ID")

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)

write.csv(traffic, "traffic_realtime_15_16_June_2016.csv")
traffic <- read.csv("traffic_realtime_15_16_June_2016.csv")

# remove rows containing NA values in the averageSpeed column
# traffic <- traffic[!is.na(traffic$averagespeed),]

# remove duplicates of lon & lat and sort latitude from big to small
traffic <- traffic %>%
distinct(lat, lon, .keep_all = TRUE)


# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic <- traffic[traffic$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic <- traffic[traffic$lat!=36.1560165880387,]   
traffic <- traffic[traffic$lat!=36.147002876331,]  
# traffic <- traffic[traffic$averagespeed!=0,] 
# traffic$ID <- 1:nrow(traffic)


################################################################################
# calcualte nearest neighbours distances-------------------------------------

# traffic_dist <- cbind(traffic$lon, traffic$lat)
# traffic_dist <- as.data.frame(traffic_dist)
# colnames(traffic_dist) <- c("lon", "lat")
# 
# # make a spatial object
# coordinates(traffic_dist) <- ~lon+lat
# 
# # define CRS
# proj4string(traffic_dist) <- CRS("+proj=longlat +ellps=WGS84") 
# 
# 
# # Transform projection system into a meterinng system
# epsg.2062 <- "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs"
# wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# traffic_meters <- spTransform(traffic_dist,CRS(epsg.2062))
# # traffic_meters <- as.data.frame(traffic_meter)
# 
# 
# # Calculate pairwise distances between points--------------------------------------------
# # d <- gDistance(traffic_dist, byid=T) # distance in degrees
# d <- gDistance(traffic_meters, byid=T) # distance in meters
# 
# # Find second shortest distance (closest distance is of point to itself and therefore,
# # use second shortest)
# 
# min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
# 
# # Construct new data frame with desired variables
# traffic_distance <- cbind(as.character(traffic$ID), traffic_meters@coords[,1],
#                                                     traffic_meters@coords[,2])
# traffic_distance <- as.data.frame(traffic_distance)
# colnames(traffic_distance) <- c("ID_dist", "x_dist", "y_dist")
# # newdata <- cbind(traffic, traffic[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
# # colnames(newdata) <- c(colnames(mydata), 'neighbor', 'n.lat', 'n.long', 'n.area', 'n.canopy', 'n.avg.depth', 'distance')
# traffic_new <- cbind(traffic, traffic_distance[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
# colnames(traffic_new)[25] <- "distance"

# # first nearest neighbours
# distance_1 <- nndist(traffic_dist)
# # second nearest neighbours
# distance_2 <- nndist(traffic_dist, k = 2)
# traffic <- cbind(traffic, distance_1, distance_2)
# 
# 
# # Distances of all traffic points between each other (in meters)
# for (i in 1:nrow(traffic)) {
# traffic$distance <- distance_by_haversine(
# traffic$lat, traffic$lon, traffic$lat[i], traffic$lon[i], "m")
# }




# load open street data (routes)
#  dir <-  "C:/RICARDO-AEA/postgreSQL_Gibraltair"
#  OSM_GIB <- readOGR(dsn = dir, layer = "gibraltar_OSM")
# read GeoJSON-------------------------------------------------------------
OSM_GIB <- readOGR(".GIB_geojson_Open_Street", "OGRGeoJSON")
plot(OSM_GIB)
  

# make a spatial dataframe with traffic data 
sp_traffic <- SpatialPointsDataFrame(traffic[,22:23], traffic,            # lat, lon
                                            proj4string=CRS("+init=epsg:4326")) 
plot(sp_traffic)
  

#########################################################################################
# cut routes between points--------------------------

# transform lat lon coordinated into PROJECTED COORDINATES ED50 / UTM zone 30N in METERS
pc <- spTransform(sp_traffic, CRS( "+init=epsg:23030")) 
# non overlapping buffers
# buffer_sp_traffic <- rgeos::gBuffer(pc, width=70, byid = TRUE)  # metre
# overlapping buffers
buffer_sp_traffic <- rgeos::gBuffer(pc, width=20, byid = FALSE)  # metre (10m)

plot(buffer_sp_traffic)
# count points inside each overlapping buffer
union_buffer <- over(pc, disaggregate(buffer_sp_traffic))
union_buffer <- as.data.frame(union_buffer)
colnames(union_buffer) <- "polygon"

# group and find the total number of cars inside polygons (from intersecting buffers)
union_buffer$count <- 1
union_buffer <- union_buffer %>%
  group_by(polygon) %>%
  summarise(sum_cars = sum(count))

# select polygons based on counts.......

# reproject in WGS84
buffer_sp_traffic <- spTransform(buffer_sp_traffic, CRS("+init=epsg:4326")) 
plot(sp_traffic)
plot(buffer_sp_traffic, add = TRUE)


# make intersection between open street and the buffer----------------------
# reproject Open Street Road Map for Gibraltar
OSM_GIB <- spTransform(OSM_GIB, CRS( "+init=epsg:4326")) 
gI <- gIntersection(OSM_GIB, buffer_sp_traffic,  byid=c(TRUE, TRUE))
# check_intersect_points <- rgeos::gCrosses(OSM_GIB, buffer_sp_traffic,  byid=c(TRUE, TRUE))
plot(gI)
plot(gI, col=1:4, lwd=2, add=TRUE)



#########################################################################################

# make a quick leaflet map with site loaction only


# MAX_speed <- max(traffic$averagespeed)+0.5  
# MIN_speed <- min(traffic$averagespeed)-0.5

MAX_speed <- max(traffic$freeflowspeed)+0.5  
MIN_speed <- min(traffic$freeflowspeed)-0.5

pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed),na.color = "transparent")

# define popup for the iteractive map
# popup_speed <- paste0("<p>Avg Speed", ": <strong> ", traffic$averagespeed, " </strong>km/h")
popup_speed <- paste0("<p>Flow Speed", ": <strong> ", traffic$freeflowspeed, " </strong>(km/h)")


# Marker + lines + Static Label using custom label options
map <- leaflet(data = traffic[,]) %>%
#  setView(-5.355, 36.150, 16) %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                  # weight = 1, radius=5, color = 'black',
                  # stroke = FALSE, fillOpacity = 1,
                   popup = popup_speed,
                   weight = 3, radius= 4, 
                   color = pal_speed(traffic$freeflowspeed), stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(traffic$informationstatus),
                #  label = ~as.character( format(round(traffic$ID, 2), nsmall = 2) ),
                  labelOptions = labelOptions(noHide = F),
                   group = "Traffic Flow") %>%
  addPolylines(data = OSM_GIB , color='blue', group='OSM', weight = 1) %>%
  addPolylines(data = gI, color="blue", group='intersects', weight = 2,
               label = as.character(traffic$informationstatus),
               labelOptions = labelOptions(noHide = F)) %>%
  addLegend(
    "bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
    title = paste("<strong>Flow Speed Km/h:"),
    labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Hydda_Full", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("OSM", "Traffic Flow", "intersects"),
    options = layersControlOptions(collapsed = TRUE)) %>%
    hideGroup(c("OSM","intersects"))

map

saveWidget(map, 'TomTom_14_15_June_Traffic_Flow_free_speed.html', selfcontained = FALSE)

