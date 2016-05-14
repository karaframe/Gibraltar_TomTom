
library(shiny)
library(maptools)
library(threadr) # load this package before dplyr
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)
library(shinydashboard)
library(BH)
library(maptools)

options(warn=-1)  # warnings OFF!

traffic_stationary <- read.csv("traffic_stationary_until_11May2016.csv")
traffic_realtime <- read.csv("traffic_realtime_05May2016.csv")

# Load OpenStreed data (GeoJSON)
OSM_GIB <- readOGR(".GIB_geojson_Open_Street", "OGRGeoJSON")

###############################################################################
# stationary traffic------------------------

# make a spatial dataframe with traffic data 
sp_traffic_staz <- SpatialPointsDataFrame(traffic_stationary[,40:41], traffic_stationary,            # lat, lon
                                     proj4string=CRS("+init=epsg:4326")) 

buffer_sp_traffic_staz <- rgeos::gBuffer(sp_traffic_staz, width=0.00015)  #0.00005
gI_staz <- gIntersection(OSM_GIB, buffer_sp_traffic_staz,  byid=c(TRUE, TRUE))


#################################################################################
# Real time traffic flow--------------------

# remove duplicates of lon & lat and sort latitude from big to small
traffic_realtime <- traffic_realtime %>%
  distinct(lat) 
# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic_realtime <- traffic_realtime[traffic_realtime$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic_realtime <- traffic_realtime[traffic_realtime$lat!=36.1560165880387,]   


traffic_realtime <- traffic_realtime %>%
  arrange(lat) %>%
  arrange(lon)

# make lines
p1 = Line(traffic_realtime[,22:23]) #lon & lat
# make Polygon class
p2 = Lines(list(p1), ID = "drivetime")
# make spatial polygons class
traffic_lines = SpatialLines(list(p2),proj4string=CRS("+init=epsg:4326"))


# make a spatial dataframe with traffic data 
sp_traffic_real <- SpatialPointsDataFrame(traffic_realtime[,22:23], traffic_realtime,     # lat, lon
                                     proj4string=CRS("+init=epsg:4326")) 


buffer_sp_traffic_real <- rgeos::gBuffer(sp_traffic_real, width=0.00015)  #0.00005
gI_real <- gIntersection(OSM_GIB, buffer_sp_traffic_real,  byid=c(TRUE, TRUE))


#### shiny! #####################################################################


shinyServer(function(input, output, session) {
  
# add first animated image
  
  output$myImage_traffic_flow <- renderImage({
  
  # image_file <- "C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR/hours/traffic_Gibraltar_Tom_Tom.gif"
  image_file <- "traffic_flow_Gibraltar_TomTom.gif"
  

  return(list(
    src = image_file,
    filetype = "image/gif",

#     height = 520,
#     width = 696
#     height = 580,
#     width = 750
#     height = 680,
#     width = 950
height = 680,
width = 950
  ))
  
}, deleteFile = FALSE)
  
  
  
# add second animated image
  
  output$myImage_traffic_data <- renderImage({
    
    image_file <- "traffic_data_Gibraltar_TomTom.gif"
    
    
    return(list(
      src = image_file,
      filetype = "image/gif",

      height = 680,
      width = 950
    ))
    
  }, deleteFile = FALSE)
  
  
  
# add leaflet map for traffic data (stationary)  all data from 12 Aprli to 11 May 2016
  
  Map_Stationary <- reactive({
    
    map <- leaflet(data = traffic_stationary[,]) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
      addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, 
                       weight = 1, radius=5, color = 'black',
                       stroke = FALSE, fillOpacity = 1,
                       label = ~as.character(traffic_stationary$abnormaltraffictype),
                       labelOptions = labelOptions(noHide = F),
                       group = "traffic") %>%
      addPolylines(data = gI_staz, color='red', group='Route',
                   label = as.character(traffic_stationary$abnormaltraffictype),
                   labelOptions = labelOptions(noHide = F)) %>%
      addLayersControl(
        baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
        overlayGroups = c("Route", "traffic"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("traffic"))
    
    map
  })
  

  
# add leaflet map for traffic data (stationary)  
    output$Stationary_Traffic = renderLeaflet(Map_Stationary())
    
    
    
    
# add leaflet map for traffic flow data (real time)  all data on 05 may 2016 (every 15 minutes)
    
    Map_Realtime <- reactive({
      
      map <- leaflet(data = traffic_realtime[,]) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
        addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
        addCircleMarkers(lng = ~lon, lat = ~lat, 
                         weight = 1, radius=5, color = 'black',
                         stroke = FALSE, fillOpacity = 1,
                         label = as.character(traffic_realtime$averagespeed),
                         labelOptions = labelOptions(noHide = F),
                         group = "speed (km/h)") %>%
 #       addPolylines(data = traffic_lines, color='blue', weight = 2, group='Route') %>%
        addPolylines(data = gI_real, color='blue', group='Nodes',
                     label = as.character(traffic_realtime$informationstatus),
                     labelOptions = labelOptions(noHide = F)) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = c("speed (km/h)", "Nodes"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("speed (km/h)"))
      map
    })
    
    
    
    # add leaflet map for traffic data (stationary)  
    output$Realtime_Traffic = renderLeaflet(Map_Realtime())    
  
})