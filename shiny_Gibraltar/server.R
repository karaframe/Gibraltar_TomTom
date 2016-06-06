
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
library(sp)
library(ggplot2)

options(warn=-1)  # warnings OFF!

# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR/shiny_Gibraltar")
traffic_stationary <- read.csv("traffic_stationary_until_19May2016.csv")
traffic_realtime <- read.csv("traffic_realtime_18_19May2016.csv")

# Load OpenStreed data (GeoJSON)
OSM_GIB <- readOGR(".GIB_geojson_Open_Street", "OGRGeoJSON")

###############################################################################
# stationary traffic------------------------

# remove duplicates of lon & lat and sort latitude from big to small
traffic_stationary <- traffic_stationary %>%
  distinct(lon) 
# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic_stationary <- traffic_stationary[traffic_stationary$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic_stationary <- traffic_stationary[traffic_stationary$lat!=36.1560165880387,]   
traffic_stationary <- traffic_stationary[traffic_stationary$averagespeed!=0,] 


# make a spatial dataframe with traffic data 
sp_traffic_staz <- SpatialPointsDataFrame(traffic_stationary[,40:41], traffic_stationary,            # lat, lon
                                     proj4string=CRS("+init=epsg:4326")) 
# plot(sp_traffic_staz)

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
traffic_realtime <- traffic_realtime[traffic_realtime$lat!=36.147002876331,]  
traffic_realtime <- traffic_realtime[traffic_realtime$averagespeed!=0,] 

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

# plot(sp_traffic_real)
# plot(sp_traffic_staz, add=TRUE, lwd=1, col = "red")

buffer_sp_traffic_real <- rgeos::gBuffer(sp_traffic_real, width=0.00015)  #0.00005
gI_real <- gIntersection(OSM_GIB, buffer_sp_traffic_real,  byid=c(TRUE, TRUE))
# plot(gI_real)

# intersect the stationary traffic with the Real time traffic flow
# change the buffer size if needed in order to allow better intersection
gI_real_staz <- gIntersection(sp_traffic_real, buffer_sp_traffic_staz,  byid=c(TRUE, TRUE))
# plot(gI_real_staz)
Intersect_points <- as.data.frame(gI_real_staz)
n_Intersect_points <- nrow(Intersect_points)
Stationary_fraction <- (n_Intersect_points / nrow(sp_traffic_real)) *100
Stationary_fraction <- round(Stationary_fraction,1)
# Intersect_points <- fortify(gI_real_staz)[, 1:2]
# plot(Intersect_points)


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

      height = 900,
      width = 950
    ))
    
  }, deleteFile = FALSE)
  
  
  
# add leaflet map for traffic data (stationary)  all data from 12 Aprli to 11 May 2016
  
MIN_speed_STZ <- min(traffic_stationary$averagespeed)-0.5
MAX_speed_STZ <- max(traffic_stationary$averagespeed)+0.5  

pal_speed_STZ <- colorNumeric(c("#0000ff", "#ff0000"),
                        c(MIN_speed_STZ, MAX_speed_STZ),na.color = "transparent")
  
# define popup for the iteractive map
popup_speed_STZ <- paste0("<p>Avg Speed", ": <strong> ", traffic_stationary$averagespeed, " </strong>km/h")
  
  Map_Stationary <- reactive({
    map <- leaflet(data = traffic_stationary[,]) %>%
      setView(-5.35, 36.135, 14) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
      addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, 
                       popup = popup_speed_STZ,
                       weight = 3, radius= 4, 
                       color = pal_speed_STZ(traffic_stationary$averagespeed), stroke = FALSE, fillOpacity = 1,
                       label = ~as.character(traffic_stationary$abnormaltraffictype),
                       labelOptions = labelOptions(noHide = F),
                       group = "Traffic Data") %>%
      addPolylines(data = gI_staz, color='blue', group="intersects", weight = 2,
                   label = as.character(traffic_stationary$abnormaltraffictype),
                   labelOptions = labelOptions(noHide = F)) %>%
      addLegend("bottomright", pal = pal_speed_STZ, values = c(MIN_speed_STZ, MAX_speed_STZ),
                title = paste("<strong>Avg Speed Km/h:"),
                labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
      addLayersControl(
        baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
        overlayGroups = c("Traffic Data", "intersects"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("intersects"))
    
    map
    
  })
  

  
# add leaflet map for traffic data (stationary)  
    output$Stationary_Traffic = renderLeaflet(Map_Stationary())
    
    
    
    
# add leaflet map for traffic flow data (real time)  all data on 05 may 2016 (every 15 minutes)
    
MAX_speed <- max(traffic_realtime$averagespeed)+0.5  
MIN_speed <- min(traffic_realtime$averagespeed)-0.5
    
pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed),na.color = "transparent")
    
# define popup for the iteractive map
popup_speed <- paste0("<p>Avg Speed", ": <strong> ", traffic_realtime$averagespeed, " </strong>km/h")
    
    
    Map_Realtime <- reactive({

      map <- leaflet(data = traffic_realtime[,]) %>%
        setView(-5.355, 36.150, 16) %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
        addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
        addCircleMarkers(lng = ~lon, lat = ~lat, 
                         # weight = 1, radius=5, color = 'black',
                         # stroke = FALSE, fillOpacity = 1,
                         popup = popup_speed,
                         weight = 3, radius= 4, 
                         color = pal_speed(traffic_realtime$averagespeed), stroke = FALSE, fillOpacity = 1,
                         label = ~as.character(traffic_realtime$informationstatus),
                         labelOptions = labelOptions(noHide = F),
                         group = "Traffic Flow") %>%
        addPolylines(data = gI_real, color="blue", group='intersects', weight = 2,
                     label = as.character(traffic_realtime$informationstatus),
                     labelOptions = labelOptions(noHide = F)) %>%
        addLegend("bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
          title = paste("<strong>Avg Speed Km/h:"),
          labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = c("Traffic Flow", "intersects"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("intersects"))
      
      map
      
    })
    
    
    
    # add leaflet map for traffic data (stationary)  
    output$Realtime_Traffic = renderLeaflet(Map_Realtime())    
    
    
    
    output$intersects <- renderInfoBox({
      infoBox(
        "Traffic Nodes", n_Intersect_points, icon = icon("list"),
        color = "purple"
      )
    })
    

    
      output$percentage <- renderInfoBox({

        
        if(input$goButton==0)
        return(
          infoBox(           
          "----", isolate({ paste0(" ", "%") }) , icon = icon("list"),
          color = "green"
        ))
      
      
      infoBox(           
     #  input$goButton,
        "Stationary Traffic",  isolate({ paste0(Stationary_fraction, "%") }) , icon = icon("list"),
        color = "purple"
      )
    })
        
    
    
    
})