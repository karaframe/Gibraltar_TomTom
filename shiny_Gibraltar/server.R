
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

traffic_stationary <- read.csv("traffic_stationary_11May2016.csv")
traffic_realtime <- read.csv("traffic_realtime_05May2016.csv")


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
                       label = ~as.character(traffic_stationary$averagespeed),
                       labelOptions = labelOptions(noHide = F),
                       group = "speed (km/h)") %>%
      addLayersControl(
        baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
        overlayGroups = c("speed (km/h)"),
        options = layersControlOptions(collapsed = FALSE)) 
    
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
                         label = ~as.character(traffic_realtime$averagespeed),
                         labelOptions = labelOptions(noHide = F),
                         group = "speed (km/h)") %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = c("speed (km/h)"),
          options = layersControlOptions(collapsed = FALSE)) 
      
      map
    })
    
    
    
    # add leaflet map for traffic data (stationary)  
    output$Realtime_Traffic = renderLeaflet(Map_Realtime())    
  
})