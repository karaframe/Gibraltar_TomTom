
library(shinydashboard)
library(shiny)
library(maptools)
library(threadr) # load this package before dplyr
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)
library(lubridate)
library(BH)


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Gibraltar TomTom"),
  dashboardSidebar(
    "Data are from 12 April 2016 to 11 May 2016",
    sidebarMenu(
      menuItem(
        "Real time data", tabName = "Real", icon = icon("dashboard")
      ),
      menuItem(
        "Hourly averaged data", tabName = "Averaged", icon = icon("th")
      )
    )
  ),
  
  
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "Averaged",
            fluidRow(
              box(
                h1("Traffic flow data: real time data from 12 April to 11 May 2016"),
                imageOutput("myImage_traffic_flow"), height = 800, width = 12
              )
              #   tags$audio(src ="test_audio.mp3",type ="audio/mp3",autoplay = NA, controls = NA)
              #  box(imageOutput("myImage_traffic_data"),height = 500, width = 5)
            ),
            fluidRow(
              box(
                h1("Traffic data: stationary, queueing, slow Traffic"), imageOutput("myImage_traffic_data"),height = 800, width = 12
              )
            )),
    
    # Second tab content
    tabItem(tabName = "Real",
            
            
            fluidRow(
              box(
                h1("Traffic flow data: real time data on 05 May 2016 (every 15 min.)"), leafletOutput('Realtime_Traffic', height = 680 , width = 950),
                height = 800, width = 12
              )
            ),
            
            
            fluidRow(
              box(
                h1("Traffic data: stationary, queueing, slow Traffic (all data)"), leafletOutput('Stationary_Traffic', height = 680 , width = 950),
                height = 800, width = 12
              )
            ))
    
  ))
)
