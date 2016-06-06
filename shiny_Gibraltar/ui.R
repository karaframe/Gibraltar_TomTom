
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
    " ",
    sidebarMenu(
      menuItem(
        "Traffic Flow (speed)", tabName = "Real", icon = icon("dashboard")
      ),
      menuItem(
        "Traffic Data (speed)", tabName = "Stationary", icon = icon("dashboard")
      ),
#       menuItem(
#         "Hourly averaged data", tabName = "Averaged", icon = icon("th")
#       ),
      menuItem(
        "Hourly Traffic Flow", tabName = "Animated_Traffic_Flow", icon = icon("car")
      ),
      menuItem(
        "Hourly Traffic Data", tabName = "Animated_Traffic_Data", icon = icon("car")
      )
    )
  ),
  
  
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "Animated_Traffic_Flow",
            
            fluidRow(
              box(
                h1("Traffic flow data: real time data from 7 to 8 May 2016 (animation)"),
                imageOutput("myImage_traffic_flow"), height = 800, width = 12
              )
              #   tags$audio(src ="test_audio.mp3",type ="audio/mp3",autoplay = NA, controls = NA)
              #  box(imageOutput("myImage_traffic_data"),height = 500, width = 5)
            )
#             fluidRow(
#               box(
#                 h1("Traffic data: stationary, queueing, slow Traffic"), imageOutput("myImage_traffic_data"),height = 800, width = 12
#               ))
             ),
    
    
    # Second tab content 
    tabItem(tabName = "Animated_Traffic_Data",
            
            fluidRow(
              box(
                h1("Traffic data: stationary, queueing, slow Traffic (animation)"), imageOutput("myImage_traffic_data"),
                height = 1200, width = 12
              )
            )),
    
    
    
    # Third tab content
    tabItem(tabName = "Real",
            
            fluidRow(
              box(
                h1("Traffic flow data: real time data 18-19 May 2016 (1 min. resolution)"), leafletOutput('Realtime_Traffic', height = 680 , width = 950),
                height = 800, width = 12
              )
            ),


            fluidRow(
             # actionButton("goButton", tags$b("Calculate percentage of Stationary Traffic"))
              box(width = 4, actionButton("goButton", tags$h4(tags$b("Percentage of Stationary Traffic (click here)"))))
            ),
            
            fluidRow(
              infoBoxOutput("percentage", width = 2),
              infoBoxOutput("intersects", width = 2)
            )
    ),


            
#             fluidRow(
#               box(
#                 h1("Traffic data: stationary, queueing, slow Traffic (all data)"), leafletOutput('Stationary_Traffic', height = 680 , width = 950),
#                 height = 800, width = 12
#               )
#             )),
    
    
    # Fourth tab content
    tabItem(tabName = "Stationary",
            
            fluidRow(
              box(
                h1("Traffic data: stationary, queueing, slow Traffic (all data)"), leafletOutput('Stationary_Traffic', height = 680 , width = 950),
                height = 800, width = 12
              )
            ))

    
  ))
)
