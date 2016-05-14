
library(maptools)
library(threadr) # load this package befroe dplyr
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

# name <- "C/wybxm1wwsnBQAaAEsLDg=="
# name <-  C/wxHRmzsSOCBQCKAPMjVW0=
# name <-  C/wybxm1wwsnBQAaAEsLDg==
# name <-  C/wyRRm07AsgDwB2AhgLbhoX
# name <- C/wyRRm07AsgDwB2AhgLbhoX

# Load data previously downloaded from PostgreSQL DB (1-2 May 2016)
# traffic <- read.csv("traffic_data.csv")
traffic <- read.csv("traffic_flow_FK.csv")
head(traffic)

# traffic <- filter(traffic, traveltime == 0)  # filter traffic data with travel time ==0
traffic <- traffic[traffic$traveltime!=0,]   # remove traffic data with travel time == 0
# traffic <- traffic[1:50,]

########################################################################

# truncate date
traffic$time <- str_sub(traffic$publicationtime, start = 1, end = -10)

# group data by hour
# create a new field with the hour (use package threadr)



# traffic <- traffic %>%
#   mutate(date = ymd_hms(publicationtime, tz = "UTC"),
#          hour = hour(date),
#          date = date)%>% 
#   group_by(elaborateddataid,
#            openlrbinary,
#            hour,
#            time) %>%
#   summarise(AVG_Speed = mean(averagespeed, na.rm = TRUE))%>%
#   ungroup() %>%
#   arrange(hour) 



traffic <- traffic %>%
  mutate(date = ymd_hms(publicationtime, tz = "UTC"),
         hour = hour(date),
         date = date)%>% 
  group_by(elaborateddataid,
           openlrbinary,
           hour) %>%
  summarise(AVG_Speed = mean(averagespeed, na.rm = TRUE))%>%
  ungroup() %>%
  arrange(hour) 


############################################################################ 
############################################################################

# RUN this only if you are using direct connection with the Ricardo's servers
# merge traffic data with Air Quality Data in Gibraltar
# import Air Quality data from Gibraltar database

# Helpers
print_database_names()
print_statistic_types()

info_sites_GIB <- search_database("gibraltar")    ### metadata
# info_sites <- search_database("gibraltar", "V10|V25")    ### metadata
info_sites_GIB <- search_database("gibraltar", "no2")    ### metadata
# info_sites <- subset(info_sites, variable %in% c("V10", "V25")) ### metadata
write.csv(info_sites_GIB, "info_sites_GIB.csv")
info_sites_GIB <- subset(info_sites_GIB, variable %in% "no2") ### metadata
info_sites_GIB <- info_sites_GIB[!(is.na(info_sites_GIB$longitude)), ]

# Make a site vector to use in importr functions
site_vector <- unique(info_sites_GIB$site)

# setup start_date that is one week before the current date
week_before <- Sys.time()-604800  # 60 seconds * 60 minutes * 24 hours * 7 days = 1 week
week_before <- as.character(week_before)
week_before <- str_sub(week_before, start = 1, end = -10) # read only 10 characters (the date)

# stats_NO2 <- import_stats("gibraltar",site = site_vector, 
#                           variable = c("no2"),
#                           # start = "2016-01-01",
#                           start = week_before,
#                           # end = "2016-03-31",
#                           statistic = "daily_mean",
#                           extra = TRUE)


NO2_GIB <- import_measures("gibraltar",site = site_vector, 
                           variable = "no2",
                           start = week_before,
                           extra = FALSE)

write.csv(NO2_GIB, "NO2_GIB_data.csv")

##############################################################################
##############################################################################

info_sites_GIB <- read.csv("info_sites_GIB.csv")
info_sites_GIB <- subset(info_sites_GIB, variable %in% "no2") ### metadata
# remove empty lines
info_sites_GIB <- info_sites_GIB[!(is.na(info_sites_GIB$longitude)), ]

# filter traffic data with travel time ==0 # retain only measurement
info_sites_GIB <- filter(info_sites_GIB, data_table == "measurement")  
info_sites_GIB <- info_sites_GIB %>% 
  mutate(site_name = str_replace_all(site_name, " ", "_"))

NO2_GIB <- read.csv("NO2_GIB_data.csv")
NO2_GIB <- NO2_GIB %>% 
  mutate(site_name = str_replace_all(site_name, " ", "_"))

# remove empty lines
NO2_GIB <- NO2_GIB[!(is.na(NO2_GIB$date)), ]
# truncate date
NO2_GIB$time <- str_sub(NO2_GIB$date, start = 1, end = -10)

# keep only the range of date as the one for the traffic flow used for TomTom data
# filter only 1-2 may data
NO2_GIB <- filter(NO2_GIB, time == "2016-05-01" | time == "2016-05-02")


# gorup NO2 data by hour
NO2_GIB <- NO2_GIB %>%
  mutate(date = ymd_hms(date, tz = "UTC"),
         hour = hour(date),
         date = date) %>%
  group_by(site,
           hour) %>%
  summarise(AVG_NO2 = mean(no2, na.rm = TRUE))%>%
  ungroup() %>%
  arrange(hour) 


# add lat & lon to AQ data in Gibraltar 
NO2_GIB <- NO2_GIB %>%
   left_join(info_sites_GIB, "site") 
 NO2_GIB <- as.data.frame(NO2_GIB)
 

NO2_GIB <- cbind(NO2_GIB$longitude,
                 NO2_GIB$latitude,
                 NO2_GIB$AVG_NO2,
                 NO2_GIB$hour)
colnames(NO2_GIB) <- c("longitude", "latitude", "AVG_NO2", "hour")
NO2_GIB <- as.data.frame(NO2_GIB)
                 

###############################################################################################
###############################################################################################

# for each OPENLRBINARY THERE ARE TWO PAIRS OF COORDINATES, THAT BECAUSE WE ARE DEALING WITH LINES
# LET'S EXTRACT 1st POINT OF THE PAIR

POINTS <- NULL

 for (i in 1:nrow(traffic))  {
#  for (i in 1:10)  {
  name <- as.character(traffic$openlrbinary[i])
  
# encode the OpenLR binary data and generate a kml file
  system(paste("java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i", name, "-k", sprintf('Coords%02d.kml', i)))

# extract coordinates from kml file
tkml <- getKMLcoordinates(kmlfile=sprintf('Coords%02d.kml', i), ignoreAltitude=T)[1] # keep only THE 1st POINT (coordinate)
coords <- t(as.data.frame(tkml))
# add ID to the dataframe
ID <- as.character(traffic$elaborateddataid)[i]
coords <- cbind(ID, coords)
colnames(coords) <- c("ID", "lon", "lat")

# bind data together
POINTS <- rbind(coords, POINTS)
row.names(POINTS) <- NULL
POINTS_1st <- as.data.frame(POINTS)

# remove the kml file
if (file.exists(sprintf('Coords%02d.kml', i))) file.remove(sprintf('Coords%02d.kml', i))

}

##################################################################################

write.csv(POINTS_1st, "points_traffic_1st.csv")

##################################################################################
# LET'S EXTRACT now the 2nd POINT OF THE PAIR

POINTS <- NULL

for (i in 1:nrow(traffic))  {
  #  for (i in 1:10)  {
  name <- as.character(traffic$openlrbinary[i])
  
  # encode the OpenLR binary data and generate a kml file
  system(paste("java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i", name, "-k", sprintf('Coords%02d.kml', i)))
  
  # extract coordinates from kml file
  tkml <- getKMLcoordinates(kmlfile=sprintf('Coords%02d.kml', i), ignoreAltitude=T)[2] # keep only THE 2nd POINT (coordinate)
  coords <- t(as.data.frame(tkml))
  # add ID to the dataframe
  ID <- as.character(traffic$elaborateddataid)[i]
  coords <- cbind(ID, coords)
  colnames(coords) <- c("ID", "lon", "lat")
  
  # bind data together
  POINTS <- rbind(coords, POINTS)
  row.names(POINTS) <- NULL
  POINTS_2nd <- as.data.frame(POINTS)
  
  # remove the kml file
  if (file.exists(sprintf('Coords%02d.kml', i))) file.remove(sprintf('Coords%02d.kml', i))
  
}

##################################################################################

write.csv(POINTS_2nd, "points_traffic_2nd.csv")

#########################################################################################
#########################################################################################

POINTS_1st <- read.csv("points_traffic_1st.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_2nd.csv")[2:4]

# Join traffic data (speed) to spatial data
# traffic <- cbind(traffic,POINTS)
traffic_1st <- cbind(traffic,POINTS_1st)
traffic_2nd <- cbind(traffic,POINTS_2nd)

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)
traffic <- traffic %>%
  arrange(hour)

traffic <- traffic[,3:7]

# join traffic data and Air Quality data for NO2
# NO2_GIB <- NO2_GIB %>%
#   left_join(traffic, "hour")


##############################################################################################
##############################################################################################
###################### Traffic data from Tom Tom #############################################

setwd("C:/PostgreSQL_Gibraltair/OpenLR/hours")
# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR/hours")
 
# write a .csv file for each hour
for (i in 0:22) {
traffic_hour <- filter(traffic, hour == i)  # filter traffic data with hour == i
write.csv(traffic_hour, paste(sprintf('traffic_h%02d',i), ".csv", sep = ""))


# make a spatial dataframe with traffic data for each hour
sp_traffic_hour <- SpatialPointsDataFrame(traffic_hour[,4:5], traffic_hour,  # lat, lon
                                    proj4string=CRS("+init=epsg:4326")) 
plot(sp_traffic_hour)

# Export shp file
# writeOGR(sp_traffic,"gibraltar_traffic",
#          "traffic_flow", driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)

writeOGR(sp_traffic_hour, sprintf('gibraltar_traffic_h%02d',i),
         "traffic_flow", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
}


####################################################################################################
####################################################################################################
###################  NO2 data from Gibraltar DB ####################################################

setwd("C:/PostgreSQL_Gibraltair/OpenLR/hours_NO2")
# setwd("C:/RICARDO-AEA/PostgreSQL_Gibraltair/OpenLR/hours_NO2")


# write a .csv file for each hour
for (i in 0:22) {
  NO2_hour <- filter(NO2_GIB, hour == i)  # filter NO2 averaged data with hour == i
  write.csv(NO2_hour, paste(sprintf('AVG_NO2_h%02d',i), ".csv", sep = ""))
  
  
  # make a spatial dataframe with traffic data for each hour
  sp_NO2_hour <- SpatialPointsDataFrame(NO2_hour[,1:2], NO2_hour,  # longitude, latitude
                                        proj4string=CRS("+init=epsg:4326")) 
  plot(sp_NO2_hour)
  
  
  writeOGR(sp_NO2_hour, sprintf('gibraltar_AVG_NO2_h%02d',i),
           "AVG_NO2", driver = "ESRI Shapefile",
           overwrite_layer = TRUE)
}



############################################################################################
############################################################################################

# Plot traffic flow data (AVG speed) from TomTom data for each hour + average NO2 concentrations
  
# define range for color scale base on average speed

# MAX & MIN values for speed
SPEED <- data.frame()
  for  (i in 1:22) { 
    # traffic_hour <- read.csv(paste(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/traffic_h%02d",i), ".csv", sep = ""))
    traffic_hour <- read.csv(paste(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours/traffic_h%02d",i), ".csv", sep = ""))
    SPEED <- rbind(SPEED, traffic_hour)
  }
MAX_speed <- max(SPEED$AVG_Speed)  
MIN_speed <- min(SPEED$AVG_Speed)

  

# MAX & MIN values for NO2
NO2_HOUR <- data.frame()
for  (i in 0:22) { 
  # NO2_hour <- read.csv(paste(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours_NO2/AVG_NO2_h%02d",i), ".csv", sep = ""))
  NO2_hour <- read.csv(paste(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_NO2/AVG_NO2_h%02d",i), ".csv", sep = ""))
  NO2_HOUR <- rbind(NO2_HOUR, NO2_hour)
}
MAX_NO2 <- max(NO2_HOUR$AVG_NO2)
MIN_NO2 <- min(NO2_HOUR$AVG_NO2)


  
  # load shp file just generated with traffic points data for each hour and make Leaflet MAPS
  # for each hour
  
  for (i in 1:22) {
    dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours/gibraltar_traffic_h%02d",i))
    # dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/gibraltar_traffic_h%02d",i))
    sp_traffic_hour <- readOGR(dsn = dir, layer = "traffic_flow")
    traffic_density <- nrow(sp_traffic_hour@data)
    
    # dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours_NO2/gibraltar_AVG_NO2_h%02d",i))
    dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_NO2/gibraltar_AVG_NO2_h%02d",i))
    sp_NO2_hour <- readOGR(dsn = dir, layer = "AVG_NO2")

    
    pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                        c(MIN_speed, MAX_speed),na.color = "transparent")
    

    pal_NO2 <- colorNumeric(c("#0000ff", "#ff0000"),  
                            c(MIN_NO2, MAX_NO2),na.color = "transparent")

    # popout average speed for each waypoint
    popup_speed <- paste0("<p><strong>speed: </strong>", 
                          sp_traffic_hour$AVG_Speed)    
    
    # popout average speed for each waypoint
    popup_NO2 <- paste0("<strong>NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                        sp_NO2_hour$AVG_NO2)
    
    "h2 { font-size: 40px;}"
#     content_NO2 <- paste(sprintf('<h2><strong>NO<sub>2</sub></strong> hour %02d:00',i),'<h1>', sep = "")
#     popup_info <- paste0(content_NO2, sp_NO2_hour$AVG_NO2)
    
#    content <- paste(sprintf('<h2><strong> hour %02d:00',i),'<h2>', sep = "")
    content <- paste(sprintf('<h2><center><strong> hour %02d:00',i), "<br><strong>traffic density: </strong>", traffic_density)
    popup_info <- paste0(content, sp_traffic_hour$AVG_Speed)
    
    map <- leaflet() %>%
      addTiles() %>% 
      setView(-5.35, 36.135, 14) %>%
       addPopups(-5.34, 36.155, content,
                 options = popupOptions(closeButton = FALSE)) %>%
      # Add tiles
       addTiles(group = "OSM (default)") %>%
       addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
       addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
       addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
       addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addCircles(lng=sp_traffic_hour$lon, lat=sp_traffic_hour$lat,  
                 popup= as.vector(popup_speed),
                 weight = 3, radius=30, 
                 color = pal_speed(sp_traffic_hour$AVG_Speed), stroke = FALSE, fillOpacity = 1,
                 # color="#000000", stroke = TRUE, fillOpacity = 6, 
                 group= "Position") %>%
      addLegend("bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
                title = "<strong>AVG Speed (km/h): </strong><br>(small circles)",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.6) %>%
      addCircleMarkers(lng=sp_NO2_hour$longitude, lat=sp_NO2_hour$latitude,  
                popup= as.vector(popup_NO2),
                weight = 4, radius=8,
                color = pal_NO2(sp_NO2_hour$AVG_NO2), stroke = FALSE, fillOpacity = 1,
                group= "NO2") %>%
      addLegend("bottomright", pal = pal_NO2, values = c(MIN_NO2, MAX_NO2),
                title = "<strong>Mean NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong><br>(large circles)",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.6) %>%
      addLayersControl(
        baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
        overlayGroups = c("Position", "NO2"),
        options = layersControlOptions(collapsed = TRUE)) 

    
map

    ## create a series of .png figures for each hour
    saveWidget(map, 'gibraltar_TomTom.html', selfcontained = FALSE)
    webshot('gibraltar_TomTom.html', file=sprintf('traffic_flow_h%02d.png', i), 
            vwidth = 1300, vheight = 900, cliprect = 'viewport')
 
  } 
  
map


# to use with ImageMagik using the commnad line "cmd" in windows
# convert -delay 120 -loop 0 *.png traffic_Gibraltar_Tom_Tom.gif

###################### end ##############################################################
########################################################################################

    











    
    
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
    
    # Plot traffic flow data (AVG speed) from TomTom data for each hour
    
    # define range for color scale base on average speed
    # MAX value
    for  (i in 0:22) { 
      traffic_hour <- read.csv(paste(sprintf("traffic_h%02d",i), ".csv", sep = ""))
      MIN_speed <- min(traffic_hour$AVG_Speed)
    }
    
    
    # MIN value
    for  (i in 0:22) { 
      traffic_hour <- read.csv(paste(sprintf("traffic_h%02d",i), ".csv", sep = ""))
      MAX_speed <- max(traffic_hour$AVG_Speed)
    }
    
    
    # load shp file just generated with traffic points data for each hour and make Leaflet MAPS
    # for each hour
    
    for (i in 0:22) {
      dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours/gibraltar_traffic_h%02d",i))
      # dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/gibraltar_traffic_h%02d",i))
      sp_traffic_hour <- readOGR(dsn = dir, layer = "traffic_flow")
      
      
      pal <- colorNumeric(
        palette = "Reds",
        # domain = sp_traffic_hour$AVG_Speed,
        c(MIN_speed, MAX_speed))
      
      pal <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed),na.color = "transparent")
      
      # popout average speed for each waypoint
      popup_speed <- paste0("<p><strong>speed: </strong>", 
                            sp_traffic_hour$AVG_Speed)
      
      
      "h1 { font-size: 40px;}"
      # content <- paste("<h1>2001 (PM10)</h1>")
      content <- paste(sprintf('<h1>speed hour %02d:00',i),'<h1>', sep = "")
      popup_info <- paste0(content, sp_traffic_hour$AVG_Speed)
      
      
      map <- leaflet() %>%
        addTiles() %>% 
        setView(-5.35, 36.15, 15) %>%
        addPopups(-5.34, 36.15, content,
                  options = popupOptions(closeButton = FALSE)) %>%
        # Add tiles
        # addTiles(group = "OSM (default)") %>%
        # addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
        # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        # addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
        addCircles(lng=sp_traffic_hour$lon, lat=sp_traffic_hour$lat,  
                   popup= as.vector(popup_speed),
                   weight = 3, radius=20, 
                   color = pal(sp_traffic_hour$AVG_Speed), stroke = FALSE, fillOpacity = 6,
                   # color="#000000", stroke = TRUE, fillOpacity = 6, 
                   group= "Position") %>%
        addLegend("bottomright", pal = pal, values = c(MIN_speed, MAX_speed),
                  title = "<br><strong>Average Speed (km/h): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = c("Position"),
          options = layersControlOptions(collapsed = TRUE)) 
      map
      
      
      ## create a series of .png figures for each hour
      saveWidget(map, 'gibraltar_Tom_Tom.html', selfcontained = FALSE)
      webshot('gibraltar_Tom_Tom.html', file=sprintf('traffic_flow_h%02d.png', i),
              cliprect = 'viewport')
      
    }
    
    
# to use with ImageMagik using the commnad line cmd in windows
# convert -delay 120 -loop 0 *.png traffic_Gibraltar_Tom_Tom.gif
    
##############################################################################################
##############################################################################################
##############################################################################################
    
    
    
    # Plot traffic AVG NO2 data for each hour
    
    # define range for color scale base on average speed
    # MIN value
    for  (i in 0:22) { 
      NO2_hour <- read.csv(paste(sprintf("AVG_NO2_h%02d",i), ".csv", sep = ""))
      MIN_NO2 <- min(NO2_hour$AVG_NO2)
    }
    
    
    # MAX value
    for  (i in 0:22) { 
      NO2_hour <- read.csv(paste(sprintf("AVG_NO2_h%02d",i), ".csv", sep = ""))
      MAX_NO2 <- max(NO2_hour$AVG_NO2)
    }
    
    
    # load shp file just generated with AVG_NO2 points data for each hour and make Leaflet MAPS
    # for each hour
    
    for (i in 0:22) {
      # dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_NO2/gibraltar_traffic_h%02d",i))
      dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours_NO2/gibraltar_AVG_NO2_h%02d",i))
      sp_NO2_hour <- readOGR(dsn = dir, layer = "AVG_NO2")
      
      
      pal_NO2 <- colorNumeric(
        palette = "Reds",
        c(MIN_NO2, MAX_NO2))
      
      pal_NO2 <- colorNumeric(c("#0000ff", "#ff0000"),
                              c(MIN_NO2, MAX_NO2),na.color = "transparent")
      
      
      # popout average speed for each waypoint
      popup_NO2 <- paste0("<strong>NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                          sp_NO2_hour$AVG_NO2)
      
      
      "h1 { font-size: 40px;}"
      content <- paste(sprintf('<h1><strong>NO<sub>2</sub></strong> hour %02d:00',i),'<h1>', sep = "")
      popup_info <- paste0(content, sp_NO2_hour$AVG_NO2)
      
      
      map <- leaflet() %>%
        addTiles() %>% 
        setView(-5.35, 35.15, 14) %>%
        addPopups(-5.34, 36.15, content,
                  options = popupOptions(closeButton = FALSE)) %>%
        # Add tiles
        # addTiles(group = "OSM (default)") %>%
        # addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
        # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        # addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
        addCircleMarkers(lng=sp_NO2_hour$longitude, lat=sp_NO2_hour$latitude,  
                         popup= as.vector(popup_NO2),
                         weight = 3, radius=5,
                         color = pal_NO2(sp_NO2_hour$AVG_NO2), stroke = TRUE, fillOpacity = 0.5,
                         # color="#000000", stroke = TRUE, fillOpacity = 6, 
                         group= "Position") %>%
        addLegend("bottomright", pal = pal_NO2, values = c(MIN_NO2, MAX_NO2),
                  title = "<br><strong>NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 0.6) %>%
        addLayersControl(
          baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
          overlayGroups = c("Position"),
          options = layersControlOptions(collapsed = TRUE)) 
      
    }
    
    map
    
    
    
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
    
##### additional development stuff...DO NOT RUN ##############################################################
    
##############################################################################################################
##############################################################################################################
    
site_vector <- unique(traffic$ID, traffic$lon, traffic$lat) ### sort out traffic site names only once
site_vector <- as.data.frame(site_vector)
colnames(site_vector) <- "ID"

TomTom_sites <- site_vector %>%
  inner_join(traffic[3:5], "ID") 

TomTom_sites <- join(traffic, site_vector, by = "ID")


traffic_lon <- traffic[4]
traffic_lat <- traffic[5]

GIB_lon <- info_sites_GIB[10]
GIB_lat <- info_sites_GIB[9]

d <- data.frame()

for (j in 1:nrow(traffic)) {
  for(i in 1:nrow(info_sites_GIB)) {
    distance = sqrt((traffic_lon[j,]- GIB_lon[i,])^2 + (traffic_lat[j,]- GIB_lat[i,])^2) < 0.01
  }
}



for (j in 1:nrow(traffic)) {
  for(i in 1:nrow(info_sites_GIB)) {
    distance[j] = sqrt((traffic_lon[j,]- GIB_lon[i,])^2 + (traffic_lat[j,]- GIB_lat[i,])^2) < 0.01
    d <- rbind(d,distance[j])
  }
}




AAA <- split(d, 1:nrow(info_sites_GIB))
AAA <- as.data.frame(AAA)
colnames(AAA) <- info_sites_GIB$site   #### Site CODES
rownames(AAA) <- traffic$ID  #### Site CODE


#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################















###### OLD STUFF ############################################################################
# 
# 
# #make polygon
# p1 = Polygon(POINTS)
# #make Polygon class
# p2 = Polygons(list(p1), ID = "drivetime")
# #make spatial polygons class
# p3= SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))
# plot(p3)
# 
# 
# 
# 
# # Export shp file
# writeOGR(p3,"Gibraltar_Polygons",
#          "Traffic_Flow", driver = "ESRI Shapefile",
#          overwrite_layer = TRUE)
#   
# 
# 
# m <- leaflet() %>%
#   
#   # Add tiles
#   addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
#   addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
#   addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
#   
#   addLegend(position = 'bottomright',opacity = 0.4, 
#             colors = 'blue', 
#             labels = 'Gibraltair trial TomTom',
#             title = 'Routes in Gibraltair') %>%
#   
#   addPolylines(data=p3, color='blue',
#                group='Route') %>%
#   
#   # Layers control
#   addLayersControl(position = 'bottomright',
#                    baseGroups = c("Road map", "Topographical", "Satellite"),
#                    overlayGroups = c("Route"),
#                    options = layersControlOptions(collapsed = FALSE))
# 
# m
# 
# 
# # paste("java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i", name, "-k", sprintf('Coords%02d.kml', 1))
# 
# # sprintf('Coords%02d.kml', i)
# # sprintf('pm1020%02d.png', 100)
# 
# # system windows (cmd command tu run in this directory)
# # java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i C/wxURm16gw4DAI7/9wLAA== -o binview-line.txt -k binview-line.kml 
# 
# system(paste("java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i", name, "-o binview-line.txt -k binview-line.kml"))
# system(paste("java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i", name, "-k binview-line.kml"))
# 
# 
# 
# 
# # get coordinates from KML file generated from conversion in OpenLR
# tkml <- getKMLcoordinates(kmlfile="prova.kml", ignoreAltitude=T)
# coords <- as.data.frame(tkml)
# colnames(coords) <- c("p1", "p2")
# coords <- t(coords)
# colnames(coords) <- c("lon", "lat")
# #make polygon
# p1 = Polygon(tkml)
# #make Polygon class
# p2 = Polygons(list(p1), ID = "drivetime")
# #make spatial polygons class
# p3= SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))
# plot(p3)
# 
# AAA <- as.data.frame(tkml)
# AAA <- t(AAA)
