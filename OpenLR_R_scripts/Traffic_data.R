
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

# load traffic data until 11 May 2016
# these are stationary Traffic & queueing Traffic & low Traffic
traffic <- read.csv("traffic_data_FK_until_11_May_2016.csv")  


########################################################################

# truncate date
traffic$time <- str_sub(traffic$publicationtime, start = 1, end = -10)

# group data by hour

traffic <- traffic %>%
  mutate(date = ymd_hms(publicationtime, tz = "UTC"),
         hour = hour(date),
         date = date)%>% 
   group_by(situationid,
            openlrbinary,
            hour) %>%
#            time) %>%
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

# download data from April 12 2016
NO2_GIB <- import_measures("gibraltar",site = site_vector, 
                           variable = "no2",
                           #  start = week_before,
                           start = "2016-04-12",
                           extra = FALSE)

write.csv(NO2_GIB, "NO2_GIB_data_until_11_May_2016.csv")

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

NO2_GIB <- read.csv("NO2_GIB_data_until_11_May_2016.csv")
NO2_GIB <- NO2_GIB %>% 
  mutate(site_name = str_replace_all(site_name, " ", "_"))

# remove empty lines
NO2_GIB <- NO2_GIB[!(is.na(NO2_GIB$date)), ]
# truncate date
NO2_GIB$time <- str_sub(NO2_GIB$date, start = 1, end = -10)

# keep only the range of date as the one for the traffic flow used for TomTom data
# filter only 7-8 may data
# NO2_GIB <- filter(NO2_GIB, time == "2016-05-07" | time == "2016-05-08")


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
colnames(NO2_GIB) <- c("longitude", "latitude", "AVG_NO2","hour")
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
ID <- as.character(traffic$situationid)[i]
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

write.csv(POINTS_1st, "points_traffic_data_1st_11_May_hour.csv")

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
  ID <- as.character(traffic$situationid)[i]
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

write.csv(POINTS_2nd, "points_traffic_data_2nd_11_May_hour.csv")

#########################################################################################
#########################################################################################

POINTS_1st <- read.csv("points_traffic_data_1st_11_May_hour.csv")[2:4]
POINTS_2nd <- read.csv("points_traffic_data_2nd_11_May_hour.csv")[2:4]

# POINTS_1st <- read.csv("points_traffic_data_1st_11_May.csv")[2:4]
# POINTS_2nd <- read.csv("points_traffic_data_2nd_11_May.csv")[2:4]

# Join traffic data (speed) to spatial data
# traffic <- cbind(traffic,POINTS)
traffic_1st <- cbind(traffic,POINTS_1st)
traffic_2nd <- cbind(traffic,POINTS_2nd)

# bind 1st and 2nd point together
traffic <- rbind(traffic_1st, traffic_2nd)
# remove traffic data with latitude == 36.1470043657 (point in the water)
traffic <- traffic[traffic$lat!=36.1470043657487,]   
# remove traffic data with latitude == 36.1470043657 (point before the border)
traffic <- traffic[traffic$lat!=36.1560165880387,]  
traffic <- traffic[traffic$lat!=36.147002876331,] 

write.csv(traffic, "traffic_stationary_11May2016.csv")
traffic <- read.csv("traffic_stationary_11May2016.csv")

traffic <- traffic %>%
  arrange(hour)

traffic <- traffic[,4:8]

# join traffic data and Air Quality data for NO2
# NO2_GIB <- NO2_GIB %>%
#   left_join(traffic, "hour")


# make a quick leaflet map with site loaction only

# Marker + Static Label using custom label options
map <- leaflet(data = traffic[,]) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   weight = 1, radius=5, color = 'black',
                   stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(traffic$averagespeed),
                   labelOptions = labelOptions(noHide = F)) %>%
addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  # overlayGroups = c("Position", "NO2"),
  options = layersControlOptions(collapsed = TRUE)) 

map


## create a series of .png figures for each hour
saveWidget(map, 'gibraltar_TomTom_Stationary.html', selfcontained = FALSE)

##############################################################################################
##############################################################################################
###################### Traffic data from Tom Tom #############################################

setwd("C:/PostgreSQL_Gibraltair/OpenLR/hours_traffic_data")

# write a .csv file for each hour
for (i in 7:19) {
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

setwd("C:/PostgreSQL_Gibraltair/OpenLR/hours_NO2_traffic_data")
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

# Plot ONLY traffic flow data (AVG speed) from TomTom data for each hour
  
# define range for color scale base on average speed

# MAX & MIN values for speed
SPEED <- data.frame()
  for  (i in 7:19) { 
    # traffic_hour <- read.csv(paste(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/traffic_h%02d",i), ".csv", sep = ""))
    traffic_hour <- read.csv(paste(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_traffic_data/traffic_h%02d",i), ".csv", sep = ""))
    SPEED <- rbind(SPEED, traffic_hour)
  }
MAX_speed <- max(SPEED$AVG_Speed)+0.5  
MIN_speed <- min(SPEED$AVG_Speed)-0.5


pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed), na.color = "transparent")


  # load shp file just generated with traffic points data for each hour and make Leaflet MAPS
  # for each hour
  
  for (i in 7:19) {
 #   for (i in 7:8) {
    dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_traffic_data/gibraltar_traffic_h%02d",i))
    # dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/gibraltar_traffic_h%02d",i))
    sp_traffic_hour <- readOGR(dsn = dir, layer = "traffic_flow")
    traffic_density <- nrow(sp_traffic_hour@data)


  
    # popout average speed for each waypoint
    popup_speed <- paste0("<p><strong>speed: </strong>", 
                          sp_traffic_hour$AVG_Speed)    
    

 #   "h3 { font-size: 40px;}"

    content <- paste(sprintf('<h1><center><strong> hour %02d:00',i))
    popup_info <- paste0(content, sp_traffic_hour$AVG_Speed)
    
    map <- leaflet() %>%
      addTiles() %>% 
      setView(-5.35, 36.130, 15) %>%
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
      addLegend("topleft", pal = pal_speed, values = c(MIN_speed, MAX_speed),
                title = "<strong>AVG Speed (km/h): </strong>",
                labFormat = labelFormat(prefix = ""),
                opacity = 1) %>%
        addLayersControl(
        baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
        overlayGroups = c("Position"),
        options = layersControlOptions(collapsed = TRUE)) 


    ## create a series of .png figures for each hour
    saveWidget(map, 'gibraltar_TomTom.html', selfcontained = FALSE)
    webshot('gibraltar_TomTom.html', file=sprintf('traffic_data_h%02d.png', i), 
            vwidth = 1200, vheight = 1200, cliprect = 'viewport')
 

      } 
  
map


# to use with ImageMagik using the commnad line "cmd" in windows in windows in the directory "C:/PostgreSQL_Gibraltair/OpenLR/hours_NO2_traffic_data"
# convert -delay 120 -loop 0 *.png traffic_data_Gibraltar_TomTom.gif

###################### end ##############################################################
#########################################################################################














############################################################################################
############################################################################################

# Plot traffic flow data (AVG speed) from TomTom data for each hour + average NO2 concentrations

# define range for color scale base on average speed

# MAX & MIN values for speed
SPEED <- data.frame()
for  (i in 7:19) { 
  # traffic_hour <- read.csv(paste(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/traffic_h%02d",i), ".csv", sep = ""))
  traffic_hour <- read.csv(paste(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_traffic_data/traffic_h%02d",i), ".csv", sep = ""))
  SPEED <- rbind(SPEED, traffic_hour)
}
MAX_speed <- max(SPEED$AVG_Speed)+0.5  
MIN_speed <- min(SPEED$AVG_Speed)-0.5



# MAX & MIN values for NO2
NO2_HOUR <- data.frame()
for  (i in 0:22) { 
  # NO2_hour <- read.csv(paste(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours_NO2/AVG_NO2_h%02d",i), ".csv", sep = ""))
  NO2_hour <- read.csv(paste(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_NO2_traffic_data/AVG_NO2_h%02d",i), ".csv", sep = ""))
  NO2_HOUR <- rbind(NO2_HOUR, NO2_hour)
}
MAX_NO2 <- max(NO2_HOUR$AVG_NO2)+0.5
MIN_NO2 <- min(NO2_HOUR$AVG_NO2)-0.5

pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed), na.color = "transparent")


pal_NO2 <- colorNumeric(c("#0000ff", "#ff0000"),  
                        c(MIN_NO2, MAX_NO2), na.color = "transparent")


# load shp file just generated with traffic points data for each hour and make Leaflet MAPS
# for each hour

for (i in 7:19) {
  dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_traffic_data/gibraltar_traffic_h%02d",i))
  # dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours/gibraltar_traffic_h%02d",i))
  sp_traffic_hour <- readOGR(dsn = dir, layer = "traffic_flow")
  traffic_density <- nrow(sp_traffic_hour@data)
  
  # dir <-  paste0(sprintf("C:/RICARDO-AEA/postgreSQL_Gibraltair/OpenLR/hours_NO2/gibraltar_AVG_NO2_h%02d",i))
  dir <-  paste0(sprintf("C:/postgreSQL_Gibraltair/OpenLR/hours_NO2_traffic_data/gibraltar_AVG_NO2_h%02d",i))
  sp_NO2_hour <- readOGR(dsn = dir, layer = "AVG_NO2")
  
  
  
  # popout average speed for each waypoint
  popup_speed <- paste0("<p><strong>speed: </strong>", 
                        sp_traffic_hour$AVG_Speed)    
  
  # popout average speed for each waypoint
  popup_NO2 <- paste0("<strong>NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong>", 
                      sp_NO2_hour$AVG_NO2)
  
  "h2 { font-size: 40px;}"
  
  content <- paste(sprintf('<h2><center><strong> hour %02d:00',i), "<br><strong>traffic density: </strong>", traffic_density)
  popup_info <- paste0(content, sp_traffic_hour$AVG_Speed)
  
  map <- leaflet() %>%
    addTiles() %>% 
    setView(-5.35, 36.135, 14) %>%
    addPopups(-5.34, 36.160, content,
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
              title = "<strong><p><i>until_11_May_2016</i></p><strong><br>Mean NO<sub>2</sub> (<font face=symbol>m</font>g/m<sup>3</sup>): </strong><br>(large circles)",
              labFormat = labelFormat(prefix = ""),
              opacity = 0.6) %>%
    addLayersControl(
      baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
      overlayGroups = c("Position", "NO2"),
      options = layersControlOptions(collapsed = TRUE)) 
  
  
  ## create a series of .png figures for each hour
  saveWidget(map, 'gibraltar_TomTom.html', selfcontained = FALSE)
  webshot('gibraltar_TomTom.html', file=sprintf('traffic_data_h%02d.png', i), 
          vwidth = 1300, vheight = 900, cliprect = 'viewport')
  
  
} 

map


# to use with ImageMagik using the commnad line "cmd" in windows in windows in the directory "C:/PostgreSQL_Gibraltair/OpenLR/hours_NO2_traffic_data"
# convert -delay 120 -loop 0 *.png traffic_data_Gibraltar_TomTom.gif

###################### end ##############################################################
########################################################################################
