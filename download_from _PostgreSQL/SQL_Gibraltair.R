
library(RPostgreSQL)

#########################################################################################################################
#########################################################################################################################
# launch first putty.exe from c:/tools 
# once putty.exe is open, load the session FedericoK and and insert password Interoute01....let tunnel open on the PC
#########################################################################################################################
#########################################################################################################################

setwd("C:/PostgreSQL_Gibraltair/OpenLR")
# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
# pw <- {
#   "Password07"
# }

pw <- {
  "Interoute1"
}


# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database

# con <- dbConnect(drv, dbname = "stuarts_sandbox",
#                  host = "172.31.113.9", port = 5432,       
#                  user = "federicok", password = pw)
# # rm(pw) # removes the password
# dbListTables(con)


# Connection to postdev-01 server where DB with TomTom data from Gibraltar is stored
con <- dbConnect(drv, dbname = "gibraltar_traffic",
                 host = "localhost", port = 5432,       
                 user = "fk8", password = "Interoute1")
# rm(pw) # removes the password
dbListTables(con)

# use  http://postdev.dc20.dev.ricardo-aea.com/ to see content of the DB
# userID = fk8, password = Interoute1

# check for the public
# dbExistsTable(con, "hysplit_data")
# TRUE
dbExistsTable(con, "traffic_data")
dbExistsTable(con, "traffic_flow")
# TRUE

# get data from the Data Base
# rs <- dbSendQuery(con, "SELECT * FROM traffic_data limit 100")  
# rs <- dbSendQuery(con, "SELECT * FROM traffic_flow limit 100")  
# rs <- dbSendQuery(con, "SELECT * FROM traffic_flow WHERE publicationtime >= '2016-05-06 00:13:30' AND publicationtime < '2016-05-07'")
# rs <- dbSendQuery(con, "SELECT * FROM traffic_flow WHERE publicationtime BETWEEN '2016-05-07 00:14:30' AND '2016-05-10 23:58:30'")  
 
# traffic flow
# rs <- dbSendQuery(con, "SELECT * FROM traffic_flow WHERE publicationtime BETWEEN '2016-05-07' AND '2016-05-08'")  
# rs <- dbSendQuery(con, "SELECT * FROM traffic_flow") # all available data
rs <- dbSendQuery(con, "SELECT * FROM traffic_flow WHERE publicationtime BETWEEN '2016-05-18 23:00:00' AND '2016-05-19 07:00:00'")  

# traffic data
# rs <- dbSendQuery(con, "SELECT * FROM traffic_data WHERE publicationtime BETWEEN '2016-05-07' AND '2016-05-08'")
rs <- dbSendQuery(con, "SELECT * FROM traffic_data") # all available data

AAA <- fetch(rs,n=-1)  # convert to a data.frame
str(AAA)
AAA

# all available traffic flow data from 12 April 2016 till today 12 May
# write.csv(AAA, file= "traffic_flow_FK_18May2016_5pm.csv")
# write.csv(AAA, file= "traffic_flow_FK_18May2016_23pm.csv")
# write.csv(AAA, file= "traffic_flow_FK_19May2016_7am.csv")

# available traffic data from 12 April 2016 till today 12 May
# write.csv(AAA, file= "traffic_data_FK_until_11_May_2016.csv")
write.csv(AAA, file= "traffic_data_FK_until_19_May_2016.csv")

#############################################################################################################################
setwd("C:/PostgreSQL_Gibraltair/OpenLR")
system(paste("java -jar otk-1.4.2-with-dependencies.jar binview -b64 -i", "C/wyWRm1ogogBwBKAJELLiw=", "-k", 'test_1.kml'))
##############################################################################################################################


dbDisconnect(con)
