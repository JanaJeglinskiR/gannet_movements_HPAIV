#############################################################################
##  Pre - processing of GPS tracking data     breeding pairs spring 2022   ##
##                  Jana Jeglinski October 2022                            ##
#############################################################################

# The data for the year 2015 - 2019 is publicly accessible from the Seabird Tracking data base
# https://www.seabirdtracking.org/

# 2015 chick rearing - study IDs 1341, 1342 
# 2016 chick rearing - study ID 1472
# 2017 chick rearing - study ID 1473
# 2018 chick rearing - study ID 1660
# 2019 chick rearing - study ID 1653

# 2017 - 2019 incubation - study ID 1662

library(sf)
library(plyr)
library(tidyverse)
library(tmap)
library(viridis)
library(viridisLite)
library(move)



### load data----
root = "~/OneDrive - University of Glasgow/Msc 1 TIME BUDGETS gannets/DATA/previous_years_new"  # tell where data is
setwd(root)


# get Tag Id from file
myfiles <- list.files(path=root)

tracks2015<-read.table("2015_trips.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks2015 <- tracks2015[-1,]


tracks2016<-read.table("2016_trips.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks2016 <- tracks2016[-1,]


tracks2017<-read.table("2017_trips.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks2017 <- tracks2017[-1,]

tracks2018<-read.table("2018_trips.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks2018 <- tracks2018[-1,]

tracks2019<-read.table("2019_trips.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks2019 <- tracks2019[-1,]


tracks_inc2017<-read.table("2017_spring.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks_inc2017 <- tracks_inc2017[-1,]

tracks_inc2018<-read.table("2018_spring.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks_inc2018 <- tracks_inc2018[-1,]

tracks_inc2019<-read.table("2019_spring.csv", header = FALSE, sep = ",", dec = ".", numerals = "no.loss", 
                       col.names = c("Year","BIRD_ID","Sex","Age","Stage","TRIP_ID","DT_GMT","Longitude","Latitude"))
tracks_inc2019 <- tracks_inc2019[-1,]


## combine data

ptracks1519 <- rbind(tracks2015,tracks2016,tracks2017,tracks2018,tracks2019,tracks_inc2017,tracks_inc2018,tracks_inc2019)


### Processing ----

# convert Date Time to UTC
ptracks1519$GMT <- lubridate::dmy_hm(ptracks1519$DT_GMT)
lubridate::tz(ptracks1519$GMT) <- "UTC"
max(ptracks1519$GMT)
min(ptracks1519$GMT)

ptracks1519$sensor <- "GPS"

# remove duplicates

tags <- unique(ptracks1519$BIRD_ID)

for (i in 1:length(tags)){
  subdat1 <- ptracks1519 %>% dplyr::filter(BIRD_ID == tags[i])
  subdat <- subdat1[order(subdat1$GMT),]
  dups <- which(duplicated(subdat$GMT)==TRUE)
  if (length(dups) == 0) subkeep <- subdat else {
    subkeep <- subdat[-dups, ] } # omit duplicates
    
  if (i ==1) tdat <- subkeep else tdat <- rbind(tdat,subkeep)
  print(i)
}

rm(subdat,subdat1, subkeep,dups)


# test duplicate removal - there were 392 duplicated timesteps

length(getDuplicatedTimestamps(x=as.factor(ptracks1519$BIRD_ID), 
                        timestamps=as.POSIXct(ptracks1519$GMT, 
                                              format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                        sensorType=ptracks1519$sensor))

#no duplicates remain
getDuplicatedTimestamps(x=as.factor(tdat$BIRD_ID), 
                               timestamps=tdat$GMT,
                               sensorType=tdat$sensor)

# overview stats 
overview <- tdat %>% dplyr::group_by(BIRD_ID, Year, Stage) %>% dplyr::summarize(max(GMT), min(GMT), duration = as.numeric(max(GMT)-min(GMT)))

overview$BIRD_ID <- reorder(overview$BIRD_ID, overview$duration, FUN = mean)

summary(overview$duration)

other_dat <- tdat

rm(tdat)


  





