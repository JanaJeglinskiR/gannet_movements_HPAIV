##########################################################
##        HPAIV triggers long-distance movements        ##
##                    Analyses                          ##
##           Jana Jeglinski October 2022                ##
##########################################################

library(scales)
library(sf)
library(sfheaders)
library(nngeo)
library(data.table)
library(maptools)
library(raster)
library(tmap)
library(maps)
library(mapdata)
library(move)
library(adehabitatLT)
library(viridis)
library(viridisLite)
library(stars)
library(fasterize)
library(rmapshaper)
library(gdistance)
library(lubridate)
#devtools::install_github("teunbrand/ggh4x")
library(tidyverse)
library(ggh4x)
library(mgcv)
library(mgcViz)
library(gamm4)
library(tidymv)
library(ggmap)
library(plotKML)
library(rnaturalearth)
#library(rnaturalearthhires)


## load GPS data

alldat<-read.csv("data/GPS_tracking_data_gannet_15-22.csv", header = T) 

# load colony location data (in GitHub folder data)
cols_keep<-read.csv("data/Existing_gannet_coloniesNEAtlantic.csv", header = T) 

# in environment: 
#  GPS tracking data object called alldat
#  colony locations object called cols_keep

# omit colonisation attempt at St. Abbs 

cols_keep <- cols_keep %>% dplyr::filter(Colony != "St. Abbs")

# make sf object and project to LAEA
cols_keep1 <- st_as_sf(cols_keep,coords=c("Long_rough","Lat_rough")) %>% 
  st_set_crs(4326) %>%  st_transform(3035) 

alldat1 <- st_as_sf(alldat,coords=c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%  st_transform(3035) 


### Analysis 1: Colony visit detection - nearest feature approach----
# calculate nearest distance to colony centroids
# using 2 km as distance threshold (Carter et al. 2018)

tags <- unique(alldat1$BIRD_ID)

for (i in 1: length(tags)){
  
  subdat <- alldat1 %>% filter(BIRD_ID == tags[i])
  print(tags[i])
  
# column i = index of nearest point in data y, column j = distance in meters 
  subdist <- nngeo::st_nn(subdat,cols_keep1,k=1,returnDist=T)  %>% 
  set_names("ColID", "Distance_m") %>% 
  map_df(., unlist)
  subdist$Status <- NA
  
  ## threshold value 2000 - can modify here
  subdist$Status <- ifelse(subdist$Distance_m <= 2000, "Colony", "Trip")
  subdist$ColID <- ifelse(subdist$Status == "Trip", NA, subdist$ColID)

  subdist <- subdist %>% mutate(StatusID = rleid(Status))

# now replace StatusID with sequential number for each trip and Colony attendance

 keep <- cbind(subdat,subdist)
 
 if (i == 1) t_data <- keep else t_data <- rbind(t_data,keep)
 
}


## overview trip & colony attendance summary
tdata <- data.frame(t_data)

check <- tdata %>% dplyr::group_by(BIRD_ID, StatusID) %>% filter(Status == "Colony")

ovs <- data.frame(check %>%                   
  dplyr::group_by(BIRD_ID) %>%          
  dplyr::summarise(Col_visits = n_distinct(ColID)))

### attendance duration
tvs <- ovs %>% filter(Col_visits > 1)

# manually check colony visits of different birds
# here need to look at duration

test <- tdata %>% filter(BIRD_ID =="1491259") # flew for 4 min past Troup Head, closes distance 385 m
unique(test$ColID)
which(test$ColID==45)
test[4217:4219,] 
difftime(test$GMT[4219],test$GMT[4217])

#
test <- tdata %>% filter(BIRD_ID =="18220") # visit of 81 minutes closest distance 344 m
unique(test$ColID)
which(test$ColID==16)
test[5625:5629,] # 18220
difftime(test$GMT[5629],test$GMT[5626], units = "min")


test <- tdata %>% filter(BIRD_ID =="18247") # 
unique(test$ColID)
which(test$ColID==45)
test[4945:4949,] # 45 min Troup Head
difftime(test$GMT[4949],test$GMT[4946], units = "min")

which(test$ColID==43) # Sule Stac
test[5145:5147,] # 
difftime(test$GMT[5147],test$GMT[5146], units = "min") # 15 mins

which(test$ColID==36) # St. Kilda
test[5174:5176,] # too short
difftime(test$GMT[5176],test$GMT[5175], units = "min") # 14.9 mins
test[5339:5345,]
difftime(test$GMT[5345],test$GMT[5340], units = "min") # 75 mins

which(test$ColID==1) # Ailsa Craig
test[5454:5458,]
difftime(test$GMT[5458],test$GMT[5455], units = "min") # 60mins


### ANALYSIS: across-the-sea distance from home colony----

## Generate distance raster from point - extract distances for other points
## following https://www.seascapemodels.org/rstats/2020/02/08/calculating-distances-in-R.html

# load high res land polygon data from Natural earth using high res (resolution = 10m)

land <- st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
land <- land %>% st_set_crs(4326) %>%  st_transform(3035) 


home <- cols_keep1 %>% filter(Colony == "Bass Rock") 
cols_buff <- st_buffer(cols_keep1, dist = 1000000) # 100 km buffer to generate suitably large distance raster template

# create land of suitable size
land_small <- st_crop(land, st_bbox(cols_buff))


# rasterize template for land raster
r <- raster(extent(land_small), resolution = 1000) # 1 km resolution
landr <- fasterize(dplyr::summarize(land_small), r)
raster::crs(r) <- "EPSG:3035"

# extract coordinates of colonies
landr_pts <- landr
xy <- st_coordinates(home)
icell <- cellFromXY(landr, xy) # raster cells where Bass Rock sits

# change value of Bass Rock colony point
landr_pts[icell[1]] <- 2

# check
image(landr_pts)

# create distance raster (in km) - using 8 cell neighbourhood

d <- gridDistance(landr_pts, origin = 2, omit = 1)/1000

# check 
image(d)

# convert to stars to extract data
dstars <- stars::st_as_stars(d) %>% sf::st_set_crs(3035)

# double check crs
st_crs(dstars)

# extract distance values (in km, based on 1 km distance raster omitting land) for each relevant location for each animals

distances <- st_extract(dstars,at = t_data) %>% st_as_sf()
t_data$dist_BR_km <- as.numeric(distances$layer)


rm(tdata)
tdata <- data.frame(t_data)


## calculate daily max distance - response variable----

overview_dists <- data.frame(tdata %>% dplyr::group_by(BIRD_ID,julian_d,Year,season) %>% dplyr::summarize(maxdist = max(dist_BR_km, na.rm=T)))

## omit days without trips (distance = 0)
ov_dists <- overview_dists %>% filter(maxdist > 0)


## overview stats all years

summary_dists <- ov_dists %>% group_by(Year,season) %>% dplyr::summarize(max.dist = max(maxdist, na.rm=T),mean.dist = mean(maxdist, na.rm=T),sd.dist = sd(maxdist, na.rm=T))
ov_dists$AI_status <- ifelse(ov_dists$Year == 2022, "HPAI year", "non HPAI years")


## overview over transmission cessation for 2022----

ov_dists1 <- ov_dists %>% filter(Year == 2022 & season == "early season")
tperiods <- ov_dists1 %>% group_by(BIRD_ID) %>% filter(AI_status =="HPAI year") %>% 
                                                  summarize(tDur = max(julian_d)-min(julian_d), # transmission duration
                                                            maxJD = max(julian_d),              # last transmission
                                                            tPr =      155 - min(julian_d),      # days before outbreak
                                                            tP = max(julian_d)-155)              # days following outbreak




#### 2. model distances #### ----

ov_dists$AI_status <- factor(ov_dists$AI_status, levels = c("non HPAI years", "HPAI year"))
tdata$Year <- as.factor(tdata$Year)
ov_dists$Year <- as.factor(ov_dists$Year)
ov_dists$BIRD_ID <- as.factor(ov_dists$BIRD_ID)


## GAMM  ----
#gam with individual level random effect - following https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/

m.dist <- gam(maxdist ~ s(julian_d, by = AI_status) + AI_status + s(BIRD_ID, bs = "re"), family = "gaussian", data = ov_dists, method= "REML")
plot(m.dist, page = 1)

summary(m.dist) ## summary of gam

# simplify
m.dist.1 <- gam(maxdist ~ s(julian_d) + AI_status + s(BIRD_ID, bs = "re"), family = "gaussian", data = ov_dists, method= "REML")
plot(m.dist.1, page = 1)
summary(m.dist.1) ## summary of gam

# simplify further
m.dist.2 <- gam(maxdist ~ s(julian_d) + s(BIRD_ID, bs = "re"), family = "gaussian", data = ov_dists, method= "REML")
plot(m.dist.2, page = 1)
summary(m.dist.2) ## summary of gam


# compare
AIC(m.dist,m.dist.1,m.dist.2)

# plotting
#https://cran.r-project.org/web/packages/itsadug/vignettes/overview.html


smooth <- plot_smooths(m.dist, series = julian_d, facet_terms = AI_status, exclude_random = TRUE)

fig1E <- smooth + geom_point(data = ov_dists, aes(x=julian_d, y = maxdist), alpha = 0.2, size = 0.6) +
  theme_bw() + ylim(-250, 850) +
   theme(strip.background =element_rect(fill="white")) + 
  #geom_vline(xintercept = 155,col = "black", alpha = 0.6) +
  geom_ribbon(data=data.frame(x=c(155,213)), aes(x=x, ymin=-250, ymax=850), fill="gray", inherit.aes=F, alpha=0.2) +
  ylab("Maximum daily distance from Bass Rock (km)") + xlab("Julian day")

fig1E

### sensitivity analysis as suggested by reviewer 2 - omit gannets tracked late in 2022

ov_dists1 <- ov_dists %>% filter (season != "late season")


r.dist <- gam(maxdist ~ s(julian_d, by = AI_status) + AI_status + s(BIRD_ID, bs = "re"), family = "gaussian", data = ov_dists1, method= "REML")
plot(r.dist, page = 1)

summary(r.dist) ## summary of gam

# simplify
r.dist.1 <- gam(maxdist ~ s(julian_d) + AI_status + s(BIRD_ID, bs = "re"), family = "gaussian", data = ov_dists1, method= "REML")
plot(r.dist.1, page = 1)

summary(m.dist.1) ## summary of gam

# simplify further
r.dist.2 <- gam(maxdist ~ s(julian_d) + s(BIRD_ID, bs = "re"), family = "gaussian", data = ov_dists1, method= "REML")
plot(r.dist.2, page = 1)

summary(r.dist.2) ## summary of gam


# compare

AIC(r.dist,r.dist.1,r.dist.2)

## plot
ov_dists1 <- ov_dists %>% filter(season != "late season")

smooth <- plot_smooths(r.dist, series = julian_d, facet_terms = AI_status, exclude_random = TRUE)

fig1E.a <- smooth + geom_point(data = ov_dists1, aes(x=julian_d, y = maxdist), alpha = 0.2, size = 0.6) +
  theme_bw() + ylim(-250, 850) +
  theme(strip.background =element_rect(fill="white")) + 
  #geom_vline(xintercept = 155,col = "black", alpha = 0.6) +
  geom_ribbon(data=data.frame(x=c(155,213)), aes(x=x, ymin=-250, ymax=850), fill="gray", inherit.aes=F, alpha=0.2) +
  ylab("Maximum daily distance from Bass Rock (km)") + xlab("Julian day")

fig1E.a


