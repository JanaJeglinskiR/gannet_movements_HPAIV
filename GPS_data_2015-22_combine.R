##########################################################
##           combine GPS tracking data                  ##
##           Jana Jeglinski August 2022                 ##
##########################################################

# run 'breeding_gannets_2015-19_processing.R' and 'Movebank_data_access.R'

# align names 
names(april_dat)
names(august_dat)
names(other_dat)


april_dat <- april_dat %>% dplyr::rename(BIRD_ID = tag.local.identifier, Latitude = location.lat, Longitude = location.long, GMT = timestamp)
august_dat <- august_dat %>% dplyr::rename(BIRD_ID = tag.local.identifier, Latitude = location.lat, Longitude = location.long, GMT = timestamp)
april_dat$Year <- 2022
august_dat$Year <- 2022


april_dat <- april_dat[c(14,3,4,5,11)]
august_dat <- august_dat[c(16,3,4,5,13)]
other_dat <- other_dat[c(1,10,8,9,2)]

alldat <- rbind(other_dat,april_dat,august_dat)

## append julian day
alldat$julian_d <- lubridate::yday(alldat$GMT)



