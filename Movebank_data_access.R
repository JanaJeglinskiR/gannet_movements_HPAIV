##########################################################
##          Access movebank data                        ##
##        Jana Jeglinski April 2023                     ##
##########################################################

library(move)

# create login with user specific user name and password


login <- movebankLogin(username=" ", password=" ")

# login, supply username and password
getMovebankLocationData(login)

# April birds
studyID <- 2658220054

april_tags <- c("18209","18215","18220", "18226", "18232", "18233", "18239", "18242", "18244", "18247")

# download data for April birds
april_dat <- getMovebankLocationData(study=studyID, sensorID="GPS",
                            animalName=april_tags)


# April birds
studyID <- 2658117564

august_tags <- c("1395582","139597","1395976", "1395977", "1395979", "1395983", "1395986", "1498285", "1491303", "1491430")

# download data for April birds
august_dat <- getMovebankLocationData(study=studyID, sensorID="GPS",
                            animalName=august_tags)


# Alternatively download movebank data as csv and load 

april_dat_ref <- read.csv("Northern gannet (breeders) Bass Rock HPAIV outbreak April 2022-reference-data.csv", header = TRUE)
april_dat <- read.csv("Northern gannet (breeders) Bass Rock HPAIV outbreak April 2022.csv", header = TRUE)
august_dat <- read.csv("Northern gannet (breeders) Bass Rock HPAIV outbreak August 2022.csv", header = TRUE)
