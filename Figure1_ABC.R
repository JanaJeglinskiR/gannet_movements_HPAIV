################################################
##    Figure 1 A B C maps                     ##
##     Jana Jeglinski     December 2022       ##
################################################
library(colourvalues)
library(tidyr)
library(sf)
library(ggplot2)
library(scales)
library(viridis)
library(viridisLite)
library(ggmap)
library(tmap)
library(gridExtra)
library(patchwork)


## Run Analysis - colony_visits & max distance_HPAIV_gannet.R before 
##  and Figure 1D.R before running code below


##  crop land data to reduce map size
land <- land_small %>% st_transform(4326) 

box = c(xmin = -15, ymin = 45, xmax = 14, ymax = 68)
land <- st_crop(land,box)

land <- land %>% st_transform(3035)


### Prepare Map (Figure 1 A, B, C)
# visited colonies from script Analysis - colony_visits & max distance_HPAIV_gannet.R 

visited <- c("Troup Head" ,"Ailsa Craig", "Helgoland", "St Kilda")

c_dat <- cols_keep1 %>% filter(Colony %in% visited)
other <- cols_keep1 %>% filter(!(Colony %in% visited))


# Separate GPS data into relevant periods

alldat$period <- ifelse(alldat$julian_d < 155 , "before",
                        ifelse(alldat$julian_d > 212,"after","during"))
                        
alldat$period <- factor(alldat$period, levels = c("before", "during", "after"))

alldat1 <- st_as_sf(alldat,coords=c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%  st_transform(3035) 

# check duration
min(alldat$GMT)
max(alldat$GMT)


#### Figure 1 A Map ####----
# specify plotting extent

mybox <- st_as_sfc(st_bbox(alldat1))
mybox <- st_buffer(mybox, 130000)

# option 1
# extract colours for legend
show_col(viridis_pal()(10))

tag.colours <- colour_values(palette = "viridis", 1:10)

# split data for plotting - colour GPS data during outbreak

tags_sub <- c("18220", "18209","18247","18233","18244","18232","18226","18239","18242", "18215")

t_locs <- alldat1 %>% filter(BIRD_ID %in% tags_sub)
o_locs <- alldat1 %>% filter(!BIRD_ID %in% tags_sub)


# re order tag levels for better plotting
t_locs$BIRD_ID <- factor(t_locs$BIRD_ID, 
                       levels = c("18220", "18209","18247","18233","18244","18232","18226","18239","18242", "18215"))


 
 ## Figure 1 A B C  MAP ----

 b_dat <- t_locs %>% filter(period == "before")
 o_dat <- o_locs %>% filter(period == "before")
 
## Map 1 - before----

 tmap_mode("plot")
 map1 <- tm_shape(land, bbox = mybox) + tm_polygons(col = "lightgrey", alpha = 0.4) +
   tm_shape(o_dat, bbox = mybox) + 
   tm_dots(col = "grey", alpha = 0.1, legend.show = FALSE) +
   tm_shape(cols_keep1) + tm_dots(col = "black", size = 0.05, shape = 5, legend.show = FALSE) +
   tm_shape(b_dat, bbox = mybox) + 
   tm_dots(col = "BIRD_ID", palette = "viridis", alpha = 0.6, legend.show = FALSE) +
   tm_compass(size = 4, type = "arrow", position = c("left", "TOP")) +
   tm_scale_bar(position = c("right", "BOTTOM"), text.size = 0.5) 
   #tm_layout(title.size = 0.8,  main.title = "Before outbreak", main.title.position = "center")
 map1 
 
 
## Map 2 - during ----
 # here add stars for visited colonies
 
 b_dat <- t_locs %>% filter(period == "during")
 o_dat <- o_locs %>% filter(period == "during")
 
 tmap_mode("plot")
 map2 <- tm_shape(land, bbox = mybox) + tm_polygons(col = "lightgrey", alpha = 0.4) +
   tm_shape(o_dat, bbox = mybox) + 
   tm_dots(col = "grey", alpha = 0.1, legend.show = FALSE) +
   tm_shape(other) + tm_dots(col = "black", size = 0.05, shape = 5, legend.show = FALSE) +
   tm_shape(c_dat) + tm_dots(col = "orange",size = 0.5, shape = 23, legend.show = FALSE) +
   tm_shape(b_dat, bbox = mybox) + 
   tm_dots(col = "BIRD_ID", palette = "viridis", alpha = 0.6, legend.show = FALSE) +
   tm_compass(size = 4, type = "arrow", position = c("left", "TOP")) +
   tm_scale_bar(position = c("right", "BOTTOM"), text.size = 0.5) 
   #tm_layout(title.size = 0.8, main.title = "During outbreak", main.title.position = "center")
 map2
 
## Map 3 - after----
 
 b_dat <- t_locs %>% filter(period == "after")
 o_dat <- o_locs %>% filter(period == "after")
 
 tmap_mode("plot")
 map3 <- tm_shape(land, bbox = mybox) + tm_polygons(col = "lightgrey", alpha = 0.4) +
   tm_shape(o_dat, bbox = mybox) + 
   tm_dots(col = "grey", alpha = 0.1, legend.show = FALSE) +
   tm_shape(cols_keep1) + tm_dots(col = "black", size = 0.05, shape = 5, legend.show = FALSE) +
   tm_shape(b_dat, bbox = mybox) + 
   tm_dots(col = "BIRD_ID", palette = "viridis", alpha = 0.6, legend.show = FALSE) +
   tm_compass(size = 4, type = "arrow", position = c("left", "TOP")) +
   tm_scale_bar(position = c("right", "BOTTOM"), text.size = 0.5) 
   #tm_layout(title.size = 0.8, main.title = "After outbreak", main.title.position = "center")
 map3
 
# combine Figures----
 
#map0g <- tmap_grob(map0)
map1g <- tmap_grob(map1)
map2g <- tmap_grob(map2)
map3g <- tmap_grob(map3)

## combine map with Figure 1D & Figure 1E

library(cowplot)

ggdraw() +
  draw_plot(map1g, x = 0.03, y = 0.6, width = 0.33, height = 0.38) +
  draw_plot(map2g, x = 0.03, y = 0.3, width = 0.33, height = 0.38) +
  draw_plot(map3g, x = 0.03, y = 0, width = 0.33, height = 0.38) +
  draw_plot(fig1E, x = 0.39, y = 0.02, width = 0.6, height = 0.42) +
  draw_plot(fig1D, x = 0.39, y = 0.45, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E"), size = 15,
                  x = c(0.004, 0.004, 0.004,0.39,0.39), y = c(0.96, 0.66,0.36,0.96,0.46))


## Saving composite Figure

ggsave("Figure 1.jpg", width = 17.3, height = 20, units = "cm", dpi = 300)





#####################################  experiments below ####################### 
 
## Plots for individuals 
 
 tmap_mode("plot")
 
 map2 <- tm_shape(land, bbox = mybox) + tm_polygons(col = "grey") +
   tm_shape(t_locs, bbox = mybox) + 
   tm_dots(title = "Tag ID",col = "BIRD_ID", palette = "viridis",alpha = 0.4, legend.show = FALSE) +
   tm_compass(type = "arrow", position = c("left", "TOP")) +
   tm_scale_bar(position = c("left", "BOTTOM"), text.size = 0.5) +
   tm_facets(by = c("period","BIRD_ID"), free.coords = FALSE) 
 
map2
 




#### SUPPLEMENTARY MATERIAL----

## Plot tracks for 18247, 18220 and 18226 with gogle maps background

t18220 <- t_data %>% filter(BIRD_ID =="18220") %>% st_transform(4326) 
t18247 <- t_data %>% filter(BIRD_ID =="18247") %>% st_transform(4326) 
t18226 <- t_data %>% filter(BIRD_ID =="18226") %>% st_transform(4326) 

# specific colonies
HE <- cols_keep1 %>% filter(Colony == "Helgoland") %>% st_buffer(dist = 2000) %>% st_transform(4326) 
AC <- cols_keep1 %>% filter(Colony == "Ailsa Craig") %>% st_buffer(dist = 2000) %>% st_transform(4326) 
ST <- cols_keep1 %>% filter(Colony == "Sule Stack") %>% st_buffer(dist = 2000) %>% st_transform(4326) 
SK <- cols_keep1 %>% filter(Colony == "St Kilda") %>% st_buffer(dist = 2000) %>% st_transform(4326) 
TH <- cols_keep1 %>% filter(Colony == "Troup Head") %>% st_buffer(dist = 2000) %>% st_transform(4326) 


# quick view

tmap_mode("view")
tm_shape(HE) + tm_polygons(alpha = 0.3) + tm_shape(t18220) + tm_dots(col = "red")
tm_shape(AC) + tm_polygons(alpha = 0.3) + tm_shape(t18247) + tm_dots(col = "red")
tm_shape(ST) + tm_polygons(alpha = 0.3) + tm_shape(t18247) + tm_dots(col = "red")
tm_shape(SK) + tm_polygons(alpha = 0.3) + tm_shape(t18247) + tm_dots(col = "red")
tm_shape(TH) + tm_polygons(alpha = 0.3) + tm_shape(t18247) + tm_dots(col = "red")


## detailed maps with aerial images

backgroundHE <- get_googlemap(center = c(lon = 7.874546, lat = 54.18682), zoom = 9, maptype = "satellite") 
backgroundTH <- get_googlemap(center = c(lon = -2.269213, lat = 57.6968), zoom = 9, maptype = "satellite") 
backgroundSK <- get_googlemap(center = c(lon = -8.463387, lat = 57.8849), zoom = 9, maptype = "satellite") 
backgroundAC <- get_googlemap(center = c(lon = -5.085834, lat = 55.2563), zoom = 9, maptype = "satellite") 
backgroundST <- get_googlemap(center = c(lon = -4.472824, lat = 59.0276), zoom = 9, maptype = "satellite") 



background18226 <- get_googlemap(center = c(lon = 5.862654, lat = 58.428286), zoom = 8, maptype = "satellite") 

background18226d <- get_googlemap(center = c(lon = 6.356023, lat = 58.660262), zoom = 11, maptype = "satellite")
background18226d <- get_googlemap(center = c(lon = 6.356023, lat = 58.660262), zoom = 11, maptype = "terrain")


## Supplementary material S1 Figure S1----
ggmap(backgroundHE) + geom_sf(data = t18220, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 

## Supplementary material S1 Figure S2----
ggmap(backgroundTH) + geom_sf(data = t18247, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 

## Supplementary material S1 Figure S3----
ggmap(backgroundSK) + geom_sf(data = t18247, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 

## Supplementary material S1 Figure S4----
ggmap(backgroundAC) + geom_sf(data = t18247, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 


## Supplementary material S1 Figure S5----
ggmap(backgroundAC) + geom_sf(data = t18247, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 


## Supplementary material S1 Figure S6----
ggmap(backgroundST) + geom_sf(data = t18247, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 


## information for 18226

ggmap(background18226) + geom_sf(data = t18226, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 

ggmap(background18226d) + geom_sf(data = t18226, aes(colour = Status), size = 2, alpha = 0.6, inherit.aes = F) +
  scale_colour_viridis_d(direction = -1) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(1, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical,height = unit(0.8, "cm"),
                         width = unit(1, "cm")) 


# write kml file to better check path
setwd("~/OneDrive - University of Glasgow/Msc 1 TIME BUDGETS gannets/DATA/2022")

plotKML(t18226, "t18226", open.kml = F)














