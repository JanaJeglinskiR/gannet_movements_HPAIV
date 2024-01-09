################################################
##    Figure 1 D                              ##
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

## run Analysis - colony_visits & max distance_HPAIV_gannet.R before code below


# 2022 data----
tags22 <- c("18220", "18209","18247","18233","18244","18232","18226","18239","18242", "18215")
ov_dists22 <- ov_dists %>% filter(BIRD_ID %in% tags22)

## calculate transmission duration

t_dur <- ov_dists22 %>% group_by(BIRD_ID) %>% summarise(duration = max(julian_d)-155)


# reference data

refdat <- read.csv("ref_data_April2022.csv", header = TRUE)
refdat <- refdat %>% filter(Tag_ID %in% tags22) %>% rename(BIRD_ID = Tag_ID)
refdat <- refdat[,c(33,5,32,40)]
refdat$BIRD_ID <- factor(refdat$BIRD_ID, 
                  levels = c("18220", "18209","18247","18233","18244","18232","18226","18239","18242", "18215"))

ov_dists22 <- dplyr::left_join(ov_dists22,refdat, by = "BIRD_ID")

# reorder factor levels to match mapping in Figure 1 A B C

ov_dists22$BIRD_ID <- factor(ov_dists22$BIRD_ID, 
                  levels = c("18220", "18209","18247","18233","18244","18232","18226","18239","18242", "18215"))
                             
ov_dists22$Pair_ID <- ifelse(ov_dists22$Pair_ID==1, "Pair 1",
                            ifelse(ov_dists22$Pair_ID==2, "Pair 2",
                             ifelse(ov_dists22$Pair_ID==3, "Pair 3",
                             ifelse(ov_dists22$Pair_ID==4, "Pair 4",
                             ifelse(ov_dists22$Pair_ID==5, "Pair 5","Pair 6")))))


ov_dists22$Sex <- ifelse(ov_dists22$Sex =="F", "Female", "Male")

## Figure 1 B ----


show_col(viridis_pal()(10))

tag.colours <- colourvalues::colour_values(1:10, palette = "viridis")

ind_dists <- ggplot(ov_dists22, aes(x=julian_d,y=maxdist)) + geom_point(aes(col = BIRD_ID), alpha = 0.8) + 
  scale_color_viridis(discrete=TRUE, name = "Bird ID") +
  geom_vline(xintercept = 155,col = "black") + xlab("Julian day") +
  ylab("Maximum daily distance from Bass Rock (km)")+
  theme_bw() + 
  facet_grid(cols =vars(Sex), rows = vars(Pair_ID)) +
  theme(strip.background = element_rect(fill="white")) 

# add resighting history
dat_text <- ov_dists22 %>% dplyr::group_by(BIRD_ID, Pair_ID, Sex, Status) %>% dplyr::summarise(n = n())


fig1D <- ind_dists + geom_text(x = 250, y = 900,
                               data = dat_text,aes(label = Status), size = 3, fontface = "italic")

### Fig 1d with ribbon

ind_dists <- ggplot(ov_dists22, aes(x=julian_d,y=maxdist)) + geom_point(aes(col = BIRD_ID), alpha = 0.8) + 
  scale_color_viridis(discrete=TRUE, name = "Bird ID") +
  geom_ribbon(data=data.frame(x=c(155,213)), aes(x=x, ymin=0, ymax=850), fill="gray", inherit.aes=F, alpha=0.2) +
  xlab("Julian day") +
  ylab("Maximum daily distance from Bass Rock (km)")+
  theme_bw() + 
  facet_grid(cols =vars(Sex), rows = vars(Pair_ID)) +
  theme(strip.background = element_rect(fill="white")) 

# add resighting history
dat_text <- ov_dists22 %>% dplyr::group_by(BIRD_ID, Pair_ID, Sex, Status) %>% dplyr::summarise(n = n())


fig1D <- ind_dists + geom_text(x = 240, y = 900,
                               data = dat_text,aes(label = Status), size = 3, fontface = "italic")


ggsave("Fig1d_1.jpeg", width = 13, height = 16, units = "cm", dpi = 400)


