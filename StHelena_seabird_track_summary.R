############################################################################################################
#######  ST HELENA SEABIRD MARINE DISTRIBUTION ANALYSIS  ###################################################
############################################################################################################
## simple trip summaries for St Helena Seabird tracking data
## based on original mIBA script from 2016
## updated 17 July 2018
## updated 15 March 2019 - completely revised data manipulation
## included new track2kba functions

### 23 Sept 2019: changed deployments to different query in RODBC_import

### 13 Nov 2019: reduced script to make it less confusing


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY SPECIES AND YEARS OF INTEREST TO AVOID DEALING WITH OLD DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(lubridate)
DATE_CUTOFF<-ymd("2017-01-01")
SPEC<-c("MASPE","BRONO")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES AND CUSTOM SCRIPTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(maps)
require(maptools)
require(geosphere)
require(sp)
require(rgdal)
require(rgeos)
library(move)
library(raster)
library(ggplot2)
library(tidyverse)
library(data.table)
library(adehabitatHR)
filter<-dplyr::filter
select<-dplyr::select

###### LOAD NEW PACKAGE DEVELOPED BY BIRDLIFE ######
library(devtools)
devtools::install_github("steffenoppel/track2iba", dependencies=T)
library(track2KBA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA FROM DATABASE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### RUN FUNCTION TO READ IN DATA IN 32-bit R ONLY AFTER CHANGES TO DATABASE
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds/RODBC_seabird_data.r")), wait = TRUE, invisible = FALSE)
#setwd("S:/ConSci/DptShare/SteffenOppel/RSPB/UKOT/StHelena/Science/Birds/seabirds")
setwd("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds")
load("Seabird_Tracking_Data.RData")
head(tracks)
head(deployments)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODIFY SPECIES NAMES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## THIS MAY NO LONGER BE NECESSARY IF SPECIES NAMES HAVE BEEN CHANGED IN ACCESS DATABASE

tracks$Species<-ifelse(tracks$Species=="MASBO","MABO",as.character(tracks$Species))
tracks$Species<-ifelse(tracks$Species=="REBTR","RBTB",as.character(tracks$Species))
deployments$species<-ifelse(deployments$species=="REBTR","RBTB",as.character(deployments$species))
unique(tracks$Species)
unique(deployments$species)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODIFY DATA TO MEET REQUIREMENTS FOR PROCESSING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CREATE DEPLOYMENTS TABLE
# THERE ARE MULTIPLE DEPLOYMENTS PER NEST
# THERE ARE MULTIPLE DEPLOYMENTS PER RING NR
# THERE ARE MULTIPLE DEPLOYMENTS OF GPS TAGS
### none of these numbers alone are unique - the Deployment_id is the only unique number


### MODIFY DEPLOYMENTS DATA
deployments<-deployments %>% mutate(start=ymd_hms(paste(MinOfDate,format(MinOfRelease_time,format="%H:%M:%S")))) %>%
  mutate(end=ymd_hms(paste(MaxOfDate,format(MaxOfCapture_time,format="%H:%M:%S")))) %>%
  mutate(Year=year(start)) %>%
  filter(species %in% SPEC) %>%
  rename(Longitude=MaxOfLongitude,Latitude=MaxOfLatitude,Deployment_id=MinOfEncounter_ID,Species=species,depl_mass=weight,retrieval_mass=weight.1) %>%  
  dplyr::select(Deployment_id,Ring_Nr,Species,Year,breeding_status,Nest_Nr,sex,start,end,depl_mass,retrieval_mass,Latitude,Longitude) %>%
  mutate(season = ifelse(month(start) %in% c(9,10,11,12,1),"Sept-Jan","Mar-Jul")) %>%
  filter(!is.na(start))
head(deployments)



### MODIFY TRACKS DATA
head(tracks)
tracks<-tracks %>% mutate(DateTime=ymd_hms(paste(Date,format(Time,format="%H:%M:%S")))) %>%
  filter(Species %in% SPEC) %>%
  mutate(Season=deployments$season[match(Deployment_id,deployments$Deployment_id)]) %>%
  mutate(breeding_status = ifelse(grepl(pattern="incub",x=breeding_status, ignore.case = T),"incubation","chick-rearing")) %>%
  rename(ID=Deployment_id) %>%
  arrange(ID,DateTime)

dim(tracks)
head(tracks)
unique(tracks$ID)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT THE RAW TRACKING DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xlow<-min(tracks$Longitude)-1.5
xup<-max(tracks$Longitude)+1.5
yup<-max(tracks$Latitude)+1.5
ylow<-min(tracks$Latitude)-1.5

### CREATE MULTIPANEL PLOT OF FORAGING TRIPS FOR SPECIES, SEASON and STAGE

ggplot(data=tracks,aes(x=Longitude, y=Latitude, col=breeding_status)) +
    geom_path() +
    geom_point(data=nests, aes(x=Longitude, y=Latitude), col='green', shape=16, size=1.5) +
    facet_wrap(~Species+Season+breeding_status, ncol=2) +
    coord_sf(xlim = c(xlow, xup), ylim = c(ylow, yup), expand = FALSE) +
    borders("world",fill="black",colour="black") +
  
    guides(colour=guide_legend(title="Breeding Stage",override.aes = list(size = 4)))+
    scale_color_manual(values=c("firebrick","midnightblue")) +
    theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.key = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        panel.grid.major = element_line("white",0), 
        panel.grid.minor = element_line("white",0), 
        panel.border = element_blank())




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE KERNEL UD AND PLOT ON MAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xlow<-min(tracks$Longitude)-1.5
xup<-max(tracks$Longitude)+1.5
yup<-max(tracks$Latitude)+1.5
ylow<-min(tracks$Latitude)-1.5

### CREATE MULTIPANEL PLOT OF FORAGING TRIPS FOR SPECIES, SEASON and STAGE

ggplot(data=tracks,aes(x=Longitude, y=Latitude, col=breeding_status)) +
  geom_path() +
  geom_point(data=nests, aes(x=Longitude, y=Latitude), col='green', shape=16, size=1.5) +
  facet_wrap(~Species+Season+breeding_status, ncol=2) +
  coord_sf(xlim = c(xlow, xup), ylim = c(ylow, yup), expand = FALSE) +
  borders("world",fill="black",colour="black") +
  
  guides(colour=guide_legend(title="Breeding Stage",override.aes = list(size = 4)))+
  scale_color_manual(values=c("firebrick","midnightblue")) +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.key = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        panel.grid.major = element_line("white",0), 
        panel.grid.minor = element_line("white",0), 
        panel.border = element_blank())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE SIMPLE 95% KERNEL DENSITIES FOR EACH SPECIES SEASON AND STAGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(tracks)
tracks <- tracks %>%
  mutate(Group=paste(Species,Season,breeding_status,sep="_")) %>%
  arrange(Group,DateTime)
           
DAT<-SpatialPointsDataFrame(coords=tracks[,5:4], proj4string=CRS("+proj=longlat +datum=WGS84"), data=tracks[,c(1,13)])
proj.DAT<-spTransform(x=DAT, CRS=CRS("+proj=aeqd"), center=TRUE)
proj.DAT@data$ID<-NULL


## Calculation of the 95 percent home range
UD<-kernelUD(proj.DAT, h="href", same4all=T, grid=1000)
ver95 <- getverticeshr(UD, 95)
ver75 <- getverticeshr(UD, 75)
ver50 <- getverticeshr(UD, 50)

## transform back to WGS84
ver50<-spTransform(x=ver50, CRSobj=CRS("+proj=longlat +datum=WGS84"), center=TRUE)
ver75<-spTransform(x=ver75, CRSobj=CRS("+proj=longlat +datum=WGS84"), center=TRUE)
ver95<-spTransform(x=ver95, CRSobj=CRS("+proj=longlat +datum=WGS84"), center=TRUE)

## turn into data frame for plotting
ver75_df <- fortify(ver75) %>% separate(id,into=c("Species","Season","breeding_status"),sep="_")
ver95_df <- fortify(ver95) %>% separate(id,into=c("Species","Season","breeding_status"),sep="_")
ver50_df <- fortify(ver50) %>% separate(id,into=c("Species","Season","breeding_status"),sep="_")





### CREATE MULTIPANEL PLOT OF FORAGING TRIPS FOR SPECIES, SEASON and STAGE

tracks <- tracks %>%
  arrange(ID,DateTime)

ggplot(data=tracks,aes(x=Longitude, y=Latitude, col=breeding_status)) +
  geom_path() +
  geom_point(data=nests, aes(x=Longitude, y=Latitude), col='green', shape=16, size=1.5) +
  facet_wrap(~Species+Season+breeding_status, ncol=2) +
  coord_sf(xlim = c(xlow, xup), ylim = c(ylow, yup), expand = FALSE) +
  borders("world",fill="black",colour="black") +
  
  ### ADD THE CONCENTRATION AREAS
  geom_polygon(data=ver50_df, aes(x=long, y=lat, group=piece,fill=id),colour="black", lwd=1, alpha = .7, fill='goldenrod')+
  geom_polygon(data=ver75_df, aes(x=long, y=lat, group=group),colour="black", lwd=1,alpha = .5, fill='goldenrod')+
  geom_polygon(data=ver95_df, aes(x=long, y=lat, group=group),colour="black", lwd=1, lty=2,alpha = .3, fill='goldenrod')+
  
  ### ADD LEGEND AND MODIFY COLOR
  
  guides(colour=guide_legend(title="Breeding Stage",override.aes = list(size = 4)))+
  scale_color_manual(values=c("firebrick","midnightblue")) +
  
  ### MODIFY APPEARANCE AND STYLE OF LABELS
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.key = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        panel.grid.major = element_line("white",0), 
        panel.grid.minor = element_line("white",0), 
        panel.border = element_blank())










#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPLIT INTO FORAGING TRIPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### NEEDS TO BE DONE TWICE FOR SHORT AND MULTI-TRIPS
names(tracks)
names(nests)

tracks<- tracks %>% filter(!is.na(Latitude)) %>% filter(!is.na(Longitude))
colony<-nests %>% filter(species %in% SPEC) %>% group_by(species) %>%
  summarise(Latitude=mean(Latitude, na.rm=T), Longitude=mean(Longitude, na.rm=T)) %>%
  dplyr::select(Latitude, Longitude)

multiTrips <- tripSplit(tracks[tracks$ID>1088,], Colony=colony, InnerBuff=10, ReturnBuff=25, Duration=0.25, plotit=T, rmColLocs = T)
shortTrips <- tripSplit(tracks[tracks$ID>1088,], Colony=colony, InnerBuff=2, ReturnBuff=25, Duration=0.25, plotit=T, rmColLocs = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE FORAGING TRIPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### SUMMARISE MAX DIST FROM COLONY AND TRIP TRAVELLING TIME FOR EACH TRIP
head(multiTrips)
trip_distancesMulti<-tripSummary(multiTrips, Colony=colony) %>%
  mutate(Species=deployments$Species[match(ID, deployments$Deployment_id)]) %>%
  mutate(Stage=deployments$breeding_status[match(ID, deployments$Deployment_id)]) %>%
  mutate(Year=year(departure)) %>%
  dplyr::select(Species,Year, Stage, trip_id, departure, return, duration, max_dist, total_dist, n_locs,direction) %>%
  mutate(InnerBuff=10)

trip_distancesShort<-tripSummary(shortTrips, Colony=colony) %>%
  mutate(Species=deployments$Species[match(ID, deployments$Deployment_id)]) %>%
  mutate(Stage=deployments$breeding_status[match(ID, deployments$Deployment_id)]) %>%
  mutate(Year=year(departure)) %>%
  dplyr::select(Species,Year, Stage, trip_id, departure, return, duration, max_dist, total_dist, n_locs,direction) %>%
  mutate(InnerBuff=2)

trip_distances<-rbind(trip_distancesShort,trip_distancesMulti) %>%
	arrange(Species,Year, Stage, trip_id,InnerBuff)

head(trip_distances)
dim(trip_distances)

fwrite(trip_distances, "StHelena_MASPE_TripSummaries2019_byradius.csv")
fwrite(deployments, "StHelena_MASPE_Deployments.csv")




