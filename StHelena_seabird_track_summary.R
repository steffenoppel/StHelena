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
filter<-dplyr::filter
select<-dplyr::select



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
  #filter(species %in% SPEC) %>%
  rename(Longitude=MaxOfLongitude,Latitude=MaxOfLatitude,Deployment_id=MinOfEncounter_ID,Species=species,depl_mass=weight,retrieval_mass=weight.1) %>%  
  dplyr::select(Deployment_id,Ring_Nr,Species,Year,breeding_status,Nest_Nr,sex,start,end,depl_mass,retrieval_mass,Latitude,Longitude) %>%
  mutate(season = ifelse(month(start) %in% c(9,10,11,12,1),"Sept-Jan","Mar-Jul")) %>%
  filter(!is.na(start))
head(deployments)



### MODIFY TRACKS DATA
head(tracks)
tracks<-tracks %>% mutate(DateTime=ymd_hms(paste(Date,format(Time,format="%H:%M:%S")))) %>%
  #filter(Species %in% SPEC) %>%
  mutate(Season=deployments$season[match(Deployment_id,deployments$Deployment_id)]) %>%
  mutate(breeding_status = ifelse(grepl(pattern="incub",x=breeding_status, ignore.case = T),"incubation","chick-rearing")) %>%
  rename(ID=Deployment_id) %>%
  arrange(ID,DateTime)

dim(tracks)
head(tracks)
unique(tracks$ID)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT THE DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xlow<-min(tracks$Longitude)-1.5
xup<-max(tracks$Longitude)+1.5
yup<-max(tracks$Latitude)+1.5
ylow<-min(tracks$Latitude)-1.5

### CREATE MULTIPANEL PLOT OF FORAGING TRIPS WITH INCOMPLETE TRIPS SHOWN AS DASHED LINE

#pdf("StHelena_MASP_tracks.pdf", width=10, height=6)  
ggplot(data=tracks,aes(x=Longitude, y=Latitude, col=breeding_status)) +
    geom_path() +
    geom_point(data=nests, aes(x=Longitude, y=Latitude), col='red', shape=16, size=1.5) +
    facet_wrap(~Species) +
    #facet_wrap(~Season) +
    #removeGrid()+
    coord_sf(xlim = c(xlow, xup), ylim = c(ylow, yup), expand = FALSE) +
    borders("world",fill="black",colour="black") +
    theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_line("white",0), 
        panel.grid.minor = element_line("white",0), 
        panel.border = element_blank())

dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPLIT INTO FORAGING TRIPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### NEEDS TO BE DONE TWICE FOR SHORT AND MULTI-TRIPS


source("C:\\STEFFEN\\track2iba\\R\\tripSplit.r")
names(tracks)
names(nests)

tracks<- tracks %>% filter(!is.na(Latitude)) %>% filter(!is.na(Longitude))
#colony<- depl.nests %>% summarise(Longitude=mean(Longitude, na.rm=T), Latitude=mean(Latitude, na.rm=T))
colony<-nests %>% filter(species %in% SPEC) %>% group_by(species) %>%
  summarise(Latitude=mean(Latitude, na.rm=T), Longitude=mean(Longitude, na.rm=T)) %>%
  dplyr::select(Latitude, Longitude)

multiTrips <- tripSplit(tracks[tracks$ID>1088,], Colony=colony, InnerBuff=10, ReturnBuff=25, Duration=0.25, plotit=T, rmColLocs = T)
shortTrips <- tripSplit(tracks[tracks$ID>1088,], Colony=colony, InnerBuff=2, ReturnBuff=25, Duration=0.25, plotit=T, rmColLocs = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE FORAGING TRIPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("C:\\STEFFEN\\track2iba\\R\\tripSummary.r")

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

#trip_distances$sex<-Trips@data$sex[match(trip_distances$trip, Trips@data$trip_id)]
head(trip_distances)
dim(trip_distances)

fwrite(trip_distances, "StHelena_MASPE_TripSummaries2019_byradius.csv")
fwrite(deployments, "StHelena_MASPE_Deployments.csv")




