############################################################################################################
#######  ST HELENA SEABIRD MARINE DISTRIBUTION ANALYSIS  ###################################################
############################################################################################################
## simple trip summaries for St Helena Seabird tracking data
## based on original mIBA script from 2016
## updated 17 July 2018
## updated 15 March 2019 - completely revised data manipulation
## included new track2kba functions

## changed deployments on 23 Sept 2019


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY SPECIES AND YEARS OF INTEREST TO AVOID DEALING WITH OLD DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(lubridate)
DATE_CUTOFF<-ymd("2017-01-01")
SPEC<-c("MASPE")


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
# EXPORT DATA TO MOVEBANK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exportDepl<- deployments %>% mutate(start=ymd_hms(paste(MinOfDate,format(MinOfRelease_time,format="%H:%M:%S")))) %>%
  mutate(end=ymd_hms(paste(MaxOfDate,format(MaxOfCapture_time,format="%H:%M:%S")))) %>%
  mutate(Year=year(start)) %>%
  mutate(species=ifelse(species=="MASBO","MABO",as.character(species))) %>%
  mutate(species=ifelse(species=="REBTR","RBTB",as.character(species))) %>%
  mutate(GPS_ID=ifelse(is.na(GPS_ID),NUM_ON_LOG,GPS_ID)) %>%
  mutate(TagID=paste(species,Year,GPS_ID, sep="_"))
head(exportDepl) 

fwrite(exportDepl,"StHel_Movebank_deployments.csv")



exportTracks<- tracks %>% mutate(TagID=exportDepl$TagID[match(Deployment_id,exportDepl$MinOfEncounter_ID)]) %>%
  mutate(AnimalID=exportDepl$Ring_Nr[match(Deployment_id,exportDepl$MinOfEncounter_ID)]) %>%
  mutate(DateTime=ymd_hms(paste(Date,format(Time,format="%H:%M:%S"))))
  
head(exportTracks)

#unique(exportTracks$Deployment_id) %in% exportDepl$MinOfEncounter_ID

fwrite(exportTracks,"StHel_Movebank_tracks.csv")

fwrite(exportTracks[10001:100000,],"StHel_Movebank_tracksTEST.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODIFY DATA TO MEET REQUIREMENTS FOR PROCESSING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# THERE ARE MULTIPLE DEPLOYMENTS PER NEST
# THERE ARE MULTIPLE DEPLOYMENTS PER RING NR
# THERE ARE MULTIPLE DEPLOYMENTS OF GPS TAGS
### none of these numbers alone are unique - the Deployment_id is the only unique number

unique(tracks$Species)
tracks$Species<-ifelse(tracks$Species=="MASBO","MABO",as.character(tracks$Species))
tracks$Species<-ifelse(tracks$Species=="REBTR","RBTB",as.character(tracks$Species))
deployments$species<-ifelse(deployments$species=="REBTR","RBTB",as.character(deployments$species))


### CREATE DEPLOYMENTS TABLE 
head(deployments)
head(retrievals)

retrievals<-retrievals %>% mutate(end=ymd_hms(paste(Date,format(Capture_time,format="%H:%M:%S")))) %>%
  filter(end>DATE_CUTOFF) %>%
  #filter(species %in% SPEC) %>%
  mutate(return_mass=weight) %>%
  dplyr::select(Ring_Nr,species,breeding_status,Nest_Nr,sex,GPS_ID,end,return_mass)

deployments<-deployments %>% mutate(start=ymd_hms(paste(Date,format(Release_time,format="%H:%M:%S")))) %>%
  filter(start>DATE_CUTOFF) %>%
  #filter(species %in% SPEC) %>%
  mutate(Longitude=nests$Longitude[match(Nest_Nr, nests$Nest_Nr)],Latitude=nests$Latitude[match(Nest_Nr, nests$Nest_Nr)]) %>%
  mutate(TAG=if_else(is.na(NUM_ON_LOG),as.character(GPS_ID),as.character(NUM_ON_LOG)),depl_mass=weight) %>%
  dplyr::select(Encounter_ID,Ring_Nr,species,breeding_status,Nest_Nr,sex,TAG, GPS_ID,start,depl_mass) %>%
  left_join(retrievals, by=c('Ring_Nr','species','breeding_status','Nest_Nr','sex','GPS_ID')) %>%
  mutate(season = ifelse(grepl(pattern="H",x=Nest_Nr, ignore.case = T),"Sept-Jan","Mar-Jul"))
deployments %>% filter(is.na(start))

### MODIFY TRACKS DATA
tracks<-tracks %>% mutate(DateTime=ymd_hms(paste(Date,format(Time,format="%H:%M:%S")))) %>%
  filter(DateTime>DATE_CUTOFF) %>%
  #filter(Species %in% SPEC) %>%
  mutate(ID=Deployment_id) %>%
  mutate(Season=deployments$season[match(ID,deployments$Encounter_ID)]) %>%
  mutate(breeding_status = ifelse(grepl(pattern="incub",x=breeding_status, ignore.case = T),"incubation","chick-rearing"))

dim(tracks)
head(tracks)
unique(tracks$Deployment_id)





### MODIFY NESTS DATA
depl.nests <- nests %>% filter(species %in% SPEC) %>% filter(Nest_Nr %in% deployments$Nest_Nr) %>%
  dplyr::select(Nest_Nr, Latitude, Longitude) %>%
  full_join(deployments, by="Nest_Nr") %>%
  rename(ID=Encounter_ID) %>%
  dplyr::select(ID, Latitude, Longitude) 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT THE DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xlow<-min(tracks$Longitude)-1.5
xup<-max(tracks$Longitude)+1.5
yup<-max(tracks$Latitude)+1.5
ylow<-min(tracks$Latitude)-1.5

### CREATE MULTIPANEL PLOT OF FORAGING TRIPS WITH INCOMPLETE TRIPS SHOWN AS DASHED LINE
tracks %>%
    arrange(Deployment_id,DateTime) %>%


#pdf("StHelena_MASP_tracks.pdf", width=10, height=6)  
ggplot(aes(x=Longitude, y=Latitude, col=breeding_status)) +
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
# PLOT THE CHICK-REARING 2019 DATA TO FIX tripSPlit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CREATE MULTIPANEL PLOT OF FORAGING TRIPS WITH INCOMPLETE TRIPS SHOWN AS DASHED LINE
example<-tracks %>%
  arrange(Deployment_id,DateTime) %>%
  filter(Deployment_id==1106) %>%
  mutate(TimeGap=as.numeric(DateTime-dplyr::lag(DateTime))) %>%
  #mutate(TimeGap=if_else(TimeGap<65,1,if_else(TimeGap<240,2,3))) %>%
  
  #pdf("StHelena_MASP_tracks.pdf", width=10, height=6)  
  ggplot(aes(x=Longitude, y=Latitude)) +
  geom_path() +
  geom_point(aes(x=Longitude, y=Latitude, size=TimeGap), col='firebrick') +
  geom_point(data=nests, aes(x=Longitude, y=Latitude), shape=16, size=1.5) +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=18), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_line("white",0), 
        panel.grid.minor = element_line("white",0), 
        panel.border = element_blank())



fwrite(example,"C:\\STEFFEN\\track2iba\\all_orig_dev_files\\example_data\\StHel_StormPetrel_tripSplit_example.csv")



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

multiTrips <- tripSplit(tracks[tracks$Deployment_id>1088,], Colony=colony, InnerBuff=10, ReturnBuff=25, Duration=0.25, plotit=T, rmColLocs = T)
shortTrips <- tripSplit(tracks[tracks$Deployment_id>1088,], Colony=colony, InnerBuff=2, ReturnBuff=25, Duration=0.25, plotit=T, rmColLocs = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE FORAGING TRIPS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("C:\\STEFFEN\\track2iba\\R\\tripSummary.r")
### SUMMARISE MAX DIST FROM COLONY AND TRIP TRAVELLING TIME FOR EACH TRIP
head(multiTrips)
trip_distancesMulti<-tripSummary(multiTrips, Colony=colony) %>%
  mutate(Species=deployments$species[match(ID, deployments$Encounter_ID)]) %>%
  mutate(Stage=deployments$breeding_status[match(ID, deployments$Encounter_ID)]) %>%
  mutate(Year=year(departure)) %>%
  dplyr::select(Species,Year, Stage, trip_id, departure, return, duration, max_dist, total_dist, n_locs,direction) %>%
  mutate(InnerBuff=10)
trip_distancesShort<-tripSummary(shortTrips, Colony=colony) %>%
  mutate(Species=deployments$species[match(ID, deployments$Encounter_ID)]) %>%
  mutate(Stage=deployments$breeding_status[match(ID, deployments$Encounter_ID)]) %>%
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







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT TO BIRDLIFE TRACKING DATABASE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## OUTPUT MUST BE IN THIS FORMAT

#names(export)<-c("BirdId","Sex","Age","Breed Stage","TrackId","DateGMT","TimeGMT","Latitude","Longitude","Equinox","ArgosQuality")
#head(export)

#Sex	limited choice	String	female	male 	unknown											
#Age	limited choice	String	adult	immature	juvenile	fledgling	unknown									
#Breed Stage	limited choice	String	pre-egg	incubation	brood-guard	post-guard	chick-rearing	creche	breeding	fail (breeding season)	migration	winter	sabbatical	pre-moult	non-breeding	unknown

head(tracks)

export <- tracks[,-(11)] %>% filter(Species=="MASPE") %>%
  mutate(breeding_status=ifelse(breeding_status %in% c("Incubation","INCUBATION","Incubating","INCUBATING ADULT"),"incubation",breeding_status)) %>%
  mutate(breeding_status=ifelse(breeding_status %in% c("Brood-guard","brood-guard"),"brood-guard","chick-rearing")) %>%
  mutate(Sex=ifelse(sex %in% c("male", "female"),as.character(sex),"unknown")) %>%
  mutate(DateGMT=format(Date, format="%d/%m/%Y")) %>%
  mutate(TimeGMT=format(Time, format="%H:%M:%S")) %>%
  mutate(BirdId=deployments$Ring_Nr[match(ID, deployments$Encounter_ID)]) %>%
  #mutate(Age=ifelse(age==2,"adult","fledgling")) %>%
  mutate(Age="adult") %>%
  select(BirdId,Sex,Age,breeding_status,ID,DateGMT,TimeGMT,Latitude,Longitude)
  names(export)<-c("BirdId","Sex","Age","Breed Stage","TrackId","DateGMT","TimeGMT","Latitude","Longitude")
head(export)
dim(export)
write.table(export,"MASPE_SeabirdTrackDB_1401.csv", row.names=F, sep=",")

export <- tracks[,-(11)] %>% filter(Species=="BRONO") %>%
  mutate(breeding_status=ifelse(breeding_status %in% c("Incubation","INCUBATION","Incubating","INCUBATING ADULT"),"incubation",breeding_status)) %>%
  mutate(breeding_status=ifelse(breeding_status %in% c("Brood-guard","brood-guard"),"brood-guard","chick-rearing")) %>%
  mutate(Sex=ifelse(sex %in% c("male", "female"),as.character(sex),"unknown")) %>%
  mutate(DateGMT=format(Date, format="%d/%m/%Y")) %>%
  mutate(TimeGMT=format(Time, format="%H:%M:%S")) %>%
  mutate(BirdId=deployments$Ring_Nr[match(ID, deployments$Encounter_ID)]) %>%
  #mutate(Age=ifelse(age==2,"adult","fledgling")) %>%
  mutate(Age="adult") %>%
  select(BirdId,Sex,Age,breeding_status,ID,DateGMT,TimeGMT,Latitude,Longitude)
names(export)<-c("BirdId","Sex","Age","Breed Stage","TrackId","DateGMT","TimeGMT","Latitude","Longitude")
head(export)
dim(export)
write.table(export,"BRONO_SeabirdTrackDB_1400.csv", row.names=F, sep=",")



