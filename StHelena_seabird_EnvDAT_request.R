############################################################################################################
#######  ST HELENA SEABIRD MARINE ENV DATA  ###################################################
############################################################################################################
## extract 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY SPECIES AND YEARS OF INTEREST TO AVOID DEALING WITH OLD DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(lubridate)
DATE_CUTOFF<-ymd("2015-01-01")
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

deployments<-deployments %>% mutate(start=ymd_hms(paste(MaxOfDate,format(MinOfRelease_time,format="%H:%M:%S")))) %>%
  filter(start>DATE_CUTOFF) %>%
  #filter(species %in% SPEC) %>%
  mutate(Longitude=nests$Longitude[match(Nest_Nr, nests$Nest_Nr)],Latitude=nests$Latitude[match(Nest_Nr, nests$Nest_Nr)]) %>%
  #mutate(TAG=if_else(is.na(NUM_ON_LOG),as.character(GPS_ID),as.character(NUM_ON_LOG)),depl_mass=weight) %>%
  rename(depl_mass=weight, Encounter_ID=MinOfEncounter_ID) %>%
  dplyr::select(Encounter_ID,Ring_Nr,species,breeding_status,Nest_Nr,sex,GPS_ID, start,depl_mass) %>%
  left_join(retrievals, by=c('Ring_Nr','species','breeding_status','Nest_Nr','sex','GPS_ID')) %>%
  mutate(season = ifelse(grepl(pattern="H",x=Nest_Nr, ignore.case = T),"Sept-Jan","Mar-Jul"))
deployments %>% filter(is.na(start))

### MODIFY TRACKS DATA
tracks<-tracks %>% mutate(DateTime=ymd_hms(paste(Date,format(Time,format="%H:%M:%S")))) %>%
  filter(DateTime>DATE_CUTOFF) %>%
  filter(Species %in% SPEC) %>%
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

#dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACT BOUNDING BOX AND DATE STAMPS FOR ENVDAT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
min(tracks$Longitude)
max(tracks$Longitude)
max(tracks$Latitude)
min(tracks$Latitude)

days<-as.character(unique(as.Date(tracks$DateTime)))
days<-paste(days," 12:00:00", sep="")
paste(days,collapse=",")

### SPECIFY pixels as 400 * 400

