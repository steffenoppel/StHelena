############################################################################################################
#######  SEABIRD GPS LOGGER DATA MANAGEMENT  ###################################################################
############################################################################################################

##### RUN THE FOLLOWING SCRIPT TO CONVERT RAW CSV FILES TO UTM COORDINATES AND GOOGLE EARTH FILES  #########
##### CREATE ONE FILE WITH ALL TRACK RECORDS TO IMPORT INTO THE ACCESS DATABASE  #########

##### WARNING: THIS SCRIPT WILL ONLY WORK FOR FILE NAMES THAT ARE EXACTLY AS SPECIFIED IN THE INSTRUCTIONS!
##### NEEDS MANUAL ADJUSTMENT WHEN SPECIES CODE FLIPS FROM 4 to 5 DIGIT ABBREVIATION!!!

### modified 11 Oct 2013 to allow 5-letter species code
### modified 21 January 2014 to allow for longer file names
### modified 12 March 2015 to import 2015 data
### modified 17 JANURAY 2016 to import 2015 data

### updated 16 July 2018 to import .pos data from PATHTRACK loggers
### updated 16 March 2019 to import .pos data from PATHTRACK loggers
## major change necessary as 2 of the 3 BRONO deployments did not show in query Logger_deployments for unknown reasons



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFY SPECIES AND YEARS OF INTEREST TO AVOID DEALING WITH OLD DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATE_CUTOFF<-ymd("2018-11-01")
SPEC<-c("BRONO","MASPE")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NO MODIFICATIONS NECESSARY BELOW THIS LINE, EXCEPT WORKING DIRECTORIES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(geosphere)
library(RODBC)
library(sp)
library(shapefiles)
library(rgdal)
library(maptools)
library(lubridate)
library(tidyverse)
library(dplyr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DEPLOYMENT DATA FROM DATABASE AND MANIPULATE INTO SINGLE TABLE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## run the RODBC import in a 32-bit version of R
setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")
#setwd("A:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\RODBC_seabird_data2.r")), wait = TRUE, invisible = FALSE)
load("DEPLOYMENT_DATA.RData")

### Load data from database
# 
# setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")
# db <- odbcConnectAccess2007('StHelena_seabird_tracking.accdb')
# deployments <- sqlQuery(db, "SELECT * FROM deployments")
# retrievals <- sqlQuery(db, "SELECT * FROM retrievals")
# nests <- sqlQuery(db, "SELECT * FROM deployments")
# odbcClose(db)

### Convert Dates and Times and format start and end times for each deployment
head(deployments)
head(retrievals)

retrievals<-retrievals %>% mutate(end=ymd_hms(paste(Date,format(Capture_time,format="%H:%M:%S")))) %>%
	filter(end>DATE_CUTOFF) %>%
	filter(species %in% SPEC) %>%
	mutate(return_mass=weight) %>%
	dplyr::select(Ring_Nr,species,breeding_status,Nest_Nr,sex,end,return_mass)

deployments<-deployments %>% mutate(start=ymd_hms(paste(Date,format(Release_time,format="%H:%M:%S")))) %>%
	filter(start>DATE_CUTOFF) %>%
	filter(species %in% SPEC) %>%
	mutate(Longitude=nests$Longitude[match(Nest_Nr, nests$Nest_Nr)],Latitude=nests$Latitude[match(Nest_Nr, nests$Nest_Nr)]) %>%
	mutate(TAG=if_else(is.na(NUM_ON_LOG),as.character(GPS_ID),as.character(NUM_ON_LOG)),depl_mass=weight) %>%
	dplyr::select(Encounter_ID,Ring_Nr,species,breeding_status,Nest_Nr,sex,TAG, start,depl_mass) %>%
	left_join(retrievals, by=c('Ring_Nr','species','breeding_status','Nest_Nr','sex'))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# READ AND PROCESS LOGGER DATA FROM EACH POS FILE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## SET THE WORK ENVIRONMENT WHERE THE DATA ARE STORED
# the existing two lines are Steffen's work and home computer paths, just add your own line for your computer
setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\RAW_LOG_FILES")
#setwd("A:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\RAW_LOG_FILES")

files<-list.files(pattern = "\\.pos$")


## CREATE FILE THAT WILL HOLD ALL OUTPUT RECORDS
ASI_bird_tracks<-data.frame()	


## PROCESS THE DATA FOR EACH LOGGER FILE

for (i in 1:length(files)){
setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\RAW_LOG_FILES")
#setwd("A:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\RAW_LOG_FILES")
#setwd("C:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\RAW_LOG_FILES")

bird_track<-read.table(files[i], sep=",", header=F, skip=5)
names(bird_track)<-c("Day", "Month", "Year","Hour","Minute","Second","elapsed","SVs","Latitude","Longitude","Altitude","Accuracy_ind","Clock_offset","Voltage")
flength<-nchar(files[i])
bird_track<-bird_track[!is.na(bird_track$Latitude),] %>%
	filter(Latitude!=0) %>%
	mutate(Year=Year+2000) %>%
	mutate(GPS_ID=as.numeric(substr(files[i],flength-8,flength-4))) %>%
	mutate(Species=deployments$species[match(GPS_ID,deployments$TAG)]) %>%
	mutate(Nest=deployments$Nest_Nr[match(GPS_ID,deployments$GPS_ID)]) %>%
	mutate(ID=deployments$ID[match(GPS_ID,deployments$GPS_ID)]) %>%
	mutate(DateTime=ymd_hms(paste(paste(Year, Month,Day, sep="-"),paste(Hour, Minute,Second, sep=":"), sep=" "))) %>%
	mutate(Date=as.Date(DateTime, format = "%Y-%m-%d")) %>%
	mutate(Time=format(DateTime,format="%H:%M:%S")) %>%
	mutate(TrackTime=as.double(DateTime)) %>%
	arrange(DateTime)
bird_track$Sequence<-seq(1,dim(bird_track)[1],1)
head(bird_track)



### MATCHING THE DATA TO A BIRD FROM THE DEPLOYMENTS TABLE
### ENSURE THAT REPEAT DEPLOYMENTS OF THE SAME TAG USE THE RIGHT DEPLOYMENT ID BY PICKING DEPLOYMENT BASED ON MEDIAN DATE IN DATA
### [NOTE THAT THIS CAN FAIL IF THE RETRIEVED DATA ARE HIGHLY SKEWED AND THE MEDIAN LIES OUTSIDE THE DEPLOYMENT INTERVAL!!]
gooddepl<-deployments[median(bird_track$DateTime)>deployments$start & median(bird_track$DateTime)<deployments$end,]
gooddepl2<-deployments[min(bird_track$DateTime)>deployments$start & min(bird_track$DateTime)<deployments$end,] 
gooddepl3<-deployments[max(bird_track$DateTime)>deployments$start & max(bird_track$DateTime)<deployments$end,] 
if(dim(gooddepl2)[1]>dim(gooddepl)[1]){gooddepl<-gooddepl2}
if(dim(gooddepl3)[1]>dim(gooddepl)[1]){gooddepl<-gooddepl3}

if(unique(bird_track$GPS_ID) %in% gooddepl$TAG){}else{
gooddepl<-deployments[min(bird_track$datetime)<deployments$end,] 			### changed from median to min because of weird data in 2015 sept
}

bird_track$Deployment_id<-gooddepl$Encounter_ID[match(min(bird_track$GPS_ID), gooddepl$TAG)]
bird_track$start_date<-gooddepl$start[match(min(bird_track$GPS_ID), gooddepl$TAG)]
bird_track$Ring_Nr<-gooddepl$Ring_Nr[match(min(bird_track$GPS_ID), gooddepl$TAG)]


### DELETING IRRELEVANT DATA BEFORE OR AFTER ATTACHMENT ON BIRD
off<-gooddepl$end[match(min(bird_track$GPS_ID), gooddepl$TAG)]
on<-gooddepl$start[match(min(bird_track$GPS_ID), gooddepl$TAG)]
bird_track<-bird_track[!(bird_track$DateTime < on),]
bird_track<-bird_track[!(bird_track$DateTime > off),]

bird_track<- bird_track %>% dplyr::select(Deployment_id, GPS_ID, Ring_Nr, Species, Date, Time, Sequence, Latitude, Longitude, DateTime)


### CALCULATE DISTANCE AND FILTER LOCATIONS THAT HAVE TOO LARGE SPEED

for (l in 1:(dim(bird_track)[1]-1)){
if((l+1)>dim(bird_track)[1]) break() else x<- bird_track[l:(l+1),]
Distance = distm(c(x$Longitude[1], x$Latitude[1]), c(x$Longitude[2], x$Latitude[2]), fun=distHaversine)
Tdiff = as.numeric(x$DateTime[2]-x$DateTime[1])
Speed=(Distance/1000)/Tdiff
if(Speed>50){bird_track<-bird_track[-(l+1),]}		### remove locations if the distance is too large and a speed >50 km/h is required
}

bird_track<-bird_track %>%
	mutate(nextLong=dplyr::lag(Longitude),nextLat=dplyr::lag(Latitude)) %>%
	rowwise() %>%
	mutate(Distance = distm(c(Longitude, Latitude), c(nextLong, nextLat), fun=distHaversine)) %>%
	dplyr::select(Deployment_id, GPS_ID, Ring_Nr, Species, Date, Time, Sequence, Latitude, Longitude, Distance)

### CONVERT TO SPATIAL POINTS OBJECT ###

x<-SpatialPointsDataFrame(bird_track[,c(9,8)], bird_track, proj4string = CRS("+proj=longlat +datum=WGS84"))

### GENERATE KML AND SAVE GOOGLE EARTH FILE
out_kml<-sprintf("StHelena_%s_GPS%s.kml", unique(bird_track$Species),min(bird_track$GPS_ID))
layer<-sprintf("StHelena_%s_GPS%s", unique(bird_track$Species),min(bird_track$GPS_ID))
setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\GOOGLE_EARTH")
#setwd("A:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\GOOGLE_EARTH")
#setwd("C:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\GOOGLE_EARTH")
writeOGR(x, out_kml, layer, driver="KML", overwrite_layer=T)

#### ADD DATA TO THE DATAFRAME THAT HOLDS ALL RECORDS
ASI_bird_tracks<-rbind(ASI_bird_tracks, bird_track)
}


head(ASI_bird_tracks)
#fix(ASI_bird_tracks)
dim(ASI_bird_tracks)
str(ASI_bird_tracks)


##### WRITE TAble of collated output in the same column order as in the Access database for easy import

names(ASI_bird_tracks)[2]<-"GPS_id"
ASI_bird_tracks$GPS_id<-as.numeric(ASI_bird_tracks$GPS_id)

setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")
#setwd("A:\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")
write.table(ASI_bird_tracks, "SH_seabird_GPS_tracks2019.csv", sep=',', row.names=F)

#quit(save="no",status=0)