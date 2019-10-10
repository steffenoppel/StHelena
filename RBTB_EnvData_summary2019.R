############################################################################################################
#######  ST HELENA SEABIRD MARINE ENVIRONMENT SUMMARY  ###################################################
############################################################################################################
## written by steffen oppel 10 Oct 2019
## Downloaded data from Movebank on 24 Sept 2019 (environmental variables contemporaneously to tracking data)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(move)
library(tidyverse)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IMPORT DATA FROM MOVEBANK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MYlogin<-movebankLogin(username="Steffen",password="xxxx")
data<-getMovebankData(study=	964155864,login=MYlogin,
                 removeDuplicatedTimestamps=T)
ind<-getMovebankAnimals(study=	964155864,login=MYlogin)


head(data)
head(ind)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD ENVIRONMENTAL DATA AND MERGE WITH OTHER DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Load data from database
#setwd("S:/ConSci/DptShare/SteffenOppel/RSPB/UKOT/StHelena/Science/Birds/seabirds")
setwd("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds/TrackingData")
envdatRAW<-fread("St Helena seabird tracking-5528736674850573850.csv")
head(envdatRAW)



envdat<-envdatRAW %>%
	select(-visible) %>%
	mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(year=year(timestamp)) %>%
  rename(event_id=`event-id`,
	`taxon_canonical_name`=`individual-taxon-canonical-name`,
	tag_local_identifier=`tag-local-identifier`,
	location_lat=`location-lat`,
	location_long=`location-long`,
	SSTnight=`MODIS Ocean Aqua OceanColor 4km 8d Nighttime SST (Bands 22-23)`,
    NPP=`OSU Ocean NPP 0.083deg 8d NPP`,
    Wind_U=`ECMWF Interim Full Daily SFC-FC Wind (10 m above Ground U Component)`,
    CHLA=`MODIS Ocean Aqua OceanColor 4km 8d Chlorophyll A (OCI)`,
    Wind_V=`ECMWF Interim Full Daily SFC-FC Wind (10 m above Ground V Component)`,
    SST=`MODIS Ocean Aqua OceanColor 4km 8d Daytime SST`,
	Curr_V=`OSCAR 1/3deg 5d Surface Currents Meridional Velocity`,
	Curr_U=`OSCAR 1/3deg 5d Surface Currents Zonal Velocity`) %>%
  mutate(windspeed=sqrt(Wind_U^2+Wind_V^2), currentspeed=sqrt(Curr_U^2+Curr_V^2))








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MERGE INDIVIDUAL BIRD TRACKING AND ENVIRONMENTAL DATA 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(envdat)
head(data)
head(ind)


## MERGE TRACKING DATA WITH INDIVIDUAL INFO
#birddat<- data@data %>% select(location_lat,location_long,timestamp,deployment_id,event_id,tag_id) %>%
#	left_join(ind, by=c("tag_id","deployment_id")) %>%
#	select(-sensor_type_id,-comments,-death_comments,-earliest_date_born,-exact_date_of_birth,-latest_date_born,-tag_local_identifier,-local_identifier,-nick_name,-taxon_detail)
#head(birddat)

## MERGE BIRD DATA WITH ENVIRONMENTAL DATA
#ALLDATA<- birddat %>% left_join(envdat, by=c("event_id")) %>%

ALLDATA<- envdat %>% left_join(ind, by=c("tag_local_identifier","taxon_canonical_name")) %>%
	select(-sensor_type_id,-comments,-death_comments,-earliest_date_born,-exact_date_of_birth,-latest_date_born,-tag_local_identifier,-local_identifier,-nick_name,-taxon_detail) %>%
	mutate(SEASON= ifelse(month(timestamp) %in% c(9,10,11,12,1,2),"hot","cool"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE ENVIRONMENTAL DATA USED BY EACH SPECIES, YEAR, AND SEASON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### THIS WILL REQUIRE MORE WORK:
### (1) REMOVE LOCATIONS ON LAND
### (2) group by variables of interest (year?, season?)



head(ALLDATA)

envsummary<- ALLDATA %>% group_by(taxon_canonical_name,SEASON) %>%
  summarise(SSTn=mean(SSTnight,na.rm=T),
            SSTd=mean(SST,na.rm=T),
            NPP=mean(NPP,na.rm=T),
            CHLA=mean(CHLA,na.rm=T),
            Wind_speed=mean(windspeed,na.rm=T),
            Current_speed=mean(currentspeed,na.rm=T))


envsummary














