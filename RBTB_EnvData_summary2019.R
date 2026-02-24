############################################################################################################
#######  ST HELENA SEABIRD MARINE ENVIRONMENT SUMMARY  ###################################################
############################################################################################################
## written by steffen oppel 10 Oct 2019
## Downloaded data from Movebank on 24 Sept 2019 (environmental variables contemporaneously to tracking data)

## updated on 26 Nov 2019 to include more environmental variables



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(move)
library(tidyverse)
library(lubridate)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select
library(rworldmap) # library rworldmap provides different types of global maps, e.g:
data(countriesLow)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IMPORT DATA FROM MOVEBANK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MYlogin<-movebankLogin(username="Steffen",password=xxxxxxxx)
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
setwd("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds")
envdatRAW<-fread("St Helena seabird tracking-5605684338234142518.csv")
head(envdatRAW)


### RENAME THE VARIABLES ###
envdat<-envdatRAW %>%
	select(-visible,-`study-name`,-`VIIRS Ocean Mapped OceanColor 4km 8d Euphotic Depth`,-`MODIS Ocean Aqua OceanColor 4km Monthly POC`) %>%  ### too many missing data
	mutate(timestamp=ymd_hms(timestamp)) %>%
  mutate(year=year(timestamp)) %>%
  rename(event_id=`event-id`,
         `taxon_canonical_name`=`individual-taxon-canonical-name`,
         tag_local_identifier=`tag-local-identifier`,
         location_lat=`location-lat`,
         location_long=`location-long`,
         Wind_U=`ECMWF Interim Full Daily SFC Wind (10 m above Ground U Component)`,
    Wind_V=`ECMWF Interim Full Daily SFC Wind (10 m above Ground V Component)`,
    CHLA=`MODIS Ocean Aqua OceanColor 4km Monthly Chlorophyll A (OCI)`,
    SST=`ECMWF Interim Full Daily SFC Sea Surface Temperature`,
	  Curr_V=`OSCAR 1/3deg 5d Surface Currents Meridional Velocity`,
	  Curr_U=`OSCAR 1/3deg 5d Surface Currents Zonal Velocity`,
	Uplift=`Movebank Thermal Uplift (from ECMWF)`,
	Rain=`ECMWF Interim Full Daily SFC-FC Total Precipitation`,
	Pressure=`ECMWF Interim Full Daily SFC Mean Sea Level Pressure`,
	AirTemp=`ECMWF Interim Full Daily SFC Temperature (Vertically Integrated)`,
	Charnock=`ECMWF Interim Full Daily SFC Charnock Parameter`,
	Sunshine=`ECMWF Interim Full Daily SFC-FC Sunshine Duration`,
	Cloud=`ECMWF Interim Full Daily SFC Total Cloud Cover`,
	WaveHeight=`ECMWF Interim Full Daily SFC Significant Wave Height`,
	WaveDirection=`ECMWF Interim Full Daily SFC Mean Wave Direction`) %>%
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
head(ALLDATA)
dim(ALLDATA)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REMOVE LOCATIONS ON LAND
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DAT<-SpatialPointsDataFrame(coords=ALLDATA[,3:4], proj4string=CRS("+proj=longlat +datum=WGS84"), data=ALLDATA)
proj4string(DAT)<-proj4string(countriesLow)
overlay<-over(DAT,countriesLow)
overlay<- overlay %>% mutate(index=seq(1:dim(overlay)[1])) %>%
  filter(is.na(NAME))
ALLDATA<- ALLDATA[overlay$index,]
dim(ALLDATA)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE ENVIRONMENTAL DATA USED BY EACH SPECIES, YEAR, AND SEASON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(ALLDATA)

envsummary<- ALLDATA %>% mutate(Year=year(timestamp)) %>%
  group_by(taxon_canonical_name,SEASON,Year) %>%
  summarise(SST=mean(SST,na.rm=T),
            CHLA=mean(CHLA,na.rm=T),
            Wind_speed=mean(windspeed,na.rm=T),
            Current_speed=mean(currentspeed,na.rm=T),
            Rain=mean(Rain,na.rm=T),
            Uplift=mean(Uplift,na.rm=T),
            AirTemp=mean(AirTemp,na.rm=T),
            Sunshine=mean(Sunshine,na.rm=T),
            Cloud=mean(Cloud,na.rm=T),
            WaveHeight=mean(WaveHeight,na.rm=T),
            WaveDirection=mean(WaveDirection,na.rm=T),
            Charnock=mean(Charnock,na.rm=T))


envsummary
fwrite(envsummary,"StHelena_seabirds_envdat_summary.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE ENVIRONMENTAL DATA USED BY EACH SPECIES, YEAR, AND SEASON
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(ALLDATA)

envsummary<- ALLDATA %>% mutate(Year=year(timestamp)) %>%
  group_by(taxon_canonical_name,SEASON,Year) %>%
  summarise(SST=mean(SST,na.rm=T),
            CHLA=mean(CHLA,na.rm=T),
            Wind_speed=mean(windspeed,na.rm=T),
            Current_speed=mean(currentspeed,na.rm=T),
            Rain=mean(Rain,na.rm=T),
            Uplift=mean(Uplift,na.rm=T),
            AirTemp=mean(AirTemp,na.rm=T),
            Sunshine=mean(Sunshine,na.rm=T),
            Cloud=mean(Cloud,na.rm=T),
            WaveHeight=mean(WaveHeight,na.rm=T),
            WaveDirection=mean(WaveDirection,na.rm=T),
            Charnock=mean(Charnock,na.rm=T))


envsummary
fwrite(envsummary,"StHelena_seabirds_envdat_summary.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CREATE PLOTS TO COMPARE ENVIRONMENTAL DATA AMONG SPECIES AND SEASONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



## PLOT FOR SEA SURFACE TEMPERATURE
Medians<-ALLDATA %>%
  group_by(taxon_canonical_name) %>%
  summarise(MED=median(SST-273.15, na.rm=T))


ggplot(ALLDATA) +
  geom_histogram(aes(x=SST-273.15,y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill=SEASON),alpha=0.5,binwidth=0.5)+
  #geom_line(aes(x=SST-273.15,col=SEASON),stat = "density") +
  facet_wrap(~taxon_canonical_name, ncol=2, scales = "fixed")+
  geom_vline(data=Medians, aes(xintercept=MED),colour="red", size=0.5, linetype=2) +
  ylab("Proportion of locations") +
  xlab("Sea surface temperature (C)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=14, color="black", angle=45, vjust=0.5), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))

ggsave("StHel_Seabird_SST_freq.pdf", width=8, height=8)





## PLOT FOR SUNSHINE DURATION

Medians<-ALLDATA %>%
  group_by(taxon_canonical_name) %>%
  summarise(MED=median(Sunshine, na.rm=T))


ggplot(ALLDATA) +
  geom_histogram(aes(x=Sunshine,y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill=SEASON),alpha=0.5,binwidth=1000)+
  #geom_line(aes(x=SST-273.15,col=SEASON),stat = "density") +
  facet_wrap(~taxon_canonical_name, ncol=2, scales = "fixed")+
  geom_vline(data=Medians, aes(xintercept=MED),colour="red", size=0.5, linetype=2) +
  ylab("Proportion of locations") +
  xlab("Daily sunshine duration (seconds)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=14, color="black", angle=45, vjust=0.5), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))

ggsave("StHel_Seabird_Sunshine_freq.pdf", width=8, height=8)





## PLOT FOR CHL A

Medians<-ALLDATA %>%
  group_by(taxon_canonical_name) %>%
  summarise(MED=median(CHLA, na.rm=T))


ggplot(ALLDATA) +
  geom_histogram(aes(x=CHLA,y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill=SEASON),alpha=0.5,binwidth=0.05)+
  #geom_line(aes(x=SST-273.15,col=SEASON),stat = "density") +
  geom_vline(data=Medians, aes(xintercept=MED),colour="red", size=0.5, linetype=2) +
  facet_wrap(~taxon_canonical_name, ncol=2, scales = "fixed")+
  ylab("Proportion of locations") +
  xlab("Chlorophyll a concentration") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=14, color="black", angle=45, vjust=0.5), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))

ggsave("StHel_Seabird_CHLA_freq.pdf", width=8, height=8)


