############################################################################################################
#######  ST HELENA TROPICBIRD MARINE DISTRIBUTION ANALYSIS  ###################################################
############################################################################################################
## this analysis is based on BirdLife International marine IBA processing scripts
## updated by steffen oppel 29 Jan 2014
## included 2015 data on 13 March 2015
## updated 24 March 2016 for RBTB
## included new analysis for collaboration with Jacob Gonzalez Solis

## SPLIT INCUBATION AND CHICK REARING on 30 March 2016 for separate analysis

## ADDED ENVIRONMENTAL VARIABLES IN DECEMBER 2016 - NEED TO MATCH LOCATIONS BECAUSE TIMES FROM MOVEBANK LACK SECONDS

## Downloaded data from Movebank on 3 Jan 2017 to create new 'background' file for Movebank to download environmental variables contemporaneously to tracking data

## ADDED RBTB DATA FROM SENEGAL on 4 JAN 2017

## RE-RUN WITH TRIP SPLIT FOR SENEGAL AND MOVED DATA ANALYSIS TO SEPARATE SCRIPT

## END 9 Jan 2017 #########


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES AND CUSTOM SCRIPTS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(maps)
require(mapdata)
require(foreign)
require(maptools)
require(geosphere)
require(sp)
library(move)
library(rworldmap)
library(tidyverse)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IMPORT DATA FROM MOVEBANK
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MYlogin<-movebankLogin(username="Steffen",password="Fr1gateb1rd30")
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
envdatRAW<-read.table("RBTB_EnvData_2016.csv", header=T, sep=",")
head(envdatRAW)



envdat<-envdatRAW %>% #filter(individual.taxon.canonical.name=="Phaethon aethereus") %>%
  mutate(year=year(timestamp)) %>%
  rename(SSTnight=MODIS.Ocean.Aqua.OceanColor.4km.Daily.Nighttime.SST..Bands.31.32.,
    NPP=OSU.Ocean.NPP.0.083deg.Monthly.NPP,
    Wind_U=ECMWF.Interim.Full.Daily.SFC.Wind..10.m.above.Ground.U.Component.,
    CHLA=MODIS.Ocean.Aqua.OceanColor.4km.Monthly.Chlorophyll.A,
    Wind_V=ECMWF.Interim.Full.Daily.SFC.Wind..10.m.above.Ground.V.Component.,
    SST=MODIS.Ocean.Aqua.OceanColor.4km.Daily.Daytime.SST) %>%
  mutate(speed=sqrt(Wind_U^2+Wind_V^2))


head(envdat)
head(data)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SUMMARISE ENVIRONMENTAL DATA 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

envsummary<- envdat %>% group_by(individual.taxon.canonical.name,year) %>%
  summarise(SSTn=mean(SSTnight,na.rm=T),
            SSTd=mean(SST,na.rm=T),
            NPP=mean(NPP,na.rm=T),
            CHLA=mean(CHLA,na.rm=T),
            Wind_speed=mean(speed,na.rm=T))














####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			LOAD ENVIRONMENTAL VARIABLES FROM COPERNICUS				   ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

### added during revision on 9 July 2018
### script from Ascension adapted to St Helena and Senegal
#
library(raster)
library(ncdf4)





setwd("C:\\STEFFEN\\MANUSCRIPTS\\submitted\\RBTB_StHelena")

ncdfs<-list.files(pattern=".nc")


envdat2<-data.frame()

for (f in 2: length(ncdfs)){
  
  # open a NetCDF file
  ncin <- nc_open(ncdfs[f])
  print(ncin)
  
  #lon <- ncvar_get(ncin, "lon")-360			### longitudes are in degrees EAST, so we subtract 360 to get negative values
  #lat <- ncvar_get(ncin, "lat", verbose = F)
  
  lon <- ncvar_get(ncin, "longitude")			
  lat <- ncvar_get(ncin, "latitude", verbose = F)
  
  t <- ncvar_get(ncin, "time")
  tunits <- ncatt_get(ncin, "time", "units")
  start<-as.POSIXct("1950-01-01 00:00:00")		### convert times to dates - NEEDS MANUAL STAT DATE DEFINITION
  dates<-start+(t*3600)
  nt <- dim(t)
  
  ## CREATE A FLAT FILE WITH MONTH AS COLUMN
  lonlatDF <- expand.grid(lon, lat, dates)
  names(lonlatDF)<-c('long','lat','date')
  
  
  #### FOR EACH OF THE VARIABLES EXTRACT DATA ####
  
  CMEMS_DATA<-data.frame()
  
  varnames<-c("zos","mlotst","so","bottomT","uo","vo")
  colnames<-c("SSH","MixLayDep","Salin","botTemp","CurVel_U","CurVel_V")
  
  
  for (v in 1:length(varnames)){
    dname<-varnames[v]
    #dname<-"sokaraml"
    
    tmp.array <- ncvar_get(ncin, dname)
    dlname <- ncatt_get(ncin, dname, "long_name")
    dunits <- ncatt_get(ncin, dname, "units")
    fillvalue <- ncatt_get(ncin, dname, "_FillValue")		### extract the FillValue (=NA)
    tmp.array[tmp.array == fillvalue$value] <- NA			### replaces random fill value with NA
    tmp.vec.long <- as.vector(tmp.array)
    if(length(dim(tmp.array))>3){tmp.vec.long <- as.vector(tmp.array[,,1,])}
    tmp.df02 <- data.frame(cbind(lonlatDF, tmp.vec.long))
    head(tmp.df02)
    
    # SUMMARISE ENVIRONMENTAL VARIABLES FOR EACH MONTH
    
    out <- tmp.df02 %>% group_by(long,lat) %>%
      summarise(mean=mean(tmp.vec.long, na.rm=T))
    out$Var<-colnames[v]
    CMEMS_DATA<-rbind(CMEMS_DATA,as.data.frame(out))
  
    
  }	# close loop over each variable
  
  head(CMEMS_DATA)
  dim(CMEMS_DATA)

  nc_close(ncin)
  
  envdat2<-rbind(envdat2,CMEMS_DATA)
  
} # close loop over each ncdf file

head(envdat2)
dim(envdat2)



### CAST TO HAVE ONE COLUMN PER VARIABLE ###
envdat2<-as.data.frame(cast(envdat2,lat+long+PERIOD~Var, value='mean', fun.aggregate=min))





##### PREPARE TABLE FOR MOVEBANK DATA EXTRACTION ######

pers<-ddply(TRIPS, "PERIOD",summarise, n=sum(count), start=min(DateTime), end=max(DateTime), mid=mean(DateTime))
envdat2$midDate<-pers$mid[match(envdat2$PERIOD, pers$PERIOD)]

#write.table(envdat2,"ASI_FRIG_env_data_input.csv", row.names=F, sep=",")

write.table(pers,"clipboard", row.names=F, sep="\t")



