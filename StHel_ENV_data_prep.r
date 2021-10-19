############################################################################################################
#######  St HELENA SEABIRD ENVIRONMENT DATA PREPARATION  ###############################################
############################################################################################################
## adapted from Ascension Frigatebird analysis on 16 Nov 2020
## ENVDAT downloads from movebank for all tracking data and for gridded background data



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(data.table)
require(maps)
require(mapdata)
require(maptools)
require(geosphere)
require(sp)
require(sf)
require(rgdal)
require(rgeos)
library(raster)
library(trip)
library(marmap)
select<-dplyr::select
filter<-dplyr::filter

setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			LOAD TRACKING DATA WITH ENVIRONMENTAL VARIABLES FROM MOVEBANK			   ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

bird.tracks<-fread("St Helena seabird tracking-5605684338234142518.csv")
names(bird.tracks)
dim(bird.tracks)
head(bird.tracks)


#create Spatial Points Data Frame 
BIRDLOCS<-bird.tracks %>% mutate(year=year(timestamp), month=month(timestamp)) %>%
  mutate(period=ifelse(month>7,"hot","cool")) %>%
  mutate(period=paste(period, year, sep="_")) %>%
  select(timestamp,`location-long`,`location-lat`,`individual-taxon-canonical-name`,`tag-local-identifier`,period) %>%
  rename(LAT=`location-lat`,LONG=`location-long`)
coordinates(BIRDLOCS)= ~ LONG + LAT




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GET BATHYMETRY DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bathymap<-getNOAA.bathy(lon1=-11.5, lon2=3.5, lat1=-24, lat2=-9, resolution=1)
bathgrid<-expand.grid(as.numeric(dimnames(bathymap)[[1]]),as.numeric(dimnames(bathymap)[[2]]))
bathgrid$depth<- as.vector(bathymap)
names(bathgrid)[1:2]<-c("Long","Lat")
head(bathgrid)




####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			LOAD ENVIRONMENTAL VARIABLES FROM MOVEBANK BACKGROUND				   ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
## this creates a file with 35 million rows, which is too big
## calculating means in Raster Stack: https://gis.stackexchange.com/questions/222291/extracting-mean-of-multiple-raster-layers-using-r
## for each of 4 tracking periods we average the raster values

seasons<-data.frame(season='hot_2017',file=seq(1,24)) %>%
  bind_rows(data.frame(season='cool_2018',file=seq(25,41))) %>%
  bind_rows(data.frame(season='hot_2018',file=seq(42,61))) %>%
  bind_rows(data.frame(season='cool_2019',file=seq(62,92)))

setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\ENVBackground")
subfolders<-list.dirs()
env.bgr<-data.frame()
env.bird<-data.frame()

for (d in c(2:17,20,22:length(subfolders))){
  
  setwd(paste("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds/ENVBackground",subfolders[d],sep=""))

  ## list all the tif files
  grid.files<-list.files(pattern=".tif")
  env.bg.add<-data.frame()
  env.bird.add<-data.frame()
  
  ## read in all files
  ## 1-24 = 'hot_2017', 25-42='cool_2018', 43-63='hot_2018', 64-92='cool_2019'
  
  for (s in unique(seasons$season)){
  
  seasonfiles<-grid.files[seasons$file[seasons$season==s]]
  #for (f in seasons$file[seasons$season]){
    
    ## EXTRACT DATE FROM FILE NAME
    #regexp <- "[[:digit:]]+"
    #adjfilename<-str_replace(grid.files[f], pattern=c("2 m|10 m|4km|8d|AR 1|3deg|5d"), replace="") ### this needs to be adjusted for some file/folder names that have 2m 8 d 10 m etc. in the name!
    #filedate<-ymd_hms(substr(str_extract(adjfilename, regexp),1,14)) 
    
    ## READ IN DATA
    STACK1 <- stack(seasonfiles)
    season.mean <- calc(STACK1, fun = mean, na.rm = T)
    #dat.in<-raster(grid.files[f])
    out<-raster::as.data.frame(season.mean,xy=TRUE)
    head(out)

    ## ADD THE VARIABLE AND THE DATE
    names(out)<-c("Long","Lat","value")
    env.bg.add<-out %>%
      mutate(variable=substr(subfolders[d],3,nchar(subfolders[d]))) %>%
      #mutate(time=filedate) %>%
      mutate(season=s) %>%
      bind_rows(env.bg.add)
    
    ## EXTRACT THE ENV DATA FOR BIRD LOCATIONS
    bird.ras<-extract(season.mean,BIRDLOCS)
    BIRDLOCS$value<-raster::as.data.frame(bird.ras,xy=TRUE)
    env.bird.add<-as.data.frame(BIRDLOCS) %>%
      mutate(variable=substr(subfolders[d],3,nchar(subfolders[d]))) %>%
      mutate(season=s) %>%
      rename(value=bird.ras) %>%
      bind_rows(env.bird.add)

  }	# close loop over each season

env.bgr<-rbind(env.bgr,env.bg.add)
env.bird<-rbind(env.bird,env.bird.add)

} # close loop over each directory


head(env.bird)
dim(env.bgr)
unique(env.bgr$time)




## SAVE AND EXPORT DATA ##
setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\ENVBackground")
save.image("StHel_EnvDat_Background.RData")
fwrite(env.bgr,"StHel_EnvDat_Background.csv")
fwrite(env.bird,"StHel_EnvDat_BirdLocs.csv")

## SAVE ONLY MASP DATA
unique(env.bird$individual.taxon.canonical.name)
MASP<-env.bird %>% filter(`individual.taxon.canonical.name`=="Oceanodroma castro")
fwrite(MASP,"StHel_EnvDat_MASPLocs.csv")





####### ABANDONED CODE - AVERAGING ACROSS SEASONS WHEN EACH FILE IS READ IN ONE BY ONE ##########
# 
# ##### AVERAGE ALL THE ENVIRONMENTAL VARIABLES FOR SEVERAL TIME PERIODS ###
# head(env.bgr)
# 
# env.per<- env.bgr %>% mutate(year=year(time), month=month(time)) %>%
#   mutate(period=ifelse(month>7,"hot","cool")) %>%
#   mutate(period=paste(period, year, sep="_"))
# head(env.per)
# 
# 
# env.sum<-env.per %>% group_by(Long,Lat,period,variable) %>%
#   summarise(value=mean(value, na.rm=T))
# dim(env.sum)
# fwrite(env.sum,"StHel_EnvDat_Background.csv")
