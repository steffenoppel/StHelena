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

setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds")



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####~~~~~ 			LOAD TRACKING DATA WITH ENVIRONMENTAL VARIABLES FROM MOVEBANK			   ~~~~~~####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

bird.tracks<-fread("St Helena seabird tracking-5605684338234142518.csv")
names(bird.tracks)
dim(bird.tracks)



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

setwd("C:\\STEFFEN\\RSPB\\UKOT\\StHelena\\Science\\Birds\\seabirds\\ENVBackground")
subfolders<-list.dirs()
env.bgr<-data.frame()

for (d in c(2:17,20,22:length(subfolders))){
  
  setwd(paste("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds/ENVBackground",subfolders[d],sep=""))

  ## list all the tif files
  grid.files<-list.files(pattern=".tif")
  env.bg.add<-data.frame()
  
  ## read in all files
  for (f in 1:length(grid.files)){
    
    ## EXTRACT DATE FROM FILE NAME
    regexp <- "[[:digit:]]+"
    adjfilename<-str_replace(grid.files[f], pattern=c("2 m|10 m|4km|8d|AR 1|3deg|5d"), replace="") ### this needs to be adjusted for some file/folder names that have 2m 8 d 10 m etc. in the name!
    filedate<-ymd_hms(substr(str_extract(adjfilename, regexp),1,14)) 
    
    ## READ IN DATA
    dat.in<-raster(grid.files[f])
    out<-raster::as.data.frame(dat.in,xy=TRUE)
    head(out)

    ## ADD THE VARIABLE AND THE DATE
    names(out)<-c("Long","Lat","value")
    env.bg.add<-out %>%
      mutate(variable=substr(subfolders[d],3,nchar(subfolders[d]))) %>%
      mutate(time=filedate) %>%
      bind_rows(env.bg.add)
  }	# close loop over each file

env.bgr<-rbind(env.bgr,env.bg.add)

} # close loop over each directory


head(env.bgr)
dim(env.bgr)

fwrite(env.bgr,"StHel_EnvDat_Background.csv")


