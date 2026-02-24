#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Modelling growth curves using HEPE model from steffen####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###INSTALL PACKAGES
library(RODBC)
library(AICcmodavg)
library(MuMIn)
library(maptools)
library(rgdal)
require(maps)
require(mapdata)
require(maptools)
library(ggmap)
library(move)
require(geosphere)
library(FlexParamCurve)
library(readxl)

###LOAD DATAFRAME
setwd("C://STEFFEN")# Set working directory 
growthsthcb <- read.csv("CHICK_GROWTH_CABOVERDEsh.csv") # Read in the csv file
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       2.1 ESTIMATE LOGISTIC GROWTH CURVES WITH FLEXPARAMCURVE FOR CABO VERDE AND STH #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

growthsthcb$WEIGHT<- as.numeric(growthsthcb$WEIGHT)
growthsthcb$CAVITY<- as.factor(growthsthcb$CAVITY)
summary(growthsthcb)

parameter.estimatescb<-data.frame()

par(mfrow=c(2,2),mar=c(2,4.5,0,1), oma=c(2,0,0,0))
ylabels<-c('Body mass (g)','Wing length (mm)','Head and bill (mm)','Culmen length (mm)')

for (m in 1:4){ 
  ## FIT GROWTH CURVE ##
  modpar(growthsthcb$ESTIMATED_AGE, growthsthcb[,m+3],pn.options = "massinits")
  if(m==1){M1 <- nls((WEIGHT ~ A/((1+m*exp(-k*(ESTIMATED_AGE-i)))^(1/m))), data=growthsthcb, start=list(A=massinits$Asym,m=massinits$M,k=massinits$K,i=massinits$Infl))}
  if(m==2){M1 <- nls((WING ~ A/((1+m*exp(-k*(ESTIMATED_AGE-i)))^(1/m))), data=growthsthcb, start=list(A=massinits$Asym,m=massinits$M,k=massinits$K,i=massinits$Infl))}
  if(m==3){M1 <- nls((HEAD_AND_BILL ~ A/((1+m*exp(-k*(ESTIMATED_AGE-i)))^(1/m))), data=growthsthcb, start=list(A=massinits$Asym,m=massinits$M,k=massinits$K,i=massinits$Infl))}
  if(m==4){M1 <- nls((FEATHERS_TO_BILL ~ A/((1+m*exp(-k*(ESTIMATED_AGE-i)))^(1/m))), data=growthsthcb, start=list(A=massinits$Asym,m=massinits$M,k=massinits$K,i=massinits$Infl))}
  
  outsum<-as.data.frame(summary(M1)$coefficients)
  outsum$variable<-names(growthsthcb)[m+3]
  outsum$param<-row.names(outsum)
  parameter.estimatescb<-rbind(parameter.estimatescb,outsum)
  
  ## create plot with lines
  sthcvpl<-growthsthcb[,c(m+3,8,3)]
  sthcvpl<- sthcvpl[!is.na( sthcvpl[,1]),]
  upylim<-as.integer(round(max( sthcvpl[,1]+20, na.rm =T),0))
  sthcvpl$color<-ifelse(sthcvpl$COLONY=="STH","firebrick","cornflowerblue")
  plot( sthcvpl[,1]~ESTIMATED_AGE, data= sthcvpl, pch=16, cex=0.5, col=color, ylim=c(0,upylim), xlim=c(0,110), axes=F, frame=F, xlab="", ylab=ylabels[m], cex.lab=1.2, mgp=c(3.3,0.8,0))#MPG SETS WHERE AXIS LABEL SITS IN RELATION TO GRAPH
  lines(predict(M1, newdat=data.frame(ESTIMATED_AGE=seq(0,100,1))), lty=2,lwd=2) # SET WHERE TO STOP THE PREDICTION LINE
  
  
  axis(2, at=seq(0,upylim,upylim/5), labels=T, cex.axis=1.0, las=1)
  axis(1, at=seq(0,as.numeric(max(growthsthcb$ESTIMATED_AGE, na.rm =T)),10), labels=T, cex.axis=1.0)
}

mtext("Age of chick (days)",1,1,outer=T, cex=1.2)