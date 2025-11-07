### ##################################################
### ST HELENA STORM PETREL ENVIRONMENTAL ANALYSIS
### written by steffen.oppel@vogelwarte.ch
### ##################################################

## requested by Annalea Beard on 7 October 2023
## very simple t-test, U-test, and bootstrap sample test comparison


## updated on 26 July 2025 to include multivariate analysis for revision

## updated on 6 Nov 2025 to refine multivariate analysis and expand it for revision


### Load libraries
library(ggplot2)
library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
filter<-dplyr::filter
select<-dplyr::select
require(png)
library(grid)
library(gtable)
require(jpeg)
library(magick)



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####     DATA IMPORT AND MANIPULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

try(setwd("C:/STEFFEN/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/STEFFEN/MANUSCRIPTS/in_prep/StHelenStorm"), silent=T)
data <- fread("MASPE_envdat_AVAILABLE_USED_FORAGINGLOCS.csv")
head(data)
unique(data$variable)
summary(data)
dim(data)
data<-data %>% select(-R6TFU) %>%
  filter(!is.na(value)) %>%
  filter(!(variable=="rainmm" & value<0))
dim(data)

## TWO COMPARISONS NEEDED: HOT vs COOL [subset AVAILABLE==0] and AVAILABLE 0 vs. 1 [for each of HOT and COOL separately]
## only the use - vs available is shown here - the used between seasons will work in the same way

imgSPe<-readPNG("stormpetrel.png")
spicon <- rasterGrob(imgSPe, interpolate=TRUE)

#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SET UP OUTPUT TABLE FOR WITHIN SEASON COMPARISON  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

output<-expand.grid(variable=unique(data$variable),season=unique(data$SEASON), mean_used=0,mean_avail=0,diff_mean=0, diff_lcl=0,diff_ucl=0,P=0) %>%
  mutate(simul=seq_along(variable))



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    start the loop over each USED VS AVAILABLE comparison   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
for (v in 1:max(output$simul)) {
  
  ### select subset of data
  x<-data %>%
    filter(variable==output$variable[v]) %>%
    filter(SEASON==output$season[v])
  
  ### write basic summaries in output table
  output$mean_used[v]<-mean(x$value[x$AVAILABLE==0])
  output$mean_avail[v]<-mean(x$value[x$AVAILABLE==1])
  
  ### quantify n of samples
  n<-length(x$value[x$AVAILABLE==0])
  
  ### perform simple non-parametric test
  output$P[v]<-wilcox.test(value~AVAILABLE, data=x)$p.value
  
  ### perform simple bootstrapping test to quantify the differences (if confidence interval includes 0 then NOT significant)
  used.samples <- matrix(sample(x$value[x$AVAILABLE==0], size = 10000 * n, replace = TRUE),10000, n)
  used.statistics <- apply(used.samples, 1, mean)
  avail.samples <- matrix(sample(x$value[x$AVAILABLE==1], size = 10000 * n, replace = TRUE),10000, n)
  avail.statistics <- apply(avail.samples, 1, mean)

  output$diff_mean[v]<-quantile(used.statistics-avail.statistics,0.5)
  output$diff_lcl[v]<-quantile(used.statistics-avail.statistics,0.025)
  output$diff_ucl[v]<-quantile(used.statistics-avail.statistics,0.975)
  
}










#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SET UP OUTPUT TABLE FOR BETWEEN SEASON COMPARISON  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

output2<-expand.grid(variable=unique(data$variable),mean_hot=0,mean_cool=0,diff_mean=0, diff_lcl=0,diff_ucl=0,P=0) %>%
  mutate(simul=seq_along(variable))



#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    start the loop over each SEASONAL comparison   ~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
for (v in 1:max(output2$simul)) {
  
  ### select subset of data
  x<-data %>% filter(AVAILABLE==0) %>%
    filter(variable==output2$variable[v])
  
  ### write basic summaries in output2 table
  output2$mean_hot[v]<-mean(x$value[x$SEASON=="HOT"])
  output2$mean_cool[v]<-mean(x$value[x$SEASON=="COOL"])
  
  ### quantify n of samples
  n<-min(length(x$value[x$SEASON=="HOT"]),length(x$value[x$SEASON=="COOL"]))
  
  ### perform simple non-parametric test
  output2$P[v]<-wilcox.test(value~SEASON, data=x)$p.value
  
  ### perform simple bootstrapping test to quantify the differences (if confidence interval includes 0 then NOT significant)
  hot.samples <- matrix(sample(x$value[x$SEASON=="HOT"], size = 10000 * n, replace = TRUE),10000, n)
  hot.statistics <- apply(hot.samples, 1, mean)
  cool.samples <- matrix(sample(x$value[x$SEASON=="COOL"], size = 10000 * n, replace = TRUE),10000, n)
  cool.statistics <- apply(cool.samples, 1, mean)
  
  output2$diff_mean[v]<-quantile(hot.statistics-cool.statistics,0.5)
  output2$diff_lcl[v]<-quantile(hot.statistics-cool.statistics,0.025)
  output2$diff_ucl[v]<-quantile(hot.statistics-cool.statistics,0.975)
  
}




fwrite(output2,"MASPE_Table2_addition.csv")












#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SIMPLE RANDOM FOREST COMPARISON BETWEEN SEASON   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
library(ranger)
library(caret)
foragedat<- data %>% filter(AVAILABLE==0) %>% select(-AVAILABLE) %>%
  pivot_wider(., id_cols=c(Trip_id,Longitude, Latitude, YEAR, Date_Time,SEASON,STAGE),
              names_from=variable, values_from=value) %>%
  mutate(HOT=ifelse(SEASON=="HOT",1,0))



RF2 <- ranger::ranger(as.factor(HOT) ~ Rainmm+Cloud+windspeed+AIRTEMPC+SSTC+CHLA1+WaveDirection+WaveHeight+winddir,
                      data=foragedat, mtry=3, num.trees=1500, replace=T, importance="permutation", oob.error=T)
RF2

IMP<-as.data.frame(RF2$variable.importance) %>%
  dplyr::mutate(variable=names(RF2$variable.importance)) %>%
  dplyr::rename(red.accuracy=`RF2$variable.importance`) %>%
  dplyr::arrange(dplyr::desc(red.accuracy)) %>%
  dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  dplyr::select(variable,red.accuracy,rel.imp)


#### classification success of training data

PRED<-stats::predict(RF2,data=foragedat, type = "response")
foragedat <- foragedat %>%
  mutate(pred=PRED$predictions)
caret::confusionMatrix(data = factor(foragedat$HOT), reference = factor(foragedat$pred))






#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SIMPLE RANDOM FOREST COMPARISON WITHIN SEASON   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

foragedat<- data %>% 
  pivot_wider(., id_cols=c(Trip_id,Longitude, Latitude, YEAR, Date_Time,SEASON,STAGE,AVAILABLE),
              names_from=variable, values_from=value)


# ABANDONED DURING REVISION ON 6 November 2025
# RF3 <- ranger::ranger(as.factor(AVAILABLE) ~ SEASON+Rainmm+Cloud+windspeed+AIRTEMPC+SSTC+CHLA1+WaveDirection+WaveHeight+winddir,
#                       data=foragedat, mtry=3, num.trees=1500, replace=T, importance="permutation", oob.error=T)
# RF3
# 
# IMP2<-as.data.frame(RF3$variable.importance) %>%
#   dplyr::mutate(variable=names(RF3$variable.importance)) %>%
#   dplyr::rename(red.accuracy=`RF3$variable.importance`) %>%
#   dplyr::arrange(dplyr::desc(red.accuracy)) %>%
#   dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
#   dplyr::select(variable,red.accuracy,rel.imp)
# 
# 
# #### classification success of training data
# 
# PRED<-stats::predict(RF3,data=foragedat, type = "response")
# foragedat <- foragedat %>%
#   mutate(pred=PRED$predictions)
# caret::confusionMatrix(data = factor(foragedat$AVAILABLE), reference = factor(foragedat$pred))
# 
# IMP2





### create Table S3
variables<- tibble(variable=c("SEASON","AIRTEMPC","Cloud","Rainmm","winddir","windspeed","CHLA1","SSTC","WaveDirection","WaveHeight"),
                   EnvironmentalVariable=c("Season (hot/cool)","Air temperature (°C)","Total cloud cover (0-1)","Rain (mm)","Wind direction (º)","Wind speed (m/s)","Chlorophyll a concentration (mg m^-3)","Sea surface temperature (°C)","Wave direction (º)","Wave height (m)")
)




#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####    SIMPLE RANDOM FOREST COMPARISON WITHIN EACH SEASON (2 separate models)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
#####
#####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########


## SEPARATE MODELS FOR EACH SEASON TO SHOW DIFFERENT HABITAT SELECTION

RF_hot <- ranger::ranger(as.factor(AVAILABLE) ~ Rainmm+Cloud+windspeed+AIRTEMPC+SSTC+CHLA1+WaveDirection+WaveHeight+winddir,
                      data=foragedat[foragedat$SEASON=="HOT",], mtry=3, num.trees=1500, replace=T, importance="permutation", oob.error=T, probability=T)
RF_hot

IMP_hot<-as.data.frame(RF_hot$variable.importance) %>%
  dplyr::mutate(variable=names(RF_hot$variable.importance)) %>%
  dplyr::rename(red.accuracy=`RF_hot$variable.importance`) %>%
  dplyr::arrange(dplyr::desc(red.accuracy)) %>%
  dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  dplyr::select(variable,red.accuracy,rel.imp)


#### classification success of training data

PRED<-stats::predict(RF_hot,data=foragedat, type = "response")
foragedat <- foragedat %>%
  mutate(pred=ifelse(PRED$predictions[,2]<0.5,0,1))
caret::confusionMatrix(data = factor(foragedat$AVAILABLE[foragedat$SEASON=="HOT"]), reference = factor(foragedat$pred[foragedat$SEASON=="HOT"]))
caret::confusionMatrix(data = factor(foragedat$AVAILABLE[foragedat$SEASON=="COOL"]), reference = factor(foragedat$pred[foragedat$SEASON=="COOL"]))

IMP_hot






RF_cool <- ranger::ranger(as.factor(AVAILABLE) ~ Rainmm+Cloud+windspeed+AIRTEMPC+SSTC+CHLA1+WaveDirection+WaveHeight+winddir,
                         data=foragedat[foragedat$SEASON=="COOL",], mtry=3, num.trees=1500, replace=T, importance="permutation", oob.error=T, probability=T)
RF_cool

IMP_cool<-as.data.frame(RF_cool$variable.importance) %>%
  dplyr::mutate(variable=names(RF_cool$variable.importance)) %>%
  dplyr::rename(red.accuracy=`RF_cool$variable.importance`) %>%
  dplyr::arrange(dplyr::desc(red.accuracy)) %>%
  dplyr::mutate(rel.imp=(red.accuracy/max(red.accuracy))*100) %>%
  dplyr::select(variable,red.accuracy,rel.imp)


#### classification success of training data

PRED<-stats::predict(RF_cool,data=foragedat, type = "response")
foragedat <- foragedat %>%
  mutate(pred=ifelse(PRED$predictions[,2]<0.5,0,1))
caret::confusionMatrix(data = factor(foragedat$AVAILABLE[foragedat$SEASON=="HOT"]), reference = factor(foragedat$pred[foragedat$SEASON=="HOT"]))
caret::confusionMatrix(data = factor(foragedat$AVAILABLE[foragedat$SEASON=="COOL"]), reference = factor(foragedat$pred[foragedat$SEASON=="COOL"]))

IMP_cool





Table4<- IMP %>%
  select(variable, rel.imp) %>%
  rename(between=rel.imp) %>%
  left_join(IMP_cool, by="variable") %>%
  rename(cool=rel.imp) %>%
  left_join(IMP_hot, by="variable") %>%
  rename(hot=rel.imp) %>%
  select(variable, hot,cool, between) %>%
  left_join(variables, by="variable") %>%
  select(EnvironmentalVariable, between, hot, cool)
Table4
fwrite(Table4,"MASPE_multivariate_variable_importance_Table4.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##### CREATE A PARTIAL PLOT TO SHOW EFFECT OF MODEL  ############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
foragedat<-foragedat %>% mutate(LocType=ifelse(AVAILABLE==0,"foraging","unused"))
summary(foragedat$SSTC)

## for most flexibility construct your own
bins<-100  ## set a random number of bins (the more the longer it will take!)
pdp.plotdat<-bind_rows(rep(list(foragedat[foragedat$SEASON=="COOL",]),bins+1)) %>%   ## replicate the dataset bin times
  mutate(SSTC=rep(seq(19,26,(26-19)/bins), each=nrow(foragedat[foragedat$SEASON=="COOL",]))) %>% ## create a sequence of the variable of interest for bin intervals
  mutate(LocType=ifelse(AVAILABLE==0,"foraging","unused"))
pdp.plotdat$pred<-predict(RF_cool, data=pdp.plotdat)$predictions[,1] ## predict the model output for those data


coolplot<-pdp.plotdat %>% group_by(SSTC) %>%
  summarise(mean=mean(pred),lcl=quantile(pred, 0.025),ucl=quantile(pred, 0.975)) %>% ## summarise for each value of variable
  ggplot() +
  geom_line(mapping=aes(x=SSTC, y = mean),linewidth=2, colour="cornflowerblue")  +
  geom_ribbon(mapping=aes(x=SSTC,ymin=lcl, ymax = ucl),alpha=0.25, fill="cornflowerblue")  +
  geom_point(foragedat[foragedat$SEASON=="COOL",], mapping=aes(x=SSTC, y = (1-AVAILABLE)), color="grey27", size=0.2, position=position_jitter(width=0.2, height=0.05))  +
  labs(x ="Sea surface temperature (C)", y = "Probability to forage")  + 
  
  
  annotation_custom(grob=spicon, xmin=22, xmax=26, ymin=0.05, ymax=0.55) +
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=16),
        legend.position="none",
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"))




## for most flexibility construct your own
bins<-100  ## set a random number of bins (the more the longer it will take!)
pdp.plotdat<-bind_rows(rep(list(foragedat[foragedat$SEASON=="HOT",]),bins+1)) %>%   ## replicate the dataset bin times
  mutate(SSTC=rep(seq(19,26,(26-19)/bins), each=nrow(foragedat[foragedat$SEASON=="HOT",]))) %>% ## create a sequence of the variable of interest for bin intervals
  mutate(LocType=ifelse(AVAILABLE==0,"foraging","unused"))
pdp.plotdat$pred<-predict(RF_hot, data=pdp.plotdat)$predictions[,1] ## predict the model output for those data


hotplot<-pdp.plotdat %>% group_by(SSTC) %>%
  summarise(mean=mean(pred),lcl=quantile(pred, 0.025),ucl=quantile(pred, 0.975)) %>% ## summarise for each value of variable
  ggplot() +
  geom_line(mapping=aes(x=SSTC, y = mean),linewidth=2, colour="firebrick")  +
  geom_ribbon(mapping=aes(x=SSTC,ymin=lcl, ymax = ucl),alpha=0.25, fill="firebrick")  +
  geom_point(foragedat[foragedat$SEASON=="HOT",], mapping=aes(x=SSTC, y = (1-AVAILABLE)), color="grey27", size=0.2, position=position_jitter(width=0.2, height=0.05))  +
  labs(x ="Sea surface temperature (C)", y = "Probability to forage")  + 
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=16),
        legend.position="none",
        strip.text=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"))


library(ggpubr)
ggarrange(coolplot,hotplot, ncol=1, labels=c("cool season","hot season"),
          label.x = c(0.15,0.75),
          label.y = 0.95,
          hjust = 0,
          vjust = 1.5,
          font.label = list(size = 16, color = "black", face = "bold"))
