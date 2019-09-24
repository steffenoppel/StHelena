### ##############################################
### # ST HELENA SEABIRD NEST SURVIVAL ####
### ##############################################

# written by steffen.oppel@rspb.org.uk on 23 Sept 2019
# based on old code of Montserrat Oriole, St Helena Plover, and Aquatic Warbler
# Shaffer's logistic exposure method: https://rpubs.com/bbolker/logregexp


## EXPOSURE 42 days for incubation
## 70 days for chick rearing


### LOAD THE REQUIRED PACKAGES 
library(tidyverse)
library(data.table)
library(geosphere)
library(lubridate)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD NEST DATA FROM DATABASE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
setwd("C:/STEFFEN/RSPB/UKOT/StHelena/Science/Birds/seabirds/StHelena")
nests<- fread("Seabird_productivity.csv")
head(nests)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MANIPULATE DATA TO HAVE FATE IN ONE COLUMN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nests<-nests %>% filter(failed!="UNKNOWN") %>%
  filter(!(is.na(HATCHED) & is.na(fledged) & failed=="")) %>%
  mutate(SUCC = ifelse(failed=="TRUE",0,1)) %>%
  mutate(SUCC = ifelse(is.na(HATCHED) & is.na(fledged),0,SUCC)) %>%
  mutate(exposure=`HATCHING EXPOSURE` + `FLEDGING EXPOSURE`) %>%
  mutate(HATCH=ifelse(HATCHED=="TRUE",1,0)) %>%
  filter(!is.na(Year_Nest)) %>%
  rename(tracked=`tracked?`) %>%
  mutate(Year_Nest=as.factor(Year_Nest)) %>%
  mutate(tracked=ifelse(tracked=="TRUE",1,0)) %>%
  mutate(tracked=ifelse(is.na(tracked),0,tracked))

head(nests)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SIMPLE SUMMARIES FOR PAPER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## number of successful nests
nests %>% mutate(count=1) %>% group_by(Year_Nest,SEASON,TYPE) %>%
  summarise(n=sum(count), fledged=sum(SUCC), succ=mean(SUCC))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SIMPLE MAYFIELD NEST SUCCESS ESTIMATE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Mayfield_nest_success<-nests %>% mutate(count=1) %>% group_by(Year_Nest,SEASON,TYPE,tracked) %>%
  summarise(n=sum(count), fledged=sum(SUCC), succ=mean(SUCC)) %>%
  mutate(Mayfield_succ=0)


for (s in 1:dim(Mayfield_nest_success)[1]){
  
  x<- nests %>% filter(Year_Nest==Mayfield_nest_success$Year_Nest[s]) %>%
    filter(SEASON==Mayfield_nest_success$SEASON[s]) %>%
    filter(TYPE==Mayfield_nest_success$TYPE[s]) %>%
    mutate(fate=ifelse(SUCC==0,1,0))
  
  exposure<-sum(x$exposure)
  fate<-sum(x$fate)
  Mayfield_dsr<-1-(fate/exposure)
  Mayfield_nest_success$Mayfield_succ[s]<-Mayfield_dsr^(42+70)			### based on 42 days of incubation and 70 days for fledging
}
Mayfield_nest_success






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPECIFYING LOGISTIC EXPOSURE LINK FUNCTION (Shaffer 2004)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define logistic exposure family
# from https://rpubs.com/bbolker/logregexp

logexp <- function(exposure = 1)
{
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  ## FIXME: is there some trick we can play here to allow
  ##   evaluation in the context of the 'data' argument?
  linkinv <- function(eta)  plogis(eta)^exposure
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
    ## OR .Call(stats:::C_logit_mu_eta, eta, PACKAGE = "stats")
  }
  mu.eta <- function(eta) {       
    exposure * plogis(eta)^(exposure-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FITTING NEST SURVIVAL MODELS TO TEST FOR YEAR OR SEASON EFFECT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(lme4)
library(AICcmodavg)
library()

m0<-glmer(SUCC~1+(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))
m1<-glmer(SUCC~SEASON+(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))
m2<-glmer(SUCC~TYPE +(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))
m4<-glmer(SUCC~SEASON+TYPE+tracked+(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))
m5<-glmer(SUCC~tracked+(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))
m6<-glmer(SUCC~SEASON+TYPE+(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))
m8<-glmer(SUCC~TYPE+tracked+(1|Year_Nest), data=nests,family=binomial(link=logexp(exposure=nests$exposure)))


AIC_TABLE<-aictab(cand.set=list(m0,m1,m2,m4,m5,m6,m8),modnames=c('null','m1','m2','m4','m5','m6','m8'),sort = TRUE, c.hat = 1, second.ord = TRUE, nobs = NULL)
AIC_TABLE

## SUMMARY OF TOP MODEL
summary(m4)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREDICT NEST SURVIVAL FOR REPORTING MEAN AND CONFIDENCE INTERVAL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m4pred<-glm(SUCC~SEASON+TYPE+tracked, data=nests,family=binomial(link=logexp(exposure=nests$exposure)))

output<-nests %>%
  mutate(pred_succ=predict(m4pred, type='response')) %>%
  mutate(pred_se=predict(m4pred, type='response', se.fit=T)$se.fit) %>%
  group_by(SEASON,TYPE,tracked) %>%
  summarise(succ=mean(pred_succ), lcl=mean(pred_succ-1.96*pred_se),ucl=mean(pred_succ+1.96*pred_se))


write.table(output, "StHelena_MASPE_nest_survival_summary.csv",row.names=F, sep=',')





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRODUCE PLOT TO SHOW BREEDING SUCCESS DIFFERENCES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(output, aes(y=succ, x=SEASON,color=TYPE)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  facet_wrap(~tracked, ncol=1) +
  xlab("Year") +
  ylab("Estimated breeding success") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())


