library(MASS)
library(robustbase)
library(imputeTS)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibbletime)
library(lubridate)
options(error = function() traceback(2))

# manually constructed data for Wien
#localWienFile <- "COVID-19-AT.local.Wien.csv"
#setwd("/home/at062084/DataEngineering/COVID-19/austria")
#col.names <- c("Stamp","Region","Status","Count")
#colClasses <- c(rep("character",3),"integer")
#dfWien <- read.csv(localWienFile, colClasses = colClasses, col.names=col.names, header=FALSE, sep=",", stringsAsFactors=FALSE) %>%
#  dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>% 
#  tidyr::spread(key=Status, value=Count) %>% 
#  dplyr::mutate(sumConfirmed=Confirmed, sumDeaths=0) %>%
#  dplyr::select(-Confirmed)

# OurWorldInData data
baseDir=("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/owid")
setwd(baseDir)
owidFile <- "./data/COVID-19-owid.csv"
system2("wget", paste("https://covid.ourworldindata.org/data/full_data.csv -O", owidFile))
# owid2
# https://ourworldindata.org/f43eeb43-de44-4d61-95bb-10e3b2a980c1



nRegDays <- 5
ggMinDate <- as.POSIXct("2020-02-22")
ggMaxDate <- as.POSIXct(as.character(Sys.Date()+days(nRegDays)))
#ggMinDate <- as.POSIXct("2020-02-24")
#ggMaxDate <- as.POSIXct("2020-03-15")
reg.maxit <- 50
reg.acc <- 1e-3


# -----------------------------------------------------------------------------------------------------------------------
# Prepare COVID-19 data into dateframe grided on Day since 2020-02-01
# -----------------------------------------------------------------------------------------------------------------------

col.names <- c("Stamp","Region","newConfirmed","newDeaths", "sumConfirmed","sumDeaths")
colClasses <- c("character","character",rep("numeric",4))
df <- read.csv(owidFile, colClasses = colClasses, col.names=col.names, header=TRUE, sep=",", stringsAsFactors=FALSE)
df <- df %>% 
  dplyr::mutate(Stamp=as.POSIXct(df$Stamp)) %>% 
  tidyr::replace_na(list(newConfirmed=0, newDeaths=0, sumConfirmed=0, sumDeaths=0)) %>%
  dplyr::select(-newConfirmed, -newDeaths)

# -------------------------------------
# rolling weighted robust regression
# -------------------------------------
#rrlm <- rollify(.f = function(Stamp, vals) {as.numeric(coef(MASS::rlm(log(vals)~Stamp, maxit=reg.maxit, acc=reg.acc))[2])*24*3600}, window=nRegDays)
rrlm <- rollify(.f = function(Stamp, vals) {as.numeric(coef(lm(log(vals)~Stamp))[2])*24*3600}, window=nRegDays)
rrlmcil <- rollify(.f = function(Stamp, vals) {as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,1])*24*3600}, window=nRegDays)
rrlmciu <- rollify(.f = function(Stamp, vals) {as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,2])*24*3600}, window=nRegDays)

#dr <- dx %>% group_by(Region) %>% mutate(RRLM = rrlm(Stamp, Confirmed)) 
#dr %>% dplyr::filter(RRLM>0.01) %>% ggplot(aes(x=Stamp, y=1/log10(exp(RRLM)))) + geom_line() + geom_point() + ylim(5,15) #ylim(0.5,2)

# -----------------------------------------------------------------------------------------------------------------------
# Calculate Confirmed rate, Death rate and Time to Death for several regions
# -----------------------------------------------------------------------------------------------------------------------
EU   <- c("Austria","Belgium","Bosnia and Herzegovina","Bulgaria","Croatia","Czech Republic","Estonia","Finland","France",
          "Germany","Gibraltar","Greece","Ireland","Italy","Latvia","Liechtenstein","Luxembourg","Macedonia","Netherlands",
          "Norway","Poland","Portugal","Romania","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")
EU_IT <- c("Austria","Belgium","Bosnia and Herzegovina","Bulgaria","Croatia","Czech Republic","Estonia","Finland","France",
          "Germany","Gibraltar","Greece","Ireland",        "Latvia","Liechtenstein","Luxembourg","Macedonia","Netherlands",
          "Norway","Poland","Portugal","Romania","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","United Kingdom")

# Construct two compound regions: EU and EUit (=EU without IT)
df.EU <- df %>% 
  dplyr::filter(Region %in% EU) %>% 
  dplyr::group_by(Stamp) %>% 
  dplyr::summarize(sumDeaths=sum(sumDeaths), sumConfirmed=sum(sumConfirmed)) %>%
  dplyr::mutate(Region="EU") %>% dplyr::select(Stamp,Region,sumConfirmed,sumDeaths)
df.EU_IT <- df %>% 
  dplyr::filter(Region %in% EU_IT) %>% 
  dplyr::group_by(Stamp) %>% 
  dplyr::summarize(sumDeaths=sum(sumDeaths), sumConfirmed=sum(sumConfirmed)) %>%
  dplyr::mutate(Region="EU_IT") %>% dplyr::select(Stamp,Region,sumConfirmed,sumDeaths)

# Append all data sets
df <- df %>% dplyr::bind_rows(df.EU) %>% 
  dplyr::bind_rows(df.EU_IT) %>% 
#  dplyr::bind_rows(dfWien) %>%
  dplyr::mutate(Region=factor(Region))



dfRegion <- data.frame(Region=c("Austria","Belgium","China","Denmark","EU","EU_IT","France","Germany","Italy","Iran",
                                "Japan", "Netherlands","Norway","Spain","South Korea","Sweden","Switzerland",
                                "United Kingdom","World"),
                      cutOffDate=as.POSIXct(paste0("2020-02-",c(25,29,1,27,20,25,26,25,21,23,10,28,27,10,27,27,26,24,1))),
                      Population=c(8.8,12,1368,6,512,452,67,83,60,82,127,17,5.3,47,51,10,8.5,66,8000))
dfRegion$ppmConfirmed <- 0
dfRegion$sumConfirmed <- 0
dfRegion$sumDeaths <- 0
dfRegion$MTD <- 0
dfRegion$expBeginC <- as.POSIXct("2020-01-01")
dfRegion$cRate <- 0
dfRegion$cRateMin <- 0
dfRegion$cRateMax <- 0
dfRegion$cRateFst <- 0
dfRegion$cRateLst <- 0
dfRegion$cRateCur <- 0
dfRegion$expBeginD <- as.POSIXct("2020-01-01")
dfRegion$dRate <- 0
dfRegion$dRateMin <- 0
dfRegion$dRateMax <- 0
dfRegion$dRateFst <- 0
dfRegion$dRateLst <- 0

# -------------------------------------------------------------------------------------------------------------
covRegion <- function(Regions="World", cutOffDate=as.POSIXct("2020-02-22"), Population=10e6, bPlot=TRUE) {
# -------------------------------------------------------------------------------------------------------------
  dfw <- df %>% 
    dplyr::filter(Region %in%  Regions) %>%
    dplyr::mutate(rrlmConfirmed = rrlm(Stamp,sumConfirmed)) %>%
    dplyr::mutate(rrlmConfirmedCIl = rrlmcil(Stamp,sumConfirmed)) %>%
    dplyr::mutate(rrlmConfirmedCIu = rrlmciu(Stamp,sumConfirmed))
    
  # df of Dates with sumDeaths > 0
  dfd <- dfw %>% dplyr::filter(sumDeaths>0)

  # Calculate MTD = MeanTimetoDeath as difference of Confirmed and Death count exponential increase start times
  # put Time Axis on y so we get time origine as intercept
  dfc <- dfw %>% filter(Stamp>=cutOffDate)
  rlmC <- lmrob(dfc$Stamp~log10(dfc$sumConfirmed),tol=1e-5)
  if (nrow(dfd)>2) {
    rlmD <- lmrob(dfd$Stamp~log10(dfd$sumDeaths),tol=1e-5)
    MTD <- as.POSIXct(coef(rlmD)[1], origin='1970-01-01') - as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
  } else {
    rlmD <- NA
    MTD <- NA
  }
  
  # sanitize MTD
  if (!is.na(MTD) && MTD >20)
    MTD <- NA

  # prepare datasets of first and last week to assess measures taken
  dfwFst <- dfw %>% dplyr::filter(Stamp < min(Stamp)+days(nRegDays))
  dfwLst <- dfw %>% dplyr::filter(Stamp > max(Stamp)-days(nRegDays))
  dfdFst <- dfd %>% dplyr::filter(Stamp < min(Stamp)+days(nRegDays))
  dfdLst <- dfd %>% dplyr::filter(Stamp > max(Stamp)-days(nRegDays))
  
  # Calculate infection rate per person per day with 95% confidence intervals
  rlmc <- lm(log(dfw$sumConfirmed)~dfw$Stamp)
  rlmc <- lm(log(dfw$sumConfirmed)~dfw$Stamp)
  rlmc <- lm(log(dfw$sumConfirmed)~dfw$Stamp)
  rlmcFst <- lm(log(dfwFst$sumConfirmed)~dfwFst$Stamp)
  rlmcLst <- lm(log(dfwLst$sumConfirmed)~dfwLst$Stamp)
  expBeginC <- as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
  cRate <- exp(coef(rlmc)[2]*24*3600)
  cRateMin <- tryCatch(exp(confint(rlmc)[2,1]*24*3600),finally={cRateMin=cRate})
  cRateMax <- tryCatch(exp(confint(rlmc)[2,2]*24*3600),finally={cRateMax=cRate})
  cRateFst <- exp(coef(rlmcFst)[2]*24*3600)
  cRateLst <- exp(coef(rlmcLst)[2]*24*3600)
  
  # Calculate death rate per person per day with 95% confidence intervals
  if (nrow(dfd)>2) {
    rlmd <- lm(log(dfd$sumDeaths)~dfd$Stamp)
    rlmdFst <- lm(log(dfdFst$sumDeaths)~dfdFst$Stamp)
    rlmdLst <- lm(log(dfdLst$sumDeaths)~dfdLst$Stamp)
    expBeginD <- as.POSIXct(coef(rlmD)[1], origin='1970-01-01')
    dRate <- exp(coef(rlmd)[2]*24*3600)
    dRateMin <- tryCatch(exp(confint(rlmd)[2,1]*24*3600),finally={dRateMin=dRate})
    dRateMax <- tryCatch(exp(confint(rlmd)[2,2]*24*3600),finally={dRateMax=dRate})
    dRateFst <- exp(coef(rlmdFst)[2]*24*3600)
    dRateLst <- exp(coef(rlmdLst)[2]*24*3600)
  } else {
    rlmd <- NA
    expBeginD <- NA
    dRate <- 0
    dRateFst <- 0
    dRateMax <- 0
    dRateLst <- 0
    dRateMin <- 0
  } 

  dfg <- dfw %>% 
    dplyr::select(-rrlmConfirmed,-rrlmConfirmedCIl, -rrlmConfirmedCIu) %>% 
    tidyr::gather(key=Status, value=Count, sumConfirmed, sumDeaths)

  lmLast3d <- lm(data=dfg %>% dplyr::filter(Stamp>(max(Stamp)-days(3))) %>% dplyr::filter(Status=="sumConfirmed"), log(Count)~Stamp)
  cRateCur <- exp(coef(lmLast3d)[2]*24*3600)
  c <- data.frame(round(max(dfw$sumConfirmed)/(Population*1e6)*1000000), max(dfw$sumConfirmed), max(dfw$sumDeaths), round(MTD,1), 
                  as.POSIXct(as.character(expBeginC, format="%Y-%m-%d")),round(1/log10(cRate),1),round(1/log10(cRateMin),1),round(1/log10(cRateMax),1),round(1/log10(cRateFst),1),round(1/log10(cRateLst),1),
                  round(1/log10(cRateCur),1),
                  as.POSIXct(as.character(expBeginD, format="%Y-%m-%d")),round(1/log10(dRate),1),round(1/log10(dRateMin),1),round(1/log10(dRateMax),1),round(1/log10(dRateFst),1),round(1/log10(dRateLst),1)
                  )
  
  if (bPlot) {
    print(paste0("saveImage: ./covid.",paste(Regions,collapse="-"),".",min(dfw$Stamp),".",max(dfw$Stamp)))
    ggplot(data=dfg, aes(x=Stamp, y=log10(Count), color=Status, shape=Status)) +
      geom_point(size=2.5) +  geom_line(linetype=1, size=.25)  +
      xlim(ggMinDate,ggMaxDate) + 
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp<min(Stamp)+days(nRegDays))}, 
                  method=lm, aes(color=Status, linetype="start6days"), fullrange=TRUE, se=FALSE) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype="last6days"), fullrange=TRUE, se=FALSE) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Status=="sumConfirmed") %>% dplyr::filter(Stamp>(max(Stamp)-days(3)))}, 
                  method=lm, aes(color=Status, linetype="last3days"), fullrange=TRUE, se=TRUE) +
      geom_point(data=dfw, mapping=aes(x=Stamp, y=1/log10(exp(rrlmConfirmed))/5), inherit.aes=FALSE) +
      geom_line(data=dfw, mapping=aes(x=Stamp, y=1/log10(exp(rrlmConfirmed))/5), inherit.aes=FALSE) +
      geom_line(data=dfw, mapping=aes(x=Stamp, y=1/log10(exp(rrlmConfirmedCIl))/5), inherit.aes=FALSE, linetype=2) +
      geom_line(data=dfw, mapping=aes(x=Stamp, y=1/log10(exp(rrlmConfirmedCIu))/5), inherit.aes=FALSE, linetype=2) +
      scale_y_continuous(limits=c(0,5), sec.axis = sec_axis(~ . *5, name="numDays to *10 Confirmed. 6 days regression [90% confInterval]")) +
      ggtitle(paste0("Region=",paste(Regions,collapse="-"), " Date=",max(dfw$Stamp), 
                     "  Population=",Population, "Mio Confirmed=",round(max(dfw$sumConfirmed)/(Population*1e6)*1000000),"ppm",
                     "  Confirmed*10-Last3d=",round(1/log10(exp(coef(lmLast3d)[2]*24*3600)),1),"d")) + 
      xlab(paste0("Confirmed=",max(dfw$sumConfirmed)," Deaths=",max(dfw$sumDeaths),
                  "  MTD=",round(MTD,1), "d   Confirmed*10-mean=",round(1/log10(cRate),1), "d  Deaths*10-mean=",round(1/log10(dRate),1),"d\n",
                  "Confirmed*10-Fst6Days=",round(1/log10(cRateFst),1), "d  Confirmed*10-Lst6Days=",round(1/log10(cRateLst),1), 
                  "d    Deaths*10-First6Days=",round(1/log10(dRateFst),1), "d  Deaths*10-Last6Days=",  round(1/log10(dRateLst),1),"d"))
        ggsave(filename=paste0(baseDir,"/thumbs/covid.",paste(Regions,collapse="-"),".",min(dfw$Stamp),".",max(dfw$Stamp),".png"),
               width=200, height=150, units="mm", scale=1 ,dpi=100)
        ggsave(filename=paste0(baseDir,"/plots/covid.",paste(Regions,collapse="-"),".",min(dfw$Stamp),".",max(dfw$Stamp),".png"),
               width=350, height=200, units="mm", scale=.8 ,dpi=150)
  }
  
  #  rateConf=", round(cRate,3)," rateDeath=",round(dRate,3)
  return(c)
}

# calculate properites for all defined regions
for (r in 1:nrow(dfRegion)) {
  Regions <- dfRegion$Region[r]
  Population <- dfRegion$Population[r]
  cutOffDate <- dfRegion$cutOffDate[r]
  cat(paste("r=",r,"  Region=", Regions, "  cutOffDate=",cutOffDate, "\n",sep=""))
  
  c <- tryCatch({covRegion(Regions=Regions, cutOffDate=cutOffDate, Population=Population)})
  if (!inherits(c, "error")) {
    dfRegion[r,4:dim(dfRegion)[2]] <- c
  }
  else {
    cat(paste("ERROR: r=",r,"  Region=",Regions, "\n",sep=""))
  }
}

dfWrite <- dfRegion %>% dplyr::select(Region,Population,ppmConfirmed,sumConfirmed, sumDeaths,MTD,cRateLst,cRateCur,dRateLst)
write.csv(dfWrite, file=paste0(baseDir,"/data/COVID-19.owid.",max(df$Stamp),".csv"))

