
options(error = function() traceback(2))








nRegDays=5
Regions="Spain"
cutOffDate=as.POSIXct("2020-02-16")
cutOffCount=2
Population=67



# -------------------------------------
# rolling weighted robust regression
# -------------------------------------
rolm <-    rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(coef   (lm(log(vals)~Stamp))[2])*24*3600))}, window=nRegDays)
rolmcil <- rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,1])*24*3600))}, window=nRegDays)
rolmciu <- rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,2])*24*3600))}, window=nRegDays)

# -------------------------------------------------------------------------------------------------------------
covRegion.owid <- function(dr, Regions="World", cutOffDate=as.POSIXct("2020-02-22"), Population=10e6, bPlot=TRUE, nRegDays=5, cutOffCount=2) {
  # -------------------------------------------------------------------------------------------------------------

  ggMinDate <- as.POSIXct("2020-02-15")
  ggMaxDate <- max(dr$Stamp)+days(7)
  
  # Calculate MTD = MinTimetoDeath as difference of Confirmed and Death count exponential increase start times
  # put Time Axis on y so we get time origine as intercept
  dfc <- dr %>% filter(Stamp>=cutOffDate)
  rlmC <- lmrob(dfc$Stamp~log(dfc$Confirmed),tol=1e-5)
  
  dfd <- dr %>% dplyr::filter(Deaths>cutOffCount)
  if (nrow(dfd)>=nRegDays) {
    rlmD <- lmrob(dfd$Stamp~log(dfd$Deaths),tol=1e-5)
    MTD <- as.POSIXct(coef(rlmD)[1], origin='1970-01-01') - as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
    if (MTD>30) MTA <- NA
  } else {
    rlmD <- NA
    MTD <- NA
  }
  
  # calculate rolling estimate of spead of increase of confirmed cases 
  dfcr <- dfc %>% 
    dplyr::mutate(rolmConfirmed    = rolm(Stamp,Confirmed)) %>%
    dplyr::mutate(rolmConfirmedCIl = rolmcil(Stamp,Confirmed)) %>%
    dplyr::mutate(rolmConfirmedCIu = rolmciu(Stamp,Confirmed))
  dfcLast <- tail(dfcr$rolmConfirmed,1)
  c <- dfc %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+10),Stamp<=max(Stamp)-days(nRegDays))
  dfcPrev <-  1/log10(exp(as.numeric(coef(lm(log(Confirmed)~Stamp, data=c))[2])*24*3600))
  
  
  # calculate rolling estimate of spead of increase of fatalities
  dfdLast <- 0
  if (nrow(dfd)>=nRegDays) {
    dfdr <- dfd %>% 
      dplyr::mutate(rolmDeaths = rolm(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIl = rolmcil(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIu = rolmciu(Stamp,Deaths))
      dfdLast <- tail(dfdr$rolmDeaths,1)
  }
  d <- dfd %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+10),Stamp<=max(Stamp)-days(nRegDays))
  dfdPrev <-  1/log10(exp(as.numeric(coef(lm(log(Deaths)~Stamp, data=d))[2])*24*3600))
  
  # gather Confirmed, Deaths into Status
  dfg <- dfc %>% 
    tidyr::gather(key=Status, value=Count, Confirmed, Recovered, Deaths) %>%
    dplyr::filter(Count>cutOffCount) %>%
    dplyr::mutate(Count=log10(Count))

    
  c <- data.frame(round(max(dfw$sumConfirmed)/(Population*1e6)*1000000), max(dfw$sumConfirmed), max(dfw$sumDeaths), round(MTD,1), 
                  as.POSIXct(as.character(expBeginC, format="%Y-%m-%d")),round(1/log10(cRate),1),round(1/log10(cRateMin),1),round(1/log10(cRateMax),1),round(1/log10(cRateFst),1),round(1/log10(cRateLst),1),
                  round(1/log10(cRateCur),1),
                  as.POSIXct(as.character(expBeginD, format="%Y-%m-%d")),round(1/log10(dRate),1),round(1/log10(dRateMin),1),round(1/log10(dRateMax),1),round(1/log10(dRateFst),1),round(1/log10(dRateLst),1)
  )
  
  if (bPlot) {
    print(paste0("saveImage: ./covid.",paste(Regions,collapse="-"),".",min(dfw$Stamp),".",max(dfw$Stamp)))
    ggplot(data=dfg, aes(x=Stamp, y=Count, color=Status, shape=Status)) +
      geom_point(size=2.5) +  geom_line(linetype=1, size=.5)  +
      #stat_smooth(method=lm, fullrange=TRUE, se=FALSE,linetype=2, size=.4)+
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays))}, 
                  method=MASS::rlm, aes(color=Status, linetype="Last5Days"), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+10),Stamp<=max(Stamp)-days(nRegDays))}, 
                  method=MASS::rlm, aes(color=Status, linetype="Prev10Days"), fullrange=TRUE, se=FALSE, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/5), inherit.aes=FALSE, size=1, color="darkgrey") +
      geom_point(data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/5), inherit.aes=FALSE, size=1) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIl/5), inherit.aes=FALSE, linetype=3, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIu/5), inherit.aes=FALSE, linetype=3, size=.25) +
      scale_y_continuous(limits=c(0,5), sec.axis = sec_axis(~ . *5, name="numDays to *10 Confirmed. 5 days regression [90% confInterval]")) +
      xlim(ggMinDate,ggMaxDate) + 
      xlab(paste0("Confirmed=",max(dfc$Confirmed),"  Recovered=",max(dfc$Recovered), " Deaths=",max(dfc$Deaths),"  minTimeToDeath=",round(MTD,1), "d\n",
                  "Confirmed*10-Prev10Days=",round(dfcPrev,1), "d  Deaths*10-Prev10Days=",round(dfdPrev,1), "d\n", 
                  "Confirmed*10--Last5Days=",round(dfcLast,1), "d  Deaths*10--Last5Days=",round(dfdLast,1), "d")) +
      ggtitle(paste0("Region=",paste(Regions,collapse="-"), " Date=",max(dfg$Stamp), 
                     "  Population=",Population, "Mio Confirmed=",round(max(dfc$Confirmed)/Population),"ppm"))

    ggsave(filename=paste0(baseDir,"/thumbs/covid.",paste(Regions,collapse="-"),".",min(dfw$Stamp),".",max(dfw$Stamp),".png"),
           width=200, height=150, units="mm", scale=1 ,dpi=100)
    ggsave(filename=paste0(baseDir,"/plots/covid.",paste(Regions,collapse="-"),".",min(dfw$Stamp),".",max(dfw$Stamp),".png"),
           width=350, height=200, units="mm", scale=.8 ,dpi=150)
    
    #                   "Confirmed*10-mean=      ",round(dfcrLast,1), "d  Deaths*10-mean=      ",round(dfdrLast,1), "d\n",

  }
  
  return(c)
}



# -------------------------------------------------------------------------------------------------------------
covRegion.jhucsse <- function(dx, Regions="Italy", cutOffDate=as.POSIXct("2020-02-24"), bPlot=TRUE) {
  # -------------------------------------------------------------------------------------------------------------
  # df with valid data after cutOffDate
  dfw <- dx %>%
    dplyr::filter(Stamp >= cutOffDate) %>%
    dplyr::filter(Region %in%  Regions)  %>% 
    dplyr::group_by(Stamp) %>%
    dplyr::summarize(sumConfirmed = sum(Confirmed), sumRecovered=sum(Recovered), sumDeaths=sum(Deaths))
  
  # Calculate rolling robust regression
  dfw <- dfw %>% mutate(rrlmConfirmed = rrlm(Stamp,sumConfirmed)) 
  
  # df of Dates with sumDeaths > 0
  dfd <- dfw %>% dplyr::filter(sumDeaths>0)
  
  # Calculate MTD = MeanTimetoDeath as difference of Confirmed and Death count exponential increase start times
  # put Time Axis on y so we get time origine as intercept
  rlmC <- lmrob(dfw$Stamp~log10(dfw$sumConfirmed))
  if (nrow(dfd)>=2) {
    rlmD <- lmrob(dfd$Stamp~log10(dfd$sumDeaths))
    MTD <- as.POSIXct(coef(rlmD)[1], origin='1970-01-01') - as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
  } else {
    rlmD <- NA
    MTD <- NA
  }
  
  # prepare datasets of first and last wekk to assess measures taken
  dfwFst <- dfw %>% dplyr::filter(Stamp < min(Stamp)+days(nRegDays))
  dfwLst <- dfw %>% dplyr::filter(Stamp > max(Stamp)-days(nRegDays))
  dfdFst <- dfd %>% dplyr::filter(Stamp < min(Stamp)+days(nRegDays))
  dfdLst <- dfd %>% dplyr::filter(Stamp > max(Stamp)-days(nRegDays))
  
  # Calculate infection rate per person per day with 95% confidence intervals
  rlmc <- MASS::rlm(log(dfw$sumConfirmed)~dfw$Stamp)
  rlmcFst <- MASS::rlm(log(dfwFst$sumConfirmed)~dfwFst$Stamp)
  rlmcLst <- MASS::rlm(log(dfwLst$sumConfirmed)~dfwLst$Stamp)
  expBeginC <- as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
  cRate <- exp(coef(rlmc)[2]*24*3600)
  cRateMin <- tryCatch(exp(confint(rlmc)[2,1]*24*3600),finally={cRateMin=cRate})
  cRateMax <- tryCatch(exp(confint(rlmc)[2,2]*24*3600),finally={cRateMax=cRate})
  cRateFst <- exp(coef(rlmcFst)[2]*24*3600)
  cRateLst <- exp(coef(rlmcLst)[2]*24*3600)
  
  # Calculate death rate per person per day with 95% confidence intervals
  if (nrow(dfd)>=2) {
    rlmd <- MASS::rlm(log(dfd$sumDeaths)~dfd$Stamp)
    rlmdFst <- MASS::rlm(log(dfdFst$sumDeaths)~dfdFst$Stamp)
    rlmdLst <- MASS::rlm(log(dfdLst$sumDeaths)~dfdLst$Stamp)
    expBeginD <- as.POSIXct(coef(rlmd)[1], origin='1970-01-01')
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
  
  
  c <- data.frame(MTD,expBeginC,cRate,cRateMin,cRateMax,expBeginD,dRate,dRateMin,dRateMax)
  
  if (bPlot) {
    dfg <- dfw %>% dplyr::select(-rrlmConfirmed) %>% tidyr::gather(key=Status, value=Count, sumConfirmed, sumRecovered, sumDeaths)
    print(paste0("saveImage: ./covid.",paste(Regions,collapse="-"),".",cutOffDate,".",max(dfw$Stamp)))
    ggplot(data=dfg, aes(x=Stamp, y=log10(Count), color=Status, shape=Status)) +
      geom_point(size=2.5) +  geom_line(linetype=1, size=.25)  +
      #geom_smooth(method = MASS::rlm) + 
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp<min(Stamp)+days(nRegDays))}, 
                  method=MASS::rlm, aes(color=Status, linetype="start week"), fullrange=TRUE, se=FALSE) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays)))}, 
                  method=MASS::rlm, aes(color=Status, linetype="last week"), fullrange=TRUE, se=FALSE) +
      xlim(as.POSIXct("2020-02-24"),max(dfg$Stamp)) + 
      geom_line(data=dfw, mapping=aes(x=Stamp, y=1/log10(exp(rrlmConfirmed))/5), inherit.aes=FALSE) +
      scale_y_continuous(limits=c(0,5), sec.axis = sec_axis(~ . *5, name="numDays to *10 Confirmed. 5 days robust regression")) +
      ggtitle(paste0("Region=",paste(Regions,collapse="-")," cutOffDate=",cutOffDate, " Date=",max(dfw$Stamp))) + 
      xlab(paste0("Confirmed=",max(dfw$sumConfirmed)," Recovered=",max(dfw$sumRecovered)," Deaths=",max(dfw$sumDeaths),
                  "  MTD=",round(MTD,2), "d   Confirmed*10=",round(1/log10(cRate),1), "d  Deaths*10=",round(1/log10(dRate),1),"d\n",
                  "Confirmed*10-FirstWeek=",round(1/log10(cRateFst),1), "d  Confirmed*10-LstWeek=",round(1/log10(cRateLst),1), 
                  "d    Deaths*10-FirstWeek=",round(1/log10(dRateFst),1), "d  Deaths*10-LastWeek=",  round(1/log10(dRateLst),1)))
    ggsave(filename=paste0(baseDir,"thumbs/covid.",paste(Regions,collapse="-"),".",cutOffDate,".",max(dfw$Stamp),".png"),
           width=200, height=150, units="mm", scale=1 ,dpi=100)
    ggsave(filename=paste0(baseDir,"plots/covid.",paste(Regions,collapse="-"),".",cutOffDate,".",max(dfw$Stamp),".png"),
           width=350, height=200, units="mm", scale=.8 ,dpi=150)
  }
  
  #  rateConf=", round(cRate,3)," rateDeath=",round(dRate,3)
  return(c)
}



