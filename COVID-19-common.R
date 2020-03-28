library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tibbletime)
library(scales)
options(error = function() traceback(2))


# -------------------------------------------------------------------------------------------------------------
covRegionPlot <- function(dr, Regions="World", cutOffDate=as.POSIXct("2020-02-22"), 
                          Population=10e6, bPlot=TRUE, nRegDays=5, nEstDays=10, nCutOff=2, baseDir=".",
                          ggMinDate=as.POSIXct("2020-02-15"), ggMaxDate=max(dr$Stamp)+days(7), filePrefix="") {
# -------------------------------------------------------------------------------------------------------------
  
  # rolling regression
  rolm <-    rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(coef   (lm(log(vals)~Stamp))[2])*24*3600))}, window=nRegDays)
  rolmcil <- rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,1])*24*3600))}, window=nRegDays)
  rolmciu <- rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,2])*24*3600))}, window=nRegDays)

  # Calculate MTD = MinTimetoDeath as difference of Confirmed and Death count exponential increase start times
  # put Time Axis on y so we get time origine as intercept
  dfc <- dr %>% 
    filter(Stamp>=cutOffDate) %>% 
    dplyr::filter(Confirmed>0)
  rlmC <- lmrob(dfc$Stamp~log(dfc$Confirmed),tol=1e-5)
  
  dfd <- dr %>% dplyr::filter(Deaths>nCutOff)
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
  # Calculate 2 points with different window
  dfcLast <- tail(dfcr$rolmConfirmed,1)
  c <- dfc %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+nEstDays),Stamp<=max(Stamp)-days(nRegDays))
  dfcPrev <-  1/log10(exp(as.numeric(coef(lm(log(Confirmed)~Stamp, data=c))[2])*24*3600))
  f <- dfc %>% dplyr::filter(Stamp<min(Stamp)+days(nEstDays))
  dfcFrst <-  1/log10(exp(as.numeric(coef(lm(log(Confirmed)~Stamp, data=f))[2])*24*3600))
  
  
  # calculate rolling estimate of spead of increase of fatalities
  dfdLast <- NA
  if (nrow(dfd)>=nRegDays) {
    dfdr <- dfd %>% 
      dplyr::mutate(rolmDeaths = rolm(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIl = rolmcil(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIu = rolmciu(Stamp,Deaths))
      dfdLast <- tail(dfdr$rolmDeaths,1)
  }
  
  # Calculate first period with different window size
  dfdFrst <- NA
  if(nrow(dfd)>=nEstDays) {  
    e <- dfd %>% dplyr::filter(Stamp<min(Stamp)+days(nEstDays))
    dfdFrst <-  1/log10(exp(as.numeric(coef(lm(log(Deaths)~Stamp, data=e))[2])*24*3600))
  } 
  
  # Calculate prev period with different window size
  dfdPrev <- NA
  if(nrow(dfd)>=(nRegDays+nEstDays)) {  
    d <- dfd %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+nEstDays),Stamp<=max(Stamp)-days(nRegDays))
    dfdPrev <-  1/log10(exp(as.numeric(coef(lm(log(Deaths)~Stamp, data=d))[2])*24*3600))
  } 
  
  # gather Confirmed, Deaths into Status
  dfg <- dfc %>% 
    tidyr::gather(key=Status, value=Count, Confirmed, Recovered, Deaths) %>%
    dplyr::filter(Count>nCutOff) %>%
    dplyr::mutate(Count=log10(Count))

    
  #c <- data.frame(round(max(dfw$sumConfirmed)/(Population*1e6)*1000000), max(dfw$sumConfirmed), max(dfw$sumDeaths), round(MTD,1), 
  #                as.POSIXct(as.character(expBeginC, format="%Y-%m-%d")),round(1/log10(cRate),1),round(1/log10(cRateMin),1),round(1/log10(cRateMax),1),round(1/log10(cRateFst),1),round(1/log10(cRateLst),1),
  #                round(1/log10(cRateCur),1),
  #                as.POSIXct(as.character(expBeginD, format="%Y-%m-%d")),round(1/log10(dRate),1),round(1/log10(dRateMin),1),round(1/log10(dRateMax),1),round(1/log10(dRateFst),1),round(1/log10(dRateLst),1)
  #)
  
  if (bPlot) {
    print(paste0("saveImage: ./covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"))
    gg <- ggplot(data=dfg, aes(x=Stamp, y=Count, color=Status, shape=Status)) +
      geom_point(size=2.5) +  geom_line(linetype=1, size=.5)  +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype=paste0("Last",nRegDays,"Days")), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays+nEstDays)),Stamp<=(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype=paste0("Prev",nEstDays,"Days")), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp<(min(Stamp)+days(nEstDays)))}, 
                  method=lm, aes(color=Status, linetype=paste0("Zero",nEstDays,"Days")), fullrange=TRUE, se=FALSE, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/5), inherit.aes=FALSE, size=1, color="darkgrey") +
      geom_point(data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/5), inherit.aes=FALSE, size=1) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIl/5), inherit.aes=FALSE, linetype=3, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIu/5), inherit.aes=FALSE, linetype=3, size=.25) +
      scale_y_continuous(limits=c(0,5), sec.axis = sec_axis(~ . *5, name=paste0("numDays to *10 Confirmed. ",nRegDays," days rolling regression [90% confInterval]"))) +
      xlim(ggMinDate,ggMaxDate) + 
      xlab(paste0("Confirmed*10-Frst", nEstDays,"Days=",round(dfcFrst,1), "d  Deaths*10-Frst",nEstDays,"Days=",round(dfdFrst,1), "d\n",
                  "Confirmed*10-Prev", nEstDays,"Days=",round(dfcPrev,1), "d  Deaths*10-Prev",nEstDays,"Days=",round(dfdPrev,1), "d\n", 
                  "Confirmed*10-Last", nRegDays,"Days=",round(dfcLast,1), "d  Deaths*10-Last",nRegDays,"Days=",round(dfdLast,1), "d")) +
      ggtitle(paste0("Region=",paste(Regions,collapse="-"), " Date=",max(dfg$Stamp,na.rm=TRUE), 
                     "  Population=",round(Population/1e6,1), "Mio Confirmed=",round(max(dfc$Confirmed,na.rm=TRUE)/Population*1e6),"ppm",
                     "  Confirmed=",max(dfc$Confirmed,na.rm=TRUE),"  Recovered=",max(dfc$Recovered,na.rm=TRUE), " Deaths=",max(dfc$Deaths,na.rm=TRUE),
                     "  minTimeToDeath=",round(MTD,1), "d"))
    print(gg)
    
    ggsave(filename=paste0(baseDir,"/thumbs/covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"),
           width=200, height=150, units="mm", scale=1.4 ,dpi=100)
    ggsave(filename=paste0(baseDir,"/plots/covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"),
           width=350, height=200, units="mm", scale=1 ,dpi=150)
  }
  
  return(0)
}



# -------------------------------------------------------------------------------------------------------------
covCounty <- function(Date="2020-03-27", dataDir="./data") {
# -------------------------------------------------------------------------------------------------------------  
  #csvFileStates <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/covid-county/DE_states_202003261407.csv"
  #states <- read.csv(csvFileStates, stringsAsFactors=FALSE)
  #str(states)
  #colnames(states)

  setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
  
  sourceLinks <- c("https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html",
                   "https://info.gesundheitsministerium.at",
                   "https://www.sozialministerium.at/Informationen-zum-Coronavirus/Dashboard/Zahlen-zur-Hospitalisierung")
  
  
  # create empty dataframe along samples in BOX at https://ibm.ent.box.com/folder/108160857311
  cc <- data.frame(country=character(),
                   state=character(),
                   county=character(),
                   cases=integer(),
                   deaths=integer(),
                   recovered=integer(),
                   tested=integer(),
                   tests=integer(),
                   public_tests=integer(),
                   private_tests=integer(),
                   hospitalized=integer(),
                   presumed_cases=integer(),
                   monitoring=integer(),
                   Attribution=character(),
                   AttributionURL=character(),
                   publication_date=numeric(),
                   scrape_date=numeric(),
                   source_links=character(),stringsAsFactors=FALSE)
  
  # read data for Confirmed
  csvFile <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.csv"
  da <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    dplyr::filter(date(Stamp)==as.POSIXct("2020-03-27"))
  
  BL <- data.frame(ID=colnames(da[3:12]),
                   Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                   NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                   NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                   Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                   stringsAsFactors=FALSE)
  
  
  # read data for Hospitalized
  csvFile <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.hospital.csv"
  dh <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    dplyr::filter(date(Stamp)==as.POSIXct("2020-03-27"))
  
  
  # Merge these data and select last record of each Status per day
  df <- rbind(da,dh) %>% 
    dplyr::mutate(Date=date(Stamp)) %>%
    dplyr::arrange(Stamp) %>%
    dplyr::group_by(Date,Status) %>%
    dplyr::filter(row_number()==n()) %>%
    dplyr::filter(!is.na(AT)) %>%
    dplyr::ungroup()
  
  # Spread Status and mutate to Box format
  ds <- df %>% 
    dplyr::select(Date,Status,AT) %>%
    tidyr::spread(key=Status, val=AT) %>%
    dplyr::mutate(country="Austria",county="ALL",state="ALL",publication_date=as.numeric(as.POSIXct(Date)),scrape_date=as.numeric(as.POSIXct(Date))) %>%
    dplyr::rename(cases=Confirmed, deaths=Deaths,tested=Tested, hospitalized=Hospitalisierung) %>%
    dplyr::select(-Intensivstation, -Date) %>%
    dplyr::mutate(source_links=paste(sourceLinks[1:2],collapse=","))
  
  # append to box format
  atc <- dplyr::bind_rows(cc, ds)
  write.csv(atc, file=paste0(dataDir,"/AT_country_",format(as.POSIXct(Date),"%Y%m%d"),"160000",".csv"), quote=FALSE, row.names=FALSE)
  
  BL <- data.frame(ID=colnames(df)[3:12],
                   Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                   NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                   NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                   Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                   stringsAsFactors=FALSE)
  
  colnames(df) <- c(colnames(df)[1:3],BL$Name[2:10],"Date")
  
  dg <- df %>%
    dplyr::filter_all(all_vars(!is.na(.))) %>%
    dplyr::select(-Stamp,-AT) %>%
    dplyr::filter(Status!="Intensivstation") %>%
    tidyr::gather(key=state,val=Count,2:10) %>%
    tidyr::spread(key=Status, val=Count) %>%
    dplyr::mutate(country="Austria",county="ALL",publication_date=as.numeric(as.POSIXct(Date)),scrape_date=as.numeric(as.POSIXct(Date))) %>%
    dplyr::rename(cases=Confirmed, deaths=Deaths, hospitalized=Hospitalisierung) %>%
    dplyr::select(-Date) %>%
    dplyr::mutate(source_links=paste(sourceLinks[1:2],collapse=","))

  # append to box format
  ato <- dplyr::bind_rows(cc, dg)
  write.csv(ato, file=paste0(dataDir,"/AT_states_",format(as.POSIXct(Date),"%Y%m%d"),"160000",".csv"), quote=FALSE, row.names=FALSE)
  
  
  # read data for Bezirke
  csvFile <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.regions.csv"
  dr <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    dplyr::filter(date(Stamp)==as.POSIXct("2020-03-27"))
  
  dt <- dr %>% 
    dplyr::mutate(Date=date(Stamp)) %>%
    dplyr::arrange(Stamp) %>%
    dplyr::group_by(Date,Status,Region) %>%
    dplyr::filter(row_number()==n()) %>%
    dplyr::ungroup()
  
  # Spread Status and mutate to Box format
  du <- dt %>% 
    dplyr::mutate(country="Austria",state="ALL",county=Region,
                  publication_date=as.numeric(Stamp), scrape_date=as.numeric(Stamp)) %>%
    dplyr::rename(cases=Count) %>%
    dplyr::select(-Date,-Status,-Stamp,-Region) %>%
    dplyr::mutate(source_links=sourceLinks[3])
  
  # append to box format
  atu <- dplyr::bind_rows(cc, du)
  write.csv(atu, file=paste0(dataDir,"/AT_counties_",format(as.POSIXct(Date),"%Y%m%d"),"160000",".csv"), quote=FALSE, row.names=FALSE)
  

}


