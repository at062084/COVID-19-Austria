library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(tibbletime)
library(scales)
library(ggplot2)
library(forcats)
library(zoo)
options(error = function() traceback(2))

wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk"
setwd(wd)
# source("../COVID-19-common.R")  


# do some logging
logFile <- "./COVID-19-AGES.log"
logMsg <- function(msg) {
  #cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}

# COVID-19 Ages files
#cfGKZ <- "CovidFaelle_GKZ.csv"
cfGKZtl <- "CovidFaelle_Timeline_GKZ.csv"
cftl <- "CovidFaelle_Timeline.csv"
cfz <- "CovidFallzahlen.csv"
# epi <- "Epikurve.csv"
gtl <- "GenesenTimeline.csv"
tftl <- "TodesfaelleTimeline.csv"
#cfag <- "CovidFaelle_Altersgruppe.csv"



# -------------------------------------------------------------------------------------------
# CFR: GenesenTimeline.csv + TodesfaelleTimeline.csv: Case Fatality Rate
# -------------------------------------------------------------------------------------------
csvFile <- paste0("./data/ages/",tftl)
dd <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>%
  dplyr::mutate(time=as.Date(as.POSIXct(time, format="%d.%m.%Y"))) %>%
  dplyr::rename(Deaths=Todesfälle)
csvFile <- paste0("./data/ages/",gtl)
dr <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>%
  dplyr::mutate(time=as.Date(as.POSIXct(time, format="%d.%m.%Y"))) %>%
  dplyr::rename(Recovered=Genesen)
df <- dd %>% left_join(dr, by="time")
str(df)

ggplot(data=df, aes(x=time, y=Deaths/(Recovered+Deaths))) + geom_line() + geom_point() +
  scale_x_date(limits=c(as.Date(strptime("2020-04-13",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") + 
  scale_y_continuous(limits=c(0,.05), breaks=seq(0,0.1, by=0.01))
  


# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline_GKZ.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
csvFile <- paste0("./data/ages/",cfGKZtl)
df <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
  dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
  dplyr::rename(RegionID=GKZ, Region=Bezirk, Population=AnzEinwohner) %>%
  dplyr::rename(newConfirmed=AnzahlFaelle, sumConfirmed=AnzahlFaelleSum, rm7Confirmed=AnzahlFaelle7Tage) %>%
  dplyr::rename(newDeaths=AnzahlTotTaeglich, sumDeaths=AnzahlTotSum) %>%
  dplyr::rename(newRecovered=AnzahlGeheiltTaeglich, sumRecovered=AnzahlGeheiltSum) %>%
  dplyr::select(-SiebenTageInzidenzFaelle, -Time) %>% dplyr::select(11,12,1:10)
  #dplyr::mutate(SiebenTageInzidenzFaelle=as.integer(SiebenTageInzidenzFaelle))
str(df)
summary(df)
unique(df$Region)

# apply rolling mean to 'new*' cols
dfrm <- df %>%
  dplyr::arrange(Date, Region) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, fill=NA, align="right") %>%
  dplyr::ungroup()
str(dfrm)

ggplot(data=dfrm, aes(x=Date, y=newConfirmed/Population*1000000, color=Region)) + 
  geom_line() + 
  scale_x_date(limits=c(as.Date(strptime("2020-08-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(0,500)) + 
  ggtitle("AGES Bezirke Timeline: Wien")

# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline.csv: TimeLine BundesLänder (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cftl <- function() {
  csvFile <- paste0("./data/ages/",cftl)
  dc <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";")%>% 
    dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, sumConfirmed=AnzahlFaelleSum, rm7Confirmed=AnzahlFaelle7Tage) %>%
    dplyr::rename(newDeaths=AnzahlTotTaeglich, sumDeaths=AnzahlTotSum) %>%
    dplyr::rename(newRecovered=AnzahlGeheiltTaeglich, sumRecovered=AnzahlGeheiltSum) %>%
    dplyr::select(-SiebenTageInzidenzFaelle, -Time) %>% dplyr::select(11,12,1:10)
  #str(dc)
  #summary(dc)
  return(dc)
}

# -------------------------------------------------------------------------------------------
# CovidFallzahlen.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cfz <- function() {
  csvFile <- paste0("./data/ages/",cfz)
  dt <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(MeldeDatum, format="%d.%m.%Y %H:%M:%S"), Date=date(as.POSIXct(Meldedat, format="%d.%m.%Y"))) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, sumTested=TestGesamt) %>%
    dplyr::rename(curHospital=FZHosp, freeHospital=FZHospFree, curICU=FZICU, freeICU=FZICUFree) %>%
    dplyr::arrange(Date) %>% group_by(Region) %>% 
    dplyr::mutate(newTested=sumTested-lag(sumTested))  %>% 
    dplyr::ungroup() %>%
    dplyr::select(-Meldedat, -MeldeDatum) %>% dplyr::select(8,9,6,7,10,1,2,4,3,5)
  #str(dt)
  #summary(dt)
  dt$Region[dt$Region=="Alle"] <- "Österreich"
  return(dt)
}



# --------------------------------------------------------------------------------------------------------
# AGES Bundesländer: TimeLine Bundesländer (Tested, Confirmed, Recovered, Deaths, Hosptital, ICU)
# --------------------------------------------------------------------------------------------------------
caAgesRead_tlrm <- function(bRolling=TRUE, bPlot=FALSE) {

  # Read timeline of confirmed, hospitalized, deaths
  dc <- caAgesRead_cftl()
  if (bPlot) {
    ggplot(data=dc %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newConfirmed)) + 
      geom_line() + 
      scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
                   date_breaks="1 weeks", date_labels="%a.%d.%m") +
      ggtitle("AGES BundesLänder Timeline Confirmed: Wien")
  }
  
  # Read timeline of tested
  dt <- caAgesRead_cfz()
  if(bPlot) {
    ggplot(data=dt %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newTested)) + 
      geom_line() + 
      scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
                   date_breaks="1 weeks", date_labels="%a.%d.%m") +
      ggtitle("AGES BundesLänder Timeline Tested: Wien")
  }
  
  df <- dc %>% left_join(dt, by=c("Date","RegionID", "Stamp","Region")) %>%
    dplyr::mutate(newConfPop=newConfirmed/Population*100000, newConfTest=newConfirmed/newTested)
    
  
  # remove today's data as semantics undefined
  # df <- df %>% dplyr::filter(Date != max(Date))
  # str(df)
  
  if (!bRolling)
    return(df)
  
  # apply rolling mean to 'new*' cols
  dfrm <- df %>%
    dplyr::arrange(Date, Region) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, fill=NA, align="right") %>%
    dplyr::ungroup()
  # str(dfrm)
  
  return(dfrm)
}

dfrm <- caAgesRead_tlrm(bRolling=TRUE)
df <- caAgesRead_tlrm(bRolling=FALSE)

# Vienna plot
scaled=1000
dfrmw <- dfrm %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/10000, newConfirmed_x10=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed_x10) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed_x10)
dfw <- df %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/10000, newConfirmed_x10=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed_x10) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed_x10)

ggplot(data=dfrmw, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.2,20), breaks=seq(1,20,by=1), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=seq(0,20000,by=1000))) +
  geom_point(size=3) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  geom_point(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), size=1.5) + 
  geom_line(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), linetype=3, size=.75) + 
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Wien")

ggplot(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.1,20), breaks=seq(1,20,by=1), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=seq(1,20000,by=1000))) +
  geom_point(size=2) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested Daily: Wien")



dfrmat <- dfrm %>% dplyr::filter(Region=="Österreich") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/1000, newConfirmed_x10=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed_x10) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed_x10)
dfat <- df %>% dplyr::filter(Region=="Österreich") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/1000, newConfirmed_x10=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed_x10) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed_x10)

ggplot(data=dfrmat, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.5,50),  breaks=c(1,2,5,7,10,15,20,30,40,50), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(1000,2000,5000,10000,20000,30000,40000,50000))) +
  geom_point(size=2) + 
  geom_line() +
  geom_point(data=dfat, aes(x=Date, y=Count, color=Status, shape=Status), size=1) + 
  geom_line(data=dfat, aes(x=Date, y=Count, color=Status, shape=Status), linetype=3, size=.5) + 
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Österreich")
#breaks=seq(.5,50,by=1),
# , breaks=seq(.5,50000,by=1000)

ggplot(data=dfrm, 
       aes(x=Date, y=newConfPop, color=Region, shape=Region)) +
  scale_shape_manual(values=c(1:10)) +
  scale_x_date(limits=c(as.Date(strptime("2020-10-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(.1,100), breaks=c(seq(.1,1,by=.1),1:10,seq(10,100,by=10)), position="right") + 
  geom_point(size=3) + 
  geom_line() +
  geom_line(data=dfrm %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=newConfPop), color="darkgreen", size=1) +
  geom_line(data=dfrm %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newConfPop), color="red", size=1) +
  ggtitle("AGES BundesLänder Timeline newConfirmed/per100.000 WeekMeans")

ggplot(data=df, aes(x=Date, y=newConfPop, color=Region, shape=Region)) +
  scale_shape_manual(values=c(1:10)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") +
  geom_point(size=3) + 
  geom_line() +
  ggtitle("AGES BundesLänder Timeline newConfirmed/per100.000 perDay")


ggplot(data=dfrm, aes(x=Date, y=newConfTest*100, color=Region, shape=Region)) +
  scale_shape_manual(values=c(1:10)) +
  scale_x_date(limits=c(as.Date(strptime("2020-08-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(0,25), breaks=1:25) + 
  geom_point(size=3) + 
  geom_line() +
  ggtitle("AGES BundesLänder Timeline newConfirmed/newTested [%] WeekMeans")



# Plot spread rate vs current newConfirmed per 100.000
dfrm7 <- dfrm %>% dplyr::filter(Date >= max(Date)-days(7))
dflm7 <- dfrm7 %>% dplyr::arrange(Region,Date) %>% 
  dplyr::group_by(Region) %>% summarize(lm7 = exp(coef(lm(log(newConfPop)~Date))[2]))
dfrm1 <- dfrm %>% dplyr::filter(Date == max(Date))
dflmrm <- dfrm1 %>% dplyr::left_join(as.data.frame(dflm7), by="Region")
ggplot(data=dflmrm, aes(x=newConfPop, y=lm7, color=Region, shape=Region)) + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  scale_x_continuous(limits=c(0,65), breaks=seq(0,100,by=10)) + 
  scale_y_continuous(limits=c(.95,1.2), breaks=seq(0.5,1.5,by=.1)) + 
  geom_point(aes(size=newConfTest*100), stroke=1.5)



lastWeeks=2
nRegDays=7
dblDays=c(1:7,10,14,21,28,35,50,100,Inf,-100,-50,-28,-21,-14,-10,-7)
dfrm7 <- dfrm %>% dplyr::filter(Date >= max(Date)-(lastWeeks+1)*days(7))
rolm <- rollify(.f=function(Date,vals) {exp(coef(lm(log(vals)~Date))[2])}, window=nRegDays)

dflm7 <- dfrm7 %>% 
  dplyr::arrange(Region,Date) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::mutate(Spread=rolm(Date,newConfirmed)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(newTested>1)

str(dflm7)
#%>% dplyr::filter(Region %in% c("Wien","Österreich","Kärnten","Vorarlberg")), trans="log10",
#sec.axis = sec_axis(~ . *1, breaks=seq(.95,2,by=0.05),name=paste0("R0"))
ggplot(data=dflm7 %>% dplyr::arrange(Region,Date), 
       aes(x=newConfPop, y=Spread, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,by=10)) + 
  scale_y_continuous(limits=c(.95,1.2), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=as.character(round(exp(log(2)/dblDays),2)), name="Tägliche Steigerungsrate")) +
  geom_path() + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Region, size=as.numeric(Date), stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  geom_path(data=dflm7 %>% dplyr::filter(Region=="Österreich"), aes(x=newConfPop, y=Spread), color="darkgreen", size=1) +
  geom_path(data=dflm7 %>% dplyr::filter(Region=="Wien"), aes(x=newConfPop, y=Spread), color="red", size=1) +
  geom_point(data=dflm7 %>% dplyr::filter(Date==max(Date)), 
             aes(x=newConfPop, y=Spread, color=Region, shape=Region), size=5, stroke=1.5) +
  geom_text(data=dflm7 %>% dplyr::filter(Date==max(Date)), 
            aes(x=newConfPop, y=Spread, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30") +
  geom_point(data=dflm7 %>% dplyr::filter(Date==min(Date)+days(6)), 
             aes(x=newConfPop, y=Spread, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dflm7 %>% dplyr::filter(Date==min(Date)+days(6)), 
            aes(x=newConfPop, y=Spread, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  ggtitle(paste("COVID-19 Österreich: Entwicklung der Verbreitungsrate und Neuinfektionen in den letzen zwei Wochen.  Zeitraum:", min(dflm7$Date)+days(6),"-",max(dflm7$Date))) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")






