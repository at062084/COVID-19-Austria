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
source("../COVID-19-common.R")


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
# gtl <- "GenesenTimeline.csv"
# tftl <- "TodesfaelleTimeline.csv"
# cfag <- "CovidFaelle_Altersgruppe.csv"

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

ggplot(data=df %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newConfirmed)) + 
  geom_line() + 
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  ggtitle("AGES Bezirke Timeline: Wien")

# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline.csv: TimeLine BundesLänder (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
csvFile <- paste0("./data/ages/",cftl)
dc <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";")%>% 
  dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
  dplyr::rename(RegionID=BundeslandID, Region=Bundesland, Population=AnzEinwohner) %>%
  dplyr::rename(newConfirmed=AnzahlFaelle, sumConfirmed=AnzahlFaelleSum, rm7Confirmed=AnzahlFaelle7Tage) %>%
  dplyr::rename(newDeaths=AnzahlTotTaeglich, sumDeaths=AnzahlTotSum) %>%
  dplyr::rename(newRecovered=AnzahlGeheiltTaeglich, sumRecovered=AnzahlGeheiltSum) %>%
  dplyr::select(-SiebenTageInzidenzFaelle, -Time) %>% dplyr::select(11,12,1:10)
str(dc)
summary(dc)

ggplot(data=dc %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newConfirmed)) + 
  geom_line() + 
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  ggtitle("AGES BundesLänder Timeline Confirmed: Wien")


# -------------------------------------------------------------------------------------------
# CovidFallzahlen.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
csvFile <- paste0("./data/ages/",cfz)
dt <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
  dplyr::mutate(Stamp=as.POSIXct(MeldeDatum, format="%d.%m.%Y %H:%M:%S"), Date=date(as.POSIXct(Meldedat, format="%d.%m.%Y"))) %>%
  dplyr::rename(RegionID=BundeslandID, Region=Bundesland, sumTested=TestGesamt) %>%
  dplyr::rename(curHospital=FZHosp, freeHospital=FZHospFree, curICU=FZICU, freeICU=FZICUFree) %>%
  dplyr::arrange(Date) %>% group_by(Region) %>% 
  dplyr::mutate(newTested=sumTested-lag(sumTested))  %>% 
  dplyr::ungroup() %>%
  dplyr::select(-Meldedat, -MeldeDatum) %>% dplyr::select(8,9,6,7,10,1,2,4,3,5)
str(dt)
summary(dt)

ggplot(data=dt %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newTested)) + 
  geom_line() + 
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  ggtitle("AGES BundesLänder Timeline Tested: Wien")



# --------------------------------------------------------------------------------------------------------
# AGES Bundesländer: TimeLine Bundesländer (Tested, Confirmed, Recovered, Deaths, Hosptital, ICU)
# --------------------------------------------------------------------------------------------------------
df <- dc %>% left_join(dt, by=c("Date","RegionID", "Stamp","Region"))
str(df)


# apply rolling mean to 'new*' cols
dfrm <- df %>%
  dplyr::arrange(Date, Region) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, fill=NA, align="right") %>%
  dplyr::ungroup()
str(dfrm)

# Vienna plot
scaled=1000
dfw <- dfrm %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfirmedTested=newConfirmed/newTested*100, newTested=newTested/1000, newConfirmed_x10=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed_x10) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed_x10)

ggplot(data=dfw, aes(x=Date, y=Count, color=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(0,10), breaks=seq(0,18,by=1), 
                     sec.axis = sec_axis(~ . *scaled, breaks=seq(0,18000,by=1000))) +
  geom_point(size=2) + 
  geom_line() +
  ggtitle("AGES BundesLänder Timeline Confirmed/Tested WeekMeans: Wien")

