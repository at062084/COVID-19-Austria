library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(tibbletime)
library(scales)
options(error = function() traceback(2))

setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")


# do some logging
logFile <- "./COVID-19-covid-county-extract.log"
logMsg <- function(msg) {
  #cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}



# -------------------------------------------------------------------------------------------------------------
covCounty <- function(Date="2020-03-27", scrapeStamp=now(), dataDir="./data") {
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
  
  publishStamp <- max(df$Stamp)
  
  # Spread Status and mutate to Box format
  ds <- df %>% 
    dplyr::select(Date,Status,AT) %>%
    tidyr::spread(key=Status, val=AT) %>%
    dplyr::mutate(country="Austria",county="ALL",state="ALL",publication_date=as.numeric(publishStamp),scrape_date=as.numeric(scrapeStamp)) %>%
    dplyr::rename(cases=Confirmed, deaths=Deaths,tested=Tested, hospitalized=Hospitalisierung) %>%
    dplyr::select(-Intensivstation, -Date) %>%
    dplyr::mutate(source_links=paste(sourceLinks[1:2],collapse=","))
  
  # append to box format
  atc <- dplyr::bind_rows(cc, ds)
  fileName <- paste0(dataDir,"/AT_country_",format(scrapeStamp,"%Y%m%d%H%M%S"),".csv")
  logMsg(paste("Writing", fileName))
  write.csv(atc, file=fileName, quote=FALSE, row.names=FALSE)
  
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
    dplyr::mutate(country="Austria",county="ALL",publication_date=as.numeric(publishStamp),scrape_date=as.numeric(scrapeStamp)) %>%
    dplyr::rename(cases=Confirmed, deaths=Deaths, hospitalized=Hospitalisierung) %>%
    dplyr::select(-Date) %>%
    dplyr::mutate(source_links=paste(sourceLinks[1:2],collapse=","))
  
  # append to box format
  ato <- dplyr::bind_rows(cc, dg)
  fileName <- paste0(dataDir,"/AT_states_",format(scrapeStamp,"%Y%m%d%H%M%S"),".csv")
  logMsg(paste("Writing", fileName))
  write.csv(ato, file=fileName, quote=FALSE, row.names=FALSE)
  
  
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
  
  publishStamp <- max(dt$Stamp)
  
  # Spread Status and mutate to Box format
  du <- dt %>% 
    dplyr::mutate(country="Austria",state="ALL",county=Region,
                  publication_date=as.numeric(publishStamp), scrape_date=as.numeric(scrapeStamp)) %>%
    dplyr::rename(cases=Count) %>%
    dplyr::select(-Date,-Status,-Stamp,-Region) %>%
    dplyr::mutate(source_links=sourceLinks[3])
  
  # append to box format
  atu <- dplyr::bind_rows(cc, du)
  fileName <- paste0(dataDir,"/AT_counties_",format(scrapeStamp,"%Y%m%d%H%M%S"),".csv")
  logMsg(paste("Writing", fileName))
  write.csv(atu, file=fileName, quote=FALSE, row.names=FALSE)
}



# --------------------------------------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------------------------------------

# Convert data to covid-county format and write into three csv files
logMsg(paste("Running COVID-19-covid-county-extract.R"))

ts <- now()
logMsg(paste("Executing covCounty with", ts))
covCounty(Date=format(ts,format="%Y-%m-%d"), scrapeStamp=ts, dataDir="./data")
logMsg("Done executing covCounty")
logMsg("Done running COVID-19-covid-county-extract.R")



