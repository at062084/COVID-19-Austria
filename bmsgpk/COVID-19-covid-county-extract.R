library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(tibbletime)
library(scales)
options(error = function() traceback(2))

setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
source("../COVID-19-common.R")


# do some logging
logFile <- "./COVID-19-covid-county-extract.log"
logMsg <- function(msg) {
  #cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}



# -------------------------------------------------------------------------------------------------------------
covCounty <- function(scrapeStamp=now(), dataDate=format(scrapeStamp,"%Y-%m-%d"), dataDir="./data") {
# -------------------------------------------------------------------------------------------------------------  
  #csvFileStates <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/covid-county/DE_states_202003261407.csv"
  #states <- read.csv(csvFileStates, stringsAsFactors=FALSE)
  #str(states)
  #colnames(states)
  
  #setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
  
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

  
  # -------------------------------------------------------------------------------------------------------------
  # Data files for country and state
  # -------------------------------------------------------------------------------------------------------------
  
  # read data for Confirmed
  csvFile <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.csv"
  da <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    dplyr::filter(date(Stamp)==as.POSIXct(dataDate))
  
  
  # read data for Hospitalized
  csvFile <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.hospital.csv"
  dh <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    dplyr::filter(date(Stamp)==as.POSIXct(dataDate))
  
  
  # Merge these data and select last record of each Status per day
  df <- rbind(da,dh) %>% 
    dplyr::mutate(Date=date(Stamp)) %>%
    dplyr::arrange(Stamp) %>%
    dplyr::group_by(Date,Status) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::filter(!is.na(AT)) %>%
    dplyr::ungroup()
  
  publishStamp <- max(df$Stamp)

  
  # -------------------------------------------------------------------------------------------------------------
  # Write Data file for country 
  # -------------------------------------------------------------------------------------------------------------
  
  # Spread Status and mutate to Box format
  #ds <- df %>% 
  #  dplyr::select(Date,Status,AT) %>%
  #  tidyr::spread(key=Status, val=AT) %>%
  #  dplyr::mutate(country="Austria",county="ALL",state="ALL",publication_date=as.numeric(publishStamp),scrape_date=as.numeric(scrapeStamp)) %>%
  #  dplyr::rename(cases=Confirmed, deaths=Deaths,tested=Tested, hospitalized=Hospitalisierung) %>%
  #  dplyr::select(-Intensivstation, -Date) %>%
  #  dplyr::mutate(source_links=paste(sourceLinks[1:2],collapse=","))

  # simplified format for PR
  ds <- df %>% 
    dplyr::select(Date,Status,AT) %>%
    tidyr::spread(key=Status, val=AT) %>%
    dplyr::mutate(country="Austria",county="ALL",state="ALL") %>%
    dplyr::rename(cases=Confirmed, deaths=Deaths,tested=Tested, recovered=Recovered, hospitalized=Hospitalisierung) %>%
    dplyr::select(-Intensivstation, -Date) %>% 
    dplyr::select(country,county,state,cases,deaths,recovered,tested,hospitalized)
  
  # Write country file for covid-county
  #atc <- dplyr::bind_rows(cc, ds)
  fileName <- paste0(dataDir,"/AT_country.bmsgpk.csv")
  logMsg(paste("Writing", fileName))
  write.csv(ds, file=fileName, quote=FALSE, row.names=FALSE)

  
  
  # -------------------------------------------------------------------------------------------------------------
  # Write Data file for states
  # -------------------------------------------------------------------------------------------------------------
  colnames(df) <- c(colnames(df)[1:2],"ALL",BL$Name[2:10],"Date")
  
  #dg <- df %>%
  #  dplyr::filter_all(all_vars(!is.na(.))) %>%
  #  dplyr::select(-Stamp) %>%
  #  dplyr::filter(Status!="Intensivstation") %>%
  #  tidyr::gather(key=state,val=Count,2:11) %>%
  #  tidyr::spread(key=Status, val=Count) %>%
  #  dplyr::mutate(country="Austria",county="ALL",publication_date=as.numeric(publishStamp),scrape_date=as.numeric(scrapeStamp)) %>%
  #  dplyr::rename(cases=Confirmed, deaths=Deaths, hospitalized=Hospitalisierung) %>%
  #  dplyr::select(-Date) %>%
  #  dplyr::mutate(source_links=paste(sourceLinks[1:2],collapse=","))
  
  # simplified format
  dg <- df %>%
    dplyr::select(-Stamp) %>%
    dplyr::filter(Status!="Intensivstation") %>%
    tidyr::gather(key=state,val=Count,2:11) %>%
    tidyr::spread(key=Status, val=Count) %>%
    dplyr::mutate(country="Austria", county="ALL") %>%
    dplyr::rename(cases=Confirmed, deaths=Deaths, hospitalized=Hospitalisierung, recovered=Recovered, tested=Tested) %>%
    dplyr::select(-Date) %>%
    dplyr::select(country,state,county,cases,deaths,recovered,tested,hospitalized)
  
  # Write state file for covid-county
  # ato <- dplyr::bind_rows(cc, dg)
  fileName <- paste0(dataDir,"/AT_states.bmsgpk.csv")
  logMsg(paste("Writing", fileName))
  write.csv(dg, file=fileName, quote=FALSE, row.names=FALSE)
  
  
  
  # -------------------------------------------------------------------------------------------------------------
  # Write Data file for counties (regions, Bezirke)
  # -------------------------------------------------------------------------------------------------------------
  # read data for Bezirke
  csvFile <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.regions.csv"
  dr <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
    dplyr::mutate(Stamp=as.POSIXct(Stamp)) %>%
    dplyr::filter(date(Stamp)==as.POSIXct("2020-03-27"))
  
  dt <- dr %>% 
    dplyr::mutate(Date=date(Stamp)) %>%
    dplyr::arrange(Stamp) %>%
    dplyr::group_by(Date,Status,Region) %>%
    dplyr::filter(row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(OBR, by=c("Region"="county")) %>%
    dplyr::select(country,state,county=Region,cases=Count)
  
  
  # Spread Status and mutate to Box format
  #du <- dt %>% 
  #  dplyr::mutate(country="Austria",state="ALL",county=Region,
  #                publication_date=as.numeric(publishStamp), scrape_date=as.numeric(scrapeStamp)) %>%
  #  dplyr::rename(cases=Count) %>%
  #  dplyr::select(-Date,-Status,-Stamp,-Region) %>%
  #  dplyr::mutate(source_links=sourceLinks[3])
  
  # append to box format
  # atu <- dplyr::bind_rows(cc, du)
  fileName <- paste0(dataDir,"/AT_counties.bmsgpk.csv")
  logMsg(paste("Writing", fileName))
  write.csv(dt, file=fileName, quote=FALSE, row.names=FALSE)
}



# --------------------------------------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------------------------------------

# Convert data to covid-county format and write into three csv files
logMsg(paste("Running COVID-19-covid-county-extract.R"))

ts <- now()-hours(2)
logMsg(paste("Executing covCounty with", ts))
covCounty(scrapeStamp=ts, dataDate=format(ts,"%Y-%m-%d"), dataDir="./data")
logMsg("Done executing covCounty")
logMsg("Done running COVID-19-covid-county-extract.R")



