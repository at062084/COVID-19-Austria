# COVID-19 data provided by Austrian authoritiese
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(imputeTS)
library(tibbletime)
library(MASS)
library(robustbase)
library(scales)
options(error = function() traceback(2))
source("../COVID-19-common.R")

# do some logging
logFile <- "./COVID-19.austria.log"
logMsg <- function(msg) {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}

setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
csvFile <- "./data/COVID-19-austria.csv"

#----------------------------------------------------------------------------------------------------
# Create Standard plot for each Bundesland
#----------------------------------------------------------------------------------------------------
df <- read.csv(csvFile, stringsAsFactors=FALSE) %>% 
  dplyr::mutate(Stamp=as.POSIXct(Stamp))

# Bundesländer in Österreich
db <- data.frame(ID=colnames(df[3:12]),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)

# Iterate AT and Bundesländer
for (bl in 1:nrow(db)) {
  da <- df %>% 
    # use unquoted variant of dply methods so a string variable can be used for Bundesland
    dplyr::select_("Stamp","Status",db$ID[bl]) %>%
    dplyr::filter(Status!="Tested") %>%
    dplyr::mutate(Date=date(Stamp)) %>%
    dplyr::arrange(Stamp) %>%
    dplyr::group_by(Date,Status) %>%
    dplyr::filter(row_number()==n()) %>%   # select latest entry for each status in current day
    dplyr::ungroup() %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(Stamp=max(Stamp)) %>%    # set all Stamp to latest point in time for current day
    dplyr::ungroup() %>%
    dplyr::select(-Date) %>%   
    tidyr::spread_(key="Status", val=db$ID[bl]) %>%
    imputeTS::na_interpolation(method="linear") %>%
    dplyr::mutate(Confirmed=round(Confirmed), Deaths=round(Deaths), Recovered=round(Recovered))

  covRegionPlot(da, Regions=db$Name[bl], Population=db$Population[bl]*1e3, filePrefix="bmsgpk.stand",
                nRegDays=4, nCutOff=1, nEstDays=7, ggMinDate=as.POSIXct("2020-02-22"))
}



