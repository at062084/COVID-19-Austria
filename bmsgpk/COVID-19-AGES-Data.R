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


# Several reported 'Tested' records seem to be wrong. 
# Here obvious errors are promoted to imputation
err <- data.frame(rbind(
  c("B","2020-05-16","Tested",NA),
  c("B","2020-05-31","Tested",NA),
  c("B","2020-06-01","Tested",NA),
  c("B","2020-06-21","Tested",NA),
  c("B","2020-06-26","Tested",NA),
  c("K","2020-06-21","Tested",NA),
  c("Szb","2020-05-06","Tested",NA),
  c("Szb","2020-06-09","Tested",NA),
  c("Szb","2020-06-10","Tested",NA),
  c("W","2020-04-15","Tested",32660),
  c("W","2020-04-22","Tested",NA),
  c("W","2020-04-23","Tested",NA),
  c("W","2020-08-14","Tested",NA),
  c("W","2020-08-15","Tested",NA),
  c("W","2020-07-13","Tested",NA)
), stringsAsFactors=FALSE)
colnames(err)<-c("County","Date","Status","Value")
err$Value <- as.integer(err$Value)

caAgesDataImpute <- function(df, err) {

  # Correct 'Tested' data along err
  for(k in 1:dim(err)[1]) {
    df[df$Date==strptime(err[k,"Date"],format="%Y-%m-%d") & d$Status==err[k,"Status"] ,err[k,"County"]] = err[k,"Value"] 
  }
  
  # All Counties
  dg <- df %>% 
    dplyr::arrange(Stamp) %>%
    dplyr::group_by(Date, Region, RegionID) %>%
    # impute missing values. Was difficult to get the syntax working ! Dont mess up
    dplyr::mutate_at(vars(-group_cols()), imputeTS::na_interpolation, option="linear", maxgap=3) %>% 
    dplyr::ungroup()

    return(dg)    
}


  





# -------------------------------------------------------------------------------------------
# CFR: GenesenTimeline.csv + TodesfaelleTimeline.csv: Case Fatality Rate
# -------------------------------------------------------------------------------------------
caAgesRead_tftl <- function() {
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
}


# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline_GKZ.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cfGKZtl <- function() {
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
}

# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline.csv: TimeLine BundesLänder (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cftl <- function(csvFile=paste0("./data/ages/",cftl)) {
  #csvFile <- paste0("./data/ages/",cftl)
  dc <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, newRecovered=AnzahlGeheiltTaeglich, newDeaths=AnzahlTotTaeglich) %>%
    dplyr::rename(sumConfirmed=AnzahlFaelleSum, sumRecovered=AnzahlGeheiltSum, sumDeaths=AnzahlTotSum) %>%
    dplyr::select(-SiebenTageInzidenzFaelle, -Time, -AnzahlFaelle7Tage) %>% 
    dplyr::select(11,10,2,1,3,4,8,6,5,9,7)
  #str(dc)
  #summary(dc)
  return(dc)
}

# -------------------------------------------------------------------------------------------
# CovidFallzahlen.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cfz <- function(csvFile=paste0("./data/ages/",cfz)) {
  #csvFile <- paste0("./data/ages/",cfz)
  dt <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(MeldeDatum, format="%d.%m.%Y %H:%M:%S"), Date=date(as.POSIXct(Meldedat, format="%d.%m.%Y"))) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, sumTested=TestGesamt) %>%
    dplyr::rename(curHospital=FZHosp, freeHospital=FZHospFree, curICU=FZICU, freeICU=FZICUFree) %>%
    dplyr::arrange(Date) %>% 
    dplyr::group_by(Region) %>% 
    dplyr::mutate(newTested=sumTested-lag(sumTested)) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-Meldedat, -MeldeDatum) %>% 
    dplyr::select(8,9,6,7,10,1,2,3,4,5)
  #str(dt)
  #summary(dt)
  dt$Region[dt$Region=="Alle"] <- "Österreich"
  return(dt)
}


# --------------------------------------------------------------------------------------------------------
# AGES Bundesländer: TimeLine Bundesländer (Tested, Confirmed, Recovered, Deaths, Hosptital, ICU)
# --------------------------------------------------------------------------------------------------------
caAgesRead_tlrm <- function(fileDate_cftl=NULL, fileDate_cfz=NULL, bPlot=FALSE, bEstimate=TRUE, bCompleteCases=FALSE) {
  
  if(is.null(fileDate_cftl)) dc<-caAgesRead_cftl() else dc<-caAgesRead_cftl(csvFile=fileDate_cftl)
  # Read timeline of confirmed, hospitalized, deaths
  if (bPlot) {
    ggplot(data=dc %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newConfirmed)) + 
      geom_line() + 
      scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
                   date_breaks="1 weeks", date_labels="%a.%d.%m") +
      ggtitle("AGES BundesLänder Timeline Confirmed: Wien")
  }
  
  # Read timeline of tested
  if(is.null(fileDate_cfz)) dt<-caAgesRead_cfz() else dt<-caAgesRead_cfz(csvFile=fileDate_cfz)
  if(bPlot) {
    ggplot(data=dt %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=newTested)) + 
      geom_line() + 
      scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
                   date_breaks="1 weeks", date_labels="%a.%d.%m") +
      ggtitle("AGES BundesLänder Timeline Tested: Wien")
  }
  
  # need to check if both datasets are available up to the same date
  jointDate <- min(max(dc$Date), max(dt$Date))
  
  # Join 'Confirmed' and 'Tested' datasets
  dj <- dc %>% left_join(dt, by=c("Date","RegionID", "Stamp","Region")) 
  
  # Exclude data before 2020-04-02, these have NA's vor newTested and sumTested
  df <- dj %>% 
    dplyr:: filter(Date <=jointDate) %>%
    dplyr::filter(Date>as.Date("2020-04-01")) %>% 
    dplyr::arrange(Region, Date)

  # impute wrong newTested
  idx <- which(df$newTested==0)
  df$newTested[idx] <- round((df$newTested[idx-1]+df$newTested[idx+1])/2)
  
  df <- df %>%
    dplyr::mutate(newConfPop=newConfirmed/Population*100000) %>%
    dplyr::mutate(newConfTest=newConfirmed/newTested)
  
  # impute wrong newConfTest TODO --> impute complete dataset upfront
  idx <- which(df$newConfTest >.75)
  df$newConfTest[idx] <- round((df$newConfTest[idx-1]+df$newConfTest[idx+1])/2)

  # apply rolling mean to 'new*' cols.
  rmSize=7
  df <- df %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(rm7NewTested=(rollmean(newTested, k=rmSize, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfirmed=(rollmean(newConfirmed, k=rmSize, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewRecovered=(rollmean(newRecovered, k=rmSize, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewDeaths=(rollmean(newDeaths, k=rmSize, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfPop=(rollmean(newConfPop, k=rmSize, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfTest=rollmean(newConfTest, k=rmSize, align="center", fill=NA)) %>%
    #dplyr::mutate_at(vars(starts_with("new")), rollmean, k=rmSize, align="center", fill=NA) %>%
    dplyr::ungroup() 
  # str(df)

  # patch rm7NewConfirmed data
  if(bEstimate) {
  dx <- caAgesRM7Estimate(df)
    for (i in 1:dim(dx)[1]) {
      idxf <- which(df$Date==dx$Date[i] & df$Region==dx$Region[i])
      idxx <- which(dx$Date==dx$Date[i] & dx$Region==dx$Region[i])
      df[idxf,"rm7NewConfirmed"] <- round(df[idxf,"newConfirmed"]/dx[idxx,"meanProConfirmed"])
      df[idxf,"rm7NewConfPop"] <- round(df[idxf,"rm7NewConfirmed"]/df[idxf,"Population"]*100000)
      df[idxf,"newConfirmed"] <- round(df[idxf,"newConfirmed"]/dx[idxx,"meanProConfirmed"])
      # cat(dx$WeekDay[i],dx$Region[i],as.numeric(dx[idxx,"meanProConfirmed"]),as.numeric(df[idxf,"newConfirmed"]),as.numeric(df[idxf,"rm7NewConfirmed"]),'\n')
    }
  }
  # add smootheds
  # Span adjusted manually to 19 after visual inspection of several options. maybe a bit too smooth. Smaller number for closer match to rm7
  if(bCompleteCases) {
    df <- df[complete.cases(df),]
  }
  
  span <- 19/dim(df)[1]*10
  df <- df %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(id=1:n()) %>%
    dplyr::mutate(smoothNewTested=round(predict(loess(newTested~id, span=span, na.omit=na.exclude)))) %>%
    dplyr::mutate(smoothNewConfirmed=round(predict(loess(newConfirmed~id, span=span, na.omit=na.exclude)))) %>%
    dplyr::mutate(smoothNewRecovered=round(predict(loess(newRecovered~id, span=span, na.omit=na.exclude)))) %>%
    dplyr::mutate(smoothNewDeaths=round(predict(loess(newDeaths~id, span=span, na.omit=na.exclude)))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(smoothNewConfPop=smoothNewConfirmed/Population*100000) %>%
    dplyr::mutate(smoothŃewConfTest=smoothNewConfirmed/smoothNewTested) %>% 
    dplyr::select(1:5,12,6:8,18,19,13,9:11,14:17,20:32)
  
  return(df)
}



# --------------------------------------------------------------------------------------------------------
# AGES Estimate of rm7 data for past three days based on estimate of over/under reports depending on day of week
# --------------------------------------------------------------------------------------------------------
caAgesRM7Estimate <- function(df, bPlot=FALSE) {
  # estimate the weekly rolling mean for today and the past two days by 
  # compensating the over/under estimation as the mean in the past three weeks
  begDate <- max(df$Date) - weeks(3) - days(3)
  endDate <- max(df$Date) - days(3)
  
  # plot 
  if (bPlot) {
    ggplot(data=df %>% dplyr::filter(Date > begDate), aes(x=Date, y=newConfirmed, group=Region, color=Region)) + geom_line(size=1.5) +
      geom_point(data=dfx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) +
      geom_line(data=dfx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) +
      geom_smooth(data=dfx, aes(x=Date, y=newConfirmed, color=Region), method="loess", n=10, se=FALSE, color="black",linetype=3) +
      facet_wrap(.~Region, nrow=2, scales="free_y")
  }
  
  # Calculate over/under estimation factor
  dft <- df %>%
    dplyr::filter(Date > begDate) %>%
    dplyr::mutate(WeekDay=wday(Date, week_start=getOption("lubridate.week.start",1))) %>%
    dplyr::mutate(proConfirmed=newConfirmed/rm7NewConfirmed) %>%
    dplyr::mutate(WeekNo=week(Date))
  
  if (bPlot) {
    ggplot(data=dft, aes(x=WeekDay, y=proConfirmed, shape=as.character(WeekNo))) +
      scale_x_continuous(breaks=1:7)+
      scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
      geom_point(size=5) +
      facet_wrap(.~Region, nrow=2)
  }
  
  # extract correction factor for the last three days from the same week days on the three weeks before
  estWeekDays <- dft %>% dplyr::filter(Date>endDate) %>% dplyr::select(WeekDay) %>% dplyr::distinct()
  estWeekDays <- as.data.frame(estWeekDays)[,1]
  dfp <- dft %>% 
    dplyr::filter(Date<=endDate) %>%
    dplyr::filter(WeekDay %in% estWeekDays) %>%
    dplyr::group_by(Region,WeekDay) %>% 
    dplyr::summarize(meanProConfirmed=mean(proConfirmed)) %>%
    dplyr::ungroup() %>%
    # add Date colum back
    dplyr::inner_join (dft %>% dplyr::filter(Date>endDate, Region=="Österreich") %>% dplyr::select(Date,WeekDay), by="WeekDay") %>%
    dplyr::select(Date, Region, WeekDay, meanProConfirmed)
  
  # let the caller handle the estimates
  return(dfp)    
}


# --------------------------------------------------------------------------------------------------------
# AGES Bundesländer: Ein/Nach Meldungen zu Tested, Confirmed auf Basis Datum Laborbefund
# --------------------------------------------------------------------------------------------------------
caAgesTestConfFlow <- function(flowDir="./extract") {
  base_dir <- "/home/at062084/DataEngineering/COVID-19/covid-ibm/covid-19-at-scraper"
  cftlFiles <- list.files(path=paste0(base_dir,"/",flowDir), pattern=paste0("^CovidFaelle_Timeline.*csv$"))
  cfzFiles  <- list.files(path=paste0(base_dir,"/",flowDir), pattern=paste0("^CovidFallzahlen.*csv$"))
  
  # construct list of files with matching dates
  cftlDates <- str_split(cftlFiles,"\\.", simplify=TRUE)[,2]
  cfzDates <- str_split(cfzFiles,"\\.", simplify=TRUE)[,2]
  Dates <- cftlDates[cftlDates %in% cfzDates]
  
  dr <- data.frame()
  for (d in Dates) {
    df <- caAgesRead_tlrm(fileDate_cftl=paste(base_dir,flowDir,paste("CovidFaelle_Timeline",d,"csv",sep="."),sep="/"), 
                          fileDate_cfz=paste(base_dir,flowDir,paste("CovidFallzahlen",d,"csv",sep="."),sep="/"),
                          bRolling=FALSE, bPlot=FALSE) %>% 
      dplyr::mutate(DateReport=as.Date(d, format="%Y%m%d")) %>%
      dplyr::select(Date, DateReport, Region, newTested, sumTested, newConfirmed, sumConfirmed)
    dr <- rbind(dr,df)  
  }
  
  write.csv(dr, paste0("TestConfFlow.csv"))
  
  dw <- dr %>% dplyr::filter(Region=="Wien")
  
  ggplot(data=dw, aes(x=Date, y=newConfirmed, group=Date, color=Date)) + 
    scale_x_date(limits=c(as.Date(strptime("2020-09-01",format="%Y-%m-%d")),NA), 
                 date_breaks="1 weeks", date_labels="%a.%d.%m") +
    geom_point(size=3) + geom_line() +
    geom_line(data=dw %>% 
                dplyr::group_by(Date) %>% 
                dplyr::summarize(minNewConfirmed=min(newConfirmed)) %>%
                dplyr::ungroup(), 
              mapping=aes(x=Date, y=minNewConfirmed), color="green", inherit.aes=FALSE) +
    geom_line(data=dw %>% 
                dplyr::group_by(Date) %>% 
                dplyr::summarize(maxNewConfirmed=max(newConfirmed)) %>%
                dplyr::ungroup(), 
              mapping=aes(x=Date, y=maxNewConfirmed), color="blue", size=1.5, inherit.aes=FALSE) +
    geom_line(data=dw %>% 
                dplyr::group_by(Date) %>% 
                dplyr::filter(newConfirmed != min(newConfirmed)) %>%
                dplyr::summarize(minNewConfirmed=min(newConfirmed)) %>%
                dplyr::ungroup(), 
              mapping=aes(x=Date, y=minNewConfirmed), color="red", size=1, inherit.aes=FALSE) +
    ggtitle("AGES: COVID-Austria: Vienna: Number of newConfirmed cases reported for Laborotory result date")
  
  ggplot(data=dw, aes(x=Date, y=newTested, group=Date, color=Date)) + 
    scale_x_date(limits=c(as.Date(strptime("2020-03-01",format="%Y-%m-%d")),NA), 
                 date_breaks="1 weeks", date_labels="%a.%d.%m") +
    geom_point(size=3) + geom_line() +
    geom_line(data=dw %>% 
                dplyr::group_by(Date) %>% 
                dplyr::summarize(minNewTested=min(newTested)) %>%
                dplyr::ungroup(), 
              mapping=aes(x=Date, y=minNewTested), color="green", inherit.aes=FALSE) +
    geom_line(data=dw %>% 
                dplyr::group_by(Date) %>% 
                dplyr::summarize(maxNewTested=max(newTested)) %>%
                dplyr::ungroup(), 
              mapping=aes(x=Date, y=maxNewTested), color="blue", size=1.5, inherit.aes=FALSE) +
    geom_line(data=dw %>% 
                dplyr::group_by(Date) %>% 
                dplyr::filter(newTested != min(newTested)) %>%
                dplyr::summarize(minNewTested=min(newTested)) %>%
                dplyr::ungroup(), 
              mapping=aes(x=Date, y=minNewTested), color="red", size=1, inherit.aes=FALSE) +
    ggtitle("AGES: COVID-Austria: Number of newTested cases reported for Laborotory result date")
  
}
