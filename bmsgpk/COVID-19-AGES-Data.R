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
cfGKZtl <- "CovidFaelle_Timeline_GKZ.csv"  # https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv    # caAgesRead_cfGKZtl()
cftl <- "CovidFaelle_Timeline.csv"         # https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv        # caAgesRead_cftl()
cfz <- "CovidFallzahlen.csv"               # https://covid19-dashboard.ages.at/data/CovidFallzahlen.csv                                                                      # caAgesRead_cfz
tftl <- "TodesfaelleTimeline.csv"                                                                                   # caAgesRead_tftl()
#cfGKZ <- "CovidFaelle_GKZ.csv"            # https://covid19-dashboard.ages.at/data/CovidFaelle_GKZ.csv             
gtl <- "GenesenTimeline.csv"
#cfag <- "CovidFaelle_Altersgruppe.csv"
# epi <- "Epikurve.csv"


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
  # https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv
  csvFile <- paste0("./data/ages/",cfGKZtl)
  df <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
    dplyr::rename(RegionID=GKZ, Region=Bezirk, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, sumConfirmed=AnzahlFaelleSum, rmaConfirmed=AnzahlFaelle7Tage) %>%
    dplyr::rename(newDeaths=AnzahlTotTaeglich, sumDeaths=AnzahlTotSum) %>%
    dplyr::rename(newRecovered=AnzahlGeheiltTaeglich, sumRecovered=AnzahlGeheiltSum) %>%
    dplyr::mutate(newConfPop = newConfirmed/Population*100000) %>%
    dplyr::select(-SiebenTageInzidenzFaelle, -Time) %>% dplyr::select(11,12,2,1,3,4,6,13,9,7,5,10,8) 
  
  #dplyr::mutate(SiebenTageInzidenzFaelle=as.integer(SiebenTageInzidenzFaelle))
  #str(df)
  #summary(df)
  #unique(df$Region)
      
  # apply rolling mean to 'new*' cols
  df <- df %>%
    dplyr::arrange(Date, Region) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_at(vars(starts_with("new")), rollmean, k=7, fill=NA, align="right") %>%
    dplyr::ungroup()
  
  write.csv(df, file="./data/COVID-19-AGES-GKZ.csv", row.names=FALSE)
  #str(df)
  
  #Capitals=c("Wien","Eisenstadt","Sankt Pölten(Land)","Linz(Stadt)","Klagenfurth Stadt","Graz(Stadt)","Salzburg(Stadt)","Innsbruck-Stadt","Feldkirch")
  #Big10Cities=c("Wien","Graz(Stadt)","Linz(Stadt)","Baden","Vöcklabruck","Bregenz","Innsbruck-Stadt","Mödling","Amstetten","Kufstein")
  #dc <- df %>%
  #  dplyr::mutate(regionBigCity=Region %in% Big10Cities, regionCapital=Region %in% Capitals) %>%
  #  dplyr::group_by(regionBigCity, Date) %>%
  #  dplyr::summarize(cityConfPop = sum(newConfirmed)/sum(Population)*100000) %>%
  #  dplyr::ungroup()
  
  
  #ggplot(data=dfrm, aes(x=Date, y=newConfirmed/Population*1000000, color=Region)) + 
  #  geom_line() + 
  #  scale_x_date(limits=c(as.Date(strptime("2020-08-01",format="%Y-%m-%d")),NA), 
  #               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  #  scale_y_continuous(limits=c(0,500)) + 
  #  ggtitle("AGES Bezirke Timeline: Wien")
}

# -------------------------------------------------------------------------------------------
# CovidFaelle_Timeline.csv: TimeLine BundesLänder (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cftl <- function(csvFile=paste0("./data/ages/",cftl)) {
  # https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv
  dc <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(Time, format="%d.%m.%Y %H:%M:%S"), Date=date(Stamp)) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, Population=AnzEinwohner) %>%
    dplyr::rename(newConfirmed=AnzahlFaelle, newRecovered=AnzahlGeheiltTaeglich, newDeaths=AnzahlTotTaeglich) %>%
    dplyr::rename(sumConfirmed=AnzahlFaelleSum, sumRecovered=AnzahlGeheiltSum, sumDeaths=AnzahlTotSum) %>%
    dplyr::mutate(curConfirmed=sumConfirmed-sumRecovered-sumDeaths) %>%
    dplyr::select(-SiebenTageInzidenzFaelle, -Time, -AnzahlFaelle7Tage) %>% 
    dplyr::select(11,10,2,1,3,4,8,6,5,9,7,12)
  #str(dc)
  #summary(dc)
  return(dc)
}

# -------------------------------------------------------------------------------------------
# CovidFallzahlen.csv: TimeLine Bezirke (Confirmed, Recovered, Deaths)
# -------------------------------------------------------------------------------------------
caAgesRead_cfz <- function(csvFile=paste0("./data/ages/",cfz)) {
  # https://covid19-dashboard.ages.at/data/CovidFallzahlen.csv
  
  cfzImpute <- function(newTested) {
    newTested[newTested<0]=0
    newTested[newTested==0]=mean(newTested, na.rm=TRUE)
    newTested[is.na(newTested)]=mean(newTested, na.rm=TRUE)
    newTested=round(newTested)
    return(newTested)
  }    
  
  dt <- read.csv(csvFile, stringsAsFactors=FALSE, sep=";") %>% 
    dplyr::mutate(Stamp=as.POSIXct(MeldeDatum, format="%d.%m.%Y %H:%M:%S"), Date=date(as.POSIXct(Meldedat, format="%d.%m.%Y"))) %>%
    dplyr::rename(RegionID=BundeslandID, Region=Bundesland, sumTested=TestGesamt) %>%
    dplyr::rename(curHospital=FZHosp, freeHospital=FZHospFree, curICU=FZICU, freeICU=FZICUFree) %>%
    dplyr::arrange(Date) %>% 
    dplyr::group_by(Region) %>% 
    dplyr::mutate(newTested=sumTested-lag(sumTested)) %>% 
    dplyr::mutate(newTested=cfzImpute(newTested)) %>%
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
fileDate_cftl=NULL; fileDate_cfz=NULL; bPlot=FALSE
nRm7Days=7; bDt7=TRUE; nDt7Days=7; bLpr=TRUE; nLprDays=19
bResiduals=TRUE; dResFirst=as.Date("2020-07-01"); dResLast=as.Date("2020-12-07"); bShiftDown=TRUE
bPredict=TRUE; nPolyDays=7; nPoly=2
bEstimate=FALSE; bCompleteCases=FALSE

caAgesRead_tlrm <- function(fileDate_cftl=NULL, fileDate_cfz=NULL, bPlot=FALSE, 
                            nRm7Days=7, bDt7=TRUE, nDt7Days=7, bLpr=TRUE, nLprDays=19,
                            bResiduals=TRUE, dResFirst=as.Date("2020-07-01"), dResLast=as.Date("2020-12-07"), bShiftDown=TRUE,
                            bPredict=TRUE, nPolyDays=7, nPoly=2,
                            bEstimate=FALSE, bCompleteCases=FALSE) {
  
  # Read timeline of confirmed, hospitalized, deaths
  if(is.null(fileDate_cftl)) dc<-caAgesRead_cftl() else dc<-caAgesRead_cftl(csvFile=fileDate_cftl)
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

  # Add col for Month and Week
  df <- dj %>% 
    dplyr::mutate(Month=as.character(month(Date, label=TRUE))) %>%
    dplyr::mutate(Week=week(Date))
    
  # Add col for record grouped record ID
  df <- df %>% dplyr::arrange(Date, Region) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(ID=1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(1,2,19,20,3,4,21,5,6:18)
    
  # Exclude data before 2020-04-02, these have NA's vor newTested and sumTested
  df <- df %>% 
    dplyr:: filter(Date <=jointDate) %>%
    dplyr::filter(Date>as.Date("2020-04-01")) %>% 
    ### DIRTY: upcoming bad AGES data imputation depends on data to be sorted !!!
    dplyr::arrange(Region, Date)   

  # impute wrong newTested. Heuristic :(
  idx <- which(df$newTested==0 | is.na(df$newTested))
  df$newTested[idx] <- round((df$newTested[idx-1]+df$newTested[idx+1])/2)
  
  # impute wrong newTested. Heuristic :(
  idx <- which(is.na(df$curHospital))
  df$curHospital[idx] <- round((df$curHospital[idx-1]+df$curHospital[idx+1])/2)
  idx <- which(is.na(df$curICU))
  df$curICU[idx] <- round((df$curICU[idx-1]+df$curICU[idx+1])/2)
  idx <- which(is.na(df$freeHospital))
  df$freeHospital[idx] <- round((df$freeHospital[idx-1]+df$freeHospital[idx+1])/2)
  idx <- which(is.na(df$freeICU))
  df$freeICU[idx] <- round((df$freeICU[idx-1]+df$freeICU[idx+1])/2)
  
  
  # add derived properties
  df <- df %>%
    dplyr::mutate(newConfPop=newConfirmed/Population*100000) %>%
    dplyr::mutate(newConfTest=newConfirmed/newTested)
  
  # impute wrong newConfTest TODO --> impute complete dataset upfront
  idx <- which(df$newConfTest >.7) # Heuristic :(
  df$newConfTest[idx] <- round((df$newConfTest[idx-1]+df$newConfTest[idx+1])/2)

  # apply rolling mean to 'new*' cols.
  df <- df %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(rm7NewTested=(rollmean(newTested, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfirmed=(rollmean(newConfirmed, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewRecovered=(rollmean(newRecovered, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewDeaths=(rollmean(newDeaths, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfPop=(rollmean(newConfPop, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7NewConfTest=rollmean(newConfTest, k=nRm7Days, align="center", fill=NA)) %>%
    dplyr::mutate(rm7CurConfirmed=(rollmean(curConfirmed, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7CurHospital=(rollmean(curHospital, k=nRm7Days, align="center", fill=NA))) %>%
    dplyr::mutate(rm7CurICU=(rollmean(curICU, k=nRm7Days, align="center", fill=NA))) %>%
    #dplyr::mutate_at(vars(starts_with("new")), rollmean, k=nRm7Days, align="center", fill=NA) %>%
    dplyr::ungroup() 
  # str(df)
  
  
  # patch rm7* NA's with predicts from lm poly model
  if(bPredict) {
    dp <- caAgesRm7EstimatePoly(df, nPolyDays=nPolyDays, nPoly=nPoly) %>%
      dplyr::arrange(Date, Region)
    
    df <- df %>% dplyr::arrange(Date, Region)
    rm7Cols=c("rm7NewTested", "rm7NewConfirmed","rm7NewRecovered","rm7NewDeaths", "rm7NewConfPop", "rm7NewConfTest", 
              "rm7CurConfirmed", "rm7CurHospital", "rm7CurICU")
    df[df$Date %in% unique(dp$Date),rm7Cols] <- dp[,rm7Cols]
  }

  # df %>% dplyr::filter(Region=="Wien", Date > max(Date)-days(10)) %>% dplyr::select(Date, Region, newConfPop,rm7NewConfPop)
  
  # Calculate speed of spread in percent change of new* per day. TODO: better handling of zeros in data 
  rolm <- rollify(.f=function(Date,vals) {exp(coef(lm(log(vals+0.001)~Date), na.action=na.ignore, singular.ok=TRUE)[2])}, window=nDt7Days)
  
  # Add spread 
  if (bDt7) {
    # currently only looking at derivative of weekly means
    df <- df %>% 
      dplyr::arrange(Region,Date) %>% 
      dplyr::group_by(Region) %>% 
      dplyr::mutate(dt7rm7NewTested=rolm(Date,rm7NewTested)) %>%
      #dplyr::mutate(dt7NewConfirmed=rolm(Date,newConfirmed)) %>%
      dplyr::mutate(dt7rm7NewConfirmed=rolm(Date,rm7NewConfirmed)) %>%
      dplyr::mutate(dt7rm7NewConfPop=rolm(Date,rm7NewConfPop)) %>%
      dplyr::mutate(dt7rm7NewConfTest=rolm(Date,rm7NewConfTest)) %>%    # produces some NA's
      #dplyr::mutate(dt7CurConfirmed=rolm(Date,curConfirmed)) %>%
      dplyr::mutate(dt7rm7CurConfirmed=rolm(Date,rm7CurConfirmed)) %>%
      #dplyr::mutate(dt7CurHospital=rolm(Date,curHospital)) %>%
      dplyr::mutate(dt7rm7CurHospital=rolm(Date,rm7CurHospital)) %>%
      dplyr::mutate(dt7rm7CurICU=rolm(Date,rm7CurICU)) %>%
      #dplyr::mutate(dt7NewDeaths=rolm(Date,newDeaths)) %>%
      dplyr::mutate(dt7rm7NewRecovered=rolm(Date,rm7NewRecovered)) %>%
      dplyr::mutate(dt7rm7NewDeaths=rolm(Date,rm7NewDeaths)) %>%
      dplyr::ungroup() 
  }
  
  # add lpreds
  if(bLpr) {
    # Span adjusted manually to 19 after visual inspection of several options. maybe a bit too lpr. Smaller number for closer match to rm7
    span <- nLprDays/(dim(df)[1]/10) # Heuristic :(
    df <- df %>%
      dplyr::arrange(Region, Date) %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate(lprNewTested=round(predict(loess(newTested~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewConfirmed=round(predict(loess(newConfirmed~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewRecovered=round(predict(loess(newRecovered~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewDeaths=round(predict(loess(newDeaths~ID, span=span, na.action=na.exclude)))) %>%
      dplyr::mutate(lprNewConfPop=lprNewConfirmed/Population*100000) %>%
      dplyr::mutate(lprŃewConfTest=lprNewConfirmed/lprNewTested) %>% 
      dplyr::mutate(dt7lprNewConfirmed=rolm(Date,lprNewConfirmed)) %>%
      dplyr::mutate(dt7lprNewDeaths=rolm(Date,lprNewDeaths)) %>%
      dplyr::ungroup()
  } 
  
  # Calculate robust regression on log transformed 
  if (bResiduals) {
    
    modLogLM <- function(Date, Count, dResFirst=as.Date("2020-07-01"), dResLast=as.Date("2020-11-15"), bPlot=FALSE, bShiftDown=FALSE) {
      # Complete log(Count)~Date dataframe d, with index into dateRange s considered for regression
      d <- data.frame(Date=Date, logCount=log(Count+0.001))
      s <- Date>=dResFirst & Date<=dResLast
      # m <- MASS::rlm(logCount~Date, data=d[s,], method="MM", maxit=100)
      # model and prediction of log(Count)
      m <- lm(logCount~Date, data=d[s,])
      p <- predict(m, newdata=d)
      
      # Shift down regression line just below all data points
      if(bShiftDown) {
        i <- min(residuals(m))
        m$coefficients[1] <- m$coefficients[1]+i-0.01
      }
      q <- predict(m, newdata=d)
      
      if(bPlot) {
        gg <- ggplot(data=d, aes(x=Date, y=logCount)) + geom_point(size=.5) + 
          geom_line(data=data.frame(Date=Date,logPred=p), mapping=aes(x=Date, y=logPred), color="red", size=.25) +
          geom_line(data=data.frame(Date=Date,parPred=q), mapping=aes(x=Date, y=parPred), color="blue", size=.25)
        options(digits.secs=9)
        ggsave(filename=format(now(),"resNew-%H:%M:%OS9.png"), plot=gg, device="png")
      }
      
      # return prediction as Count (not log(Count))
      r <- exp(q)
      return(r)
    }
    
    df <- df %>%
      dplyr::arrange(Region, Date) %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate(modrm7NewTested=modLogLM(Date,rm7NewTested, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewConfirmed=modLogLM(Date,rm7NewConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewConfPop=modLogLM(Date,rm7NewConfPop, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewConfTest=modLogLM(Date,rm7NewConfTest, dResFirst, dResLast)) %>%    # produces some NA's
      dplyr::mutate(modrm7CurConfirmed=modLogLM(Date,rm7CurConfirmed, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7CurHospital=modLogLM(Date,rm7CurHospital, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7CurICU=modLogLM(Date,rm7CurICU, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewRecovered=modLogLM(Date,rm7NewRecovered, dResFirst, dResLast)) %>%
      dplyr::mutate(modrm7NewDeaths=modLogLM(Date,rm7NewDeaths, as.Date("2020-09-21"), as.Date("2020-12-07"))) %>%
      
      dplyr::mutate(resrm7NewTested=rm7NewTested-modrm7NewTested) %>%
      dplyr::mutate(resrm7NewConfirmed=rm7NewConfirmed-modrm7NewConfirmed) %>%
      dplyr::mutate(resrm7NewConfPop=rm7NewConfPop-modrm7NewConfPop) %>%
      dplyr::mutate(resrm7NewConfTest=rm7NewConfTest-modrm7NewConfTest) %>%    # produces some NA's
      dplyr::mutate(resrm7CurConfirmed=rm7CurConfirmed-modrm7CurConfirmed) %>%
      dplyr::mutate(resrm7CurHospital=rm7CurHospital-modrm7CurHospital) %>%
      dplyr::mutate(resrm7CurICU=rm7CurICU-modrm7CurICU) %>%
      dplyr::mutate(resrm7NewRecovered=rm7NewRecovered-modrm7NewRecovered) %>%
      dplyr::mutate(resrm7NewDeaths=rm7NewDeaths-modrm7NewDeaths) %>%
      dplyr::ungroup()
  }
  
  # patch rm7NewConfirmed data. DISABLED !
  if(bEstimate == "#") {
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
  
  # remove NA's
  if(bCompleteCases) {
    df <- df[complete.cases(df),]
  }
  
  # Sort cols
  df <- df %>% dplyr::select(1:8,sort(colnames(df)[c(9:dim(df)[2])]))
  
  write.csv(df, file="./data/COVID-19-AGES-Curated.csv", quote=FALSE, sep=" ", dec=".", na="NA", row.names=FALSE)
  return(df)
}


# --------------------------------------------------------------------------------------------------------
# AGES Estimate of rm7 data for last three days with poly fit to rm7 data
# Fit a polynom to the last nPolyDays days
# --------------------------------------------------------------------------------------------------------
caAgesRm7EstimatePoly <- function(df, nPolyDays=7, nPoly=2, nRm7Days=7) {

  curDate <- max(df$Date)                      
  maxDate <- curDate - days(floor(nRm7Days/2)) # Prediction interval: last day
  minDate <- maxDate - days(nPolyDays)         # Prediction interval: first day
  
  rm7PolyLog <- function(y, nPoly=2, nPolyDays=7) {
    nx=1:length(y)
    x <- 1:nPolyDays
    y <- y[x]
    pm <- lm(formula = log(y) ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=x)
    exp(predict(pm, newdata=data.frame(x=nx)))
  }
  rm7PolyLin <- function(y, nPoly=2, nPolyDays=7) {
    nx=1:length(y)
    x <- 1:nPolyDays
    y <- y[x]
    pm <- lm(formula = y ~ poly(x, nPoly, raw=TRUE), na.action="na.omit", weights=x)
    predict(pm, newdata=data.frame(x=nx))
  }
  
  # Calc lm for each Region and each rm7 feature for the days since rollingMean center
  dp <- df %>%
    dplyr::filter(Date>minDate) %>%
    dplyr::select(Date, Region, starts_with("rm7")) %>%
    dplyr::group_by(Region) %>%
    # Log poly model for potentially exponentially growing items
    dplyr::mutate_at(vars(c(starts_with("rm7"),-rm7NewTested,-rm7NewConfTest)), rm7PolyLog, nPoly, nPolyDays) %>%
    # nonLog linear model for newTested
    dplyr::mutate_at(vars(rm7NewTested), rm7PolyLin, 2, nPolyDays) %>%
    # Calc newConfProp from estimated Confirmed and Tested
    dplyr::mutate(rm7NewConfTest = rm7NewConfirmed/rm7NewTested) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Date>maxDate)
  
  #dp %>% dplyr::filter(Region=="Wien", Date > max(Date)-days(10)) %>% dplyr::select(Date, Region, rm7NewConfPop)
  
  return(dp)
}

 
# --------------------------------------------------------------------------------------------------------
# AGES Estimate of rm7 data for past three days based on estimate of over/under reports depending on day of week
# --------------------------------------------------------------------------------------------------------
caAgesRM7Estimate <- function(df, bPlot=FALSE, nWeeks=5) {
  # estimate the weekly rolling mean for today and the past two days by 
  # compensating the over/under estimation as the mean in the past three weeks
  begDate <- max(df$Date) - weeks(nWeeks) - days(3)
  endDate <- max(df$Date) - days(3)
  
  dfx <- df %>% dplyr::filter(Date > begDate)
  # plot 
  if (bPlot) {
    ggplot(data=df, aes(x=Date, y=newConfirmed, group=Region, color=Region)) + geom_line(size=1.5) +
      geom_point(data=dfx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) +
      geom_line(data=dfx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) +
      geom_smooth(data=df, aes(x=Date, y=newConfirmed, color=Region), method="loess", n=10, se=FALSE, color="black",linetype=3) +
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
      geom_line(aes(y=1)) +
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

reg <- "Oberösterreich"
ggplot(data=df%>%dplyr::filter(Date>max(Date)-days(38),Region==reg), aes(x=Date, y=rm7NewConfirmed, color=Region)) + 
  geom_point()+geom_line(size=1.5)+
  geom_point(aes(y=newConfirmed))+
  geom_line(aes(y=newConfirmed), linetype=2, size=0.5) +
  geom_point(data=dc%>%dplyr::filter(Date>max(Date)-days(38),Region==reg), aes(y=newConfirmed))+
  geom_line(data=dc%>%dplyr::filter(Date>max(Date)-days(38),Region==reg), aes(y=newConfirmed), linetype=2, size=0.5, color="blue")

# --------------------------------------------------------------------------------------------------------
# AGES Bundesländer: Ein/Nach Meldungen zu Tested, Confirmed auf Basis Datum Laborbefund
# --------------------------------------------------------------------------------------------------------
caAgesTestConfFlow <- function(flowDir="./extract") {
  base_dir <- "/home/at001335/DataEngineering/COVID-19/covid-ibm/covid-19-at-scraper"
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
