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

# git clone git@github.com:CSSEGISandData/COVID-19.git
# setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/jhucsse")
setwd("/home/at062084/DataEngineering/COVID-19/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")
system2("git", "pull")

baseDir=("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/jhucsse/")
nRegDays <- 4


# -----------------------------------------------------------------------------------------------------------------------
# Prepare COVID-19 data into dateframe grided on Day since 2020-02-01
# -----------------------------------------------------------------------------------------------------------------------

system2("cat", "01*2020.csv 02*2020.csv | grep -v Confirmed > covid6.csv")
col.names6 <- c("State","Region","Stamp","Confirmed","Deaths","Recovered")
colClasses6 <- c("character","character","character", rep("numeric",3))
df6 <- read.csv("covid6.csv", colClasses = colClasses6, col.names=col.names6, header=FALSE, sep=",", quote="\"", stringsAsFactors=FALSE)

system2("cat", "03-[01]?-2020.csv | grep -v Confirmed > covid8.csv")
col.names8 <- c("State","Region","Stamp","Confirmed","Deaths","Recovered","Lat","Lon")
colClasses8 <- c("character","character","character", rep("numeric",5))
df8 <- read.csv("covid8.csv", colClasses = colClasses8, col.names=col.names8, header=FALSE, sep=",", quote="\"", stringsAsFactors=FALSE) %>% 
  dplyr::select(-Lat, -Lon)

# Bind different source formats in terms of LonLat provided
df <- rbind(df6,df8)

# Handle ambiguous tine formats
df1 <- df %>% filter(grepl("T", Stamp))
df1$Stamp <- as.POSIXct(df1$Stamp)
df2 <- df %>% filter(!grepl("T", Stamp))
df21 <- df2 %>% filter(grepl("2020",Stamp))
df21$Stamp <- as.POSIXct(df21$Stamp, tryFormats=c("%m/%d/%Y %H:%M"))
df22 <- df2 %>% filter(!grepl("2020",Stamp))
df22$Stamp <- as.POSIXct(df22$Stamp, tryFormats=c("%m/%d/%y %H:%M"))

# all data from all files
df <- rbind(df1,df21,df22)
df <- df %>% mutate(Stamp=as.POSIXct(format(Stamp, format="%Y%m%d"),tryFormats=c("%Y%m%d")))


# Generate time grid all data will be mapped to
dateGrid <- data.frame(Stamp=seq(min(df$Stamp),max(df$Stamp),by="days"))

# Extract data for european countries
#europe <- c("Austria","Belgium","Croatia","Czech Republic", "Denmark", "Finland", "France", "Germany","Greece" ,"Iceland",
#            "Ireland", "Italy","Latvia","Luxembourg", "North Ireland", "Norway", "Portugal","Romania", "Spain", "Sweden",
#            "Switzerland","UK")
#europe <- c("Austria","Belgium","Croatia","Czech Republic", "Denmark", "Finland", "France", "Germany","Greece" ,"Iceland",
#            "Ireland", "Italy","Luxembourg",  "Norway", "Romania", "Spain", "Sweden",
#            "Switzerland","UK")
# dfe <- subset(df, Region %in% europe)

# ---------------------------------------------------------------------------------------------------------------------
# Imputation:
# ---------------------------------------------------------------------------------------------------------------------
# Remove all duplicates (introduced in original data on days with no report. these are duplicated from last report)
dft <- df %>%
  dplyr::distinct() %>%
  dplyr::mutate(Region = factor(Region), State=factor(State)) %>%
  dplyr::select(Stamp, State, Region, Confirmed, Recovered, Deaths)


# impute dates with missing data with a linear interpolation (method na_interpolation)
dx <- dft %>% dplyr::filter(Region=="xxx")               # create empty dataframe
dfr <- dft %>% dplyr::select(State, Region) %>% unique() # list of unique State+Region combinations
for (k in 1:dim(dfr)[1]) {                               # loop over these state+region combinations
  d <- dft %>%
    dplyr::filter(State==dfr[k,1], Region==dfr[k,2]) %>% # extract data for one state+region
    dplyr::arrange(Stamp)                                # sort by stamp for join
  if (dim(d)[1]>2 && sum(!is.na(d$Recovered))>=2 && sum(!is.na(d$Deaths))>=2) {  # filter out state+regions that report sensible data
    e <- d %>%
      dplyr::right_join(dateGrid, by="Stamp") %>%        # align data to day grid
      dplyr::mutate(State=dfr[k,1], Region=dfr[k,2]) %>% # fill in State and Region in case empty
      # impute days with missing values with linear interpolation
      dplyr::mutate(Confirmed=round(na_interpolation(Confirmed)),  
             Recovered=round(na_interpolation(Recovered)),
             Deaths=round(na_interpolation(Deaths)))
    dx <- rbind(dx,e)
  }
}
# working dataset
dx <- dx %>% dplyr::mutate(State=factor(State), Region=factor(Region))
dr <- dx %>% dplyr::filter(Region=="Spain")


# persist data
write.csv(dx, file=paste0(baseDir,"/data/COVID-19-jhucsse.csv"), row.names=FALSE, quote=FALSE)


# -----------------------------------------------------------------------------------------------------------------------
# Calculate Confirmed rate, Death rate and Time to Death for several regions
# -----------------------------------------------------------------------------------------------------------------------

allEU <- c("Italy","France","UK","Germany","Austria","Spain","Switzerland","Sweden","Norway","Netherlands","Belgium")
EU27IT <- c("Austria","France","Germany","Spain","Switzerland","Sweden","UK","Italy")
EU27 <- c("Austria","France","Germany","Spain","Switzerland","Sweden","UK")

# Dataframe describing the properities of regions and groups of regions
#dplyr::group_by(Stamp) %>% filter(Region=="Mainland China") %>%  # 15.2. 1743d ???
#dplyr::group_by(Stamp) %>% filter(Region=="South Korea") %>%     # 23.2.   15d
#dplyr::group_by(Stamp) %>% filter(Region=="Iran") %>%            # 21.2.

#dplyr::group_by(Stamp) %>% filter(Region=="Italy") %>%           # 24.2.   12d
#dplyr::group_by(Stamp) %>% filter(Region=="Germany") %>%         # 25.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Spain") %>%           # 25.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Switzerland") %>%     # 25.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Sweden") %>%          # 26.2.
#dplyr::group_by(Stamp) %>% filter(Region=="France") %>%          # 26.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Austria") %>%         # 26.2.
#dplyr::group_by(Stamp) %>% filter(Region=="UK") %>%              # 27.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Norway") %>%          # 28.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Netherlands") %>%     # 29.2.
#dplyr::group_by(Stamp) %>% filter(Region=="Belgium") %>%         #  3.3.
dfRegion <- data.frame(Region=c("Mainland China","South Korea","Iran","Italy","Germany","Spain","Switzerland","Sweden","France","Austria","UK","EU27","EU27IT"),
                       cutOffDate=as.POSIXct(paste("2020-02-",c("15","23","21","24","25","25","25","26","26","26","27","29","24"),sep="")))
#colnames(dfRegion) <- c("Region", "cutOffDate")
dfRegion$MTD <- 0
dfRegion$expBeginC <- as.POSIXct("2020-01-01")
dfRegion$cRate <- 0
dfRegion$cRateMin <- 0
dfRegion$cRateMax <- 0
dfRegion$expBeginD <- as.POSIXct("2020-01-01")
dfRegion$dRate <- 0
dfRegion$dRateMin <- 0
dfRegion$dRateMax <- 0
dfRegion$cutOffDate[12]=as.POSIXct("2020-03-01")

# -------------------------------------
# rolling weighted robust regression
# -------------------------------------
rrlm <- rollify(.f = function(Stamp, vals) {as.numeric(coef(MASS::rlm(log(vals)~Stamp))[2])*24*3600}, window=nRegDays)
#dr <- dx %>% group_by(Region) %>% mutate(RRLM = rrlm(Stamp, Confirmed)) 
#dr %>% dplyr::filter(RRLM>0.01) %>% ggplot(aes(x=Stamp, y=1/log10(exp(RRLM)))) + geom_line() + geom_point() + ylim(5,15) #ylim(0.5,2)


# -------------------------------------------------------------------------------------------------------------
covRegion <- function(dx, Regions="Italy", cutOffDate=as.POSIXct("2020-02-24"), bPlot=TRUE) {
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

# calculate properites for all defined regions
for (r in 1:nrow(dfRegion)) {
  Regions <- eval(dfRegion[r,"Region"])
  if (Regions=="EU27") Regions <- EU27
  if (Regions=="EU27IT") Regions <- EU27IT
  cutOffDate <- dfRegion[r,"cutOffDate"]
  cat(paste("r=",r,"  Region=",paste(Regions,collapse="-"), "  cutOffDate=",cutOffDate,"\n",sep=""))
  
  c <- tryCatch({covRegion(dx, Regions=Regions, cutOffDate=cutOffDate)})
  if (!inherits(c, "error")) {
    dfRegion[r,3:dim(dfRegion)[2]] <- c
  }
  else {
    cat(paste("ERROR: r=",r,"  Region=",regions, "  cutOffDate=",cutOffDate,"\n",sep=""))
  }
}


