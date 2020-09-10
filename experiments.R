

# -----------------------------------------------------------------------------------------------------------------------
# Addendum to "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/jhucsse"
# Predict Confirmed Cases and Deaths based on exponential rates calculated with robust models above
# -----------------------------------------------------------------------------------------------------------------------
r = 1
MTD = dfRegion$MTD[r]
expBeginC = dfRegion$expBeginC[r]
cRate = dfRegion$cRate[r]
cRateMin = dfRegion$cRateMin[r]
cRateMax = dfRegion$cRateMax[r]
expBeginD = dfRegion$expBeginD[r]
dRate = dfRegion$dRate[r]
dRateMin = dfRegion$dRateMin[r]
dRateMax = dfRegion$dRateMax[r]
Regions <- eval(dfRegion[r,"Region"])
if (Regions=="EU27") Regions <- EU27
if (Regions=="EU27IT") Regions <- EU27

estInitRange <- nRegDays
cutOffDate <- dfRegion[r,"cutOffDate"]

dfw <- dx %>%
  dplyr::filter(Stamp >= cutOffDate) %>%
  dplyr::group_by(Stamp) %>% filter(Region %in%  Regions)  %>%
  dplyr::summarize(sumConfirmed = sum(Confirmed), sumRecovered=sum(Recovered), sumDeaths=sum(Deaths))


maxDate = as.POSIXct(Sys.Date()) + days(14)
nDays = as.integer(maxDate - expBeginC) +2
expCOVID <- data.frame(Stamp=seq(from=expBeginC,length.out=nDays,by="days"),
                       Confirmed      =cRate^(0:(nDays-1)),
                       ConfirmedMin=cRateMin^(0:(nDays-1)),
                       ConfirmedMax=cRateMax^(0:(nDays-1)),
                       Deaths   =c(rep(0,round(MTD)),dRate^(0:(nDays-1-round(MTD)))),
                       DeathsMin=c(rep(0,round(MTD)),dRateMin^(0:(nDays-1-round(MTD)))),
                       DeathsMax=c(rep(0,round(MTD)),dRateMax^(0:(nDays-1-round(MTD))))
)
plot(log10(expCOVID$Confirmed)~expCOVID$Stamp, type="l", col="red", xlab="")
lines(log10(expCOVID$ConfirmedMin)~expCOVID$Stamp, type="l", col="black")
lines(log10(expCOVID$ConfirmedMax)~expCOVID$Stamp, type="l", col="black")
points(log10(dfw$sumConfirmed)~dfw$Stamp, pch=16, cex=1.5, col="blue")
lines(log10(expCOVID$Deaths)~expCOVID$Stamp, type="l", col="red")
lines(log10(expCOVID$DeathsMin)~expCOVID$Stamp, type="l", col="black")
lines(log10(expCOVID$DeathsMax)~expCOVID$Stamp, type="l", col="black")
points(log10(dfw$sumDeaths)~dfw$Stamp, pch=16, cex=1.5, col="blue")
title(main=paste("r=",r,"  Region=",paste(Regions,collapse="-"), "  cutOffDate=",cutOffDate,sep=""),
      sub=paste("Confirmed=",max(dfw$sumConfirmed), 
                " Deaths=",max(dfw$sumDeaths), 
                " MTD=", round(MTD), 
                " Rate=",round(max(dfw$sumDeaths)/dfw$sumConfirmed[nrow(dfw)-min(as.integer(nrow(dfw)),as.integer(MTD))+1],2)),
      xlab=paste("Prediction 2 Weeks:", " Confirmed=",round(max(expCOVID$Confirmed))," Deaths=", round(max(expCOVID$Deaths)), sep=""))
grid()

plot((expCOVID$Confirmed)~expCOVID$Stamp, type="l", col="black")
lines((expCOVID$ConfirmedMin)~expCOVID$Stamp, type="l", col="red", lty=2)
lines((expCOVID$ConfirmedMax)~expCOVID$Stamp, type="l", col="red", lty=2)
points((dfw$sumConfirmed)~dfw$Stamp, cex=1.25, col="blue")
grid()

# calculate death rate by model estimated Confirmed and Deaths
dRateM <- expCOVID$Deaths/expCOVID$Confirmed*100
plot(expCOVID$Stamp, dRateM, type="l", ylim=c(1.5,3.5), xlim=c(expCOVID$Stamp[16],max(expCOVID$Stamp))); grid()

# calculate death rate by model estimated Confirmed and Deaths
dRateD <- dfw$sumDeaths/dfw$sumConfirmed*100
points(dfw$Stamp, dRateD); grid()

# -----------------------------------------------------------------
nDays = 35
expBeginC <- as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
expBeginD <- as.POSIXct(coef(rlmD)[1], origin='1970-01-01')
expCOVID <- data.frame(Stamp=seq(from=expBegin,length.out=nDays,by="days"),
                       Confirmed=cRate^(0:(nDays-1)),
                       ConfirmedMin=cRateMin^(0:(nDays-1)),
                       ConfirmedMax=cRateMax^(0:(nDays-1)),
                       Deaths=dRate^(0:(nDays-1))
)
plot(log10(expCOVID$Confirmed)~expCOVID$Stamp, type="l", col="red")
lines(log10(expCOVID$ConfirmedMin)~expCOVID$Stamp, type="l", col="black")
lines(log10(expCOVID$ConfirmedMax)~expCOVID$Stamp, type="l", col="black")
points(log10(dfw$sumConfirmed)~dfw$Stamp, pch=16, cex=1.5, col="blue")
grid()

plot((expCOVID$Confirmed)~expCOVID$Stamp, type="l", col="black")
lines((expCOVID$ConfirmedMin)~expCOVID$Stamp, type="l", col="red", lty=2)
lines((expCOVID$ConfirmedMax)~expCOVID$Stamp, type="l", col="red", lty=2)
points((dfw$sumConfirmed)~dfw$Stamp, cex=1.25, col="blue")
grid()






(lead = min(as.integer(MTD),dim(dfw)[1]-2))
dfd <- dfw %>%
  dplyr::mutate(ccsumDeaths=lead(sumDeaths,lead)) %>%
  dplyr::mutate(CFR=(ccsumDeaths/sumConfirmed*100))
ggplot(data=dfd, aes(x=Stamp, y=CFR)) + geom_point() + geom_line()









# set work dir here
setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
detailsFile <- "html/COVID-19-austria.detail.20200325-0940.dmp"
detailsFile <- "html/details.dmp"
html <- read_file(detailsFile)


library(xml2)
x <- xml2::read_html(detailsFile)
xml_name(x)
xml_children(x)
xml_text(x)

xpathBezirke <- '//*[@id="tblBezirke"]'
tblBezirke <- xml_find_all(x, xpathBezirke)
xml_structure(tblBezirke)
xml_text(tblBezirke)
l <- xml2::as_list(tblBezirke)

xpathRowsBezirke <- './/td'
rowsBezirke <- xml_find_all(tblBezirke,xpathRowsBezirke)
tbl <- xml_text(rowsBezirke, trim=FALSE)
n = length(tbl)
df <- data.frame(Region=tbl[seq(3,n,by=2)], Count=tbl[seq(4,n,by=2)])

#/html/body/div[2]/div/div/article/div[1]/div[1]/div[2]/h2[3]
#//*[@id="tblBezirke"]
#/html/body/div[2]/div[2]/div[2]/table/tbody/tr[2]


# //*[@id="divErkrankungen"]
# //*[@id="Hospitalisiert"]
# //*[@id="Intensivstation"]
# //*[@id="MilderVerlauf"]
# //*[@id="divLetzteAktualisierung"]


# /html/body/div[2]/div[1]/div[6]/div/canvas[2]
# /html/body/div[2]/div[2]/div[3]/div[1]/div/canvas[2]
# /html/body/div[2]/div[2]/div[3]/div[2]/div/canvas[2]

library(xml2)
x <- xml2::read_html(detailsFile)

xpathBezirke <- '//*[@id="tblBezirke"]'
tblBezirke <- xml_find_all(x, xpathBezirke)

xpathRowsBezirke <- './/td'
rowsBezirke <- xml_find_all(tblBezirke,xpathRowsBezirke)
tbl <- xml_text(rowsBezirke, trim=TRUE)
n = length(tbl)
df <- data.frame(Region=tbl[seq(3,n,by=2)], Count=tbl[seq(4,n,by=2)])


bmsgpk/html/COVID-19-austria.detail.20200325-0044.dmp
bmsgpk/html/COVID-19-austria.detail.20200325-1531.dmp


bmsgpk/html/COVID-19-austria.detail.20200324-1412.dmp
bmsgpk/html/COVID-19-austria.detail.20200325-0940.dmp
bmsgpk/html/COVID-19-austria.detail.20200325-1510.dmp



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




#----------------------------------------------------------------------------------------------------
# Create a few plots
#----------------------------------------------------------------------------------------------------

# log(Count) with regression on last 4 days for selected regions
lmDays=4
fltRegion=c("AT","W","T","Szbg")
gg <- dl %>% dplyr::filter(Status=="Confirmed", Region %in% fltRegion) %>%
  ggplot(aes(x=Stamp,y=Count, color=Region, shape=Region)) + 
  geom_line(aes(linetype=Region)) + 
  geom_point(aes(shape=Region)) +
  stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(lmDays)))}, 
              method=lm, aes(color=Region, linetype="last4days"), fullrange=TRUE, se=FALSE) + 
  scale_y_log10() +
  xlim(as.POSIXct("2020-03-01"),as.POSIXct("2020-03-31")) +
  ggtitle(paste(format(max(df$Stamp),"%Y-%m-%d"),"COVID-19-Austria/bmsgpk: Confirmed.", "Regression last ", lmDays, " days"))
ggsave(filename=paste0("./thumbs/bmsgpk.Confirmed.logCount.lm-",lmDays,"d.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=200, height=150, units="mm", scale=1 ,dpi=100)
ggsave(filename=paste0("./plots/bmsgpk.Confirmed.logCount.lm-",lmDays,"d.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=350, height=200, units="mm", scale=.8 ,dpi=150)
ggplotly(gg, tooltip=c("Stamp","Region","Count"), width=1280, height=756, dynamicTicks=TRUE)

# Tested, Confirmed and Confirmed/Tested (scaled to similar scale)
gg <- dl %>% dplyr::filter(Region=="AT")  %>% 
  dplyr::select(-Region) %>%
  tidyr::spread(key=Status, value=Count) %>% 
  dplyr::filter(!is.na(Tested), !is.na(Confirmed)) %>% 
  ggplot(aes(x=as.Date(Stamp), y=Confirmed/Tested*100)) + 
  geom_line(aes(color="Confirmed/Tested*100")) +geom_point(aes(color="Confirmed/Tested*100")) + 
  geom_line(aes(x=as.Date(Stamp),y=Tested/1000, color="Tested/1000")) +
  geom_line(aes(x=as.Date(Stamp),y=Confirmed/100, color="Confirmed/100")) +
  scale_x_date(date_breaks = "days" , date_labels="%d.%m", limits=c(as.Date("2020-03-01"), as.Date("2020-03-20"))) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits=c(.1,15)) +
  ggtitle(paste(format(max(df$Stamp),"%Y-%m-%d"),"COVID-19-Austria/bmsgpk: Tested-Confirmed"))
ggsave(filename=paste0("./thumbs/bmsgpk.Tested-Confirmed.logCount.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=200, height=150, units="mm", scale=1 ,dpi=100)
ggsave(filename=paste0("./plots/bmsgpk.Tested-Confirmed.logCount.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=350, height=200, units="mm", scale=.8 ,dpi=150)


#----------------------------------------------------------------------------------------------------
# Calculate a few properties
#----------------------------------------------------------------------------------------------------

# Run rolling regression with windows size nRegDays
rolllm <- rollify(.f=function(Stamp, vals) {1/log10(exp(as.numeric(coef(lm(log(vals)~Stamp))[2])*24*3600))}, window=lmDays)
dr <- dl %>% 
  dplyr::group_by(Region,Status) %>%
  dplyr::filter(Count>0) %>% 
  dplyr::filter(hour(Stamp)==15) %>%  # picj last measurement for each day
  dplyr::filter(n()>=nRegDays) %>%
  dplyr::mutate(timesTenDays=rolllm(Stamp,Count)) %>%
  dplyr::ungroup()
str(dr)

gg <- dr %>% filter(Status=="Confirmed") %>% dplyr::filter(Region %in% fltRegion) %>%
  ggplot(aes(x=as.Date(Stamp),y=timesTenDays, color=Region, shape=Region)) + geom_line() + geom_point(size=3) +  #aes(linetype=Region)
  scale_x_date(date_breaks = "days" , date_labels="%d.%m", limits=c(as.Date("2020-03-07"), as.Date("2020-03-20"))) +
  ylim(0,20) +
  ggtitle(paste(format(max(df$Stamp),"%Y-%m-%d"),"COVID-19-Austria/bmsgpk: Days to Confirmed*10.", "Rolling Regression. Window=", lmDays, " days"))
ggsave(filename=paste0("./thumbs/bmsgpk.Confirmed.times10.lm-",lmDays,"d.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=200, height=150, units="mm", scale=1 ,dpi=100)
ggsave(filename=paste0("./plots/bmsgpk.Confirmed.times10.lm-",lmDays,"d.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=350, height=200, units="mm", scale=.75 ,dpi=150)






# Gather dat into long format by 'Status' and 'Region'. Imputate NAs with linear interpolation
dl <- df %>% 
  tidyr::gather(key=Region, value=Count, AT,B,K,Noe,Ooe,Szbg,Stmk,T,V,W) %>%
  dplyr::group_by(Status,Region) %>%
  dplyr::arrange(Status,Region,Stamp) %>% 
  imputeTS::na_interpolation(option="linear") %>%  # TODO: log-linear interpolation ! 
  dplyr::ungroup()
str(dl)

# Spread into wide format by Status and long format by 'Region'
dg <- dl %>% 
  tidyr::spread(key=Status, value=Count)
str(dg)


# ----------------------------------------------------------------------
# covid-county
# ----------------------------------------------------------------------

library(lubridate)
library(stringr)
library(readr)
library(dplyr)

sourceLinks <- c("https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html",
           "https://info.gesundheitsministerium.at",
           "https://www.sozialministerium.at/Informationen-zum-Coronavirus/Dashboard/Zahlen-zur-Hospitalisierung")

csvFileStates <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/covid-county/DE_states_202003261407.csv"
states <- read.csv(csvFileStates, stringsAsFactors=FALSE)
str(states)
colnames(states)

csvFileCounties <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/covid-county/DE_counties_202003261407.csv"
counties <- read.csv(csvFileCounties, stringsAsFactors=FALSE)
str(counties)
colnames(counties)

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



col.names <- c("Stamp","Region","newConfirmed","newDeaths", "sumConfirmed","sumDeaths")
replace_na(list(newConfirmed=0, newDeaths=0, sumConfirmed=0, sumDeaths=0))


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





