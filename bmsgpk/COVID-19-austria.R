# COVID-19 data provided by Austrian authoritiese
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(imputeTS)
library(tibbletime)
library(MASS)
library(scales)


setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")

#----------------------------------------------------------------------------------------------------
# Read and format data into wide and long formats
#----------------------------------------------------------------------------------------------------

# Read data. These are spread by 'Region' and gathered by 'Status'
csvFile <- "./data/COVID-19-austria.csv"
df <- read.csv(csvFile, stringsAsFactors=FALSE) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp))
str(df)


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
ggsave(filename=paste0("./plots/bmsgpk.Confirmed.logCount.lm-",lmDays,"d.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=350, height=200, units="mm", scale=.75 ,dpi=150)
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
ggsave(filename=paste0("./plots/bmsgpk.Tested-Confirmed.logCount.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=350, height=200, units="mm", scale=.75 ,dpi=150)


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
ggsave(filename=paste0("./plots/bmsgpk.Confirmed.times10.lm-",lmDays,"d.",format(max(df$Stamp),"%Y-%m-%d"),".png"), plot=gg, 
       width=350, height=200, units="mm", scale=.75 ,dpi=150)


