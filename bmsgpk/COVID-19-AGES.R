# -------------------------------------------------------------------------------------------------
# main
# -------------------------------------------------------------------------------------------------
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk"
setwd(wd)
source("./COVID-19-AGES-Data.R")

# read Ages Data
df <- caAgesRead_tlrm()

# Plot parameters
xTrans="identity"
xLimMin <- 1
yLimMin <- 0.85
yLimMax <- 1.20
xBreaks=c(1:10,seq(10,150,by=10))
fltRegion=""
# time range

# Calculate speed of spread in percent change of newConfirmed per day
nRegDays=7
dblDays=c(1:7,10,14,21,28,35,50,100,Inf,-100,-50,-28,-21,-14,-10,-7)
rolm <- rollify(.f=function(Date,vals) {exp(coef(lm(log(vals)~Date))[2])}, window=nRegDays)

nullDate=as.Date("2020-08-01")
ds <- df %>% 
  dplyr::filter(Date>=nullDate) %>%
  dplyr::arrange(Region,Date) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::mutate(rm7NewSpread=rolm(Date,rm7NewConfirmed)) %>%
  dplyr::mutate(smoothNewSpread=rolm(Date,smoothNewConfirmed)) %>%
  dplyr::ungroup() 

begDate=as.Date("2020-10-01")
dd <- ds %>% dplyr::filter(Date>begDate)

ggplot(data=dd %>% dplyr::arrange(Region,Date), 
       aes(x=rm7NewConfPop, y=rm7NewSpread, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(xLimMin,120), breaks=xBreaks, trans=xTrans) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=as.character(round(exp(log(2)/dblDays),2)), name="Tägliche Steigerungsrate")) +
  geom_path() + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Region, size=as.numeric(Date), stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  geom_path(data=dd %>% dplyr::filter(Region=="Österreich"), aes(x=rm7NewConfPop, y=rm7NewSpread), color="darkgreen", size=1) +
  geom_path(data=dd %>% dplyr::filter(Region=="Wien"), aes(x=rm7NewConfPop, y=rm7NewSpread), color="red", size=1) +
  geom_point(data=dd %>% dplyr::filter(weekdays(Date)==weekdays(max(Date))), 
             aes(x=rm7NewConfPop, y=rm7NewSpread, color=Region, shape=Region), size=5, stroke=1.5) +
  geom_text(data=dd %>% dplyr::filter(Date==max(Date)-days(3)), 
            aes(x=rm7NewConfPop, y=rm7NewSpread, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30") +
  geom_point(data=dd %>% dplyr::filter(Date==min(Date)+days(6)), 
             aes(x=rm7NewConfPop, y=rm7NewSpread, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dd %>% dplyr::filter(Date==min(Date)+days(6)), 
            aes(x=rm7NewConfPop, y=rm7NewSpread, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  ggtitle(paste("AGES COVID-19. Österreich:", fltRegion,"- Entwicklung der Verbreitungsrate und Neuinfektionen von", min(dd$Date),"bis", max(dd$Date))) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-PopSpreadRM7-",max(dd$Date),"-120.pdf"), dpi=300, width=4, height=3, scale=4)


ggplot(data=dd %>% dplyr::arrange(Region,Date), 
       aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(xLimMin,120), breaks=xBreaks, trans=xTrans) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=as.character(round(exp(log(2)/dblDays),2)), name="Tägliche Steigerungsrate")) +
  geom_path() + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Region, size=as.numeric(Date), stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  geom_path(data=dd %>% dplyr::filter(Region=="Österreich"), aes(x=smoothNewConfPop, y=smoothNewSpread), color="darkgreen", size=1) +
  geom_path(data=dd %>% dplyr::filter(Region=="Wien"), aes(x=smoothNewConfPop, y=smoothNewSpread), color="red", size=1) +
  geom_point(data=dd %>% dplyr::filter(weekdays(Date)==weekdays(max(Date))), 
             aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, shape=Region), size=5, stroke=1.5) +
  geom_text(data=dd %>% dplyr::filter(Date==max(Date)), 
            aes(x=smoothNewConfPop, y=smoothNewSpread, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30") +
  geom_point(data=dd %>% dplyr::filter(Date==min(Date)+days(6)), 
             aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dd %>% dplyr::filter(Date==min(Date)+days(6)), 
            aes(x=smoothNewConfPop, y=smoothNewSpread, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  ggtitle(paste("AGES COVID-19. Österreich:", fltRegion,"- Entwicklung der Verbreitungsrate und Neuinfektionen von", min(dd$Date),"bis", max(dd$Date))) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-PopSpreadSmooth-",max(dd$Date),"-120.pdf"), dpi=300, width=4, height=3, scale=4)



# Vienna Tested and Confirmed
dsw <- ds %>% dplyr::filter(Region=="Wien")
dsa <- ds %>% dplyr::filter(Region=="Österreich")

  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/10000, newConfirmed_x10=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed_x10) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed_x10)
  
  
ggplot(data=dsw, aes(x=Date, y=newConfirmed)) +

  scale_x_date(limits=c(as.Date(strptime("2020-08-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(0,1500), breaks=seq(0,1500,by=100)) + 

  # Confirmed
  geom_point(size=1, color="grey20") +  geom_line(color="grey20", linetype=3) +
  geom_line(aes(x=Date, y=rm7NewConfirmed), color="grey20") +
  geom_line(aes(x=Date, y=smoothNewConfirmed), color="grey20", linetype=2) +

  # Confirmed/Tested
  geom_point(aes(x=Date, y=newConfTest*1000), size=1, color="blue") +
  geom_line(aes(x=Date,  y=newConfTest*1000), linetype=3, color="blue") +
  geom_line(aes(x=Date, y=rm7NewConfTest*1000), color="blue") +
  geom_line(aes(x=Date, y=smoothŃewConfTest*1000), color="blue", linetype=2) +

  geom_point(aes(x=Date, y=newTested/10), size=1, color="red") +
  geom_line(aes(x=Date,  y=newTested/10), linetype=3, color="red") +
  geom_line(aes(x=Date, y=rm7NewTested/10), color="red") +
  geom_line(aes(x=Date, y=smoothNewTested/10), color="red", linetype=2)


  #scale_y_continuous(limits=c(.2,20), breaks=seq(1,20,by=1),
  #sec.axis = sec_axis(~ . *scaled, breaks=seq(0,1500,by=1000))) +
  
  
geom_smooth(method="lm", se=FALSE) +
  geom_point(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), size=1.5) + 
  geom_line(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), linetype=3, size=.75) + 
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Wien")





fltRegion=NULL
if (!is.null(fltRegion)) {
  dflmf <- dfrmd %>% dplyr::filter(Region %in% fltRegion)
  xTrans="log10"
  xBreaks=c(seq(.1,1,by=.1),1:10,seq(10,100,by=10))
  xLimMin <- .25
  yLimMin <- .85
  yLimMax <- 1.25
} else {
  dflmf <- dfrmd %>% dplyr::filter(newConfirmed>0)
  xTrans="identity"
  xBreaks=c(seq(0,100,by=10))
  xLimMin <- 0
  yLimMin <- .95
  yLimMax <- 1.2
}





# Vienna plot
scaled=1000
dfrmw <- df %>% dplyr::filter(Region=="Wien") %>% 
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
  scale_y_continuous(limits=c(.5,100),  breaks=c(1,2,5,7,10,15,20,30,40,50,60,70,80,90,100), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(1000,2000,5000,10000,20000,30000,40000,50000,60000,70000))) +
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




#fltRegion=c("Österreich", "Niederösterreich", "Steiermark", "Burgenland", "Wien")
#begDate=as.Date("2020-03-03")

#fltRegion="Wien"
#begDate=as.Date("2020-03-03")
#begDate=as.Date("2020-06-18")
begDate=as.Date("2020-10-01")
dfrmd <- dfrm %>% dplyr::filter(Date >= begDate)

fltRegion=c("Wien","Österreich")


yLimMin <- .90
yLimMax <- 1.25



# , group=Region, color=Region, shape=Region
ggplot(data=dflm7%>% dplyr::filter(Date>=as.Date("2020-08-01")), aes(x=Date, y=Spread)) + 
  scale_x_date(date_breaks="1 months", date_labels="%m") +
  scale_y_continuous(limits=c(.8,1.2))+
  geom_line() +
  facet_wrap(.~Region, ncol=5) + 
  ggtitle("AGES COVID-19 SpreadFactor vs. Date by Region")

ggplot(data=dflm7 %>% dplyr::filter(Date>=as.Date("2020-08-01")), aes(x=newConfPop, y=Spread)) + 
  scale_x_continuous(limits=c(xLimMin,100), breaks=c(seq(.1,1,by=.1),1:10,seq(10,100,by=10)), trans=xTrans) + 
  scale_y_continuous(limits=c(.8,1.2), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=as.character(round(exp(log(2)/dblDays),2)), name="Tägliche Steigerungsrate")) +
  geom_path() +
  facet_wrap(.~Region, ncol=5) +
  ggtitle("AGES COVID-19 SpreadFactor vs. newConfirmed/100.000 by Region")



# curl -X GET "https://api.netatmo.com/api/getpublicdata?lat_ne=16.0&lon_ne=48.0&lat_sw=16.1&lon_sw=48.1&filter=false" -H "accept: application/json"

# curl -X GET "https://api.netatmo.com/api/getpublicdata?lat_ne=16&lon_ne=48&lat_sw=16.1&lon_sw=48.1&filter=false" -H "accept: application/json" -H "Authorization: Bearer 5fa54f3699f37238057c4272|5b3c7efc208af00fe71ccf173fe4b7a3"

fltRegion=c("Wien","Österreich")
fltRegion=c("Wien")
begDate=as.Date("2020-10-01")
dfx <- df %>% 
  dplyr::filter(Region %in% fltRegion) %>% 
  dplyr::filter(newConfirmed>0) %>%
  dplyr::filter(Date >= begDate) %>%
  dplyr::select(Date, Region, newConfirmed, newTested, newConfPop, newConfTest)
  
dfrmx <- dfrm %>% 
  dplyr::filter(Region %in% fltRegion) %>% 
  dplyr::filter(newConfirmed>0) %>%
  dplyr::filter(Date >= begDate) %>%
  dplyr::select(Date, Region, newConfirmed, newTested, newConfPop, newConfTest)


ggplot(data=dfrmx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) + geom_line(size=1.5) +
  geom_point(data=dfx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) +
  geom_line(data=dfx, aes(x=Date, y=newConfirmed, group=Region, color=Region)) +
  geom_smooth(data=dfx, aes(x=Date, y=newConfirmed, color=Region), method="loess", n=10, se=FALSE, color="black",linetype=3) +
  facet_wrap(.~Region, nrow=2, scales="free_y")



dft <- dfx %>% left_join(dfrmx, by=c("Date","Region"), suffix=c("",".rm")) %>%
  dplyr::mutate(WeekDay=wday(Date, week_start=getOption("lubridate.week.start",1)), difConfirmed=(newConfirmed-newConfirmed.rm)/newConfirmed.rm, WeekNo=week(Date))
dft %>% group_by(Region,WeekDay) %>% ggplot(aes(x=WeekDay, y=difConfirmed, shape=as.character(WeekNo))) +
  scale_x_continuous(breaks=1:7)+
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(size=5) +
  facet_wrap(.~Region, nrow=2)

dw <- dft %>% dplyr::filter(Region=="Wien") %>%   dplyr::mutate(id=1:n()) %>%
  dplyr::mutate(smoothNewConfirmed1=round(loess(newConfirmed~id, span=19/dim(dft)[1])$fitted)) %>%
  dplyr::mutate(smoothNewConfirmed2=round(loess(newConfirmed~id, span=17/dim(dft)[1])$fitted))

plot(dw$Date, dw$newConfirmed)
lines(dw$Date, dw$newConfirmed.rm, col="black")
lines(dw$Date, dw$smoothNewConfirmed1, col="red")
lines(dw$Date, dw$smoothNewConfirmed2, col="blue")

dw %>% ggplot(aes(x=Date, y=newConfirmed)) + geom_point() + geom_line()


loess(newConfirmed~id, data=dw)$fitted



