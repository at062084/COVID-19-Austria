# =================================================================================================
# main
# =================================================================================================
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk"
setwd(wd)
source("./COVID-19-AGES-Data.R")

# read Ages Data
df <- caAgesRead_tlrm(bEstimate=FALSE, bCompleteCases=FALSE)
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/pdf"
setwd(wd)

# -------------------------------------------------------------------------------------------------
# rm7NewConfPop ~ Date | Region
# -------------------------------------------------------------------------------------------------
ggplot(data=df, 
       aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region)) +
  scale_shape_manual(values=c(1:10)) +
  scale_x_date(limits=c(as.Date(strptime("2020-10-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(.1,130), breaks=c(seq(.1,1,by=.1),1:10,seq(10,150,by=10)), position="right") + 
  geom_point(size=3) + 
  geom_line() +
  geom_line(data=df %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=rm7NewConfPop), color="darkgreen", size=1) +
  geom_line(data=df %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=rm7NewConfPop), color="red", size=1) +
  geom_line(data=df %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=max(Date-days(7))), aes(x=Date, y=newConfPop), color="darkgreen", size=.5, linetype=3) +
  geom_line(data=df %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=max(Date-days(7))), aes(x=Date, y=newConfPop), color="red", size=.5, linetype=3) +
  ggtitle("AGES BundesLänder Timeline newConfirmed/per100.000 WeekMeans")
ggsave(file=paste0("COVID-19-rm7NewConfPop-ATW-",max(df$Date),".pdf"), dpi=300, width=4, height=3, scale=4)



# Plot parameters
xTrans="identity"
xLimMin <- 1
yLimMin <- 0.90
yLimMax <- 1.20
xBreaks=c(1:10,seq(10,150,by=10))
fltRegion=""
# time range

# Calculate speed of spread in percent change of newConfirmed per day
nRegDays=7
dblDays=c(1:7,10,14,21,28,35,50,100,Inf,-100,-50,-28,-21,-14,-10,-7)
rolm <- rollify(.f=function(Date,vals) {exp(coef(lm(log(vals)~Date), na.action=na.ignore, singular.ok=TRUE)[2])}, window=nRegDays)

nullDate=as.Date("2020-09-14") # zero newConfirmed in Salzburg on 2020-09-13 !!! TODO correct !!!
maxDate <- max(df$Date)
ds <- df %>% 
  dplyr::filter(Date>=nullDate) %>%
  dplyr::filter(!is.na(rm7NewConfirmed)) %>%
  dplyr::arrange(Region,Date) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::mutate(newSpread=rolm(Date,newConfirmed)) %>%
  dplyr::mutate(rm7NewSpread=rolm(Date,rm7NewConfirmed)) %>%
  dplyr::mutate(smoothNewSpread=rolm(Date,smoothNewConfirmed)) %>%
  dplyr::ungroup() 


# -------------------------------------------------------------------------------------------------
# rm7NewSpread ~ Date | Region
# -------------------------------------------------------------------------------------------------
ggplot(data=ds, 
       aes(x=Date, y=rm7NewSpread, color=Region, shape=Region)) +
  scale_shape_manual(values=c(1:10)) +
  scale_x_date(limits=c(as.Date(strptime("2020-10-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m", name=paste0("Datum (Rohdaten bis ",maxDate, ", Weekly rolling mean bis ",max(ds$Date),")")) +
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=as.character(round(exp(log(2)/dblDays),2)), name="Tägliche Steigerungsrate")) +
  geom_point(size=3) + 
  geom_line() +
  geom_line(data=ds %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=rm7NewSpread), color="darkgreen", size=1) +
  geom_line(data=ds %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=smoothNewSpread), color="black", size=1, linetype=3) +
  geom_line(data=ds %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=rm7NewSpread), color="red", size=1) +
  geom_line(data=ds %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=smoothNewSpread), color="black", size=1, linetype=3) +
  geom_line(data=ds %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=max(Date)-days(7)), aes(x=Date, y=newSpread), color="darkgreen", size=.5, linetype=2) +
  geom_line(data=ds %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=max(Date)-days(7)), aes(x=Date, y=newSpread), color="red", size=.5, linetype=3) +
  geom_line(data=ds, aes(x=Date, y=1), color="blue", size=1, linetype=5) +
  ggtitle("AGES BundesLänder Timeline SpreadFactor WeekMeans [y-scales: left: % newConfirmed/Day, right: Number of days to double newConfirmed]")
ggsave(file=paste0("COVID-19-rm7NewSpread-ATW-",maxDate,".pdf"), dpi=300, width=4, height=3, scale=4)




# -------------------------------------------------------------------------------------------------
# rm7NewSpread ~ rm7NewConfPop | Region
# -------------------------------------------------------------------------------------------------
begDate <- as.Date("2020-10-12")
ds <- df %>% 
  dplyr::filter(Date>=nullDate) %>%
  # dplyr::filter(!is.na(rm7NewConfirmed)) %>%
  dplyr::arrange(Region,Date) %>% 
  dplyr::group_by(Region) %>% 
  dplyr::mutate(newSpread=rolm(Date,newConfirmed)) %>%
  dplyr::mutate(rm7NewSpread=rolm(Date,rm7NewConfirmed)) %>%
  dplyr::mutate(smoothNewSpread=rolm(Date,smoothNewConfirmed)) %>%
  dplyr::ungroup() 
dd <- ds %>% dplyr::filter(Date >= begDate)

ggplot(data=dd %>% dplyr::arrange(Region,Date), 
       aes(x=rm7NewConfPop, y=rm7NewSpread, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(xLimMin,120), breaks=xBreaks, trans=xTrans) + 
  scale_y_continuous(limits=c(.9,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_path() + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Region, size=as.numeric(Date), stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  geom_path(data=dd %>% dplyr::filter(Region=="Österreich"), aes(x=rm7NewConfPop, y=rm7NewSpread), color="darkgreen", size=1) +
  geom_path(data=dd %>% dplyr::filter(Region=="Wien"), aes(x=rm7NewConfPop, y=rm7NewSpread), color="red", size=1) +
  geom_point(data=dd %>% dplyr::filter(weekdays(Date)==weekdays(min(Date))), 
             aes(x=rm7NewConfPop, y=rm7NewSpread, color=Region, shape=Region), size=5, stroke=1.5) +
  geom_text(data=dd %>% dplyr::filter(Date==max(Date)-days(3)), 
            aes(x=rm7NewConfPop, y=rm7NewSpread, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30") +
  geom_point(data=dd %>% dplyr::filter(Date==min(Date)), 
             aes(x=rm7NewConfPop, y=rm7NewSpread, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dd %>% dplyr::filter(Date==min(Date)), 
            aes(x=rm7NewConfPop, y=rm7NewSpread, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  geom_line(data=dd, aes(x=rm7NewConfPop, y=1)) +
  ggtitle(paste("AGES COVID-19. Österreich: WochenMittel der Entwicklung der Verbreitungsrate und Neuinfektionen von", min(dd$Date),"bis", max(dd$Date), "(",max(dd$Date)-days(3),")")) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-PopSpreadRM7-",max(dd$Date),"-120-2.pdf"), dpi=300, width=4, height=3, scale=4)

# -------------------------------------------------------------------------------------------------
# smoothNewSpread ~ smoothNewConfPop | Region
# -------------------------------------------------------------------------------------------------
ggplot(data=dd %>% dplyr::arrange(Region,Date), 
       aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(xLimMin,120), breaks=xBreaks, trans=xTrans) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_path() + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Region, size=as.numeric(Date), stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  geom_path(data=dd %>% dplyr::filter(Region=="Österreich"), aes(x=smoothNewConfPop, y=smoothNewSpread), color="darkgreen", size=1) +
  geom_path(data=dd %>% dplyr::filter(Region=="Wien"), aes(x=smoothNewConfPop, y=smoothNewSpread), color="red", size=1) +
  geom_point(data=dd %>% dplyr::filter(weekdays(Date)==weekdays(min(Date))), 
             aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, shape=Region), size=5, stroke=1.5) +
  geom_text(data=dd %>% dplyr::filter(Date==max(Date)), 
            aes(x=smoothNewConfPop, y=smoothNewSpread, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30") +
  geom_point(data=dd %>% dplyr::filter(Date==min(Date)), 
             aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dd %>% dplyr::filter(Date==min(Date)), 
            aes(x=smoothNewConfPop, y=smoothNewSpread, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  geom_line(data=dd, aes(x=rm7NewConfPop, y=1)) +
  ggtitle(paste("AGES COVID-19. Österreich: Geglättete Entwicklung der Verbreitungsrate und Neuinfektionen von", min(dd$Date),"bis", max(dd$Date))) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-PopSpreadSmooth-",max(dd$Date),"-120-2.pdf"), dpi=300, width=4, height=3, scale=4)



# -------------------------------------------------------------------------------------------------
# rm7NewSpread ~ rm7NewConfPop | Region: Complete Timeline
# -------------------------------------------------------------------------------------------------
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk"
setwd(wd)
df <- caAgesRead_tlrm(bEstimate=FALSE, bCompleteCases=FALSE)
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/pdf"
setwd(wd)

xTrans="log10"
xLimMin <- .5
xLimMax <- 120
yLimMin <- 0.9
yLimMax <- 1.22
xBreaks=c(1,2,5,10,20,50,100)
nRegDays=7
dblDays=c(1:7,10,14,21,28,35,50,100,Inf,-100,-50,-28,-21,-14,-10,-7)
rolm <- rollify(.f=function(Date,vals) {exp(coef(lm(log(vals+0.001)~Date), na.action=na.ignore, singular.ok=TRUE)[2])}, window=nRegDays)
begDate <- as.Date("2020-07-01")
  
dt <- df %>% 
  dplyr::filter(Date>=begDate) %>%
  dplyr::group_by(Region) %>% 
  dplyr::mutate(newSpread=rolm(Date,newConfirmed)) %>%
  dplyr::mutate(rm7NewSpread=rolm(Date,rm7NewConfirmed)) %>%
  dplyr::mutate(smoothNewSpread=rolm(Date,smoothNewConfirmed)) %>%
  dplyr::ungroup()  %>%
  dplyr::mutate(Month=as.character(month(Date, label=TRUE)))
  
ggplot(data=dt %>% dplyr::arrange(Region,Date), 
       aes(x=rm7NewConfPop, y=rm7NewSpread)) + 
  scale_x_continuous(limits=c(xLimMin,xLimMax), breaks=xBreaks, trans=xTrans) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_path(aes(color=Month)) + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Month, color=Month), size=2) +
  geom_line(data=dt, aes(x=rm7NewConfPop, y=1)) +
  facet_wrap(Region~., nrow=2)+
  ggtitle(paste("AGES COVID-19. Österreich: WochenMittel der Entwicklung der Verbreitungsrate und Neuinfektionen von", min(dt$Date),"bis", max(dt$Date), "(",max(dt$Date)-days(3),")")) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-PopSpreadRM7-",max(dt$Date),"-120-Region.pdf"), dpi=300, width=4, height=3, scale=4)


# -------------------------------------------------------------------------------------------------
# rm7NewSpread ~ rm7NewConfPop | Region: Complete Timeline
# -------------------------------------------------------------------------------------------------
xTrans="identity"
xLimMin <- as.Date("2020-04-01")
xLimMax <- max(dt$Date)
yLimMin <- 0.5
yLimMax <- 1.5
dblDays=c(1:7,10,14,21,30,60,Inf,-60,-30,-21,-14,-10,seq(-7,-1,by=1))
dt <- df %>% 
  dplyr::arrange(Region,Date) %>% 
  dplyr::filter(!is.na(rm7NewConfirmed)) %>%
  dplyr::group_by(Region) %>% 
  dplyr::mutate(newSpread=rolm(Date,newConfirmed)) %>%
  dplyr::mutate(rm7NewSpread=rolm(Date,rm7NewConfirmed)) %>%
  dplyr::mutate(smoothNewSpread=rolm(Date,smoothNewConfirmed)) %>%
  dplyr::ungroup() 
ggplot(data=dt %>% dplyr::arrange(Region,Date), 
       aes(x=Date, y=rm7NewSpread, color=Region, shape=Region, group=Region)) + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  scale_x_date(limits=c(xLimMin,xLimMax),date_breaks="1 months", date_labels="%B") + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_line() + 
  geom_point(size=1.5) +
  geom_line(data=dt, aes(x=Date, y=1)) +
  geom_line(data=dt %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=rm7NewSpread), color="red", size=1.5) +
  geom_line(data=dt %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=rm7NewSpread), color="black", size=.75) +
  ggtitle(paste("AGES COVID-19. Österreich: WochenMittel der Entwicklung der Verbreitungsrate von", min(dt$Date),"bis", max(dt$Date), "(",max(dt$Date)-days(3),")")) +
  # xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-DateSpreadRM7-April-",max(dt$Date),".pdf"), dpi=300, width=4, height=3, scale=4)





# -------------------------------------------------------------------------------------------------
# Vienna Tested and Confirmed
# -------------------------------------------------------------------------------------------------

dsw <- ds %>% dplyr::filter(Region=="Wien")
dsa <- ds %>% dplyr::filter(Region=="Österreich")

ggplot(data=dsw, aes(x=Date, y=newConfirmed)) +

  scale_x_date(limits=c(as.Date(strptime("2020-10-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(0,2000), breaks=seq(0,2000,by=100)) + 

  # Confirmed
  geom_point(size=1, color="grey20") +  geom_line(color="grey20", linetype=3) +
  geom_line(aes(x=Date, y=rm7NewConfirmed), color="grey20") +
  geom_line(aes(x=Date, y=smoothNewConfirmed), color="grey20", linetype=2) +

  # Confirmed/Tested
  geom_point(aes(x=Date, y=newConfTest*10000), size=1, color="blue") +
  geom_line(aes(x=Date,  y=newConfTest*10000), linetype=3, color="blue") +
  geom_line(aes(x=Date, y=rm7NewConfTest*10000), color="blue") +
  geom_line(aes(x=Date, y=smoothŃewConfTest*10000), color="blue", linetype=2) +

  geom_point(aes(x=Date, y=newTested/10), size=1, color="red") +
  geom_line(aes(x=Date,  y=newTested/10), linetype=3, color="red") +
  geom_line(aes(x=Date, y=rm7NewTested/10), color="red") +
  geom_line(aes(x=Date, y=smoothNewTested/10), color="red", linetype=2) +

  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Wien")

ggsave(file=paste0("COVID-19-rm7ConfTest-W-",max(dsw$Date),".pdf"), dpi=300, width=4, height=3, scale=4)



dk <- df %>% 
  dplyr::filter(!is.na(rm7NewConfPop)) %>%
  dplyr::group_by(Region) %>% dplyr::filter(rm7NewConfPop>=10) 
#  dplyr::filter(Region=="Wien" | Region=="Österreich" | Region=="Oberösterreich" | Region=="Tirol")
ggplot(data=dk, aes(x=rm7NewConfPop, y=rm7NewConfTest, shape=Region, color=Region)) +
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  scale_x_continuous(limits=c(0,150), breaks=seq(10,150,by=10)) +
  scale_y_continuous(limits=c(0,0.55), breaks=seq(0,1,by=.05)) +
  geom_line() + geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE) +
  geom_text(data=dk %>% dplyr::group_by(Region) %>% dplyr::filter(rm7NewConfPop==max(rm7NewConfPop)), 
            aes(x=rm7NewConfPop, y=rm7NewConfTest, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30", inherit.aes=FALSE) +
  ggtitle("AGES BundesLänder rm7NewConfTest~rm7NewConfPop | Region")
ggsave(file=paste0("COVID-19-rm7ConfTest-rm7ConfPop-",max(dk$Date),".pdf"), dpi=300, width=4, height=3, scale=4)



fltRegion=NULL
if (!is.null(fltRegion)) {
  #dflmf <- dfrmd %>% dplyr::filter(Region %in% fltRegion)
  xTrans="log10"
  xBreaks=c(seq(.1,1,by=.1),1:10,seq(10,100,by=10))
  xLimMin <- .25
  yLimMin <- .85
  yLimMax <- 1.25
} else {
  #dflmf <- dfrmd %>% dplyr::filter(newConfirmed>0)
  xTrans="identity"
  xBreaks=c(seq(0,100,by=10))
  xLimMin <- 0
  yLimMin <- .95
  yLimMax <- 1.2
}


# -------------------------------------------------------------------------------------------------------------------
# Vienna plot
# -------------------------------------------------------------------------------------------------------------------
scaled=100
dfw <- df %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/100, newConfirmed=newConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed)
dfrmw <- df %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfirmedTested=rm7NewConfTest*100, newTested=rm7NewTested/100, newConfirmed=rm7NewConfirmed/100) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed)

# Weekly moving average
ggplot(data=dfrmw, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.1,100), breaks=c(seq(1,10,by=1),12,15,seq(10,40,by=10)), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000)))) +
  geom_point(size=3) + geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  geom_point(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), size=1.5) + 
  geom_line(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), linetype=3, size=.75) + 
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Wien")
ggsave(file=paste0("COVID-19-rm7ConfTest-W-20202-07-01_",max(dsw$Date),".pdf"), dpi=300, width=4, height=3, scale=4)



# -------------------------------------------------------------------------------------------------------------------
# Austria plots
# -------------------------------------------------------------------------------------------------------------------
dfrmat <- df %>% dplyr::filter(Region=="Österreich") %>% 
  dplyr::mutate(relConfirmedTested=rm7NewConfTest*100, newTested=rm7NewTested/1000, newConfirmed=rm7NewConfirmed/1000) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed)
dfat <- df %>% dplyr::filter(Region=="Österreich") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/1000, newConfirmed=newConfirmed/1000) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed)

ggplot(data=dfrmat, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.05,50),  breaks=c(seq(1,10,by=1),12,15,seq(10,30,by=10)), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000), seq(10000,100000,by=10000)))) +
  geom_point(size=2) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  geom_point(data=dfat, aes(x=Date, y=Count, color=Status, shape=Status), size=1) + 
  geom_line(data=dfat, aes(x=Date, y=Count, color=Status, shape=Status), linetype=3, size=.5) + 
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Österreich")
ggsave(file=paste0("COVID-19-rm7ConfTest-AT-20202-07-01_",max(dsw$Date),".pdf"), dpi=300, width=4, height=3, scale=4)






  



