# =================================================================================================
# main
# =================================================================================================
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk"
setwd(wd)
source("./COVID-19-AGES-Data.R")
  
# read Ages Data
df <- caAgesRead_tlrm(bEstimate=FALSE, bCompleteCases=FALSE, bPredict=TRUE, nPolyDays=14, nPoly=2, bShiftDown=FALSE, nDt7Days=7)
dg <- caAgesRead_cfGKZtl()
wd <- "/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/pdf"
setwd(wd)

aprDate <- as.Date("2020-04-01")
julDate <- as.Date("2020-07-02")
augDate <- as.Date("2020-08-03")
sepDate <- as.Date("2020-09-14") # zero newConfirmed in Salzburg on 2020-09-13 !!! TODO correct !!!
octDate <- as.Date("2020-10-12")
novDate <- as.Date("2020-11-02")
maxDate <- max(df$Date)


# global axis labels
dblDays <- c(1:7,10,14,21,28,50,100,Inf,-100,-50,-28,-21,-14,-10,-7,-6,-5,-4,-3,-2,-1)
popBreaks <- c(0,1,2,5,10,15,20,25,seq(30,150,by=10))
popBreaksAll <- c(0,1,2,3,4,5,6,7,8,9,10,12,15,seq(20,150,by=10))
popLogBreaks <- (c(.1,.2,.5,1,2,5,10,20,50,100))
logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000))
logBreaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000),seq(10000,100000,by=10000))

# -------------------------------------------------------------------------------------------------
# rm7NewConfPop ~ Date | Region
# -------------------------------------------------------------------------------------------------
yLimMax <- 125

# construct dataframe for long term prediction
nModelDays=28
nPredDays=14
regions <- df %>% group_by(Region) %>% summarize(Date=first(Date)) %>% dplyr::ungroup() %>% dplyr::select(Region)
predRegions=rep(regions$Region,each=(nPredDays+nModelDays))
predDate=seq.Date(maxDate-days(nModelDays-1),maxDate+days(nPredDays),1)
predDates=rep(predDate,nrow(regions))
predDF <- data.frame(Date=predDates, Region=predRegions, stringsAsFactors=FALSE)
dplm <- df %>% 
  dplyr::filter(Date>maxDate-days(nModelDays)) %>%
  dplyr::select(Date, Region, rm7NewConfPop) %>%
  dplyr::full_join(predDF, by=c("Date","Region")) %>%
  dplyr::arrange(Region, Date) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(predNewConfPop=exp(predict(lm(log(rm7NewConfPop)~Date, na.action=na.exclude), newdata=data.frame(Date=predDate)))) %>%
  dplyr::ungroup()

# construct dataframe for short term prediction
nModelDaysST=7
nPredDaysST=7
regions <- df %>% group_by(Region) %>% summarize(Date=first(Date)) %>% dplyr::ungroup() %>% dplyr::select(Region)
predRegions=rep(regions$Region,each=nPredDaysST+nModelDaysST)
predDate=seq.Date(maxDate-days(nModelDaysST-1),maxDate+days(nPredDaysST),1)
predDates=rep(predDate,nrow(regions))
predDF <- data.frame(Date=predDates, Region=predRegions, stringsAsFactors=FALSE)
dplmST <- df %>% 
  dplyr::filter(Date>maxDate-days(nModelDaysST)) %>%
  dplyr::select(Date, Region, rm7NewConfPop) %>%
  dplyr::full_join(predDF, by=c("Date","Region")) %>%
  dplyr::arrange(Region, Date) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate(predNewConfPop=exp(predict(lm(log(rm7NewConfPop)~Date, na.action=na.exclude), newdata=data.frame(Date=predDate)))) %>%
  dplyr::ungroup()

dp <- df %>% dplyr::filter(Date>=julDate)

ggplot(data=dp, aes(x=Date, y=rm7NewConfPop, color=Region, shape=Region)) +
  theme(panel.grid.major = element_line(color = "darkgray", linetype=3), panel.grid.minor=element_line(color = "gray90", linetype=1)) +
  scale_shape_manual(values=c(1:10)) +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m", limits=c(novDate, maxDate+days(nPredDays+2)), expand=expand_scale(mult=0.01)) +
  scale_y_continuous(limits=c(1,yLimMax), breaks=popBreaksAll, position="right", expand=expand_scale(mult=0.01), trans="log10", name="Positive/100.000 Einwohnern. \nAmpelfarben entlang ECDC (European Centre for Disease Prevention and Control) ") + 
  geom_line(data=dp, aes(x=Date, y=1.4-0.025), size=2.0, color="green") +
  geom_line(data=dp, aes(x=Date, y=1.4+0.025), size=2.0, color="yellow") +
  geom_line(data=dp, aes(x=Date, y=4.3-.075), size=1.0, color="yellow") +
  geom_line(data=dp, aes(x=Date, y=4.3+.075), size=1.0, color="orange") +
  geom_line(data=dp, aes(x=Date, y=8.6-.15), size=1.0, color="orange") +
  geom_line(data=dp, aes(x=Date, y=8.6+.15), size=1.0, color="red") +
  geom_line(data=dp, aes(x=Date, y=17.1-.30), size=1.0, color="red") +
  geom_line(data=dp, aes(x=Date, y=17.1+.30), size=1.0, color="black") +
  geom_line(data=dp, aes(x=Date, y=100), size=2, color="black") +
  geom_line(data=dplm, aes(x=Date, y=predNewConfPop), size=.5, linetype=3) +
  geom_point(data=dplm %>% dplyr::filter(Date==maxDate+days(nPredDays)), aes(x=Date, y=predNewConfPop), size=3) +
  geom_line(data=dplmST, aes(x=Date, y=predNewConfPop), size=.25, linetype=1) +
  geom_point(data=dplmST %>% dplyr::filter(Date>maxDate), aes(x=Date, y=predNewConfPop), size=.5) +
  geom_line(data=dplmST %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=predNewConfPop), color="darkgreen", size=.5) +
  geom_line(data=dplmST %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=predNewConfPop), color="red", size=.5) +
  geom_point(data=dplmST %>% dplyr::filter(Date==maxDate+days(nPredDaysST)), aes(x=Date, y=predNewConfPop), size=3) +
  geom_point(size=2.5) + 
  geom_line() +
  geom_line(data=dp %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=rm7NewConfPop), color="darkgreen", size=1.5) +
  geom_line(data=dp %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=rm7NewConfPop), color="red", size=1.5) +
  geom_point(data=dp %>% dplyr::filter(Date==maxDate), size=4) + 
  ggtitle(paste0("COVID-19 Österreich, Wien und Bundesländer: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date)," mit Prognose bis ", max(dplm$Date), ".  Basisdaten: AGES", 
                 "\n\t\t  Lang+Kurzeit Prognose: 1. Kommende Woche aus letzter Woche. 2. Kommende zwei Wochen aus letzten vier Wochen.  Stand: ", maxDate))
ggsave(file=paste0("COVID-19-rm7NewConfPop_Date_Region-",min(dp$Date),"_",maxDate,".pdf"), dpi=300, width=12, height=8, scale=1.10)
ggsave(file=paste0("../COVID-19-Austria-newConfPop_Date_Region.png"), dpi=100, width=9, height=9, scale=1.4)

  #geom_point(data=dplm %>% dplyr::filter(Date>maxDate), aes(x=Date, y=predNewConfPop), size=.25) +
#geom_line(data=dplm %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=predNewConfPop), color="darkgreen", size=.5, linetype=2) +
#geom_line(data=dplm %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=predNewConfPop), color="red", size=.5, linetype=2) +

#geom_line(data=dplmw, aes(x=Date, y=predNewConfPop), color="red", size=.75, linetype=1, inherit.aes=FALSE) +
#geom_line(data=dp %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=max(Date-days(7))), aes(x=Date, y=newConfPop), color="darkgreen", size=.5, linetype=3) +
#geom_line(data=dp %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=max(Date-days(7))), aes(x=Date, y=newConfPop), color="red", size=.5, linetype=3) +


dp <- df %>% dplyr::filter(Date>=julDate) %>%dplyr::filter(Region=="Österreich")
ggplot(data=dp, aes(x=Date, y=modrm7NewConfPop))+
  scale_y_continuous(trans="log10",limits=c(0.8,100),breaks=c(seq(1:10),15,seq(10,100,by=10))) +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") + 
  geom_line(aes(x=Date,y=15), color="red", size=1)+
  geom_line(aes(x=Date,y=20), color="black", size=1)+
  geom_line(color="blue")+
  geom_point(aes(x=Date, y=rm7NewConfPop), color="blue") +
  ggtitle(paste0("COVID-19 Österreich: Positiv Getestete pro 100.000 Einw. seit ", min(dp$Date),".  Wochenmittel. Basisdaten: AGES",
                 "\n\tInzidenzFahrplan: Rückblick auf Q3+Q4 2020: LockDown bei mittlerer TagesInzidenz=15 am 10.Okt."))
ggsave(file=paste0("COVID-19-rm7NewConfPop_Date_Austria-",min(dp$Date),"_",maxDate,".pdf"), dpi=300, width=12, height=8, scale=1.10)
ggsave(file=paste0("../COVID-19-Austria-newConfPop_Date_Austria.png"), dpi=100, width=9, height=9, scale=1.4)


# -------------------------------------------------------------------------------------------------
# dt7rm7NewConfirmed ~ Date | Region
# -------------------------------------------------------------------------------------------------
# Plot parameters
xTrans="identity"
xLimMin <- 1
yLimMin <- 0.85
yLimMax <- 1.20


dp <- df %>% dplyr::filter(Date>=sepDate)
ggplot(data=dp, 
       aes(x=Date, y=dt7rm7NewConfirmed, color=Region, shape=Region)) +
  scale_shape_manual(values=c(1:10)) +
  #scale_x_date(limits=c(as.Date(strptime("2020-09-14",format="%Y-%m-%d")),NA), 
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m", name=paste0("Datum (Rohdaten bis ",maxDate, ", Weekly rolling mean bis ",max(dp$Date),")")) +
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=as.character(round((exp(log(2)/dblDays)-1)*100,1)), name="Tägliche Steigerungsrate [%]")) +
  geom_point(size=3) + 
  geom_line() +
  geom_line(data=dp %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=dt7rm7NewConfirmed), color="darkgreen", size=1) +
  geom_line(data=dp %>% dplyr::filter(Region=="Österreich"), aes(x=Date, y=dt7lprNewConfirmed), color="black", size=1, linetype=3) +
  geom_line(data=dp %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=dt7rm7NewConfirmed), color="red", size=1) +
  geom_line(data=dp %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=dt7lprNewConfirmed), color="black", size=1, linetype=3) +
#  geom_line(data=dp %>% dplyr::filter(Region=="Österreich") %>% dplyr::filter(Date>=max(Date)-days(7)), aes(x=Date, y=newConfirmed), color="darkgreen", size=.5, linetype=2) +
#  geom_line(data=dp %>% dplyr::filter(Region=="Wien") %>% dplyr::filter(Date>=max(Date)-days(7)), aes(x=Date, y=newConfirmed), color="red", size=.5, linetype=3) +
  geom_line(data=dp, aes(x=Date, y=1), color="blue", size=1, linetype=5) +
  ggtitle(paste0("AGES BundesLänder. Timeline. Wochenmittel der Ausbreitungsgeschwindigkeit.",
                 "\n [y-Achse: links: Zuwachs positive Getestete pro Tag in %, rechts: Anzahl der Tage bis Verdoppelung/Halbierung der Fallzahlen]"))
ggsave(file=paste0("COVID-19-dt7rm7NewConfirmed_Date_Region-",min(dp$Date),"_",maxDate,".pdf"), dpi=300, width=12, height=8, scale=1.00)


  # -------------------------------------------------------------------------------------------------
# rm7NewSpread ~ rm7NewConfPop | Region
# -------------------------------------------------------------------------------------------------
xLimMin <- 0
xLimMax <- 125
yLimMin <- 0.90
yLimMax <- 1.18

dp <- df %>% dplyr::filter(Date>=sepDate) # %>% dplyr::filter(Region=="Österreich")
ggplot(data=dp %>% dplyr::arrange(Region,Date), aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(xLimMin,xLimMax), breaks=popBreaks) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_path() + 
  # daily points
  geom_point(aes(shape=Region, size=1), show.legend=FALSE) +
  #geom_point(aes(shape=Region, size=1, stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  # starting point
  geom_point(data=dp %>% dplyr::filter(Date==min(Date)), 
             aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dp %>% dplyr::filter(Date==min(Date)), 
            aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  # weekly points
  #geom_point(data=dp %>% dplyr::filter(weekdays(Date)==weekdays(min(Date))), 
  #           aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed, color=Region, shape=Region), size=3, stroke=1.5, inherit.aes=FALSE) +
  # end point
  geom_point(data=dp %>% dplyr::filter(Date==max(Date)), 
             aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed, color=Region, shape=Region), size=4, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dp %>% dplyr::filter(Date==max(Date)), 
            aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed, label=Region), hjust="left", nudge_x=.7, size=4, color="gray30") +
  geom_line(data=dp, aes(x=rm7NewConfPop, y=1)) +
  # emphasize Wien an dÖsterreich paths
  geom_path(data=dp %>% dplyr::filter(Region=="Österreich"), aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed), color="darkgreen", size=1) +
  geom_path(data=dp %>% dplyr::filter(Region=="Wien"), aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed), color="red", size=1) +
  ggtitle(paste("AGES COVID-19. Österreich: WochenMittel der Entwicklung der Verbreitungsrate und Neuinfektionen von", 
                min(dp$Date),"bis", max(dp$Date))) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-dt7rm7NewConfirmed_rm7NewConfPop_Region-",min(dp$Date),"_",maxDate,".pdf"), dpi=300, width=12, height=8, scale=1.00)


# -------------------------------------------------------------------------------------------------
# dt7rm7NewConfirmed ~ Date | Region: since July
# -------------------------------------------------------------------------------------------------
xTrans="identity"
yLimMin <- 0.80
yLimMax <- 1.40

dp <- df %>% dplyr::filter(Date>=julDate)
ggplot(data=dp %>% dplyr::arrange(Region,Date), 
       aes(x=Date, y=dt7rm7NewConfirmed, color=Region, shape=Region, group=Region)) + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  scale_x_date(date_breaks="1 months", date_labels="%B") + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_line() + 
  geom_point(size=1.8) +
  geom_line(data=dp, aes(x=Date, y=1)) +
  geom_line(data=dp %>% dplyr::filter(Region=="Wien"), color="red", size=1.5) +
  geom_line(data=dp %>% dplyr::filter(Region=="Österreich"), color="black", size=.75) +
  ggtitle(paste("AGES COVID-19. Österreich: WochenMittel der Entwicklung der Verbreitungsrate von", 
                min(dp$Date),"bis", max(dp$Date))) +
  # xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-dt7rm7NewConfirmed_Date_Region-",min(dp$Date),"_",maxDate,".pdf"), dpi=300, width=12, height=8, scale=1.00)



# -------------------------------------------------------------------------------------------------
# dt7rm7NewConfirmed ~ rm7NewConfPop | Region: Complete Timeline
# -------------------------------------------------------------------------------------------------

xTrans="log10"
xLimMin <- .5
xLimMax <- 120
yLimMin <- 0.9
yLimMax <- 1.22
xBreaks=c(1,2,5,10,20,50,100)
popLogBreaks <- (c(1,2,5,10,20,50,100))

xLimMin <- .9
xLimMax <- 100
yLimMin <- 0.95
yLimMax <- 1.12

dp <- df %>% dplyr::filter(Date>=julDate) %>% dplyr::filter(Region=="Österreich"|Region=="Wien")
ggplot(data=dp %>% dplyr::arrange(Region,Date), 
       aes(x=rm7NewConfPop, y=dt7rm7NewConfirmed)) + 
  theme(panel.grid.major  = element_line(color = "darkgray", linetype=3)) +
  scale_x_continuous(limits=c(xLimMin,xLimMax), breaks=round(10^seq(0,2,by=.2),1), trans=xTrans) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_path(aes(color=Month)) + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Month, color=Month), size=3) +
  geom_line(data=dp, aes(x=rm7NewConfPop, y=1)) +
  facet_wrap(Region~., nrow=2)+
  ggtitle(paste("AGES COVID-19. Österreich: WochenMittel der Entwicklung der Verbreitungsrate und Neuinfektionen von", 
                min(dp$Date),"bis", max(dp$Date), "(",max(dp$Date)-days(3),")")) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-dt7rm7NewConfirmed_rm7NewConfPop_Region-",min(dp$Date),"_",maxDate,"-Facet.pdf"), dpi=300, width=12, height=8, scale=1.00)




# -------------------------------------------------------------------------------------------------
#  Approach: Wien
# -------------------------------------------------------------------------------------------------
dp <- df %>% 
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::mutate(resrm7NewConfirmed=ifelse(abs(resrm7NewConfPop)<=max(resrm7NewConfPop),resrm7NewConfPop,NA)) %>%
  dplyr::mutate(relresrm7NewConfirmed=ifelse(abs(resrm7NewConfPop/modrm7NewConfPop)<=max(resrm7NewConfPop/modrm7NewConfPop),resrm7NewConfPop/modrm7NewConfPop,NA)) %>%
  dplyr::mutate(lognewConfirmed=log10(rm7NewConfPop), dtLog10NewConfirmed=(dt7rm7NewConfPop-1)*100) %>%
  dplyr::select(Date, newConfirmed=rm7NewConfPop, log10NewConfirmed=lognewConfirmed, 
                diffAbsLinRegNewConfirmed=resrm7NewConfirmed, diffRelLinRegNewConfirmed=relresrm7NewConfirmed, 
                changeNewConfirmed=dtLog10NewConfirmed) %>%
  tidyr::gather(key=Status, val=Count, -Date, factor_key=TRUE)

dm <- df %>%
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::mutate(Status="log10NewConfirmed") %>% 
  dplyr::select(Date,Status,Count=modrm7NewConfPop)
de <- df %>%
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::mutate(expNewConfPop=(modrm7NewConfPop)) %>% 
  dplyr::mutate(Status="newConfirmed") %>% 
  dplyr::select(Date,Status,Count=expNewConfPop)

ggplot(data=dp, aes(x=Date, y=Count, color=Status, shape=Status)) +
  theme(panel.grid.major = element_line(color = "darkgray", linetype=3), panel.grid.minor=element_line(color = "gray90", linetype=1)) +
  scale_shape_manual(values=c(15,16,17,18,20,21)) +
  scale_x_date(date_breaks="1 months", date_labels="%b", limits=c(julDate,max(dp$Date)+days(0))) +
  scale_y_continuous(position="right") +
  geom_point(size=1.5, show.legend=FALSE) + geom_line(show.legend=FALSE) +
  geom_line(data=dp, aes(x=Date, y=0), color="darkgrey") +
  facet_wrap(.~Status, ncol=1, scales="free_y", as.table=FALSE) +
  geom_line(data=dm %>% dplyr::filter(Date<as.Date("2020-12-01")), aes(x=Date, y=log10(Count)), size=.5, show.legend=FALSE) +
  geom_line(data=de %>% dplyr::filter(Date<as.Date("2020-12-01")), aes(x=Date, y=Count), size=.5, show.legend=FALSE) +
  ggtitle(paste0("COVID-19 Wien: Exponentielles Wachstum tägl.Positive/100.000 seit ", min(dp$Date),".  Basisdaten: AGES", 
                 "\n vonu: 1. Positive, 2. log10(Positive) +linReg.  3. rel. und 4. abs. Diff von linReg, 5. Änderung [%/Tag].",
                 "\n -> 1.Stufe Aug, 2.Stufe Sep, 3.Stufe Oct/Nov. ->  x10 täglPositive in 13 Wochen"))
ggsave(file=paste0("COVID-19-x4Confirmed_Date_Vienna-",min(dp$Date),"_",maxDate,"-Facet.pdf"), dpi=300, width=8, height=12, scale=1.10)


# -------------------------------------------------------------------------------------------------
# Count data: Counts
# -------------------------------------------------------------------------------------------------
# Calc coefs of robust regression
dc <- df %>% 
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::select(ID, newTested=rm7NewTested, newConfPop=rm7NewConfPop, newDeaths=rm7NewDeaths, curConfirmed, curHospital, curICU) %>%
  tidyr::gather(key=Status, val=Count, -ID) %>%
  dplyr::group_by(Status) %>%
  dplyr::summarize(dt10=log(10)/(coef(MASS::rlm(log(Count+0.0001)~ID), na.action=na.ignore, singular.ok=TRUE)[2]),
                   dt2=log(2)/(coef(MASS::rlm(log(Count+0.0001)~ID), na.action=na.ignore, singular.ok=TRUE)[2])) %>%
  dplyr::ungroup()

dp <- df %>% 
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::select(Date, newTested=rm7NewTested, newConfPop=rm7NewConfPop, newDeaths=rm7NewDeaths, curConfirmed, curHospital, curICU) %>%
  tidyr::gather(key=Status, val=Count, -Date)

ggplot(data=dp, aes(x=Date, y=Count, color=Status, shape=Status)) +
  theme(panel.grid.major  = element_line(color = "darkgray", linetype=3)) +
  scale_shape_manual(values=c(15,16,17,18,20,21)) +
  scale_x_date(date_breaks="1 months", date_labels="%b") +
  scale_y_continuous(position="right", trans="log10", breaks=logBreaks) +
  geom_point(size=1.5, show.legend=FALSE) + geom_line(size=.25, show.legend=FALSE) +
  geom_smooth(method="lm", se=FALSE, linetype=1, size=.5, show.legend=FALSE)+
  facet_wrap(.~Status, ncol=3, scales="free_y", 
             # labeller=function(df){df$dt2=paste(" *2 time=",round(dc$dt2,2),"days"); df$dt10=paste("*10 time=",round(dc$dt10,2),"days");return(df)}) +
             labeller=function(df){df$dt=paste(paste0("mean growth rate:   *2 days=",round(dc$dt2,2),"   "), paste0("*10 days=",round(dc$dt10,2)));return(df)}) +
  ggtitle(paste0("COVID-19. Entwicklung in Wien: Mittleres exponentielles Wachstum seit ", min(dp$Date),".  Basisdaten: AGES", 
                 "\n\t\t  obere Reihe: Aktuelle Gesamtanzahl: Kranke, inSpital, aufIntensiv.  untere Reihe: tägl Neumeldungen: Positive/100.000, Tote, Getestete"))
ggsave(file=paste0("COVID-19-Counts_Date_Vienna-",min(dp$Date),"_",maxDate,"-Facet.pdf"), dpi=300, width=12, height=8, scale=1.00)


# -------------------------------------------------------------------------------------------------
# Count data: Residuals
# -------------------------------------------------------------------------------------------------
dr <- df %>% 
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::select(Date, resNewTested=resrm7NewTested, resNewConfPop=resrm7NewConfPop, resNewDeaths=resrm7NewDeaths, 
                resCurConfirmed=resrm7CurConfirmed, resCurHospital=resrm7CurHospital, resCurICU=resrm7CurICU) %>%
  tidyr::gather(key=Status, val=Count, -Date, factor_key=TRUE) %>%
  dplyr::group_by(Status) %>%
  dplyr::filter(abs(Count)<=max(Count))

ggplot(data=dr, aes(x=Date, y=Count, color=Status, shape=Status)) +
  theme(panel.grid.major  = element_line(color = "darkgray", linetype=3)) +
  scale_shape_manual(values=c(15,16,17,18,20,15)) +
  scale_x_date(date_breaks="1 months", date_labels="%b") +
  scale_y_continuous(position="right") +
  geom_point(size=1.5, show.legend=FALSE) + geom_line(show.legend=FALSE) +
  geom_line(aes(x=Date, y=0), size=.5, show.legend=FALSE) +
  facet_wrap(.~Status, nrow=2, ncol=3, scales="free_y", as.table=FALSE) +
  ggtitle(paste0("COVID-19. Entwicklung in Wien.  Abweichung vom mittleren exponentiellen Wachstum seit ", min(dr$Date), ".   Basisdaten: AGES.",
                 "\n\t\t  obere Reihe: Aktuelle Gesamtanzahl: Kranke, inSpital, aufIntensiv.  untere Reihe: tägl Neumeldungen: Getestete, Positive/100.000, Tote"))
ggsave(file=paste0("COVID-19-Residuals_Date_Vienna-",min(dr$Date),"_",maxDate,"-Facet.pdf"), dpi=300, width=12, height=8, scale=1.00)


              
# -------------------------------------------------------------------------------------------------
# Spread data: Wien
# -------------------------------------------------------------------------------------------------
dp <- df %>% 
  dplyr::filter(Date>=julDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::select(Date, dtNewTested=dt7rm7NewTested, dtNewConfPop=dt7rm7NewConfPop, dtNewDeaths=dt7rm7NewDeaths, 
                dtCurConfirmed=dt7rm7CurConfirmed, dtCurHospital=dt7rm7CurHospital, dtCurICU=dt7rm7CurICU) %>%
  tidyr::gather(key=Status, val=Count, -Date)

ggplot(data=dp %>% dplyr::filter(abs(Count-1)<.25), aes(x=Date, y=(Count-1)*100, color=Status, shape=Status)) +
  theme(panel.grid.major  = element_line(color = "darkgray", linetype=3)) +
  scale_shape_manual(values=c(15,16,17,18,20,21)) +
  scale_x_date(date_breaks="1 months", date_labels="%b") +
  scale_y_continuous(position="right") +
  geom_point(size=1.5, show.legend=FALSE) + geom_line(show.legend=FALSE) +
  geom_line(aes(x=Date, y=0), size=.5, show.legend=FALSE) +
  facet_wrap(.~Status, ncol=3, scales="free_y") +
  ggtitle(paste0("COVID-19. Entwicklung in Wien: Ausbreitungsgeschwindigkeit in % Steigerung/Rückgang pro Tag seit ", min(dp$Date),".  Basisdaten: AGES", 
                 "\n\t\t  aktuellKranke, inSpital, aufIntensiv.  täglPositive/100.000, täglTote, täglGetested"))
  ggsave(file=paste0("COVID-19-Spread_Date_Vienna-",min(dp$Date),"_",maxDate,"-Facet.pdf"), dpi=300, width=12, height=8, scale=1.00)



# -------------------------------------------------------------------------------------------------
# Spread data: Wien
# -------------------------------------------------------------------------------------------------
dp <- df %>% 
  dplyr::filter(Date>=augDate) %>%
  dplyr::filter(Region=="Wien") %>%
  dplyr::filter(dt7rm7NewDeaths<1.25) %>%
  dplyr::mutate(newConfirmed=log10(rm7NewConfPop), curHospital=log10(rm7CurHospital), newDeaths=log10(rm7NewDeaths), 
                dtNewConfirmed=(dt7rm7NewConfPop-1)*100, dtCurHospital=(dt7rm7CurHospital-1)*100, dtNewDeaths=(dt7rm7NewDeaths-1)*100) %>%
  dplyr::select(Date, newConfirmed, curHospital, newDeaths, dtNewConfirmed, dtCurHospital, dtNewDeaths) %>%
  tidyr::gather(key=Status, val=Count, -Date, factor_key=TRUE)

ggplot(data=dp, aes(x=Date, y=Count, color=Status, shape=Status)) +
  theme(panel.grid.major  = element_line(color = "darkgray", linetype=3)) +
  scale_shape_manual(values=c(15,16,17,18,20,21)) +
  scale_x_date(date_breaks="1 months", date_labels="%b") +
  scale_y_continuous(position="right") +
  geom_point(size=1.5, show.legend=FALSE) + geom_line(show.legend=FALSE) +
  #geom_smooth(method="lm", se=FALSE, size=.25) + 
  #geom_line(aes(x=Date, y=0), size=.5, show.legend=FALSE) +
  facet_wrap(.~Status, nrow=2, ncol=3, scales="free_y", as.table=TRUE) +
  ggtitle(paste0("COVID-19. Entwicklung in Wien: Exponentielles Wachstum und Ausbreitungsgeschwindigkeit seit ", min(dp$Date),".  Basisdaten: AGES", 
                 "\n\t\t  Reihen: log10(Anzahl), Änderung(% zum Vortag).   Spalten: tägPositive/100000, inSpital, täglTote"))
ggsave(file=paste0("COVID-19-CountsSpread_Date_Vienna-",min(dp$Date),"_",maxDate,"-Facet.pdf"), dpi=300, width=12, height=8, scale=1.00)


# -------------------------------------------------------------------------------------------------
# Dateil data: Wien
# -------------------------------------------------------------------------------------------------
dp <- df %>% 
  dplyr::filter(Region=="Niederösterreich") %>%
  dplyr::filter(Date>sepDate)

trans="log10"
trans="identity"

ggplot(data=dp, aes(x=Date, y=newConfPop)) +
  
  scale_x_date(date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(1,100), breaks=c(seq(1,10,by=1),seq(10,200,by=10)), trans=trans) + 
  
  # Confirmed
  geom_point(size=1, color="grey20") +  
  geom_line(color="grey20", linetype=1, size=.25) +
  geom_point(aes(x=Date, y=rm7NewConfPop), size=2, color="grey20") +
  geom_line(aes(x=Date, y=rm7NewConfPop), size=1.1, color="grey20") +
  #geom_line(aes(x=Date, y=smoothNewConfirmed), color="grey20", linetype=2) +
  
  # Confirmed/Tested
  geom_point(aes(x=Date, y=newConfTest*100), size=1, color="blue") +
  geom_line(aes(x=Date,  y=newConfTest*100), linetype=3, color="blue") +
  geom_point(aes(x=Date, y=rm7NewConfTest*100), size=1, color="blue") +
  geom_line(aes(x=Date, y=rm7NewConfTest*100), color="blue") +
  #geom_line(aes(x=Date, y=smoothŃewConfTest*10000), color="blue", linetype=2) +
  
  geom_point(aes(x=Date, y=newTested/1000), size=1, color="red") +
  geom_line(aes(x=Date,  y=newTested/1000), linetype=3, color="red") +
  geom_point(aes(x=Date, y=rm7NewTested/1000), size=1, color="red") +
  geom_line(aes(x=Date, y=rm7NewTested/1000), color="red") +
  #geom_line(aes(x=Date, y=smoothNewTested/10), color="red", linetype=2) +
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Wien")

ggplot(data=df %>% filter(Date<as.Date("2020-12-01"), rm7NewConfPop>7), aes(x=(rm7NewConfPop), y=(rm7NewConfTest)))+geom_point() +
  geom_smooth(method="lm")


