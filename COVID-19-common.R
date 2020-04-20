library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tibbletime)
library(scales)
options(error = function() traceback(2))


# -------------------------------------------------------------------------------------------------------------
covRegionPlot <- function(dr, Regions="World", cutOffDate=as.POSIXct("2020-02-22"), 
                          Population=10e6, bPlot=TRUE, nRegDays=5, nEstDays=10, nCutOff=2, baseDir=".",
                          ggMinDate=as.POSIXct("2020-02-24"), ggMaxDate=max(dr$Stamp)+days(10), filePrefix="") {
# -------------------------------------------------------------------------------------------------------------
  
  # rolling regression
  rolm <-    rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(coef   (lm(log(vals)~Stamp))[2])*24*3600))}, window=nRegDays)
  rolmcil <- rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,1])*24*3600))}, window=nRegDays)
  rolmciu <- rollify(.f = function(Stamp, vals) {1/log10(exp(as.numeric(confint(lm(log(vals)~Stamp), level=.9)[2,2])*24*3600))}, window=nRegDays)

  # Calculate MTD = MinTimetoDeath as difference of Confirmed and Death count exponential increase start times
  # put Time Axis on y so we get time origine as intercept
  dfc <- dr %>% 
    filter(Stamp>=cutOffDate) %>% 
    dplyr::filter(Confirmed>0)
  rlmC <- lmrob(dfc$Stamp~log(dfc$Confirmed),tol=1e-5)
  
  dfd <- dr %>% dplyr::filter(Deaths>nCutOff)
  if (nrow(dfd)>=nRegDays) {
    rlmD <- lmrob(dfd$Stamp~log(dfd$Deaths),tol=1e-5)
    MTD <- as.POSIXct(coef(rlmD)[1], origin='1970-01-01') - as.POSIXct(coef(rlmC)[1], origin='1970-01-01')
    if (MTD>30) MTD <- NA
  } else {
    rlmD <- NA
    MTD <- NA
  }
  
  # calculate rolling estimate of spead of increase of confirmed cases 
  dfcr <- dfc %>% 
    dplyr::mutate(rolmConfirmed    = rolm(Stamp,Confirmed)) %>%
    dplyr::mutate(rolmConfirmedCIl = rolmcil(Stamp,Confirmed)) %>%
    dplyr::mutate(rolmConfirmedCIu = rolmciu(Stamp,Confirmed))
  # Calculate 2 points with different window
  dfcLast <- tail(dfcr$rolmConfirmed,1)
  c <- dfc %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+nEstDays),Stamp<=max(Stamp)-days(nRegDays))
  dfcPrev <-  1/log10(exp(as.numeric(coef(lm(log(Confirmed)~Stamp, data=c))[2])*24*3600))
  f <- dfc %>% dplyr::filter(Stamp<min(Stamp)+days(nEstDays))
  dfcFrst <-  1/log10(exp(as.numeric(coef(lm(log(Confirmed)~Stamp, data=f))[2])*24*3600))
  
  
  # calculate rolling estimate of spead of increase of fatalities
  dfdLast <- NA
  dfdr <- NA
  if (nrow(dfd)>=nRegDays) {
    dfdr <- dfd %>% 
      dplyr::mutate(rolmDeaths = rolm(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIl = rolmcil(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIu = rolmciu(Stamp,Deaths))
      dfdLast <- tail(dfdr$rolmDeaths,1)
  } 
  
  # calculate rolling estimate of spead of increase of Hospitalized
  dfh <- dr %>% dplyr::filter(Hospitalized>nCutOff)
  dfhLast <- NA
  if (nrow(dfh)>=nRegDays) {
    dfhr <- dfh %>% 
      dplyr::mutate(rolmHospitalized = rolm(Stamp,Hospitalized))
    dfhLast <- tail(dfhr$rolmHospitalized,1)
  }
  
  # calculate rolling estimate of spead of increase of Hospitalized
  dfi <- dr %>% dplyr::filter(IntenseCare>nCutOff)
  dfiLast <- NA
  if (nrow(dfi)>=nRegDays) {
    dfir <- dfi %>% 
      dplyr::mutate(rolmIntenseCare = rolm(Stamp,IntenseCare))
    dfiLast <- tail(dfir$rolmIntenseCare,1)
  }
  
  # Calculate first period with different window size
  dfdFrst <- NA
  if(nrow(dfd)>=nEstDays) {  
    e <- dfd %>% dplyr::filter(Stamp<min(Stamp)+days(nEstDays))
    dfdFrst <-  1/log10(exp(as.numeric(coef(lm(log(Deaths)~Stamp, data=e))[2])*24*3600))
  } 
  
  # Calculate prev period with different window size
  dfdPrev <- NA
  if(nrow(dfd)>=(nRegDays+nEstDays)) {  
    d <- dfd %>% dplyr::filter(Stamp>max(Stamp)-days(nRegDays+nEstDays),Stamp<=max(Stamp)-days(nRegDays))
    dfdPrev <-  1/log10(exp(as.numeric(coef(lm(log(Deaths)~Stamp, data=d))[2])*24*3600))
  } 

  
  # gather Confirmed, Deaths into Status
  if (Regions == "Oesterreich" || Regions == "AT") {
    dfg <- dfc %>% 
      tidyr::gather(key=Status, value=Count, Confirmed, Recovered, Deaths, Hospitalized, IntenseCare, Tested) %>%
      dplyr::filter(Count>nCutOff) %>%
      dplyr::mutate(Count=log10(Count))
  } else {
    dfg <- dfc %>% 
      tidyr::gather(key=Status, value=Count, Confirmed, Recovered, Deaths, Hospitalized, IntenseCare) %>%
      dplyr::filter(Count>nCutOff) %>%
      dplyr::mutate(Count=log10(Count))
  }

  
      
  #c <- data.frame(round(max(dfw$sumConfirmed)/(Population*1e6)*1000000), max(dfw$sumConfirmed), max(dfw$sumDeaths), round(MTD,1), 
  #                as.POSIXct(as.character(expBeginC, format="%Y-%m-%d")),round(1/log10(cRate),1),round(1/log10(cRateMin),1),round(1/log10(cRateMax),1),round(1/log10(cRateFst),1),round(1/log10(cRateLst),1),
  #                round(1/log10(cRateCur),1),
  #                as.POSIXct(as.character(expBeginD, format="%Y-%m-%d")),round(1/log10(dRate),1),round(1/log10(dRateMin),1),round(1/log10(dRateMax),1),round(1/log10(dRateFst),1),round(1/log10(dRateLst),1)
  #)
  
  allGrid=c(1,2,3,4,5,6,7,8,9,10)
  majGrid=c(10)
  minGrid=c(1,2,3,4,5,6,7,8,9)
  yLogTicsLabels <-     as.character(c(allGrid*1e0,allGrid*1e1,allGrid*1e2,allGrid*1e3,allGrid*1e4,allGrid*1e5,allGrid*1e6))
  yLogTicsLabelsMajor <-as.character(c(majGrid*1e0,majGrid*1e1,majGrid*1e2,majGrid*1e3,majGrid*1e4,majGrid*1e5,majGrid*1e6))
  yLogTicsBreaksAll <-         log10(c(allGrid*1e0,allGrid*1e1,allGrid*1e2,allGrid*1e3,allGrid*1e4,allGrid*1e5,allGrid*1e6))
  yLogTicsBreaksMajor <-       log10(c(majGrid*1e0,majGrid*1e1,majGrid*1e2,majGrid*1e3,majGrid*1e4,majGrid*1e5,majGrid*1e6))
  yLogTicsBreaksMinor <-       log10(c(minGrid*1e0,minGrid*1e1,minGrid*1e2,minGrid*1e3,minGrid*1e4,minGrid*1e5,minGrid*1e6))
  
  
  scale10 <- 50
  if (bPlot) {
    print(paste0("saveImage: ./covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"))
    gg <- ggplot(data=dfg, aes(x=Stamp, y=Count, color=Status, shape=Status)) +
      geom_point(size=2.5) +  geom_line(linetype=1, size=.5)  +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype="Last4Days"), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays+nEstDays)),Stamp<=(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype="Prev7Days"), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp<(min(Stamp)+days(nEstDays)))}, 
                  method=lm, aes(color=Status, linetype="Zero7Days"), fullrange=TRUE, se=FALSE, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/scale10), inherit.aes=FALSE, size=1.5, color="darkgrey") +
      geom_point(data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/scale10), inherit.aes=FALSE, size=1.5) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIl/scale10), inherit.aes=FALSE, linetype=3, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIu/scale10), inherit.aes=FALSE, linetype=3, size=.25) +
      geom_line( data=dfdr, mapping=aes(x=Stamp, y=rolmDeaths/scale10), inherit.aes=FALSE, size=1.5, color="orange") +
      geom_point(data=dfdr, mapping=aes(x=Stamp, y=rolmDeaths/scale10), inherit.aes=FALSE, size=1.5, color="darkred") +
      scale_x_datetime(date_breaks="1 week", date_minor_breaks="1 day", labels=date_format("%d.%m"), limits=c(ggMinDate,ggMaxDate)) +
      scale_y_continuous(limits=c(0,6), labels=yLogTicsLabelsMajor, breaks=yLogTicsBreaksMajor, minor_breaks=yLogTicsBreaksMinor,
                         sec.axis = sec_axis(~ . *scale10, breaks=seq(0,500,by=10),name=paste0("Days to *10 of Confirmed(grey) and Deaths(orange) \n",nRegDays," days rolling regression [90% confInterval]"))) +
      xlab(paste0("Confirmed*10-Frst", nEstDays,"Days=",round(dfcFrst,1), "d  Deaths*10-Frst",nEstDays,"Days=",round(dfdFrst,1), "d\n",
                  "Confirmed*10-Prev", nEstDays,"Days=",round(dfcPrev,1), "d  Deaths*10-Prev",nEstDays,"Days=",round(dfdPrev,1), "d\n", 
                  "Confirmed*10-Last", nRegDays,"Days=",round(dfcLast,1), "d  Deaths*10-Last",nRegDays,"Days=",round(dfdLast,1), "d\n",
                  "Hospitalized*10-Last", nRegDays,"Days=",round(dfhLast,1), "d  IntenseCare*10-Last",nRegDays,"Days=",round(dfiLast,1), "d")) +
      ggtitle(paste0("Region=",paste(Regions,collapse="-"), " Date=",format(max(dfg$Stamp,na.rm=TRUE),"%Y-%m-%d %Hh"), 
                     "  Population=",round(Population/1e6,1), "Mio Confirmed=",round(max(dfc$Confirmed,na.rm=TRUE)/Population*1e6),"ppm",
                     "  Tested=",max(dfc$Tested,na.rm=TRUE),       "  Confirmed=",max(dfc$Confirmed,na.rm=TRUE),
                     "  Recovered=",max(dfc$Recovered,na.rm=TRUE), "  Deaths=",max(dfc$Deaths,na.rm=TRUE)))
    print(gg)
    
    ggsave(gg, filename=paste0(baseDir,"/thumbs/covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"),
           width=200, height=150, units="mm", scale=1.4 ,dpi=100)
    ggsave(gg, filename=paste0(baseDir,"/plots/covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"),
           width=350, height=200, units="mm", scale=1 ,dpi=150)
  }
  
  return(0)
}


BL <- data.frame(ID=c("AT","B","K","Noe","Ooe","Szbg","Stmk","T","V","W"),
                 ISO=c("AT","AT-1","AT-2","AT-3","AT-4","AT-5","AT-6","AT-7","AT-8","AT-9"),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)

# Dataframe with NUTSx structure and Bezirke
obr <- matrix(byrow=TRUE, ncol=7,
              data=c(
                c("Österreich","AT","Burgenland","AT11","AT112","Eisenstadt_Stadt","Eisenstadt_Stadt"),
                c("Österreich","AT","Burgenland","AT11","AT112","Eisenstadt-Umgebung","Eisenstadt_Umgebung"),
                c("Österreich","AT","Burgenland","AT11","AT113","Güssing","Guessing"),
                c("Österreich","AT","Burgenland","AT11","AT113","Jennersdorf","Jennersdorf"),
                c("Österreich","AT","Burgenland","AT11","AT113","Mattersburg","Mattersburg"),
                c("Österreich","AT","Burgenland","AT11","AT112","Neusiedl_am_See","Neusiedl_See"),
                c("Österreich","AT","Burgenland","AT11","AT111","Oberpullendorf","Oberpullendorf"),
                c("Österreich","AT","Burgenland","AT11","AT113","Oberwart","Oberwart"),
                c("Österreich","AT","Kärnten","AT21","AT212","Feldkirchen","Feldkirchen"),
                c("Österreich","AT","Kärnten","AT21","AT212","Hermagor","Hermagor"),
                c("Österreich","AT","Kärnten","AT21","AT211","Klagenfurt_Land","Klagenfurt_Land"),
                c("Österreich","AT","Kärnten","AT21","AT211","Klagenfurt_Stadt","Klagenfurt_Stadt"),
                c("Österreich","AT","Kärnten","AT21","AT213","Sankt_Veit_an_der_Glan","St_Veit_Glan"),
                c("Österreich","AT","Kärnten","AT21","AT212","Spittal_an_der_Drau","Spittal_Drau"),
                c("Österreich","AT","Kärnten","AT21","AT211","Villach_Land","Villach_Land"),
                c("Österreich","AT","Kärnten","AT21","AT211","Villach_Stadt","Villach_Stadt"),
                c("Österreich","AT","Kärnten","AT21","AT213","Völkermarkt","Voelkermarkt"),
                c("Österreich","AT","Kärnten","AT21","AT213","Wolfsberg","Wolfsberg"),
                c("Österreich","AT","Niederösterreich","AT12","AT121","Amstetten","Amstetten"),
                c("Österreich","AT","Niederösterreich","AT12","AT122","Baden","Baden"),
                c("Österreich","AT","Niederösterreich","AT12","AT127","Bruck_an_der_Leitha","Bruck_Leitha"),
                c("Österreich","AT","Niederösterreich","AT12","AT125","Gänserndorf","Gaenserndorf"),
                c("Österreich","AT","Niederösterreich","AT12","AT124","Gmünd","Gmünd"),
                c("Österreich","AT","Niederösterreich","AT12","AT125","Hollabrunn","Hollabrunn"),
                c("Österreich","AT","Niederösterreich","AT12","AT124","Horn","Horn"),
                c("Österreich","AT","Niederösterreich","AT12","AT126","Korneuburg","Korneuburg"),
                c("Österreich","AT","Niederösterreich","AT12","AT124","Krems_an_der_Donau_Stadt","Krems_Stadt"),
                c("Österreich","AT","Niederösterreich","AT12","AT124","Krems_Land","Krems_Land"),
                c("Österreich","AT","Niederösterreich","AT12","AT122","Lilienfeld","Lilienfeld"),
                c("Österreich","AT","Niederösterreich","AT12","AT121","Melk","Melk"),
                c("Österreich","AT","Niederösterreich","AT12","AT125","Mistelbach","Mistelbach"),
                c("Österreich","AT","Niederösterreich","AT12","AT127","Mödling","Moedling"),
                c("Österreich","AT","Niederösterreich","AT12","AT122","Neunkirchen","Neunkirchen"),
                c("Österreich","AT","Niederösterreich","AT12","AT123","Sankt_Pölten_Land","St_Poelten_Land"),
                c("Österreich","AT","Niederösterreich","AT12","AT123","Sankt_Pölten_Stadt","St_Poelten_Stadt"),
                c("Österreich","AT","Niederösterreich","AT12","AT121","Scheibbs","Scheibbs"),
                c("Österreich","AT","Niederösterreich","AT12","AT126","Tulln","Tulln"),
                c("Österreich","AT","Niederösterreich","AT12","AT124","Waidhofen_an_der_Thaya","Waidhofen_Thaya"),
                c("Österreich","AT","Niederösterreich","AT12","AT121","Waidhofen_an_der_Ybbs_Stadt","Waidhofen_Ybbs"),
                c("Österreich","AT","Niederösterreich","AT12","AT122","Wiener_Neustadt_Land","Wiener_Neustadt_Land"),
                c("Österreich","AT","Niederösterreich","AT12","AT122","Wiener_Neustadt_Stadt","Wiener_Neustadt_Stadt"),
                c("Österreich","AT","Niederösterreich","AT12","AT124","Zwettl","Zwettl"),
                c("Österreich","AT","Oberösterreich","AT31","AT311","Braunau_am_Inn","Braunau_Inn"),
                c("Österreich","AT","Oberösterreich","AT31","AT312","Eferding","Eferding"),
                c("Österreich","AT","Oberösterreich","AT31","AT313","Freistadt","Freistadt"),
                c("Österreich","AT","Oberösterreich","AT31","AT315","Gmunden","Gmunden"),
                c("Österreich","AT","Oberösterreich","AT31","AT311","Grieskirchen","Grieskirchen"),
                c("Österreich","AT","Oberösterreich","AT31","AT314","Kirchdorf_an_der_Krems","Kirchdorf_Krems"),
                c("Österreich","AT","Oberösterreich","AT31","AT312","Linz-Land","Linz_Land"),
                c("Österreich","AT","Oberösterreich","AT31","AT312","Linz_Stadt","Linz_Stadt"),
                c("Österreich","AT","Oberösterreich","AT31","AT313","Perg","Perg"),
                c("Österreich","AT","Oberösterreich","AT31","AT311","Ried_im_Innkreis","Ried_Innkreis"),
                c("Österreich","AT","Oberösterreich","AT31","AT313","Rohrbach","Rohrbach"),
                c("Österreich","AT","Oberösterreich","AT31","AT311","Schärding","Schaerding"),
                c("Österreich","AT","Oberösterreich","AT31","AT314","Steyr-Land","Steyr_Land"),
                c("Österreich","AT","Oberösterreich","AT31","AT314","Steyr_Stadt","Steyr_Stadt"),
                c("Österreich","AT","Oberösterreich","AT31","AT312","Urfahr-Umgebung","Urfahr_Umgebung"),
                c("Österreich","AT","Oberösterreich","AT31","AT315","Vöcklabruck","Voecklabruck"),
                c("Österreich","AT","Oberösterreich","AT31","AT312","Wels-Land","Wels_Land"),
                c("Österreich","AT","Oberösterreich","AT31","AT312","Wels_Stadt","Wels_Stadt"),
                c("Österreich","AT","Salzburg","AT32","AT323","Hallein","Hallein"),
                c("Österreich","AT","Salzburg","AT32","AT323","Salzburg_Stadt","Salzburg_Stadt"),
                c("Österreich","AT","Salzburg","AT32","AT323","Salzburg-Umgebung","Salzburg_Umgebung"),
                c("Österreich","AT","Salzburg","AT32","AT322","Sankt_Johann_im_Pongau","St_Johann_Pongau"),
                c("Österreich","AT","Salzburg","AT32","AT321","Tamsweg","Tamsweg"),
                c("Österreich","AT","Salzburg","AT32","AT322","Zell_am_See","Zell_See"),
                c("Österreich","AT","Steiermark","AT22","AT223","Bruck-Mürzzuschlag","Bruck_Muerzzuschlag"),
                c("Österreich","AT","Steiermark","AT22","AT225","Deutschlandsberg","Deutschlandsberg"),
                c("Österreich","AT","Steiermark","AT22","AT221","Graz_Stadt","Graz_Stadt"),
                c("Österreich","AT","Steiermark","AT22","AT221","Graz-Umgebung","Graz_Umgebung"),
                c("Österreich","AT","Steiermark","AT22","AT222","Gröbming","Groebming"),
                c("Österreich","AT","Steiermark","AT22","AT224","Hartberg-Fürstenfeld","Hartberg_Fuerstenfeld"),
                c("Österreich","AT","Steiermark","AT22","AT225","Leibnitz","Leibnitz"),
                c("Österreich","AT","Steiermark","AT22","AT223","Leoben","Leoben"),
                c("Österreich","AT","Steiermark","AT22","AT222","Liezen","Liezen"),
                c("Österreich","AT","Steiermark","AT22","AT226","Murau","Murau"),
                c("Österreich","AT","Steiermark","AT22","AT226","Murtal","Murtal"),
                c("Österreich","AT","Steiermark","AT22","AT224","Südoststeiermark","Suedoststeiermark"),
                c("Österreich","AT","Steiermark","AT22","AT225","Voitsberg","Voitsberg"),
                c("Österreich","AT","Steiermark","AT22","AT224","Weiz","Weiz"),
                c("Österreich","AT","Tirol","AT33","AT334","Imst","Imst"),
                c("Österreich","AT","Tirol","AT33","AT332","Innsbruck-Land","Innsbruck_Land"),
                c("Österreich","AT","Tirol","AT33","AT332","Innsbruck-Stadt","Innsbruck_Stadt"),
                c("Österreich","AT","Tirol","AT33","AT335","Kitzbühel","Kitzbuehel"),
                c("Österreich","AT","Tirol","AT33","AT335","Kufstein","Kufstein"),
                c("Österreich","AT","Tirol","AT33","AT334","Landeck","Landeck"),
                c("Österreich","AT","Tirol","AT33","AT333","Lienz","Lienz"),
                c("Österreich","AT","Tirol","AT33","AT331","Reutte","Reutte"),
                c("Österreich","AT","Tirol","AT33","AT335","Schwaz","Schwaz"),
                c("Österreich","AT","Vorarlberg","AT34","AT341","Bludenz","Bludenz"),
                c("Österreich","AT","Vorarlberg","AT34","AT341","Bregenz","Bregenz"),
                c("Österreich","AT","Vorarlberg","AT34","AT342","Dornbirn","Dornbirn"),
                c("Österreich","AT","Vorarlberg","AT34","AT342","Feldkirch","Feldkirch"),
                c("Österreich","AT","Wien","AT13","AT130","Wien_Stadt","Wien")))

OBR <- data.frame(obr, stringsAsFactors=FALSE)
colnames(OBR) <- c("Land","NUTS0","Bundesland","NUTS2","NUTS3","Bezirk","county")


