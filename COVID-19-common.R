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
                          ggMinDate=as.POSIXct("2020-02-15"), ggMaxDate=max(dr$Stamp)+days(7), filePrefix="") {
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
  if (nrow(dfd)>=nRegDays) {
    dfdr <- dfd %>% 
      dplyr::mutate(rolmDeaths = rolm(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIl = rolmcil(Stamp,Deaths)) %>%
      dplyr::mutate(rolmDeathsCIu = rolmciu(Stamp,Deaths))
      dfdLast <- tail(dfdr$rolmDeaths,1)
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
  dfg <- dfc %>% 
    tidyr::gather(key=Status, value=Count, Confirmed, Recovered, Deaths) %>%
    dplyr::filter(Count>nCutOff) %>%
    dplyr::mutate(Count=log10(Count))

    
  #c <- data.frame(round(max(dfw$sumConfirmed)/(Population*1e6)*1000000), max(dfw$sumConfirmed), max(dfw$sumDeaths), round(MTD,1), 
  #                as.POSIXct(as.character(expBeginC, format="%Y-%m-%d")),round(1/log10(cRate),1),round(1/log10(cRateMin),1),round(1/log10(cRateMax),1),round(1/log10(cRateFst),1),round(1/log10(cRateLst),1),
  #                round(1/log10(cRateCur),1),
  #                as.POSIXct(as.character(expBeginD, format="%Y-%m-%d")),round(1/log10(dRate),1),round(1/log10(dRateMin),1),round(1/log10(dRateMax),1),round(1/log10(dRateFst),1),round(1/log10(dRateLst),1)
  #)
  
  if (bPlot) {
    print(paste0("saveImage: ./covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"))
    gg <- ggplot(data=dfg, aes(x=Stamp, y=Count, color=Status, shape=Status)) +
      geom_point(size=2.5) +  geom_line(linetype=1, size=.5)  +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype=paste0("Last",nRegDays,"Days")), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp>(max(Stamp)-days(nRegDays+nEstDays)),Stamp<=(max(Stamp)-days(nRegDays)))}, 
                  method=lm, aes(color=Status, linetype=paste0("Prev",nEstDays,"Days")), fullrange=TRUE, se=FALSE, size=.25) +
      stat_smooth(data=function(x) {x %>% dplyr::filter(Stamp<(min(Stamp)+days(nEstDays)))}, 
                  method=lm, aes(color=Status, linetype=paste0("Zero",nEstDays,"Days")), fullrange=TRUE, se=FALSE, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/5), inherit.aes=FALSE, size=1, color="darkgrey") +
      geom_point(data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmed/5), inherit.aes=FALSE, size=1) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIl/5), inherit.aes=FALSE, linetype=3, size=.25) +
      geom_line( data=dfcr, mapping=aes(x=Stamp, y=rolmConfirmedCIu/5), inherit.aes=FALSE, linetype=3, size=.25) +
      scale_y_continuous(limits=c(0,5), sec.axis = sec_axis(~ . *5, name=paste0("numDays to *10 Confirmed. ",nRegDays," days rolling regression [90% confInterval]"))) +
      xlim(ggMinDate,ggMaxDate) + 
      xlab(paste0("Confirmed*10-Frst", nEstDays,"Days=",round(dfcFrst,1), "d  Deaths*10-Frst",nEstDays,"Days=",round(dfdFrst,1), "d\n",
                  "Confirmed*10-Prev", nEstDays,"Days=",round(dfcPrev,1), "d  Deaths*10-Prev",nEstDays,"Days=",round(dfdPrev,1), "d\n", 
                  "Confirmed*10-Last", nRegDays,"Days=",round(dfcLast,1), "d  Deaths*10-Last",nRegDays,"Days=",round(dfdLast,1), "d")) +
      ggtitle(paste0("Region=",paste(Regions,collapse="-"), " Date=",max(dfg$Stamp,na.rm=TRUE), 
                     "  Population=",round(Population/1e6,1), "Mio Confirmed=",round(max(dfc$Confirmed,na.rm=TRUE)/Population*1e6),"ppm",
                     "  Confirmed=",max(dfc$Confirmed,na.rm=TRUE),"  Recovered=",max(dfc$Recovered,na.rm=TRUE), " Deaths=",max(dfc$Deaths,na.rm=TRUE),
                     "  minTimeToDeath=",round(MTD,1), "d"))
    print(gg)
    
    ggsave(filename=paste0(baseDir,"/thumbs/covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"),
           width=200, height=150, units="mm", scale=1.4 ,dpi=100)
    ggsave(filename=paste0(baseDir,"/plots/covid.",filePrefix,".",paste(Regions,collapse="-"),".",format(max(dfc$Stamp),"%Y-%m-%d"),".png"),
           width=350, height=200, units="mm", scale=1 ,dpi=150)
  }
  
  return(0)
}



BL <- data.frame(ID=c("AT","B","K","Noe","Ooe","Szbg","Stmk","T","V","W"),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)

# Dataframe with NUTSx structure and Bezirke
obr <- matrix(byrow=TRUE, ncol=3,
              data=c(
                c("Österreich","Burgenland","Eisenstadt_Stadt"),
                c("Österreich","Burgenland","Eisenstadt-Umgebung"),
                c("Österreich","Burgenland","Güssing"),
                c("Österreich","Burgenland","Jennersdorf"),
                c("Österreich","Burgenland","Mattersburg"),
                c("Österreich","Burgenland","Neusiedl_am_See"),
                c("Österreich","Burgenland","Oberpullendorf"),
                c("Österreich","Burgenland","Oberwart"),
                c("Österreich","Kärnten","Feldkirchen"),
                c("Österreich","Kärnten","Hermagor"),
                c("Österreich","Kärnten","Klagenfurt_Land"),
                c("Österreich","Kärnten","Klagenfurt_Stadt"),
                c("Österreich","Kärnten","Sankt_Veit_an_der_Glan"),
                c("Österreich","Kärnten","Spittal_an_der_Drau"),
                c("Österreich","Kärnten","Villach_Land"),
                c("Österreich","Kärnten","Villach_Stadt"),
                c("Österreich","Kärnten","Völkermarkt"),
                c("Österreich","Kärnten","Wolfsberg"),
                c("Österreich","Niederösterreich","Amstetten"),
                c("Österreich","Niederösterreich","Baden"),
                c("Österreich","Niederösterreich","Bruck_an_der_Leitha"),
                c("Österreich","Niederösterreich","Gänserndorf"),
                c("Österreich","Niederösterreich","Gmünd"),
                c("Österreich","Niederösterreich","Hollabrunn"),
                c("Österreich","Niederösterreich","Horn"),
                c("Österreich","Niederösterreich","Korneuburg"),
                c("Österreich","Niederösterreich","Krems_an_der_Donau_Stadt"),
                c("Österreich","Niederösterreich","Krems_Land"),
                c("Österreich","Niederösterreich","Lilienfeld"),
                c("Österreich","Niederösterreich","Melk"),
                c("Österreich","Niederösterreich","Mistelbach"),
                c("Österreich","Niederösterreich","Mödling"),
                c("Österreich","Niederösterreich","Neunkirchen"),
                c("Österreich","Niederösterreich","Sankt_Pölten_Land"),
                c("Österreich","Niederösterreich","Sankt_Pölten_Stadt"),
                c("Österreich","Niederösterreich","Scheibbs"),
                c("Österreich","Niederösterreich","Tulln"),
                c("Österreich","Niederösterreich","Waidhofen_an_der_Thaya"),
                c("Österreich","Niederösterreich","Waidhofen_an_der_Ybbs_Stadt"),
                c("Österreich","Niederösterreich","Wiener_Neustadt_Land"),
                c("Österreich","Niederösterreich","Wiener_Neustadt_Stadt"),
                c("Österreich","Niederösterreich","Zwettl"),
                c("Österreich","Oberösterreich","Braunau_am_Inn"),
                c("Österreich","Oberösterreich","Eferding"),
                c("Österreich","Oberösterreich","Freistadt"),
                c("Österreich","Oberösterreich","Gmunden"),
                c("Österreich","Oberösterreich","Grieskirchen"),
                c("Österreich","Oberösterreich","Kirchdorf_an_der_Krems"),
                c("Österreich","Oberösterreich","Linz-Land"),
                c("Österreich","Oberösterreich","Linz_Stadt"),
                c("Österreich","Oberösterreich","Perg"),
                c("Österreich","Oberösterreich","Ried_im_Innkreis"),
                c("Österreich","Oberösterreich","Rohrbach"),
                c("Österreich","Oberösterreich","Schärding"),
                c("Österreich","Oberösterreich","Steyr-Land"),
                c("Österreich","Oberösterreich","Steyr_Stadt"),
                c("Österreich","Oberösterreich","Urfahr-Umgebung"),
                c("Österreich","Oberösterreich","Vöcklabruck"),
                c("Österreich","Oberösterreich","Wels-Land"),
                c("Österreich","Oberösterreich","Wels_Stadt"),
                c("Österreich","Salzburg","Hallein"),
                c("Österreich","Salzburg","Salzburg_Stadt"),
                c("Österreich","Salzburg","Salzburg-Umgebung"),
                c("Österreich","Salzburg","Sankt_Johann_im_Pongau"),
                c("Österreich","Salzburg","Tamsweg"),
                c("Österreich","Salzburg","Zell_am_See"),
                c("Österreich","Steiermark","Bruck-Mürzzuschlag"),
                c("Österreich","Steiermark","Deutschlandsberg"),
                c("Österreich","Steiermark","Graz_Stadt"),
                c("Österreich","Steiermark","Graz-Umgebung"),
                c("Österreich","Steiermark","Gröbming"),
                c("Österreich","Steiermark","Hartberg-Fürstenfeld"),
                c("Österreich","Steiermark","Leibnitz"),
                c("Österreich","Steiermark","Leoben"),
                c("Österreich","Steiermark","Liezen"),
                c("Österreich","Steiermark","Murau"),
                c("Österreich","Steiermark","Murtal"),
                c("Österreich","Steiermark","Südoststeiermark"),
                c("Österreich","Steiermark","Voitsberg"),
                c("Österreich","Steiermark","Weiz"),
                c("Österreich","Tirol","Imst"),
                c("Österreich","Tirol","Innsbruck-Land"),
                c("Österreich","Tirol","Innsbruck-Stadt"),
                c("Österreich","Tirol","Kitzbühel"),
                c("Österreich","Tirol","Kufstein"),
                c("Österreich","Tirol","Landeck"),
                c("Österreich","Tirol","Lienz"),
                c("Österreich","Tirol","Reutte"),
                c("Österreich","Tirol","Schwaz"),
                c("Österreich","Vorarlberg","Bludenz"),
                c("Österreich","Vorarlberg","Bregenz"),
                c("Österreich","Vorarlberg","Dornbirn"),
                c("Österreich","Vorarlberg","Feldkirch"),
                c("Österreich","Wien","Wien_Stadt")))

OBR <- data.frame(OBR, stringsAsFactors=FALSE)
colnames(OBR) <- c("country","state","county")


