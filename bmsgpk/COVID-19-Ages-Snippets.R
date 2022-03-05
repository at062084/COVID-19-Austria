# COVID-19-Ages-Snippets

# -------------------------------------------------------------------------------------------------
# Vienna Tested and Confirmed
# -------------------------------------------------------------------------------------------------

dsw <- dss %>% dplyr::filter(Region=="Wien")
dsa <- dss %>% dplyr::filter(Region=="Österreich")

ggplot(data=dsw, aes(x=Date, y=newConfirmed)) +
  
  scale_x_date(date_breaks="1 weeks", date_labels="%a.%d.%m") +
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





# -------------------------------------------------------------------------------------------------
# lm(newRelConfirmed ~ newConfPop | Region)
# -------------------------------------------------------------------------------------------------
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
# library(MASS)
scaled=100
dfw <- df %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfTested=newConfTest*100, newTested=newTested/100, newConfPop=newConfPop) %>%
  dplyr::select(Date, relConfTested, newConfPop) %>% 
  tidyr::gather(key=Status, val=Count, relConfTested, newConfPop)
dfrmw <- ds %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(relConfTested=rm7NewConfTest*100, curHospital=rm7CurHospital/100, newDeaths=rm7NewDeaths, newConfPop=rm7NewConfPop) %>%
  dplyr::select(Date, relConfTested, newConfPop, curHospital, newDeaths) %>% 
  tidyr::gather(key=Status, val=Count, relConfTested, newConfPop, curHospital, newDeaths)

# Weekly moving average 1+(rm7NewSpread-1)*10
ggplot(data=dfrmw, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(.09,100), breaks=c(seq(.1,1,by=.1),seq(1,10,by=1),12,15,seq(10,100,by=10)), trans="log10", position="right",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(seq(10,100,by=10),seq(100,1000,by=100)))) +
  geom_point(size=2.5) + geom_line() +
  geom_smooth(method="lm", se=FALSE, linetype=2, size=.5) +
  geom_point(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status), size=1.5) + 
  geom_line(data=dfw, aes(x=Date, y=Count, color=Status), linetype=3, size=.75) + 
  # geom_line(data=dfrmw, aes(x=Date, y=1), linetype=2) +
  ggtitle("AGES BundesLänder: Wien. Entwicklung seit 2020-07-01. WochenMittel.     Skala links: imSpital     Skala rechts: neuPositive/100.000, neuSterbefälle, ProzentPositive [%]")
ggsave(file=paste0("COVID-19-rm7ConfTest-W-20202-07-01_",max(dsw$Date),".pdf"), dpi=300, width=4, height=3, scale=4)



#dfw <- df %>% dplyr::filter(Region=="Wien") %>% 
#  dplyr::mutate(relConfTested=newConfTest*100, newTested=newTested/100, newConfPop=newConfPop) %>%
#  dplyr::select(Date, relConfTested, newTested, newConfPop) %>% 
#  tidyr::gather(key=Status, val=Count, relConfTested, newTested, newConfPop)

dfrsw <- ds %>% dplyr::filter(Region=="Wien") %>% 
  dplyr::mutate(newConfPop=rm7NewConfPop, 
                confirmedChangeRate=rm7NewSpread, hospChangeRate=rm7HosSpread, deathChangeRate=rm7DeathSpread) %>%
  dplyr::select(Date, confirmedChangeRate, hospChangeRate, deathChangeRate) %>% 
  tidyr::gather(key=Status, val=Count, confirmedChangeRate, hospChangeRate, deathChangeRate)



# Weekly moving average 1+(rm7NewSpread-1)*10
ggplot(data=dfrsw %>% dplyr::filter(abs(1-Count)<.25), aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(breaks=c(seq(.75,1.25,by=.01)), position="right") +
  geom_point(size=3) + geom_line() +
  geom_line(data=dfrsw, aes(x=Date, y=1), linetype=2) +
  facet_grid(Status~., scales="free") +
  ggtitle("AGES BundesLänder: Wien. Entwicklung seit 2020-07-01. Ausbreitungsgeschwindigkeit WochenMittel.     NeuePositive [Änderung in %], NeueSterbeFälle [Änderung in %], AnzahlImSpital [Änderung %]")
ggsave(file=paste0("COVID-19-rm7NewSpread-W-20202-07-01_",max(dsw$Date),".pdf"), dpi=300, width=4, height=3, scale=4)




# -------------------------------------------------------------------------------------------------------------------
# Austria plots
# -------------------------------------------------------------------------------------------------------------------
dfat <- ds %>% dplyr::filter(Region=="Österreich") %>% 
  dplyr::mutate(relConfirmedTested=newConfTest*100, newTested=newTested/1000, newConfirmed=newConfirmed/1000, spreadSpeed=1+(rm7NewSpread-1)*10) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed, spreadSpeed) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed, spreadSpeed)
dfrmat <- ds %>% dplyr::filter(Region=="Österreich") %>% 
  dplyr::mutate(relConfirmedTested=rm7NewConfTest*100, newTested=rm7NewTested/1000, newConfirmed=rm7NewConfirmed/1000, spreadSpeed=1+(rm7NewSpread-1)*10) %>%
  dplyr::select(Date, relConfirmedTested, newTested, newConfirmed, spreadSpeed) %>% 
  tidyr::gather(key=Status, val=Count, relConfirmedTested, newTested, newConfirmed, spreadSpeed)

ggplot(data=dfrmat, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(.05,50),  breaks=c(seq(1,10,by=1),12,15,seq(10,30,by=10)), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000), seq(10000,100000,by=10000)))) +
  geom_point(size=2) + 
  geom_line() +
  geom_smooth(data=dfrmat %>% dplyr::filter(Status!="spreadSpeed"), method="lm", se=FALSE) +
  geom_point(data=dfat, aes(x=Date, y=Count, color=Status, shape=Status), size=1) + 
  geom_line(data=dfrmat, aes(x=Date, y=1)) +
  geom_line(data=dfat, aes(x=Date, y=Count, color=Status), linetype=3, size=.5) + 
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested WeekMeans: Österreich")
ggsave(file=paste0("COVID-19-rm7ConfTest-AT-20202-07-01_",max(dsw$Date),".pdf"), dpi=300, width=4, height=3, scale=4)



# -------------------------------------------------------------------------------------------------
# smoothNewSpread ~ smoothNewConfPop | Region
# -------------------------------------------------------------------------------------------------
ggplot(data=dso %>% dplyr::arrange(Region,Date), 
       aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, alpha=Date)) + 
  scale_x_continuous(limits=c(xLimMin,120), breaks=xBreaks, trans=xTrans) + 
  scale_y_continuous(limits=c(yLimMin,yLimMax), breaks=exp(log(2)/dblDays), labels=dblDays, position="right",
                     sec.axis=dup_axis(labels=round((round(exp(log(2)/dblDays),2)-1)*100), name="Tägliche Steigerungsrate [%]")) +
  geom_path() + 
  scale_shape_manual(values=c(21:25,7,9,10,12,13,14)) +
  geom_point(aes(shape=Region, size=as.numeric(Date), stroke=as.numeric(Date)/40000), show.legend=FALSE) +
  geom_path(data=dso %>% dplyr::filter(Region=="Österreich"), aes(x=smoothNewConfPop, y=smoothNewSpread), color="darkgreen", size=1) +
  geom_path(data=dso %>% dplyr::filter(Region=="Wien"), aes(x=smoothNewConfPop, y=smoothNewSpread), color="red", size=1) +
  geom_point(data=dso %>% dplyr::filter(weekdays(Date)==weekdays(min(Date))), 
             aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, shape=Region), size=5, stroke=1.5) +
  geom_text(data=dso %>% dplyr::filter(Date==max(Date)), 
            aes(x=smoothNewConfPop, y=smoothNewSpread, label=Region), hjust="left", nudge_x=.7, size=5, color="gray30") +
  geom_point(data=dso %>% dplyr::filter(Date==min(Date)), 
             aes(x=smoothNewConfPop, y=smoothNewSpread, color=Region, shape=Region), size=2, stroke=1.5, inherit.aes=FALSE) +
  geom_text(data=dso %>% dplyr::filter(Date==min(Date)), 
            aes(x=smoothNewConfPop, y=smoothNewSpread, label=Region), hjust="right", nudge_x=-.7, size=4, color="gray30", inherit.aes=FALSE) +
  geom_line(data=dso, aes(x=rm7NewConfPop, y=1)) +
  ggtitle(paste("AGES COVID-19. Österreich: Geglättete Entwicklung der Verbreitungsrate und Neuinfektionen von", 
                min(dso$Date),"bis", max(dso$Date))) +
  xlab("Aktuelle Situation: Neuinfektionen [Anzahl pro 100.000 Einwohner, gemittelt über die jeweils letzte Woche]") +
  ylab("Voraussichtliche Entwicklung: Tage bis zur Verdoppelung der täglichen Neuinfektionen")
ggsave(file=paste0("COVID-19-PopSpreadSmooth-",max(dso$Date),"-120-2.pdf"), dpi=300, width=4, height=3, scale=4)


# -------------------------------------------------------------------------------------------------
# lm(newRelConfirmed ~ newConfPop | Region)
# -------------------------------------------------------------------------------------------------
dk <- df %>% 
  dplyr::filter(!is.na(rm7NewConfPop)) %>%
  dplyr::group_by(Region) %>% dplyr::filter(rm7NewConfPop>=10) 

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


# output from caAgesRead_cfGKZtl
df %>% 
  dplyr::filter(newConfPop>0.001, Date>as.Date("2020-10-05") & Date<as.Date("2020-11-09")) %>% 
  ggplot(aes(x=Date, y=newConfPop, color=Region)) +
  geom_line(size=.125) + 
  scale_y_continuous(trans="log10") + 
  theme(legend.position = "none") + 
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m")



# -------------------------------------------------------------------------------------------------
# maps  
# -------------------------------------------------------------------------------------------------
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(geojsonsf)
library(spdplyr)
library(RColorBrewer)
library(maps)
library(leafem)

camConfPop <- c(0,1,1.4,2,2.8,4,5.6,8,11,16,22,32,45,64,90,128,Inf)
levConfPop <- round(c(0,10^seq(0,2,by=0.2),1000),1)
palConfPop <- c("#FFFFFF", brewer.pal(9,"YlOrRd"), "#081D58", "#000000")
binConfPop <- colorBin(palette=palConfPop, domain=gj2$Inzidenz, bins=levConfPop)

icon <- awesomeIcons(
  icon = 'bolt',
  library = "fa",
  markerColor = "gray",
  iconColor = 'red',
  squareMarker=TRUE,
  iconRotate=0
)

iconsDirection <- paste0("direction-",c("n","ne","e","se","s"))
iconsWeather <- c("clear","sun-rise","cloudy-mostly","rain","overcast-heavy-thunder")
iconsTest <- c("home","glass","home","heart","fire")
iconsColor <- c("darkred", "red","green","lightblue","blue")
iconMarkers <- awesomeIcons(icon = iconsTest[gj2$Trend], 
                            library = "glyphicon",
                            markerColor = "white",
                            iconColor = iconsColor[gj2$Trend], 
                            spin = FALSE,
                            extraClasses = NULL, 
                            squareMarker = TRUE, 
                            iconRotate = 0,
                            fontFamily = "monospace", 
                            text = NULL)
iconsWeatherFA <- c("sun","cloud-sun","cloud","cloud-rain","bolt")
iconMarkers <- awesomeIcons(icon = iconsWeatherFA[gj2$Trend], 
                            library = "fa",
                            markerColor = "white",
                            iconColor = iconsColor[gj2$Trend], 
                            spin = FALSE,
                            extraClasses = NULL, 
                            squareMarker = TRUE, 
                            iconRotate = 0,
                            fontFamily = "monospace", 
                            text = NULL)

iconSizeWeather=50
iconsWeather <- iconList (
  Thunder =   makeIcon(iconUrl="../images/iconWeather-Thunder.png",   iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Rain =      makeIcon(iconUrl="../images/iconWeather-Rain.png",      iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun_Rain =  makeIcon(iconUrl="../images/iconWeather-Sun-Rain.png",  iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun_Cloud = makeIcon(iconUrl="../images/iconWeather-Sun-Cloud.png", iconWidth=iconSizeWeather, iconHeight=iconSizeWeather),
  Sun =       makeIcon(iconUrl="../images/iconWeather-Sun.png",       iconWidth=iconSizeWeather, iconHeight=iconSizeWeather)
)

iconSizeDirection=80
iconsDirection <- iconList (
  dirN =  makeIcon(iconUrl="../images/iconDirection-N.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirNNE = makeIcon(iconUrl="../images/iconDirection-NNE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirENE = makeIcon(iconUrl="../images/iconDirection-ENE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirE =  makeIcon(iconUrl="../images/iconDirection-E.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirSSE = makeIcon(iconUrl="../images/iconDirection-ESE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirESE = makeIcon(iconUrl="../images/iconDirection-SSE.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection),
  dirS =  makeIcon(iconUrl="../images/iconDirection-S.png", iconWidth=iconSizeDirection, iconHeight=iconSizeDirection)
)



gj1="/home/at001335/DataEngineering/COVID-19/COVID-19-Austria/Maps/archive/nuts_rg_60m_2013_lvl_1.geojson"
gj2="/home/at001335/DataEngineering/COVID-19/COVID-19-Austria/Maps/archive/nuts_rg_60m_2013_lvl_2.geojson"
gj3="/home/at001335/DataEngineering/COVID-19/COVID-19-Austria/Maps/archive/nuts_rg_60m_2013_lvl_3.geojson"

gj1=geojsonio::geojson_read(x=gj1, what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
gj2=geojsonio::geojson_read(x=gj2, what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))
gj3=geojsonio::geojson_read(x=gj3, what="sp") %>% dplyr::filter(startsWith(NUTS_ID,"AT"))

NUTS2_AT <- c("Burgenland","Niederösterreich","Wien","Kärnten", "Steiermark","Oberösterreich","Salzburg","Tirol","Vorarlberg")

cX = mean(bbox(gj1)[1,])
cY = mean(bbox(gj1)[2,])

cx <- vector()
cy <- vector()
for(i in 1:length(gj2)) {
  cx[i] <- mean(bbox(gj2[i,])[1,])
  cy[i] <- mean(bbox(gj2[i,])[2,])
}

gj2 <- gj2 %>% 
  dplyr::mutate(Region=factor(NUTS2_AT), Inzidenz=levConfPop[1:9], cx=cx, cy=cy, Trend=c(4,1:7,4), ForeCast=c(1:5,4:1))

letPalette <- colorFactor(palConfPop, gj2$Region)

labConfPop <- sprintf(
  "<strong>%s</strong><br/>TagesInzidenz: %g pro 100000<br>Änderung letze Woche: %g%%<br>Prognose nächste Woche: %g<br>Tage bis LockDown: %g",
  gj2$Region, round(gj2$Inzidenz,2), round(gj2$SHAPE_LEN,2), round(gj2$SHAPE_AREA,2), round(gj2$cx,0)
) %>% lapply(htmltools::HTML)


leaflet(gj2) %>%
  addTiles(group="DefaultMap") %>%
  setView(lng=cX, lat=cY, zoom = 8) %>%
  addPolygons(data=gj1, stroke = TRUE, smoothFactor = 0, color="black", fillOpacity = 0, fillColor="None", weight=10, group="AT1") %>%
  addPolygons(data=gj3, stroke = TRUE, smoothFactor = 0, fillOpacity = 0, fillColor="none", weight=1, group="AT3") %>%
  addPolygons(stroke = TRUE, weight=3, color="black",
              #fill=TRUE, fillOpacity = 1, fillColor=~letPalette(Region),
              fill=TRUE, fillOpacity = 1, fillColor=~binConfPop(Inzidenz),
              #label=~paste(NUTS_ID,Region), 
              label=labConfPop, 
              labelOptions = labelOptions(style=list("font-weight"="normal", padding="3px 8px"),textsize="15px"),
              popup=~paste(NUTS_ID,round(SHAPE_AREA,2),round(SHAPE_LEN,2),Inzidenz,sep="\n\r<br>"),
              #highlightOptions(weight=5, color="#666", dashArray="", fillOpacity=0.7),
              group="AT2") %>%
#  addCircleMarkers(lng=~cx, lat=~cy, radius=~Inzidenz, 
#                   fill=TRUE, fillColor="blue", fillOpacity=1,
#                   stroke=TRUE, weight=10, color="green",  
#                   label=~paste(NUTS_ID,Region), 
#                   popup=~paste(NUTS_ID,SHAPE_AREA,SHAPE_LEN,Inzidenz,sep="<br>"), 
#                   layerId=~NUTS_ID, group="Markers")  %>%
#  addAwesomeMarkers(lng=~cx, lat=~cy, icon=~iconMakesWeather[Trend], group="Awesome") %>%
  addMarkers(lng=~cx, lat=~cy+.125, icon=~iconsWeather[ForeCast], group="ForeCast") %>%
  addMarkers(lng=~cx, lat=~cy-.125, icon=~iconsDirection[Trend], group="Trend") %>%
  #addMarkers(lng=~cx, lat=~cy, group="Awesome") %>%
  addLegend(pal=binConfPop, values=~Inzidenz, position="topleft") %>%
  addLayersControl(#baseGroups=c("Markers","Awesome"),
                  overlayGroups=c("AT1","AT3"),
                  options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("AT1","AT3","Markers"))




# custom palette for 
countries$category <- factor(sample.int(5L, nrow(countries), TRUE))
factpal <- colorFactor(topo.colors(5), countries$category)
leaflet(countries) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~factpal(category))


ds %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE, opacity=1)) %>%
  setView(lng = lonGrazCenter, lat = latGrazCenter, zoom = 14) %>%
  # addGraticule(group = "Graticule", interval = 0.01, style = list(color = "blue", weight = 1)) %>%
  # addLayersControl(overlayGroups = c("Graticule"), options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(lat=~lat, lng=~lon, radius=2, color = ~deviceIDpal(deviceID), group = ~deviceID,
                   popup =paste("Station: ", ds$deviceID, "<br>",
                                "Sensor: ", ds$sensorID, "<br>",
                                "Messwert: ", ds$sensorVal, "<br>",
                                "Uhrzeit: ", ds$timeLocal),
                   label = ds$deviceID) %>%
  addLayersControl(overlayGroups = deviceIDs, options = layersControlOptions(collapsed = FALSE))



geoJson <- readr::read_file(
  "https://rawgit.com/benbalter/dc-maps/master/maps/ward-2012.geojson"
)

leaflet() %>%
  addTiles() %>%
  setView(-77.0369, 38.9072, 11) %>%
  addBootstrapDependency() %>%
  enableMeasurePath() %>%
  addGeoJSONChoropleth(
    geoJson,
    valueProperty = "AREASQMI",
    scale = c("white", "red"),
    mode = "q",
    steps = 4,
    padding = c(0.2, 0),
    labelProperty = "NAME",
    popupProperty = propstoHTMLTable(
      props = c("NAME", "AREASQMI", "REP_NAME", "WEB_URL", "REP_PHONE", "REP_EMAIL", "REP_OFFICE"),
      table.attrs = list(class = "table table-striped table-bordered"),
      drop.na = TRUE
    ),
    color = "#ffffff", weight = 1, fillOpacity = 0.7,
highlightOptions = highlightOptions(
  weight = 2, color = "#000000",
  fillOpacity = 1, opacity = 1,
  bringToFront = TRUE, sendToBack = TRUE),
pathOptions = pathOptions(
  showMeasurements = TRUE,
  measurementOptions = measurePathOptions(imperial = TRUE)))




leaflet() %>%
  addBootstrapDependency() %>%
  setView(-98.583333, 39.833333, 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addKMLChoropleth(
    kml,
    valueProperty = JS(
      'function(feature){
         var props = feature.properties;
         var aland = props.ALAND/100000;
         var awater = props.AWATER/100000;
         return 100*awater/(awater+aland);
      }'
    ),
    scale = "OrRd", mode = "q", steps = 5,
    padding = c(0.2, 0),
    popupProperty = "description",
    labelProperty = "NAME",
    color = "#ffffff", weight = 1, fillOpacity = 1,
    highlightOptions = highlightOptions(
      fillOpacity = 1, weight = 2, opacity = 1, color = "#000000",
      bringToFront = TRUE, sendToBack = TRUE
    ),
    legendOptions = legendOptions(
      title = "% of Water Area",
      numberFormatOptions = list(
        style = "decimal",
        maximumFractionDigits = 2
      )
    )
  )




