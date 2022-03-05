



# geom_line(data=df %>% dplyr::filter(Region=="Wien"), aes(x=Date, y=rm7NewConfTest/rm7NewConfirmed*1000), linetype=1, size=1, inherit.aes=FALSE) + 


# daily stats
ggplot(data=dfw, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.1,100), breaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10)), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000)))) +
  geom_point(size=2) + geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("AGES BundesLänder Timeline newConfirmed & newTested Daily: Wien")



ggplot(data=dfat, aes(x=Date, y=Count, color=Status, shape=Status)) +
  scale_x_date(limits=c(as.Date(strptime("2020-07-01",format="%Y-%m-%d")),NA), 
               date_breaks="1 weeks", date_labels="%a.%d.%m") +
  scale_y_continuous(limits=c(.05,50),  breaks=c(seq(.1,1,by=.1),seq(1,10,by=1),seq(10,100,by=10)), trans="log10",
                     sec.axis = sec_axis(~ . *scaled, breaks=c(seq(10,100,by=10),seq(100,1000,by=100),seq(1000,10000,by=1000), seq(10000,100000,by=10000)))) +
  geom_point(size=2) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("AGES BundesLänder Timeline newConfirmed/per100.000 perDay")


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













1.) Gehen die Fallzahlen nicht um zwei Stufen zurück, steht die nächste große Stufe unmittelbar bevor. Der aktuelle Lockdown muss dafür sorgen, dass die Fallzahlen auf den Stand von Anfang August (ca. 1 positive Testung pro 100.000 pro Tag) zurück geht. Dann sollten bei üblichen Verhaltensregeln sowie einsetzender Wirkung der Impfung drei bis vier Monate ohne weiteren Lockdown möglich sein (bis ca. 10 positive Testungen pro 100.000 pro Tag) 
7.) Eine Prognose auf Basis der Entwicklung seit Ende November sieht dieses Ziel aktuell nicht im Bereich des Möglichen.
8.) Es bleibt zu hoffen, dass die weiteren Auswirkungen des harten Lockdown trotz schlechter Akzeptanz in der Bevölkerung ausreichen das Ziel 'Grün' lt. ECDC Ampelschema zu erreichen (Inzidenz ca. 1 pro 100.000 pro Tag)

Schlussfolgerungen:
  1.) Es wäre wichtig, dass der ORF diese Zusammenhänge darstellt und die aktuellen Zahlen entsprechend einordnet. Es muss zu jedem Zeitpunkt klar erkennbar sein, wie die aktuelle Situation im eigenen Bundesland ist, wohin sie sich entwickelt, und wie sie lt. Plan sein soll (z.B. Inzidenz 2 im Feber, 4 im März, 8 im April, 16 im Mai, dann HardLockdown 3 Wochen)
2.) Die Angabe von absoluten Zahlen ohne Referenz zu einer Skala (z.B. Ampelschema der ECDC) und ohne Trend sind wenig hilfreich. Eine Darstellung z.B. analog eines einfachen Wetterberichtes  (z.B. Windstärke mit Wind/Sturm/Orkan, 'Böen','auffrischendem' und 'abflauendem' Wind, etc.) pro Bundesland könnte technische Daten in anschaulicher Weise vermitteln. 
3.) Eine einfache Prognose aus den vergangenen Daten und Vergleich mit den Zielvorgaben sollte nicht fehlen (auch ohne Modellierung von Massnahmen sinnvoll)


Erläuterungen zu den graphischen Darstellungen im Anhang:
  1.) Das epidemiologische Geschehen verläuft bei konstanten Massnahmen exponentiell.
2.) Dieser Wirkungsweise wird erst in logarithmischer Darstellung der Fallzahlen sichtbar. Die vom ORF gewählte lineare Darstellung ist nicht geeignet das epidemiologische Geschehen adäquat zu beschreiben.
3.) Die Abweichungen vom mittleren Wachstum sind Wachstumsschübe von einigen/wenigen Wochen, die besser als Stufen denn als Wellen bezeichnet werden müssen (nach einer Welle geht es wieder hinunter)



Damit nicht ab Ende Jänner eine ähnlich große Stufe stattfindet, muss d

Die gewählte Art der graphischen Darstellung macht es schwierig den Entwicklungsstand der Epidemie anhand der aktuellen Zahlen sachgerecht einzuordnen. Das liegt zum einen daran, dass grundlegende Prinzipien der Ausbreitung von COVID-19 nicht vermittelt werden, zum anderen daran, dass Ziele und Prognosen nicht formuliert werden.




# old page
csvF <- "./html/COVID-19-austria.bmsgpk.20210404-1942.html"
csv <- xml2::read_html(csvF)
csv %>% html_nodes(xpath=".//*")
csv %>% html_nodes("table")
csv %>%  html_table()

# new page
url <- "https://info.gesundheitsministerium.gv.at/?re=tabelle"
rt <- xml2::read_html(url)
rt %>% html_nodes(xpath=".//*")
rt %>% html_nodes(xpath=".//table[@class='table-faelle']")
rt %>% html_nodes("table.table-faelle")
rt %>% html_nodes(xpath="//html/body/table[3]")


t <- x %>% html_nodes("table.table-faelle")
 x %>% html_table()




html_nodes(rt, xpath="html/body/table[1]")

url %>% read_html() %>% html_nodes(xpath=".//*")

url %>% read_html()  %>% html_nodes("body") %>% extract2(1) 
url %>% read_html()  %>% html_nodes("body") %>% extract2(1) %>% html_nodes(xpath=".//*")

url %>% read_html()   %>% html_table()



html_table(rt, header=FALSE, dec=".", fill=TRUE)
str(rt)





lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- lego_movie %>%
  html_nodes("#titleCast .primary_photo img") %>%
  html_attr("alt")
cast

