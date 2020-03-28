# COVID-19 data provided by Austrian bmsgpk (Gesundheitsminiterium)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(xml2)
library(rvest)

options(error = function() traceback(2))

# set work dir here
setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")

# do some logging
logFile <- "./html/COVID-19.extract.log"
logMsg <- function(msg) {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}

logMsg(paste("Running COVID-19-bmsgpk-extract.R"))


# Bundesländer in Österreich
BL <- data.frame(ID=colnames(df[3:12]),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)


# --------------------------------------------------------------------------------------------------------
# Extract data from www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html
# --------------------------------------------------------------------------------------------------------

# read current data file
csvFile <- paste0("./data/COVID-19-austria.csv")
df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp))
nRows <- nrow(df)

# get html page from bmsgpk
ts <- format(now(),"%Y%m%d-%H%M")
bmsgpkFile <- paste0("./html/COVID-19-austria.bmsgpk.",ts,".html")
logMsg(paste("Dumping bmsgpk page to", bmsgpkFile))
cmd <- paste("\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\"", "-O", bmsgpkFile)
system2("wget", cmd)

logMsg(paste("Parsing dump of bmsgpk page in", bmsgpkFile))
html <- read_file(bmsgpkFile)

# Identifiers of sections  in html response of wget 
atTested <- "<p><strong>Bisher durchgef&uuml;hrte Testungen in &Ouml;sterreich"
atConfirmed <- "<p><strong>Best&auml;tigte F&auml;lle, </strong><strong>Stand"
atRecovered <- "<p><strong>Genesene Personen,</strong> <strong>Stand "
atDeaths <- "<p><strong>Todesf&auml;lle</strong>, <strong>Stand"
closeAll <- "</p>"

# Definition of Bundesländer for localized data
Bundeslaender <- data.frame(Name=c("Burgenland","K&auml;rnten","Nieder&ouml;sterreich","Ober&ouml;sterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                            Letter=c("B","K","Noe","Ooe","Szbg","Stmk","T","V","W"), stringsAsFactors=FALSE)

# add four rows for data from website to current data
tR=nRows+1
cR=nRows+2
rR=nRows+3
dR=nRows+4

# Stamp
s <- str_extract(html, paste0(atConfirmed,".*",closeAll))
t <- stringr::str_match(s,paste0("<strong>Stand ","([\\d\\s\\.,:]*)"," Uhr:</strong>"))[2]
Stamp <- as.POSIXct(t, format="%d.%m.%Y, %H:%M", tz="CET")
logMsg(paste("Running extraction for", Stamp, "from", bmsgpkFile))

# Extract number of Tested
s <- str_extract(html, paste0(atTested,".*",closeAll))
t <- str_match(s,paste0("</strong>","([\\d\\.]*)","</p>"))[,2]
totTested <- as.integer(str_remove(t,"\\."))
df[tR,"Stamp"] <- Stamp 
df[tR,"Status"] <- "Tested"
df[tR,"AT"] <- totTested
logMsg(paste("Tested",totTested))

# Extract number of Confirmed cases
logMsg("Confirmed")
s <- str_extract(html, paste0(atConfirmed,".*",closeAll))
df[cR,"Stamp"] <- Stamp 
df[cR,"Status"] <- "Confirmed"
if(!is.na(s)) {
  nAT <- as.integer(str_remove(str_match(s,paste0("Uhr:</strong> ","([\\d\\.]*)"," F&auml;lle,"))[2],"\\."))
  df[cR,"AT"] <- nAT
  for (bl in Bundeslaender$Name) {
    n <- as.integer(str_remove(str_match(s,paste0(bl," \\(","([\\d\\.]*)","\\)"))[2],"\\."))
    df[cR,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
  }
  logMsg(paste("Confirmed",nAT))
} else {
  logMsg("WARN: No record for Confirmed found")
}

# Extract number of Recovered cases
s <- str_extract(html, paste0(atRecovered,".*",closeAll))
df[rR,"Stamp"] <- Stamp 
df[rR,"Status"] <- "Recovered"
if (!is.na(s)) {
  nAT <- as.integer(str_remove(str_match(s,paste0(":</strong> (","([\\d\\.]*)","\\)"))[2],"\\."))
  df[rR,"AT"] <- nAT
  for (bl in Bundeslaender$Name) {
    n <- as.integer(str_remove(str_match(s,paste0(bl," \\(","([\\d\\.]*)","\\)"))[2],"\\."))
    if (is.na(n)) n <- 0
    df[rR,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
  }
  logMsg(paste("Recovered",nAT))
} else {
  logMsg("WARN: No record for Recovered found")
}

# Extract number of Deaths
s <- str_extract(html, paste0(atDeaths,".*",closeAll))
df[dR,"Stamp"] <- Stamp 
df[dR,"Status"] <- "Deaths"
sAT <- str_match(s,paste0("Uhr:</strong> ","([\\d\\.]*)",", nach"))[2]
if (!is.na(sAT)) {
  nAT <- as.integer(str_remove(sAT,"\\."))
  df[dR,"AT"] <- nAT
  for (bl in Bundeslaender$Name) {
    n <- n <- as.integer(str_remove(str_match(s,paste0(bl," \\(","([\\d\\.]*)","\\)"))[2],"\\."))
    if (is.na(n)) n <- 0
    df[dR,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
  }
  logMsg(paste("Deaths",nAT))
} else {
  logMsg("WARN: No record for Deaths found")
}

# Print to console
df %>% tail() %>% print()

# Persist data
write.csv(df, csvFile, row.names=FALSE, quote=FALSE)

quit(save="no", status=0)


scrapeInfo <- function() {

  # --------------------------------------------------------------------------------------------------------
  # Some more Details from info.gesundheitsministerium.at
  # --------------------------------------------------------------------------------------------------------
  # crawl and dump javascripted site using chrome headless
  dmpFile=paste0("./html/COVID-19-austria.detail.",ts,".dmp")
  logMsg(paste("Creating dump of info page using headless chrome into", dmpFile))
  
  chrome="/opt/google/chrome/chrome"
  url="https://info.gesundheitsministerium.at/"
  flags="--headless --disable-gpu --dump-dom"
  system2(chrome,paste(url, flags, ">", dmpFile))
  
  # read previous info data from disk
  csvFile <- paste0("./data/COVID-19-austria.info.csv")
  di <- read.csv(csvFile, stringsAsFactors=FALSE) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp))
  
  # extract AT data from above section
  rs <- c(tR,cR,rR,dR)
  dr <- df[rs,-c(4:12)] %>% dplyr::mutate(Region="AT",Count=AT) %>% dplyr::select(Stamp,Region,Status,Count)
  
  # append AT extract from above section
  di <- rbind(di,dr)
  nRows=nrow(di)
  
  # add four more rows for data from info website to current data
  eR=nRows+1
  hR=nRows+2
  iR=nRows+3
  mR=nRows+4
  
  # xpath anchor points in dump of info.gesundheitsministerium.at
  xpathAktualisierung  <- '//*[@id="divLetzteAktualisierung"]'
  xpathErkrankungen    <- '//*[@id="divErkrankungen"]'
  xpathHospitalisiert  <- '//*[@id="Hospitalisiert"]'
  xpathIntensivStation <- '//*[@id="Intensivstation"]'
  xpathMilderVerlauf   <- '//*[@id="MilderVerlauf"]'
  
  # use xml2 methods to extract information from dump
  logMsg(paste("Analysing dump file", dmpFile))
  x <- xml2::read_html(dmpFile)
  
  # Aktualisierung
  xa <- xml2::xml_find_all(x, xpathAktualisierung)
  xd <- xml2::xml_text(xa, trim=TRUE)
  tAktualisierung <- as.POSIXct(xd, format="%d.%m.%Y %H:%M.%S")
  logMsg(paste("Aktualisierung",tAktualisierung))
  
  # Erkrankungen
  xa <- xml2::xml_find_all(x, xpathErkrankungen)
  xd <- xml2::xml_text(xa, trim=TRUE)
  nErkrankungen <- as.integer(xd)
  di[eR,"Stamp"] <- Stamp 
  di[eR,"Count"] <- nErkrankungen
  di[eR,"Status"] <- "Erkrankungen"
  di[eR,"Region"] <- "AT"
  logMsg(paste("Erkrankungen",nErkrankungen))
  
  # Hospitalisiert
  xa <- xml2::xml_find_all(x, xpathHospitalisiert)
  xp <- './/div'
  xr <- xml2::xml_find_all(xa, xp)
  xd <- xml2::xml_text(xr, trim=TRUE)
  nHospitalisiert <- as.integer(xd[2])
  di[hR,"Stamp"] <- Stamp 
  di[hR,"Region"] <- "AT"
  di[hR,"Status"] <- "Hospitalisiert"
  di[hR,"Count"] <- nHospitalisiert
  logMsg(paste("Hospitalisiert",nHospitalisiert))
  
  # IntensivStation
  xa <- xml2::xml_find_all(x, xpathIntensivStation)
  xp <- './/div'
  xr <- xml2::xml_find_all(xa, xp)
  xd <- xml2::xml_text(xr, trim=TRUE)
  nIntensivStation <- as.integer(xd[2])
  di[iR,"Stamp"] <- Stamp 
  di[iR,"Region"] <- "AT"
  di[iR,"Status"] <- "IntensivStation"
  di[iR,"Count"] <- nIntensivStation
  logMsg(paste("IntensivStation",nIntensivStation))
  
  # MilderVerlauf
  xa <- xml2::xml_find_all(x, xpathMilderVerlauf)
  xp <- './/div'
  xr <- xml2::xml_find_all(xa, xp)
  xd <- xml2::xml_text(xr, trim=TRUE)
  nMilderVerlauf <- as.integer(xd[2])
  di[mR,"Stamp"] <- Stamp 
  di[mR,"Region"] <- "AT"
  di[mR,"Status"] <- "MilderVerlauf"
  di[mR,"Count"] <- nMilderVerlauf
  logMsg(paste("MilderVerlauf",nMilderVerlauf))
  
  # Print to console
  di %>% tail(n=10) %>% print()
  
  # Persist data
  logMsg(paste("Writing data to",csvFile))
  write.csv(di, csvFile, row.names=FALSE, quote=FALSE)
  
  
  # --------------------------------------------------------------------------------------------------------
  # Confirmed data for 100+ Regions in Austria -> goes into dedicated file
  # --------------------------------------------------------------------------------------------------------
  logMsg(paste("Extracting table of Confirmed in 100+ regions"))
  csvFile <- paste0("./data/COVID-19-austria.regions.csv")
  
  #Stamp=as.POSIXct("2020-03-25 08:00:00")
  #dmpFile="./html/COVID-19-austria.detail.20200325-0940.dmp"
  #x <- xml2::read_html(dmpFile)
  
  # table 'Confirmed by Bezirk'
  xpathBezirke <- '//*[@id="tblBezirke"]'
  tblBezirke <- xml2::xml_find_all(x, xpathBezirke)
  
  xpathRowsBezirke <- './/td'
  rowsBezirke <- xml2::xml_find_all(tblBezirke,xpathRowsBezirke)
    tbl <- xml2::xml_text(rowsBezirke, trim=TRUE)
  n = length(tbl)
  dx <- data.frame(Region=tbl[seq(3,n,by=2)], Count=tbl[seq(4,n,by=2)], stringsAsFactors=FALSE)
  dc <- dx %>% dplyr::mutate(Stamp=Stamp, Status="Confirmed", Count=as.integer(Count)) %>%
    dplyr::select(Stamp, Status, Region, Count) %>%
    dplyr::mutate(Region=str_remove(Region,",")) %>%
    dplyr::mutate(Region=str_replace_all(Region,"[ \\.\\(\\)]","_")) %>%
    dplyr::mutate(Region=str_replace(Region,"_$",""))
  
  # print a few results to console
  dc %>% tail(n=10) %>% print()
  
  # append new records to datafile
  logMsg(paste("Writing new data to", csvFile))
  dd <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp))
  dd <- rbind(dd,dc)
  write.csv(dd, file=csvFile, row.names=FALSE, quote=FALSE)
}



# --------------------------------------------------------------------------------------------------------
# Confirmed data for 100+ Regions in Austria -> goes into dedicated file
# --------------------------------------------------------------------------------------------------------
scrapeHospitalisierung <- function() {

  url="https://www.sozialministerium.at/Informationen-zum-Coronavirus/Dashboard/Zahlen-zur-Hospitalisierung"
  html <- read_html(url)

  # Extract numbers
  nTested=0
  Stamp = as.POSIXct("2020-03-26 15:00:00")
  
  # Extract Bundesländertable
  tables <- html_table(html)
  dh <- tables[[1]]
  dh %>% dplyr::inner_join(BL, by=c("Bundesland"="NameUTF82")) %>%
    dplyr::mutate(Stamp=Stamp) %>%
    dplyr::select(Stamp,Region=ID,Hospitalisierung,Intensivstation) %>%
    dplyr::arrange(Region) %>%
    tidyr::gather(key=Status, val=Count, Hospitalisierung, Intensivstation) %>%
    tidyr::spread(key=Region, val=Count)

  return (list(dh=dh, Tested=nTested, Stamp=Stamp))
}





logMsg("Done runing data extraction from bmsgpk web pages")
