# COVID-19 data provided by Austrian bmsgpk (Gesundheitsminiterium)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(xml2)
library(rvest)


options(error = function() traceback(2))
options(width=256)

setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")


# do some logging
logFile <- "./COVID-19-bmsgpk-extract.log"
logMsg <- function(msg) {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}

scrapeCovid2 <- function(ts=format(now(),"%Y%m%d-%H%M")) {

  # wget -O CoronaAmpel-20200904.js https://corona-ampel.gv.at/sites/corona-ampel.gv.at/files/assets/Warnstufen_Corona_Ampel_aktuell.json
  
  ampelFile <- paste0("./data/ampel/CoronaAmpel.",ts,".js")
  url="\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\""
  logMsg(paste("Download Ampel data from", url))
  logMsg(paste("Storing Ampel data to", ampelFile))
  cmd <- paste(url, "-O", ampelFile)
  system2("wget", cmd)
  
  # get html page from bmsgpk
  bmsgpkFile <- paste0("./html/COVID-19-austria.bmsgpk.",ts,".html")
  url="\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\""
  logMsg(paste("Scraping", url))
  logMsg(paste("Dumping page to", bmsgpkFile))
  cmd <- paste(url, "-O", bmsgpkFile)
  system2("wget", cmd)
  
  #xpathTable <- "/html/body/div[3]/div/div/div/div[2]/main/div[2]/table"
  #xt <- xml2::xml_find_all(html, xpathTable)

  logMsg(paste("Parsing dump in", bmsgpkFile))
  html <- xml2::read_html(bmsgpkFile)
  
  logMsg(paste("Extracting Status table in Bundesländer"))
  tables <- rvest::html_table(html, dec=",", fill=TRUE)
  dx <- tables[[1]][,1:11]
  
  # Extract Stamp and Status from first col
  S0 <- dx[,1]
  # S1 <- str_match(S0, paste0("(.*)","\\\n\\\t\\\t\\\t\\("))[,2]
  S2 <- str_split_fixed(S0, "\\(", n=2)[,1]
  Status <- str_split_fixed(S2, "\\*",n=2)[,1]
  Status <- str_match(Status,"[a-zA-Z0-9äöüÄÖÜß ]*")
  Status <- trimws(Status)
  # Must watchout for invisible blank characters that are encoded as '&nbsp;' in original html
  Stamp <- as.POSIXct(str_replace_all(str_match(S0, paste0("Stand","(.*)","Uhr"))[,2],"[^0-9:.,]",""),format="%d.%m.%Y,%H:%M")
  # Be tolerant to time stamp format changes on website
  idx <- which(is.na(Stamp))
  if (length(idx)>0) {
    Stamp[idx] <- as.POSIXct(str_replace_all(str_match(S0, paste0("Stand","(.*)","Uhr"))[idx,2],"[^0-9:.,]",""),format="%d.%m.%Y,%H.%M")
  }
  # Be tolerant to time stamp format changes on website
  idx <- which(is.na(Stamp))
  if (length(idx)>0) {
    Stamp[idx] <- as.POSIXct(str_replace_all(str_match(S0, paste0("Stand","(.*)","Uhr"))[idx,2],"[^0-9:.,]",""),format="%d.%m.%Y.%H:%M")
  }
  
  df <- dx %>%
    dplyr::select(11,2:10) %>% 
    mutate_all(funs(str_replace_all(., "\\.", ""))) %>% 
    mutate_all(funs(as.integer(.))) %>%
    dplyr::mutate(Stamp=Stamp,Status=Status)  %>%
    dplyr::select(Stamp,Status,1:10) 
  colnames(df) <- c("Stamp","Status",BL$ID)

  # Rename Status to previous Labels
  StatusMap <- data.frame(
    from=c("Bestätigte Fälle","Todesfälle","Genesen","Hospitalisierung","Intensivstation","Testungen","Davon PCR","Davon Antigen"),
    to=c("Confirmed","Deaths","Recovered","Hospitalisierung","Intensivstation","Tested","Tested_PCR","Tested_AG"), stringsAsFactors=FALSE)
  
  for (s in 1:nrow(df)) {
    n = which(df$Status[s]==StatusMap$from)
    df$Status[s] <- StatusMap$to[n]
  }
  
  # Print to console
  df %>% tail(n=10) %>% print()
  return(df)
}


# --------------------------------------------------------------------------------------------------------
# Extract data from www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html
# https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html
# --------------------------------------------------------------------------------------------------------
scrapeCovid <- function(ts=format(now(),"%Y%m%d-%H%M")) {

  # get html page from bmsgpk
  bmsgpkFile <- paste0("./html/COVID-19-austria.bmsgpk.",ts,".html")
  url="\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\""
  logMsg(paste("Scraping", url))
  logMsg(paste("Dumping page to", bmsgpkFile))
  cmd <- paste(url, "-O", bmsgpkFile)
  system2("wget", cmd)
  
  logMsg(paste("Parsing dump in", bmsgpkFile))
  html <- read_file(bmsgpkFile)
  
  # read current data to extract data structure
  csvFile <- paste0("./data/COVID-19-austria.csv")
  df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))
  df <- df[1,]
  df <- df[-1,]

  # add records to empty data frame  
  iTested <- 1
  iConfirmed <- 2
  iRecovered <- 3
  iDeaths <- 4
  
  # Identifiers of sections  in html response of wget 
  atTested <- "<p><strong>Bisher durchgef&uuml;hrte Testungen in &Ouml;sterreich"
  # <strong>Bisher durchgeführte Testungen in Österreich (tägliche Aktualisierung des Ist-Standes um 10:00 Uhr): </strong> #2020-04-02
  atConfirmed <- "<p><strong>Best&auml;tigte F&auml;lle, </strong><strong>Stand"
  atConfirmed <- "<p><strong>Best&auml;tigte F&auml;lle, Stand "                  # new as of 2020-03-27
  atRecovered <- "<p><strong>Genesen, Stand "
  #atDeaths <- "<p><strong>Todesf&auml;lle</strong>, <strong>Stand"
  atDeaths <- "<p><strong>Todesf&auml;lle</strong><sup>\\(1\\)</sup>, <strong>Stand "  # update 2020-03-30
  closeAll <- "</p>"
  
  # Definition of Bundesländer for localized data
  Bundeslaender <- data.frame(Name=c("Burgenland","K&auml;rnten","Nieder&ouml;sterreich","Ober&ouml;sterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                              Letter=c("B","K","Noe","Ooe","Szbg","Stmk","T","V","W"), stringsAsFactors=FALSE)
  

  # default timestamp
  Stamp <- as.POSIXct(format(now(),"%Y-%m-%d %H:%M"), format="%Y-%m-%d %H:%M", tz="CEST")
    
  # Pick up individual Stamp anyway
  bCalcStamp=TRUE
  
  
  
  # Extract number of Tested
  # ------------------------
  s <- str_extract(html, paste0(atTested,".*",closeAll))
  t <- str_match(s,paste0("</strong>","([\\d\\. ]*)"))[,2]
  totTested <- as.integer(str_remove(t,"\\."))
  if(bCalcStamp) {
    # t <- str_match(s, paste0("Aktualisierung des Ist-Standes um","([\\d :]*)","Uhr"))[2]
    # Stamp <- as.POSIXct(paste0(format(now(),"%Y-%m-%d "),t), format="%Y-%m-%d %H:%M", tz="CEST")
    t <- str_match(s, paste0("Stand","(.*)","Uhr"))[2]
    Stamp <- as.POSIXct(t, format="%d.%m.%Y, %H:%M", tz="CEST")
  }
  df[iTested,"Stamp"] <- Stamp 
  df[iTested,"Status"] <- "Tested"
  df[iTested,"AT"] <- totTested
  logMsg(paste("Tested",totTested))
  
  # Extract number of Confirmed cases  F&auml;lle,
  #     Stamp <- as.POSIXct(paste0(format(now(),"%Y-%m-%d "),t), format="%Y-%m-%d %H:%M")
  # ---------------------------------
  logMsg("Confirmed")
  s <- str_extract(html, paste0(atConfirmed,".*",closeAll))
  if(bCalcStamp) {
    t <- str_match(s, paste0("Stand","(.*)","Uhr"))[2]
    Stamp <- as.POSIXct(t, format="%d.%m.%Y, %H.%M", tz="CEST")
  }
  df[iConfirmed,"Stamp"] <- Stamp 
  df[iConfirmed,"Status"] <- "Confirmed"
  if(!is.na(s)) {
    nAT <- as.integer(str_remove(str_match(s,paste0("</strong>","([\\d\\. ]*)","F&auml;lle,"))[2],"\\."))
    if(is.na(nAT)) {
      nAT <- as.integer(str_remove(str_match(s,paste0("<strong>","([\\d\\. ]*)","</strong>F&auml;lle,"))[2],"\\."))
    }
    df[iConfirmed,"AT"] <- nAT
    for (bl in Bundeslaender$Name) {
      n <- as.integer(str_remove(str_match(s,paste0(bl," \\(","([\\d\\.]*)","\\)"))[2],"\\."))
      df[iConfirmed,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
    }
    logMsg(paste("Confirmed",nAT))
  } else {
    logMsg("WARN: No record for Confirmed found")
  }
  
  # Extract number of Recovered cases
  # ---------------------------------
  s <- str_extract(html, paste0(atRecovered,".*",closeAll))
  if(bCalcStamp) {
    t <- str_match(s, paste0("Stand","(.*)","Uhr"))[2]
    Stamp <- as.POSIXct(t, format="%d.%m.%Y, %H:%M", tz="CEST")
  }
  
  df[iRecovered,"Stamp"] <- Stamp 
  df[iRecovered,"Status"] <- "Recovered"
  if (!is.na(s)) {
    nAT <- as.integer(str_remove(str_match(s,paste0("Uhr:</strong> ","([\\d\\. ]*)","[,<]"))[2],"\\."))
    df[iRecovered,"AT"] <- nAT
    for (bl in Bundeslaender$Name) {
      n <- as.integer(str_remove(str_match(s,paste0(bl," \\(","([\\d\\.]*)","\\)"))[2],"\\."))
      if (is.na(n)) n <- NA
      df[iRecovered,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
    }
    logMsg(paste("Recovered",nAT))
  } else {
    logMsg("WARN: No record for Recovered found")
  }
  
  # Extract number of Deaths
  # ------------------------
  s <- str_extract(html, paste0(atDeaths,".*",closeAll))
  if(bCalcStamp) {
    t <- str_match(s, paste0("Stand","(.*)","Uhr"))[2]
    Stamp <- as.POSIXct(t, format="%d.%m.%Y, %H:%M", tz="CEST")
  }
  df[iDeaths,"Stamp"] <- Stamp 
  df[iDeaths,"Status"] <- "Deaths"
  sAT <- str_match(s,paste0("Uhr:</strong> ","([\\d\\.]*)",", nach"))[2]
  if(is.na(sAT)) {
    sAT <- str_match(s,paste0("strong>","([ \\d\\.]*)",", nach"))[2]
  }
  if(is.na(sAT)) {
    sAT <- str_match(s,paste0("strong>","([ \\d\\.]*)",", "))[2]
  }
  if (!is.na(sAT)) {
    nAT <- as.integer(str_remove(sAT,"\\."))
    df[iDeaths,"AT"] <- nAT
    for (bl in Bundeslaender$Name) {
      n <- as.integer(str_remove(str_match(s,paste0(bl," \\(","([\\d\\.]*)","\\)"))[2],"\\."))
      if (is.na(n)) n <- 0
      df[iDeaths,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
    }
    logMsg(paste("Deaths",nAT))
  } else {
    logMsg("WARN: No record for Deaths found")
  }
  
  # Print to console
  df %>% tail() %>% print()
  return(df)
}


# --------------------------------------------------------------------------------------------------------
# Confirmed cases by Region from info.gesundheitsministerium.at
# --------------------------------------------------------------------------------------------------------
scrapeInfo <- function(ts=format(now(),"%Y%m%d-%H%M")) {
  
  # xpath of Bundesländer data on info site
  # /html/body/div[2]/div[2]/div[3]/div[2]/div/canvas[2]

  # crawl and dump javascripted site using chrome headless
  dmpFile=paste0("./html/COVID-19-austria.info.",ts,".dmp")
  
  url="https://info.gesundheitsministerium.at/"
  url="https://info.gesundheitsministerium.at/dashboard_Epidem.html?l=de"
  logMsg(paste("Scraping using headless chrome for", url))
  
  chrome="/opt/google/chrome/chrome"
  flags="--headless --disable-gpu --dump-dom"
  logMsg(paste("Dumping page to", dmpFile))
  system2(chrome,paste(url, flags, ">", dmpFile))

  # xpath anchor points in dump of info.gesundheitsministerium.at
  #xpathAktualisierung  <- '//*[@id="divLetzteAktualisierung"]'
  #xpathErkrankungen    <- '//*[@id="divErkrankungen"]'

  # use xml2 methods to extract information from dump
  logMsg(paste("Analysing dump file", dmpFile))
  x <- xml2::read_html(dmpFile)
  
  # Aktualisierung
  #xa <- xml2::xml_find_all(x, xpathAktualisierung)
  #xd <- xml2::xml_text(xa, trim=TRUE)
  #Stamp <- as.POSIXct(xd, format="%d.%m.%Y %H:%M.%S", tz="CEST")
  #logMsg(paste("Aktualisierung",Stamp))

  #xpathErkrankungen <- "/html/body/main/div/div/div[7]/div/div/table"
  
  # Erkrankungen
  #xa <- xml2::xml_find_all(x, xpathErkrankungen)
  #xd <- xml2::xml_text(xa, trim=TRUE)
  #nErkrankungen <- as.integer(xd)
  #logMsg(paste("Erkrankungen",nErkrankungen))  
  
  logMsg(paste("Extracting table of Confirmed in 100+ regions"))
  Stamp <- as.POSIXct(now(), format="%Y-%m-%d %H:%M:%S", tz="CEST")
  tables <- rvest::html_table(x, dec=",")
  dx <- tables[[1]]
  colnames(dx) <- c("Region","Count")

  dc <- dx %>% 
    #dplyr::mutate_all(funs(str_replace(., "\\.000", ""))) %>% 
    dplyr::mutate_all(funs(str_replace(., "\\.", ""))) %>% 
    dplyr::mutate(Stamp=Stamp, Status="Confirmed", Count=as.integer(Count)) %>%
    dplyr::select(Stamp, Status, Region, Count) %>%
    dplyr::mutate(Region=str_remove(Region,",")) %>%
    dplyr::mutate(Region=str_replace_all(Region,"[ \\.\\(\\)]","_")) %>%
    dplyr::mutate(Region=str_replace(Region,"_$",""))
    
  
  # print a few results to console
  dc %>% tail() %>% print()
  return(dc)
}

scrapeInfo2 <- function(ts=format(now(),"%Y%m%d-%H%M")) {
  
  # xpath of Bundesländer data on info site
  # /html/body/div[2]/div[2]/div[3]/div[2]/div/canvas[2]
  
  # crawl and dump javascripted site using chrome headless
  dmpFile=paste0("./html/COVID-19-austria.info.",ts,".dmp")
  
  #url="https://info.gesundheitsministerium.at/"
  #url="https://info.gesundheitsministerium.at/dashboard_Epidem.html?l=de"
  url="https://covid19-dashboard.ages.at/dashboard.html"
  logMsg(paste("Scraping using headless chrome for", url))
  
  chrome="/opt/google/chrome/chrome"
  flags="--headless --disable-gpu --dump-dom"
  logMsg(paste("Dumping page to", dmpFile))
  system2(chrome,paste(url, flags, ">", dmpFile))
  
  # xpath anchor points in dump of info.gesundheitsministerium.at
  #xpathAktualisierung  <- '//*[@id="divLetzteAktualisierung"]'
  #xpathErkrankungen    <- '//*[@id="divErkrankungen"]'
  
  # use xml2 methods to extract information from dump
  logMsg(paste("Analysing dump file", dmpFile))
  x <- xml2::read_html(dmpFile)
  
  # Aktualisierung
  #xa <- xml2::xml_find_all(x, xpathAktualisierung)
  #xd <- xml2::xml_text(xa, trim=TRUE)
  #Stamp <- as.POSIXct(xd, format="%d.%m.%Y %H:%M.%S", tz="CEST")
  #logMsg(paste("Aktualisierung",Stamp))
  
  #xpathErkrankungen <- "/html/body/main/div/div/div[7]/div/div/table"
  
  # Erkrankungen
  #xa <- xml2::xml_find_all(x, xpathErkrankungen)
  #xd <- xml2::xml_text(xa, trim=TRUE)
  #nErkrankungen <- as.integer(xd)
  #logMsg(paste("Erkrankungen",nErkrankungen))  
  
  logMsg(paste("Extracting table of Confirmed in 100+ regions"))
  Stamp <- as.POSIXct(now(), format="%Y-%m-%d %H:%M:%S", tz="CEST")
  tables <- rvest::html_table(x, dec=",")
  dx <- tables[[1]]
  colnames(dx) <- c("Region","Count")
  
  dc <- dx %>% 
    #dplyr::mutate_all(funs(str_replace(., "\\.000", ""))) %>% 
    dplyr::mutate_all(funs(str_replace(., "\\.", ""))) %>% 
    dplyr::mutate(Stamp=Stamp, Status="Confirmed", Count=as.integer(Count)) %>%
    dplyr::select(Stamp, Status, Region, Count) %>%
    dplyr::mutate(Region=str_remove(Region,",")) %>%
    dplyr::mutate(Region=str_replace_all(Region,"[ \\.\\(\\)]","_")) %>%
    dplyr::mutate(Region=str_replace(Region,"_$",""))
  
  
  # print a few results to console
  dc %>% tail() %>% print()
  return(dc)
}


# --------------------------------------------------------------------------------------------------------
# Confirmed data for 100+ Regions in Austria -> goes into dedicated file
# --------------------------------------------------------------------------------------------------------
scrapeHospitalisierung <- function(ts=format(now(),"%Y%m%d-%H%M")) {

  dmpFile=paste0("./html/COVID-19-austria.regions.",ts,".html")
  url="https://www.sozialministerium.at/Informationen-zum-Coronavirus/Dashboard/Zahlen-zur-Hospitalisierung"
  logMsg(paste("Scraping", url))
  
  logMsg(paste("Dumping page to", dmpFile))
  cmd <- paste0("\"",url,"\"", " -O ", dmpFile)
  system2("wget", cmd)

  logMsg(paste("Reading dump from", dmpFile))
  html <- xml2::read_html(dmpFile)

  # Stamp of data update
  xpathStand ="/html/body/div[3]/div/div/div/div[2]/main/h2[2]"
  s <- html_text(html_nodes(html, xpath = xpathStand))
  # for some reason a ancoding problem has sufaced here. This is a workaround until permanent solution
  t <- stringr::str_match(s,paste0(".*","(\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d)"))[2]
  d <- stringr::str_match(s,paste0("(\\d\\d:\\d\\d)"))[2]
  Stamp <- as.POSIXct(paste(t,d), format="%d.%m.%Y %H:%M", tz="CEST")
  
  # Number of confirmed cases
  xpathAnzahl="/html/body/div[3]/div/div/div/div[2]/main/p[1]"
  s <- html_text(html_nodes(html, xpath = xpathAnzahl))
  t <- stringr::str_match(s,paste0(".*","(\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d)"))[2]
  d <- stringr::str_match(s,paste0(".*(\\d\\d:\\d\\d)"))[2]
  StampTested <- as.POSIXct(paste(t,d), format="%d.%m.%Y %H:%M", tz="CEST")
  nTested <- as.integer(str_replace(stringr::str_match(s,paste0(".*: ","([\\.\\d]*)"))[2],"\\.",""))
  
  # Extract Bundesländertable
  logMsg("Extracting table of Hospitalized cases for Bundeslaender")
  tables <- rvest::html_table(html, dec="")
  dh <- tables[[1]]
  df <- dh %>% dplyr::inner_join(BL, by=c("Bundesland"="NameUTF82")) %>%
    dplyr::mutate(Stamp=Stamp) %>%
    dplyr::select(Stamp,Region=ID,Hospitalisierung,Intensivstation) %>%
    dplyr::arrange(Region) %>%
    tidyr::gather(key=Status, val=Count, Hospitalisierung, Intensivstation) %>%
    tidyr::spread(key=Region, val=Count) %>% 
    dplyr:: mutate_all(str_replace_all, "\\.", "")

  df %>% tail() %>% print()
  return (df)
}


# --------------------------------------------------------------------------------------------------------
# Confirmed cases by Bezirk from info.gesundheitsministerium.at
# --------------------------------------------------------------------------------------------------------
scrapeZIP <- function(ts=format(now(),"%Y-%m-%d_%H%M")) {
  
  zipFile=paste0("./data/zip/COVID-19-austria.V0414.",ts,".zip")
  #zipDir=paste0("./data/zip/",ts)
  url="https://info.gesundheitsministerium.at/data/data.zip"
  
  logMsg(paste("Downloading", url, "to", zipFile))
  cmd <- paste0("\"",url,"\"", " -O ", zipFile)
  system2("wget", cmd)
  
  #cmd <- paste(zipFile, "-d", zipDir)
  #system2("unzip",cmd)
  
  return(0)
}
scrapeZIP_AGES <- function(ts=format(now(),"%Y-%m-%d_%H%M")) {
  
  zipFile=paste0("./data/zip/COVID-19-austria.V1006.",ts,".zip")
  unzipDir="./data/unzip"
  agesDir="./data/ages"
  url="https://covid19-dashboard.ages.at/data/data.zip"
  
  logMsg(paste("Downloading", url, "to", zipFile))
  cmd <- paste0("\"",url,"\"", " -O ", zipFile)
  system2("wget", cmd)
  
  cmd <- paste("-fo", zipFile, "-d", unzipDir)
  system2("unzip",cmd)
  
  parms <- paste0("-f ",unzipDir, "/Covid*", " ", agesDir)
  system2("cp", parms)
  
  parms <- paste0("-f ", unzipDir, "/*Time*", " ", agesDir)
  system2("cp", parms)

  parms <- paste0("-f ", unzipDir, "/Epikurve.csv", " ", agesDir)
  system2("cp", parms)
  
  return(0)
}



# --------------------------------------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------------------------------------


# Bundesländer in Österreich
csvFile <- paste0("./data/COVID-19-austria.csv")
df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))
BL <- data.frame(ID=colnames(df[3:12]),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)


# timestamp for dump files
ts=format(now(),"%Y%m%d-%H%M")
logMsg(paste("Running COVID-19-bmsgpk-extract.R"))

# OK scrapeCovid
logMsg(paste("Calling scrapeCovid2 with", ts))
dc <- scrapeCovid2(ts=ts)

# scrapeHospitalisierung
logMsg(paste("DISABLED: Calling scrapeHospitalisierung with", ts))
#dh <- scrapeHospitalisierung(ts=ts)

# OK scrapeInfo
logMsg(paste("DISABLED: Calling scrapeInfo with", ts))
#di <- scrapeInfo(ts=ts)

# OK scrapeInfo
logMsg(paste("Calling scrapeZIP_AGES with", ts))
z <- scrapeZIP_AGES(ts=ts)

# Persist data
csvFile <- paste0("./data/COVID-19-austria.csv")
logMsg(paste("Writing new data to", csvFile))
df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))
df <- rbind(df,dc)
write.csv(df, file=csvFile, row.names=FALSE, quote=FALSE)

#csvFile <- paste0("./data/COVID-19-austria.hospital.csv")
#logMsg(paste("Writing new data to", csvFile))
#df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))
#df <- rbind(df,dh)
#write.csv(df, file=csvFile, row.names=FALSE, quote=FALSE)

csvFile <- paste0("./data/COVID-19-austria.regions.csv")
logMsg(paste("DISABLED: Writing new data to", csvFile))
#df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))
#df <- rbind(df,di)
#write.csv(df, file=csvFile, row.names=FALSE, quote=FALSE)

logMsg("Done runing data extraction from bmsgpk web pages")


