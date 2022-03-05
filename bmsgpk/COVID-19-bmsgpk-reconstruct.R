# COVID-19 data provided by Austrian bmsgpk (Gesundheitsminiterium)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(xml2)
library(rvest)

options(error = function() traceback(2))
options(width=256)

setwd("/home/at001335/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")

#csvFile <- paste0("./data/COVID-19-austria.csv")
#df <- read.csv(file=csvFile)
BL <- data.frame(ID=c("AT","B","K","Noe","Ooe","Szbg","Stmk","T","V","W"),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)


# do some logging
logFile <- "./COVID-19-bmsgpk-reconstract.log"
logMsg <- function(msg) {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="", file=logFile, append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), msg, "\n"), sep="")
}

reconstructCovid2 <- function(htmlFile) {
  
  # wget -O CoronaAmpel-20200904.js https://corona-ampel.gv.at/sites/corona-ampel.gv.at/files/assets/Warnstufen_Corona_Ampel_aktuell.json
  
  # get html page from bmsgpk
  logMsg(paste("Parsing dump in", htmlFile))
  html <- xml2::read_html(htmlFile)
  
  logMsg(paste("Extracting Status table in Bundesländer"))
  tryCatch({
    tables <- rvest::html_table(html, dec=",", fill=TRUE)
    dx <- tables[[1]][,1:11]
    
    # Extract Stamp and Status from first col
    S0 <- dx[,1]
    # S1 <- str_match(S0, paste0("(.*)","\\\n\\\t\\\t\\\t\\("))[,2]
    S2 <- str_split_fixed(S0, "\\(", n=2)[,1]
    Status <- str_split_fixed(S2, "\\*",n=2)[,1]
    Status <- str_match(Status,"[a-zA-Z0-9äöüÄÖÜß ]*")
    
    # !!! TODO: this is an error in the original implementation !!!
    Status <- as.vector(trimws(Status))
    
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

    #dx[,2:10] %>% print()
    #dx[,2:11] %>% mutate_all(funs(str_replace_all(., "[\\.\\*,]", ""))) %>% mutate_all(funs(str_replace_all(., fixed("kA"), "NA"))) %>% print()
    #dx[,2:11] %>%  mutate_all(funs(str_replace_all(., "[\\.\\*, ◊kA]", ""))) %>% print()
    
    df <- dx %>%
      dplyr::select(11,2:10) %>% 
      mutate_all(funs(str_replace_all(., "[\\.\\*, ◊kA]", ""))) %>% 
      #mutate_all(funs(str_extract(., "(\\d+)"))) %>% 
      mutate_all(funs(as.integer(.))) %>%
      dplyr::mutate(Stamp=Stamp, Status=Status)  %>%
      dplyr::select(Stamp,Status,1:10) 
    colnames(df) <- c("Stamp","Status",BL$ID)
    
    # Rename Status to previous Labels
    StatusMap <- data.frame(
      from=c("Bestätigte Fälle","Todesfälle","Genesen","Hospitalisierung","Intensivstation","Testungen","Davon PCR","Davon Antigen"),
      to=c("Confirmed","Deaths","Recovered","Hospitalisierung","Intensivstation","Tested","Tested_PCR","Tested_AG"), stringsAsFactors=FALSE)
    
    for (s in 1:nrow(df)) {
      n = which(df$Status[s]==StatusMap$from)
      df[s,"Status"] <- StatusMap$to[n]
    }
  }, error = function(msg) { logMsg(paste("ERROR:",htmlFile, msg)); return(NULL)})  
  
  # Print to console
  return(df)
}


# -----------------------------------------------------------------------------------------------------
# main
# -----------------------------------------------------------------------------------------------------

# Downloaded html files from COS into /home/at001335/DataEngineering/COVID-19/covid-ibm/download/dumps
htmlPath<-"../../covid-ibm/download/dumps"
htmlFiles <- list.files(path=htmlPath, pattern="COVID-19-austria.bmsgpk.2021([0-9-]*).html")

da <- data.frame(stringsAsFactors=FALSE)
# htmlFile <- paste(htmlPath,list.files(path=htmlPath, pattern="COVID-19-austria.bmsgpk.20200814(.*).html"),sep="/")
# htmlFile <-list.files(path=htmlPath, pattern="COVID-19-austria.bmsgpk.20210814(.*).html")
logMsg(paste("Processing files in", htmlPath))
for (htmlFile in htmlFiles) {
  logMsg(paste("  Working", htmlFile))
  df <- reconstructCovid2(paste(htmlPath,htmlFile,sep="/"))
  if(dim(da)[1]==0) {
    da <- df
  }  else {
    if (!is.null(df)) {
      da <- rbind(da,df)
    }
  }
}

# remove double entries
da <- da %>% unique()

csvFile <- paste0("./data/COVID-19-austria.reconstructed.2021.csv")
logMsg(paste("Writing reconstructed datafile", csvFile))
write.csv(da, file=csvFile, quote=FALSE, row.names=FALSE)

rdaFile <- paste0("./data/COVID-19-CWM-BMSGPK-Dashboard.reconstructed.2021.rda")
logMsg(paste("Writing reconstructed datafile", rdaFile))
saveRDS(da, file=rdaFile)

