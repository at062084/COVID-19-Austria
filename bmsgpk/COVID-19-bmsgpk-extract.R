# COVID-19 data provided by Austrian bmsgpk (Gesundheitsminiterium)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)


setwd("/home/at062084/DataEngineering/COVID-19/bmsgpk")

#read manually maintained excel sheet
#library(readxl)
#xlsFile <- paste0("./data/COVID-19-austria.xls")
#df <- read_excel(xlsFile)

# read current data file
csvFile <- paste0("./data/COVID-19-austria.csv")
df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp))
nRows <- nrow(df)

# get html page from bmsgpk
bmsgpkFile <- paste0("./html/COVID-19-austria.bmsgpk.",format(now(),"%Y%m%d-%H%M"),".html")
cmd <- paste("\"https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html\"", "-O", bmsgpkFile)
system2("wget", cmd)
html <- read_file(bmsgpkFile)


# add four rows for data from website to current data
tR=nRows+1
cR=nRows+2
rR=nRows+3
dR=nRows+4

# Identifiers of sections  in html response of wget 
atTested <- "<p><strong>Bisher durchgef&uuml;hrte Testungen in &Ouml;sterreich"
atConfirmed <- "<p><strong>Best&auml;tigte F&auml;lle"
atRecovered <- "<p><strong>Genesene Personen"
atDeaths <- "<p><strong>Todesf&auml;lle"
closeAll <- "</p>"

# Stamp
s <- str_extract(html, paste0(atConfirmed,".*",closeAll))
t <- stringr::str_match(s,paste0("<strong>Stand ","([\\d\\s\\.,:]*)"," Uhr</strong>"))[2]
Stamp <- as.POSIXct(t, format="%d.%m.%Y, %H:%M", tz="CET")

# Extract total number of Tested
s <- str_extract(html, paste0(atTested,".*",closeAll))
t <- str_match(s,paste0("</strong>","([\\d\\.]*)","</p>"))[,2]
totTested <- as.integer(str_remove(t,"\\."))
df[tR,"Stamp"] <- Stamp + hours(1)
df[tR,"Status"] <- "Tested"
df[tR,"AT"] <- totTested

# Definition of Bundesländer for localized data
Bundeslaender <- data.frame(Name=c("Burgenland","K&auml;rnten","Nieder&ouml;sterreich","Ober&ouml;sterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                            Letter=c("B","K","Noe","Ooe","Szbg","Stmk","T","V","W"), stringsAsFactors=FALSE)

# Extract number of Confirmed cases
s <- str_extract(html, paste0(atConfirmed,".*",closeAll))
df[cR,"Stamp"] <- Stamp + hours(1)
df[cR,"Status"] <- "Confirmed"
for (bl in Bundeslaender$Name) {
  n <- as.integer(str_match(s,paste0(bl," \\(","(\\d*)","\\)"))[2])
  #cat(bl,n,"\n")
  df[cR,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
}
df[cR,"AT"] <- sum(df[cR,c(Bundeslaender[,2])])

# Extract number of Recovered cases
s <- str_extract(html, paste0(atRecovered,".*",closeAll))
nAT <- as.numeric(str_match(s,paste0("</strong>: (\\d*),"))[2])
df[rR,"Stamp"] <- Stamp + hours(1)
df[rR,"Status"] <- "Recovered"
df[rR,"AT"] <- nAT
for (bl in Bundeslaender$Name) {
  n <- str_match(s,paste0(bl," \\(","(\\d*)","\\)"))[2]
  if (is.na(n)) n <- 0
  df[rR,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
  #cat(bl,n,"\n")
}

# Extract number of Deaths
s <- str_extract(html, paste0(atDeaths,".*",closeAll))
nAT <- as.numeric(str_match(s,paste0("Uhr:</strong> (\\d*),"))[2])
df[dR,"Stamp"] <- Stamp + hours(1)
df[dR,"Status"] <- "Deaths"
df[dR,"AT"] <- nAT
for (bl in Bundeslaender$Name) {
  n <- str_match(s,paste0(bl," \\(","(\\d*)","\\)"))[2]
  if (is.na(n)) n <- 0
  df[dR,Bundeslaender[Bundeslaender$Name==bl,2]] <-n
  #cat(bl,n,"\n")
}


# Persist data
write.csv(df, csvFile, row.names=FALSE, quote=FALSE)



