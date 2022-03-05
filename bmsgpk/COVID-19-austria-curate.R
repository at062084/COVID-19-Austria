library(dplyr)
library(tidyr)
library(lubridate)

# --------------------------------------------------------------------------------------------
# States
# --------------------------------------------------------------------------------------------
setwd("/home/at001335/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk")
source("../COVID-19-common.R")

BL <- data.frame(ID=c("AT","B","K","NOe","OOe","Szb","Stmk","T","V","W"),
                 ISO=c("AT","AT-1","AT-2","AT-3","AT-4","AT-5","AT-6","AT-7","AT-8","AT-9"),
                 Name=c("Oesterreich","Burgenland","Kaernten","Niederoesterreich","Oberoesterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF8=c("Österreich","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 NameUTF82=c("Österreich gesamt","Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),
                 Population=c(8800, 294, 561, 1684, 1490, 558, 1247, 757, 397, 1900),
                 stringsAsFactors=FALSE)

csvFile <- paste0("./data/COVID-19-austria.csv")
df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))

csvFile <- paste0("./data/COVID-19-austria.hospital.csv")
dg <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))

# Set IDs to new convention used in dockerized scraper
dd <- rbind(df,dg)
colnames(dd) <- c("Stamp","Status",BL$ID)
str(dd)

# regions in cols
di <- dd %>% dplyr::arrange(Stamp,Status)
csvFile <- paste0("./data/COVID-19-austria.states.ingest.csv")
write.csv(di, file=csvFile, row.names=FALSE, quote=FALSE)

# Consolidation Step 1: sanitize
du <- dd %>% 
  dplyr::arrange(Stamp,Status) %>% 
  dplyr::filter(!is.na(Stamp),!is.na(AT)) %>%   # remove NA's on country level
  dplyr::filter(AT != 0) %>%                    # remove zeros on country level
  dplyr::distinct()                             # remove duplicates
csvFile <- paste0("./data/COVID-19-austria.states.curated.csv")
write.csv(du, file=csvFile, row.names=FALSE, quote=FALSE)

# Consolidation Step 2: one record per per Status
dc <- du %>%
  dplyr::mutate(Date=format(Stamp,"%Y-%m-%d"),"%Y-%m-%d") %>%
  dplyr::arrange(Date,Status,Stamp) %>% 
  dplyr::group_by(Date,Status) %>%              # one record per day for each status
  dplyr::filter(AT==max(AT)) %>%          # select latest entry for each status in current day
  dplyr::filter(row_number()==1) %>%   # select latest entry for each status in current day
  dplyr::ungroup() %>%
  dplyr::select(Stamp,Status,3:12)
# regions in cols
csvFile <- paste0("./data/COVID-19-austria.states.curated.status.csv")
write.csv(dc, file=csvFile, row.names=FALSE, quote=FALSE)

# long format wirh only count in cols
dl <- dc %>% tidyr::gather(key=Region, val=Count,3:12)
csvFile <- paste0("./data/COVID-19-austria.states.curated.status.region.csv")
write.csv(dl, file=csvFile, row.names=FALSE, quote=FALSE)

# status in cols
ds <- dl %>% tidyr::spread(key=Status, val=Count)
csvFile <- paste0("./data/COVID-19-austria.states.curated.regions.csv")
write.csv(ds, file=csvFile, row.names=FALSE, quote=FALSE)


# --------------------------------------------------------------------------------------------
# Counties
# --------------------------------------------------------------------------------------------

csvFile <- paste0("./data/COVID-19-austria.regions.csv")
df <- read.csv(file=csvFile) %>% dplyr::mutate(Stamp=as.POSIXct(Stamp, tz="CEST"))
str(df)

dg <- df %>% 
  arrange(Stamp,Status,Region) %>% 
  dplyr::filter(!is.na(Stamp)) %>%
  dplyr::distinct()
csvFile <- paste0("./data/COVID-19-austria.county.ingest.csv")
write.csv(dg, file=csvFile, row.names=FALSE, quote=FALSE)

csvFile <- paste0("./data/COVID-19-austria.county.curated.csv")
write.csv(dg, file=csvFile, row.names=FALSE, quote=FALSE)




