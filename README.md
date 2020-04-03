# COVID-19-Austria
- Dataset for Austria based on Sozial Ministerium (bmsgpk). R Scripts for download, visualization, analysis and prediction
- Additional visualization, analysis and prediction for the John Hopkins University dataset 
- Dataset and extractors for the IBM covid-county  scraper project
- 'number of days until times ten cases': This is the main unit for speed of spread of COVID-19 used for all estimates

## News and Enhancements
- 2020-04-02: orf.at documents in detail the process of data publication by Sozialminiterium. All observation correct and experienced here during maintainance work on the scrapers. https://orf.at/corona/stories/daten-faq/
- 2020-04-01: Add 'Tested' to plot
- 2020-03-31: Add line on plot for rolling regression of number of days till deaths*10. Plotted in orange/red
- 2020-03-30: Add extractor for IBM covid-county project. This is scraper platform with a configurable scraper per country
- 2020-03-29: Create plots of current state of measures taken. *10 for confirmed cases is 24 days now. Not good enough  yet
- 2020-03-27: Nominated country lead for IBM covid-county data scraper project. New routines to convert to their data format
- 2020-03-26 21h: bmsgpk info site back online. Modified structure, new page with hopsitalized/intensecare cases per Bundesland
- 2020-03-26 09h: The bmsgpk info site is currently under maintenance. Manual update of COVID-19-austria.csv from bmsgpk COVID site
- 2020-03-25: Added datasets for 'Confirmed for 100+ regions in Austria' and 'Numbers for Hospitalization' from bmsgpk info site
- 2020-03-24: Scripts to support autoupdate of data from bmsgpk sites

## Current situation in Austria
- y-Axis left: log10 of cases
- y-Axis right: linear. number of days for black line: Number of days until Confirmed *10 calculated from previous 4 days
- ppm: Number of confirmed cases per million inhabitants. 1000 means one promille.
<p><img src="https://github.com/at062084/COVID-19-Austria/blob/master/bmsgpk/thumbs/covid.bmsgpk.stand.Oesterreich.2020-04-03.png" alt="covid.bmsgpk.stand.Oesterreich.2020-04-03.png"  width="1102" height="826"/></p>


##   bmsgpk - Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz
The official statistics for Austria are provided by the 'Sozial Ministerium (bmsgpk)'
- This dataset starts at 2020-02-25, the day the first COVID-19 case was confirmed in Austria. 
- Data on cases tested, confirmed, recovered and deaths are reported for Austria and the 'Bundeslaender' every day at 15h00. 
- From 2020-03-10 an additional report is issued at 08h00.
- Updated twice per day around 9h and 16h from 2020-03-21

### ./bmsgpk/data/COVID-19-austria.csv
Data up to 2020-03-20 have been manually compiled from
- https://www.kommunal.at/coronavirus-diese-daten-sind-die-grundlagen-fuer-massnahmen
- https://www.sn.at/panorama/oesterreich/coronavirus-in-oesterreich-und-salzburg-aktuelle-infektionszahlen-im-ueberblick-85045132
- and a few other sites

Starting from 2020-03-21 data have been manually extracted from 
- https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html
- Starting from 2020-03-21 these data are extracted from the above website by the ./bmsgpk/COVID-19-bmsgpk-extract.R script
- https://www.sozialministerium.at/Informationen-zum-Coronavirus/Dashboard/Zahlen-zur-Hospitalisierung
- https://info.gesundheitsministerium.gv.at/

### ./bmsgpk/data/COVID-19-austria.regions.csv
- Contains Confirmed cases data for 100+ regions in Austria (from 2020-03-24, extracted from info.sozialministerium.at)

### ./bmsgpk/data/COVID-19-austria.info.csv
- Contains AT wide data for 'hospitalized, seriouslyIll, intensiveCare, mildlyIll' plus Tested,Confirmed,Deaths from COVID-19-austria.csv (from 2020-03-24, extracted from info.sozialministerium.at)
- info no longer available as of 2020-03-26. Substituted by below data

### ./bmsgpk/data/COVID-19-austria.hospital.csv
- Contains Bundesland specific data for 'hospitalized, intensiveCare' (from 2020-03-26, extracted from new page linked form info.sozialministerium.at)

## jhucsse - John Hopkins University - Center for Systems Science and Engineering
Data published by the John Hopkins University, slightly polished.
- Reactivated after plots of defects around 2020-03-13. 
- Data for at least Austria have been corrected
### ./jhucsse/data/COVID-19-jhucsse.csv

## owid - OurWorldInData
Dataset provided by the OurWorldInData site
- https://covid.ourworldindata.org/data/full_data.csv
- Last data provided for 2020-03-17
- currently no updates here
- Plots with lots of data and calculations for 10+ regions worldwide
### ./owid/data/COVID-19-owid.csv

## ecdc - European Council for Decease Control
in progress
