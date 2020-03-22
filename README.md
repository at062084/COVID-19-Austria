# COVID-19-Austria
COVID-19: Dataset for Austria. R Scripts for download, visualization, analysis and prediction

## bmsgpk - Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz
This dataset starts at 2020-02-25, the day the first COVID-19 case was confirmed in Austria. The official statisitcs are provided by the 'Sozial Ministerium (bmsgpk)'. Data on cases tested, confirmed, recovered and deaths are reported for Austria and the 'Bundeslaender' every day at 15h00. From 2020-03-10 an additional report is issued at 08h00.

### ./bmsgpk/data/COVID-19-austria.csv
Data up to 2020-03-20 have been manually compiled from
- https://www.kommunal.at/coronavirus-diese-daten-sind-die-grundlagen-fuer-massnahmen
- https://www.sn.at/panorama/oesterreich/coronavirus-in-oesterreich-und-salzburg-aktuelle-infektionszahlen-im-ueberblick-85045132
- and a few other sites

Starting from 2020-03-21 data have been manually extracted from 
- https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html
- Starting from 2020-03-21 these data are extracted from the above website by the ./bmsgpk/COVID-19-bmsgpk-extract.R script

## owid - OurWorldInData
In progress


## ecdc - European Council for Decease Control
in progress


## jhu - John Hopkins University
in progress