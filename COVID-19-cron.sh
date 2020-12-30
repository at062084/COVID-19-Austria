#!/bin/bash

if [ $# -ne 1 ]
then
	echo "Usage: $0 <bmsgpk|county>"
	exit 1
fi

SECTION=$1

STAMP=`date "+%Y-%m-%d %H:%M:%S"`
DT=`date "+%Y%m%d-%H%M"`
BASE_DIR="/home/at062084/DataEngineering/COVID-19/COVID-19-Austria"
LOG_FILE=$BASE_DIR/COVID-19.cron.log
cd $BASE_DIR


echo "" >> $LOG_FILE
echo "--------------------------------------------------------------------" >> $LOG_FILE
echo "$STAMP Running $0 for SECTION=$SECTION" >> $LOG_FILE
echo "$STAMP Workdir $BASE_DIR" >> $LOG_FILE
echo "--------------------------------------------------------------------" >> $LOG_FILE

if [ "$SECTION" == "bmsgpk" ]
then
	# Run Rscript that downlads and stores the data from bmsgpk
	cd  $BASE_DIR/bmsgpk

	echo "$STAMP Running auto-extraction of COVID-19 data from bmsgpk website" | tee -a $LOG_FILE
	/usr/bin/Rscript ./COVID-19-bmsgpk-extract.R 2>&1 |tee -a  $LOG_FILE

	# Update git
	git add ../*.log
	git add *.log
	git add ./html/*
	git add ./data/*
	git commit -m "$STAMP Auto-Extract data from bmsgpk website" 2>&1 | tee -a $LOG_FILE
	git push 2>&1 | tee -a $LOG_FILE
fi

if [ "$SECTION" == "county" ]
then
	# Run Rscript that downlads and stores the data from bmsgpk
	cd  $BASE_DIR/bmsgpk

	echo "$STAMP Running conversion of COVID-19 data to covid-county format"  | tee -a $LOG_FILE
	/usr/bin/Rscript ./COVID-19-covid-county-extract.R 2>&1 | tee -a  $LOG_FILE
	for i in ./data/AT_*.bmsgpk.csv; do echo ""; echo $i; head -11 $i; done

	# Update git
	git add ../*.log
	git add *.log
	git add ./data/*
	git commit -m "$STAMP Auto-Conversion of bmsgpk data to covid-county format" 2>&1 | tee -a  $LOG_FILE
	git push 2>&1 | tee -a $LOG_FILE
fi

