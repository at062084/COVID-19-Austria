#!/bin/bash

if [ $# -ne 1 ]
then
	echo "Usage: $0 <bmsgpk|info>"
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
	echo "$STAMP Running auto-extraction of COVID-19 data from bmsgpk website"  >> $LOG_FILE
	/usr/bin/Rscript ./COVID-19-bmsgpk-extract.R 2>&1 >> $LOG_FILE
	git add $LOG_FILE
	git add ./html/*
	git add ./data/*
	git commit -m "$STAMP Auto-Extract data from bmsgpk website" 2>&1 >> $LOG_FILE
	git push 2>&1 >> $LOG_FILE
fi

if [ "$SECTION" == "info" ]
then
	cd $BASE_DIR/bmsgpk
	echo ""
	echo "$STAMP Running auto-dump of COVID-19 info from bmsgpk infosite"       >> $LOG_FILE
	CHROME="/opt/google/chrome/chrome"
	URL="https://info.gesundheitsministerium.at/"
	# FLAGS="--headless --disable-gpu --dump-dom  --blink-settings=imagesEnabled=false --remote-debugging-port=9222"
	FLAGS="--headless --disable-gpu --dump-dom"
	DMP_FILE="$BASE_DIR/bmsgpk/html/COVID-19-info.$DT.dmp"
	echo "$STAMP $CHROME $FLAGS $URL > $DMP_FILE"  >> $LOG_FILE
	$CHROME $FLAGS $URL > $DMP_FILE
fi


