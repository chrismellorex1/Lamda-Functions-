#!/bin/bash

### SET DEBUG TO "-v" FOR LOG OUTPUT OR "" FOR NOTHING:
DEBUG="-v"

#######################################################################
# PROGRAM LOGIC:   DO NOT EDIT BELOW THIS LINE
#######################################################################

if [[ $DEBUG == "-v" ]]
then
	DATE=$(date +%Y%m%d)
	logfile=/data/logs/ldad/${DATE}/RECON.log
	echo "----------------------------------------------------" >> $logfile
	date                                                        >> $logfile
	echo "----------------------------------------------------" >> $logfile
	echo "Calling Args: $@"                                     >> $logfile
else
	logfile=/dev/null
fi

BASEDIR='/awips2/edex/data/manual/'
mv $1 /awips2/edex/data/manual/  


if [[ $DEBUG == "-v" ]]
then
	echo "INFILE   = $INFILE"   >> $logfile
	echo "BASENAME = $BASENAME" >> $logfile
	echo "DIR      = $DIR"      >> $logfile
	echo "FILENAME = $FILENAME" >> $logfile
	echo "DESTDIR  = $DESTDIR"  >> $logfile
	echo "DESTFILE = $DESTFILE" >> $logfile
fi

if [[ ! -d $DESTDIR ]]
then
	mkdir -vp $DESTDIR >> $logfile 2>&1
	chmod 775 $DESTDIR
fi




