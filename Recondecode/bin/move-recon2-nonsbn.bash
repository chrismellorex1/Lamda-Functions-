#!/bin/bash
#
#  This script converts does two functions 
#  1. moves airep ingest to non sbn ingest point   
#
#   Mello
#
######################################################################
#
#  Move recon data to cpsbn2 non-sbn entry point.
#

################################################################################
lasthour=`date '+%Y%m%d_%H' -d '1 hour ago'`
currenthour=`date '+%Y%m%d_%H' -d '0 hour ago'`
datetime=`date '+%Y%m%d/%H'`

echo $datetime
#  Send Recon decode to non SBN
#

scp /data/fxa/LOCAL/recondecode/output/*acars* awpdbnet@cpsbn2:/home/awpdbnet/data/airep_ingest
mv  /data/fxa/LOCAL/recondecode/output/*acars* /data/fxa/LOCAL/recondecode/archive 

#
# Send raw airplane (non-recon) reports from SBN to NON-SBN for backup ingest
#
scp /data_store/airep/${datetime}/*  awpdbnet@cpsbn2:/home/awpdbnet/data/airep_ingest
rm /data_store/airep/${datetime}/*

