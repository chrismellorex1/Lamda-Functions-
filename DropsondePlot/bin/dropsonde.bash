#!/bin/bash
#
# Script reads current version of text dropsinde data in AWIPS2 and stores as text file. 
# It then calls PYTHON/FORTRAN script that reads the data and converts to proper format
# for D2D to plot.  

SITE="MIA"
workdirectory="/localapps/runtime/DropsondePlot/bin"
finaloutputdir="/data/fxa/LOCAL/recondecode/output"
######################
cd ${workdirectory} 

/awips2/fxa/bin/textdb -r $1 > ${SITE}REPNT3
./dropsonde.py  ${workdirectory}/${SITE}REPNT3


####### Create Surface wind grid from dropsonde points 

dir=`grep XXAA rawfile | cut -c43-45`
speed=`grep XXAA rawfile | cut -c46-47`

lat1=`grep SPG rawfile |tail -1 | rev | cut -d " " -f3 | rev  | cut -c1-2`
lat2=`grep SPG rawfile |tail -1 | rev | cut -d " " -f3 | rev  | cut -c3-4`
lat2=`echo "scale=0;  $lat2 *  6 /10 "  | bc  `

       if [ `echo "${lat2} < 10" | bc` -eq 1 ];then
               lat2="0${lat2}"
        fi

lat="${lat1}${lat2}"

lon1=`grep SPG rawfile  | tail -1 | rev |  cut -d " " -f3 | rev  | cut -c6-8`
lon2=`grep SPG rawfile  | tail -1 | rev |  cut -d " " -f3 | rev  | cut -c9-10`


lon2=`echo "scale=0;  $lon2 *  6 /10 "  | bc  `

     if [ `echo "${lon2} < 10" | bc` -eq 1 ];then
               lon2="0${lon2}"
        fi

lon="${lon1}${lon2}"    

echo "lat"
echo "$lat" 

echo "lon"
echo "$lon1 $lon2"

echo "dir"
echo $dir

hour=`grep XXAA rawfile | cut -c9-10`
pressure=`grep XXAA rawfile | cut -c34-35`

echo "$hour $lat $lon $dir $speed"
date=`date -u "+%d%H%M"`
minute=`date -u "+%M"`
second=`date -u "+%S"`
plane="SUR$hour$minute$second"

      sed -e "s/plane/${plane}/g" -e "s/date/${date}/g" -e "s/lat/${lat}/g" -e "s/lon/${lon}/g" -e "s/time/${hour}${minute}/g" \
          -e "s/level/001/g" -e "s/temp/PS${pressure}/g" -e "s/direction/${dir}/g" -e "s/speed/${speed}/g"  airep.template 

#sed -e "s/plane/${plane}/g" -e "s/date/${date}/g" -e "s/lat/${lat}/g" -e "s/lon/${lon}/g" -e "s/time/${hour}${minute}/g" \
#          -e "s/level/001/g" -e "s/temp/PS${pressure}/g" -e "s/direction/${dir}/g" -e "s/speed/${speed}/g"  airep.template > /data/fxa/LOCAL/UANT01_CWAO_acars${date}${lat}${lon}${hour}${minute}

 sed -e "s/plane/${plane}/g" -e "s/date/${date}/g" -e "s/lat/${lat}/g" -e "s/lon/${lon}/g" -e "s/time/${hour}${minute}/g" \
          -e "s/level/001/g" -e "s/temp/PS${pressure}/g" -e "s/direction/${dir}/g" -e "s/speed/${speed}/g"  airep.template > ${finaloutputdir}/UANT01_CWAO_acars${date}${lat}${lon}${hour}${minute} 


scp /data/fxa/LOCAL/recondecode/output/*acars* /nsbn_store/drop_box/airep_ingest/
mv  /data/fxa/LOCAL/recondecode/output/*acars* /data/fxa/LOCAL/recondecode/archive

