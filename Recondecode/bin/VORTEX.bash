#!/bin/bash
#
# Chris mello---recon plot version one 
# 
# This file converts vortex message into an airep observation format for display in AWIPS2 D2D 
#
################################

workingdirectory=/localapps/runtime/Recondecode
#finaloutputdir=/awips2/edex/data/manual
finaloutputdir=/data/fxa/LOCAL/recondecode/output

##############################33
cd ${workingdirectory}/bin 
date=`date -u "+%d%H%M"`
/awips2/fxa/bin/textdb -r $1 > vortexfile 

### Parse out the lat and lon

lat=`  grep 'deg N' vortexfile | cut -c4-5`
latmin=`  grep 'deg N' vortexfile | cut -c7-8`

lon=` grep 'deg W' vortexfile | cut -c16-18`
lonmin=` grep 'deg W' vortexfile | cut -c20-21`





         templonmin=`echo "scale=2; ${lonmin}*.60" |bc -l | cut -c1-2`
         templatmin=`echo "scale=2; ${latmin}*.60" |bc -l | cut -c1-2`


          case $latmin in
     00)
        templatmin="00"
        ;;
      01)
        templatmin="00"
        ;;
      02)
        templatmin="01"
        ;;
      03)
        templatmin="01"
        ;;
      04)
        templatmin="02"
        ;;
      05)
        templatmin="03"
        ;;
      06)
        templatmin="03"
        ;;
     07)
        templatmin="04"
        ;;
     08)
        templatmin="04"
        ;;
     09)
        templatmin="05"
        ;;
     10)
        templatmin="06"
        ;;
      11)
        templatmin="06"
        ;;

      12)
        templatmin="07"
        ;;

      13)
        templatmin="07"
        ;;

      14)
        templatmin="08"
        ;;

      15)
        templatmin="09"
        ;;

     16)
        templatmin="09"
        ;;

      esac



       case $lonmin in
     00)
        templonmin="00"
        ;;
      01)
        templonmin="00"
        ;;
      02)
        templonmin="01"
        ;;
      03)
        templonmin="01"
        ;;
      04)
        templonmin="02"
        ;;
      05)
        templonmin="03"
        ;;
      06)
        templonmin="03"
        ;;
     07)
        templonmin="04"
        ;;
     08)
        templonmin="04"
        ;;
     09)
        templonmin="05"
        ;;
     10)
        templonmin="06"
        ;;
      11)
        templonmin="06"
        ;;

      12)
        templonmin="07"
        ;;

      13)
        templonmin="07"
        ;;

      14)
        templonmin="08"
        ;;

      15)
        templonmin="09"
        ;;

     16)
        templonmin="09"
        ;;

      esac


latmin=$templatmin
lonmin=$templonmin


acarslat=$lat$latmin
acarslon=$lon$lonmin


### Sometimes there are reporting syntax  errors  ..fixed the ones we tested with for 2017

sed -e "s/\/ 1 KT/\/ 01 KT/g" -e "s/\/ 2 KT/\/ 02 KT/g" -e "s/\/ 3 KT/\/ 03 KT/g" -e "s/\/ 4 KT/\/ 04 KT/g" -e "s/\/ 5 KT/\/ 05 KT/g" -e "s/\/ 6 KT/\/ 06 KT/g" \
-e "s/\/ 7 KT/\/ 07 KT/g" -e "s/\/ 8 KT/\/ 08 KT/g" -e "s/\/ 9 KT/\/ 09 KT/g"  vortexfile > temp
mv temp  vortexfile 

speed=` grep "CNTR DROPSONDE SFC WIND [0-9][0-9][0-9] / " vortexfile | cut -c31-32`
direction=`grep "CNTR DROPSONDE SFC WIND [0-9][0-9][0-9] / " vortexfile | cut -c25-27`

#
#  If not reporting winds needs to be given a dafault value or will be null 
#

if [ -z $direction ]
     then
     speed="00"
     direction="000"
     echo  "missing surface data"
fi

newspeed="00"
newdirection="000"

if [ $speed -lt 10 ]
     then
     newspeed="${speed}"
     newdirection=$direction
     else
     newspeed=${speed}
     newdirection=$direction
 fi

#### easy step...just reporting hour 

hour=`grep "A. [0-9][0-9]/[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z" vortexfile | cut -c7-8`
minute=`grep "A. [0-9][0-9]/[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z" vortexfile | cut -c10-11`
time="${hour}${minute}"

# sometimes there was an extra EXTRAP that made a null chracter...will remove  

sed  's/EXTRAP //g' vortexfile > temp
mv temp vortexfile 


#### Get the pressure

mslp=`grep "D\. [0-9][0-9][0-9]" vortexfile | cut -c4-7`
bouymslp=`grep "D\. [0-9][0-9][0-9]" vortexfile | cut -c3-5`
testbouymslpgt1000=`grep "D\. [0-9][0-9][0-9]" vortexfile | cut -c5`

#### If greater than 1000 mb need to change the cut command 

bouytest=`echo  $bouymslp | cut -c1 `

 if [ $testbouymslpgt1000 -lt "1" ]
        then
          bouymslp=`grep "D\. [0-9][0-9][0-9]" vortexfile | cut -c6-7`
          templabel="PS${bouymslp}" # will use temp slot to 
          bouymslp="0${bouymslp}0"

        else

         bouymslp=`grep "D\. [0-9][0-9][0-9]" vortexfile | cut -c5-6`
         templabel="PS${bouymslp}" 
         bouymslp="9${bouymslp}0"
       fi

labelday=`echo $time | cut -c1-2`



currentday=`date '+%d'`
date=`date -u "+%d%H%M"`

acarspressure=`echo ${bouymslp} | cut -c1-2`
obhour=$hour

### acars  


    sed -e "s/plane/VORTE${minute}/g" -e "s/date/${date}/g" -e "s/lat/ ${acarslat}/g" -e "s/lon/${acarslon}/g" -e "s/time/${obhour}${minute}/g" \
        -e "s/level/010/g" -e "s/temp/${templabel}${temp}/g" -e "s/direction/${newdirection}/g" -e "s/speed/${newspeed}/g"  vortex-airep.template # > ${workingdirectory}/data/UANT01_CWAO_.${date}${lat}${lon}VORTEX


    sed -e "s/plane/VORTE${minute}/g" -e "s/date/${date}/g" -e "s/lat/ ${acarslat}/g" -e "s/lon/${acarslon}/g" -e "s/time/${obhour}${minute}/g" \
        -e "s/level/010/g" -e "s/temp/${templabel}${temp}/g" -e "s/direction/${newdirection}/g" -e "s/speed/${newspeed}/g"  vortex-airep.template > temp
        mv temp "${workingdirectory}/data/UANT01_CWAO_acars-Vortex-${obhour}${minute}" 

      cp ${workingdirectory}/data/UANT01_CWAO_acars-Vortex-${obhour}${minute} ${finaloutputdir}   
  echo "$1 $acars $mslp ARP"


exec 5<&-
