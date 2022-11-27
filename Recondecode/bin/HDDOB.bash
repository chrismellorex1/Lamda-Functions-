#!/bin/bash
#
#
#
#  This script reformats recon data for ingest into AWIPS2 as both an airmet and moving maritime observation
#  Mello 5/2018---- version 1  
#
#
#
workingdirectory="/localapps/runtime/Recondecode"
#finaloutputdir=/awips2/edex/data/manual
finaloutputdir=/data/fxa/LOCAL/recondecode/output
###########################################################################################3
touch /localapps/runtime/Recondecode/data/acars.touch
#rm /localapps/runtime/Recondecode/data/*acars*

date=`date -u "+%d%H%M"`
second=`date -u "+%S"`

echo $date > ${workingdirectory}/log/reconlog 
echo $1 >>  ${workingdirectory}/log/reconlog 

#
cd  $workingdirectory/bin
rm HDOB-observations.txt 
#
############     STEP ONE READ FILE   ###################################################################################
# 
#  The awips2 product trigger sends the product PIL as an argument when it gets ingested via noaaport feed  
#
/awips2/fxa/bin/textdb -r $1 > name
cat name ${workingdirectory}/log/reconlog
# gets  Universal format from the HDDOBS from P3 and aircraft recon 
#
grep --no-filename "[0-9][0-9][0-9][0-9]N [0-9][0-9][0-9][0-9][0-9]W" name > HDOB-observations.txt 

exec 3< HDOB-observations.txt
while read 0<&3 line; do

##########  PARSE THE DATA 

   # is it a noaa or airforce plane 

  noaatest=`grep -i noaa name | cut -c1-4`
  
  ssmr=`echo $line | cut -c58-60`
  lat=`echo $line | cut -c8-11`
  lon=`echo $line| cut -c14-18` 
  latmin=`echo $line | cut -c10-11` 
  lonmin=`echo $line| cut -c17-18`

  direction=`echo $line| cut -c47-49` 
  speed=`echo $line| cut -c54-56`
  bouyspeed=`echo $line| cut -c51-52` 
  time=`echo $line| cut -c1-4`  
  level=`echo $line| cut -c27-29`  
  if [ $level == '///' ];then
   level="999"
  fi
  obhour=`echo $line| cut -c1-2`  
  obminute=`echo $line| cut -c3-4`    


#  Convert meters to feet

  level=`echo "scale=0; ${level}*328084/1000000" |bc -l`
  level=`echo $level*1 | bc `
  finallevel=$level

#  Add proper zeros for airep format for level in feet  


      if [ `echo "${level} < 100" | bc` -eq 1 ];then
                finallevel="0${level}"
        fi
 
        if [ `echo "${level} < 10" | bc` -eq 1 ];then
                finallevel="00${level}"
        fi


  acarsextrappressurecheck=`echo $line| cut -c32`
  acarsextrappressure=`echo $line| cut -c33-34`
  acarsextrappressure="$acarsextrappressure"
 
   

 if [ $acarsextrappressurecheck == '0' ];then
 acarsextrappressure=`echo $line| cut -c33-34`
  fi
###

  temp=`echo $line| cut -c38-39`

  plane="NO${lonmin}${latmin}${second}"
  echo $noaatest 

  if [ -z $noaatest ]
   then
     plane="AF${lonmin}${latmin}${second}"
   fi

##### Part two reformat the data for surface moving maritime....

      labelday=`date '+%d'`
      labelday=${labelday}${obhour} 


####  Put the parts together for airmet at a higher elevations...view using upper air menu
      twohours="$obhour"
      case $obhour in
     00)
        twohours="00 23" 
        filelabel="002322"
        ;;  
      01)
        twohours="01 00"
        filelabel="002322"
        ;;
      02)
        twohours="02 01"
        filelabel="020100"
        ;;
      03)
        twohours="03 02"
        filelabel="030201" 
        ;;
      04)
        twohours="04 03"
        filelabel="040302"
        ;;
      05)
        twohours="05 04"
        filelabel="050403"
        ;;
      06)
        twohours="06 05"
        filelabel="060504"
        ;;
     07)
        twohours="07 06"
        filelabel="070605"
        ;;
     08)
        twohours="08 07"
        filelabel="880706"
        ;;
     09)
        twohours="09 08"
        filelabel="090807"
        ;;
     10)
        twohours="10 09"
        filelabel="100908"
        ;;
      11)
        twohours="11 10"
        filelabel="111009"
        ;;
      esac

      if [ $obhour -gt 11 ]; then
       minusonehour=`echo "$obhour - 1" | bc`
       minustwohour=`echo "$obhour - 2" | bc`
       minusthreehour=`echo "$obhour - 3" | bc`
       twohours="$obhour $minusonehour"

      filelabel="${minusonehour}${minustwohour}${minusthreehour}"
   fi


   if [ $ssmr == '///'  ]; then
      ssmr="000" 
      fi

     sign="PS"
  if [ $acarsextrappressure == '//' ]; then
       acarsextrappressure="99"
       sign="MS" 
      fi
       

      for loophour in ${obhour}
        do

 
         sed -e "s/plane/${plane}/g" -e "s/date/${date}/g" -e "s/lat/${lat}/g" -e "s/lon/${lon}/g" -e "s/time/${loophour}${obminute}/g" \
          -e "s/level/${ssmr}/g" -e "s/temp/${sign}${acarsextrappressure}/g" -e "s/direction/${direction}/g" -e "s/speed/${speed}/g"  airep.template > ${workingdirectory}/data/UANT01_CWAO_acars${date}${lat}${lon}${obhour}${filelabel}${loophour}


         sed -e "s/plane/${plane}/g" -e "s/date/${date}/g" -e "s/lat/${lat}/g" -e "s/lon/${lon}/g" -e "s/time/${loophour}${obminute}/g" \
          -e "s/level/${ssmr}/g" -e "s/temp/${sign}${acarsextrappressure}/g" -e "s/direction/${direction}/g" -e "s/speed/${speed}/g"  airep.template


         chmod 777 ${workingdirectory}/data/UANT01_CWAO_acars${date}${lat}${lon}${obhour}${filelabel}${loophour}
         cp ${workingdirectory}/data/UANT01_CWAO_acars${date}${lat}${lon}${obhour}${filelabel}${loophour} ${finaloutputdir} 

done


exec 5<&-
done
exec 3<&-





/localapps/runtime/Recondecode/bin/HDDOB-color.bash $1

scp /data/fxa/LOCAL/recondecode/output/*acars* /nsbn_store/drop_box/airep_ingest/
mv  /data/fxa/LOCAL/recondecode/output/*acars* /data/fxa/LOCAL/recondecode/archive

