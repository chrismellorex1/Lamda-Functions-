#!/usr/bin/python
import os
import sys
import datetime
#
#
#
#
workdirectory="/localapps/runtime/DropsondePlot/bin/"
datadirectory="/localapps/runtime/DropsondePlot/data/"
############################################
#
#
#
 

def file_read(fname):
        content_array = []
        with open(fname) as f:
                #Content_list is the list that contains the read lines.     
                for line in f:
                        content_array.append(line)
                return (content_array) 



currentfile=sys.argv[1]

command0="rm " + workdirectory + "inputfile"
command1="ln -s " + currentfile + " " + workdirectory + "inputfile" 
command2="cd " +  workdirectory
command3="touch fort.15"
command4="rm fort.15"
command5="cd " + workdirectory
command6=  workdirectory   + "dropsonde.x inputfile " 
command7= "cp " + currentfile + " " + workdirectory + "rawfile"

os.system(command0)
os.system(command1)
os.system(command2)
os.system(command3) 
os.system(command4)
os.system(command5)
os.system(command6)
os.system(command7)

now = datetime.datetime.now()
month="%d" % now.month
year="%d" % now.year
storm="storm"
type="DROP"

file_read('fort.15')

rawdatalist=file_read('rawfile')
n = len(rawdatalist)
for i in range(1,n):
    if i <= n:
      XXBBcheck=rawdatalist[i][0:4]
      OBcheck=rawdatalist[i][31:33]
      OBcheck1=rawdatalist[i][37:39]
      Centercheck=rawdatalist[i][6:12] 
      Eyewallcheck=rawdatalist[i][6:13]
      maxbandcheck=rawdatalist[i][6:14]
      if Centercheck == "CENTER":
        type="CENTER"
      if Eyewallcheck == "EYEWALL":
        type="EYEWALL" 
      if maxbandcheck == "MXWNDBND":
        type="MXWNDBND"
      if XXBBcheck == "XXBB":
        lat1=rawdatalist[i][14:16]        
        lat2=rawdatalist[i][16:17]
        lon1=rawdatalist[i][19:22]
        lon2=rawdatalist[i][22:23]
      if XXBBcheck == "XXAA":
         day=rawdatalist[i-1][12:14]
         hour=rawdatalist[i-1][14:16]
         minute=rawdatalist[i-1][16:18]
      if OBcheck == "OB" or OBcheck1 == "OB":
         storm=rawdatalist[i][18:23] 
         missionID=rawdatalist[i][11:17]
         OBnumber=rawdatalist[i][31:45]
    continue

finalproduct=""
print " "
print " "
header="SNDTYPE=NA;  TITLE=" + storm + " " + year + month + day +  "/" + hour + " (NHC) BUFRUA; STNID=KNHC; LAT=" + lat1 + "." + lat2 + "5280380249023; " + "LON=-" + lon1 + "." + lon2 + "639251708984;" + "\n"
header=header + "PRESSURE        HGHT    TEMP    DWPT    WDIR    WSPD    OMEG" + "\n" 

dropsondedata=file_read('fort.15')
length=len(dropsondedata)
firstheight=0


n = len(dropsondedata) 
validheight = []
mandatorylevel = []
pressurelevel = []
sortpressure  = []
mandatorylevel.append("start")
pressurelevel.append("start")
mandatory= 0
speedcheck=0
repeattest=0
pressure700=1
pressure850=1
donecheck700=0
donecheck850=0
donecheck925=0
invalid925=0
man925=0
countlevels=0
height700=2900
height850=1850
height925=120
n = len(dropsondedata)
mantest700=int(0)
mantest850=int(0)
previous850height=float(1000000) 
previous700height=float(1000000)
previous925height=float(1000000)
oldpressure="000"
minus2="001"
maxspeed=0

for i in range(1,n):
    if i <= n:
     MANTEST=dropsondedata[i][76:80]     
     checkpressurelevel=0

     if MANTEST == 'MANL':     
      mandatory=mandatory + 1
      mandatorylevel.append(dropsondedata[i][53:62])
      pressurelevel.append(dropsondedata[i][33:39])
    

      checkpressurelevel=pressurelevel[mandatory]
      checkpressurelevel=float(checkpressurelevel)
      if checkpressurelevel == 850.0 and donecheck850 == 0: 
        height850=mandatorylevel[mandatory] 
        current850height=float(height850) 
        if current850height < previous850height:
              height850=current850height                 
              previous850height=current850height
        temp=dropsondedata[i][40:46] 
        strcurrent850height=str(current850height) 
        sttemp=str(temp)   
        printss= temp + " " + strcurrent850height 
        
        donecheck850=1
      if checkpressurelevel == 700.0 and donecheck700 == 0:
        height700=mandatorylevel[mandatory]
        height700=mandatorylevel[mandatory]
        current700height=float(height700)
        if current700height < previous700height:
           height700=current700height
           previous700height=current700height
        donecheck700=1 
     if checkpressurelevel == 925.0 and donecheck925 == 0:
        height925=mandatorylevel[mandatory]
        height925=mandatorylevel[mandatory]
        current925height=float(height925)
        if current925height < previous925height:
           height925=current925height
           previous925height=current925height
        donecheck925=1
    continue



for i in range(1,n):
    if i <= n:
        list=dropsondedata[i]
        pressure=dropsondedata[i][33:39]
        pressure=float(pressure)
        hieght=dropsondedata[i][53:62]
        space1="     "  
        if pressure > 999:
           hieght="  -9999.0 "                   
        pressure=str(pressure)
        hieght=str(hieght)
        test1=1030  
        hieghtint=float(hieght)
        if hieghtint == -99:
          test1=pressure
          test1=float(test1)  
          if  test1 > 850:
            height850=float(height850)
            hieght=height850-(9.75*(test1-850))
          if  test1 < 850 and test1 > 700: 
           height850=float(height850) 
           height700=float(height700) 
           hieght=height700-(9.75*(test1-700))  
        if test1 < 925 and hieghtint < height925:
           invalid925=1 
        temp=dropsondedata[i][40:46]
        printtemp=temp        
        temp=float(temp) 
        if temp == -99:
           printtemp="-9999.00"  
        temp=str(temp)
        DP=dropsondedata[i][48:51]
        DP=int(DP)  
        if DP != -99:
          tempfloat=float(temp)
          DP= tempfloat - ((100 - DP)/5)
          DP=str(DP)
          space="    " 
        space2="      "
        if DP == -99:
           DP=str(DP) + "99.00" 
           space2="  " 

        DIR=dropsondedata[i][63:66]
        DIR=int(DIR)
        space4="     "
        if DIR != -99:
           DIR=DIR # +180
           space4="     "
        DIR=str(DIR) 

        speed=dropsondedata[i][70:73]
        speed=float(speed)
        if speed != -99:
           speed=float(speed)
           speed=speed * 1.94384
        speed=abs(speed)
        speed=str(speed)
        space3="     "

        hieghtint=float(hieght)
        if hieghtint == -99:
          hieght=-9999
          hieght=str(hieght)
        if hieghtint > -9999 and hieghtint < 0 : 
          hieght=-9999 
          hieght=str(hieght) 
        hieght=str(hieght)
        hieghttest=float(hieght)
        if hieghtint > 0:
         firstheight=1 
        testDIR=float(DIR)
        printDIR=DIR
        flagwind=0
        if testDIR == -99:
            printDIR="-9999.000000"  
            flagwind=1  

        currentline= pressure + "     " + hieght + space1 + printtemp +  space2  + DP + space3 + printDIR + space4 + speed + "  -9999.000000"
        speedcheck=float(speed)
        if speedcheck > maxspeed and speedcheck !=99:      
           maxspeed =  speedcheck
        speedcheck=abs(speedcheck)
        validspeed=1
        if speedcheck == 9999: 
           validspeed=0 
        
        pressurecheck=float(pressure)
        printrepeattest=str(repeattest)
        printpressurecheck=str(pressurecheck)
        if pressure700 == 0 and pressurecheck == 700.0:
         repeattest=0

        if pressure850 == 0 and pressurecheck == 850.0:
          repeattest=0
        if invalid925 == 1 and pressurecheck == 925.0:
           man925=1 

             
        fupressurecheck=str(pressurecheck)
        fuhieghtint=str(hieghtint) 
        fufirstheight=str(firstheight)
        fuspeedcheck=str(speedcheck)
        if fuspeedcheck == "99.0":
           speed="-9999.000000"
        furepeattest=str(repeattest)
        fuhieghttest=str(hieghttest)
        # had to kill spped check..some are good sounding 
          
        if  fupressurecheck == oldpressure:
           fufirstheight=float(firstheight)
           fupressurecheck=float(pressurecheck)
           fufirstheight=fufirstheight-1
           fupressurecheck=fupressurecheck+1 
           height=str(fufirstheight) 
           pressure=str(fupressurecheck)

        if  fupressurecheck == minus2:
           fufirstheight=float(firstheight)
           fupressurecheck=float(pressurecheck)
           fufirstheight=fufirstheight-2
           fupressurecheck=fupressurecheck+2
           height=str(fufirstheight)
           pressure=str(fupressurecheck)


        minus2=oldpressure 
        oldpressure=fupressurecheck
         
        finalproduct= finalproduct + pressure + "     " + hieght + space1 + printtemp +  space2  + DP + space3 + printDIR + space4 + speed + "  -9999.000000" + "\n"
        if pressurecheck < 1010:  
          sortpressure.append( pressure + "     " + hieght + space1 + printtemp +  space2  + DP + space3 + printDIR + space4 + speed + "  -9999.000000" + "\n") 
          countlevels=countlevels+1 
          firstheight=1
        if pressurecheck == 700.0:
           pressure700=0  
        if pressurecheck == 850.0:
           pressure850=0

        speedcheck=1
        repeattest=0
        man925=0 
	continue
  
  
    print i

finalproduct=header
n=countlevels
for i in range(1,n):
    if i <= n:
      finalproduct=finalproduct + sortpressure[n-i] 
    continue
print finalproduct
maxspeed=int(maxspeed + .5)
filenameout="Environmental-"
if maxspeed > 34:
  filenameout="Tropicalstorm-maxwind-" 
if maxspeed > 64:
  filenameout="Hurricane-maxwind-" 
if maxspeed > 96:
  filenameout="MajorHurricane-maxwind-" 
maxspeed=str(maxspeed)
storm.strip()
# missionID=rawdatalist[i][11:16]
#         OBnumber=rawdatalist[i][31:37]

filenameout="../soundings/"+storm+"-"+day+hour+minute+"-"+ missionID + "-" + OBnumber + "-" + type + "-" + lat1+"N-"+lon1+"W.nsp"
filenameout = ''.join(filenameout.split()) 
print filenameout
f = open(filenameout,'w')
print >>f, finalproduct     # Python 2.x
