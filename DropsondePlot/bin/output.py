#!/usr/bin/python
import os
import sys
import datetime
#
#
#
site="NH2"
# Tropical sites Gridded TCM, GTCMAT1-5, only need D2DMODELS
if site == "NH2" or site == "ONA":
    
    for i in range(1, 5):
        for _type in ['','ocean']:
            m='GTCMAT%d%s' % (i, _type)
#            serverConfig.modelDict[m] = {'D2DMODELS': m, 'D2DDBVERSIONS': 5}   
            print m   
elif site == "SJU":
    
    for i in range(1, 5):
        m='GTCMAT%dpr' % (i)
#        serverConfig.modelDict[m] = {'D2DMODELS': m, 'D2DDBVERSIONS': 5}       
        print m  
else:

    for i in range(1, 5):
        m='GTCMAT%d' % (i)
        print m 
#        serverConfig.modelDict[m] = {'D2DMODELS': m, 'D2DDBVERSIONS': 5} 



#
