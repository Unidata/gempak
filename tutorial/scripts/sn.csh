#!/bin/csh                            

source /home/gempak/NAWIPS/Gemenviron  

set YMD=`date -u '+%Y%m%d'`          

set SFFILE=$GEMDATA/surface/${YMD}_sao.gem  

echo -n "Enter the station ID you which to list: "
set STATION=$<                        

sflist << EOF                        
 SFFILE   = $SFFILE                 
 AREA     = @${STATION}
 DATTIM   = all
 SFPARM   = tmpf;dwpf;pmsl;sped;drct
 OUTPUT   = t
 IDNTYP   = stid
 r

 e
EOF

echo " "
exit(0)
# END of shell script
#
