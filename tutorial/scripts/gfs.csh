#! /bin/csh 
# Script for plotting 500mb height forecasts from current 
# day's ETA model to a GIF file.   Can be run from cron
# 
#
# IF run from cron, make sure an X server is available 
source /home/gempak/NAWIPS/Gemenviron
set GIFFILENAME=GFS_500mb.gif
#
## Define current date
set date=`date -u '+%Y%m%d'`
#
# Define the data file name. Naming convention: YYMMDDHH_eta.gem
#
set gridfile=$MODEL/gfs/${date}00_gfs003.gem
set gridfile=$MODEL/gfs/2010102500_gfs003.gem
#
# Run GDCNTR and generate the GIF file
#
gdcntr << GDCNTR_EOF 

GDFILE	= ${gridfile}
GDATTIM	= F00
DEVICE	= gf|${GIFFILENAME}||||NP
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 1
MAP     = 1
CLEAR	= yes
CLRBAR  = 0
GAREA   = us
PROJ	= STR/90;-100;0
LATLON	= 0
GLEVEL	= 500
GVCORD	= pres
GFUNC	= hght
SCALE	= 0
CINT	= 60
LINE	= 6/1/3
TITLE	= 6/-1
FINT	= 
FLINE	=
CTYPE	= c
HLSYM   = 2;1.5//21//hw
HILO    = 6;2/H#;L#////y 
\$RESPOND = YES
\$MAPFIL = hipowo.gsf
run

exit
GDCNTR_EOF

gdwind << gdwind_eof

GDATTIM= F00
GLEVEL = 500
GVCORD = pres 
GVECT  = WND  
SKIP   = /2;2                                       
WIND   = bk2                                       
TITLE  = 2/-2                                 
CLEAR = NO
run

\$MAPFIL = mepowo.gsf
exit
gdwind_eof

#
## Run GPEND to clean up
gpend
#
#exit(0)
