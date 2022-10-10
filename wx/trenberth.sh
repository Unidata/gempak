#! /bin/bash -f
. /home/gempak/NAWIPS/Gemenviron.profile

# set dirs
# TOP=/home/gempak/scripts

# set date/time
YYYYMMDD=`date -u +%Y%m%d`
HHNN=`date -u +%H%M`
FTIME=f012
GFSdir=$MODEL/gfs
lastFile=`ls $GFSdir | grep gfs211 | tail -1`
GDFILE=$GFSdir/$lastFile
DATAAREA='20;-130;52;-50'
PROJ='str/90;-100;0'
TITLE='700 mb Vorticity / 1000-500 mb Thermal Wind'

DEV='gif|trenberth.gif|1024;768'
#DEV='xw|xw|900;600'

# 1) 700 mb vorticity color-filled contours
# 2) 500-1000 mb heights

gdcntr <<EOF1
 GDFILE   = $GDFILE
 GDATTIM  = $FTIME
 GLEVEL   = 700
 GVCORD   = pres
 PANEL    = 0
 SKIP     = 
 SCALE    = 5
 CTYPE    = f
 GFUNC    = avor(wnd)
 CONTUR   = 2
 CINT     = 2
 LINE     = 1/1
 FINT     = 10;12;14;16;18;20;22;24
 FLINE    = 101;21;22;23;5;19;17;16;15;5
 HILO     =
 HLSYM    =
 CLRBAR   = 31/V/LL/
 WIND     = 0
 REFVEC   =
 TITLE    = 31/-2/GFS ~
 TEXT     = 1/2//hw
 CLEAR    = n
 GAREA    = us
 IJSKIP   = 0
 PROJ     = $PROJ
 MAP      = 6
 MSCALE   = 0
 BND      = lakes/24
 LATLON   = 0
 DEVICE   = $DEV
 STNPLT   =
 SATFIL   = 
 RADFIL   =
 IMCBAR   =
 LUTFIL   = 
 STREAM   =
 POSN     = 0
 MARKER   = 0
 GRDLBL   = 0
 FILTER   = YES
r

 GLEVEL  = 500:1000
 GFUNC   = SUB(HGHT@500,HGHT@1000)
 CINT    = 60
 LINE    = 5/1/2
 SCALE   = 0
 CLRBAR  =
 TITLE   = 31/-1/ $TITLE 
 CONTUR  = 3
 SKIP    = 0/1
 FINT    = 70;90;110;130;150
 FLINE   = 0;25;24;29;7;15
 CTYPE   = c
r

e
EOF1


gdwind << EOF3
   GVECT   = thrm(hght)
   CTYPE   = 
   CINT    = 
   LINE    = 6/1/2
   CLEAR   = n
   WIND    = bk31/.5//2
   TEXT    = 
   CONTUR  = 0
   r

   e
EOF3

gpend
rm last.nts
rm gemglb.nts
