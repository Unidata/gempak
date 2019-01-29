# GDWIND

GDWIND displays a vector grid using wind barbs or arrows.

### Input Parameters
 
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GVECT     Vector grid
    GDFILE    Grid file
    GAREA     Graphics area
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    SKIP      Skip_cntr/skip_plt_x;skip_plt_y
    WIND      Wind symbol/siz/wdth/typ/hdsz
    REFVEC    Mag;x;y;txtsiz/font/wdth/HW;labl
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    TITLE     Title color/line/title
    DEVICE    Device|name|x size;y size|color type
    PROJ      Map projection/angles/margins|drop flag
    CLEAR     Clear screen flag
    SCALE     Scalar scale / vector scale
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
 
 

### Program Description
 
GDWIND draws wind barbs or arrows at each grid point of a
vector grid.  Vector plots can be drawn for any vector field
computed using the GEMPAK grid diagnostic functions.  Vectors
may be drawn in any valid GEMPAK projection and may be
overlaid on images.

A list of times may be given in GDATTIM allowing animation
of the plots.

SKIP specifies the number of points to skip in both coordinate
directions.  For example, `SKIP = /1;2` will display winds at every
other grid point in the x direction and every third grid point in
the y direction.  `SKIP = /0` displays winds at every grid point.
Wind barbs or arrows can also be staggered by specifying negative
values for SKIP.

IF M is entered as part of the wind symbol, winds will be
displayed in m/s, unless the KNOTV operator has been specified
in GVECT, in which case the winds will be displayed in knots.
If K is entered in WIND, the wind is displayed in knots.

 
### Examples
 
1.  Draw wind barbs in m/s in a staggered array.  The vector
    field is the gridded wind at 850 mb for the 18 hour forecast.
The barbs are plotted as an overlay on the entire visible
satellite image for 18Z.
    
        GDATTIM	 =  f18
        GLEVEL	 =  850
        GVCORD	 =  pres
        GVECT	 =  wnd
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  dset
        SATFIL	 =  $GEMPAK/data/VIS_910819_1801
        RADFIL	 =
        IMCBAR   =  1/V/LL/0;.05/.90
        SKIP	 =  /-1
        WIND	 =  bk6/1.5/3
        REFVEC	 =  10
        MAP	     =  1/7
        LATLON	 =  2/10/1/1/5;5
        PANEL	 =  0
        TITLE	 =  1
        DEVICE	 =  xw
        PROJ	 =  sat
        CLEAR	 =  yes
        SCALE	 =
        TEXT	 =  1
        LUTFIL   =
        STNPLT   =

2.  Using the values of the variables supplied above, change
the GAREA to Rhode Island.  The satellite image is subset
for the given area and the data is replotted.
    
        GDATTIM	 =  f18
        GLEVEL	 =  850
        GVCORD	 =  pres
        GVECT	 =  wnd
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  ri
        SATFIL	 =  $GEMPAK/data/VIS_910819_1801
        RADFIL	 =
        IMCBAR   =  1/V/LL/0;.05/.90
        SKIP	 =  0
        WIND	 =  bk6/1.5/3
        REFVEC	 =  10
        MAP	     =  1/7
        LATLON	 =  2/10/1/1/5;5
        PANEL	 =  0
        TITLE	 =  1
        DEVICE	 =  xw
        PROJ	 =  sat
        CLEAR	 =  yes
        SCALE	 =
        TEXT	 =  1
        LUTFIL   =
        STNPLT   =

3.  Plot arrows for the temperature gradient at 850 mb.  Scale the
data by 10**6.  Also, plot a reference vector with the label
"degrees C / meter".  The display area is centered on New York.
    
        GDATTIM	 =  f18
        GLEVEL	 =  850
        GVCORD	 =  pres
        GVECT	 =  grad(tmpc)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  ny-
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        SKIP	 =  0
        WIND	 =  am7//3
        REFVEC	 =  10;;;;degrees C / meter
        MAP	     =  1/7
        LATLON	 =  2/10/1/1/5;5
        PANEL	 =  0
        TITLE	 =  1
        DEVICE	 =  xw
        PROJ	 =  nps
        CLEAR	 =  yes
        SCALE	 =  /6
        TEXT	 =  1
        LUTFIL   =
        STNPLT   =

### Error Messages
 
    [GDWIND  +2]    WARNING:  ... not found.  CONTINUING---
    [GDWIND  -1]    Fatal error initializing TAE.
    [GDWIND  -2]    Fatal error reading TAE parameters.
    [GDWIND  -3]    Fatal error initializing GEMPLT.
    [GDWIND -13]    No times in grid file.
