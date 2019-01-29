# GDSTREAM

GDSTREAM draws streamlines through a vector grid.

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
    LINE      Color/type/width/label/smth/fltr/scflg
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    LATLON    Line color/dash/width/freq/inc/label/format
    TITLE     Title color/line/title
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    DEVICE    Device|name|x size;y size|color type
    PROJ      Map projection/angles/margins|drop flag
    CLEAR     Clear screen flag
    WIND      Wind symbol/siz/wdth/typ/hdsz
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
    STREAM    lines/arrows/stop/slow/scale
 
 

### Program Description
 
GDSTREAM draws streamlines through any vector grid computed
by the grid diagnostics package. The vector grid is specified
in GVECT.

Streamlines are plotted in an animation sequence by specifying
more than one time in GDATTIM.

The line color, dash pattern and line width are set in LINE.
A map, title and latitude/longitude lines may also be included.
The size of the arrow heads may be controlled by specifying
the fifth value in WIND.

The STREAM parameter controls several parameters dealing with
the overall streamline calculation and display.  "Lines" is a
real number multiplier which controls the number (density) of
streamlines drawn (default 1.0); "arrows" is a real number
multiplier which controls the number of arrowheads displayed
(default 1.5*lines); "stop" is a real number multiplier which
controls how close a streamline comes to another streamline
before it is stopped being drawn (default 0.5); "slow" is a
real number multiplier which controls the minimum vector speed
threshold for stopping a streamline (default 0.67); "scale" is
a real number multiplier which controls how much the input vector
field is scaled prior to streamline calculation (default 0.33).


### Examples
 
1.  Draw streamlines through the gridded wind field at 850
    mb using color 3.  Add a North Polar Stereographic map
in color 1 and line type 7 and a title in color 5.
Center the graphics area on Ohio.
    
        GDATTIM	 =  f24
        GLEVEL	 =  850
        GVCORD	 =  pres
        GVECT	 =  wnd
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  oh-
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        LINE	 =  3//2
        MAP	     =  1/7
        LATLON	 =  2/10/1/1/5;5
        TITLE	 =  5
        PANEL	 =  0
        TEXT	 =  1
        DEVICE	 =  xw
        PROJ	 =  nps
        CLEAR	 =  yes
        WIND	 =
        LUTFIL   =
        STNPLT   =

2.  Similar to 1), except for a global 1x1 degree dataset over the
Pacific Ocean, specifying the STREAM parameter to properly
thin and display the field given the overall density of
grid boxes.

        GDFILE   = $MODEL/fnl/fnl_96052800
        GDATTIM	 = f00
        GLEVEL	 = 250
        GAREA    = -25;-180;55;-45
        PROJ     = mer
        LATLON   = 31/12/2//10
        STREAM   = 0.4/0.8/3//
        WIND     = am1/2/3//0.8
        TITLE    = 31/-2/@ FNL ANALYSIS VALID ~ NHC MIAMI FLA

3.  Clear the screen and draw the streamlines of the thermal
    wind.  Make the arrowheads 0.75 of the default size.
Plot the streamlines in a second window named "window2".

        GDATTIM	 =  f24
        GLEVEL	 =  500:850
        GVCORD	 =  pres
        GVECT	 =  thrm
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  oh-
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        LINE	 =  3//2
        MAP	     =  1/7
        LATLON	 =  2/10/1/1/5;5
        TITLE	 =  5
        PANEL	 =  0
        TEXT	 =  1
        DEVICE	 =  xw|window2
        PROJ	 =  nps
        CLEAR	 =  yes
        WIND	 =  an3////.75
        LUTFIL   =
        STNPLT   =

### Error Messages
 
    [GDSTREAM  +2]  WARNING:  ... not found.  CONTINUING---
    [GDSTREAM  -1]  Fatal error initializing TAE.
    [GDSTREAM  -2]  Fatal error reading TAE parameters.
    [GDSTREAM  -3]  Fatal error initializing GEMPLT.

