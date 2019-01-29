# GDTHGT

GDTHGT draws contours and wind barbs or arrows on a time section at a
	point within a grid.

### Input Parameters
 
    GPOINT    Grid point
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GVECT     Vector grid
    GDFILE    Grid file
    PTYPE     Plot type/h:w ratio/margins
    TAXIS     Time1-time2-tinc;lbl;gln;tck
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    BORDER    Background color/type/width
    LINE      Color/type/width/label/smth/fltr/scflg
    CINT      Contour interval/min/max
    WIND      Wind symbol/siz/wdth/typ/hdsz
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    SCALE     Scalar scale / vector scale
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
 
 

### Program Description
 
GDTHGT is a user contributed program that draws a
vertical time section at a location in a grid field.
The location is specified by setting GPOINT.  Gridded
data are interpolated to the time-section plane at regular
intervals of time specified in GDATTIM.

The grid vertical coordinate (GVCORD) may be PRES, THTA, or
HGHT, depending on what is available in the grid file.
The vertical axis scaling, set in PTYPE, can be LIN,
LOG, KAP or STUVE.  STUVE and KAP are the same; SKEWT may
not be entered.  The plot aspect ratio and margins may also be
entered in PTYPE.

Vector fields may be specified in GVECT and displayed as
arrows or barbs. For a wind vector field, the default is north
relative direction. A "/G" indicates grid relative direction.
The vector function CIRC cannot be computed for time sections.

If M is entered in WIND, winds will be displayed in meters
per second unless the KNOTV operator has been specified in
GVECT, in which case the winds will be displayed in knots.
IF K is entered in WIND, the wind is displayed in knots.


### Examples
 
1.  Plot temperature in Celsius on a log-P chart on a
    time section for all times at BWI.  Plot the contours
    in color number 2, using thick, solid lines.  Plot wind
    barbs for the forecast wind.  Plot the chart to 50
    millibars using the default labels.
    
        GPOINT	 =  bwi
        GDATTIM	 =  all
        GVCORD	 =  PRES
        GFUNC	 =  temp
        GVECT	 =  wnd
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        PTYPE	 =  log
        TAXIS	 =
        YAXIS	 =  /50
        BORDER	 =  1
        LINE	 =  2/2/3/1
        CINT	 =  5
        WIND	 =  bm1
        TITLE	 =  1
        CLEAR	 =  YES
        SCALE	 =  999
        PANEL	 =  0
        DEVICE	 =  xw
        TEXT	 =  1/21//hw

2.  Now, using the same file, plot a log pressure time
    section of the vorticity advection.  Positive values
are contoured with solid lines, negative values are
contoured with dashed lines. Plot the wind in grid
relative direction.

        GPOINT	 =  bwi
        GDATTIM	 =  all
        GVCORD	 =  PRES
        GFUNC	 =  adv(avor(obs);obs)
        GVECT	 =  wnd/G
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        PTYPE	 =  log
        TAXIS	 =
        YAXIS	 =  /50
        BORDER	 =  1
        LINE	 =  2/-2/3/1
        CINT	 =  5
        WIND	 =  bm1
        TITLE	 =  1
        CLEAR	 =  YES
        SCALE	 =  999
        PANEL	 =  0
        DEVICE	 =  xw
        TEXT	 =  1/21//hw

### Error Messages
 
    [GDTHGT  -1]   Fatal error initializing TAE.
    [GDTHGT  -2]   Fatal error reading TAE parameters.
    [GDTHGT  -3]   Fatal error initializing GEMPLT.
    [GDTHGT  -4]    Invalid vertical coordinate.
    [GDTHGT  -7]    Invalid axis type.
    [GDTHGT  -8]    Invalid vertical coordinate type.
    [GDTHGT -14]    Input for GPOINT is invalid.
    [GDTHGT -20]    Input for YAXIS is invalid.
