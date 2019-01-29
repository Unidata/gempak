# GDPROF

GDPROF draws profiles of a scalar grid and/or a vector grid.

### Input Parameters
 
    GPOINT    Grid point
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GVECT     Vector grid
    GDFILE    Grid file
    LINE      Color/type/width/label/smth/fltr/scflg
    MARKER    Marker color/type/size/width/hw
    BORDER    Background color/type/width
    PTYPE     Plot type/h:w ratio/margins
    SCALE     Scalar scale / vector scale
    XAXIS     Xstrt/xstop/xinc/lbl;gln;tck
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    WIND      Wind symbol/siz/wdth/typ/hdsz
    REFVEC    Mag;x;y;txtsiz/font/wdth/HW;labl
    WINPOS    Wind position
    FILTER    Filter data factor
    TITLE     Title color/line/title
    PANEL     Panel loc/color/dash/width/regn
    CLEAR     Clear screen flag
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    DEVICE    Device|name|x size;y size|color type
    OUTPUT    Output device/filename
    THTALN    THTA color/dash/width/mn/mx/inc
    THTELN    THTE color/dash/width/mn/mx/inc
    MIXRLN    MIXR color/dash/width/mn/mx/inc
 
 

### Program Description
 
GDPROF draws vertical profiles at a point.  Data from each
level in the grid file are interpolated to the point.

GPOINT specifies the location for the profile.  It may be
entered as a latitude and longitude separated with a semicolon,
as a station character identifier or station number, or as a
set of x and y grid points separated with a semicolon and
preceded with an `@`.  The profile location may also be selected
graphically with the CURSOR command.  The location of the
profile is then selected by clicking on a horizontal map in
another GEMPAK XW window.

Profiles are plotted in an animation sequence by specifying more
than one time in GDATTIM.

The vertical coordinate, set in GVCORD, can be PRES, THTA, or
HGHT.  The vertical axis type, set in PTYPE, can be LIN,
LOG, KAP, or SKEW.  Only temperatures vs. pressure may be
plotted on a skew t plot.  The height-to-width ratio of the
plot may be specified in PTYPE following a `/`.  If no value is
entered, a value of 0 will be used.

GFUNC is computed at every level in the data set.  No errors
are returned if the function cannot be computed at any level,
except at the surface.  If data at the surface cannot be
computed, a warning message is written and plotting continues.
GVECT specifies a vector to be plotted in the right margin.

If GFUNC is a temperature or dewpoint temperature, background
lines of potential temperature, equivalent potential
temperature and mixing ratio can be displayed by specifying
THTALN, THTELN, and MIXRLN.  If these background lines are
requested and GFUNC is not a temperature, an error message
is generated.

GVECT specifies a vector grid to be displayed as arrows or
barbs in one of three display columns on the right of the
profile plot. For a wind vector field, the default is north
relative direction. A `/G` indicates grid relative direction.
The number of the column is specified in WINPOS.

If M is entered in WIND, winds will be displayed in m/s unless
the KNOTV operator has been specified in GVECT, in which case the
winds will be displayed in knots.  If K is entered in WIND,
the wind is displayed in knots.

 
### Examples
 
1.  Plot temperature in Celsius on a log P chart at IAD for
    the latest time in the file.  Label the Y axis from 1000 mb
    to 100 mb in increments of 100 mb.  Label the X axis from
    -40 degrees to +40 degrees in increments of 10 degrees.  Add
    all the background lines.  Plot wind barbs in the right margin.

        GPOINT	 =  iad
        GDATTIM	 =  last
        GVCORD	 =  pres
        GFUNC	 =  tmpc
        GVECT	 =  wnd
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        LINE	 =  2//3
        MARKER	 =  5//1.25
        BORDER	 =  1
        PTYPE	 =  skewt
        SCALE	 =  0
        XAXIS	 =  -40/40/10
        YAXIS	 =  1000/100/100/;1
        WIND	 =  bm6//2
        REFVEC	 =  10
        WINPOS	 =  1
        FILTER	 =  no
        TITLE	 =  1/3
        PANEL	 =  0
        CLEAR	 =  yes
        TEXT	 =  1
        DEVICE	 =  xw
        OUTPUT	 =  n
        THTALN	 =  8/3/1
        THTELN	 =  23/1/1
        MIXRLN	 =  16/10/2

2.  Overlay the dewpoint for IAD. Do not redraw the background
lines. Plot the wind in grid relative direction.
    
        GPOINT	 =  iad
        GDATTIM	 =  last
        GVCORD	 =  pres
        GFUNC	 =  dwpc
        GVECT	 =  wnd/G
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        LINE	 =  3//3
        MARKER	 =  5//1.25
        BORDER	 =  1
        PTYPE	 =  skewt
        SCALE	 =  0
        XAXIS	 =  -40/40/10
        YAXIS	 =  1000/100/100/;1
        WIND	 =  0
        REFVEC	 =  10
        WINPOS	 =  1
        FILTER	 =  no
        TITLE	 =  0
        PANEL	 =  0
        CLEAR	 =  no
        TEXT	 =  1
        DEVICE	 =  xw
        OUTPUT	 =  n
        THTALN	 =  0
        THTELN	 =  0
        MIXRLN	 =  0

3.  Clear the screen and plot the vorticity at BWI in
    color number 2 using a dashed line.  Do not plot any
    winds.  Have the graph plot in a second window named
"profile2".
    
        GPOINT	 =  bwi
        GDATTIM	 =  last
        GVCORD	 =  pres
        GFUNC	 =  vor(wnd)
        GVECT	 =
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        LINE	 =  2/2/3
        MARKER	 =  0
        BORDER	 =  1
        PTYPE	 =  log
        SCALE	 =  5
        XAXIS	 =
        YAXIS	 =  1000/300/100
        WIND	 =
        REFVEC	 =  10
        WINPOS	 =  1
        FILTER	 =  no
        TITLE	 =  1/3
        PANEL	 =  0
        CLEAR	 =  yes
        TEXT	 =  1
        DEVICE	 =  xw|profile2
        OUTPUT	 =  t
        THTALN	 =  0
        THTELN	 =  0
        MIXRLN	 =  0

### Error Messages
 
    [GDPROF  +2]   WARNING:  ... not found.  CONTINUING---
    [GDPROF  +1]    Invalid parameter for background lines.
    [GDPROF  -1]    Fatal error initializing TAE.
    [GDPROF  -2]    Fatal error reading TAE parameters.
    [GDPROF  -3]    Fatal error initializing GEMPLT.
    [GDPROF  -4]    Input for GPOINT is invalid.
    [GDPROF  -5]    Input for GDATTIM is invalid.
    [GDPROF  -6]    Input for GVCORD is invalid.
    [GDPROF  -7]    Input for PTYPE is invalid.
    [GDPROF  -8]    Error defining graph coordinates.
    [GDPROF  -9]    No points found for profile.
    [GDPROF -10]    There are no levels at this time.
    [GDPROF -11]    Input ... for YAXIS is invalid.
    [GDPROF -12]    Input ... for XAXIS is invalid.
