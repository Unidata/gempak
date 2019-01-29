# GDTSER

GDTSER draws a time series of a scalar at a single level.

### Input Parameters
 
    GPOINT    Grid point
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GDFILE    Grid file
    PTYPE     Plot type/h:w ratio/margins
    TAXIS     Time1-time2-tinc;lbl;gln;tck
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    BORDER    Background color/type/width
    LINE      Color/type/width/label/smth/fltr/scflg
    MARKER    Marker color/type/size/width/hw
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    SCALE     Scalar scale / vector scale
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
 
 

### Program Description
 
GDTSER draws a time series of a scalar parameter at a
fixed level.  The variable may be any scalar computed by
the grid diagnostics package.

GPOINT specifies the location for the time series.  It may
be entered as a latitude and longitude separated with a
semicolon, as a station character identifier or station
number, or as a set of x and y grid points separated with a
semicolon and preceded with an @.  The location may also
be selected graphically by setting GPOINT using the CURSOR
command.  The location of the time series is then selected
by clicking on a horizontal map in another GEMPAK XW window.

The times to plot are specified in GDATTIM.  Only those
times specified will be plotted.  Note that the times
may represent a series of base times with the same or no
forecast time, or a series with the same base time and
a list of forecast times.

The time axis is specified in TAXIS using the usual GEMPAK
date/time conventions, including FIRST and LAST.  If
TAXIS has no increment, a reasonable value is chosen.
If TAXIS is blank, the limits are taken from the first and
last valid times set in GDATTIM.

If the user desires multiple lines on the same graph,
TAXIS and YAXIS should be explicitly set; then LINE, MARKER,
and TITLE can be varied for successive combinations of GLEVEL,
GVCORD, GPOINT, and GFUNC.

The axis type, height-to-width ratio of the plot and the
margins may be specified in PTYPE.

 
### Examples
 
1.  Plot the BWI 500 mb temperature for the all of the forecast
    times in the file.  Label the Y axis from -15 to 15 in
    increments of 3 degrees.  Label the time axis from
    18/21 to 20/03 in increments of 12 h.  Make the plot
    half as high as wide.

        GPOINT	 =  bwi
        GDATTIM	 =  all
        GLEVEL	 =  500
        GVCORD	 =  pres
        GFUNC	 =  tmpc
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        PTYPE	 =  lin/.5
        TAXIS	 =  18/21-20/03
        YAXIS	 =  -15/15/3
        BORDER	 =  1
        LINE	 =  3//3
        MARKER	 =  2/13/1.25/2
        TITLE	 =  5//^  _  @  #
        CLEAR	 =  yes
        SCALE	 =  0
        PANEL	 =  0
        DEVICE	 =  xw
        TEXT	 =  1

2.  Overlay the 700 mb temperature for all of the forecast
times in the file.  Use the above specifications for
the other parameters.

        GPOINT	 =  bwi
        GDATTIM	 =  all
        GLEVEL	 =  700
        GVCORD	 =  pres
        GFUNC	 =  tmpc
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        PTYPE	 =  lin/.5
        TAXIS	 =  18/21-20/03
        YAXIS	 =  -15/15/3
        BORDER	 =  1
        LINE	 =  2//3
        MARKER	 =  3/13/1.25/2
        TITLE	 =  5/-2/^  _  @  #
        CLEAR	 =  no
        SCALE	 =  0
        PANEL	 =  0
        DEVICE	 =  xw
        TEXT	 =  1

### Error Messages
 
    [GDTSER  1]    Some points missing for plot of ....
    [GDTSER  -1]    Fatal error initializing TAE.
    [GDTSER  -2]    Fatal error reading TAE parameters.
    [GDTSER  -3]    Fatal error initializing GEMPLT.
    [GDTSER  -4]    Input for GPOINT is invalid.
    [GDTSER  -5]    Input for GLEVEL is invalid.
    [GDTSER  -6]    Input for GVCORD is invalid.
    [GDTSER  -7]    Input for PTYPE is invalid.
    [GDTSER  -8]    Error defining graph coordinates.
    [GDTSER  -9]    No points found for plot of ....
    [GDTSER -11]    Input for ... in TAXIS is invalid.
    [GDTSER -12]    Time range is size zero.
    [GDTSER -13]    No grids found with times in GDATTIM.
