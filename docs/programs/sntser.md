# SNTSER

SNTSER draws a time series at a sounding station.

### Input Parameters
 
    SNFILE    Sounding data file
    DATTIM    Date/time
    TAXIS     Time1-time2-tinc;lbl;gln;tck
    LEVELS    Vertical levels
    VCOORD    Vertical coordinate type
    SNPARM    Sounding parameter list
    STNDEX    Stability indices
    AREA      Data area
    PTYPE     Plot type/h:w ratio/margins
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    BORDER    Background color/type/width
    LINE      Color/type/width/label/smth/fltr/scflg
    MARKER    Marker color/type/size/width/hw
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    DEVICE    Device|name|x size;y size|color type
 
 

### Program Description
 
SNTSER draws a time series plot for a sounding station.

Only one parameter may be plotted at a time.  This parameter
may be a level parameter defined in SNPARM or a stability
index defined in STNDEX.  If both SNPARM and STNDEX have
values, SNPARM will be used.  If a level parameter is
defined in SNPARM, a vertical level must also be set in
LEVELS.

The type of y axis can be set in PTYPE.  Either LIN or LOG
is valid.  The limits on the y axis can be set in YAXIS.

The times to plot are specified in DATTIM.  Only those
times specified will be plotted.  Lines will be drawn
connecting the data points, provided that no more than two
points are missing between segments.

The time axis is specified in TAXIS using the usual GEMPAK
date/time conventions, including FIRST and LAST.  If
TAXIS has no increment, a reasonable value is chosen.
If TAXIS is blank, the limits are taken from the first and
last valid times set in DATTIM.

When the user desires multiple lines on the same graph, TAXIS
and YAXIS should be explicitly set.  Then LINE, MARKER, and
TITLE can be varied for successive combinations of LEVELS,
VCOORD, AREA, and SNPARM.

The height-to-width ratio of the plot may be specified in
PTYPE following a slash.  If no value is entered, a value of
0.5 will be used.

 
### Examples
 
1.	Plot the relative humidity at the 850 mb level for
station CHH for all of the times in the data set.  Use
hardware font 2 for the text and plot the relative
humidity using a solid green line.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  all
        TAXIS	 =
        LEVELS	 =  850
        VCOORD	 =  pres
        SNPARM	 =  relh
        STNDEX	 =
        AREA	 =  @chh
        PTYPE	 =  lin
        YAXIS	 =
        BORDER	 =  1
        LINE	 =  3
        MARKER	 =  1
        TITLE	 =  1
        CLEAR	 =  y
        PANEL	 =  0
        TEXT	 =  1/2/1/hw
        DEVICE	 =  xw

2.	Plot the lifted index at CHH for the times between
18/00 and 20/00.  Plot the line with a thick short
dashed pattern.  Use asterisks for the markers.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  all
        TAXIS	 =  18/00-20/00
        LEVELS	 =  850
        VCOORD	 =  pres
        SNPARM	 =
        STNDEX	 =  lift
        AREA	 =  @chh
        PTYPE	 =  lin
        YAXIS	 =
        BORDER	 =  1
        LINE	 =  2/2/3
        MARKER	 =  3/12
        TITLE	 =  1
        CLEAR	 =  y
        PANEL	 =  0
        TEXT	 =  1/2/1/hw
        DEVICE	 =  xw

### Error Messages
 
    [SNTSER  +2]    WARNING, invalid PTYPE, LIN will be used.
    [SNTSER  +1]    WARNING, more than one parameter specified.
    [SNTSER  -1]    Fatal error initializing TAE.
    [SNTSER  -2]    Fatal error reading TAE parameters.
    [SNTSER  -3]    Fatal error initializing GEMPLT.
    [SNTSER  -4]    Input for LEVEL is invalid.
    [SNTSER  -5]    Error defining graph coordinates.
    [SNTSER  -6]    No points found for plot.
    [SNTSER  -7]    Session not interactive.
    [SNTSER  -8]    No parameters entered.
    [SNTSER  -9]    Parm ... not calculable.
    [SNTSER -10]    Parm ... is a character.
    [SNTSER -11]    PTYPE chosen is invalid.
