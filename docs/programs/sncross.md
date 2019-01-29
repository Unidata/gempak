# SNCROSS

SNCROSS draws cross sections through sounding data.

### Input Parameters
 
    CXSTNS    Cross-section station line
    SNPARM    Sounding parameter list
    SNFILE    Sounding data file
    DATTIM    Date/time
    VCOORD    Vertical coordinate type
    PTYPE     Plot type/h:w ratio/margins
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    TAXIS     Time1-time2-tinc;lbl;gln;tck
    LINE      Color/type/width/label/smth/fltr/scflg
    BORDER    Background color/type/width
    CINT      Contour interval/min/max
    WIND      Wind symbol/siz/wdth/typ/hdsz
    TITLE     Title color/line/title
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    CLEAR     Clear screen flag
    FILTER    Filter data factor
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CURVE     Curve fit type
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    CONTUR    Subbox/smooth
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    CTYPE     Contour type:  C/F
 
 

### Program Description
 
SNCROSS draws cross sections using upper-air sounding data.
The cross-section line, CXSTNS, must be specified as a list
of stations separated with semicolons.  Alternatively, a
single station may be entered in CXSTNS and a list of times
in DATTIM.  In this case, a time section will be drawn.

Any parameter that can be computed from the data set
parameters can be displayed.  The parameter to be displayed
is specified in SNPARM.  If the value of SNPARM is ISEN,
isentropes will be drawn.  Note that SNPARM may also be
THTA, in which case, potential temperature will be gridded
and contoured.  ISEN will fit splines to the station data
in plot space and then check for tangled lines and untangle
them if necessary.

Contours may be displayed as lines or as a color fill.
If CTYPE is C, contour lines are drawn using input from CINT
and LINE.  If CTYPE is F, filled contours are drawn using
specifications from FINT and FLINE. Both contour lines and
filled contours are drawn if CTYPE is F/C.

The attributes of the contour lines, including the color,
line type, line width, and label frequency are specified
in LINE.  The four attributes are separated with slashes;
semicolons separate the values for each attribute.  If the
line type is set to a single negative number, negative
contour values will have the absolute value of the line type
and positive values will be solid.  If the label type is set
to a single number, n, then every nth value will be labeled.

The contour fill intervals are specified in FINT; the attributes
for the fill are specified in FLINE.  The first color specified
in FLINE fills values less than the first level; while the
last color fills values greater than the last level.  Therefore,
n levels require n+1 colors.  CLRBAR allows a color bar to be
added for color fill contours.

A range of colors may be specified in either FLINE or LINE by
specifying starting, ending and increment values in that order
separated by dashes.  If the increment is missing, a default
of 1 is used.

The fill type may be set to 1 (solid), 2 (slanted dash) or 3
(slanted line).  If fill type is set to 0, solid fill is used.
If the fill type is set to a single negative number, negative
values will use the absolute value of the fill type, and positive
values will be solid.

The plot background consists of a pressure axis, a horizontal
axis with the station names, a filled region indicating the
part of the plot below the earth surface, and vertical lines
at each station.  The station lines are the specified line type
up to the level at which wind data stop, and are dotted from
there to the level at which temperature data stop.  The color
and other attributes for the background axes and labels are
given by the first numbers separated by semicolons in the color
number, line type and line width entry sections for BORDER.  The
second set of numbers applies to the station lines, and the third
set to the underground color fill (for which the line type and
width do not apply).  To eliminate, the background, the station
lines, or the underground fill, just set the corresponding color
number to zero.  If one color number is entered, it is used for
all three; if two are entered, the second is used for both the
station lines and the underground fill.  So, BORDER has the
following entries:

     background color; station line color; underground fill color /
     background line type; station line type /
     background line width; station line width

The horizontal axis represents a straight line between the
first and last stations.  The positions of intervening stations
are proportional to the perpendicular projections of the actual
positions onto the section line.  All of these calculations
are done in lat/lon coordinates.  If the plot is a time section,
the times will be displayed on the x axis with the earliest
time at the left.  If the first character in TAXIS is an R, the
earliest time will appear on the right.

The vertical coordinate may be specified as LIN, LOG, or STUVE;
SKEWT is not valid in this program.  The bottom and top limits
for the y axis are specified in YAXIS, but the axis labeling
specifications are ignored.

 
### Examples
 
1.	Draw isentropes in color 3 for the last time in the
data set.  Plot the background in color 1.  Use a
logarithmic scale for the y axis and a 5 degree interval
for the isentropes.  Draw wind barbs in color number
6 with line width 2.

        CXSTNS	 =  chh;acy;wal;hat;chs;ays
        SNPARM	 =  thta
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  last
        VCOORD	 =  pres
        PTYPE	 =  log
        YAXIS	 =
        TAXIS	 =
        LINE	 =  3
        BORDER	 =  1
        CINT	 =  5
        WIND	 =  bm6//2
        TITLE	 =  1
        PANEL	 =  0
        DEVICE	 =  xw
        CLEAR	 =  y
        FILTER	 =  .8
        TEXT	 =  1
        CURVE	 =  2
        CLRBAR	 =
        CONTUR	 =  0
        FINT	 =  0
        FLINE	 =  10-20
        CTYPE	 =  C

2.	Draw filled contours of relative humidity for the same
cross section.  Fill in contours greater than 70%.  Draw
a horizontal color bar at the bottom of the plot.  Draw
the cross section from 1050 mb to 300 mb.

        CXSTNS	 =  chh;acy;wal;hat;chs;ays
        SNPARM	 =  relh
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  last
        VCOORD	 =  pres
        PTYPE	 =  log/.5
        YAXIS	 =  1050/300
        TAXIS	 =
        LINE	 =  3
        BORDER	 =  1
        CINT	 =  5
        WIND	 =  bm6//2
        TITLE	 =  1
        PANEL	 =  0
        DEVICE	 =  xw
        CLEAR	 =  y
        FILTER	 =  .8
        TEXT	 =  1
        CURVE	 =  2
        CLRBAR	 =  1/h
        CONTUR	 =  0
        FINT	 =  70;80;90
        FLINE	 =  0;21;22;23
        CTYPE	 =  f

3.	Draw a time section of relative humidity for ACY.
Reverse the time axis to plot latest time on the left.

        CXSTNS	 =  acy
        SNPARM	 =  relh
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  all
        VCOORD	 =  pres
        PTYPE	 =  log/.5
        YAXIS	 =  1050/300
        TAXIS	 =  r
        LINE	 =  3
        BORDER	 =  1
        CINT	 =  5
        WIND	 =  bm6//2
        TITLE	 =  1
        PANEL	 =  0
        DEVICE	 =  xw
        CLEAR	 =  y
        FILTER	 =  .8
        TEXT	 =  1
        CURVE	 =  2
        CLRBAR	 =  1/h
        CONTUR	 =  0
        FINT	 =  70;80;90
        FLINE	 =  0;21;22;23
        CTYPE	 =  f

### Error Messages
 
    [SNCROSS  -1]   Fatal error initializing TAE.
    [SNCROSS  -2]   Fatal error reading TAE parameters.
    [SNCROSS  -3]   GEMPLT initialization error.
    [SNCROSS  -4]   Vertical coordinate for isentropes must be PRES.
    [SNCROSS  -5]   There are no times in the file.
    [SNCROSS  -6]   Fewer than four stations/times were selected.
    [SNCROSS  -7]   Data buffer is too small.
    [SNCROSS  -8]   Temperature or pressure data not available.
    [SNCROSS  -9]   The station ... cannot be found in the data set.
    [SNCROSS -10]   Error setting up graph; check invalid LOG axis.
    [SNCROSS -11]   Input ... for PTYPE is invalid.
    [SNCROSS -12]   Either input ... for YAXIS or input for VCOORD is invalid.
    [SNCROSS -13]   Parameter ... is not computable.
    [SNCROSS -14]   Parameter ... is a character.
    [SNCROSS -15]   The grid coordinates cannot be defined.
    [SNCROSS -16]   Multiple station entry is invalid for time sections.
    [SNCROSS -17]   Data at time ... is not in the file.
    [SNCROSS -18]   Wind data cannot be computed.
