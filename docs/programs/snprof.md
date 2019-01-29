# SNPROF

SNPROF draws profiles of upper air data.

### Input Parameters
 
    SNFILE    Sounding data file
    DATTIM    Date/time
    AREA      Data area
    SNPARM    Sounding parameter list
    LINE      Color/type/width/label/smth/fltr/scflg
    PTYPE     Plot type/h:w ratio/margins
    VCOORD    Vertical coordinate type
    STNDEX    Stability indices
    STNCOL    Stability index color
    WIND      Wind symbol/siz/wdth/typ/hdsz
    WINPOS    Wind position
    MARKER    Marker color/type/size/width/hw
    BORDER    Background color/type/width
    TITLE     Title color/line/title
    DEVICE    Device|name|x size;y size|color type
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    XAXIS     Xstrt/xstop/xinc/lbl;gln;tck
    FILTER    Filter data factor
    CLEAR     Clear screen flag
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    THTALN    THTA color/dash/width/mn/mx/inc
    THTELN    THTE color/dash/width/mn/mx/inc
    MIXRLN    MIXR color/dash/width/mn/mx/inc
 
 

### Program Description
 
SNPROF draws profiles of upper air data.

Any two parameters that can be computed from the data set can
be plotted.  These parameters are specified in SNPARM.
The profiles' colors, line types, and widths are
specified in LINE.  Markers identifying the data points
can be plotted by setting a color in MARKER.

Profiles are plotted in an animation sequence if more than
one station and/or time is specified in AREA and DATTIM,
respectively.

The type of y axis is specified in PTYPE.  LIN, LOG, STUVE,
and SKEW are all valid.  Note that SKEW is only valid when
plotting temperature vs. pressure.  The bottom and top of
the axis, along with an increment for labels, are set in
YAXIS.  If the panel is changed to be less than the full
screen, the default margins will often be too large.  The
margins can be set explicitly in PTYPE.

Winds are plotted in the right margin and can be specified in
WIND.  There are three positions available for plotting winds.
They are numbered 1 to 3, with 1 being the leftmost position.
The location for the wind to be plotted is specified in WINPOS.
The positions also apply to the station identifier, date/time
and stability indices which are plotted above the diagram.

Dry adiabats, moist adiabats and mixing ratio background
lines can be added to the diagram.  The attributes of these
lines are specified in THTALN, THTELN, and MIXRLN.

If the color is set to 0, no lines will be plotted.  If the
start, stop and increment are not set, defaults will be
supplied.  Note that these lines can only be drawn on plots
of pressure versus temperature.

FILTER in this program determines whether the wind barbs are
filtered.


### Examples
 
1.  Plot a Skew T chart for HAT for all of the times in the
file.  Display the Showalter index, the bulk Richardson
number, CAPE, the Lifted index and the K index.  Plot TMPC
in color 2 using a solid line with width of 3.  Plot DWPC
in color 3 using a dashed line with width of 3.  Include
theta, theta-e, and mixing ratio lines.  Plot wind barbs
in color 6 in position 1.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  all
        AREA	 =  @hat
        SNPARM	 =  tmpc;dwpc
        LINE	 =  2;3/1;3/3
        PTYPE	 =  skewt
        VCOORD	 =  pres
        STNDEX	 =  show;brch;cape;lift;kinx
        STNCOL	 =  1
        WIND	 =  bm6//2
        WINPOS	 =  1
        MARKER	 =
        BORDER	 =  1
        TITLE	 =  1
        DEVICE	 =  xw
        YAXIS	 =  ///;1
        XAXIS	 =  -40/40/10/;1
        FILTER	 =  no
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        THTALN	 =  8/3/1
        THTELN	 =  23/1/1
        MIXRLN	 =  16/10/2

 2. Plot relative humidity with a logarithmic pressure axis.
    Plot the data from the surface to 200 mb. The theta,
theta-e, and mixing ratio lines are turned off, and no
stability indices are computed.
    
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  all
        AREA	 =  @hat
        SNPARM	 =  relh
        LINE	 =  3/1/3
        PTYPE	 =  log
        VCOORD	 =  pres
        STNDEX	 =
        STNCOL	 =  1
        WIND	 =  0
        WINPOS	 =  1
        MARKER	 =
        BORDER	 =  1
        TITLE	 =  1
        DEVICE	 =  xw
        YAXIS	 =  1050/200
        XAXIS	 =  0/100/20
        FILTER	 =  no
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        THTALN	 =
        THTELN	 =
        MIXRLN	 =

3.  Plot equivalent potential temperature on the x axis vs.
    potential temperature on the y axis.  Display the data
for the first time in the data file for HAT, ALB and BNA.
Set the X axis to range from 310 to 360 Kelvin in increments
of 10. Also, set the Y axis to ranged from 280 to 350
Kelvin in increments of 10.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        DATTIM	 =  first
        AREA	 =  @hat;alb;bna
        SNPARM	 =  thte
        LINE	 =  3/1/3
        PTYPE	 =  lin
        VCOORD	 =  thta
        STNDEX	 =
        STNCOL	 =  1
        WIND	 =  0
        WINPOS	 =  1
        MARKER	 =
        BORDER	 =  1
        TITLE	 =  1
        DEVICE	 =  xw
        YAXIS	 =  280/350/10
        XAXIS	 =  310/360/10
        FILTER	 =  no
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        THTALN	 =
        THTELN	 =
        MIXRLN	 =

### Error Messages
 
    [SNPROF  +6]    Background lines cannot be drawn.
    [SNPROF  +5]    Stability indicies are specified with color = 0
    [SNPROF  +4]    Parameter ... was requested with color set to 0.
    [SNPROF  +3]    Winds cannot be computed.
    [SNPROF  +2]    Parameter ... is a character type.
    [SNPROF  +1]    Parameter ... cannot be computed.
    [SNPROF  -1]    Fatal error initializing TAE.
    [SNPROF  -2]    Fatal error reading TAE parameters.
    [SNPROF  -3]    Fatal error initializing GEMPLT.
    [SNPROF  -4]    The plot type ... is invalid.
    [SNPROF  -5]    The the x-axis range must be specified in XAXIS.
    [SNPROF  -6]    The two parms use different temperature units.
    [SNPROF  -7]    The vertical coordinate ... cannot be computed.
    [SNPROF  -8]    The range along the x or y axis is invalid.
    [SNPROF  -9]    No valid stations were found.
    [SNPROF -10]    SNPARM has not been specified.
