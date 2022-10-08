# ACPROF

ACPROF draws profiles of ACARS data

### Input Parameters
 
    SFFILE    Surface data file
    DATTIM    Date/time
    AREA      Data area
    SNPARM    Sounding parameter list
    LINE      Color/type/width/label/smth/fltr
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
 
ACPROF draws profiles of ACARS data. The program is derived from
[SNPROF](snprof) to use ACARS data stored in ship format files. If AREA
is given as a single tail number (eg @FSL0001) then only data from
that aircraft will be used. If AREA is given as a region (eg DEN)
then all aircraft in vicinity of that location will be used.
AREA can be expanded/contracted as usual with +,*/-. All observations
in the given time range will be used. Once data is obtained, program
behavior is similar to SNPROF.

Any two parameters that can be computed from the data set can
be plotted.  These parameters are specified in SNPARM.
The profiles' colors, line types, and widths are
specified in LINE.  Markers identifying the data points
can be plotted by setting a color in MARKER. A different MARKER
number is assigned to each aircraft. If marker numbers overlap, the
marker color is incremented.

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
 
1.  Plot a Skew T chart using ACARS observations in the vicinity
    of DEN.

        SFFILE   = acars.gem
        DATTIM   = all
        AREA     = den
        SNPARM   = tmpc;dwpc
        LINE     = 2;4
        PTYPE    = skewt
        VCOORD   = PRES
        STNDEX   =
        STNCOL   = 1
        WIND     = BM1
        WINPOS   = 1
        MARKER   = 0
        BORDER   = 1
        TITLE    = 1
        DEVICE   = XW
        YAXIS    =
        XAXIS    =
        FILTER   = N
        CLEAR    = YES
        PANEL    = 0
        TEXT     = 1
        THTALN   = 8/1/1
        THTELN   = 6/10/1
        MIXRLN   = 5/2/1

### Error Messages
 
    [ACPROF  +6]    Background lines cannot be drawn.
    [ACPROF  +5]    Stability indicies are specified with color = 0
    [ACPROF  +4]    Parameter ... was requested with color set to 0.
    [ACPROF  +3]    Winds cannot be computed.
    [ACPROF  +2]    Parameter ... is a character type.
    [ACPROF  +1]    Parameter ... cannot be computed.
    [ACPROF  -1]    Fatal error initializing TAE.
    [ACPROF  -2]    Fatal error reading TAE parameters.
    [ACPROF  -3]    Fatal error initializing GEMPLT.
    [ACPROF  -4]    The plot type ... is invalid.
    [ACPROF  -5]    The the x-axis range must be specified in XAXIS.
    [ACPROF  -6]    The two parms use different temperature units.
    [ACPROF  -7]    The vertical coordinate ... cannot be computed.
    [ACPROF  -8]    The range along the x or y axis is invalid.
    [ACPROF  -9]    No valid stations were found.
    [ACPROF -10]    SNPARM has not been specified.

