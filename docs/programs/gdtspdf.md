# GDTSPDF

GDTSPDF draws contour or color fill of cumulative probability 
or probability density as a function of grid values and
forecast time at a selected point and level or layer.

### Input Parameters
 
    GPOINT  Grid point
    GDATTIM Grid date/time
    GVCORD  Grid vertical coordinate
    GLEVEL  Grid level
    GFUNC   Scalar grid
    GDFILE  Grid file
    PTYPE   Plot type/h:w ratio/margins
    PRBTYP  Probability type
    CTYPE   Contour type:  C/F
    TAXIS   Time1-time2-tinc;lbl;gln;tck
    YAXIS   Ystrt/ystop/yinc/lbl;gln;tck
    BORDER  Background color/type/width
    LINE    Color/type/width/label/smth/fltr/scflg
    CINT    Contour interval/min/max
    HILO    Color/symbol/rng/rad/cnt/intp
    HLSYM   HILO txt size/posn/font/wdth/hw
    FINT    Fill interval/min/max
    FLINE   Fill colors/fill types
    CLRBAR  Color/ornt/anch/x;y/ln;wd/freq|text_info
    TITLE   Title color/line/title
    CLEAR   Clear screen flag
    SCALE   Scalar scale / vector scale
    PANEL   Panel loc/color/dash/width/regn
    DEVICE  Device|name|x size;y size|color type
    TEXT    Size/fnt/wdth/brdr/N-rot/just/hw flg
    OUTPUT  Output device/filename
 


### Program Description
 
GDTSPDF displays contours or color fills of a time series of
probabilty at a fixed point and level or layer.  The probability
is computed from ensemble data.

GDFILE must specify an ensemble entry, a list of members
enclosed within curly brackets. 

GPOINT specifies the location for the probability.  
It may be entered as a latitude and longitude 
separated with a semicolon, as a station character 
identifier or station number, or as a set of x and 
y grid points separated with a semicolon and preceded 
with an `@`.  The location may also be selected 
graphically by setting GPOINT using the CURSOR
command.  The location of the time series is then 
selected by clicking on a horizontal map in another 
GEMPAK XW window.

The probabilities are computed as a function of GFUNC
values and forecast time, and plotted as contours or
color fills depending on user input CTYPE. If CTYPE 
is C, contour lines are drawn using input from CINT
and LINE.  If CTYPE is F, filled contours are drawn using
specifications from FINT and FLINE. Both contour lines and
filled contours are drawn if CTYPE is F/C.  The GFUNC values
are plotted along the veritcal axis(Y) and the forecast times
are plotted along the horizontal axis(X).
 
PRBTYP specifies the probability type. If it is 1, the
cumulative probability will be computed and plotted.  If it
is 2, the probability density will be plotted.  The default
value is 1.

The increment entered in the YAXIS parameter determines
the discretization for displaying the cumulative probability
and for computing the difference in the cumulative
distribution for displaying the density function.  The
beginning and ending values specified in the YAXIS
parameter determine the range of values for which
the cumulative probability or probability density is
to be displayed.  The label frequency may be used to
reduce the number of labeled tick marks when high
resolution of the distribution requires a small increment.
Default values are provided if not specified in YAXIS. 

Please note that SKEWT is not valid in vertical coordinate
system in this program.

 
### Examples

Plot the probability density contours of temperature at
    IAD 500 mb level for f24-f60 forecast hours, based on NAM 
    model output at 00, 06, 12z.

	GPOINT   = iad
 	GDATTIM  = f24-f60
	GVCORD   = PRES
 	GLEVEL   = 500
	GFUNC    = TMPC
	GDFILE   = {nam|00, nam|06, nam|12}
 	PTYPE    = lin
 	PRBTYP   = 2
 	CTYPE    = c
 	TAXIS    =  
 	YAXIS    =  
 	BORDER   = 1
 	LINE     = 2/2/3/1
 	CINT     = 10
 	HILO     = 2;4
 	HLSYM    = 2;1/3/2//HW
 	FINT     = 1
 	FLINE    = 1-20
 	CLRBAR   = 1
 	TITLE    = 1
 	CLEAR    = YES
 	SCALE    = 2
 	PANEL    = 0
 	DEVICE   = xw
 	TEXT     = 1/21//hw
 	OUTPUT   = T


### Error Messages
 
    [GDTSPDF  -1]    Fatal error initializing TAE.
    [GDTSPDF  -2]    Fatal error reading TAE parameters.
    [GDTSPDF  -3]    Fatal error initializing GEMPLT.
    [GDTSPDF  -5]    Invalid input for GDFILE.
    [GDTSPDF  -7]    Invalid input for PTYPE. 
    [GDTSPDF -11]    Input for YAXIS is invalid.
    [GDTSPDF -14]    Input for GPOINT is invalid.
