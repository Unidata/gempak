# GDPTPDF

GDPTPDF draws cumulative probability or probability density
at selected point and time at a single level.

### Input Parameters
 
    GPOINT    Grid point
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GLEVEL    Grid level
    GFUNC     Scalar grid
    GDFILE    Grid file
    LINE      Color/type/width/label/smth/fltr/scflg
    MARKER    Marker color/type/size/width/hw
    BORDER    Background color/type/width
    PTYPE     Plot type/h:w ratio/margins
    PRBTYP    Prob type
    SCALE     Scalar scale / vector scale
    XAXIS     Xstrt/xstop/xinc/lbl;gln;tck
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    TITLE     Title color/line/title
    PANEL     Panel loc/color/dash/width/regn
    CLEAR     Clear screen flag
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    DEVICE    Device|name|x size;y size|color type
    OUTPUT    Output device/filename

 

### Program Description
 
GDPTPDF draws the probabilty at a fixed point, time 
and level.  The probability is computed from the 
ensemble data at each grid point.

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

The probabilities of GFUNC are computed and plotted 
along the horizontal axis(X), and the values are plotted 
along the vertical axis(Y).

PRBTYP specifies the probability type. If it is 1,
the cumulative probability will be computed and plotted.
If it is 2, the probability density will be plotted.	
Default value is 1.

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
 
1.  Plot the cumulative probabilities of temperature at
    IAD 500 mb level for 24 forecast hours, based on GFS
    model output at 00, 06, 12z.

        GPOINT	 =  iad
        GDATTIM	 =  f24
        GLEVEL	 =  500
        GVCORD	 =  pres
        GFUNC	 =  tmpc
        GDFILE	 =  {gfs|00, gfs|06, gfs|12} 
        LINE     = 4/1;2/4/
        MARKER   = 0
        BORDER   = 1
        PTYPE    = lin
        PRBTYP   =  
        SCALE    = 0 
        XAXIS    =  
        YAXIS    =  
        TITLE    = 2/-1
        PANEL    = 0
        CLEAR    = YES
        TEXT     = 1.3/23/hw
        DEVICE   = XW
        OUTPUT   = t
    

### Error Messages
 
    [GDPTPDF  -1]    Fatal error initializing TAE.
    [GDPTPDF  -2]    Fatal error reading TAE parameters.
    [GDPTPDF  -3]    Invalid vertical coordinate 
    [GDPTPDF  -4]    Input for GPOINT is invalid.
    [GDPTPDF  -5]    Input for GDATTIM is invalid.
    [GDPTPDF  -7]    Input for PTYPE is invalid.
    [GDPTPDF  -8]    Error defining graph coordinates.
    [GDPTPDF -10]    Invalid data.
    [GDPTPDF -11]    Input ... for YAXIS is invalid.
    [GDPTPDF -12]    Input ... for XAXIS is invalid.
