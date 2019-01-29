# SNHODO

SNHODO draws a hodograph of upper air data.

### Input Parameters
 
    SNFILE    Sounding data file
    AREA      Data area
    LINE      Color/type/width/label/smth/fltr/scflg
    MARKER    Marker color/type/size/width/hw
    BORDER    Background color/type/width
    TITLE     Title color/line/title
    XAXIS     Xstrt/xstop/xinc/lbl;gln;tck
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    LEVELS    Vertical levels
    VCOORD    Vertical coordinate type
    DATTIM    Date/time
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
 
 

### Program Description
 
SNHODO draws a hodograph which represents the vertical
distribution of the horizontal wind at a station.  The winds
are plotted in meters/sec.

The line color, line type, and width are specified in LINE.
The marker color, type, and size are specified in MARKER.
The levels at which the line will be labeled are specified
in LEVELS; VCOORD specifies the vertical coordinate for
LEVELS.

The x and y axis limits are specified in XAXIS and YAXIS.  If
these values are blank, the axes will be scaled to the actual
data.

 
### Examples
 
1.	Plot the hodograph for station CHH for the latest time in
the data set.  Label the hodograph every 100 mb from 1000 mb
to 100 mb.  Use hardware font 2 for the text and display
the hodograph in a XW window named snhodo.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        AREA	 =  @chh
        LINE	 =  3
        MARKER	 =  1
        BORDER	 =  1
        TITLE	 =  1
        XAXIS	 =
        YAXIS	 =
        LEVELS	 =  1000-100-100
        VCOORD	 =  pres
        DATTIM	 =  last
        CLEAR	 =  y
        DEVICE	 =  xw|snhodo
        PANEL	 =  0
        TEXT	 =  1/2/1/hw

2.	Plot the CHH hodograph using height coordinates.
Label every 2000 meters from 1000 meters to
24000 meters.  Draw the x and y axes from -10 to
40 meters/sec.
    
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        AREA	 =  @chh
        LINE	 =  3
        MARKER	 =  1
        BORDER	 =  1
        TITLE	 =  1
        XAXIS	 =  -10/40/10
        YAXIS	 =  -10/40/10
        LEVELS	 =  1000-24000-2000
        VCOORD	 =  hght
        DATTIM	 =  last
        CLEAR	 =  y
        DEVICE	 =  xw|snhodo
        PANEL	 =  0
        TEXT	 =  1/2/1/hw

### Error Messages
 
    [SNHODO  -1]    Fatal error initializing TAE.
    [SNHODO  -2]    Fatal error reading TAE parameters.
    [SNHODO  -3]    Fatal error initializing GEMPLT.
    [SNHODO  -4]    The input for XAXIS is invalid.
    [SNHODO  -5]    The input for YAXIS is invalid.
    [SNHODO  -6]    Winds cannot be computed.
    [SNHODO  -7]    The vertical coordinate ... cannot be computed.
    [SNHODO  -8]    The range along the x or y axis is invalid.
    [SNHODO  -9]    No winds can be plotted.
    [SNHODO -10]    No valid stations were found.
