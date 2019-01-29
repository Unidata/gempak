# GPANOT

GPANOT will allow the user to place objects at any location on the
current graphic device.

### Input Parameters
 
    GDFILE    Grid file
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    PROJ      Map projection/angles/margins|drop flag
    GAREA     Graphics area
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    CLEAR     Clear screen flag
    SHAPE     Object to draw
    INFO      Shape information
    LOCI      Point(s) associated with SHAPE
    LINE      Color/type/width/label/smth/fltr/scflg
    CTYPE     Contour type:  C/F
 
 

### Program Description
 
This program will plot an object at any location on the graphic
device. It uses Normal, Map and Grid coordinates. See the help
file on the specific parameters, since the definitions change
for each shape specified.

 
### Examples
 
1.  Plot the text string "This is plot 1" centered at normal
coordinates .5;.5, in color 15, with a filled box of
color 25 around the text.
    
        DEVICE  = xw
        PANEL   = 0
        SHAPE   = text
        INFO    = 1/3////c/hw/This is plot 1
        LOCI    = .5;.5
        LINE    = 15///25
        CTYPE   = f

2.	Plot a square centered at grid location 40;23, with radius
2 grid points, and rotated 45 degrees. Use color 8 for the
unfilled square, and use line type 5.

        SHAPE   = regpoly
        INFO    = 5/2/4/45
        LOCI    = @40;23
        LINE    = 8
        CTYPE   = c

### Error Messages
 
    [GPANOT  -1]    Fatal error initializing TAE.
    [GPANOT  -2]    Fatal error reading TAE parameters.
    [GPANOT  -3]    Fatal error initializing GEMPLT.
    [GPANOT  -4]    No symbol specified.
    [GPANOT  -5]    No front type specified.
    [GPANOT  -6]    Must have at least 2 pips.
