# GDMAP

GDMAP plots data from a scalar grid.

### Input Parameters
 
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GDFILE    Grid file
    GAREA     Graphics area
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    SKIP      Skip_cntr/skip_plt_x;skip_plt_y
    POSN      Position / Text format
    COLORS    Color list
    MARKER    Marker color/type/size/width/hw
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    CINT      Contour interval/min/max
    TITLE     Title color/line/title
    SCALE     Scalar scale / vector scale
    DEVICE    Device|name|x size;y size|color type
    PROJ      Map projection/angles/margins|drop flag
    CLEAR     Clear screen flag
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    GRDLBL    Grid point label color
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
 
 

### Program Description
 
GDMAP plots data computed from GEMPAK grid files on a map.
Data may be plotted in any valid GEMPAK projection and
may be overlaid on images.

A list of times may be given in GDATTIM allowing animation
of the data plots.

The variable, POSN, is used to select the position for the
data relative to the grid point using the following position
numbers:

				7
			1		3
			2	0	4
			5		6
				8

Position 0 will plot data centered at the station.  If an
invalid position number is entered, position 0 will be used.

The variable, SCALE, is used to scale the data by 10 ** SCALE.
The data will be scaled and then rounded to the nearest
integer before it is plotted.

SKIP specifies the number of grid points to skip when displaying
the data.  For example, SKIP = 1 displays data at every other
grid point in each coordinate direction.  SKIP = 0 displays all
points.

If GRDLBL is not 0, the grid point row and column numbers will be
displayed using the color number specified.  The grid rows and
columns will be centered on the plot area.

A map and title may also be included.

 
### Examples
 
1.  Plot the dewpoint depression at 850 mb in color 3 at
position 0.  The data will be plotted at every point
for an area centered on Nebraska.  The row and column
labels will be plotted in color 17.  The title will be
written in color 1.

        GDATTIM	 =  f18
        GLEVEL	 =  850
        GVCORD	 =  pres
        GFUNC	 =  sub(tmpc,dwpc)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  ne
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        SKIP	 =  0
        POSN	 =  0
        COLORS	 =  3
        MARKER	 =  0
        MAP	 =  1/7
        LATLON	 =  2/10/1/1/5;5
        PANEL	 =  0
        CINT	 =
        TITLE	 =  1/-3/~  @  DEWPOINT DEPRESSION
        SCALE	 =  0
        DEVICE	 =  xw
        PROJ	 =  nps
        CLEAR	 =  yes
        TEXT	 =  1
        GRDLBL	 =  17
        LUTFIL   =
        STNPLT   =

2.  Do not clear the screen and replot the same function
in color 7, for the same area.  The minimum value to
plot is set to 10 in CINT.  The text of the title
is changed to reflect the colored values.
    
        GDATTIM	 =  f18
        GLEVEL	 =  850
        GVCORD	 =  pres
        GFUNC	 =  sub(tmpc,dwpc)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  ne
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        SKIP	 =  0
        POSN	 =  0
        COLORS	 =  7
        MARKER	 =  0
        MAP	 =  1/7
        LATLON	 =  2/10/1/1/5;5
        PANEL	 =  0
        CINT	 =  /10
        TITLE	 =  1/-2/(MAGENTA >10; GREEN <10)
        SCALE	 =  0
        DEVICE	 =  xw
        PROJ	 =  nps
        CLEAR	 =  no
        TEXT	 =  1
        GRDLBL	 =  17
        LUTFIL   =
        STNPLT   =

### Error Messages
 
    [GDMAP  +2]     WARNING:  ... not found.  CONTINUING---
    [GDMAP  -1]     Fatal error initializing TAE.
    [GDMAP  -2]     Fatal error reading TAE parameters.
    [GDMAP  -3]     Fatal error initializing GEMPLT.
