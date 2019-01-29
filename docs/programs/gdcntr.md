# GDCNTR

GDCNTR draws contour lines through a scalar grid.

### Input Parameters
 
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GDFILE    Grid file
    CINT      Contour interval/min/max
    LINE      Color/type/width/label/smth/fltr/scflg
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    TITLE     Title color/line/title
    DEVICE    Device|name|x size;y size|color type
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    PROJ      Map projection/angles/margins|drop flag
    GAREA     Graphics area
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
    CLEAR     Clear screen flag
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    SCALE     Scalar scale / vector scale
    LATLON    Line color/dash/width/freq/inc/label/format
    HILO      Color/symbol/rng/rad/cnt/intp
    HLSYM     HILO txt size/posn/font/wdth/hw
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    CONTUR    Subbox/smooth
    SKIP      Skip_cntr/skip_plt_x;skip_plt_y
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    CTYPE     Contour type:  C/F
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
 
 

### Program Description

GDCNTR draws contour lines through a scalar grid computed
using the GEMPAK grid diagnostic functions.  Contours may be
drawn in any valid GEMPAK projection and may be overlaid on
images.

A list of times may be given in GDATTIM allowing animation
of the contours.

Contours may be plotted in different display windows by specifying
a name for the XW driver in DEVICE.

Contours may be displayed as lines or as a color fill.
If CTYPE is C, contour lines are drawn using input from CINT
and LINE.  If CTYPE is F, filled contours are drawn using
specifications from FINT and FLINE. Both contour lines and
filled contours are drawn if CTYPE is F/C.

The attributes of the contour lines, including the color,
line type, line width, and label frequency are specified
in LINE.  The four attributes must be separated with slashes;
semicolons separate the values for each attribute.  If the
line type is set to a single negative number, negative
contour values will have the absolute value of the line type
and positive values will be solid.  If the label frequency
is set to a single number, n, then every nth value will be
labeled.

CINT specifies the contour interval, minimum value, and maximum
value separated with slashes.  A scaling factor may be entered
in SCALE. The data in the grid file will be multiplied by
10 ** SCALE before the contour levels are selected.  If no
contour interval is entered, a default value which will generate
5 to 10 contour levels will be selected.

The contour fill intervals are specified in FINT; the attributes
for the fill are specified in FLINE.  The first color specified
in FLINE fills values less than the first level; while the
last color fills values greater than the last level.  Therefore,
n levels require n+1 colors.

A range of colors may be specified in either FLINE or LINE by
specifying starting, ending and increment values in that order
separated by dashes.  If the increment is missing, a default
of 1 is used.

The fill type may be set to 1 (solid), 2 (slanted dash) or 3
(slanted line).  If fill type is set to 0, solid fill is used.
If the fill type is set to a single negative number, negative
values will use the absolute value of the fill type, and positive
values will be solid.

The HILO and HLSYM parameters control marking and labeling the
values of relative maxima and minima.  CLRBAR allows a color bar
to be added for color fill contours.

The SKIP parameter specifies the number of grid points to skip
in generating contours.  For example, if SKIP = 1, every other
point is used to generate the contours.

 
### Examples
 
1.  Draw contours every 2 degrees through the 700 mb
    temperature field for all of the forecast times.  Negative
    values will be dashed using line type 3; every other
    contour line will be labeled; all the contour lines will be
    drawn in color number 3. The display area will be a enlarged
    Mercator map centered on New York.  The contours are drawn on
    a clear screen with a map in dash pattern 7, dotted lat/lon
    lines every 5 degrees and no title.

        GDATTIM	 =  all
        GLEVEL	 =  700
        GVCORD	 =  pres
        GFUNC	 =  tmpc
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        CINT	 =  2
        LINE	 =  3/-3/1/2
        MAP	     =  1/7
        TITLE	 =  0
        DEVICE	 =  xw
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        PROJ	 =  mer
        GAREA	 =  ny-
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        SCALE	 =  999
        LATLON	 =  2/10/1/1/5;5
        HILO	 =
        HLSYM	 =
        CLRBAR	 =
        CONTUR	 =  3
        SKIP	 =  0
        FINT	 =  0
        FLINE	 =  10-20
        CTYPE	 =  C
        LUTFIL   =
        STNPLT   =

2.  Now overlay dewpoint lines on the above plots. The contour
    interval is set to 5.  The lines will be drawn in color 17,
    with labeling and line types as above.  The map and lat/lon
    lines will not be drawn for the overlay.  The title, which
    includes the date, level and the string "TEMPERATURE AND
    DEWPOINT", is added in color 1.
    
        GDATTIM	 =  all
        GLEVEL	 =  700
        GVCORD	 =  pres
        GFUNC	 =  dwpc
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        CINT	 =  5
        LINE	 =  17/-3/1/2
        MAP	     =  0
        TITLE	 =  1//~  @  TEMPERATURE AND DEWPOINT
        DEVICE	 =  xw
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        PROJ	 =  mer
        GAREA	 =  ny-
        CLEAR	 =  no
        PANEL	 =  0
        TEXT	 =  1
        SCALE	 =  999
        LATLON	 =  0
        HILO	 =
        HLSYM	 =
        CLRBAR	 =
        CONTUR	 =  3
        SKIP	 =  0
        FINT	 =  0
        FLINE	 =  10-20
        CTYPE	 =  C
        LUTFIL   =
        STNPLT   =

3.  Now clear the screen and draw a color fill of the divergence
    of the gridded wind, alternating between color 23 and 19, for
    the 24 hour forecast time.  Scale the data by 10**5 and use a
    contour interval of 0.5.  Draw contour lines in color 2 using
    heavy, solid lines, labeling every contour level. Draw a
    color bar of the fill colors using the default conditions.
    The display area is changed to Missouri and the lat/lon lines
    are plotted at 5 degree intervals.
    
        GDATTIM	 =  f24
        GLEVEL	 =  700
        GVCORD	 =  pres
        GFUNC	 =  div(wnd)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        CINT	 =  .5
        LINE	 =  2/1/7/1
        MAP   	 =  1
        TITLE	 =  1
        DEVICE	 =  xw
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        PROJ	 =  mer
        GAREA	 =  mo
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        SCALE	 =  5
        LATLON	 =  0
        HILO	 =
        HLSYM	 =
        CLRBAR	 =  1
        CONTUR	 =  3
        SKIP	 =  0
        FINT	 =  .5
        FLINE	 =  23;19
        CTYPE	 =  f/c
        LUTFIL   =
        STNPLT   =

4.  Clear the screen and draw contours of absolute vorticity.
    Label vorticity maxima with a red X and minima with a cyan N.
    Plot the values of the extrema.  Exclude minima less than
    4 * 10 ** -5.  The search radius for finding extrema is
    5 grid points.  Interpolate the extrema to off-grid point
    locations.  Plot the values in font number 2 on the
    right beneath the marking symbol.  Plot the marking label
    in size 1.5, the values in size 1.
    
        GDATTIM	 =  f24
        GLEVEL	 =  700
        GVCORD	 =  pres
        GFUNC	 =  avor(wnd)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        CINT	 =  2
        LINE	 =  2/1/2
        MAP	     =  1
        TITLE	 =  1
        DEVICE	 =  xw
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        PROJ	 =  mer
        GAREA	 =  ri
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        SCALE	 =  5
        LATLON	 =  0
        HILO	 =  2;6/X#;N#/;4-28/5//yes
        HLSYM	 =  1.5;1/3/1;2
        CLRBAR	 =  1
        CONTUR	 =  3
        SKIP	 =  0
        FINT	 =  2
        FLINE	 =  23;19
        CTYPE	 =  f/c
        LUTFIL   =
        STNPLT   =

### Error Messages
 
    [GDCNTR  +2]    WARNING:  ... not found.  CONTINUING---
    [GDCNTR  +1]    WARNING.  There are no contour levels.
    [GDCNTR  -1]    Fatal error initializing TAE.
    [GDCNTR  -2]    Fatal error reading TAE parameters.
    [GDCNTR  -3]    Fatal error initializing GEMPLT.
    [GDCNTR  -4]    Grid requested is not available.
    [GDCNTR  -5]    Error setting grid navigation for file ....
    [GDCNTR  -6]    There are no grids in grid file.
    [GDCNTR -13]    There are no times in the grid file
