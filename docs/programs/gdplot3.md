# GDPLOT3

GDPLOT3 draws contour lines through scalar grids and/or wind
        barbs or arrows or streamlines through vector grids. The
	program also plots contents of a text file and/or objects.
	Multiple sets of contours, vectors, streamlines, objects
	and/or text files can be generated for each frame.

### Input Parameters
 
    GDFILE    Grid file
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    PANEL     Panel loc/color/dash/width/regn
    SKIP      Skip_cntr/skip_plt_x;skip_plt_y
    SCALE     Scalar scale / vector scale
    GDPFUN    Scalar grid or vector grid function
    TYPE      GDPLOT2 function processing type
    CONTUR    Subbox/smooth
    CINT      Contour interval/min/max
    LINE      Color/type/width/label/smth/fltr/scflg
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    HILO      Color/symbol/rng/rad/cnt/intp
    HLSYM     HILO txt size/posn/font/wdth/hw
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    WIND      Wind symbol/siz/wdth/typ/hdsz
    REFVEC    Mag;x;y;txtsiz/font/wdth/HW;labl
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    GAREA     Graphics area
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
    PROJ      Map projection/angles/margins|drop flag
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    LATLON    Line color/dash/width/freq/inc/label/format
    DEVICE    Device|name|x size;y size|color type
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    LUTFIL    Enhancement lookup table filename
    STREAM    lines/arrows/stop/slow/scale
    POSN      Position / Text format
    COLORS    Color list
    MARKER    Marker color/type/size/width/hw
    GRDLBL    Grid point label color
    FILTER    Filter data factor
    BOXLIN    Box color/type/width/label/smth/fltr
    REGION    Region type
    TXTCOL    Text colors
    TXTYPE    Text size/fnt/wdth/brdr/N-rot/just/hw flg
    TXTFIL    Text filename or LOGO|size|mode
    TXTLOC    Text location
    COLUMN    Number of columns
    SHAPE     Object to draw
    INFO      Shape information
    LOCI      Point(s) associated with SHAPE
    ANOTLN    Annotation line color/type/width/label/smth/fltr
    ANOTYP    Annotation fill type: C/F
 
 

### Program Description
 
GDPLOT3 draws contours through scalar grids and streamlines
wind barbs or arrows at grid points for vector grids.  Plots are
generated for any field computed using the GEMPAK grid diagnostic
functions.  The program also allows the user to place an object,
draw a box or plot text data read from an ASCII text file.

GDPLOT3 can generate multiple sets of plots for each frame.  It can
also generate plots for multiple times.  The list of times may be
given in GDATTIM allowing animation.

Exclamation points are used in GDPFUN to delimit multiple overlays
of scalar and vector fields, respectively.  All other parameters
(except DEVICE, CLEAR and LUTFIL) may contain exclamation points
to delimit specifications for the fields defined by GDPFUN.  If
any parameter contains more specifications than the maximum number of
plots specified in GDPFUN or GDFILE, they will be ignored.  Positions
between exclamation points may be left blank.  Most parameters
will replace the blank with the previous value (repeat).  A trailing
exclamation point will be treated as a blank.  If there is no
trailing exclamation point, the last specification will be repeated
for subsequent plots.

#### Scalar Quantities

Scalar contours may be displayed as lines or as a color fill.
If TYPE is C, contour lines are drawn using input from CINT
and LINE.  If TYPE is F, filled contours are drawn using
specifications from FINT and FLINE. Both contour lines and
filled contours are drawn if TYPE is F/C or C/F.

Scalar grid point values may also be plotted by specifying TYPE
to be P.  If TYPE M is also specified, then markers will be plotted
at the grid point locations according to the MARKER parameter.
The value's location relative to the actual grid point location
is set by the POSN parameter.

All scalar types ( C, F and P) may be requested at
one time, e.g., TYPE=C/F/P.  The order is irrelevant, however,
contour fills (F) will be done first followed by contour
lines (C), the map background, grid point indices (M), markers (G)
and values (P).

The attributes of the contour lines, including the color,
line type, line width, and label frequency are specified
in LINE.  The four attributes must be separated with slashes;
semicolons separate the values for each attribute.  If the
line type is set to a single negative number, negative
contour values will have the absolute value of the line type
and positive values will be solid.  If the label type is set
to a single number, n, then every nth value will be labeled.

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

For contours, SKIP specifies the number of grid points to skip
in generating contours.  For example, if `SKIP = 1`, every other
point is used to generate the contours.

The HILO and HLSYM parameters control marking and labeling the
values of relative maxima and minima.  CLRBAR allows a color bar
to be added for color fill contours. IMCBAR allows a color bar
to be added for images.

#### Vector Quantities

Vector grids may be displayed as either a field of wind barbs
or arrows, or as a streamline display.  If TYPE is A, wind arrows
will be displayed; if TYPE is B, wind barbs will be displayed.
If both A and B are specified, B will prevail.
If TYPE is N and neither A nor B is also indicated, then non-unit
wind barbs will be assumed.  If TYPE is N in combination with either
A or B, then non-unit arrows or barbs, respectively, will be plotted.
If TYPE is S, streamlines will be drawn.

As with scalar functions, the order of the TYPE values is irrelevant.
The order of plotting is the map background, grid point indices (G),
markers (M), vector arrows/barbs (A or B) and streamlines (S).

Winds will be displayed in m/s, unless the KNTV operator has been
specified in GDPFUN, in which case the winds will be displayed
in knots.

For vectors, SKIP specifies the number of points to skip in both
coordinate directions.  For example, `SKIP = /1;2` will display winds
at every other grid point in the x direction and every third grid
in the y direction.  `SKIP = /0` displays winds at every grid point.
Wind barbs or arrows can also be staggered by specifying negative
values for SKIP.

The FILTER parameter filters gridded wind arrows/barbs in the same
way as it filters station models in the program SFMAP.  If
FILTER=yes, the default filter value is set to 1.0.  If FILTER=no,
then the SKIP parameter is used to filter the winds.  Otherwise
FILTER is expected to be a numeric value greater than zero which
controls the density of plotted winds.

#### Text and Annotation 

TXTFIL is the input text file name.  If the input file is NOAA or LOGO,
the NOAA logo is plotted.  The NWS logo is plotted if "NWS" is entered.
The logo can be plotted in either full color or monochrome.

The text location TXTLOC is given as the X and Y values in the
Normalized coordinate system or Map coordinate system if a '#' is
prefixed before the X and Y values.  COLUMN is the number of columns
the text will be divided into within a panel.

The TXTCOL and TXTYPE parameters control text colors and text attributes
for the text read from the TXTFIL.  The text will be divided into a
number of columns specified by COLUMN within a panel.

The parameter SHAPE is the object that the user wants to plot.  The INFO
and LOCI parameters define the information and the point(s) need to place
the object to be plotted.  Currently, Normalized and Map coordinates can
be used.  The Grid coordinate has not been implemented.

The line attributes and annotation types (C/F) of the object are controlled
by ANOTLIN and ANOTYP.

#### Miscellaneous

Grid point markers may be plotted by setting TYPE=M along with
either a scalar TYPE or vector TYPE and valid GDPFUN.
Marker attributes will be determined via the MARKER parameter.

Grid index values ( row and column numbers ) may be plotted by
setting TYPE=G along with either a scalar TYPE or vector TYPE and
valid GDPFUN.  Attributes will be determined via the GRDLBL parameter.

Station locations and identifiers may be plotted using the
STNPLT parameter.

A box can be drawn around a view, plot, or device region via BOXLIN
and REGION parameters.


### Examples
 
1.  Plot the 700 mb temperature, dewpoint and wind barbs.  Plot the
temperature in color 3 and the dewpoint in color 17.  For both
sets of contours, the lines of negative value are drawn in
pattern 3 and the those of positive value are solid.  Every other
contour is labeled.  The wind barbs are plotted in m/s in color 6
and are filtered according to the FILTER value.  The contents of
a text file mytext.txt will be displayed at the location .1;.8 in
a single column.  The text string "This is plot 1" will be centered
at normal coordinates .1;.1, in color 15, with a filled box of color
25 around the text.
The display area is an enlarged area centered on New York.  A solid
line of width 1 is drawn around the view region.

        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GDATTIM	 =  f24
        GLEVEL	 =  700
        GVCORD	 =  pres
        PANEL	 =  0
        SKIP	 =  0
        SCALE	 =  0
        GDPFUN	 =  tmpc        ! dwpc       ! wnd
        TYPE	 =  C           !            ! B
        CONTUR	 =  3
        CINT	 =  2           ! 5
        LINE	 =  3/-3/3/2    ! 17/-3/2/2
        FINT	 =  0
        FLINE	 =  10-20
        HILO	 =
        HLSYM	 =
        CLRBAR	 =
        WIND	 =  bm6//2
        REFVEC	 =  10
        TITLE	 =  1/-2/~  @  TEMPERATURE, DEWPOINT AND WIND ! 0
        TEXT	 =  1/22//hw
        CLEAR	 =  yes
        GAREA	 =  ny-
        PROJ	 =  mer
        MAP	     =  1/7
        LATLON	 =  2/10/1/1/5;5
        DEVICE	 =  xw
        STNPLT   =
        SATFIL	 =
        RADFIL   =
        IMCBAR   =
        FILTER   =  1.2
        BOXLIN   =  1
        REGION   =  view
        TXTCOL   =  1
        TXTYPE   =  1
        TXTFIL   =  mytext.txt
        TXTLOC   =  .1;.8
        COLUMN   =  1
        SHAPE    =  text
        INFO     =  1/3////c/hw/This is plot 1
        LOCI     =  .1;.1
        ANOTLN   =  15///25
        ANOTYP   =  f

2.  Draw fills and contours of the magnitude of the wind (knots),
contours of height and wind barbs (knots) at 250 mb and
plots the value of the wind magnitude at the grid points.
Winds are filtered according to the FILTER value.
The contour and fill intervals are specified in CINT and FINT.
The display area is the entire grid on a stereographic map.

        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GDATTIM	 =  f24
        GLEVEL	 =  250
        GVCORD	 =  pres
        PANEL	 =  0
        SKIP	 =  0/1
        SCALE	 =  0                       ! -1
        GDPFUN	 =  knts(mag(wnd))          ! hght      ! kntv(wnd)
        TYPE	 =  c/f/p                   ! c         ! b
        CONTUR	 =  1
        CINT	 =  30;50;70;90;110;130;150 ! 12
        LINE	 =  27/5/2/1                ! 20/1/2/1
        FINT	 =  70;90;110;130;150
        FLINE	 =  0;25;24;29;7;15
        HILO	 =
        HLSYM	 =
        CLRBAR	 =  1
        WIND	 =  bk18//1
        REFVEC	 =
        TITLE	 =  5/-2/~  @  HEIGHTS, ISOTACHS AND WIND (KTS) ! 0
        TEXT	 =  1/21//hw
        IMCBAR   =
        CLEAR	 =  yes
        GAREA	 =  grid
        PROJ	 =  str/90;-105;0
        MAP	     =  1
        LATLON	 =  0
        DEVICE	 =  XW
        STNPLT   =
        SATFIL   =
        RADFIL   =
        IMCBAR   =
        FILTER   =  yes
        BOXLIN   =
        REGION   =
        TXTCOL   =
        TXTYPE   =
        TXTFIL   =
        TXTLOC   =
        COLUMN   =
        SHAPE    =
        INFO     =
        LOCI     =
        ANOTLN   =
        ANOTYP   =

### Error Messages
 
    [GDPLOT3  +3]   WARNING: Invalid coordinate for annotation.
    [GDPLOT3  +2]   The requested scalar/vector cannot be computed.
    [GDPLOT3  +1]   WARNING.  There are no contour levels.
    [GDPLOT3  -1]   Fatal error initializing TAE.
    [GDPLOT3  -2]   Fatal error reading TAE parameters.
    [GDPLOT3  -3]   Fatal error initializing GEMPLT.
    [GDPLOT3  -4]   Grid requested is not available.
    [GDPLOT3  -5]   Error setting grid navigation for file ....
    [GDPLOT3  -6]   There are no grids in grid file.
    [GDPLOT3  -7]   File ... does not exist.
