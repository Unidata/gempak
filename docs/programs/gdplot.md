# GDPLOT

GDPLOT draws contour lines through scalar grids and/or wind
       barbs or arrows through vector grids.  Multiple sets
       of contours and vectors can be generated for each
       frame.

### Input Parameters
 
    GDFILE    Grid file
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    PANEL     Panel loc/color/dash/width/regn
    SKIP      Skip_cntr/skip_plt_x;skip_plt_y
    SCALE     Scalar scale / vector scale
    GFUNC     Scalar grid
    CTYPE     Contour type:  C/F
    CONTUR    Subbox/smooth
    CINT      Contour interval/min/max
    LINE      Color/type/width/label/smth/fltr/scflg
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    HILO      Color/symbol/rng/rad/cnt/intp
    HLSYM     HILO txt size/posn/font/wdth/hw
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    GVECT     Vector grid
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
 
 

### Program Description
 
GDPLOT draws contours through scalar grids and wind barbs
or arrows at grid points for vector grids.  Plots are
generated for any field computed using the GEMPAK grid
diagnostic functions.  The program can generate multiple
sets of contours and vector plots for each frame.  It can
also generate plots for multiple times.

A list of times may be given in GDATTIM allowing animation.

Exclamation points are used in GFUNC and GVECT to delimit
multiple sets of contours and vectors, respectively.
GLEVEL, GVCORD, PANEL, SKIP, SCALE, GFUNC, CTYPE, CONTUR,
CINT, TITLE, LINE, FINT, FLINE, GVECT, WIND, REFVEC, HILO,
and HLSYM may contain exclamation points to delimit
specifications for the fields defined by GFUNC and GVECT.
If any parameter contains more specifications than the
number of plots specified in GFUNC or GVECT, they will be
ignored.  Positions between exclamation points may be left
blank.  A trailing exclamation point will be treated as a
blank.  If there is no trailing exclamation point, the
last specification will be repeated for subsequent plots.

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

If M is entered in the wind symbol specification of WIND, winds
will be displayed in m/s, unless the KNOTV operator has been
specified in GVECT, in which case the winds will be displayed
in knots.  If K is entered in WIND, the wind is displayed in
knots.

For contours, SKIP specifies the number of grid points to skip
in generating contours.  For example, if SKIP = 1, every other
point is used to generate the contours.

For vectors, SKIP specifies the number of points to skip in both
coordinate directions.  For example, SKIP = /1;2 will display winds
at every other grid point in the x direction and every third grid
in the y direction.  SKIP = /0 displays winds at every grid point.
Wind barbs or arrows can also be staggered by specifying negative
values for SKIP.

The HILO and HLSYM parameters control marking and labeling the
values of relative maxima and minima.  CLRBAR allows a color bar
to be added for color fill contours.

 
### Examples
 
1.  Plot the 700 mb temperature, dewpoint and wind barbs.  Plot the
temperature in color 3 and the dewpoint in color 17.  For both
sets of contours, the lines of negative value are drawn in
pattern 3 and the those of positive value are solid.  Every other
contour is labeled.  The wind barbs are plotted in m/s in color 6.
The display area is an enlarged area centered on New York.

        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GDATTIM	 =  f24
        GLEVEL	 =  700
        GVCORD	 =  pres
        PANEL	 =  0
        SKIP	 =  0
        SCALE	 =  0
        GFUNC	 =  tmpc                                      ! dwpc
        CTYPE	 =  C
        CONTUR	 =  3
        CINT	 =  2                                         ! 5
        LINE	 =  3/-3/3/2                                  ! 17/-3/2/2
        FINT	 =  0
        FLINE	 =  10-20
        HILO	 =
        HLSYM	 =
        CLRBAR	 =
        GVECT	 =  wnd
        WIND	 =  bm6//2
        REFVEC	 =  10
        TITLE	 =  1/-2/~  @  TEMPERATURE, DEWPOINT AND WIND ! 0
        TEXT	 =  1/22//hw
        CLEAR	 =  yes
        GAREA	 =  ny-
        PROJ	 =  mer
        MAP	 =  1/7
        LATLON	 =  2/10/1/1/5;5
        DEVICE	 =  xw
        STNPLT   =

2.  Draw fills and contours of the magnitude of the wind (knots),
contours of height and wind barbs (knots) at 250 mb.  The
contour and fill intervals are specified in CINT and FINT.
The display area is the entire grid on a stereographic map.
    
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GDATTIM	 =  f24
        GLEVEL	 =  250
        GVCORD	 =  pres
        PANEL	 =  0
        SKIP	 =  0/1
        SCALE	 =  0                                           ! -1
        GFUNC	 =  knts(mag(wnd))                              ! hght
        CTYPE	 =  c/f                                         ! c
        CONTUR	 =  1
        CINT	 =  30;50;70;90;110;130;150                     ! 12
        LINE	 =  27/5/2/1                                    ! 20/1/2/1
        FINT	 =  70;90;110;130;150
        FLINE	 =  0;25;24;29;7;15
        HILO	 =
        HLSYM	 =
        CLRBAR	 =  1
        GVECT	 =  kntv(wnd)
        WIND	 =  bk18//1
        REFVEC	 =
        TITLE	 =  5/-2/~  @  HEIGHTS, ISOTACHS AND WIND (KTS) ! 0
        TEXT	 =  1/21//hw
        CLEAR	 =  yes
        GAREA	 =  grid
        PROJ	 =  str/90;-105;0
        MAP	 =  1
        LATLON	 =  0
        DEVICE	 =  XW
        STNPLT   =

### Error Messages
 
    [GDPLOT  +3]    WARNING:  ... not found.  CONTINUING---
    [GDPLOT  +2]    The requested scalar/vector cannot be computed.
    [GDPLOT  +1]    WARNING.  There are no contour levels.
    [GDPLOT  -1]    Fatal error initializing TAE.
    [GDPLOT  -2]    Fatal error reading TAE parameters.
    [GDPLOT  -3]    Fatal error initializing GEMPLT.
    [GDPLOT  -4]    Grid requested is not available.
    [GDPLOT  -5]    Error setting grid navigation for file ....
    [GDPLOT  -6]    There are no grids in grid file.
