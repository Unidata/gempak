# GDCROSS

GDCROSS displays a vertical cross section of scalar and/or vector
        grids.

### Input Parameters
 
    CXSTNS    Cross-section station line
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GVECT     Vector grid
    GDFILE    Grid file
    WIND      Wind symbol/siz/wdth/typ/hdsz
    REFVEC    Mag;x;y;txtsiz/font/wdth/HW;labl
    PTYPE     Plot type/h:w ratio/margins
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
    CINT      Contour interval/min/max
    SCALE     Scalar scale / vector scale
    LINE      Color/type/width/label/smth/fltr/scflg
    BORDER    Background color/type/width
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    PANEL     Panel loc/color/dash/width/regn
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    CONTUR    Subbox/smooth
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    CTYPE     Contour type:  C/F
 
 

### Program Description
 
GDCROSS draws a vertical cross section between two points
in a grid field.  The cross-section path is a line segment
on the grid projection plane joining the two points.  Gridded
data are interpolated to the cross-section plane at intervals
corresponding to approximately one grid increment.

CXSTNS specifies the beginning and ending points of the cross
section.  Each point may be entered as a latitude and longitude
separated by a semicolon, as station identifiers or numbers or
as x and y grid coordinates separated by a semicolon and
preceded by `@`.  The two points are separated by `>`.  The cross
section end points may also be selected graphically by setting
CXSTNS with the CURSOR command.  The end points are then
selected by clicking on a horizontal map displayed in another
GEMPAK XW window.

The vertical coordinate, set in GVCORD, can be PRES, THTA, HGHT,
or SGMA.  The data must be stored in that vertical coordinate in
the grid file.  No automatic vertical interpolation is included.
The vertical axis scaling, set in PTYPE, can be LIN, LOG, KAP
or STUVE.  STUVE and KAP are the same; SKEWT may not be entered.
The plot aspect ratio and margins may also be entered in PTYPE.

Both scalar and vector fields can be displayed in the
cross-section plane.  Contour lines are drawn through the scalar
field.  Contours may be color filled.  The line contours and filled
contours are specified as in GDCNTR.   CLRBAR allows a color bar to
be added for color fill contours.  Vector fields may be depicted
using arrows or barbs.

If M is entered in WIND, winds will be displayed in meters
per second unless the KNOTV operator has been specified in
GVECT, in which case the winds will be displayed in knots.
IF K is entered in WIND, the wind is displayed in knots.

Circulations can be displayed in the cross-section plane by
specifying `GFUNC = CIRC ( V, W )`, where `V` is a vector field
and W is the scalar vertical motion in the GVCORD coordinate.
The horizontal component of the circulation is the tangential
component of `V`.  The vertical component is `W` scaled up to
account for the exaggerated aspect ratio of the display
relative to that of the real atmosphere.  For the CIRC
operator, `W` is assumed to be pressure velocity in mb/s
for PRES and THTA coordinates and cm/s for the HGHT coordinate.
If the vertical component does not require scaling, the
circulation can be specified explicitly in the form
`GVECT = VECR ( TANG ( V ), W )/VERT`, where `VERT` is a flag
indicating that the vector components should not be rotated to
be north-relative.


### Examples
 
1.  Plot temperature in Celsius on a log-P chart along the cross section from LAX to BWI.  Plot the filled contours
using every other color from 30 to 8.  Plot the contour
lines in color number 32, using solid lines.  Plot wind barbs for the wind.  Plot a horizontal color bar centered
under the cross section.
    
        CXSTNS	 =  lax>bwi
        GDATTIM	 =  f06
        GVCORD	 =  pres
        GFUNC	 =  tmpc
        GVECT	 =  wnd
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        WIND	 =  bm6//2
        REFVEC	 =  10
        PTYPE	 =  log
        YAXIS	 =
        CINT	 =
        SCALE	 =
        LINE	 =  32/1/3
        BORDER	 =  1
        TITLE	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw
        TEXT	 =  1
        PANEL	 =  0
        CLRBAR	 =  1/h/cc/.5;.03/.6;.01
        CONTUR	 =  3
        FINT	 =
        FLINE	 =  30-8-2
        CTYPE	 =  c/f

2.  Now, using the same file, plot a log pressure cross
    section of the vorticity advection, scaled by 10**9.
    Also, plot the ageostrophic circulation using arrows.
    Use a cross-section line from grid point (1,1) to grid
    point (10,20).  Use a height-to-width plot ratio of 0.25.

        CXSTNS	 =  @1;1>@10;20
        GDATTIM	 =  f06
        GVCORD	 =  pres
        GFUNC	 =  adv(avor(wnd),wnd)
        GVECT	 =  circ(age;omeg)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        WIND	 =  am6//2//.75
        REFVEC	 =  5;.05;.2
        PTYPE	 =  log/.25
        YAXIS	 =
        CINT	 =  .5
        SCALE	 =  9
        LINE	 =  32/1/2
        BORDER	 =  1
        TITLE	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw
        TEXT	 =  1
        PANEL	 =  0
        CLRBAR	 =  1/h/cc/.5;.2/.1;.01
        CONTUR	 =  3
        FINT	 =  0/0/0
        FLINE	 =  4;2
        CTYPE	 =  c/f

### Error Messages
 
    [GDCROSS  +3]   WARNING:  ... not found.  CONTINUING---
    [GDCROSS  +2]  Surface value of GVCORD is not available.
    [GDCROSS  +1]   WARNING.  There are no contour levels.
    [GDCROSS  -1]   Fatal error initializing TAE.
    [GDCROSS  -2]   Fatal error reading TAE parameters.
    [GDCROSS  -3]   Fatal error initializing GEMPLT.
    [GDCROSS  -4]   Input for CXSTNS is invalid.
    [GDCROSS  -5]   Input for GDATTIM is invalid.
    [GDCROSS  -6]   Input for GVCORD is invalid.
    [GDCROSS  -7]   Input for PTYPE is invalid.
    [GDCROSS  -8]   Graph coordinates are incorrectly defined.
    [GDCROSS  -9]   No points found for cross section.
    [GDCROSS -10]   LOG is not possible for this vertical coordinate.
    [GDCROSS -11]   Cross-section coordinates are invalid.
    [GDCROSS -12]   No levels--check input for GDATTIM and GVCORD.
    [GDCROSS -13]   @LEVEL in-line parameter is not allowed.
    [GDCROSS -14]   %VCORD in-line parameter is not allowed.
    [GDCROSS -18]   GVECT cannot be evaluated.
    [GDCROSS -19]   GFUNC cannot be evaluated.
    [GDCROSS -20]   Input for YAXIS is invalid.
