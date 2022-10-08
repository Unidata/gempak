# SFCNTR

SFCNTR plots surface station data on a map and optionally
       contours one of the fields being plotted.

### Input Parameters
 
    AREA      Data area
    GAREA     Graphics area
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    SFPARM    Surface parameter list
    DATTIM    Date/time
    SFFILE    Surface data file
    COLORS    Color list
    MAP       Map color/dash/width/filter flag
    LATLON    Line color/dash/width/label/inc
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    PROJ      Map projection/angles/margins|drop flag
    FILTER    Filter data factor
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq
    CNTRPRM   Parameter to contour
    GAMMA     Convergence parameter
    WEIGHT    Barnes Weighting parameter
    LINE      Color/type/width/label/smth/fltr
    CONTUR    Subbox/smooth
    NPASS     Number of passes
    CINT      Contour interval/min/max
 
 

### Program Description
 
SFCNTR is a special version of SFMAP which optionally
allows the user to select one of the parameters specified in
SFPARM to be contoured using Barnes objective analysis.

SFCNTR uses the mean station spacing as provided in OAGRID
in order to define a suitable number of gridpoints for the
analysis. Objective analysis is accomplished using the Barnes
method as given in OABSFC. Finally, contours are drawn as in
the GDCNTR program.

SFMAP plots data at station locations on a map.  Any
parameter that can be computed from the parameters in
the data set can be displayed.  Conditions can be specified
for the parameters.  The conditions are documented in the
SFPARM documentation.  Data may be plotted in any
valid GEMPAK projection and may be overlaid on images.

A list of times may be given in DATTIM allowing animation of
surface plots.

The order of the input parameters determines their location
on the plot.  Parameters are separated by semicolons.  A
position may be skipped by entering two consecutive semicolons
or entering the name SPAC or BLNK.  A parameter will be
plotted centered at the station if it is the first parameter
in the list, for example, if SFPARM = SKYC, the sky cover symbol
will be plotted centered on the station location.  If no
parameter is to be displayed centered on the station location,
a semicolon must appear before the first parameter listed.
SPAC or BLNK may also be entered.  The following chart shows
the location on the model of each position.

            8
        2	10	4
        3	1	5
        6	11	7
            9

Note that wind symbols and markers are always plotted at the center
of the station model.

If FILTER is set to YES, the stations will be filtered so that
overlapping stations will not be plotted.  FILTER may also be
entered as a number greater than or equal to zero.  FILTER = NO
has the same effect as FILTER = 0.  FILTER = 1 has the same
effect as FILTER = YES.  Values less than 1 allow more crowding
of stations, values exceeding 1 less crowding.  If a position
is skipped using two semicolons or if the parameter is BLNK, the
filter will not allocate the space.  The parameter SPAC may be
used to reserve the space with the filter option.  Then later
plots will plot the same stations after filtering, provided
that the same parameter locations are specified.

If certain stations are not to be removed by the filter, these
stations are listed first following an @.  The area over which
filtering is to occur is specified after a slash.  For example,

    AREA   = @bwi;iad;dca/md
    FILTER = YES

will display a filtered array of stations over the area
corresponding to MD, but BWI, IAD and DCA will be shown
regardless of the filtering.

Weather symbols can also be plotted.  The size and width of
the symbols can be specified by appending the numbers to the
parameter name using a colon as a separator.  For example,
WSYM:2:5 will plot weather symbols with size of 2. and line
width of 5.

Other symbols can be specified in a similar way.  The names for
the pressure tendency, sky cover, cloud type, weather and wind
symbols and how their characteristics are set are given in the
SFPARM documentation.

Parameters can be color-coded based on their own value or on
the value of any other computable parameter.  Refer to the
COLORS documentation for details.  If one or more parameters
are color-coded, a color bar will be displayed for the first
color-coded parameter.

CNTRPRM is the name of one of the parameters specified in SFPARM
which will be contoured from objective analysis of the data.
The parameter must be one listed in SFPARM. If the used does not
want the data values displayed, set COLOR=0 for that SFPARM value.

GAMMA is the Barnes objective analysis convergence parameter.

WEIGHT is the Barnes weighting parameter. Typical values range from 20 to 50
A value of 20 is the default.

LINE provides the settings for contour lines used in the display.

CONTUR is the grid sub-box and number of smoothing passes used by the
contouring algorithm.

NPASS is the number of passes in the Barnes scheme.

CINT is the contour interval for the display.

 
### Examples
 
1.  Plot the 12Z observerd surface temperatures (tmpf) on a US map and
    contour the data using a contour interval of 5 degrees.

        AREA	 =  dset
        GAREA	 =  nj
        SATFIL	 =
        RADFIL	 =
        SFPARM	 =  tmpf
        DATTIM	 =  1200
        SFFILE	 =  metar
        COLORS	 =  6
        MAP	     =  3/1/1
        LATLON	 =  0
        TITLE	 =  1
        CLEAR	 =  yes
        PANEL	 =  0
        DEVICE	 =  xw
        PROJ	 =  lcc
        FILTER	 =  n
        TEXT	 =  1/22//hw
        LUTFIL   =
        STNPLT   =
        CLRBAR   =
        CNTRPRM  =  tmpf
        GAMMA    =  .3
        WEIGHT   =  50
        LINE     =  5/1/1
        CONTUR   =  3/3
        NPASS    =  2
        CINT     =  5

### Error Messages
 
    [SFCNTR  -1]    Fatal error initializing TAE.
    [SFCNTR  -2]    Fatal error reading TAE parameters.
    [SFCNTR  -3]    Fatal error initializing GEMPLT.
    [SFCNTR  -4]    Parameter ... is not calculable.
    [SFCNTR  -5]    Winds are not calculable.
