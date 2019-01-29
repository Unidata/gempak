# SNMAP

SNMAP plots sounding data on a map.

### Input Parameters
 
    AREA      Data area
    GAREA     Graphics area
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    SNPARM    Sounding parameter list
    DATTIM    Date/time
    LEVELS    Vertical levels
    VCOORD    Vertical coordinate type
    SNFILE    Sounding data file
    COLORS    Color list
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    LATLON    Line color/dash/width/freq/inc/label/format
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    PROJ      Map projection/angles/margins|drop flag
    FILTER    Filter data factor
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
 
 

### Program Description
 
SNMAP plots sounding data parameters at station locations
on a map.  Any level or station parameter that can be
computed can be displayed.  Data may be plotted in any valid
GEMPAK projection and may be overlaid on images.

A list of times may be given in DATTIM allowing animation of
sounding plots.

The order of the input parameters determines their location
on the plot.  Parameters are separated by semicolons.  A
position may be skipped by entering two consecutive
semicolons or entering parameter SPAC or BLNK.  A parameter
will be plotted centered at the station if it is the first
parameter in the list. If no parameter is to be displayed
centered on the station location, a semicolon must appear
before the first parameter, or the first parameter must be
either SPAC or BLNK.  The following chart shows the placement
of the data around the station.  The number indicates the
position of the parameter in the SNPARM list:

				8
			2	10	4
			3	1	5
			6	11	7
				9

Note that wind symbols and markers are always plotted at the
center.

Station data will be filtered; i.e., overlapping stations
will not be plotted, if FILTER is set to YES.  FILTER may
also be entered as a number greater than or equal to zero.
FILTER = NO has the same effect as FILTER = 0.  FILTER = 1
has the same effect as FILTER = YES.  Values less than 1
allow more crowding of stations, values exceeding 1 less
crowding.  If a parameter is BLNK, the filter will not allocate
any space for that parameter.  The parameter SPAC may be used
to reserve the space with the FILTER option so that later calls
will plot the same stations after filtering, provided that the
same number of parameters is specified.

If certain stations are not to be removed by the filter, these
stations are listed first following an @.  The area over which
filtering is to occur is specified after a slash.  For example,

        AREA   = @iad;hts;rap/us
        FILTER = YES

will display a filtered array of stations over the area
corresponding to US, but IAD, HTS and RAP will be shown
regardless of the filtering.

Either wind barbs or wind arrows can be plotted, by
specifying a wind symbol parameter name in the list of
parameters for SNPARM.  The wind barb or arrow is plotted
at the station location according to the type specification,
which is entered as described in the SNPARM documentation.

Conditions can be specified for the parameters.  The
conditions are documented in the SNPARM documentation.  Note
that individual parameters can be scaled using these
conditional functions.  For example, TMPC*10 will plot
temperature multiplied by 10.

 
### Examples
 
1.  Plot a polar stereographic map of stations in the Eastern
United States at 850 mb for all times in the data file.
Plot a standard station model for each location. The upper
air data to plot include: 1) wind barbs in knots;
2) temperature in Celsius; 3) coded height; 4) dewpoint
depression in Celsius; and 5) station ID.  The data are
plotted using the specified color list.
    
        AREA	 =  east-
        GAREA	 =  east
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        SNPARM	 =  brbk:1:2:112;tmpc;;stdz;;dpdc;stid
        DATTIM	 =  all
        LEVELS	 =  850
        VCOORD	 =  pres
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        COLORS	 =  6;2;5;3;18
        MAP	     =  1
        LATLON	 =  2/10/1/1/10;10
        TITLE	 =  1
        CLEAR	 =  yes
        PANEL	 =  0
        DEVICE	 =  xw
        PROJ	 =  nps
        FILTER	 =  no
        TEXT	 =  1
        LUTFIL   =
        STNPLT   =

2.  Plot the temperature and height at 500 mb and the lifted
index for all times in the data file.  The data are plotted
at the stations where the temperature is less than -8
degrees Celsius.  The lifted index is multiplied by 10
before being plotted.
    
        AREA	 =  us-
        GAREA	 =  us
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        SNPARM	 =  mark:2;;tmpc<-8;;hght;;;;;lift*10
        DATTIM	 =  all
        LEVELS	 =  500
        VCOORD	 =  pres
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        COLORS	 =  2;5;3
        MAP	     =  1
        LATLON	 =  2/10/1/1/10;10
        TITLE	 =  1
        CLEAR	 =  yes
        PANEL	 =  0
        DEVICE	 =  xw
        PROJ	 =  mer
        FILTER	 =  0
        TEXT	 =  .75
        LUTFIL   =
        STNPLT   =

### Error Messages
 
    [SNMAP  +1]     Parameter ... cannot be computed.
    [SNMAP  -1]     Fatal error initializing TAE.
    [SNMAP  -2]     Fatal error reading TAE parameters.
    [SNMAP  -3]     Fatal error initializing GEMPLT.
    [SNMAP  -4]     Invalid levels or vertical coordinate have been input.
    [SNMAP  -5]     A range of levels is invalid in SNMAP.
    [SNMAP  -6]     Winds cannot be computed.
