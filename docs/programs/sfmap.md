# SFMAP

SFMAP plots surface station data on a map.

### Input Parameters
 
    AREA      Data area
    GAREA     Graphics area
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    SFPARM    Surface parameter list
    DATTIM    Date/time
    SFFILE    Surface data file
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
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    LSTPRM    Filtered parm|x;y|TEXT info
 
 

### Program Description
 
SFMAP plots data at station locations on a map.  Any
parameter that can be computed from the parameters in
the data set can be displayed.  Conditions can be specified
for the parameters.  The conditions are documented in the
SFPARM documentation.  Data may be plotted in any
valid GEMPAK projection and may be overlaid on images.
A set of multiple surface files separated with a '+' may be
given for SFFILE. The string length of multiple files should
not exceed 128 characters.

A list of times may be given in DATTIM allowing animation of
surface plots for a single file. Multiple files containing
different observations can be filtered with a common filter
for a single DATTIM. Multiple files will not work for multiple
times.

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

 
### Examples
 
1.  Display the visible satellite image from 18Z and overlay
the surface data for the area covered by the image.  Plot
a standard station model for each location in the data set.
The surface data to plot include: 1) sky cover symbol;
2) temperature in Fahrenheit; 3) weather symbol; 4) coded
mean sea level pressure; 5) pressure tendency with symbol;
6) dewpoint in Fahrenheit; 6) station ID; 7) visibility;
and 8) wind barbs in knots.  The data are plotted using the
specified color list.

        AREA	 =  us
        GAREA	 =  dset
        SATFIL	 =  $GEMPAK/data/VIS_910819_1801
        RADFIL	 =
        IMCBAR   =  1/V/LL/0;.05/.90
        SFPARM	 =  skyc:.75;tmpf;wsym:.75;smsl;ptnd;dwpf;stid;;vsby*10;brbk:1:2;mark:2
        DATTIM	 =  910819/1800
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        COLORS	 =  26;2;7;25;20;22;18;24;6
        MAP	     =  1
        LATLON	 =  2/10/1/1/5;5
        TITLE	 =  1
        CLEAR	 =  yes
        PANEL	 =  0
        DEVICE	 =  xw
        PROJ	 =  sat
        FILTER	 =  1
        TEXT	 =  1/22//hw
        LUTFIL   =
        STNPLT   =
        CLRBAR	 =
        LSTPRM   =

2.  Using the above specifications, change the area to New Jersey.
The satellite image will be subset and replotted with the data
for that area.  Change the filter factor to allow more stations
to be plotted.

        AREA	 =  nj-
        GAREA	 =  nj
        SATFIL	 =  $GEMPAK/data/VIS_910819_1801
        RADFIL	 =
        IMCBAR   =  1/V/LL/0;.05/.90
        SFPARM	 =  skyc:.75;tmpf;wsym:.75;smsl;ptnd;dwpf;stid;;vsby*10;brbk:1:2;mark:4
        DATTIM	 =  910819/1800
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        COLORS	 =  26;2;7;25;20;22;18;24;6
        MAP	     =  1
        LATLON	 =  2/10/1/1/5;5
        TITLE	 =  1
        CLEAR	 =  yes
        PANEL	 =  0
        DEVICE	 =  xw
        PROJ	 =  sat
        FILTER	 =  .45
        TEXT	 =  1/22//hw
        LUTFIL   =
        STNPLT   =
        CLRBAR   =
        LSTPRM   =

3.  Using the previous specifications, vary the color of the weather
symbols by color coding them according to their precipitation type.
    To make them more discernible, omit using the satellite image
and change the map projection.

        AREA	 =  nj-
        GAREA	 =  nj
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        SFPARM	 =  skyc:.75;tmpf;wsym:.75;smsl;ptnd;dwpf;stid;;vsby*10;brbk:1:2;mark:4
        DATTIM	 =  910819/1800
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        COLORS	 =  26;2;(19-90-10/17;6;8;17;3;3;1;22;2);25;20;22;18;24;6
        MAP	     =  1
        LATLON	 =  2/10/1/1/5;5
        TITLE	 =  1
        CLEAR	 =  yes
        PANEL	 =  0
        DEVICE	 =  xw
        PROJ	 =  mer
        FILTER	 =  .45
        TEXT	 =  1/22//hw
        LUTFIL   =
        STNPLT   =
        CLABAR   =
        LSTPRM   =

4.  Different example using the multiple files for `SFFILE = synop + metar`
    setting `DATTIM = 12`, `SFPARM = SKYC;TMPF;WSYM;PMSL;;DWPF;BRBK` and the
    edge plotting of TMPF data `LSTPRM = TMPF|0.8;0.9|0.75`
    Edge plotting of data is available only for the first file "synop".
        
        AREA     = WV
        GAREA    = WV
        SATFIL   =
        RADFIL   =
        IMCBAR   =
        SFFILE   = synop + metar
        DATTIM   = 12
        SFPARM   = SKYC;TMPF;WSYM;PMSL;;DWPF;BRBK
        LSTPRM   = tmpf|0.8;0.9|0.75
        COLORS   = 23;2;3
        MAP      = 1
        MSCALE   = 0
        LATLON   =
        TITLE    = 1
        CLEAR    = yes
        PANEL    = 0
        DEVICE   = gif|sfmap_synop_metar.gif | 1800;1300
        PROJ     = MER
        FILTER   = 1.0
        TEXT     = 1
        LUTFIL   =
        STNPLT   =
        CLRBAR   =

### Error Messages
 
    [SFMAP  -1]     Fatal error initializing TAE.
    [SFMAP  -2]     Fatal error reading TAE parameters.
    [SFMAP  -3]     Fatal error initializing GEMPLT.
    [SFMAP  -4]     Parameter ... is not calculable.
    [SFMAP  -5]     Winds are not calculable.
    [SFMAP  -6]     Multiple files do not allow multiple times.
