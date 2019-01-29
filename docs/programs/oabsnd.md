# OABSND

OABSND performs a Barnes objective analysis on upper air data.

### Input Parameters
 
    SNFILE    Sounding data file
    GDFILE    Grid file
    SNPARM    Sounding parameter list
    STNDEX    Stability indices
    LEVELS    Vertical levels
    VCOORD    Vertical coordinate type
    DATTIM    Date/time
    DTAAREA   Data area for OA
    GUESS     Guess file
    GUESFUN   Guess grid
    GAMMA     Convergence parameter
    SEARCH    Search radius/Extrapolation
    NPASS     Number of passes
    QCNTL     Quality control threshold
    OABND     Bounds file(s) to use for 'blocking'
 
 

### Program Description
 
OABSND performs a Barnes objective analysis on upper air
data.  Multiple parameters and levels may be analyzed at the
same time.  If more than one time is entered, the analyses
will be performed sequentially.

The input sounding file is specified in SNFILE.  Up to three
files may be entered by separating the names with a plus, +.
If the time is set to FIRST or LAST, the first or last time
from the first file will be used.

The projection and area for the grid are read from the
navigation block in the output grid file.  The extend grid
area, the station spacing and the data subset area are read
from the analysis block.  The extend area is used to define
a larger grid for the first pass, allowing data to be
interpolated back to stations outside the final grid area.
The average station spacing is used to compute the weighting
functions.  The data subset area specifies which data will
be used in the analysis.  If DTAAREA is blank or set to DATA,
the data area defined in the analysis block will be used.
This is the recommended procedure.  However, DTAAREA may be
specified in the same way as the AREA parameter used in other
programs.  This is especially useful to eliminate stations which
are known to have bad data.

A first-guess field can be used in the analysis.  The name
of the grid file containing the guess field must be entered
in GUESS,  and the diagnostic functions that compute the
first-guess grids must be entered in GUESFUN.  If a diagnostic
function is not specified, the corresponding surface parameter
will be used.  If the grid file contains the analysis time or
has only one time, that time will be used;  otherwise, a
forecast time that is valid at the analysis time wil be computed
based on user inputs.  Grid time, level, and vertical coordinate
could also be inlined in diagnostic functions.  When a guess field
is used, it is inserted into the grid as the zeroth pass.

Each pass of the analysis interpolates data from stations to
grid points using the weighting function:

	WTFUNC = EXP [ -DIST ** 2 / WEIGHT ]

where:

	DIST ** 2 = [ ( lat (grid) - lat (stn) ) ** 2 +
		( lon (grid) - lon (stn) ) ** 2 * coslsq (grid) ]
	COSLSQ = COS ( lat (grid) ) ** 2
	WEIGHT = 5.051457 * ( DELTAN * 2 / PI ) ** 2
	DELTAN = Station spacing read from grid file analysis block

Near the poles, an approximate calculation of the distance along
a great circle arc is used.

GAMMA, the convergence parameter, is a multiplies the
weights for passes after the first pass.  GAMMA must be within
the range 0 - 1.  Any value outside this range will default to
a value of .3.  If GAMMA is 0, the number of passes will be set
to 1.  The recommended value for GAMMA is .3.

SEARCH is used to control the search radius, which is the
maximum distance that a station may be from a grid point to be
used in the analysis for that point.  The search radius will be
set so that stations whose weighting function would be less than
EXP (-SEARCH) will not be used.  SEARCH must be in the range
1 - 50.  If it is not, a default value of 20 will be used.  If
a very small value is used, many grid points will not have 3
stations within the search area and will be set to the missing
data value.  Entering /EX after the SEARCH value allows
data extrapolation to assign values to grid points on the
periphery of the data region.  This will reduce the number of
grid points with missing values in data void regions.

NPASS controls the number of passes.  Valid values are in the
range 1 - 5.  Note that two passes are STRONGLY RECOMMENDED.

QCNTL is the quality control threshold values.  It is used only
for level parameters when the first-guess field exists.

OABND specifies the bounds file(s) to use for 'blocking'. Multiple
bound areas may be specified via '+' sign between bound names.
The bounds could be either pre-defined bound area or VG files
containing closed lines. The bound blocking results in:
1) all grid points inside of the blocking bounds will be marked
    as MISSED without objective analysis performed on them.
2) all stations inside of the bounds will not be used in analysis.
3) if the line segment between a grid point and a station
    intersects with the boundaries, that station will not be used
    in the analysis for that associated grid point.

 
### Examples
 
1.  Analyze temperature, dewpoint, height and wind components at
the mandatory levels at all time periods.  Also analyze the
lifted and Showalter indices.  If the difference between the
original temperature and height data and the interpolated
first-guess values is greater than 5 C and 50 meters, the
original data will be discarded. The blocking bounds are
specified as closed lines in florida.vgf.
    
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        GDFILE	 =  hbobsnd.grd
        SNPARM	 =  tmpc;dwpc;hght;uwnd;vwnd
        STNDEX	 =  list;show
        LEVELS	 =  man
        VCOORD	 =  pres
        DATTIM	 =  all
        DTAAREA	 =  us-
        GUESS	 =
        GUESFUN  =
        GAMMA	 =  0.3
        SEARCH	 =  20
        NPASS	 =  2
        QCNTL	 =  5;;50
        OABND    =  florida.vgf

### Error Messages
 
    [OABSND +10]    No data from ... will be used.
    [OABSND  +9]    Station parameter ... cannot be computed.
    [OABSND  +8]    Parameter ... is a character parameter.
    [OABSND  +7]    Parameter ... cannot be computed.
    [OABSND  +6]    WARNING:  Area is not DATA area in file.
    [OABSND  +5]    WARNING:  The recommended number of passes is 2.
    [OABSND  +4]    ... is invalid for NPASS.  It is set to 2.
    [OABSND  +3]    ... is invalid for search.  It is set to 20.
    [OABSND  +2]    WARNING:  GAMMA is 0.  There will be only 1 pass.
    [OABSND  +1]    ... is invalid for GAMMA.  It is set to .3 .
    [OABSND  -1]    Fatal error initializing TAE.
    [OABSND  -2]    Fatal error reading TAE parameters.
    [OABSND  -3]    Fatal error initializing GEMPLT.
    [OABSND  -4]    Grid size is too large.
    [OABSND  -5]    Not enough buffer space; reduce number of grids.
    [OABSND  -6]    There are too many stations in data subset area.
    [OABSND  -7]    There are too few stations in data subset area.
    [OABSND  -8]    There are no times in the sounding file.
    [OABSND  -9]    No valid parameters have been entered.
    [OABSND -10]    LEVELS cannot contain a range.
    [OABSND -11]    Sounding file could not be opened.
    [OABSND -12]    Time cannot be found in ... .
