# OABSFC

OABSFC performs a Barnes objective analysis on surface data.

### Input Parameters
 
    SFFILE    Surface data file
    GDFILE    Grid file
    SFPARM    Surface parameter list
    DATTIM    Date/time
    DTAAREA   Data area for OA
    GUESS     Guess file
    GUESFUN   Guess grid
    GAMMA     Convergence parameter
    SEARCH    Search radius/Extrapolation
    NPASS     Number of passes
    QCNTL     Quality control threshold
    OABND     Bounds file(s) to use for 'blocking'
    GDATTIM   Grid date/time
    GFUNC     Scalar grid
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
 
 

### Program Description
 
OABSFC performs a Barnes objective analysis on surface
data.  Multiple parameters may be analyzed at the same
time.  If more than one time is entered, the analyses
will be performed sequentially.

The input surface file is specified in SFFILE.  Up to three
files may be entered by separating the names with a plus, +.
If the time is set to FIRST or LAST, the first or last
time from the first file will be used.

The projection and area for the grid are read from the
navigation block in the output grid file.  The extend grid
area, the station spacing and the data subset area are read
from the analysis block.  The extend area is used to define
a larger grid for the first pass, allowing data to be
interpolated back to stations outside the final grid area.
The average station spacing is used to compute the weighting
functions.  The data subset area specifies which data
will be used in the analysis.  If DTAAREA is blank or set
to DATA, the data area defined in the analysis block will
be used.  This is the recommended procedure.  However,
DTAAREA may be specified in the same way as the AREA
parameter used in other programs.  This is especially useful
to eliminate stations which are known to have bad data.

A first-guess field can be used in the analysis.  The name
of the grid file containing the guess field must be entered
in GUESS,  and the diagnostic functions that compute the
first-guess grids must be entered in GUESFUN.  If a diagnostic
function is not specified, the corresponding surface parameter
will be used.  If the grid file contains the analysis time or
has only one time, that time will be used;  otherwise, a
forecast time that is valid at the analysis time will be computed
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

GAMMA, the convergence parameter, multiplies the weights for
passes after the first pass.  GAMMA must be within the range
0 - 1.  Any value outside this range will default to a value
of .3.  If GAMMA is 0, the number of passes will be set to 1.
The recommended value for GAMMA is .3.

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
when the first-guess field exists.

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

GDATTIM is a list of date/time to be written into the GDFILE.
If it is blank, DATTIM will be used. Only full GEMPAK times or
forecast hours are allowed. For instance:

            050610/1200;050610/1800
            050610/1200F006
            F012;F018
            
If GDATTIM is a forecast hour, DATTIM will be added before the
forecast hour.

GFUNC is the scalar grid function. If it is blank, the surface
parameter SFPARM will be used.

GLEVEL is the vertical level of the grid.

GVCORD is the vertical coordinate of the grid.


### Examples
 
1.  Analyze 15Z temperature, dewpoint, wind components and
    mean sea level pressure in two surface files for the area
    of us-. The first-guess field is from the 3hr forecast of
    NGM.  The output grid is sfc.grd. If the difference between
    the original temperature and pressure data and the
interpolated first-guess values is greater than 5 C and 10 mb,
    the original data will be discarded. The blocking bounds
are specified as closed lines in florida.vgf.

        SFFILE   =  $OBS/hrly/990219.hrly + $OBS/ship/99021915.ship
        GDFILE   =  sfc.grd
        SFPARM   =  tmpc;dwpc;uwnd;vwnd;pmsl
        DATTIM   =  15
        DTAAREA  =  us-
        GUESS    =  $MODEL/ngm/ngm_99021912
        GUESFUN  =  ;;urel@10%hght;vrel@10%hght
        GAMMA    =  0.3
        SEARCH   =  20
        NPASS    =  2
        QCNTL    =  5;;;;10
        OABND    =  florida.vgf
        GDATTIM  =  050706/1530
        GFUNC    =  tmpc;tmpd
        GLEVEL   =  0
        GVCORD   =  none

2.  Repeat the analysis eliminating the data at stations DCA
and BUF.

        DTAAREA =  data/-@dca;buf

### Error Messages
 
    [OABSFC +11]    Surface file ... cannot be opened.
    [OABSFC +10]    There are no times in file ....
    [OABSFC  +9]    No data from ... will be used.
    [OABSFC  +8]    Parameter ... is a character parameter.
    [OABSFC  +7]    Parameter ... cannot be computed.
    [OABSFC  +6]    WARNING:  Area is not DATA area in file.
    [OABSFC  +5]    WARNING:  The recommended number of passes is 2.
    [OABSFC  +4]    ... is invalid for NPASS.  It is set to 2.
    [OABSFC  +3]    ... is invalid for search.  It is set to 20.
    [OABSFC  +2]    WARNING:  GAMMA is 0.  There will be only 1 pass.
    [OABSFC  +1]    ... is invalid for GAMMA.  It is set to .3.
    [OABSFC  -1]    Fatal error initializing TAE.
    [OABSFC  -2]    Fatal error reading TAE parameters.
    [OABSFC  -3]    Fatal error initializing GEMPLT.
    [OABSFC  -4]    Grid size is too large.
    [OABSFC  -5]    Not enough buffer space; reduce number of grids.
    [OABSFC  -6]    There are too many stations in data subset area.
    [OABSFC  -7]    There are too few stations in data subset area.
    [OABSFC  -8]    There are no times in the surface files.
    [OABSFC  -9]    No valid parameters have been entered.
    [OABSFC -10]    No surface file could be opened.
    [OABSFC -11]    Time cannot be found in ....
    [OABSFC -12]    Number of GDATTIM mismatches number of DATTIM

