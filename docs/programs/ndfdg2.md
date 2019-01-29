# NDFDG2

NDFDG2 converts NDFD gridded data in GRIB2 files to GEMPAK gridded data.

### Input Parameters
 
    GBFILE    GRIB data file name
    GDOUTF    Output grid file
    MAXGRD    Maximum number of grids
    GAREA     Graphics area
    GSKIP     Skip factor for GRIB2 files
    OUTPUT    Output device/filename
    OVERWR    Overwr flag
 
 

### Program Description

NDFDG2 will convert NDFD gridded data which is in GRIB2 files to
gridded data in a GEMPAK file.

The GRIB2 input file is specified in GBFILE.

The GEMPAK output file is given in GDOUTF.  This file will
be opened if it exists and the projection information
matches the GRIB2 message projection.  If the output filename is
"LIST", the GRIB file will be scanned and the decoded GEMPAK
header information will be printed out.

GAREA is used to subset the grid.  It may be specified as
any valid input for GAREA which are equivalent to lat/lon bounds
or as a single @ followed by the lower-left and upper-right grid
point coordinates separated by semicolons.  In the former case,
the locations are rounded to the nearest grid points.  The
projection type and angles entered as described above along with
the lat/lon coordinates of the corners of the subset define a
new grid navigation different from that of the full grid.  The
navigation entered as described in the preceding paragraph is
always that of the full grid.  To get the full grid, set GAREA to
blank, GRID or DSET.

GSKIP is used to reduce the resolution of a grid.  This may be
required if the GRIB2 full resolution grid exceeds the GEMPAK
maximum grid size.

OUTPUT defines the direction and destination of printed output.

OVERWR is a logical flag that allows the user to overwrite the existing
GEMPAK grid file.  If OVERWR is YES, then the grid file is being
overwritten.  Otherwise, it remains the same.


### Examples
 
1.  Convert the data in the GRIB2 file for the MESO model.
    
        GBFILE   =  /tmp/model/meso.mdl
        GDOUTF   =  /tmp/model/meso.mdl.gem
        MAXGRD   =  3000
        GAREA    =  KY
        GSKIP	 =
        OUTPUT   =  t

2.  Scan a GRIB2 file and print out the message info.

        GBFILE   =  ensemble.grib
        GDOUTF   =  LIST
        MAXGRD   =  3999
        GAREA    =  KY
        GSKIP	 =
        OUTPUT   =  t

3.  Skip every two grid points in X and Y for whole grid.


        GBFILE   =  /tmp/model/meso.mdl
        GDOUTF   =  /tmp/model/meso.mdl.gem
        MAXGRD   =  3000
        GAREA    =
        GSKIP    =  2
        OUTPUT   =  t

4.  Skip every other grid point in X and Y for GAREA.

        GBFILE   =  ensemble.grib
        GDOUTF   =  ensemble.grib.gem
        MAXGRD   =  3000
        GAREA    =  MD
        GSKIP    =  1
        OUTPUT   =  t

5.  Overwrite the existing GEMPAK grid file..

        GBFILE   =  /tmp/model/meso.mdl
        GDOUTF   =  /tmp/model/meso.mdl.gem
        MAXGRD   =  3000
        GAREA    =
        GSKIP    =
        OUTPUT   =  t
        OVERWR   = yes


### Error Messages
 
    [NDFDG2  +5]    GRIB version 0 cannot be decoded.
    [NDFDG2  +4]    Parameter not found in table: ...
    [NDFDG2  +3]    GRIB2 unpacking warning ... returned from unpk_grib2.
    [NDFDG2  +2]    Vertical coordinate ... not supported.
    [NDFDG2  +1]    WARNING: This grid is too large for GEMPAK programs.
    [NDFDG2  -1]    Fatal error initializing TAE.
    [NDFDG2  -2]    Fatal error reading TAE parameters.
    [NDFDG2  -3]    Fatal error initializing GEMPLT.
    [NDFDG2  -4]    Grid Definition Template number ... not supported.
    [NDFDG2  -5]    Grid area ... is invalid.
    [NDFDG2  -6]    Grid size is invalid.
    [NDFDG2  -7]    The grid file name may not be blank.
    [NDFDG2  -8]    Source of Grid Definition ... not supported.
    [NDFDG2  -9]    Fatal GRIB2 unpacking error ... returned from unpk_grib2.
    [NDFDG2 -10]    Input for GEMPAK output file is blank.
    [NDFDG2 -11]    Error opening GEMPAK grid file.
    [NDFDG2 -12]    Error creating GEMPAK grid file.
    [NDFDG2 -13]    Discipline number ... not supported.
    [NDFDG2 -14]    Data type ... not supported.
    [NDFDG2 -15]    Error opening GRIB file.
    [NDFDG2 -16]    Error getting next message.
    [NDFDG2 -17]    Error setting date/time.
    [NDFDG2 -18]    Invalid bitmap specification.
    [NDFDG2 -19]    Error reading GRIB file.
    [NDFDG2 -20]    Invalid input for GAREA.
    [NDFDG2 -21]    Grid is too large.
    [NDFDG2 -22]    Local section present, but not supported.
    [NDFDG2 -23]    No data present in grib message.
    [NDFDG2 -24]    GSKIP too large for size of grid.
