# GDRADRC

GDRADRC creates a gridded composite of Canadian (ASCII) products.

### Input Parameters
 
    GRDAREA   Area covered by grid
    PROJ      Map projection/angles/margins|drop flag
    KXKY      Number of grid points in x;y
    GDPFUN    Scalar grid or vector grid function
    GDFILE    Grid file
    RADTIM    Radar composite current/dattim
    RADDUR    Radar time window (minutes prior to RADTIM)
    RADFRQ    Update Frequency
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    STNFIL    Station information file
    MAXGRD    Maximum number of grids
    NDVAL     Replacement value for "ND" (default RMISSD)
 
 

### Program Description
 
GDRADRC samples ascii products to a common grid projection.

GDFILE specifies the output grid file. If the file does not already
exist, the file is created using the grid defined by CPYFIL,
or if CPYFIL is not defined then by PROJ, GRDAREA, and KXKY.

CPYFIL may provide either an existing grid file to read the projection
information from, or a grid number (#nnn) defined in `grdnav.tbl`.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if
the output file does not already exist, and CPYFIL is blank.

STNFIL is the station table which supplies radar IDs to be searched
for the composite. If STNFIL is blank, then `cradar.tbl` is used
by default.

GDPFUN is a list of data parameters for which composites are created.
The NEXRAD file naming is assumed to be such that the site identifier
and the product type are both present in the directory/file naming
structure. The `datatype.tbl` template CRADAR is used to provide the
file naming convention used. If CRADAR is not found in the template
database, a default directory structure for ascii radar data is assumed
where the root directory `$RAD/cradar` contains a tree structure supporting
`%SITE%/%PROD%/%PROD%_YYYYMMDD_HHNN` file names. The `%SITE%` template
will be replaced by the site IDs in the STNFIL table. The `%PROD%` will
be replaced by the GDPFUN product name. The GEMPAK data/time template
will be used with RADTIM and RADDUR to determine which NEXRAD products
are in the valid time range.

RADTIM determined the output grid time for the radar composite.
The value of RADTIM may either be 'current', or a GEMPAK dattim.
If 'current' is selected for RADTIM, then the current system clock
time is used. No data files later than RADTIM will be included
in the composite. RADDUR provides the time window previous to
RADTIM in order to include data for each site. The time closest
to RADTIM will be used. A default RADDUR of 30 minutes is
used if RADDUR is blank.

RADFRQ is the frequency in minutes at which the program will run.
When RADFRQ is defined, GDRADR will wait for the specified time
before rerunning. This option is most useful when RADTIM is
set to 'current'. When the program is sleeping, ctrl-c can be
used to exit the loop and return to the dynamic tutor. If
RADFRQ is not set, the dynamic tutor will be re-entered at the
end of processing the radar mosaic.


### Examples
 
Create a composite of CAPPI level1 reflectivity (N1R). Use the 
    current time with a 30 minute window for data. Rerun the mosaic 
    creation continuously with a 5 minute wait period between each 
    update. Use the `cradar.tbl` station table. Create the grid output file using the `YYYYMMDD_cradar.gem` file name template. 

    PROJ     = lcc/50;-110;50
    GRDAREA  = 44;-128;47;-48
    KXKY     = 1700;950
    GDPFUN   = n1r
    GDFILE   = YYYYMMDD_cradar.gem
    RADTIM   = current
    RADDUR   = 30
    RADFRQ   = 5
    CPYFIL   =
    STNFIL   = cradar.tbl
    MAXGRD   = 1000
    NDVAL    = -9999


### Error Messages
 
    [GDRADR  +5]   Mode rejected: ...
    [GDRADR  +4]    Write grid ...
    [GDRADR  +3]    Using default station file ...
    [GDRADR  +2]    CRADAR template not found using ...
    [GDRADR  +1]    Too old: ...
    [GDRADR  0]    Using:   ...
    [GDRADR  -1]    Fatal error initializing TAE.
    [GDRADR  -2]    Fatal error reading TAE parameters.
    [GDRADR  -3]    Fatal error initializing GEMPLT.
    [GDRADR  -4]    Failed to read grid projection ...
    [GDRADR  -5]    Could not create grid file ...
    [GDRADR  -6]    Could not open grid file ...
    [GDRADR  -7]    Warning: navigation block does not match file
    [GDRADR  -8]    Station table ... not found
