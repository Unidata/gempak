# NEXRCOMP

NEXRCOMP creates a GINI format image composite of NEXRAD level III products.

### Input Parameters
 
    GRDAREA   Area covered by grid
    PROJ      Map projection/angles/margins|drop flag
    KXKY      Number of grid points in x;y
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    GFUNC     Scalar grid
    RADTIM    Radar composite current/dattim
    RADDUR    Radar time window (minutes prior to RADTIM)
    RADFRQ    Update Frequency
    STNFIL    Station information file
    RADMODE   Radar operational mode
    SATFIL    Radar image filename(s)
    GDFILE    Grid file
    NDVAL     Replacement value for "ND" (default RMISSD)
    MAXGRD    Maximum number of grids
    COMPRESS  Write output in compressed format
 
 

### Program Description
 
NEXRCOMP samples NEXRAD Level III (NIDS) products to a common
grid projection. Then creates a GINI format image with
appropriate header information using the grid as the
image raster, before saving the grid to a GEMPAK grid file.
NEXRCOMP is not limited by the traditional LLMXGD
limitation for grid files.

NEXRCOMP uses a suplemental table `$GEMTBL/unidata/nex2gini.tbl` to specify 
configurations for product identifiers so that the generated images
are correctly identified for enhancement tables in `$GEMTBL/sat/imgtyp.tbl`.

SATFIL specifies the output GIF file. If the file already exists,
it is overwritten.

GDFILE specifies the output grid file. If the file does not already
exist, the file is created using the grid defined by CPYFIL,
or if CPYFIL is not defined then by PROJ, GRDAREA, and KXKY.

CPYFIL may provide either an existing grid file to read the projection
information from, or a grid number (#nnn) defined in `grdnav.tbl`.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if
CPYFIL is blank. The GINI format has additional restrictions
which limit which GEMPAK projections may be used. These are,
LCC tangent projection (eg la1 and la2 are the same), STR, and CED.

STNFIL is the station table which supplies radar IDs to be searched
for the composite. If STNFIL is blank, then `nexrad.tbl` is used
by default.

GFUNC is the data parameter which the composite is created for.
The NEXRAD file naming is assumed to be such that the site identifier
and the product type are both present in the directory/file naming
structure. The `datatype.tbl` template NEXRIII is used to provide the
file naming convention used. If NEXRIII is not found in the template
database, a default directory structure for NEXRAD data is assumed
where the root directory `$RAD/NIDS` contains a tree structure supporting
`%SITE%/%PROD%/%PROD%_YYYYMMDD_HHNN` file names. The `%SITE%` template
will be replaced by the site IDs in the STNFIL table. The `%PROD%` will
be replaced by the GFUNC product name. The GEMPAK data/time template
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
When RADFRQ is defined, NEXRCOMP will wait for the specified time
before rerunning. This option is most useful when RADTIM is
set to 'current'. When the program is sleeping, ctrl-c can be
used to exit the loop and return to the dynamic tutor. If
RADFRQ is not set, the dynamic tutor will be re-entered at the
end of processing the radar mosaic.

RADMODE allows the user to select whether to include radar data
from sites operating in (P) precipitation/storm mode, (C) clear
air mode, and/or (M) maintainence mode. The default, if none
are specified is data from all 3 modes (PCM). Multiple modes
may be specified.

COMPRESS allows the user to optionally write the GINI file using PNG
compression for the image raster. The GINI header will identify the
image as a compressed product.


### Examples
 
1.  Create a 1km National composite of NEXRAD base reflectivity (N0Q).
    Use the current time with a 30 minute window for data. Rerun the
    mosaic creation continuously with a 5 minute wait period between
    each update. Use the `nexrad.tbl` station table. Create the 
    output file using the `rad_YYYYMMDD_HHNN` file name template.
    Accept data from radars operating in precipitation and
    clear air mode. 

        GRDAREA  = 23;-120;47.2634;-63.5664
        PROJ     = lcc/40;-100;40
        KXKY     = 4736;3000
        CPYFIL   =
        GFUNC    = n0q
        RADTIM   = current
        RADDUR   = 30
        RADFRQ   = 5
        STNFIL   = nexrad.tbl
        RADMODE  = PC
        SATFIL   = rad_YYYYMMDD_HHNN
        GDFILE   = YYYYMMDD_radr.gem
	    MAXGRD   = 1000

### Error Messages
 
    [NEXRCOMP  +5] Mode rejected: ...
    [NEXRCOMP  +4]  Write image ...
    [NEXRCOMP  +3]  Using default station file ...
    [NEXRCOMP  +2]  NEXRIII template not found using ...
    [NEXRCOMP  +1]  Too old: ...
    [NEXRCOMP  0]  Using:   ...
    [NEXRCOMP  -1]  Fatal error initializing TAE.
    [NEXRCOMP  -2]  Fatal error reading TAE parameters.
    [NEXRCOMP  -3]  Fatal error initializing GEMPLT.
    [NEXRCOMP  -4]  Failed to read grid projection ...
    [NEXRCOMP  -8]  Station table ... not found
