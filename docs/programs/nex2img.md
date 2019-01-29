# NEX2IMG

NEX2IMG creates a GIF format image composite of NEXRAD level III products.

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
    RADFIL    Radar image filename(s)
    LUTFIL    Enhancement lookup table filename
 
 

### Program Description
 
NEX2IMG samples NEXRAD Level III (NIDS) products to a common
grid projection, and then creates a GIF format image raster.
NEX2IMG is not limited by the traditional LLMXGD
limitation for grid files.

NEX2IMG uses a suplemental table `$GEMTBL/unidata/nex2gini.tbl` `to specify configurations for data to pixel mappings.

RADFIL specifies the output file. If the file already exists,
it is overwritten.

CPYFIL may provide either an existing grid file to read the projection
information from, or a grid number (#nnn) defined in `grdnav.tbl`.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if
CPYFIL is blank.

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
will be used with RADTIM and RADDUR to determine which NEXRAD products are in the valid time range.

RADTIM determined the output grid time for the radar composite.
The value of RADTIM may either be 'current', or a GEMPAK dattim.
If 'current' is selected for RADTIM, then the current system clock
time is used. No data files later than RADTIM will be included
in the composite. RADDUR provides the time window previous to
RADTIM in order to include data for each site. The time closest
to RADTIM will be used. A default RADDUR of 30 minutes is
used if RADDUR is blank.

RADFRQ is the frequency in minutes at which the program will run.
When RADFRQ is defined, NEX2IMG will wait for the specified time
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

LUTFIL specifies the color table to be used for the output GIF file.

 
### Examples
 
1.  Create a National composite of NEXRAD base reflectivity (N0R).
    Use the current time with a 30 minute window for data. Rerun the
    mosaic creation continuously with a 5 minute wait period between
    each update. Use the `nexrad.tbl` station table. Create the GIF
    output file using the `YYYYMMDD_HHNN.gif` file name template.
    Accept data from radars operating in precipitation and
    clear air mode.

        GRDAREA  = 25;-125;50;-65
        PROJ     = CED
        KXKY     = 6000;2500
        CPYFIL   =
        GFUNC    = n0r
        RADTIM   = current
        RADDUR   = 30
        RADFRQ   = 5
        STNFIL   = nexrad.tbl
        RADMODE  = PC
        RADFIL   = YYYYMMDD_HHNN.gif
        LUTFIL   = upc_rad24.tbl

### Error Messages
 
    [NEX2IMG  +5]  Mode rejected: ...
    [NEX2IMG  +4]   Write image ...
    [NEX2IMG  +3]   Using default station file ...
    [NEX2IMG  +2]   NEXRIII template not found using ...
    [NEX2IMG  +1]   Too old: ...
    [NEX2IMG  0]   Using:   ...
    [NEX2IMG  -1]   Fatal error initializing TAE.
    [NEX2IMG  -2]   Fatal error reading TAE parameters.
    [NEX2IMG  -3]   Fatal error initializing GEMPLT.
    [NEX2IMG  -4]   Failed to read grid projection ...
    [NEX2IMG  -5]   Error writing to ...
    [NEX2IMG  -8]   Station table ... not found
