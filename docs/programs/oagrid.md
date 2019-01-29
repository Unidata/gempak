# OAGRID

OAGRID creates a GEMPAK grid file which can be used in a Barnes
objective analysis program.

### Input Parameters
 
    GDFILE    Grid file
    DELTAN    Station spacing
    DELTAX    X spacing
    DELTAY    Y spacing
    GRDAREA   Area covered by grid
    EXTEND    Points to extend grid
    DTAAREA   Data area for OA
    SOURCE    Data source (SN or SF)
    SNFILE    Sounding data file
    SFFILE    Surface data file
    SNPARM    Sounding parameter list
    SFPARM    Surface parameter list
    DATTIM    Date/time
    LEVELS    Vertical levels
    MAXGRD    Maximum number of grids
 
 

### Program Description
 
This program allows the user to create a GEMPAK grid file
which contains the information required to perform a Barnes
objective analysis.

The output grids will be evenly spaced latitude/longitude
(CED) grids.  Three areas used by the objective analysis
programs are defined in this program.  GRDAREA defines a
region for the output grid.  The upper right corner
specified will be moved toward the lower left in order
to align it on a grid point.

The second area, defined by EXTEND, is used to extend the
grid area outward by some number of grid points.  This area
is used as a first-pass grid area in the objective analysis.
The default EXTEND values are 2,2,2,2.

DTAAREA defines the area over which station data will be
input to the analysis.  Only data within the EXTEND area are
used for the second pass.  If a value for DTAAREA is not
specified, the EXTEND area will be used.

If values for the station spacing and grid spacings, DELTAN,
DELTAX, and DELTAY, are all specified by the user, they are
stored in the grid file for use by the analysis programs.  If
any of these numbers is 0, a suggested station spacing is
computed using the station data from the file specified.  This
station spacing will be used to compute values for DELTAX and
DELTAY.  The computed station spacing is the average of the
average minimum station spacing and the uniform station
spacing.  These station spacing values are computed using the
stations reporting data for the first DATTIM.  The average
minimum station spacing is the average of the distances from
each station to its closest station.  The uniform spacing is
the spacing that would be found between stations if they were
evenly spaced over the data area.

GDCFIL can also be used to create grid files.  OAGRID creates
only CED grids, but values for the grid spacing and station
spacing are estimated from the input surface or upper air file.
GDCFIL creates grid files for grids in any projection, but
the grid spacing and station spacing must be input directly.

 
### Examples
 
1.	Create a surface grid file called hbobsfc.grd.  Use the
temperature and dewpoint in the surface file hrcbob.sfc to
compute the station spacing.  The grid area is US; use
defaults for the remaining parameters.  Create a file which
can contain up to 200 grids.
    
        GDFILE	 =  hbobsfc.grd
        DELTAN	 =  0
        DELTAX	 =  0
        DELTAY	 =  0
        GRDAREA	 =  us
        EXTEND	 =  2;2;2;2
        DTAAREA	 =
        SOURCE	 =  sf
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        SNPARM	 =
        SFPARM	 =  tmpc;dwpc
        DATTIM	 =  last
        LEVELS	 =  850
        MAXGRD	 =  200

 2.	Create a upper-air grid file called hbobsnd.grd.  Use the
temperature and dewpoint at 500 mb from the upper-air file
hrcbob.snd to compute the station spacing.  Create a file
which can contain up to 2000 grids.
    
        GDFILE	 =  hbobsnd.grd
        DELTAN	 =  0
        DELTAX	 =  0
        DELTAY	 =  0
        GRDAREA	 =  us
        EXTEND	 =  2;2;2;2
        DTAAREA	 =  us
        SOURCE	 =  sn
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        SNPARM	 =  tmpc;dwpc
        SFPARM	 =  tmpc;dwpc
        DATTIM	 =  last
        LEVELS	 =  500
        MAXGRD	 =  2000

### Error Messages
 
    [OAGRID  +1]    WARNING : This grid is too large for GEMPAK programs.
    [OAGRID  -1]    Fatal error initializing TAE.
    [OAGRID  -2]    Fatal error reading TAE parameters.
    [OAGRID  -3]    ... is invalid for GRDAREA.
    [OAGRID  -4]    ... is invalid for DTAAREA.
    [OAGRID  -5]    No data file name specified.
    [OAGRID  -6]    Parameter input is invalid.
    [OAGRID  -7]    Parameter ... cannot be calculated.
    [OAGRID  -8]    Invalid value for DELTAX or DELTAY.
    [OAGRID  -9]    Too few stations to calculate DELTAN.
    [OAGRID -10]    Source must be set to SN or SF.
    [OAGRID -11]    Station data file is invalid.
    [OAGRID -12]    Invalid time requested.
    [OAGRID -13]    Invalid level requested.
