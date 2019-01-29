# GDCFIL

GDCFIL creates a GEMPAK grid file.

### Input Parameters
 
    GDOUTF    Output grid file
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
    MAXGRD    Maximum number of grids
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    ANLYSS    Grid analysis block
 
 

### Program Description

This program creates a GEMPAK grid file.  Information about
the file to be created is received from the input values,
from the grid navigation table, or from an existing grid file.

Each grid file must contain navigation information.  It
may also contain analysis block information which is used
when an objective analysis program writes to the grid file.

If the value in CPYFIL begins with a #, the rest of the
string specifies a grid number or name contained in the
grid navigation table. Any other non-blank entry in CPYFIL
is taken as the name of an existing grid file, whose navigation
and analysis blocks will be copied to the new grid file.

If CPYFIL is blank, the navigation and analysis information
will be obtained from PROJ, GRDAREA, KXKY, and ANLYSS.  KXKY
contains the number of points in the x and y directions.
The two integers must be separated with a semicolon.  If
the grid projection type specified in PROJ is CED, the
grid spacing consisting of deltax and deltay may be entered
in KXKY by prefixing the numbers with a #.  DELTAX and DELTAY
are in degrees longitude and degrees latitude, respectively.
The analysis information specifies DELTAN, which is the
station spacing, and the extend area separated by a slash.
The extend area is four integers separated by semicolons.  If
ANLYSS is blank, default values will be assigned.  If there
is no analysis block in the file to be copied, ANLYSS is
used to create one.

MAXGRD is the maximum number of grids which will be allowed
in the grid file.


### Examples
 
1.  Create a grid file called lfm.grd from the information in
   the grid navigation table, allowing up to 1000 grids in the
   file.  PROJ, GRDAREA and KXKY are ignored since this
   information will be obtained from the grid navigation
   table.
        
        GDOUTF  = lfm.grd
        PROJ    =
        GRDAREA =
        KXKY    =
        MAXGRD  = 1000
        CPYFIL  = #lfm
        ANLYSS  =

2.  Create a grid file called sound.grd with a maximum of 15
   grids.  The grid dimensions are 20 by 30; the area
   is US in a Mercator projection.  The station
   spacing is 5 degrees of latitude and the grid extension
   for objective analysis is 3 grid points in each direction.
        
        GDOUTF  = sound.grd
        PROJ    = mer
        GRDAREA = us
        KXKY    = 20;30
        MAXGRD  = 15
        CPYFIL  =
        ANLYSS  = 5/3;3;3;3

3.  Create a file for evenly spaced lat/lon grids over area IL-
   using a grid spacing of .75 degrees in each direction.
   Compute reasonable values for the analysis block.
        
        GDOUTF  = il.grd
        PROJ    = ced
        GRDAREA = il-
        KXKY    = #.75;.75
        MAXGRD  = 100
        CPYFIL  =
        ANLYSS  =

4.  Create a grid file called new.grd from the information
   within grid file old.grd.
        
        GDOUTF  = new.grd
        PROJ    =
        GRDAREA =
        KXKY    =
        MAXGRD  = 100
        CPYFIL  = old.grd
        ANLYSS  =


### Error Messages
 
    [GDCFIL  +1]    WARNING: This grid is too large for GEMPAK programs.
    [GDCFIL  -1]    Fatal error initializing TAE.
    [GDCFIL  -2]    Fatal error reading TAE parameters.
    [GDCFIL  -3]    Fatal error initializing GEMPLT.
    [GDCFIL  -4]    Navigation information is invalid.
    [GDCFIL  -5]    Grid area ... is invalid.
    [GDCFIL  -6]    Grid size is invalid.
    [GDCFIL  -7]    The grid file name may not be blank.
    [GDCFIL  -8]    Navigation table cannot be read.
    [GDCFIL  -9]    Grid name ... cannot be found in grid table.
    [GDCFIL -10]    Extend region is invalid.  Try 2;2;2;2.
