# GDLIST

GDLIST lists data from a scalar grid.

### Input Parameters
 
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GDFILE    Grid file
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    SCALE     Scalar scale / vector scale
    OUTPUT    Output device/filename
 
 

### Program Description
 
GDLIST lists a diagnostic grid computed from the grids in a
GEMPAK grid file.

The data may be listed over a subgrid.  The variables GAREA
and PROJ specify the data subset area.  This area is only
approximate; the subgrid selected will cover the subset area.
GAREA must be specified as a geographic area, as an area
centered on a station, as latitude/longitude bounds or as
DSET or GRID.  If DSET or GRID is chosen, the entire grid
will be printed.

Output to the terminal or to a file will be 80 columns wide.
If the output is sent to a file, the file will be named
gdlist.fil if no name is specified.

 
### Examples
 
1. List the 850 mb dewpoint temperature for the 24 hour
forecast time.  The data subset area is a zoomed area
centered on New York.
            
        GDATTIM	 =  /f24
        GLEVEL	 =  850
        GVCORD	 =  PRES
        GFUNC	 =  dwpc
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  ny*
        PROJ	 =  lcc
        SCALE	 =  999
        OUTPUT	 =  T

2. Compute the divergence at 300 mb and list the output
over the Eastern US.  Write the output to a file
called div.east.
    
        GDATTIM	 =  /f24
        GLEVEL	 =  300
        GVCORD	 =  PRES
        GFUNC	 =  div(wnd)
        GDFILE	 =  $GEMPAK/data/hrcbob.grd
        GAREA	 =  east
        PROJ	 =  lcc
        SCALE	 =  999
        OUTPUT	 =  f/div.east

### Error Messages
 
    [GDLIST  +2]    WARNING:  ... not found.  CONTINUING---
    [GDLIST  -1]    Fatal error initializing TAE.
    [GDLIST  -2]    Fatal error reading TAE parameters.
