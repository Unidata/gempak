# GDSTAT

GDSTAT computes statistics on a time series of scalar grids.


### Input Parameters
 
    GDFILE    Grid file
    GDOUTF    Output grid file
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GRDNAM    Grid parameter name
 
 
### Program Description
 
GDSTAT computes the maximum, minimum, average, standard
deviation, and number of reporting times at each grid point
for a time series of grids.  The output grids will be
written to the output grid file.

The input grid files are specified in GDFILE.  The output
file is specified in GDOUTF.  The output file may be one of
the input files.  A total of three files may be entered.

GFUNC specifies the grid diagnostic function to be computed.
GDATTIM must be a time range.  The output grid names will
be the name in GRDNAM prefixed by MAX, MIN, AVG, STD, and CNT.
If GRDNAM is blank, the default name from the grid diagnostic
computation will be used.


### Examples
 
1.  Compute the grid statistics for the 850 mb vorticity
    field using all the times in the grid file.  Name
    the output grids MAXVOR, MINVOR, AVGVOR, STDVOR and CNTVOR
    and write them into the same file.  Write the output grids
    into the same file.

        GDFILE   =  ngm.grd
        GDOUTF   =  ngm.grd
        GFUNC    =  vor(wnd)
        GVCORD   =  pres
        GLEVEL   =  850
        GDATTIM  =  all
        GRDNAM   =  vor

2.  Compute the grid statistics for the surface temperature
    data using the data for every 12 hours from Jan 1 to
    Jan 15.  The input and output files are different.  The
    default names will be used.

        GDFILE   =  jan90sfc.grd
        GDOUTF   =  stat.grd
        GFUNC    =  tmpc
        GVCORD   =  none
        GLEVEL   =  0
        GDATTIM  =  920101/0000-920115/0000-12
        GRDNAM   =


### Error Messages
 
    [GDSTAT  -1]    Fatal error initializing TAE.
    [GDSTAT  -2]    Fatal error reading TAE parameters.
    [GDSTAT  -3]    Fatal error initializing GEMPLT.
    [GDSTAT  -4]    No valid times found in grid file.
    [GDSTAT  -6]    There are fewer than four grids.
    [GDSTAT  -7]    Error writing grid ... to output file.
    [GDSTAT  -8]    Grid navigation has changed.
