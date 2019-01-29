# GDGSFC

GDGSFC computes grid data and interpolates to stations in a
GEMPAK surface file.

### Input Parameters
 
    GDFILE    Grid file
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GLEVEL    Grid level
    GFUNC     Scalar grid
    SCALE     Scalar scale / vector scale
    SFFILE    Surface data file
    SFPARM    Surface parameter list
 
 

### Program Description
 
This program interpolates computed grid data to stations
in a GEMPAK surface file.

The program computes the requested grid data from the given
grid file.  GFUNC can be any valid GEMPAK grid function.  The
other grid related parameters are used to specify the grid
to calculate.

The output surface file, SFFILE, must already exist.  The
output parameter, SFPARM, must also exist in the surface file.
The program reads each successive station from the surface file
and overwrites the existing data with the interpolated data from
the grid.


### Examples
 
1.  Read the surface temperature in Celsius from the 12 hour
   forecast from the latest ETA model and interpolate the
   values to the stations in the surface file new.sfc. The
   scaling fator is defaulted to 0.

        GDFILE  = eta
        GDATTIM = f12
        GVCORD  = none
        GLEVEL  = 0
        GFUNC   = tmpc
        SCALE   =
        SFFILE  = new.sfc
        SFPARM  = tmpc
        

2.  Read the 500 mb vorticity from the 24 hour forecast from the
   latest GFS model and interpolate the values to the stations
   in the surface file vort.sfc with the scaling factor equal
   to 5.

        GDFILE  = gfs
        GDATTIM = f24
        GVCORD  = pres
        GLEVEL  = 500
        GFUNC   = vor(wnd)
        SCALE   = 5
        SFFILE  = vort.sfc
        SFPARM  = v500


### Error Messages
 
    [GDGSFC  -1]    Fatal error initializing TAE.
    [GDGSFC  -2]    Fatal error reading TAE parameters.
    [GDGSFC  -3]    Fatal error initializing GEMPLT.
    [GDGSFC  -4]    Cannot find SFPARM "..." in this surface file.
