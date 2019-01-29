# GDBIINT

GDBIINT interpolates grids from one projection to another

### Input Parameters
 
    GDFILE    Grid file
    GDOUTF    Output grid file
    GFUNC     Scalar grid
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GDATTIM   Grid date/time
    GDNUM     Grid numbers
 
 

### Program Description
 
GDBIINT uses bi-linear interpolation to convert
grid point data from one projection to another.

The output file must be created first using GDCFIL.
The program determines the location of the output
grid point locations withing the input grid file domain.
Each grid in the input file is interpolated to the output
file.

If GDNUM is ALL, all grids from the input file will be
interpolated to the output file. Otherwise, the single grid
specified by GFUNC, GDATTIM, GLEVEL and GVCORD will
be interpolated.


### Examples
 
1.	Convert a data set from grid 211 to grid 87.
First create a new grid file with gdcfil using
`CPYFIL = #87`.

        GDFILE	 =  $HDS/98033112_grid211.gem
        GDOUTF   =  $HDS/98033112_grid87.gem
        GDNUM    =  all

2.	Convert the LAND mask from grid projection 212 to grid
projection 211.

        GDFILE	 =  $HDS/98033112_grid211.gem
        GDOUTF   =  $HDS/98033112_grid87.gem
        GFUNC    =  mask
        GDATTIM  =  f000
        GLEVEL   =  0
        GVCORD   =  none
        GDNUM    =

### Error Messages
 
    [GDBIINT  -1]   Fatal error initializing TAE.
    [GDBIINT  -2]   Fatal error reading TAE parameters.
