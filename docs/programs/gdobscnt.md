# GDOBSCNT

GDOBSCNT creates a gridded sampling of the number of surface observations within a specified radius of each grid point.

### Input Parameters
 
    SFFILE    Surface data file
    GDFILE    Grid file
    RADIUS    Radius (in meters) to search
 

### Program Description
 
GDOBSCNT produces a grid for each time found in a surface file.

SFFILE specifies the input surface file. Each station in the
surface file will be checked to see if it is within the
specified radius of every grid point in the output grid file.

GDFILE specifies the output grid file. It must already exist.

RADIUS is the distance in meters for which a surface station
will contribute to the observed density at a grid point.


### Examples
 
1.  Create a grid for each time within the NLDN lightning file
    showing the number of strikes within 50km of each grid point.
    The grid file (obs.grd) has already been created with GDCFIL.

        SFFILE   = nldn
        GDFILE   = obs.grd
        RADIUS   = 50000

### Error Messages

        [GDOBSCNT  0]    Using: 
