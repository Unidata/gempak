# GDOMEG

GDOMEG computes grids of vertical motion and adds them to the
grid file.


### Input Parameters
 
    GDFILE    Grid file
    GDATTIM   Grid date/time
    GPACK     Packing type/number of bits
 
 
### Program Description

GDOMEG computes kinematic vertical motions from gridded wind
data by vertically integrating the continuity equation in
pressure coordinates.  An O'Brien correction is applied
to assure zero vertical motion at the top pressure
level.

The O'Brien correction is based on the solution of a
variational problem which is formulated to minimize the
squared difference between the observed divergence and the
adjusted divergence while simultaneously satisfying the
isobaric continuity equation with zero vertical motion at
the top.  The error in the divergence is assumed to increase
linearly with decreasing pressure.  The effect of the
correction is to adjust the OMEG values at every level by
a fraction of the excess OMEG at the top pressure level.
The fraction increases from nearly zero at the bottom to one
at the top.

The boundary condition at the surface is:

		OMEG = - G * RHO * DOT ( V, GRAD (Z) )

where, G is the acceleration of gravity, RHO is density,
V is either the surface wind or an estimated surface wind
and Z is the terrain elevation.  If there is insufficient
data, the surface OMEG is set to zero.

The values of omega are computed on the existing pressure
levels. A weighted average in ln p of divergence in the
layer is used in the vertical integration.

The computed grids are in mb/s and are named OMEG.


### Examples
 
1.  Create vertical motion grids in file obs.grd at the last
    time using GRIB packing.

        GDFILE  = obs.grd
        GDATTIM = last
        GPACK   = grib/16

2.  Create vertical motion grids in file model.grd at the
    12-h forecast time.

        GDFILE  = model.grd
        GDATTIM = f12
        GPACK   = grib/16


### Error Messages
 
    [GDOMEG  +2]    No correction pass will be done.
    [GDOMEG  +1]    WARNING--no surface pressure grid exists.
    [GDOMEG  -1]    Fatal error initializing TAE.
    [GDOMEG  -2]    Fatal error reading TAE parameters.
    [GDOMEG  -3]    Error initializing GEMPLT.
    [GDOMEG  -4]    Requested time is not available.
    [GDOMEG  -5]    No pressure levels exist.
    [GDOMEG  -6]    The packing information is erroneous.
    [GDOMEG  -7]    Could not write output grid to the file.
