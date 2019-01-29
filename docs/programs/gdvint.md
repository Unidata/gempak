# GDVINT

GDVINT performs interpolation between vertical coordinates.

### Input Parameters
 
    GDFILE    Grid file
    GDOUTF    Output grid file
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GLEVEL    Grid level
    MAXGRD    Maximum number of grids
    GAREA     Graphics area
    VCOORD    Vertical coordinate type
 
 

### Program Description
 
This program performs vertical interpolations from one
input vertical coordinate to another.  The output grid
file may be the same as the input file or be a separate
file.  If areal subsetting is specified, output must be
to another GEMPAK file.

The input file is specified in GDFILE and the output file
in GDOUTF.  If GDOUTF does not exist, then a new file
will be created with the maximum number of grids given
in MAXGRD.  GDATTIM specifies the time at which to
perform the interpolation.

GVCORD specifies the input and output vertical
coordinates separated by a slash.  The coordinates
supported are:
    
    PRES	--	pressure
    THTA	--	potential temperature
    HGHT 	--	height above sea level
    SGMA	--	sigma p
    ETA	--	eta
    ZAGL	--	height above ground level

When SGMA is specified, the pressure on SGMA=0 is
given following a semicolon.  For example, if the
pressure at SGMA=0 is 50 mb, the specification in
GVCORD is

    SGMA:50

GLEVEL specifies the vertical coordinate levels to
interpolate.  This may be a single level, a list of
levels separated by semicolons or a range with an
increment separated by dashes.

GAREA specifies areal subsetting.  It can be specified
as either lower-left and upper-right grid
coordinates, separated by semicolons and preceded by
a single @, or as lower-left and upper-right latitude
and longitude coordinates separated by semicolons.
In the latter case, the locations are rounded to the
nearest grid points.  The navigation projection and its
parameters are used along with the specified corner
points to set the navigation in the output grid file.
If areal subsetting is used, the output file must be
different from the input file.

VCOORD specifies a list of GEMPAK coordinate names.
All grids at the specified time having one of these
coordinate names will be transfered to the output
file.  This allows surface grids with vertical
coordinate types NONE or ESFC to be transfered into
the output file.  It also allows subsetting of
grids in other vertical coordinates to be put into
the output file.  VCOORD is also used to specify
the Lorenz condition for interpolation into THTA
coordinate.  To specify the Lorenz condition,
conclude the VCOORD input with /L.

If the output vertical coordinate is THTA and
the Lorenz condition is specified, underground
isentropic surfaces are assigned surface values,
and PSYM is built down hydrostatically using the
surface pressure in the Exner function.  In all
other cases, underground surfaces are assigned
missing values.

If the output vertical coordinate is PRES, underground
pressure levels are assigned a height value computed
by extrapolating downward using the surface values
of pressure, temperature, and height.  A constant
lapse rate atmosphere with the standard lapse rate of
6.5 K / 1000 m is assumed.

If surface values of the interpolated parameters
are not available, GDVINT extrapolates values to the
surface from two levels above the surface.  These
values are used in the interpolation and are placed
in the output file where they are assigned the
output vertical coordinate and GLEVEL=0.

If heights are not available in the input grid file,
a hydrostatic integration is done in the input coordinate
to generate values for the interpolation.


### Examples
 
1.  Interpolate from sigma, with P(SGMA=0) = 0 to
 theta over a subgrid defined by the latitude
 and longitude bounds.  The actual subgrid will
 be defined by the grid points closest to the
 latitude longitude bounds.  Also transfer all
 grids with vertical coordinates NONE, MSLV and
 ESFC into the output file.  Apply the Lorenz
 condition for underground surfaces.

        GDFILE  = z00jun16.ngmsgm
        GDOUTF  = z00jun16.ngmtht
        GDATTIM = f24
        GLEVEL  = 256-412-4
        GVCORD  = sgma:0/thta
        MAXGRD  = 2000
        GAREA   = 30;-100;45;-70
        VCOORD  = none;mslv;esfc/L

2.  Interpolate from pressure to the height above
 ground coordinate over a subset of the grid
 specified by grid index bounds.
        
         GDFILE  = z00jun16.ngmprs
         GDOUTF  = z00jun16.ngmzag
         GDATTIM = f36
         GLEVEL  = 500-15000-500
         GVCORD  = pres/zagl
         MAXGRD  = 3200
         GAREA   = @12;23;34;78
         VCOORD  = mslv;esfc

3.  Interpolate from isentropic coordinates to sigma
 (ptop=50) over the entire grid.  Put the output
 in the same file as the input.  Note that sigma
 values are scaled by 10000.  In this example,
 maxgrd is ignored.

        GDFILE   = z00jun16.ngm
        GDOUTF   = z00jun16.ngm
        GDATTIM  = f48
        GLEVEL   = 9500-500-500
        GVCORD   = thta/sgma:50
        MAXGRD   = 3200
        GAREA    = grid
        VCOORD   = none;esfc

### Error Messages
 
    [GDVINT  +4]    Warning--no surface elevation grid exists.
    [GDVINT  +3]    No pressure on this level.
    [GDVINT  +2]    No output vertical coordinate value on this level.
    [GDVINT  +1]    All values are missing.
    [GDVINT  -1]    Fatal error initializing TAE.
    [GDVINT  -2]    Fatal error reading TAE parameters.
    [GDVINT  -3]    Fatal error initializing GEMPLT.
    [GDVINT  -4]    No thermodynamic data in the input file.
    [GDVINT  -5]    Navigation blocks are different.
    [GDVINT  -6]    Required surface pressure is absent.
    [GDVINT  -7]    Required surface elevation is absent.
    [GDVINT  -8]    Too few levels to interpolate this parameter.
    [GDVINT  -9]    No parameters to interpolate -- check GVCORD, GDATTIM.
    [GDVINT -10]    Top pressure for SGMA or ETA missing.
    [GDVINT -11]    Subset region is invalid.
    [GDVINT -12]    File ... cannot be opened.
    [GDVINT -13]    User specified coordinates are invalid.
    [GDVINT -14]    Levels not correctly specified.
    [GDVINT -15]    Grid size is too large.
