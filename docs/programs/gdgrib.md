# GDGRIB

GDGRIB computes a scalar diagnostic grid and adds it to a
GRIB file.

### Input Parameters
 
    GDFILE    Grid file
    GFUNC     Scalar grid
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GBTBLS    Input GRIB decoding tables
    GBFILE    GRIB data file name
    VERCEN    PDS byte_4/byte_5/byte_6/byte_26
    PDSVAL    GRIB PDS grid identifier overrides
    PRECSN    Packing precision
    WMOHDR    WMO_ID/Origin_ID/DDHHMM
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
 
 

### Program Description
 
GDGRIB computes a diagnostic grid and saves the result in
a GRIB file.

The input grids for the computation must be in GDFILE.  The
resulting GRIB message is added to the output file, GBFILE.
If GBFILE does not exist, it will be created.

The parameters PDSVAL and VERCEN allow control over how the
grid is identified in the Product Definition Section (PDS)
of the GRIB message.  The PRECSN parameter allows the user
to specify the precision of the data packing in two different
ways:  either in terms of binary precision with rounding to
the nearest power of two or decimal precision in terms of the
number of significant digits to preserve.

A World Meteorological Organization (WMO) header may be
prefixed to the GRIB message by giving at least the first
six bytes of the header in the WMOHDR input parameter.
Default values will be supplied for the originating center
(KWBC) and reference time.

GDGRIB will interpolate scalar fields horizontally to a
different output grid.  The navigation for the output grid is
specified using CPYFIL (see below) or by setting the PROJ,
GRDAREA, and KXKY parameters.  The Grid Description Section
(GDS) in the GRIB message will represent this navigation.

Do not attempt to interpolate grid-relative vector components
to another grid because this requires a rotation of a vector,
which cannot be done without both vector components and is
beyond the capability of the current version of GDGRIB.  North-
relative components may be interpolated.  Wind direction should
NOT be interpolated in any case.  GDS byte 17 is currently set
for north-relative (meteorological coordinate) wind components
if the output grid navigation (specifically, the projection type,
central longitude, and true latitudes) is different from that
of the input grid.  If the output grid navigation is not changed,
always specify grid-relative wind components in GFUNC when
writing wind components to GRIB messages.

CPYFIL provides the only means of setting PDS byte number 7
(grid identification number) to something other than 255,
which indicates that the grid is defined in the GDS.  CPYFIL
may be set to the name of a GEMPAK grid file (other than the
file specified in GDFILE), in which case the output grid is
interpolated to the grid whose navigation is defined in the
GEMPAK grid file; however, byte number 7 will be set to 255
and the navigation will be available only from the GDS.  If
CPYFIL is set to #NNN, grid number NNN is found in `grdnav.tbl`,
and NNN is less than 255, then PDS byte number 7 will be set to
NNN.  Even in this case, a GDS is provided.

The CPYFIL parameter supersedes PROJ, GRDAREA, and KXKY input.
If CPYFIL is valid, PROJ, GRDAREA, and KXKY are ignored.  If
CPYFIL is invalid, PROJ, GRDAREA, and KXKY will be used if they
are valid.

GBTBLS allows for specification of the GRIB decoding tables.
The defaults are:

	$GEMTBL/grid/wmogribX.tbl,
	$GEMTBL/grid/ncepgribX.tbl,
	$GEMTBL/grid/vcrdgrib1.tbl,

where X is replaced by the value of byte 4 of the PDS, which,
if not specified in VERCEN, defaults to 2.

 
### Examples
 
1.  Compute the average absolute vorticity of the 250- and 300-
    mb wind for the 24-h forecast.  Assign this to the 275-mb
    level in the GRIB message PDS.  Also assign the appropriate
    parameter number for absolute vorticity (AVOR).  Use decimal
    precision to pack the data so as to preserve 4 decimal
    significant digits.  Change PDS bytes 6 and 26 to reflect
    that the data source is from a high resolution eta model
    run diagnosed at the Storm Prediction Center (sub-center
    number 9).  Interpolate the grid to standard AWIPS grid
    212, without using CPYFIL so that PDS byte 7 will be 255.
    Do not make a WMO header.  The output is written to file
    hrcbob.pgrb.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GFUNC    = avg(avor(wind@300),avor(wind@250))
        GDATTIM  = f24
        GLEVEL   = 300
        GVCORD   = pres
        GBTBLS   =
        GBFILE   = hrcbob.pgrb
        VERCEN   = //110/9
        PDSVAL   = AVOR@275
        PRECSN   = d/4
        WMOHDR   =
	    CPYFIL   =
        PROJ     = lcc/25;-95;25
        GRDAREA  = 12.190;-133.459;57.290;-49.385
        KXKY     = 185;129

2.  Transfer the 24-h forecast of the 500-mb temperature grid
    into a GRIB file.  Use binary precision to pack the data
    to the nearest 1/8 K.  The output GRIB message is to be
    added to existing file hrcbob.pgrb.  Accept default
    entries for the PDS, with the parameter number coming from
    a specific lookup table (`wmogrib3.tbl`) in `$GEMTBL`.  Also,
    add the WMO header for 500-mb temperature destined for
    Family of Services, and interpolate to grid 212 using CPYFIL
    so that PDS byte 7 is set to 212.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GFUNC    = tmpk
        GDATTIM  = f24
        GLEVEL   = 500
        GVCORD   = pres
        GBTBLS   = wmogrib3.tbl
        GBFILE   = hrcbob.pgrb
        VERCEN   =
        PDSVAL   =
        PRECSN   = b/-3
        WMOHDR   = HTRE50
	    CPYFIL   = #212
        PROJ     =
        GRDAREA  =
        KXKY     =

3.  Transfer the tropopause temperature from a 24-h model forecast
    into a GRIB file. Reassign the parameter and the vertical
    coordinate name properly to treat this as a tropopause
    temperature.  Use the default parameter lookup table for
    the GRIB parameter (TMPK) identification number.  Use the
    vertical coordinate table named `vcrdgrib1.tbl` in `$GEMTBL` to
    lookup the vertical coordinate identification number.  Do not
    do horizontal interpolation, but make sure that PDS byte 7
    denotes that this is grid number 6.  Since `hrcbob.grd` data is
    already on grid #6, GDGRIB will detect this and not do
    interpolation.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GFUNC    = tmpktpps
        GDATTIM  = f24
        GLEVEL   = 0
        GVCORD   = none
        GBTBLS   = ;;vcrdgrib1.tbl
        GBFILE   = tmpk_trop.grb
        VERCEN   =
        PDSVAL   = tmpk%trop
        PRECSN   = b/-3
        WMOHDR   =
	    CPYFIL   = #6
        PROJ     =
        GRDAREA  =
        KXKY     =

4.  Transfer the 12-hour precipitation at forecast hour 18 to
    a GRIB file.  Specifically denote the generating process
    identifier as 84.  Interpolate to 1- by 1-degree resolution
    global grid #3.  Note that many points on this grid will
    have missing values.  Pack the grid to a precision of 1/4
    millimeter.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GFUNC	= p12m
        GDATTIM	= f18
        GLEVEL	= 0
        GVCORD	= none
        GBTBLS	=
        GBFILE	= p12m.grb
        VERCEN	= //84
        PDSVAL	=
        PRECSN	= b/-2
        WMOHDR	=
        CPYFIL	= #3
        PROJ	=
        GRDAREA	=
        KXKY	=

5.  Compute an "off-time" 6-hour precipitation amount using the
    average of two accumulations.  Use the PDSVAL in-line time
    specification given after ^ to assign the GRIB time properly
    as a 21-hour forecast.  Note the use of the 4-digit year in
    the in-line specification in PDSVAL.  Interpolate to high-
    resolution grid 215 so that PDS byte 7 is set to 215.  Use
    default packing precision.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GFUNC	= avg(P06M^F24,P06M^F18)
        GDATTIM	= f18
        GLEVEL	= 0
        GVCORD	= none
        GBTBLS	=
        GBFILE	= p06m.grb
        VERCEN	=
        PDSVAL	= P06M^19910819/0000F21
        PRECSN	=
        WMOHDR	=
        CPYFIL	= #215
        PROJ	=
        GRDAREA	=
        KXKY	=

6.  Transfer the 12-hour forecast of the 850-mb grid-relative
    U-wind component to a GRIB file.  Pack the data to the
    nearest 1/8 m/s.  DO NOT INTERPOLATE!

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GFUNC	= urel
        GDATTIM	= f12
        GLEVEL	= 850
        GVCORD	= pres
        GBTBLS	=
        GBFILE	= wind1.grb
        VERCEN	=
        PDSVAL	=
        PRECSN	= b/-3
        WMOHDR	=
        CPYFIL	=
        PROJ	=
        GRDAREA	=
        KXKY	=

7.  Complete the transfer of the 850-mb wind by transfering
    the grid-relative V-wind component.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GFUNC	= vrel
        GDATTIM	= f12
        GLEVEL	= 850
        GVCORD	= pres
        GBTBLS	=
        GBFILE	= wind1.grb
        VERCEN	=
        PDSVAL	=
        PRECSN	= b/-3
        WMOHDR	=
        CPYFIL	=
        PROJ	=
        GRDAREA	=
        KXKY	=

8.  Transfer the 12-hour forecast of the 850-mb north-relative
    U-wind component to a GRIB file.  Interpolate to grid 212.
Make sure that it is labeled as a u-wind component in the
    GRIB PDS.  Pack the data to the nearest 1/8 m/s.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GFUNC	= un(wind)
        GDATTIM	= f12
        GLEVEL	= 850
        GVCORD	= pres
        GBTBLS	=
        GBFILE	= wind2.grb
        VERCEN	=
        PDSVAL	= 33
        PRECSN	= b/-3
        WMOHDR	=
        CPYFIL	= #212
        PROJ	=
        GRDAREA	=
        KXKY	=

9.  Complete the transfer of the 850-mb wind by transfering
    the north-relative V-wind component.  Note that this wind
    will not be handled correctly by NAGRIB because NAGRIB
    disregards GDS byte 17 and always assumes grid relative
    vectors.  To display these winds correctly after running
    NAGRIB, use the following specification in grid diagnostic
    programs:  VECN(UREL,VREL).

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GFUNC	= vn(wind)
        GDATTIM	= f12
        GLEVEL	= 850
        GVCORD	= pres
        GBTBLS	=
        GBFILE	= wind2.grb
        VERCEN	=
        PDSVAL	= 34
        PRECSN	= b/-3
        WMOHDR	=
        CPYFIL	= #212
        PROJ	=
        GRDAREA	=
        KXKY	=

### Error Messages
 
    [GDGRIB  +6]    WMO header center ID inconsistent with PDS byte 5.
    [GDGRIB  +5]    CPYFIL not used to determine GRIB GDS navigation.
    [GDGRIB  +4]    Warning:  grid not found in grdnav.tbl...continuing.
    [GDGRIB  +3]    Warning:  decoding error in grdnav.tbl...continuing.
    [GDGRIB  +2]    Warning:  error reading grdnav.tbl...continuing.
    [GDGRIB  +1]    Warning:  cannot open grdnav.tbl...continuing.
    [GDGRIB  -1]    Fatal error initializing TAE.
    [GDGRIB  -2]    Fatal error reading TAE parameters.
    [GDGRIB  -3]    Error initializing GEMPLT.
    [GDGRIB  -4]    Grid file could not be opened.
    [GDGRIB  -5]    Grid navigation could not be set in GEMPLT.
    [GDGRIB  -6]    Grid diagnostics package initialization failed.
    [GDGRIB  -7]    Error interpolating to output grid.
    [GDGRIB  -8]    GRIB message is too long.
    [GDGRIB  -9]    First 6 characters of WMO header must be given.
    [GDGRIB -10]    User supplied WMO header is too long.
    [GDGRIB -11]    Invalid grid navigation set in PROJ.
    [GDGRIB -12]    Invalid grid area set in GRDAREA or CPYFIL.
    [GDGRIB -13]    Invalid grid size.
    [GDGRIB -14]    (i,j) -> lat/lon transformation failed.
    [GDGRIB -15]    lat/lon -> (i,j) transformation failed.
    [GDGRIB -16]    Cannot set output grid navigation.
    [GDGRIB -17]    Cannot set input grid navigation.
    [GDGRIB -18]    Invalid horizontal interpolation type.
    [GDGRIB -19]    CPYFIL entry is not valid.
    [GDGRIB ]errors:
    [GDGRIB -21]    BDS section is too long.
    [GDGRIB -22]    Number of packing bits is invalid.
    [GDGRIB -23]    Data range is not valid.
    [GDGRIB -24]    Binary scaling is invalid.
    [GDGRIB -25]    Computation of the reference value failed.
    [GDGRIB -26]    BDS array accomodation is too small.
    [GDGRIB -27]    The calculation of the # of bits needed failed.
    [GDGRIB -28]    All data is missing -- no GRIB message made.
    [GDGRIB ]errors:
    [GDGRIB -41]    BMS section is too long.
    [GDGRIB -42]    BMS array allocation is too small.
    [GDGRIB ]errors:
    [GDGRIB -61]    Not enough bytes for the GDS.
    [GDGRIB -62]    Number in i direction is too large.
    [GDGRIB -63]    Number in j direction is too large.
    [GDGRIB -64]    Latitude 1 is invalid.
    [GDGRIB -65]    Longitude 1 is invalid.
    [GDGRIB -66]    Latitude 2 is invalid.
    [GDGRIB -67]    Longitude 2 is invalid.
    [GDGRIB -68]    Rotated CED projection is not supported.
    [GDGRIB -69]    Rotated STR projection is not supported.
    [GDGRIB -70]    DX grid increment is invalid.
    [GDGRIB -71]    DY grid increment is invalid.
    [GDGRIB -72]    Central longitude is invalid.
    [GDGRIB -73]    True latitudes are invalid.
    [GDGRIB -74]    Rotated MER projection is not supported.
    [GDGRIB -75]    Grid projection is not supported.
    [GDGRIB ]errors:
    [GDGRIB -83]    Cannot find parameter in tables.
    [GDGRIB -84]    Parameter # found is not valid in GRIB.
    [GDGRIB -85]    Vertical coordinate not found in table.
    [GDGRIB -86]    Vertical coordinate is not valid in GRIB.
    [GDGRIB -87]    Level value is too large for GRIB.
    [GDGRIB -88]    Level is less than zero.
    [GDGRIB -89]    Dual GEMPAK times not supported.
    [GDGRIB -90]    4-digit year required in in-line (^) DATTIM.
    [GDGRIB -91]    Forecast must be in hours.
    [GDGRIB -92]    Array allocation for PDS is too small.
    [GDGRIB -93]    Decimal scale factor is too large.
    [GDGRIB -94]    Parameter name is too long to be in table.
    [GDGRIB -95]    GRID # in CPYFIL does not match navigation.
    [GDGRIB -96]    GRID # in CPYFIL is an invalid PDS entry.
    [GDGRIB -97]    Large forecast hour not divisible by 3, 6, or 12.
    [GDGRIB -98]    Accum/diff interval not divisible by 3, 6, or 12.
