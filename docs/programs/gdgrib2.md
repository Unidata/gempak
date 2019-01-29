# GDGRIB2

GDGRIB2 computes a scalar diagnostic grid and adds it to a
GRIB2 file.

### Input Parameters
 
    GDFILE    Grid file
    GBFILE    GRIB data file name
    GFUNC     Scalar grid
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    G2TBLS    Input GRIB2 decoding tables
    G2IS      Info for GRIB2 Section 0 (Indicator Section)
    G2IDS     Info for GRIB2 Section 1 (Identification Section)
    G2PDT     Info for GRIB2 Section 4 (Product Definition Section)
    G2DRT     Packing/scaling info for GRIB2 Section 5 (Data Representation Sect
    WMOHDR    WMO_ID/Origin_ID/DDHHMM
    G2CONV    Table containing parameters for a series conversions
 
 

### Program Description
 
GDGRIB2 computes a diagnostic grid and saves the result in
a GRIB2 file.

The input grids for the computation must be in GDFILE.  The
resulting GRIB message is added to the output file, GBFILE.
If GBFILE does not exist, it will be created.

The parameters G2IS, G2IDS, and G2PDT allow control over how the
grid is identified in the Indicator Section (IS), Identification Section
(IDS), and the Product Definition Section (PDS) of the GRIB2 message,
respectively.

The G2DRT parameter allows the user to specify the packing algorithm
used to encode the grid point data as well as the precision of the data
to maintain.  The desired precision can be specified either in terms
of binary precision with rounding to the nearest power of two or
decimal precision in terms of the number of significant digits to preserve.

A World Meteorological Organization (WMO) header may be
prefixed to the GRIB message by giving at least the first
six bytes of the header in the WMOHDR input parameter.
Default values will be supplied for the originating center
(KWBC) and reference time.

GDGRIB2 will interpolate scalar fields horizontally to a
different output grid.  The navigation for the output grid is
specified using CPYFIL (see below) or by setting the PROJ,
GRDAREA, and KXKY parameters.  The Grid Description Section
(GDS) in the GRIB message will represent this navigation.

Do not attempt to interpolate grid-relative vector components
to another grid because this requires a rotation of a vector,
which cannot be done without both vector components and is
beyond the capability of the current version of GDGRIB2.  North-
relative components may be interpolated.  Wind direction should
NOT be interpolated in any case.

The output grid navigation can be specified with the CPYFIL parameter.
CPYFIL may be set to the name of a GEMPAK grid file (other than the
file specified in GDFILE), in which case the output grid is
interpolated to the grid whose navigation is defined in the
GEMPAK grid file.  If CPYFIL is set to #NNN, grid number NNN is found
in `grdnav.tbl`, and that specification is used as the output navigation.

The CPYFIL parameter supersedes PROJ, GRDAREA, and KXKY input.
If CPYFIL is valid, PROJ, GRDAREA, and KXKY are ignored.  If
CPYFIL is not set, PROJ, GRDAREA, and KXKY will be used if they
are valid.

G2TBLS allows for specification of the GRIB2 decoding tables.
The defaults are:

	$GEMTBL/grid/g2varswmoX.tbl,
	$GEMTBL/grid/g2varsYYYYZ.tbl,
	$GEMTBL/grid/g2vcrdwmoX.tbl,
	$GEMTBL/grid/g2vcrdYYYYZ.tbl,
	$GEMTBL/grid/wmocenter.tbl

where X is replaced by the master table version number, which can be
specified in G2IDS (default is 2).  YYYY and Z (can also be specified
in parameter G2IDS) define the originating center and local tables
version, respectively.  The defaults are YYYY = "ncep" and Z = 1.

G2CONV specifies a table containing all the parameter information for 
a series of conversions. The existence of a value for G2CONV would 
signal the program to perform bulk processing. The program would take 
the parameters from the table and perform the bulk conversions. If 
G2CONV is not specified (empty), the program would take the parameters 
from user input and perform a single conversion. 

### Examples
 
1.  Compute the average absolute vorticity of the 250- and 300-
    mb wind for the 24-h forecast.  Assign this to the 275-mb
    level in the GRIB message PDT.  Also assign the appropriate
    parameter number for absolute vorticity (AVOR).  Use decimal
    precision to pack the data so as to preserve 4 decimal
    significant digits.  Specify that the data source is from a
    high resolution eta model run (generating process 110 in PDT)
    diagnosed at the Storm Prediction Center (sub-center
    number 9 in IDS).  Interpolate the grid to standard AWIPS grid
    212, without using CPYFIL.  Do not make a WMO header.  The output
    is written to file hrcbob.pgrb2.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GBFILE   = hrcbob.pgrb2
        GFUNC    = avg(avor(wind@300),avor(wind@250))
        GDATTIM  = f24
        GLEVEL   = 300
        GVCORD   = pres
        PROJ     = lcc/25;-95;25
        GRDAREA  = 12.190;-133.459;57.290;-49.385
        KXKY     = 185;129
	    CPYFIL   =
        G2TBLS   =
        G2IS     =
        G2IDS    = ;9
        G2PDT    = 0|2;10;;;110;;;;;100;0;27500;255;0;0
        G2DRT    = |4
        WMOHDR   =

2.  Transfer the 24-h forecast of the 500-mb temperature grid
    into a GRIB2 file.  Use binary precision to pack the data
    to the nearest 1/8 K.  The output GRIB message is to be
    added to existing file hrcbob.pgrb2.  Accept default
    entries for the IDS and PDS, with the parameter number coming from
    a specific lookup table (`g2varswmo2.tbl`) in `$GEMTBL`.  Also,
    add the WMO header for 500-mb temperature destined for
    Family of Services, and interpolate to grid 212 using CPYFIL.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GBFILE   = hrcbob.pgrb2
        GFUNC    = tmpk
        GDATTIM  = f24
        GLEVEL   = 500
        GVCORD   = pres
        PROJ     =
        GRDAREA  =
        KXKY     =
	    CPYFIL   = #212
        G2TBLS   = g2varswmo2.tbl
        G2IS     =
        G2IDS    =
        G2PDT    =
        G2DRT    = ||-3
        WMOHDR   = HTRE50

3.  Transfer the tropopause temperature from a 24-h model forecast
    into a GRIB file. Reassign the parameter and the vertical
    coordinate name properly to treat this as a tropopause
    temperature.  Use the default parameter lookup table for
    the GRIB parameter (TMPK) identification number.  Use the
    vertical coordinate table named `g2vcrdwmo2.tbl` in `$GEMTBL` to
    lookup the vertical coordinate identification number.  Do not
    do horizontal interpolation.  Since `hrcbob.grd` data is
    already on grid #6, GDGRIB will detect this and not do
    interpolation.  Pack grid points using first order differencing.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GBFILE   = tmpk_trop.grb2
        GFUNC    = tmpktpps
        GDATTIM  = f24
        GLEVEL   = 0
        GVCORD   = none
        PROJ     =
        GRDAREA  =
        KXKY     =
	    CPYFIL   = #6
        G2TBLS   = ;;g2vcrdwmo2.tbl
        G2IS     =
        G2IDS    =
        G2PDT    = 0|0;0;;;;;;;;7;0;0;255;0;0
        G2DRT    = 3||-3
        WMOHDR   =

4.  Transfer the 12-hour precipitation at forecast hour 18 to
    a GRIB file.  Specifically denote the generating process
    identifier as 84.  Interpolate to 1- by 1-degree resolution
    global grid #3.  Note that many points on this grid will
    have missing values.  Pack the grid to a precision of 1/4
    millimeter using second order differencing.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GBFILE	= p12m.grb2
        GFUNC	= p12m
        GDATTIM	= f18
        GLEVEL	= 0
        GVCORD	= none
        PROJ	=
        GRDAREA	=
        KXKY	=
        CPYFIL	= #3
        G2TBLS	=
        G2IS    =
        G2IDS   =
        G2PDT   = 8|;;;;84
        G2DRT   = 3||-2|2
        WMOHDR	=

5.  Compute an "off-time" 6-hour precipitation amount using the
    average of two accumulations.  Identify the precip grid as a 6 hour
    accumulation ending at 21 hours after the initial reference time.
    Interpolate to grid 215.  Use the default packing algorithm and
    precision.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GBFILE	= p06m.grb2
        GFUNC	= avg(P06M^F24,P06M^F18)
        GDATTIM	= f18
        GLEVEL	= 0
        GVCORD	= none
        PROJ	=
        GRDAREA	=
        KXKY	=
        CPYFIL	= #215
        G2TBLS	=
        G2IS    =
        G2IDS   =
        PDSVAL	= P06M^19910819/0000F21
        G2PDT   = 8|1;8;;;;;;1;15;1;0;0;255;0;0;1991;8;19;21;0;0;1;0;1;255;1;6
        G2DRT   =
        WMOHDR	=

6.  Transfer the 12-hour forecast of the 850-mb grid-relative
    U-wind component to a GRIB file.  Pack the data to the
    nearest 1/8 m/s using PNG encoding.  DO NOT INTERPOLATE!

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GBFILE	= wind1.grb2
        GFUNC	= urel
        GDATTIM	= f12
        GLEVEL	= 850
        GVCORD	= pres
        PROJ	=
        GRDAREA	=
        KXKY	=
        CPYFIL	=
        G2TBLS	=
        G2IS    =
        G2IDS   =
        G2PDT   =
        G2DRT   = 41||-3
        WMOHDR	=

7.  Complete the transfer of the 850-mb wind by transfering
    the grid-relative V-wind component.

        GDFILE	= $GEMPAK/data/hrcbob.grd
        GBFILE	= wind1.grb2
        GFUNC	= vrel
        GDATTIM	= f12
        GLEVEL	= 850
        GVCORD	= pres
        PROJ	=
        GRDAREA	=
        KXKY	=
        CPYFIL	=
        G2TBLS	=
        G2IS    =
        G2IDS   =
        G2PDT   =
        G2DRT   = 41||-3
        WMOHDR	=

### Error Messages
 
    [GDGRIB2  -1]   Fatal error initializing TAE.
    [GDGRIB2  -2]   Fatal error reading TAE parameters.
    [GDGRIB2  -3]   Error initializing GEMPLT.
    [GDGRIB2  -4]   Grid file could not be opened.
    [GDGRIB2  -6]   Grid diagnostics package initialization failed.
    [GDGRIB2  -9]  First 6 characters of WMO header must be given.
    [GDGRIB2 -11]   Invalid grid navigation set in PROJ.
    [GDGRIB2 -12]   Invalid grid area set in GRDAREA or CPYFIL.
    [GDGRIB2 -13]   Invalid grid size.
    [GDGRIB2 -16]   Cannot set output grid navigation.
    [GDGRIB2 -17]   Cannot set input grid navigation.
    [GDGRIB2 -18]   Invalid horizontal interpolation type.
    [GDGRIB2 -19]   CPYFIL entry is not valid.SAG
    [GDGRIB2 -20]   Could not process GDATTIM information
    [GDGRIB2 -21]   Could not get next Date/Time stamp.
    [GDGRIB2 -22]   Could not get requested grid
    [GDGRIB2 -23]   Could not allocate space for grid.
    [GDGRIB2 -24]   Could not open output GRIB file.
    [GDGRIB2 -25]   Error writing to output GRIB file.
    [GDGRIB2 -26]   Could not allocate space for GRIB2 message.
    [GDGRIB2 -27]   Error encoding GRIB2 message.
    [GDGRIB2 -28]   Output GRIB file (GBFILE) not specified.
    [GDGRIB2 -29]   Invalid G2PDT parameter.
    [GDGRIB2 -30]   Could not read grid in CPYFIL from grdnav.tbl
    [GDGRIB2 -31]   Could not open grid file specified in CPYFIL.
    [GDGRIB2 -32]   Error setting output grid navigation
    [GDGRIB2 -33]   Error creating navigation block from PROJ, KXKY, GRDAREA
