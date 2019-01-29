# GD2NDFD

GD2NDFD converts a GEMPAK grid to an NDFD GRIB2 file.

### Input Parameters
 
    GDFILE    Grid file
    GFUNC     Scalar grid
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GBFILE    GRIB data file name
    CENTER    Originating Center/SubcenterID/ProcID
    WMOHDR    WMO_ID/Origin_ID/DDHHMM
 

### Program Description
 
GD2NDFD converts a GEMPAK grid to an NDFD GRIB2 file.

The input grids for the computation must be in GDFILE.  The
converted GRIB2 message is added to the output file, GBFILE.
If GBFILE does not exist, it will be created.

The parameter CENTER allows the GD2NDFD user to specify the
originating or generating center ID and sub-center ID of
the GRIB2 message.

A World Meteorological Organization (WMO) header may be
prefixed to the GRIB2 message by giving at least the first
six bytes of the header in the WMOHDR input parameter.
Default values will be supplied for the originating center
(KWBC) and reference time.


### Examples
 
Convert all times for paramter TMPK from GEMPAK grid format to NDFD GRIB2 format.  The output is written to file hrcbob.ndfd.

        GDFILE   = $GEMPAK/data/hrcbob.grd
        GFUNC    = tmpk
        GDATTIM  = all
        GLEVEL   = 0
        GVCORD   = none
        GBFILE   = hrcbob.ndfd
        CENTER   = 7/5
        WMOHDR   = HTRE50


### Error Messages
 
    [GD2NDFD  +6]   WMO header center ID inconsistent with PDS byte 5.
    [GD2NDFD  -1]   Fatal error initializing TAE.
    [GD2NDFD  -2]   Fatal error reading TAE parameters.
    [GD2NDFD  -3]   Error initializing GEMPLT.
    [GD2NDFD  -4]   Grid file could not be opened.
    [GD2NDFD  -5]   Grid navigation could not be set in GEMPLT.
    [GD2NDFD  -6]   Grid diagnostics package initialization failed.
    [GD2NDFD  -7]   Error interpolating to output grid.
    [GD2NDFD  -8]   GRIB message is too long.
    [GD2NDFD  -9]   First 6 characters of WMO header must be given.
    [GD2NDFD -10]   User supplied WMO header is too long.
    [GD2NDFD -11]   Invalid grid navigation set in PROJ.
    [GD2NDFD -12]   Invalid grid area set in GRDAREA or CPYFIL.
    [GD2NDFD -13]   Invalid grid size.
    [GD2NDFD -14]   (i,j) -> lat/lon transformation failed.
    [GD2NDFD -15]   lat/lon -> (i,j) transformation failed.
    [GD2NDFD -16]   Cannot set output grid navigation.
    [GD2NDFD -17]   Cannot set input grid navigation.
    [GD2NDFD -18]   Invalid horizontal interpolation type.
    [GD2NDFD -21]   BDS section is too long.
    [GD2NDFD -22]   Number of packing bits is invalid.
    [GD2NDFD -23]   Data range is not valid.
    [GD2NDFD -24]   Binary scaling is invalid.
    [GD2NDFD -25]   Computation of the reference value failed.
    [GD2NDFD -26]   BDS array accomodation is too small.
    [GD2NDFD -27]   The calculation of the # of bits needed failed.
    [GD2NDFD -28]   All data is missing -- no GRIB message made.
    [GD2NDFD -41]   BMS section is too long.
    [GD2NDFD -42]   BMS array allocation is too small.
    [GD2NDFD -61]   Not enough bytes for the GDS.
    [GD2NDFD -62]   Number in i direction is too large.
    [GD2NDFD -63]   Number in j direction is too large.
    [GD2NDFD -64]   Latitude 1 is invalid.
    [GD2NDFD -65]   Longitude 1 is invalid.
    [GD2NDFD -66]   Latitude 2 is invalid.
    [GD2NDFD -67]   Longitude 2 is invalid.
    [GD2NDFD -68]   Rotated CED projection is not supported.
    [GD2NDFD -69]   Rotated STR projection is not supported.
    [GD2NDFD -70]   DX grid increment is invalid.
    [GD2NDFD -71]   DY grid increment is invalid.
    [GD2NDFD -72]   Central longitude is invalid.
    [GD2NDFD -73]   True latitudes are invalid.
    [GD2NDFD -74]   Rotated MER projection is not supported.
    [GD2NDFD -75]   Grid projection is not supported.
    [GD2NDFD -83]   Cannot find parameter in tables.
    [GD2NDFD -84]   Parameter # found is not valid in GRIB.
    [GD2NDFD -85]   Vertical coordinate not found in table.
    [GD2NDFD -86]   Vertical coordinate is not valid in GRIB.
    [GD2NDFD -87]   Level value is too large for GRIB.
    [GD2NDFD -88]   Level is less than zero.
    [GD2NDFD -89]   Dual GEMPAK times not supported.
    [GD2NDFD -90]   4-digit year required in in-line (^) DATTIM.
    [GD2NDFD -91]   Forecast must be in hours.
    [GD2NDFD -92]   Array allocation for PDS is too small.
    [GD2NDFD -93]   Decimal scale factor is too large.
    [GD2NDFD -94]   Parameter name is too long to be in table.
