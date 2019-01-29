# GDTOPO

GDTOPO creates a GEMPAK GRID file from a raster file of
topography or land use values.

### Input Parameters
 
    GDFILE    Grid file
    GAREA     Graphics area
    GDATTIM   Grid date/time
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    TOPOFL    Topographic file
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
 
 

### Program Description

GDTOPO will create a GEMPAK GRID file of data values.
A new grid file will be created. The grid can be displayed in
grid programs, or converted to image format for a basemap.

The raster data respresents a CED (lat/lon) projection grid
which can be subsetted to GAREA, and subsampled using IJSKIP.
The grid parameter will be created using the time, coordinate,
and grid name provided by GDATTIM, GVCORD, and GFUNC respectively.

Topography files can be obtained from:

    https://www.unidata.ucar.edu/downloads/gempak/tables/
    5 minute topography/bathymetry dataset: world_topo.5min.gz
    30 second topography dataset: world_topo.30s.gz
    30 second landuse dataset: gl-latlong-1km-landcover.gz

Topography files can be placed in $GEMTBL/unidata/.

 
### Examples
 
1.  Create a topographic grid for the US (CONUS) region using
    the 5 minute resolution dataset.

         GDFILE   = topo_us.gem
         GAREA    = 22;-125;57;-57
         GDATTIM  = 010101/0000
         GVCORD   = none
         GFUNC    = topo
         TOPOFL   = dem5
         IJSKIP   = 0

2.  Create a topographic grid for the PA region using the
    30 second resolution dataset.

         GDFILE   = testme.gem
         GAREA    = unv+
         GDATTIM  = 010101/0000
         GVCORD   = none
         GFUNC    = topo
         TOPOFL   = dem30
         IJSKIP   = 0

3.  Create a land use grid for the US region using the
    30 second resolution (~1km) landuse dataset. Sample
    the data at every 6th point.

         GDFILE   = testme.gem
         GAREA    = 10;-140;65;-40
         GDATTIM  = 010101/0000
         GVCORD   = none
         GFUNC    = luse
         TOPOFL   = land1
         IJSKIP   = 5

### Error Messages
 
    [GDTOPO  -1]    Fatal error initializing TAE.
    [GDTOPO  -2]    Fatal error reading TAE parameters.
    [GDTOPO  -3]    Error initializing GEMPLT.
    [GDTOPO  -4]    There is no input file specified.
    [GDTOPO  -5]    Different navigation in input, output files.
    [GDTOPO  -6]    Error opening input files.
    [GDTOPO  -7]    Error writing output grid.
    [GDTOPO  -8]    Only one output file is permitted.
    [GDTOPO  -9]    Output file open failure.
    [GDTOPO -10]    Grid files have different navigations.
    [GDTOPO -11]    Number of grid points is too large.
    [GDTOPO -12]    Topography file does not exist.
    [GDTOPO -13]    Grid file already exists.
