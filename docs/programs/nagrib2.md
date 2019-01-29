# NAGRIB2

NAGRIB2 converts gridded data in GRIB2 files to GEMPAK gridded data.

### Input Parameters
 
    GBFILE    GRIB data file name
    GDOUTF    Output grid file
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
    MAXGRD    Maximum number of grids
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    GAREA     Graphics area
    OUTPUT    Output device/filename
    G2TBLS    Input GRIB2 decoding tables
    G2DIAG    GRIB2 diagnostic elements
    OVERWR    Overwr flag
    PDSEXT    Y or N, add PDS extension if found
 
 

### Program Description

NAGRIB2 will convert gridded data which is in GRIB2 files to
gridded data in a GEMPAK file.

The GRIB2 input file is specified in GBFILE.

The GEMPAK output file is given in GDOUTF. This file will
be opened if it exists and the projection information
matches the GRIB message projection. If the file does not
exist, it will be created using information given by the
user.  If the output filename is "LIST", the GRIB2 file will
be scanned and the decoded GEMPAK header information will be
printed out.

The navigation information given by the user may take several
forms. The first possibility is for the user to enter
`CPYFIL=#NNN`, where NNN is a grid projection number listed in
`GRDNAV.TBL`. Second, the user may enter `CPYFIL=GDS`. This will
read the navigation from the Grid Definition Section of the
first GRIB message. Third, the user could enter `CPYFIL=OLDFILE`,
where OLDFILE is the name of an existing GEMPAK file from
which to copy the navigation information. Last, the user
may choose to use the PROJ, GRDAREA, and KXKY parameters to
specify the navigation.

GAREA is used to subset the grid.  It may be specified as
any valid input for GAREA which are equivalent to lat/lon bounds
or as a single `@` followed by the lower-left and upper-right grid
point coordinates separated by semicolons.  In the former case,
the locations are rounded to the nearest grid points.  The
projection type and angles entered as described above along with
the lat/lon coordinates of the corners of the subset define a
new grid navigation different from that of the full grid.  The
navigation entered as described in the preceding paragraph is
always that of the full grid.  To get the full grid, set GAREA to
blank, GRID or DSET.

OUTPUT defines the direction and destination of printed output.

G2TBLS allows the user to override the default GRIB2 decoding tables
by listing the filenames of the WMO and Local (e.g. NCEP) parameter tables
and vertical coordinate tables, and the WMO originating center table.

G2DIAG allows for detailed GRIB2 message section information,
entry-by-entry, to be printed out for selected GRIB2 messages.
Simply list those GRIB2 sections to be examined (IS, IDS, GDS, PDS,
DRS, BMS, or ALL for all sections), e.g., pds;gds.
Selected GRIB2 messages for examination may also be identified by
number in list and/or range format, e.g., 2;4;5-9.

OVERWR is a logical flag that allows the user to overwrite the existing
GEMPAK grid file.  If OVERWR is YES, then the grid file is being
overwritten.  Otherwise, it remains the same.


### Examples
 
1.  Convert the data in the specified NAM GRIB file3.
    Create the GEMPAK file using projection #104 from the
grid navigation table.  Define a subset using the entry in `geog.tbl`.

        GBFILE   =  nam.t00z.grbgrd30.tm00
        GDOUTF   =  nam_30.grd
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  #104
        GAREA    =  ks-
        OUTPUT   =  t
        G2TBLS   =
        G2DIAG   =

2.  Convert the 60 hour data in the given GFS GRIB2 file.
Create the GEMPAK file using the navigation
information given the Grid Definition Section of the
GRIB message.  Define a subset using grid coordinates.

        GBFILE   =  gfs.t06z.pgrb2f60
        GDOUTF   =  gfs_60.grd
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  gds
        GAREA    =  @230;110;290;140
        OUTPUT   =  t
        G2TBLS   =
        G2DIAG   =

3.  Convert the data in the GRIB file for global GFS. Create
the GEMPAK file using the user input for PROJ, GRDAREA and
KXKY.  Do not do a subset, use the full grid.
    
        GBFILE   =  gfs.t06z.pgrb2f60
        GDOUTF   =  gfs_60.grd
        PROJ     =  ced/0;0;0
        GRDAREA  =  -90.;0.;90.;-0.5
        KXKY     =  720;361
        MAXGRD   =  3000
        CPYFIL   =
        GAREA    =  grid
        OUTPUT   =  t
        G2TBLS   =
        G2DIAG   =

4.  Convert the data in the GRIB file for the NDFD Max Temperature grid. Override the default decoding parameter tables.

        GBFILE   =  /tmp/ds.maxt.bin
        GDOUTF   =  /tmp/ds.maxt.grd
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  gds
        GAREA    =  grid
        OUTPUT   =  t
        G2TBLS   =  /tmp/g2varswmo1.tbl;/tmp/g2varsncep0.tbl
        G2DIAG   =

5.  Scan a GRIB file and print out the PDS and GDS sections
for GRIB messages 1, 2, 6 through 12, and 88.

        GBFILE   =  gfs.t06z.pgrb2f60
        GDOUTF   =  LIST
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3999
        CPYFIL   =  gds
        GAREA    =  grid
        OUTPUT   =  t
        G2TBLS   =
        G2DIAG   =  pds;gds|1;2;6-12;88

6.  Convert the data in the GRIB file for the NDFD Max Temperature grid.
    Overwrite the existing GEMPAK grid file.

        GBFILE   =  /tmp/ds.maxt.bin
        GDOUTF   =  /tmp/ds.maxt.grd
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  gds
        GAREA    =  grid
        OUTPUT   =  t
        G2TBLS   =
        G2DIAG   =
        OVERWR   = yes

### Error Messages
 
    [NAGRIB2  +6]   WARNING: Resetting MAXGRD value to max limit ....
    [NAGRIB2 ] ! GRIB version 0 cannot be decoded.
    [NAGRIB2 ] ! Invalid parameter code table version.
    [NAGRIB2 ] ! No valid parameter found for ....
    [NAGRIB2 ] ! No valid vertical coordinate found for ....
    [NAGRIB2  +1]   WARNING: This grid is too large for GEMPAK programs.
    [NAGRIB2  -1]   Fatal error initializing TAE.
    [NAGRIB2  -2]   Fatal error reading TAE parameters.
    [NAGRIB2  -3]   Fatal error initializing GEMPLT.
    [NAGRIB2 ] ! Navigation information is invalid.
    [NAGRIB2 ] ! Grid area ... is invalid.
    [NAGRIB2 ] ! Grid size is invalid.
    [NAGRIB2  -7]   The grid file name may not be blank.
    [NAGRIB2 ] ! Navigation table cannot be read.
    [NAGRIB2 ] ! Grid name ... cannot be found in grid table.
    [NAGRIB2 -10]   Input for GEMPAK output file is blank.
    [NAGRIB2 -11]   Error opening GEMPAK grid file.
    [NAGRIB2 -12]   Error creating GEMPAK grid file.
    [NAGRIB2 ] ! Cannot open GRIB vertical coordinate table.
    [NAGRIB2 ] ! Cannot open GRIB parameter table.
    [NAGRIB2 -15]   Error opening GRIB file.
    [NAGRIB2 -16]   EOF on input GRIB file
    [NAGRIB2 ] ! Error setting date/time.
    [NAGRIB2 ] ! Invalid bitmap specification.
    [NAGRIB2 -19]   Error reading GRIB file.
    [NAGRIB2 -20]   Invalid input for GAREA.
    [NAGRIB2 -21]   Grid is too large.
    [NAGRIB2 ] ! Length of BDS section less than or equal to 0
    [NAGRIB2 -25]   Error unpacking GRIB2 metadata
    [NAGRIB2 -26]   Memory allocation error for next message
    [NAGRIB2 -27]   GRIB Message is not version 2
    [NAGRIB2 -30]   Could not allocate space for grid.
