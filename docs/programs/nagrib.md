# NAGRIB

NAGRIB converts gridded data in GRIB files to GEMPAK gridded data.

### Input Parameters
 
    GBFILE    GRIB data file name
    INDXFL    GRIB index file name
    GDOUTF    Output grid file
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
    MAXGRD    Maximum number of grids
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    GAREA     Graphics area
    OUTPUT    Output device/filename
    GBTBLS    Input GRIB decoding tables
    GBDIAG    GRIB diagnostic elements
    PDSEXT    Y or N, add PDS extension if found
    OVERWR    Overwr flag
 
 

### Program Description
 
NAGRIB will convert gridded data which is in GRIB files to
gridded data in a GEMPAK file.

The GRIB input file is specified in GBFILE, and the index
file associated with the GRIB file is in INDXFL.

The GEMPAK output file is given in GDOUTF. This file will
be opened if it exists and the projection information
matches the GRIB message projection. If the file does not
exist, it will be created using information given by the
user.  If the output filename is "LIST", the GRIB file will
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
or as a single @ followed by the lower-left and upper-right grid
point coordinates separated by semicolons.  In the former case,
the locations are rounded to the nearest grid points.  The
projection type and angles entered as described above along with
the lat/lon coordinates of the corners of the subset define a
new grid navigation different from that of the full grid.  The
navigation entered as described in the preceding paragraph is
always that of the full grid.  To get the full grid, set GAREA to
blank, GRID or DSET.

OUTPUT defines the direction and destination of printed output.

GBTBLS allows the user to override the default GRIB decoding tables
by listing the filenames of the WMO and NCEP parameter tables,
the vertical coordinate table and the originating center table.

GBDIAG allows for detailed GRIB message section information,
byte-by-byte, to be printed out for selected GRIB messages.
Simply list those GRIB sections to be examined (IDS, PDS, GDS,
BDS, BMS, END or ALL for all sections), e.g., pds;gds.
Selected GRIB messages for examination may also be identified by
number in list and/or range format, e.g., 2;4;5-9.

PDSEXT is a logical flag which only becomes applicable when a PDS
extension exists in the GRIB message. If PDSEXT is YES, then a
sequence of characters specifying the extension information will be
appended onto the standard GEMPAK parameter name.  If PDSEXT is NO,
then the append will not be performed. For instance, if the parameter
is HGHT and the PDS extension is an ensemble extension indicating that
this particular height field is an ensemble mean average, then the PDS
extension suffix will be "ENMA". The final GEMPAK parameter name
will be "HGHTENMA" and must be referenced as such in any GEMPAK program.

OVERWR is a logical flag that allows the user to overwrite the existing
GEMPAK grid file.  If OVERWR is YES, then the grid file is being
overwritten.  Otherwise, it remains the same.


### Examples
 
1.  Convert the data in the NGM GRIB file for 2 Sep 1993.
    Create the GEMPAK file using projection #105 from the
grid navigation table.  Define a subset using lat/lon.
    
        GBFILE   =  ngm_930902_00_000.grib
        INDXFL   =
        GDOUTF   =  ngm_930902_00.grd
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  #105
        GAREA    =  ks-
        OUTPUT   =  t

2.  Convert the data in the MRF GRIB and INDEX files for
2 Sep 1993. Create the GEMPAK file using the navigation
information given the Grid Definition Section of the
GRIB message.  Define a subset using grid coordinates.
    
        GBFILE   =  mrf_930902_00_000.grib
        INDXFL   =  mrf_930902_00_000.indx
        GDOUTF   =  mrf_930902_00.grd
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  gds
        GAREA    =  @230;110;290;140
        OUTPUT   =  t

3.  Convert the data in the GRIB file for North America. Create
the GEMPAK file using the user input for PROJ, GRDAREA and
KXKY.  Do not do a subset, use the full grid.
    
        GBFILE   =  north_amer.grib
        INDXFL   =
        GDOUTF   =  north_amer.grd
        PROJ     =  ced
        GRDAREA  =  0;-180;90;0
        KXKY     =  181;91
        MAXGRD   =  3000
        CPYFIL   =
        GAREA    =  grid
        OUTPUT   =  t

4.  Convert the data in the GRIB file for the MESO model.
Override the default decoding tables.

        GBFILE   =  /tmp/model/meso.mdl
        INDXFL   =
        GDOUTF   =  /tmp/model/meso.mdl.gem
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  gds
        GAREA    =  grid
        OUTPUT   =  t
        GBTBLS   =  /tmp/wmogrib2.tbl;/tmp/nmcgrib2.tbl;/tmp/vcrdgrib1.tbl
        GBDIAG   =

5.  Scan a GRIB file and print out the PDS and GDS sections
for GRIB messages 1, 2, 6 through 12, and 88.

        GBFILE   =  ensemble.grib
        INDXFL   =
        GDOUTF   =  LIST
        PROJ     =  ced
        GRDAREA  =  0;-180;90;0
        KXKY     =  181;91
        MAXGRD   =  3999
        CPYFIL   =
        GAREA    =  grid
        OUTPUT   =  t
        GBTBLS   =
        GBDIAG   =  pds;gds|1;2;6-12;88

6.  Convert the data in the GRIB file for the MESO model.
Overwrite the existing GEMPAK grid file.

        GBFILE   =  /tmp/model/meso.mdl
        INDXFL   =
        GDOUTF   =  /tmp/model/meso.mdl.gem
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   =  3000
        CPYFIL   =  gds
        GAREA    =  grid
        OUTPUT   =  t
        GBTBLS   =
        GBDIAG   =
        OVERWR   = yes


### Error Messages
 
    [NAGRIB  +5]    GRIB version 0 cannot be decoded.
    [NAGRIB  +4]    Invalid parameter code table version.
    [NAGRIB  +3]    No valid parameter found for ....
    [NAGRIB  +2]    No valid vertical coordinate found for ....
    [NAGRIB  +1]    WARNING: This grid is too large for GEMPAK programs.
    [NAGRIB  -1]    Fatal error initializing TAE.
    [NAGRIB  -2]    Fatal error reading TAE parameters.
    [NAGRIB  -3]    Fatal error initializing GEMPLT.
    [NAGRIB  -4]    Navigation information is invalid.
    [NAGRIB  -5]    Grid area ... is invalid.
    [NAGRIB  -6]    Grid size is invalid.
    [NAGRIB  -7]    The grid file name may not be blank.
    [NAGRIB  -8]    Navigation table cannot be read.
    [NAGRIB  -9]    Grid name ... cannot be found in grid table.
    [NAGRIB -10]    Input for GEMPAK output file is blank.
    [NAGRIB -11]    Error opening GEMPAK grid file.
    [NAGRIB -12]    Error creating GEMPAK grid file.
    [NAGRIB -13]    Cannot open GRIB vertical coordinate table.
    [NAGRIB -14]    Cannot open GRIB parameter table.
    [NAGRIB -15]    Error opening GRIB file.
    [NAGRIB -16]    Error getting next message.
    [NAGRIB -17]    Error setting date/time.
    [NAGRIB -18]    Invalid bitmap specification.
    [NAGRIB -19]    Error reading GRIB file.
    [NAGRIB -20]    Invalid input for GAREA.
    [NAGRIB -21]    Grid is too large.
    [NAGRIB -22]    Length of BDS section less than or equal to 0
