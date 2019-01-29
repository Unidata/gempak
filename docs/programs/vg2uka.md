# VG2UKA

VG2UKA converts VG files to ASCII files, using the UKMET browsable
ASCII format.  Currently, only high level significant weather (SWH)
and mid level significant weather (SWM) VG files may be converted.

### Input Parameters
 
    DATTIM    Date/time
    FHOUR     Forecast hour
    FXYTBL    FXY table file
    CENTER    Originating Center ID #/Sub-Center ID #/Generating Process ID #
    VGFILE    Vgfile | scale file | attribute file | filter
    UKAFIL    Intermediate input/output ASCII file

### Program Description

VG2UKA is a table-driven GEMPAK program with parameter input,
which reads an input VG file and converts it to the UKMET
browsable ASCII format.  (A description of this format for SWH
and SWM data may be found in the WAFC document "Representing
WAFS Significant Weather (SIGWX) Data in BUFR", version 2.5,
November 2004.)  The input parameters are GEMPAK date/time,
forecast hour, FXY table file name (chart type alias), originating
center id, VG input file name and ASCII output file name.

Parameters DATTIM and FXYTBL must be specified.  Default values
for other parameters are (from `$GEMTBL/config/datytype.tbl`)

* `FHOUR = 24`
* `CENTER = 7`
* `VGFILE = $SIGWX/vgfiles/YYYYMMDDHH_FF_final.vgf`

with `SIGWX` set by the site
administrator) and `UKAFIL = <FXYTBL>.txt`.  Only the first field of
the CENTER parameter is used; other fields are ignored.

An SWH or SWM VG file may consist of groups of clouds, fronts, jets,
turbulence, storm symbols and/or volcano symbols.  Tropopause
text information and radiation symbols appear in the the file as
ungrouped elements.  Ungrouped "JET" elements may also be used.

See the Product Generation-BUFR help for information about
creating the input VG file.


### Examples
 
1.  Convert a 24-hour forecast SWH VG file, `test1.vgf`, to an
ASCII file, `SIGWXHI.txt`.  The originating center is NCEP.

        DATTIM  = 040527/1200
    	FHOUR   = 24
        FXYTBL  = SWH
        CENTER  = 7
        VGFILE  = test1.vgf
    	UKAFIL  = SIGWXHI.txt

2.  Convert an 18-hour forecast SWH VG file, `test2.vgf`, to an
ASCII file, `SIGWXHI_18.txt`.  The originating center is
    WAFC London.

      	DATTIM  = 040526/0000
        FHOUR   = 18
        FXYTBL  = SWH
        CENTER  = 93
        VGFILE  = test2.vgf
        UKAFIL  = SIGWXHI_18.txt

3.  Using default values, convert a 24-hour forecast SWH VG
file, `$SIGWX/vgfiles/2004060718_24_final.vgf`, to an ASCII
file, `SWH.txt`.  The originating center is NCEP.

        DATTIM  = 040607/1800
        FHOUR   =
        FXYTBL  = SWH
        CENTER  =
        VGFILE  =
        UKAFIL  =

4.  Convert an 18-hour forecast SWM VG file, test4.vgf, to an
    ASCII file, `SIGWXMD_18.txt`.

        DATTIM  = 040526/0000
        FHOUR   = 18
        FXYTBL  = SWM
        CENTER  = 7
        VGFILE  = test4.vgf
        UKAFIL  = SIGWXMD_18.txt


### Error Messages
 
    [VG2UKA  -1]    The GEMPAK date/time and/or valid hour should be input.
    [VG2UKA  -2]    Group type ... cannot be found in the group type list.
    [VG2UKA  -3]    Cannot create alias from datatyp.tbl. Check alias or inputs.
    [VG2UKA  -4]  VG file ... not found.
    [VG2UKA  -5]    Cannot mix high and mid level charts in fxytbl.
    [VG2UKA  -6]    Chart type/alias should be input.
