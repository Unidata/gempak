# SFCFIL

SFCFIL creates a new GEMPAK surface file.


### Input Parameters
 
    SFOUTF    Output surface file
    SFPRMF    Surface parameter packing file
    STNFIL    Station information file
    SHIPFL    Ship data file flag
    TIMSTN    Times/additional stations
    SFFSRC    Surface file source
 
 
### Program Description
 
SFCFIL creates a GEMPAK surface file.  The file may be a
standard file or a ship format file. The file may have two
data types -- decoded data and/or text data.

SFFSRC is the surface file source and may be set to either
decoded data (including AIRW, METR, SHIP, BUOY, and SYNP)
or TEXT.

If both types are requested, they are separated by a '|'.
SFFSRC may be entered as:

    SFFSRC =		==> create a file for
                                        unknown (UNKN) decoded
                                        data

    SFFSRC = AIRW		==> create a file for
                                        decoded airways data

    SFFSRC = TEXT		==> create a file for
                                        text data

    SFFSRC = AIRW|TEXT	==> create a file for both

The decoded file type MUST be specified first, otherwise a
default to text only will occur.

If SHIPFL is set to YES, a ship format file is created.  In
this case, both station location and time are stored with
each data entry.  This capability is useful when the station
location varies in time, such as for moving ships, aircraft
or free-floating buoys.

If a standard file is to be created, the maximum number of
times to be included in the file must be entered as the first
value in TIMSTN.  If a ship format file is being created, the
maximum number of entries in the file is given by the first
value in TIMSTN.

If STNFIL is not blank, information about all the stations in
STNFIL will be added to the data set.  Space will be left in the
file for the additional number of stations specified as the
second parameter in TIMSTN.  Note that an error will result if
STNFIL is blank and TIMSTN does not request more stations.

SFPRMF contains information about the parameters to be included
in the file.  SFPRMF may be either a list of parameters or the
name of a packing file.  If a list is entered, the parameters
must be separated with semicolons; packing information may also
be included after a slash with the minimum and maximum values
and the resolution separated by dashes.  For example, to
include temperature and dewpoint in a file without packing,
SFPRMF may be entered as :

           SFPRMF = TMPC;DWPC

To pack the data, use:

           SFPRMF = TMPC/-127-63-.1;DWPC/-63-63-.1

SFPRMF may also contain the name of a packing file.  A packing
file for data which is not to be packed contains a list of
parameters with one parameter per line.  In a file for packed
data, each line must include the parameter name, the minimum
and maximum data values and the resolution, all separated with
spaces.  The default packing file for surface data is SF51.PACK.

 
### Examples
 
1.  Create a surface file which can contain both decoded and
text data called SURF.DAT with a maximum of 15 times using
the default station and parameter files.  Leave room in the
file for 100 stations in addition to the stations in STNFIL.
Use the GEMPAK standard packing file and table file.

        SFOUTF  =  surf.dat
        SFPRMF  =  sf51.tbl
        STNFIL  =  stations.tbl
        SHIPFL  =  no
        TIMSTN  =  15/100
	    SFFSRC  =  AIRW|TEXT

2.  Create a ship file which can contain decoded data only
and a maximum of 1000 ship reports.  Use a locally
developed parameter file named SHIP.PRM for packing.

        SFOUTF  =  ship.dat
        SFPRMF  =  ship.prm
        STNFIL  =
        SHIPFL  =  yes
        TIMSTN  =  1000
	    SFFSRC  =  SHIP

3.  Create a surface file which can contain text data only
called SURF.DAT. Use a locally developed parameter file
named SURF.PRM for packing.

        SFOUTF  =  ship.dat
        SFPRMF  =  surf.prm
        STNFIL  =
        SHIPFL  =  no
        TIMSTN  =
        SFFSRC  =  TEXT

### Error Messages
 
    [SFCFIL  +2]    WARNING! ADDSTN was negative -- set to 0.
    [SFCFIL  +1]    Cannot add stn to a ship file; STNFIL ignored.
    [SFCFIL  -1]    Fatal error initializing TAE.
    [SFCFIL  -2]    Fatal error reading TAE parameters.
    [SFCFIL  -3]    Error opening station file ... .
    [SFCFIL  -4]    File does not include room for any stations.
    [SFCFIL  -5]    File does not include room for any times.
    [SFCFIL  -6]    SFPRMF is incorrectly specified.
    [SFCFIL  -7]    The output file name is blank.
