# SFVGSF

SFVGSF adds or changes data in a surface file using the elements
found in a Vector Graphics file.


### Input Parameters
 
    VGFILE    Vgfile | scale file | attribute file | filter
    SFOUTF    Output surface file
    DATTIM    Date/time
    SFPARM    Surface parameter list
    COLORS    Color list
 
 
### Program Description
 
SFVGSF adds information from a Vector Graphics file to an
existing GEMPAK surface file.  The program may be used to add
new data to a file or to change existing values.  The output
file must already exist.

Each parameter in the Vector Graphics file must have a unique
color because the search method uses the color to distinguish
between parameters.  The colors listed in COLORS must correspond
to the colors used to create the Vector Graphics file, so that
the parameter names can be matched to the data values.

The Vector Graphics file can be created by specifying VG as the
device driver in the GEMPAK program SFMAP or by generating the
file in the product generation of NMAP.

The parameters to be added to the surface file are specified
in SFPARM and the associated colors are listed in COLORS.
The STID or STNM must be present in the VG file and the
parameter list so that the data can be written to the proper
station.

The date/time must also be provided by the user. Any missing
parts of the date/time will be supplied by the system time.

If a station or time does not exist in the output file, it will
be added to the file.  If stations are added, the GEMPAK
program SFSTNS can be used to add station latitudes, longitudes
and elevations.


### Examples
 
1.  Read the Vector Graphics file day3max.vgf and write the
    data for the requested parameters to the surface file
    990307.mrfmos. In this example, TDYE is the edited maximum
temperature value.

        VGFILE  = day3max.vgf
        SFOUTF  = 990307.mrfmos
        DATTIM  = 990307/1200
        SFPARM  = TDYE;STID
        COLORS  = 2;17


### Error Messages
 
    [SFVGSF  +3]    Parameter ... is not in the data set.
    [SFVGSF  +2]    Station ... cannot be added to the file.
    [SFVGSF  -1]    Fatal error initializing TAE.
    [SFVGSF  -2]    Fatal error reading TAE parameters.
    [SFVGSF  -3]    VG file ... cannot be opened.
    [SFVGSF  -4]    The output file must be a standard SF file.
    [SFVGSF  -5]    One of the parameters must be STID or STNM.
    [SFVGSF  -6]    Time ... cannot be added to data set.
