# SFEDIT

SFEDIT adds or changes data in a surface file using a sequential
text file.


### Input Parameters
 
    SFEFIL    Surface edit file
    SFFILE    Surface data file
 
 
### Program Description
 
SFEDIT adds information from a sequential text file to an
existing GEMPAK surface file.  The program may be used to add
new data to a file or to change existing values.  The output
file must already exist.

The data to be added must reside in a text file, SFEFIL.  This
file can be created by specifying F as the output device in
GEMPAK program SFLIST or by generating the file with a text
editor.  A text editor may be used to change the file.

The parameters to be edited must be specified at the beginning
of the edit file, for example:  PARM=TMPF;DWPF.  Only
parameters stored in the output file can be edited.  Parameters
that have character values, such as WTHR, are actually stored
as real numbers.  Thus, the numeric weather code must be used.
Station information, such as the latitude, SLAT, cannot be
changed using this program.  Rather, these changes can be
made using the GEMPAK program, SFSTNS.

The data follow the parameter list.  Each data listing must
include the time, followed by the station character or
numeric identifier followed by the data.  If data are
missing, the current missing data value, -9999., must be
used.  Blank fields will not be recognized.

If a station or time does not exist in the output file it will
be added to the file.  If stations are added, the GEMPAK
program SFSTNS can be used to add station latitudes, longitudes
and elevations.


### Examples
 
1.  Change values of TMPF and DWPF in the file SFCDATA.DAT.

        SFEFIL  = sflist.fil
        SFFILE  = sfcdata.dat

    The file `SFLIST.FIL` follows.  Note that the title line
    is ignored.
    
        PARM = TMPF;DWPF
    
        STN  YYMMDD/HHMM 	TMPF	DWPF
        BWI  850426/0400       55.40   42.80
        SBY  850426/0400       62.62   55.40
        BGJ  850426/0400       55.40   42.80

    The program replaces the temperature and dewpoint for
    stations BWI, SBY and NHK at the given time values in
    output file SFCDATA.DAT.


### Error Messages
 
    [SFEDIT  +2]    Station ... cannot be added to the file.
    [SFEDIT  -1]    Fatal error initializing TAE.
    [SFEDIT  -2]    Fatal error reading TAE parameters.
    [SFEDIT  -3]    Edit file ... cannot be opened.
    [SFEDIT  -5]    PARM keyword not found.
    [SFEDIT  -6]    Too many parameters to edit.
    [SFEDIT  -7]    Time ... cannot be added to data set.
    [SFEDIT  -8]    Parameter ... is not in the data set.
    [SFEDIT  -9]    SFEDIT cannot continue with invalid parameters.
    [SFEDIT -10]    Missing lat/lon pair for ship file
