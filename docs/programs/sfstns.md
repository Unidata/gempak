# SFSTNS

SFSTNS modifies the station information in a surface file.


### Input Parameters
 
    SFFILE    Surface data file
    STNFIL    Station information file
    ADDSTN    Add station flag
    IDNTYP    STNM or STID
 
 
### Program Description
 
SFSTNS updates the station information in a GEMPAK surface
file.  The station information includes the character station
identifier, STID, the station number, STNM, the latitude,
SLAT, the longitude, SLON, the elevation, SELV, the state,
STAT, and the country, COUN.

This information must be stored in a fixed format in the table
file specified in STNFIL.  See the default table for an example
of the required format.  Station names may be included, but are
not used.  The current default GEMPAK surface station table
for US, Canadian and Mexican stations is `SFSTNS.TBL`.

ADDSTN is a logical parameter that indicates whether stations
which are in the table file but not already in the surface file
will also be added to the surface file, provided there is room
for them. To create a surface data file with enough room to
allow for stations to be added, run the program SFCFIL. Set
the second parameter for TIMSTN to be large enough to hold the
number of stations to be added to the surface file.

IDNTYP governs whether station numbers, STNM, or character
identifiers, STID, will be used to identify stations in the
table.


### Examples
 
1.  Update surface file called SURF.DAT with station
    information from MYSTN.DAT, adding stations that are not
    already in the surface file.

        SFFILE  =  surf.dat
        STNFIL  =  mystn.dat
        ADDSTN  =  yes
        IDNTYP  =  stid


### Error Messages
 
    [SFSTNS  +1]    WARNING! No stations were updated.
    [SFSTNS  -1]    Fatal error initializing TAE.
    [SFSTNS  -2]    Fatal error reading TAE parameters.
    [SFSTNS  -3]    Invalid input for IDNTYP; must be STID or STNM.
    [SFSTNS  -4]    STNFIL ... cannot be opened.
