# SNSTNS

SNSTNS modifies the station information in an upper air file.


### Input Parameters
 
    SNFILE    Sounding data file
    STNFIL    Station information file
    ADDSTN    Add station flag
    IDNTYP    STNM or STID
 
 
### Program Description
 
SNSTNS updates the station information in a GEMPAK upper air
file.  The station information generally consists of the
character station identifier, STID, the station number, STNM,
the latitude, SLAT, the longitude, SLON, the elevation, SELV,
the state, STAT and the country, COUN.

This information, along with the station name, which is not
used, must be stored in a fixed format in the table file
specified in STNFIL.  The current GEMPAK upper air station
table for US, Canadian and Mexican stations is `SNSTNS.TBL`.
An upper air file containing stations for the world is
`SNWORLD.TBL`.

ADDSTN is a logical parameter which indicates whether stations
which are in the table file but not already in the upper air
file will also be added to the upper air file, provided there
is room for them.

Either STID or STNM may be used to key on the desired station
identifier by setting the desired value in IDNTYP.


### Examples
 
1.  Update upper air file called SOUNDINGS.DAT with station
    information from MYSTN.DAT adding stations that are not
    already in the file.

        SNFILE  =  soundings.dat
        STNFIL  =  mystn.dat
        ADDSTN  =  yes
        IDNTYP  =  stid


### Error Messages
 
    [SNSTNS  +1]    WARNING! No stations were updated.
    [SNSTNS  -1]    Fatal error initializing TAE.
    [SNSTNS  -2]    Fatal error reading TAE parameters.
    [SNSTNS  -3]    Invalid input for IDNTYP; must be STID or STNM.
    [SNSTNS  -4]    STNFIL ... cannot be opened.
