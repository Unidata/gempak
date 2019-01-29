# SFDELT

SFDELT deletes data from a surface data file.


### Input Parameters
 
    SFFILE    Surface data file
    DATTIM    Date/time
    AREA      Data area
 
 
### Program Description
 
SFDELT deletes data from a surface data file.  The stations
to be deleted may be specified in AREA; the times to be
deleted are given in DATTIM.

If AREA is set to DSET or ALL, all the data present at the
times in DATTIM will be deleted, along with the headers for
those times.


### Examples
 
1.  Delete all the data at time 920722/1200.

        SFFILE  =  92jul.sfc
        DATTIM  =  920722/1200
        AREA    =  dset

2.  Delete data at BWI for the most recent time in the file.

        SFFILE  =  realtime.sfc
        DATTIM  =  last
        AREA    =  @bwi


### Error Messages
 
    [SFDELT  -1]    Fatal error initializing TAE.
    [SFDELT  -2]    Fatal error reading TAE parameters.
    [SFDELT  -3]    Error deleting data at time ....
