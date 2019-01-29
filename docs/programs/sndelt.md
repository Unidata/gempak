# SNDELT

SNDELT deletes data from a sounding data file.


### Input Parameters
 
    SNFILE    Sounding data file
    DATTIM    Date/time
    AREA      Data area
 
 
### Program Description
 
SNDELT deletes data from a sounding data file.  The stations
to be deleted are specified in AREA; the times to be deleted
are given in DATTIM.

If AREA is set to DSET or ALL, all the data present at the
times in DATTIM will be deleted, along with the headers for
those times.


### Examples
 
1.  Delete all the data at time 920722/1200.

        SNFILE  =  92jul.snd
        DATTIM  =  920722/1200
        AREA    =  dset

2.  Delete data at IAD for the most recent time in the file.

        SNFILE  =  realtime.snd
        DATTIM  =  last
        AREA    =  @iad


### Error Messages
 
    [SNDELT  -1]    Fatal error initializing TAE.
    [SNDELT  -2]    Fatal error reading TAE parameters.
    [SNDELT  -3]    Error deleting data at time ....
