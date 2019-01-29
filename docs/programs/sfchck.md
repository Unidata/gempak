# SFCHCK

SFCHCK reads a GEMPAK surface data file and produces a table
of stations and an indicator showing whether each station
reported data at each time in the file.


### Input Parameters
 
    SFFILE    Surface data file
    AREA      Data area
    DATTIM    Date/time
    OUTPUT    Output device/filename
    IDNTYP    STNM or STID
    STNTYP    STN reporting type
 
 
### Program Description
 
SFCHCK can be used to quickly determine which stations report
data in an individual GEMPAK surface data file.  It is useful
for determining which stations do not report data as well as
new stations that are not in the data file.

The header of the table prints out each hour in the file.

Each station's listing appears as a separate row in the table.
For each time, a "+" indicates that the station reported data,
while a "-" indicates that the station did not report data.
The total number of times that the station reported data appears
at the end of the row.

If the station did not report at any time in the file, its
listing will be prepended with an "N".  If the station is not
in the station table, its listing will be prepended by a "#".

The DATTIM and AREA parameters can be modified to specify a
range of times and / or regions.  Note that AREA must be set
to DSET for unlisted stations to appear.

The STNTYP parameter can be modified to specify the reporting
characteristics of the stations.  For more information, please
consult the STNTYP documentation.


### Examples
 
1.	Generate a table for the most recent decoded surface METAR
file.  List all stations in the file.  Output the table to
the file "sfchck.fil".

        SFFILE	 =  hrly
        AREA	 =  dset
        DATTIM	 =  all
        OUTPUT	 =  f
        IDNTYP	 =  stid
        STNTYP   =  A

2.	Do the same as above, but only list those stations that ARE
in the station table and do NOT report any data.

        SFFILE	 =  hrly
        AREA	 =  dset
        DATTIM	 =  all
        OUTPUT	 =  f
        IDNTYP	 =  stid
        STNTYP   =  ML

3.	Do the same as above, but only list those stations that are
NOT in the station table and DO report data.

        SFFILE	 =  hrly
        AREA	 =  dset
        DATTIM	 =  all
        OUTPUT	 =  f
        IDNTYP	 =  stid
        STNTYP   =  RU

4.	Do the same as (3), but use flash flood guidance data from
4/3/97.

        SFFILE	 =  $OBS/ffg/970403.ffg
        AREA	 =  dset
        DATTIM	 =  all
        OUTPUT	 =  f
        IDNTYP	 =  stid
        STNTYP   =  RU

5.	List only stations in Vermont for the hours 0300 - 0900 UTC.
Output the table to the screen.  Use the current METAR file.

        SFFILE	 =  hrly
        AREA	 =  @VT
        DATTIM	 =  /03-09
        OUTPUT	 =  T
        IDNTYP	 =  stid
        STNTYP   =  A


### Error Messages
 
    [SFCHCK  -1]    Fatal error initializing GEMPAK.
    [SFCHCK  -2]    Fatal error reading GEMPAK parameters.
    [SFCHCK  -3]    No valid stations could be found.
