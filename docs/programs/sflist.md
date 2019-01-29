# SFLIST

SFLIST lists surface data from a GEMPAK surface data file.

### Input Parameters
 
    SFFILE    Surface data file
    AREA      Data area
    DATTIM    Date/time
    SFPARM    Surface parameter list
    OUTPUT    Output device/filename
    IDNTYP    STNM or STID
 
 

### Program Description
 
SFLIST lists any parameters which can be derived from the data
in a surface data file.  The stations and times to be included
are specified in AREA and DATTIM.  If no data are reported for
a station, that station will not be listed.  The listings will
be grouped by time.

If the surface file contains both decoded and undecoded text
data, both types can be listed by specifying GEMPAK parameters
along with parameter 'TEXT'.  The special reports may also be
listed in an undecoded format using the paramter 'SPCL'.  TEXT
and SPCL may appear anywhere in the list of parameters.

Conditions can be specified for the parameters.  The conditions
are documented in the SFPARM documentation.

 
### Examples
 
1.	List the decoded and undecoded text air and dewpoint
    temperatures in Fahrenheit and the mean sea-level pressure
and weather of stations in Pennsylvania for the latest time.
Use a locally created surface file that contains both types
of data called SAOTEXT.SFC.

        SFFILE	 =  SAOTEXT.SFC
        AREA	 =  @pa
        DATTIM	 =  last
        SFPARM	 =  tmpf;dwpf;pmsl;wthr;text
        OUTPUT	 =  t
        IDNTYP	 =  stid

2.	List the same parameters for PIT and BWI at 0500 and
0600 GMT.

        SFFILE	 =  SAOTEXT.SFC
        AREA	 =  @pit;bwi
        DATTIM	 =  /05-06
        SFPARM	 =  tmpf;dwpf;pmsl;wthr;text
        OUTPUT	 =  t
        IDNTYP	 =  stid

3.	List the temperature, dewpoint, pressure, and weather
for stations in Pennsylvania which are reporting
thunderstorms and dewpoint temperatures greater than
65 degrees Fahrenheit.

        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        AREA	 =  @PA
        DATTIM	 =  /05-06
        SFPARM	 =  tmpf;dwpf>65;pmsl;wthr=t
        OUTPUT	 =  T
        IDNTYP	 =  STID

4.	List the latest undecoded SAO text data for stations in
Pennsylvania. Use a locally created surface file with SAO
decoded and text data called SAOTEXT.SFC.
    
        SFFILE   =  SAOTEXT.SFC
        AREA     =  @PA
        DATTIM   =  last
        SFPARM   =  text;spcl
        OUTPUT   =  T
        IDNTYP   =  STID


### Error Messages
 
    [SFLIST  +1]    Parameter ... is not computable.
    [SFLIST  -1]    Fatal error initializing TAE.
    [SFLIST  -2]    Fatal error reading TAE parameters.
    [SFLIST  -3]    No stations reporting data.
    [SFLIST  -4]    No valid computable parameters.
