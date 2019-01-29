# SNDSLIST

SNDSLIST lists upper air data from a sounding file for specified
vertical levels and stations in a format used for the
AMS DataStreme web site.

### Input Parameters
 
    SNFILE    Sounding data file
    AREA      Data area
    DATTIM    Date/time
    OUTPUT    Output device/filename
 
 

### Program Description
 
SNLIST lists parameters derived from an upper air data set for
the requested stations and times.

The parameter list is predefined for this program. The list
will always be:

    PRES;HGHT;TMPC;DWPC;RELH;DRCT;SKNT;MIXR

There will also be no stablility indices listed.

The program will also automatically merge the data and show
isobaric data at all levels available.

The output will be formatted for the AMS DataStreme Project
web site. There will be no missing values and some data
will have no decimal places shown.


### Examples
 
1.	List the parameters in the data set for the stations IAD
and ACY for the last time in the data file.
    
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        AREA	 =  @iad;acy
        DATTIM	 =  last
        OUTPUT	 =  t

### Error Messages
 
    [SNDSLIST  -1]  Fatal error initializing TAE.
    [SNDSLIST  -2]  Fatal error reading TAE parameters.
    [SNDSLIST  -3]  There are no parameters to be listed.
    [SNDSLIST  -4]  Level parameter ... cannot be computed.
    [SNDSLIST  -5]  Parm ... is character; cannot be listed.
    [SNDSLIST  -6]  Vertical coordinate ... cannot be computed.
    [SNDSLIST  -7]  Station parameter ... cannot be computed.
    [SNDSLIST  -8]  Parm ... is character; cannot be listed.
    [SNDSLIST  -9]  No stations reported at these times.
    [SNDSLIST -10]  Too many parameters; ... cannot be added.
    [SNDSLIST -11]  The data set is merged.
    [SNDSLIST -12]  No inputs for levels.
