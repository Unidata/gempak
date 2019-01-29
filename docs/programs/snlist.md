# SNLIST

SNLIST lists upper air data from a sounding file for specified
vertical levels and stations.

### Input Parameters
 
    SNFILE    Sounding data file
    AREA      Data area
    DATTIM    Date/time
    SNPARM    Sounding parameter list
    STNDEX    Stability indices
    LEVELS    Vertical levels
    VCOORD    Vertical coordinate type
    OUTPUT    Output device/filename
    MRGDAT    Merge data file flag/part type
 
 

### Program Description
 
SNLIST lists parameters derived from an upper air data set for
the requested stations and times.

Parameters which can be computed at various levels in the
data set should be specified in SNPARM.  Parameters which have
a single value at the station, such as stability indices,
should be specified in STNDEX.

Any parameters which can be computed from the data set
parameters may be listed.  If the vertical levels requested
are not present in the data set, the data will be interpolated
between existing levels.  Data will also be interpolated to a
new vertical coordinate system, if requested.

Data from an unmerged data set can be listed without
being merged if MRGDAT is set to NO.  If unmerged data
are listed, SNPARM, VCOORD and LEVELS will be ignored, except
that parameter 'TEXT' is allowed for SNPARM.

If the sounding file contains both decoded data and undecoded
text data, both types can be listed by specifying parameter
'TEXT' in the parameter list for either SNPARM or STNDEX.
TEXT may appear anywhere in the list of parameters.  Only
text data for parts TTAA, TTBB, PPBB and TTCC will be listed.

 
### Examples
 
1.	List the parameters in the data set for the stations IAD
and ACY at the mandatory levels.  Also list the Showalter,
lifted and K indices.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        AREA	 =  @iad;acy
        DATTIM	 =  last
        SNPARM	 =  dset
        STNDEX	 =  show;lift;kinx
        LEVELS	 =  man
        VCOORD	 =  pres
        OUTPUT	 =  t
        MRGDAT	 =  YES

2.	List the unmerged data from IAD.

        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        AREA	 =  @iad
        DATTIM	 =  last
        SNPARM	 =  dset
        STNDEX	 =  show;lift;kinx
        LEVELS	 =  man
        VCOORD	 =  pres
        OUTPUT	 =  t
        MRGDAT	 =  no

3.  List the pressure, mixing ratio, u- and v-wind components
    and the Montgomery stream function on the isentropic
    surfaces from 300 to 400 degrees in 10-degree increments.
    Change the station to ACY.
    
        SNFILE	 =  $GEMPAK/data/hrcbob.snd
        AREA	 =  @acy
        DATTIM	 =  last
        SNPARM	 =  pres;mixr;uwnd;vwnd;psym
        STNDEX	 =
        LEVELS	 =  300-400-10
        VCOORD	 =  thta
        OUTPUT	 =  t
        MRGDAT	 =  yes

4.	List the unmerged data and the undecoded text for station
BOI.  Use a locally created sounding file containing both
types of data called `DATA.SND`.

        SNFILE	 =  DATA.SND
        AREA	 =  @boi
        DATTIM	 =  last
        SNPARM	 =  text
        STNDEX	 =
        LEVELS	 =
        VCOORD	 =
        OUTPUT	 =  t
        MRGDAT	 =  no

### Error Messages
 
    [SNLIST  -1]    Fatal error initializing TAE.
    [SNLIST  -2]    Fatal error reading TAE parameters.
    [SNLIST  -3]    There are no parameters to be listed.
    [SNLIST  -4]    Level parameter ... cannot be computed.
    [SNLIST  -5]    Parm ... is character; cannot be listed.
    [SNLIST  -6]    Vertical coordinate ... cannot be computed.
    [SNLIST  -7]    Station parameter ... cannot be computed.
    [SNLIST  -8]    Parm ... is character; cannot be listed.
    [SNLIST  -9]    No stations reported at these times.
    [SNLIST -10]    Too many parameters; ... cannot be added.
    [SNLIST -11]    The data set is merged.
    [SNLIST -12]    No inputs for levels.
