# SNMOD

SNMOD moves selected sounding data from an input sounding file to
an output sounding file.


### Input Parameters
 
    SNFILE    Sounding data file
    SNOUTF    Output sounding file
    SNPARM    Sounding parameter list
    AREA      Data area
    DATTIM    Date/time
    LEVELS    Vertical levels
    VCOORD    Vertical coordinate type
    TIMSTN    Times/additional stations
    MRGDAT    Merge data file flag/part type
    IDNTYP    STNM or STID
 
 
### Program Description
 
SNMOD takes data from a GEMPAK sounding file, SNFILE, and
writes it to an output GEMPAK sounding file, SNOUTF.
This program can be used to subset the original dataset
by time and/or stations and to change the levels and
vertical coordinate.

The output dataset may be either a merged or unmerged data
set.  If the output file does not exist, a new file will be
created by this program.  If a new file is created, MRGDAT
will determine whether the output file will be a merged or
unmerged dataset.  Note that an error will result if the
input dataset is merged and the output dataset is unmerged.

The value of SNPARM will be ignored if the output dataset is
unmerged.  If a merged data file is to be created, SNPARM
will specify the parameters to be included.  In this case,
if SNPARM = DSET, the parameters in the input dataset will be
used.  If the output file is an existing merged dataset, the
parameters specified in SNPARM must be in the dataset.  In
this case, if SNPARM = DSET, the parameters in the output file
will be used.  Undecoded text (raw reports) may be included in
the input file, but will not be moved to the output file.

If the output file is to be created, the maximum number of
times and stations which can be stored in the file must be
specified in TIMSTN.

IDNTYP has been added to specify whether station numbers or
station identifiers will be used in referring to stations.
Generally, IDNTYP should be set to STNM to use station
numbers.  However, in datasets which do not have station
numbers, or which have a single invalid number, character
station identifiers must be used and IDNTYP must be set
to STID.


### Examples
 
1.  Put the mandatory data for stations in an area centered
    on IL at time 910819/1200 into a new unmerged sounding
    file, NEW.SND, which may contain 2 times and 150
    stations.

        SNFILE  =  gemdata:hrcbob.snd
        SNOUTF  =  new.snd
        SNPARM  =  dset
        AREA    =  il
        DATTIM  =  19/12
        LEVELS  =  man
        VCOORD  =  pres
        TIMSTN  =  2/150
        MRGDAT  =  no
        IDNTYP  =  stnm

2.  Add the stations at time 910819/00 to the file created in
    example 1.

        DATTIM  =  19/00

3.  Create an isentropic dataset containing the specified
    parameters with levels every 5 degrees.  Include all the
    US stations.

        SNFILE  =  gemdata:hrcbob.snd
        SNOUTF  =  thta.snd
        SNPARM  =  thta;pres;mixr;uwnd;vwnd;psym
        AREA    =  @us
        DATTIM  =  all
        LEVELS  =  250-450-5
        VCOORD  =  thta
        TIMSTN  =  4/150
        MRGDAT	=  yes

### Error Messages
 
    [SNMOD  -1]     Fatal error initializing TAE.
    [SNMOD  -2]     Fatal error reading TAE parameters.
    [SNMOD  -3]     Output file cannot be unmerged type.
    [SNMOD  -4]     A new file cannot be created with no times.
    [SNMOD  -5]     A new file cannot be created with no stations.
    [SNMOD  -6]     Parameter ... cannot be computed.
    [SNMOD  -7]     ... is character type; cannot be added to file.
    [SNMOD  -8]     Output vertical coordinate ... cannot be used.
    [SNMOD  -9]     No valid parameters were specified.
    [SNMOD -10]     Time ... cannot be added to the output file.
    [SNMOD -11]     Station ... cannot be added to output file.
    [SNMOD -12]     Parms do not match those in output data set.
    [SNMOD -13]     Output file name is blank.
