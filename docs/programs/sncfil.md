# SNCFIL

SNCFIL creates a new GEMPAK sounding file.


### Input Parameters
 
    SNOUTF    Output sounding file
    SNPRMF    Sounding parameter packing file
    STNFIL    Station information file
    MRGDAT    Merge data file flag/part type
    TIMSTN    Times/additional stations
 
 
### Program Description
 
SNCFIL creates a GEMPAK sounding file.  The file may be
created to store merged or unmerged data.  Merged data files
may contain any meteorological parameters with all parameters
included at each level.  Unmerged files store the mandatory
data and significant-level temperature and wind data as
separate parts.

MRGDAT must be set to YES or NO.  If YES, a merged file will
be created.  The parameters to be included in the file must be
specified in SNPRMF.  If MRGDAT is NO, an unmerged file will be
created.  In this case, the type of unmerged file can be included
following a slash.  Type 1 creates a file with only mandatory
data below 100 mb.  Type 2 specifies a file with mandatory and
significant-level data below 100 mb, and type 3, which is the
default, specifies a file with mandatory and significant-level
data below and above 100 mb.

SNPRMF contains information about the parameters to be included
in a merged file.  Either a list of parameters or the name of a
packing file may be entered.  If a list is entered, the parameters
must be separated with semicolons; packing information may also
be included after a slash with the minimum and maximum values
and the resolution separated using dashes.  For example, to
include temperature and dewpoint in a file without packing,
SNPRMF may be entered as:

		SNPRMF = TMPC;DWPC

To pack the data, use:

		SNPRMF = TMPC/-127-63-.1;DWPC/-63-63-.1

SNPRMF may also contain the name of a packing file.  A packing
file for data which is not to be packed contains a list of
parameters with one parameter per line.  In a file for packed
data, each line must include the parameter name, the minimum
and maximum data values and the resolution, all separated with
spaces.  The default packing file for merged sounding data is
`SNPACK.TBL`.

If STNFIL is not blank, information about all the stations in
STNFIL will be added to the data set.  Space will be left in the
file for the additional number of stations specified as the
second parameter in TIMSTN.  Note that an error will result if
STNFIL is blank and TIMSTN does not request additional stations.


### Examples
 
1.  Create a sounding file called SOUND.DAT containing merged
    data.  Use the standard station and parameter files.  Leave
    room in the file for 100 stations in addition to the stations
    in STNFIL and allow 15 times to be added to the file.

        SNOUTF  =  sound.dat
        SNPRMF  =  snpack.tbl
        STNFIL  =  snstns.tbl
        MRGDAT  =  yes
        TIMSTN  =  15/100

2.  Create an unmerged sounding file called `WEEKLY.SND` which
    will store mandatory and significant-level data below and
    above 100 mb.  Do not add any stations now.  Leave room for
    14 times and 300 stations.

        SNOUTF  =  weekly.snd
        SNPRMF  =
        STNFIL  =
        MRGDAT  =  no/3
        TIMSTN  =  14/300


### Error Messages
 
    [SNCFIL  +2]    Negative number of stations entered -- 0 used.
    [SNCFIL  -1]    Fatal error initializing TAE.
    [SNCFIL  -2]    Fatal error reading TAE parameters.
    [SNCFIL  -3]    Error opening station file ... .
    [SNCFIL  -4]    File does not include room for any stations.
    [SNCFIL  -5]    File does not include room for any times.
    [SNCFIL  -6]    SNPRMF is incorrectly specified.
    [SNCFIL  -7]    The file name for the new file is blank.
    [SNCFIL  -8]    Vertical coordinate = ... -- specify more parameters.
