# BFR2GP

BFR2GP transfers data from a Jack Woollen BUFR file into GEMPAK
sounding and surface data files.

### Input Parameters
 
    SNEFIL    Sounding edit file
    SFFSRC    Surface file source
    SNOUTF    Output sounding file
    SFOUTF    Output surface file
    SNPRMF    Sounding parameter packing file
    SFPRMF    Surface parameter packing file
    AREA      Data area
    DATTIM    Date/time
    TIMSTN    Times/additional stations
 
 

### Program Description
 
BFR2GP transfers data in Jack Woollen's BUFR format to GEMPAK
sounding and surface data files.  The input BUFR file must be a
blocked Jack Woollen type BUFR file.  Note that both a GEMPAK
sounding file and/or a surface file may be created by this program.

Since the BUFR file must be blocked, Jack Woollen has provided
a script named blkunblk to block a file that is not blocked.
To transfer a Jack Woollen BUFR file from another computer, first
unblock it on the original machine, then transfer the unblocked
file.  Finally, reblock the file on the receiving computer
before running BFR2GP.

The name of the Jack Woollen BUFR file is entered in SNEFIL.
Following the name of the BUFR file is the name of the Woollen
table file that was used to generate the BUFR file.  The two
file names are separated by |.  The table file must be included
for the decoding to work.  BUFR table files are ASCII text files.

If SFFSRC is not blank, only data having the message type (BUFR
Table A entry) specified by SFFSRC will be read.  If SFFSRC is
blank, data will be extracted from all message types.  The
data may be further restricted by entering SFFSRC according to
the following form:

      TYPE | CAT1=v1,v2,v3,...vN; CAT2=v1,...vN; ... CATn=v1,...

where each CATn gives the name of a BUFR parameter mnemonic, and
each vN gives a permissible value of the BUFR parameter.  A BUFR
message will be accepted if all of the values taken by the given
parameters match one of the listed values.  If a value is not in
the list, the message is rejected.  For example, suppose the
specification for parameter `CNT` is `CNT=2,3,7`.  The message is
rejected if `CNT` has a value other than 2,3,7 anywhere in the BUFR
message.

The output GEMPAK sounding data file name is entered in SNOUTF.

The output GEMPAK surface data file name is entered in SFOUTF.

Conversion of the data is driven by the contents of conversion
table files.  The names of these files are entered in SNPRMF and
SFPRMF.  There must be a conversion table file for sounding data
and one for surface data.  If no sounding data is requested, then
only a surface data conversion table is needed.  However, when
only sounding data is to be processed, a surface conversion table
file is still needed for some two to eleven identification, time,
and location parameters.  These parameters are described in the
following table:

      GEMPAK name     Description     Units/type     Required?

       STID           Identifier       CHARACTER        NO
       BLOK           Block number     INTEGER          NO
       STNM           Station number   INTEGER          NO
       SLAT           Latitude         Degrees          YES
       SLON           Longitude        Degrees          YES
       SELV           Elevation        Meters           NO
       YYYY           Year (4 digit)   INTEGER          NO
       MMMM           Month            INTEGER          NO
       DDDD           Day              INTEGER          NO
       HHHH           Hour             INTEGER          NO
       NNNN           Minute           INTEGER          NO
       TOFF*          Offset time      Seconds          NO
       TADD**         Offset time      Seconds          NO

> * TOFF is used in computing observation time.
> * TADD is used in computing actual station time.
> * Either TADD or TOFF, but not both may be given.

The time associated with a station may be determined in one of
three ways:

1)  Directly, using YYYY, MMMM, DDDD, HHHH, NNNN.  If the
    data time need only be known to the nearest hour, then
        omit NNNN.  TOFF and TADD are ignored, and the obs
    time is set to zero.

    If NNNN is available, the time may be rounded to the
    nearest fraction of an hour as determined by the input
    for the factor and term associated with NNNN (see
    below).  For example, if the factor for NNNN is
    30, the data is rounded to the nearest half hour.  The
    values for the conversion factor and term are used as
    follows to compute the rounded minute:

        min = F * NEAREST_INT ( ( NNNN + T ) / F )
        IF ( min >= 60 ) min = 0
        IF ( min < 0 ) min = 0

    where F is the factor and T is the term.  Note that the
    term serves as an offset (usually zero).  If `F = 1`. for
    NNNN, no rounding is done.

2)  Indirectly, from the BUFR block time, with the obs time set using TOFF, if it is given.

3)  Indirectly, from the BUFR block time plus the offset time specified as TADD.  The obs time is set to 0.

If the block number (BLOK) is present, it is added to the
station number (STNM) to form the GEMPAK integer station number.
Therefore, the block number must be multiplied by a factor that
is large enough to yield the correct number of digits for the
station number (usually either five or six).  The use of the
conversion factors is described below.

If no block number is present, the station number alone is used
for the GEMPAK integer station identifier.  If no STID is present,
the character station ID defaults to a blank.  If neither a station
number or a character ID is present, a sequence number is assigned
to each observation to serve as a station number, and the character
station ID is blank.

Use the conversion factor and term to convert to the above units
if the corresponding BUFR parameters are in different units (see
below).  These parameters must always appear first, in any order,
in the surface data conversion table file, whose name is entered
in SFPRMF.  This information is stored in the header for the
station in the GEMPAK file; therefore, these parameters do not
count toward the limit of MMPARM (usually 40) parameters per GEMPAK
file.

The conversion table files contain four columns of data.  The
four columns are:

    BUFR name  GEMPAK name  Conversion Factor  Conversion Term

The BUFR name entry comes from the Jack Woollen BUFR table and
corresponds to a TABLE B ENTRY.  The GEMPAK name is the four-
character name used for that parameter in GEMPAK.  The conversion
factor and term are used to do unit conversions or linear trans-
formations of the data before storage to the GEMPAK file.  The
GEMPAK value is calculated as follows:

    g = b * F + T

where g is the GEMPAK value, `b` is the unpacked BUFR value, `F` is
the conversion factor, and `T` is the conversion term.  For no
conversion at all, set `F = 1.0` and `T = 0.0`.  The values in each
row of the table may be separated by spaces or tabs.  If a row
begins with `!`, that row is ignored.  Comments may be entered into
the table using this feature.

If a surface parameter is replicated in the BUFR file (one param-
eter name stands for several stored values), its values can be
transfered to the GEMPAK surface data file.  To accomplish this,
make an entry in the surface conversion table file for each value
of the replicated parameter.  These entries will have the same
BUFR name but different GEMPAK names.  There does not have to
be an entry for all replications.  But, there must be enough
entries to cover the ones desired in sequential order.  For
example, if only the third replicated value is needed, the first
two must be transfered anyway.

A backslash "\" may be placed at the beginning of an otherwise
blank line in the conversion table file.  This triggers a
separate READ operation on the BUFR file for the parameters
listed after it.

The packing information is computed from the information in the
Jack Woollen BUFR table file, taking into account any linear
transformation of the data.

By default the sounding file is a merged sounding data file.  An
unmerged file can be created by specifying a list of observed
parameter names in the following order:

	PRES HGHT TEMP DWPT DRCT SPED LFLG (or LCAT)

LFLG will be checked to make the following correspondences:

	INTEGER VALUE		DATA TYPE

	    64			Surface
	    16			Tropopause level
	     8			Maximum wind level
	    32			Mandatory level
	     4			Significant temperature level
	     2			Significant wind level

LCAT will be checked to make the following correspondences:

	INTEGER VALUE		DATA TYPE

	     0			Surface
	     5			Tropopause level
	     1			Mandatory level
	     2			Significant temperature level
	   3 or 4		Significant wind level

These are Office Note 29 categories.

The various data groups will all be placed in the same GEMPAK
sounding file.  GEMPAK programs that process or display sounding
data will be able to merge these data partitions.

TIMSTN specifies the maximum number of date/times and the maximum
number of stations to be stored in the GEMPAK file(s).  If only
one number is entered for TIMSTN, then a ship data surface file
is created, allowing each report to have a separate time stamp.
If a sounding file is being created at the same time, this number
will be used for the number of stations, the number of times will
default to 24.

AREA specifies the lat/lon bounds of the area over which data is
to be accepted.  Data outside of these lat/lon bounds on a CED
projection will not be included.  If AREA is blank, at lat/lon
values are accepted.

DATTIM specifies a time range over which data is to be accepted
for transfer into the GEMPAK file.  It is entered as two complete
GEMPAK times separated by -.  If all times are to be accepted,
enter a blank.


### Examples
 
1.  Create sounding and surface data files from the BUFR output
file name 950612.bufr containing hourly profiles and surface
data from a 48-h forecast.  Subset the area and time.  Extract
only ETACLS0 data.

        SNEFIL = 950612.bufr|eta_bufr.table
        SFFSRC = ETACLS0
        SNOUTF = 950612.snd
        SFOUTF = 950612.sfc
        SNPRMF = snbuf_emc.conv
        SFPRMF = sfbuf_emc.conv
        AREA   = 30;-100;50;-60
        DATTIM = 950612/0600-950613/0400
        TIMSTN = 49/500

2.  Create only a sounding file from prepbufr input.  The
station location information is in BUFR parameters defined
in stn_conv.tbl.  Subset the area, but not the time.
Extract data only from the ADPUPA message types.

        SNEFIL = prepbufr|prepbufr.table
        SFFSRC = ADPUPA
        SNOUTF = pbfr.snd
        SFOUTF =
        SNPRMF = snd_conv.tbl
        SFPRMF = stn_conv.tbl
        AREA   = 10;-120;75;-60
        DATTIM =
        TIMSTN = 49/500

3.  Create only a surface file from prepbufr input.  Do not
subset area or time.  Do not restrict the message type.

        SNEFIL = prepbufr|prepbufr.table
        SFFSRC =
        SNOUTF =
        SFOUTF = pbfr.sfc
        SNPRMF =
        SFPRMF = sfc_conv.tbl
        AREA   =
        DATTIM =
        TIMSTN = 49/500

### Error Messages
 
    [BFR2GP  +3]    Message rejected by parameter screening.
    [BFR2GP  +2]    No parameters for the surface data file.
    [BFR2GP  +1]    End of input file -- all done.
    [BFR2GP  -1]    Fatal error initializing TAE.
    [BFR2GP  -2]    Fatal error reading TAE parameters.
    [BFR2GP  -3]    Missing station lat/lon.
    [BFR2GP  -4]    Missing BUFR data and/or table file name.
    [BFR2GP  -5]    BUFR file could not be opened.
    [BFR2GP  -6]    Sounding parameters' list could not be made.
    [BFR2GP  -7]    Existing file sounding parms differ from those requested.
    [BFR2GP  -8]    Cannot open/create the GEMPAK sounding data file.
    [BFR2GP  -9]    Surface parameters' list could not be made.
    [BFR2GP -10]    Existing file surface parms differ from those requested.
    [BFR2GP -11]    Cannot open/create the GEMPAK surface data file.
    [BFR2GP -12]    No station location information.
    [BFR2GP -13]    BUFR file read failed.
    [BFR2GP -14]    Surface data read failed.
    [BFR2GP -15]    Not enough replicated data.
    [BFR2GP -16]    Sounding data read failed.
    [BFR2GP -17]    Number of sounding levels varies with parameters.
    [BFR2GP -18]    Too many sounding parms.
    [BFR2GP -19]    Too many surface parms.
    [BFR2GP -20]    SFPARM count does not match requested number.
    [BFR2GP -21]    SNPARM count does not match requested number.
    [BFR2GP -22]    Cannot create a GEMPAK time for the data.
    [BFR2GP -23]    Sounding data GEMPAK time cannot be set.
    [BFR2GP -24]    Surface data GEMPAK time cannot be set.
    [BFR2GP -25]    Sounding station cannot be set.
    [BFR2GP -26]    Surface station cannot be set.
    [BFR2GP -27]    Data array overflow.
    [BFR2GP -28]    No output files are open.
    [BFR2GP -29]    Existing sounding file needs to be unmerged type.
    [BFR2GP -30]    Cannot create unmerged sounding file.
    [BFR2GP -31]    Cannot write mandatory level data.
    [BFR2GP -32]    Cannot write significant level data.
    [BFR2GP -33]    @ is not valid for setting subset area filter.
    [BFR2GP -34]    Standard UPA sounding parameters were not given correctly.
    [BFR2GP -35]    There must be 7 UPA sounding parameters for unmerged file.
    [BFR2GP -36]    Significant-level wind data cannot be on both p and z.
    [BFR2GP -37]    Invalid input for SFFSRC.
    [BFR2GP -38]    Not all screening parameters were found.
    [BFR2GP -39]    Too many sounding levels.
