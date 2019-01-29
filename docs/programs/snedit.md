# SNEDIT

SNEDIT adds data in a sequential edit file to a sounding file.


### Input Parameters
 
    SNEFIL    Sounding edit file
    SNFILE    Sounding data file
    TIMSTN    Times/additional stations
 
 
### Program Description
 
SNEDIT adds information from a sequential file to a
GEMPAK sounding file.  It can be used to add or replace
data at a station.  The program can be used to add data
to either merged or unmerged data sets.

The data to be added must be in a text file, SNEFIL.  This
file can be created using F as the output device in SNLIST.
MRGDAT, which can be set in SNLIST, is a flag indicating
whether the data to be written will be merged or written
as separate parts.  If the edit file contains unmerged
data, the part name must be included and the parameters
must be in the order expected by GEMPAK.  Undecoded text
(raw reports) should not be included in SNEFIL.

If SNFILE exists, the data will be added to the file.  If it
does not already exist, a new file will be created.  A new
file will be a merged or unmerged data set, depending on the
type of data in SNEFIL.  The maximum number of stations and
times allocated in a new file will be read from TIMSTN.

For merged edit data files, the parameters to be edited must
be specified at the beginning of the edit file. For example:

		PARM=PRES;TMPC;DWPC;DRCT;SPED;HGHT

The parameter line must contain the string, PARM, and =.
Note that the output of SNLIST is SNPARM = xxxx;yyyy;..., which
is valid. If the parameter list must be continued on the
next line, the last character on the current line must be a
semicolon.  If SNFILE is an existing file, the parameters
listed in the edit file must be exactly those in the data set,
although SNFILE may include undecoded text.  For unmerged data,
information about the parameters is not necessary.


The information to be added to the sounding file consists of
a series of station entries including information for the station,
followed by station data,Station information must be listed
as  KEYWORD = value, where the valid keywords are STID, STNM, SLAT,
SLON, SELV, and TIME. The TIME keyword must be found for each
station.  Either STID or STNM is also required.  If the station is not
already in the file, the station identifier, station number,
latitude, longitude, and elevation found will be added to the file.
If the station is already in the file, the station location and
elevation will not be changed.  Note that program [SNSTNS](snstns) can
be used to modify station header values.


### Examples
 
1.  Add the data in the edit file, `SNLIST.FIL`, to the file,
    `SOUND.DAT`, which does not exist.

        SNEFIL  = snlist.fil
        SNOUTF  = sound.dat
        TIMSTN  = 1/ 10

	The file `SNLIST.FIL` follows:

	    SNPARM = PRES;TMPC;DWPT;DRCT;SPED;HGHT
        STNPRM = SHOW;LIFT;KINX
    
        STID = IAD	STNM =  72403	    TIME = 841227/1200
        SLAT = 38.98	SLON = -77.46	    SELV =    85.0
    
        SHOW = 14.11	LIFT = 25.60	KINX = 11.50
    
           PRES      TMPC      DWPT     DRCT     SPED     HGHT
        1024.00	     2.40     -1.40     0.00     0.00    85.00
        1000.00	     2.20     -3.80 -9999.00 -9999.00   277.00
         850.00	     5.60     -9.40 -9999.00 -9999.00  1592.00
         700.00	    -1.90     -2.30 -9999.00 -9999.00  3167.00
         500.00	   -15.70    -22.70 -9999.00 -9999.00  5790.00
         400.00	   -28.90    -33.40 -9999.00 -9999.00  7430.00
         300.00	   -44.30    -93.30 -9999.00 -9999.00  9420.00
         250.00	   -55.30   -104.30 -9999.00 -9999.00 10620.00
         200.00	   -66.50   -115.50 -9999.00 -9999.00 12000.00
         150.00	   -65.90   -114.90 -9999.00 -9999.00 13750.00
         100.00	   -66.90   -115.90 -9999.00 -9999.00 16220.00

    The merged file SOUND.DAT will be created.  It will contain the
    parameters `PRES;TMPC;DWPT;DRCT;SPED;HGHT`.  Space for a
    maximum of 1 time and 10 stations will be allocated.  The
    time 841227/1200 and station IAD will be added to the
    file and then the data for that time and station will be
    added.  Note that the stability indices will be ignored.

2.  Add the data in the edit file, `IAD.FIL`, to the unmerged file,
    `REALTIME.SND`, which already exists.

        SNEFIL  = realtime.snd
        SNOUTF  = iad.fil
        TIMSTN  = 0

	The file `IAD.FIL` follows:
    
        STID = IAD	STNM = 72403	TIME = 900823/0000
        SLAT = 38.98	SLON = -77.46	SELV = 85.0
    
        TTAA      0
           PRES     TMPC     DWPC     DRCT     SPED     HGHT
        1008.00    19.20    18.90     0.00     3.00 -9999.00
        1000.00	   19.00    18.20 -9999.00 -9999.00   150.00
         850.00    14.40    13.30   160.00     8.00  1538.00
         700.00	    5.40     4.30   240.00     6.00  3157.00
         500.00    -8.10    -9.80   215.00     7.00  5850.00
         400.00   -19.90   -49.90   225.00    11.00  7540.00
         300.00   -34.10   -64.10   235.00    18.00  9620.00
         250.00   -44.30 -9999.00   245.00    16.00 10870.00
         200.00   -54.30 -9999.00   250.00    22.00 12330.00
         150.00   -60.50 -9999.00   265.00    14.00 14140.00
         100.00   -64.50 -9999.00   235.00     2.00 16620.00
    
        TTBB      0
        PRES	    TMPC     DWPC
        1008.00	   19.20    18.90
         850.00	   14.40    13.30
         684.00	    3.60     1.20
         602.00	    0.00    -1.30
         481.00	   -9.90   -12.50
    
        PPBB      0
        HGHT	    DRCT     SPED
           0.00	    0.00     3.00
         914.00	  120.00    12.00
        1219.00	  130.00    10.00
        1829.00	  180.00     7.00
        2134.00	  195.00     6.00
        2438.00	  210.00     6.00


### Error Messages
 
    [SNEDIT  +1]    Data for ... has been added to the file.
    [SNEDIT  -1]    Fatal error initializing TAE.
    [SNEDIT  -2]    Fatal error reading the TAE parameters.
    [SNEDIT  -3]    The edit file cannot be opened.
    [SNEDIT  -4]    List of parameters not found in edit file.
    [SNEDIT  -5]    Too many parameters in edit file.
    [SNEDIT  -6]    Edit file ... cannot be opened.
    [SNEDIT  -7]    Error creating new sounding file ....
    [SNEDIT  -8]    Edit file parms don't match those in data set.
    [SNEDIT  -9]    First parameter not a valid vertical coordinate.
    [SNEDIT -10]    Error opening existing sounding file ....
    [SNEDIT -11]    Cannot write merged data to unmerged data set.
    [SNEDIT -12]    Sounding file can't be created with MAXTIM <= 0.
    [SNEDIT -13]    Sounding file can't be created with MAXSTN <= 0.
    [SNEDIT -14]    The time ... was not added to the file.
    [SNEDIT -15]    The station ... was not added to the file.
    [SNEDIT -16]    Error writing data to file.
    [SNEDIT -17]    The part ... has invalid parameters.
    [SNEDIT -18]    Edit file ... has an invalid format.
    [SNEDIT -19]    Station ... has an invalid format.
    [SNEDIT -20]    Cannot write unmerged data to merged data set.
