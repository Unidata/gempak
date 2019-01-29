# NAMSND

NAMSND transfers model profile output in BUFR to GEMPAK sounding
and surface data files.

### Input Parameters
 
    SNBUFR    BUFR model sounding file
    SNOUTF    Output sounding file
    SFOUTF    Output surface file
    SNPRMF    Sounding parameter packing file
    SFPRMF    Surface parameter packing file
    TIMSTN    Times/additional stations
 
 

### Program Description

NAMSND transfers model profile output in BUFR to GEMPAK
sounding and surface data files.  The input BUFR file must be a
Jack Woollen type BUFR file whose first message is a table of
information about the file contents.  Note that both a GEMPAK
sounding file and a surface file are created by this program.

NAMSND reads the table from the first BUFR message to get
information on the parameters in the file.  A print out of this
information to a local file called bufr_table.dump is done if
SNPRMF and SFPRMF are both set to blank.  No GEMPAK files or
other output files are created in this case.

If the surface file name SFOUTF is followed by +, then a second
surface file is opened.  Its name is that of the first with the
suffix _aux.  The packing table for this auxiliary file is
expected to have the name of that specified in SFPRMF with the
suffix _aux.  The auxiliary file contains any primary parameters
for which there is no space in the primary surface data file,
followed by diagnosed parameters.  The parameter table entries for
the auxiliary file must reflect this requirement.  So, the
parameter file for the auxiliary file starts with a continuation
of the list of primary parameters in the order expected from the
BUFR data file followed by any diagnosed parameters.  If there is
no overflow of primary parameters, then only diagnosed parameters
are found in the auxiliary file.  When an auxiliary file is
used, any diagnosed parameters that are desired in the primary
file must be listed in the parameter table file for both the
primary and the auxiliary file.  A list of diagnosed parameters
is given below.

If the BUFR data file name is followed by |sss=#####, where sss
is a 3-character station ID and ##### is the corresponding station
number, then an ASCII file name prof.sss is generated.  This
file will contain the output for only station #####.  If no
GEMPAK files are needed, set SNOUTF and SFOUTF to blank.  The
parameter files are required for both the GEMPAK files and the
ASCII file.

NOTE ABOUT THE PARAMETER TABLE ENTRIES:

Diagnosed or extra computed parameters are added at the end of the
parameter list for surface data.  They may be added anywhere for
profile data, but the last two columns must contain the GEMPAK
names of the actual BUFR parameters in correct order (as shown in
the file bufr_table.dump).  The penultimate column is the scaling
factor to be applied to the parameter named in the last column.
Note that the names in the last column need not agree with the
names in the first column for sounding parameter files.  Thus, in
the sounding parameter file, the last two columns are essentially
independent of the first four columns.  Surface parameter files
do not have the extra last column of names.

None of the parameter names in the GEMPAK parameter file have to be
the same as the names found in the file bufr_table.dump.  The file
bufr_table.dump gives the order in which the parameters must be
present in the GEMPAK parameter files.  The replication of surface
parameters is not indicated in bufr_table.dump.  Currently, the
only replicated surface parameters are the soil layer parameters
in the sequence SLYR (class 1 Eta file only).


    --------------------------------------------------------------
	     MODEL Sounding/Surface GEMPAK Parameter Names

    Name   TYP  C        Definition                      Units

    PRES#  SND  2  Pressure                               mb
    TMPC#  SND  2  Temperature                            C
    DWPC#  SND  2  Dewpoint temperature                   C
    SPED#  SND  2  Speed                                  m/s
    DRCT#  SND  2  Direction                              degrees
    OMEG#  SND  2  Omega                                  pa/s
    HGHT#  SND  2  Height                                 m

    DTCP   SND  1  Temp tndncy from conv phase change     K / day
    DTGP   SND  1  Temp tndncy from grd-scl phase change  K / day
    DTSW#  SND  1  Temp tndncy from short-wave rad        K / day
    DTLW#  SND  1  Temp tndncy from long-wave rad         K / day
    DTAR#* SND  1  Temp tndncy from all rad               K / day

    CFRL#  SND  1  Cloud fraction in layer                %
    CWTR#  SND  1  Cloud water                            Kg / Kg
    TKEL#  SND  1  Turbulent kinetic energy               J / Kg

    RADH   SND  -  Net radiative heating rate             K/day

                      - - - - - - - - - - -

    PMSL#  SFC  2  Mean sea-level pressure                mb
    PRES#  SFC  2  Surface pressure                       mb
    TMPC#@ SFC  2  First level temperature                C
    DWPC#@ SFC  2  First level dewpoint temperature       C
    SPED#@ SFC  2  First level wind speed                 m/s
    DRCT#@ SFC  2  First level wind direction             degrees
    SKTC#  SFC  2  Earth surface temperature              C
    SBTC#  SFC  1  Bottom soil temperature                C
    SLTC#  SFC  0  Layer 1 soil temperature               C
    TMIN   SFC  1  Min lowest layer air temp over 1 hr    C
    TMAX   SFC  1  Max lowest layer air temp over 1 hr    C

    FXLH   SFC  1  Flux of latent heat (evaporation)      W / m**2
    FXLP   SFC  1  Potential flux of latent heat          W / m**2
    FXSH   SFC  1  Flux of sensible heat                  W / m**2
    FXSS   SFC  1  Flux of sub-surface heat               W / m**2
    FXSN   SFC  1  Flux of snow phase change heat         W / m**2
    SWRD   SFC  1  Short-wave radiation downward flux     W / m**2
    SWRU   SFC  1  Short-wave radiation upward flux       W / m**2
    LWRD   SFC  1  Long-wave radiation downward flux      W / m**2
    LWRU   SFC  1  Long-wave radiation upward flux        W / m**2
    NDRF*  SFC  1  Net downward rad flux at the surface   W / m**2
    RNET*  SFC  1  Net radiative flux at the surface      W / m**2
    FXTT*  SFC  1  Total surface energy budget residual   W / m**2

    P01M   SFC  2  Total precip over 1 hr                 mm (kg/m**2)
    P06M   SFC  -  Total precip over 6 hr                 mm (kg/m**2)
    P12M   SFC  -  Total precip over 12 hr                mm (kg/m**2)
    C01M   SFC  2  Convective precip over 1 hr            mm (kg/m**2)
    S01M*  SFC  2  Stable precip over 1 hr                mm (kg/m**2)
    WTNS#  SFC  2  Sfc moisture availability (fraction)   unitless
    EVAP   SFC  0  Evaporation over 1 hr                  mm (kg/m**2)
    SNFL   SFC  1  Accumulated snowfall over 1 hr         mm (kg/m**2)
    SNRA#  SFC  1  Snow ratio                             %
    SLMM#  SFC  1  Soil moisture                          mm (kg/m**2)
    SWEM#  SFC  2  Snow water equivalent                  mm (kg/m**2)
    N01M   SFC  1  Total snow melt over 1 hr              mm (kg/m**2)
    R01M   SFC  2  Storm sfc runoff over 1 hr             mm (kg/m**2)
    BFGR   SFC  2  Baseflow-groundwater runoff over 1 hr  mm (kg/m**2)
    SLLH*  SFC  1  Surface evaporation over 1 hr          mm (kg/m**2)
    SLLP*  SFC  1  Potential sfc evaporation over 1 hr    mm (kg/m**2)
    SWBL*  SFC  1  Total water budget residual            mm (kg/m**2)

    LWRT   SFC  1  Long-wave radiation at top             W / m**2
    SWRT   SFC  1  Short-wave radiation at top            W / m**2

    WXTS#  SFC  2  Categorical weather type snow          0,1
    WXTP#  SFC  2  Categorical weather type ice pellets   0,1
    WXTZ#  SFC  2  Categorical weather type frzng rain    0,1
    WXTR#  SFC  2  Categorical weather type rain          0,1
    WSYM#* SFC  2  Weather type symbol number             -

    LCLD#  SFC  2  Low-cloud amount                       %
    MCLD#  SFC  2  Middle-cloud amount                    %
    HCLD#  SFC  2  High-cloud amount                      %

    CLPL   SFC  -  Low-level cloud pressure level         mb
    CLPM   SFC  -  Mid-level cloud pressure level         mb
    CLPH   SFC  -  Hgh-level cloud pressure level         mb

    UWND#  SFC  2  10 m u component                       m/s
    VWND#  SFC  2  10 m v component                       m/s
    TH10#  SFC  1  10 m potential temperature             K
    Q10M#  SFC  1  10 m specific humidity                 g/kg

    HLCY#  SFC  2  Estimated storm-relative helicity      m*m/s*s
    USTM#  SFC  2  Estimated storm motion u component     m/s
    VSTM#  SFC  2  Estimated storm motion v component     m/s

    T2MS#  SFC  2  2 m temperature                        C
    TD2M#* SFC  2  Estimated 2 m dewpoint temperature     C
    Q2MS#  SFC  2  2 m specific humidity                  g/kg

    SRLM   SFC  1  Surface roughness length               m
    SEXC#  SFC  1  Surface exchange coefficient           m/s
    VEGF   SFC  1  Green vegetation cover                 %
    CNPW   SFC  1  Canopy water                           mm (kg/m**2)

    SMCx#  SFC  1  Layer x=1,4 volumetric soil moisture   -
    STCx#  SFC  1  Layer x=1,4 soil temperature           K

    SMSK   SFC  2  Land/water mask (0=land, 1=water)      -
    SELV   SFC  2  Surface elevation (model terrain)      m
    SLAT   SFC  2  Station latitude                       degrees
    SLON   SFC  2  Station longitude                      degrees
    STNM   SFC  2  Station number                         -

    * Parameter is derived during data reformatting.
    # Parameter is an instantaneous value.
    ? Parameter is currently undefined (meaningless or missing value).
    @ Parameter derived from first level profile data.


    Column C has the following meaning:

       0 =  Parameter is in class 0 Eta output only
       1 =  Parameter is in class 1 Eta output only
       2 =  Parameter is in both class 0 and class 1 Eta output
       - =  Parameter is not in the Eta output

       Note that any of these parameters may be found in the output
       from other NCEP models.


   		Derived Parameter Definitions

     DTAR  =  DTLW + DTSW

     NDRF  =  SWRD + SWRU + LWRD

     RNET  =  SWRD + SWRU + LWRD + LWRU

     FXTT  =  FXLH + FXSH + FXSS + FXSN + RNET

     S01M  =  P01M - C01M

     SLLH  =  dt * FXLH / HEATVP

     SLLP  =  dt * FXLP / HEATVP

     SWBL  =  - ( SLMM (t) - SLMM (t-dt) )
              - ( SWEM (t) - SWEM (t-dt) )
              + P01M - R01M + SLLH

     WSYM  =  weather symbol number from standard symbol table

     TD2M  = saturation temperature for mixing ratio Q2MS at
             pressure PRES


     HEATVP = 2.834E06 if SWEM (t) > 0. or
     HEATVP = 2.5E06 if SWEM (t) = 0.


    NOTE:

    If UWND and VWND (the 10 m wind) are in the BUFR file, then
    do not request SPED and DRCT in the parameter file for the
    primary surface data set.  These may be requested in the
    auxiliary surface data parameter file.  Only one piece of
    wind data can be accessed from a surface data file.
    --------------------------------------------------------------

 
### Examples
 
1.  Create sounding and surface data files from the BUFR output
file name 950612.bufr containing hourly profiles and surface
data from a 48-h eta model forecast.  This is a standard
class 0 BUFR output file.
    
        SNBUFR = 950612.bufr
        SNOUTF = 950612.snd
        SFOUTF = 950612.sfc
        SNPRMF = snclass0.prm
        SFPRMF = sfclass0.prm
        TIMSTN = 49/500

2.  Create only an ASCII text file from the BUFR data.
    
        SNBUFR = 950612.bufr|pit=72520
        SNOUTF =
        SFOUTF =
        SNPRMF = snclass0.prm
        SFPRMF = sfclass0.prm
        TIMSTN = 49/500

3.  Create only the ASCII parameter list file bufr_table.dump.
    
        SNBUFR = 950612.bufr
        SNOUTF =
        SFOUTF =
        SNPRMF =
        SFPRMF =
        TIMSTN = 49/500


### Error Messages
 
    [NAMSND  +3]    Done -- just wrote out bufr_table.dump.
    [NAMSND  +2]    Creating GEMPAK file ... .
    [NAMSND  +1]    End of input file -- all done.
    [NAMSND  -1]    Fatal error initializing TAE.
    [NAMSND  -2]    Fatal error reading TAE parameters.
    [NAMSND  -3]    Error opening model sounding file ... .
    [NAMSND  -4]    Error opening sounding parameter file.
    [NAMSND  -5]    Wrong number of sounding parms in output file.
    [NAMSND  -6]    Wrong sounding parameters in output file.
    [NAMSND  -7]    Cannot create a new sounding data file.
    [NAMSND  -8]    Error opening surface parameter file.
    [NAMSND  -9]    Wrong number of surface parms in output file.
    [NAMSND -10]    Wrong surface parameters in output file.
    [NAMSND -11]    Cannot create a new surface data file.
    [NAMSND -12]    Error reading input file.
    [NAMSND -13]    Packing file not opened.
    [NAMSND -14]    Different SND parm count in input file.
    [NAMSND -15]    Different SFC parm count in input file.
    [NAMSND -16]    Cannot add SND time.
    [NAMSND -17]    Cannot add SFC time.
    [NAMSND -18]    Cannot add SND station.
    [NAMSND -19]    Cannot add SFC station.
    [NAMSND -20]    Sequence name not found in input file.
    [NAMSND -21]    No input parameter names were requested.
    [NAMSND -22]    Forecast time is missing.
    [NAMSND -23]    Station number is missing.
    [NAMSND -24]    Station latitude is missing.
    [NAMSND -25]    Station longitude is missing.
    [NAMSND -26]    Station elevation is missing.
    [NAMSND -27]    No surface data for a station.
    [NAMSND -28]    No data in output array.
    [NAMSND -29]    Sounding has only one level.
    [NAMSND -30]    Error opening auxiliary parameter file.
    [NAMSND -31]    Wrong number of auxiliary parms in output file.
    [NAMSND -32]    Wrong auxiliary parameters in output file.
    [NAMSND -33]    Cannot create a new auxiliary surface data file.
    [NAMSND -34]    Error assigning unit number to BUFR file.
    [NAMSND -35]    Must use all snd parms if not using one profile.
    [NAMSND -36]    Maximum number of profiles exceeded.
    [NAMSND -41]    Missing packing information---defective BUFR file.
    [NAMSND -42]    Too many TABLE B entries found in BUFR file.
    [NAMSND -43]    A sequence has no member parameters.
    [NAMSND -44]    Table is too long---MXTBLN is too small.
    [NAMSND -45]    No TABLE B entries found.
    [NAMSND -46]    Too many TABLE D entries found.
    [NAMSND -47]    Too many parameters in a sequence.
    [NAMSND -48]    No TABLE D entries found---defective BUFR file.
    [NAMSND -49]    The sequence ... was not found.
    [NAMSND -50]    The parameter ... was not found in table.
    [NAMSND -51]    Cannot open BUFR file ... .
    [NAMSND -52]    BUFR file name is blank.
    [NAMSND -53]    Cannot open bufr_table.dump file.
    [NAMSND -54]    6th column entry in SND packing table is missing.
    [NAMSND -55]    Error opening sounding output file.
    [NAMSND -56]    Error opening surface output file.
    [NAMSND -57]    Error opening surface auxiliary output file.
