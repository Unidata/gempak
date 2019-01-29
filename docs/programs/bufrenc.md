# BUFRENC

BUFRENC processes an ASCII input file to produce one or more
BUFR output files.

### Input Parameters
 
    FXYTBL    FXY table file
    BUFRFIL   BUFR output file
    FHOUR     Forecast hour
    UKAFIL    Intermediate input/output ASCII file
 
### Program Description
 
BUFRENC is a table-driven BUFR encoder program which reads
user input parameters, tables and an ASCII file to produce one
or more BUFR output files. The input parameters include FXY
file name, BUFR output file names, forecast hour and ASCII file.
The preferences table `bufrprefs.tbl` is used to specify applicable
geographic areas for SWM charts, as well as values for some
BUFR section 0, 1 and 3 data entries.

### Examples
 
1. 
        FXYTBL  = fxyswhcld;fxyswhfrt
        BUFRFIL =
        FHOUR   = 24
        UKAFIL  = SIGWXHI.txt

2. 
        FXYTBL  = SWH
        BUFRFIL =
        FHOUR   = 18
        UKAFIL  = SIGWXHI.txt

3.
        FXYTBL  = SWM
        BUFRFIL =
        FHOUR   = 18
        UKAFIL  = SWM.txt


### Error Messages
 
    [BUFRENC  -1]   Invalid input for FXY file names.
    [BUFRENC  -2]   Invalid input for the ASCII file.
    [BUFRENC  -3]   Error opening input ASCII file.
    [BUFRENC  -4]   Error read the header information for input ASCII file.
    [BUFRENC  -5]   Error with the number of BUFR output files.
    [BUFRENC  -6]   Cannot open FXY table.
    [BUFRENC  -7]   Mixing SWH and SWM table names in fxytbl.
    [BUFRENC  -8]   Chart levels do not match requested chart type.
