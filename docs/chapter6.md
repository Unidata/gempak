# CHAPTER 6

## GEMPAK Real-time Decoders

## 6.1 DECODER Common Characteristics

### The GEMPAK real-time decoders are a set of programs that translate meteorological

### ASCII data into data suitable for storage in GEMPAK files. The following decoders

### are currently implemented for use on real-time ASCII data feeds:

```
DCACFT decodes AIREP, PIREP, RECCO and AMDAR reports
DCAIRM decodes airmet reports
DCCSIG decodes convective sigmet and convective outlook reports
DCFFG decodes Flash Flood Guidance data
DCGMOS decodes GFS Model Output Statistics
DCHRCN decodes tropical forecast/advisory reports
DCIDFT decodes sea ice drift bulletins
DCISIG decodes international SIGMET reports
DCLSFC decodes land surface synoptic reports
DCMETR decodes raw SAO and METAR reports
DCMSFC decodes raw buoy, ship, and C-MAN reports
DCNCON decodes non-convective SIGMET reports
DCNMOS decodes NGM Model Output Statistics
DCRDF decodes Regional Digital Forecast (RDF)
DCSCD decodes Supplemental Climatological Data reports
DCSVRL decodes severe local storm reports
DCTAF decodes raw TAF (terminal aerodrome forecast) reports
DCUAIR decodes Upper Air Sounding data
DCWARN decodes flash flood/tornado/severe t-storm warnings
DCWCN decodes watch county notification reports
DCWOU decodes watch outline update reports
DCWSTM decodes winter storm reports
DCWTCH decodes tornado/severe t-storm watch and status reports
DCXMOS decodes eXtended GFS Model Output Statistics data
```

### Additional decoders for the Unidata distribution are available:

```
DCACARS decodes NetCDF ACARS files from FSL
DCGRIB decodes GRIB data
DCGRIB2 decodes GRIB data
DCNCPROF decodes NetCDF format profiler data from FSL
DCNEXR2 uncompresses and stores NEXRAD Level II data
DCNLDN decodes NLDN lightning
DCPROF decodes BUFR format profiler data
DCREANAL Converts NetCDF format reanalysis grid files
DCREDBOOK generates plots of REDBOOK graphics from STDIN
DCSHEF decodes SHEF hydrologic bulletins
DCSTORM decodes WWUS60 storm reports
DCSUOMI decodes SuomiNet GPS files
DCTROP decodes Tropical storm buletins
DCWATCH decodes WWUS40 watch box locations
```
### Each real-time decoder is a stand-alone program that may be activated from the UNIX

### command line, from a shell script, or from another program. The command line options

### are as follows:

```
-v N Set the level of verbosity for the logs
-c curtim Set the "current" time
-b nhours Number of hours to decode prior to "current" time
-d decoder_log Set the decoder log file name
-t time_out Set the interval for the time out
-n Set a flag to NOT save the text data
-h Print the help file, then exit the program
-p prmfil Set the parameter packing table
-s stntbl Set the station table
-a iadstn Set the number of additional stations
-m maxtim Set the maximum number of times
-e PARM=value Set the environment variable PARM to value
```
### The input data stream is fed to each decoder via the standard input. The standard input,

### in turn, may be fed from a file or from a real-time data feed.

### Each decoder writes its output to a GEMPAK data file. For example:

```
dcamos [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the decoded bulletin or report to replace the following characters:

```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```

## 6.2 DCACARS (Unidata)

### DCACARS is a decoder for use with the NetCDF format ACARS data files from

### NOAA/FSL. DCACARS places the individual aircraft reports of altitude, temperature,

### wind speed and direction and relative humidity (RH is reported by a few aircraft) into

### Gempak surface ship format files.

### In order to read the NetCDF file from standard input, a temporary file is created on disk

### consiting of the NetCDF product. Upon completion, the temporary file is removed, or

### if desired, the "-n netcdf_file" option may be used to store the input file in addition to

### the GEMPAK format file.

### The program is controlled by inputs to the command line.

### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dcacars [options] output_file
```
### When decoding existing files, the input file name can be specified using the "-f

### filename" argument.

```
dcacars -f input_file [options] output_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the observations within the NetCDF file to replace the following

### characters.

```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### Command line options specific to DCACARS:

```
-f input_file Read NetCDF file from disk instead of STDIN
-n output_netcdf file Save NetCDF file from STDIN to disk
```
### The following ancillary tables are used:

```
$GEMTBL/pack/acars.pack Packing file
```

## 6.3 DCACFT

### DCACFT decodes raw AIREP, PIREP, RECCO and AMDAR reports from a real-time

### data feed, or from a file fed to the program through standard input, and writes the data

### to a GEMPAK surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcacft [options] output_file
```
### Currently, for real-time operation, each output file represents a single hour, of the form

### YYMMDDHH.acf.

### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcacft -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.4 DCAIRM

### DCAIRM decodes airmet (AIRman's METeorological Information) reports from a

### real-time data feed, or from a file fed to the program through standard input, and writes

### the data to an ASCII file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcairm [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcairm -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

### |Type|Start_time|End_time|Identifier|Update|Flt_lvl_1|

```
Flt_lvl_2|Corr_amd_tst_flag|Cancel_flag
Lat1 Lon
Lat2 Lon
Lat3 Lon
```
..
..
..

### (Note that the first two lines shown above appear as a single line in the output file.)

### Where: Type is IR (instrument flight rules), MO (mountain obscuration), TB

### (turbulence), IC (icing) or

```
SW (sustained winds).
Start_time and End_time are full GEMPAK date/time strings
Identifier is composed of the region and a sequence number
Update is the airmet update number
Flt_lvl_1 and Flt_lvl_2 are the flight levels
Corr_amd_tst_flag is a flag indicating a correction (1),
an amendment (2), and/or a test (flag + 3)
Cancel_flag is a flag indicating a cancellation (0 or 1)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The latitude and longitude values are read using the FORTRAN format (2F9.2). The

### number of points may vary.

## 6.5 DCCSIG

### DCCSIG decodes convective sigmet and convective outlook reports from a real-time

### data feed, or from a file fed to the program through standard input, and writes the data

### to an ASCII file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dccsig [options] output_file
```

### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dccsig -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

### |Type|Start_time|End_time|Sequence ID|Intensity|Direction|

```
Speed|Flt_lvl_1|Flt_lvl_2|Distance|Correction_flag
Lat1 Lon
Lat2 Lon
Lat3 Lon
```
..
..
..

### (Note that the first two lines shown above appear as a single line in the output file.)

### Where: Type is AR (area), LN (line), IS (isolated area), CS ( nil convective sigmet ),

### OL (outlook)

```
Start_time and End_time are full GEMPAK date/time strings
Sequence ID is composed of the sequence number and a region
Intensity is DMSHG (weakening), DSIPTG (ending)
INTSFYG (strengthening), DVLPG (beginning/growing)
Direction is in tens of degrees
Speed is in knots
Flt_lvl_1 and Flt_lvl_2 are the flight levels
Distance is the Distance or Area diameter of the area or line
Correction_flag is a flag indicating a cancellation (0 or 1)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The latitude and longitude values are read using the FORTRAN format (2F9.2). The

### number of points may vary.

### Nil convective sigmets and convective outlook reports are decoded and are indicated in

### the decoded files by assigning the region id as: '0E', '0C' or '0W' for both convective

### sigmets and convective outlooks. Only convective sigmets use 'CS' as the type

### identifier to further indicate a nil issuance for a region. Also, there is no latitude or

### longitude information following the decoded nil issuance.


## 6.6 DCFFG

### DCFFG decodes flash flood guidance data from a real-time data feed, or from a file fed

### to the program through standard input, and writes the data to a GEMPAK surface file.

### The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcffg [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcffg -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.7 DCGMOS

### DCGMOS decodes Global Forecast System (GFS) Model Output Statistics data from

### a real-time data feed, or from a file fed to the program through standard input, and

### writes the data to a GEMPAK surface file. The program is controlled by inputs to the

### command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcgmos [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcgmos -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.8 DCGRIB (Unidata)

### DCGRIB decodes GRIB format model grids from a real-time data feed, or from a file

### fed to the program through standard input, and writes the data to a GEMPAK grid file.

### The program is controlled by inputs to the command line.

### The inputs are program options, the PACK keyword if packing is to be used and the

### output file name or template. For example, for real-time data feeds:

```
dcgrib [options] PACK output_file
```
### When using with the LDM, the GEMPAK grib routines must be able to access the grib

### tables located in the gempak distribution. This is done through the GEMTBL

### environmental variable. If the the LDM process does not have the GEMTBL variable

### set, then you must use the -g "path" option.

### If running the program interactively with standard input, the input file must be also be

### specified. For example:

```
dcgrib -c YYMMDD/HHNN [options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.9 DCGRIB2 (Unidata)

### DCGRIB2 decodes GRIB format grids from a real-time data feed, or from a file fed to

### the program through standard input, and writes the data to GEMPAK grid files. The

### program is controlled by inputs to the command line as well as several configuration

### tables.


### DCGRIB2 utilizes the GEMPAK GB library routines (as NAGRIB does) although

### several distinct features have been added to facilitate the use of the decoder with GRIB

### bulletins transmitted via NOAAPORT/FOS.

1. DCGRIB2 posses the capability to "stitch" together grids
    transmitted in tiles. Currently these grids are typically
    global grids, such as the "thinned" AVN, MRF, and ECMWF
    tiles. Since GRIB bulletins transmitted as tiles generally
    lack the projection information of the larger grid system
    of which the tile is imbedded, a lookup table
    $GEMTBL/grid/dcgrib.tbl is maintained with this information.
2. Ouput file name templates are supported using keys described below.
3. If an output filename is not provided, the program will attempt
    to find a suitable template from the table $GEMTBL/grid/gribkey.tbl.
    By using this feature, a single dcgrib2 process may be invoked for
    many different sources of GRIB data (typically for real-time data).
    If a matching set of criteria exist, the grid is decoded. The first
    template that matches the search criteria will be used, such that
    global matches can be permitted. The number of grid entries in the
    output file can be set in the gribkey.tbl file.
4. Up to MMFILE files will be opened at a time to allow for cycling of output
    files when different data sets are inter-mixed in the data stream.

### The command line inputs are program options, and the output_file name may use the

### defined template characters below. If the output_file name is ommited, a template in

### the gribkey.tbl file will be used if defined.

### For example, for real-time data feeds:

```
dcgrib2 [options] [output_file]
```
### When using with the LDM, the GEMPAK grib routines must be able to access the grib

### tables located in the gempak distribution. This is done through the GEMTBL

### environmental variable. If the the LDM process does not have the GEMTBL variable

### set, then you must use the "-e GEMTBL=path" option.

### The following tables files are expected to be found:

```
$GEMTBL/grid/cntrgrib[X].tbl
$GEMTBL/grid/vcrdgrib[X].tbl
$GEMTBL/grid/wmogrib[X].tbl
$GEMTBL/grid/[CENTER]grib[X].tbl
$GEMTBL/grid/dcgrib.tbl
$GEMTBL/grid/gribkey.tbl
```
### If running the program interactively with standard input, the input file must be also be

### specified. For example:


### dcgrib2 [options] [output_file] < input_file

### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
FF Forecast hour (2 digit)
FFF Forecast hour (3 digit)
### Generating process model id (PDS Octet 5)
@@@ Grid number (PDS Octet 7)
%%% Generating subcenter (PDS Octet 26)
```
### Valid command line options for DCGRIB2 are:

```
-v N, -m maxgrids, -d logfile, -t timeout, -h show help, -e PARM=value
```
### Other DC decoder options are ignored.

## 6.10 DCHRCN

### DCHRCN decodes forecast/advisory reports for tropical depressions, tropical storms

### and hurricanes for the Atlantic and Pacific Oceans from a real-time data feed, or from

### a file fed to the program through standard input, and writes the data to an ASCII file.

### The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dchrcn [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dchrcn -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file (with the 2 '|' lines given below appearing as one

### line in the file) is as follows:

### |Type|Valid_time|Name|Advisory_Number|Position_accuracy

```
|Direction|Speed|Minimum_Central_Press|Corr_flag
Lat Lon Type
Quadrant data
```
### Where: Type is TD (tropical depression), TS (tropical storm), HU (hurricane) or HUT

### (typhoon). If type is not 'HUT',

```
an 'E' or 'S' is appended for an extratropical or
subtropical storm, respectively
Valid_time is the full GEMPAK date/time string
Name is the storm name
Advisory_number is the number assigned to the storm
Position_accuracy is how close the center of the eye
is to the given latitude and longitude
Direction is the direction that the storm is moving toward
Speed is the speed of the storm in knots
Minimum_Central_Press is the pressure value in mb in the eye
at the valid time
Corr_flag is a flag indicating a correction (0 or 1)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The current and forecast latitude and longitude values and storm type are read using the

### FORTRAN format (2F9.2,4X,A).

### Quadrant data (current 100 kt or 64 kt, 50 kt, and 34 kt winds and 12 ft seas, and forecast

### 100 kt or 64 kt, 50 kt and 34 kt winds) are read using the FORTRAN format (4X,A).

### Prior to June 1, 2004, reports from the Joint Typhoon Warning Center (JTWC) included

### 100 kt winds instead of 64 kt winds. As of June 1, 2004, the JTWC reports 64 kt winds,

### as do the TPC and CPHC. In the decoded quadrant sections, the first wind position,

### originally labelled as '64', is now labelled as 'MW', the maximum wind, where 'MW'

### represents either the 64 kt winds from any report or the 100 kt winds from Western

### Pacific (JTWC) reports issued before June 1, 2004. If the storm type is 'HUT' and the

### date is prior to June 1, 2004, the first quadrant data is assumed to be the 100 kt wind

### information.


## 6.11 DCISIG

### DCISIG decodes international SIGMET reports from a real-time data feed, or from a

### file fed to the program through standard input, and writes the data to an ASCII file. The

### program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcisig [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcisig -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

### |Type|Start_tm|End_tm|Msg_id|Orig_idFlt_lvl|Dir|Spd|Name_Loc|Corr

```
Lat1 Lon
Lat2 Lon
Lat3 Lon
Lat4 Lon
..
..
..
```

### Where: Type is TS (thunderstorm), TB (turbulence), HU (hurricane), TR (tropical

### storm), TD (tropical depression),

```
VA (volcanic ash cloud), MW (marked mountain waves),
TC (tropical cyclone), SQ (squall line), CT (CAT),
IC (icing), GR (hail), DS (duststorm), SS (sandstorm),
CB (cumulonimbus), WS (low level wind shear, or CN (cancel)
Start_tm and End_tm are full GEMPAK date/time strings
Msg_id is the message identification and sequence number
Ori_id is the originating station for the message
Flt_lvl is the flight level (or flight level range,
expressed as nnn-nnn) in hundreds of feet
Dir is the direction of movement of the phenomenon
Spd is the speed of the phenomenon in knots
Name_Loc is the name of the storm, where applicable, or
location of the volcano, where applicable, or the
word, OTHER for reports not from CONUS, Hawaii, Guam,
Japan, UK, Tahiti, and Cuba
Corr is a flag indicating a correction (0 or 1)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The latitude and longitude values describing the bounds of the phenomenon are read

### using the FORTRAN format (2F9.2). The number of points may vary. For a

### phenomenon at a point (e.g., a hurricane), only that point is specified. For a

### phenomenon centered at a point (e.g., a thunderstorm), the first point is the center and

### the second point gives the radius (in nautical miles), followed by RMISSD.

## 6.12 DCLSFC

### DCLSFC decodes land surface synoptic reports from a real-time data feed, or from a

### file fed to the program through standard input, and writes the data to a GEMPAK

### surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dclsfc [options] output_file
```
### Currently, for real-time operation, each output file represents a single day, of the form

### YYMMDD.syn.

### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dclsfc -c YYMMDD/HHNN [other_options] output_file < input_file
```

### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.13 DCMETR

### DCMETR decodes raw SAO and METAR reports from a real-time data feed, or from

### a file fed to the program through standard input, and writes the data to a GEMPAK

### surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcmetr [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcmetr -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY or YYYY Year
MM Month number
DD Day
HH Hour
NN Minute
```
### To decode off-time reports, setting the -m option to 72 will decode up to two off-time

### reports in addition to the hourly report.

## 6.14 DCMSFC

### DCMSFC decodes raw buoy, ship, C-MAN, and Coast Guard reports from a real-time

### data feed, or from a file fed to the program through standard input, and writes the data

### to a GEMPAK surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:


```
dcmsfc [options] output_file
```
### Currently, the decoder can be run in two different modes. If the -a option is not used,

### the decoder will create hourly files of all marine surface data (ship, C-MAN, fixed and

### drifting buoys, Coast Guard). If the option "-a 6" is specified, 6-hour files of ship data

### only will be created. For real-time operation, output files have the form

### YYYYMMDDHH.ship, and are placed in different directories depending on whether

### they are hourly or 6-hour files.

### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcmsfc -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.15 DCNCON

### DCNCON decodes non-convective SIGMET reports from a real-time data feed, or

### from a file fed to the program through standard input, and writes the data to an ASCII

### file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcncon [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcncon -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

### |Type|Start_tm|End_tm|Msg_id|Flt_lvl_1|Flt_lvl_2|Corr_amd_tst

```
Lat1 Lon
Lat2 Lon
Lat3 Lon
Lat4 Lon
..
..
..
```
### Where: Type is IC (icing), TB (turbulence), VA (volcanic ash), DU (duststorm or

### sandstorm), or CN (cancel)

```
Start_tm and End_tm are full GEMPAK date/time strings
Msg_id is the message identification and sequence number
Flt_lvl_1 is the lower flight level in hundreds of feet
Flt_lvl_2 is the upper flight level in hundreds of feet
Corr_amd_tst is a flag indicating a correction (1), an
amendment (2), and/or a test (flag + 3)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The latitude and longitude values describing the bounds of the phenomenon are read

### using the FORTRAN format (2F9.2). The number of points may vary.

## 6.16 DCNCPROF (Unidata)

### DCNCPROF decodes NetCDF format profiler reports provided by NOAA/FSL from a

### real-time data feed through standard input, or a NetCDF file on disk, and writes the data

### to a GEMPAK merged upperair file. FSL profiler data is provided in 6 minute

### observations as well as hourly summaries (currently available on the Unidata IDD

### FSL2 feed). In order to read the NetCDF file from standard input, a temporary file is

### created on disk consiting of the NetCDF product. Upon completion, the temporary file

### is removed, or if desired, the "-n netcdf_file" option may be used to store the input file

### in addition to the GEMPAK format file.

### The program is controlled by inputs to the command line.


### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dcncprof [options] output_file
```
### When decoding existing files, the input file name can be specified using the "-f

### filename" argument.

### dcprof -f input_file [options] output_file

### A template may be used to specify the output file name. The file name template uses

### the date and time of the observations within the NetCDF file to replace the following

### characters.

```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### Command line options specific to DCNCPROF:

-f input_file Read NetCDF file from disk instead of
STDIN
-n output_netcdf file Save NetCDF file from STDIN to disk

### The following ancillary tables are used:

```
$GEMTBL/pack/profiler_fsl.pack Packing file
$GEMTBL/stns/profiler_fsl.stn Station table
```
## 6.17 DCNEXR2 (Unidata)

### DCNEXR2 is used to receive the Level II NEXRAD data from the CRAFT IDD data

### stream. Products are received in BZIP2 compressed pieces from a file or data stream

### fed to the program through standard input, and appended to an output file for use with

### display programs. The program is controlled by inputs to the command line.

### The inputs are program options, and the output file name. For example:

```
dcnexr2 [options] output_file
```
### By default, the program initially prepends a "." to the output file name, and renames the

### file upon closing in order to make it easier for programs to determine when a file is

### complete. Subsequent writes to an existing file bypass adding the leading "." to the

### filename.


### The only useful option to the program is "-s STID" which allows the user to specify a

### 4 character station ID to be written in to the output data file in bytes 21-24 of the

### Archive2 Level II data header, which allows programs to identify the source of the

### radar data without having to rely on a file name convention.

## 6.18 DCNLDN (Unidata)

### DCNLDN decodes NLDN lightning data reports from a real-time data feed, or from a

### file fed to the program through standard input, and writes the data to a GEMPAK ship

### format file. The program is controlled by inputs to the command line.

### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dcnldn [options] output_file
```
### For archived data dcnldn [options] output_file < input_file

### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
@@ File Sequence Number
```
### Because the number of lightning strikes that may occur in a single hour can be very

### large, it is possible that the number of strikes will exceed the size of the output file. By

### default, when the size of the output file is exceeded, a new file is created and a sequence

### number _## is appended to the file name, where ## is 01 for the first additional file

### created, 02 for the second, etc.

### The user may use the @@ template option to tailor the sequence numbering to suite

### local tastes. When using @@, the initial file will be numbered 00.

### The default file size that will be created by dcnldn is 25,000. Typical summer

### conditions may exceed 25,000 a few hours during the day. The -m option may be used

### to create smaller files, which will be more space efficient during less active periods.

### By default, strikes are assigned hourly DATTIM stamps. The user may specify data to

### be binned in NN minute bins by specifying the storage method using the "-s minuteNN"

### option. For example minute06 will generate DATTIMs with YYMMDD/HH00,

### YYMMDD/HH06, YYMMDD/HH12, ... times.


## 6.19 DCNMOS

### DCNMOS decodes NGM Model Output Statistics data from a a real-time data feed, or

### from a file fed to the program through standard input, and writes the data to a GEMPAK

### surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcnmos [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcnmos -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.20 DCPROF (Unidata)

### DCPROF decodes BUFR format profiler reports from a real-time data feed, or from a

### file fed to the program through standard input, and writes the data to a GEMPAK

### merged upperair file. The program is controlled by inputs to the command line.

### The inputs are program options, the two BUFR table files and the output file name or

### template. For example, for real-time data feeds:

```
dcprof [options] bufrb_table bufrd_table output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcprof -c YYMMDD/HHNN [options] bufrb_table bufrd_table output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.21 DCRDF

### DCRDF decodes Regional Digital Forecast (RDF) reports from a real-time data feed,

### or from a file fed to the program through standard input, and writes the data to a

### GEMPAK surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcrdf [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcrdf -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.22 DCREDBOOK (Unidata)

### DCREDBOOK creates displays of Redbook graphic format products from a real-time

### data feed, or from a file fed to the program through standard input. The program is

### controlled by inputs to the command line. Output is created using the supplied

### GEMPAK device which accepts the standard parameters for each type of device driver.

### Program usage is:

```
dcredbook [options] device
```
### The program produces products, just as can be done with GPMAP, though the input file

### is designed to be read from STDIN. Additionally, the program will use the $GEMTBL/


### nafos/redbook.tbl file to supply an optional product name and graphics area for the final

### output. Since products transmitted using the WMO identifier do not provide a

### descriptive product name, the use of the redbook.tbl file is useful for creating automated

### product generation actions.

### Three version of the program are provided: dcredbook, dcredbook_ps, and

### dcredbook_gf. The first instance uses the standard GPLT interface which allows any

### of the available device drivers to be selected. The second and third instances are linked

### directly to the GEMPAK postscript and gif drivers respectively to eliminate the need

### for creating the separate GPLT interface.

### For example:

```
dcredbook 'gf|%P-YYYYMMDD_HHNN.gif' < redbook_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the graphic file to replace the following characters.

```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
%P Product name as given in $GEMTBL/nafos/redbook.tbl
```
## 6.23 DCSCD

### DCSCD decodes Supplemental Climatological Data reports from a real-time data feed,

### or from a file fed to the program through standard input, and writes the data to a

### GEMPAK surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcscd [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcscd -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.24 DCSHEF VERSION 2

### The GEMPAK DCSHEF program decodes raw SHEF reports from a real-time data

### feed (via an LDM), or from a file containing raw SHEF reports. The data is written to

### a GEMPAK surface file.

### The decoder is available in the file dcshef.tar.Z and includes:

### exe/hpux/dcshef DCSHEF decoder exe/linux/dcshef Available only in

### dcshef.linux.tar.Z gempak/help/dcshef.hlp Help file gempak/tables/... pack/shef.pack

### Sample SHEF parameter packing file pack/SHEFparms.txt Text listing of all valid

### SHEF parms stns/shefstns.tbl.master Master SHEF station table stns/shefunits.tbl Units

### conversion table README/dcshef.README This file. README/dcshef.FAQ FAQ

### to answer common questions about

```
decoding and viewing SHEF data.
```
### Below, you will find a description of how the SHEF decoder works, and instructions on

### how to install DCSHEF. If you do not care how the decoder works, skip ahead to the

### installation steps.

### I. How DCSHEF decodes SHEF Reports ----------------------------------

### General:

### The DCSHEF decoder will decode .A, .B, and .E SHEF reports. Data from different

### report types are stored together in the same output GEMPAK surface file. DCSHEF

### does not distinguish between data decoded from .A, .B, or .E reports.

### When you configure DCSHEF to decode data, you have complete control over which

### stations are decoded, and what parameters are decoded. You can run multiple copies

### of DCSHEF, decoding different sets of parameters for different stations, as long as each

### configuration of DCSHEF has its own GEMPAK output file. You may not write to the

### same GEMPAK output file from separate instances of DCSHEF. See the installation

### steps below for more details.

### Time:


### All times in SHEF reports are converted to and stored in UTC time in the GEMPAK

### output file. Often, SHEF data is reported in local time. But, since there is no way to

### indicate the time zone in a GEMPAK data file, all times are converted to UTC. The

### only exception is the special time of 2400. In SHEF reports, 2400 is used to indicate

### the end of a given time period. For example, it may mean the end of a day or the end

### of a month. When DCSHEF encounters the time 2400, the time is not converted to

### UTC time. Instead, the date time string YYMMDD/2400 is assigned to the data.

### For .A & .B reports, the report time is rounded to the hour using the standard GEMPAK

### rounding rule. That is, when the report time is greater than or equal to 45 minutes after

### the hour, the valid time is rounded to the next hour. When the report time is less than

### 45 minutes after the hour, the valid time is rounded down to the current hour. The

### actual report time is always available in the special GEMPAK parameter STIM. STIM

### can be accessed like any other data parameter, even though it is not listed in the

### parameter packing file.

### For .E reports, the report time is rounded up to the nearest 15 minute interval. Thus,

### valid times will be HH00, HH15, HH30, or HH45. For .E reports with increments less

### than 15 minutes, data is stored only when the valid time is unique. Thus, for a report

### containing data every 5 minutes, only the data at HH00, HH05, HH20, HH35, and

### HH50 are stored at HH00, HH15, HH30, HH45 and (HH+1)00, respectively. Again,

### the actual report time for each stored report is always available in the STIM variable.

### Parameters:

### DCSHEF will decode any valid SHEF parameter. SHEF parameters can be up to 7

### characters long, although usually they are only 2 or 3 characters long. The file

### $GEMTBL/pack/SHEFparms.txt contains text versions of Tables 1-4 from the SHEF

### Users Guide. (http://hsp.nws.noaa.gov/hrl/shef/contents.htm). These tables list the

### available codes used to create the parameters names. You can also look at some raw

### SHEF reports to see examples of the parameter names.

### DCSHEF will allow you to decode any of these parameters simply by listing the

### parameter name in a GEMPAK parameter packing file. For best results, you should use

### 4 characters to define a parameter in the GEMPAK parameter packing file. If you use

### fewer than 4 characters, DCSHEF will expand the parameter name to 4 characters using

### the default values for character 3 and 4 ("I" and "R", respectively). DCSHEF will

### expand the parameter names both in the packing table and in the data, so that all the

### appropriate data is decoded. However, if you use fewer than 4 characters, you will not

### be able to use any of the mathematical functions on these data. Therefore, I suggest that

### you expand all 2 and 3 character parameter names to four characters by using the

### default values of "I" and "R" for characters 3 and 4, respectively.

### In the SOO distribution of NAWIPS/DCSHEF, up to 40 parameters may be listed in a

### single parameter packing file. If you want to decode more than 40 SHEF parameters,

### you will have to create a second packing table, and run two copies of the DCSHEF


### decoder from two entries in the LDM pattern action file. Note that each packing table

### requires it's own output GEMPAK data file, so you will have to store the data in

### separate GEMPAK files. Here's an example: In shef.pack, I list some standard

### parameters like temps, press, precip, and wind data. I decode this data into the files

### YYMMDD_shef.gem. In addition, I create a second packing table called

### shefriver.pack where I list river information such as stage heights, gate openings, fish

### counts, water quality, and discharge information. I decode this data into the files

### YYMMDD_river.gem.

### You can change the parameters to decode at any time, by changing the parameter

### packing file. However, note that changes only take affect when the GEMPAK data file

### is created. That is usually at the start of a new day.

### Finally, note that SHEF parameter names can use up to 7 characters. DCSHEF nor

### GEMPAK can distinguish between the SHEF parameter names that differ in the 5th,

### 6th, or 7th character. Thus, if a single report contains data for two different parameters

### whose names differ only in characters 5-7, DCSHEF will decode only one of those data.

### Usually, this situation does not occur.

### Stations:

### There are 10s of thousands of stations in the US reporting data in SHEF. The SOO

### distribution of NAWIPS & DCSHEF will only support up to 4700 stations per

### GEMPAK data file. Therefore, you will have to choose your favorite 4700 (or fewer)

### stations to decode.

### The file $GEMTBL/stns/shefstns.tbl.master contains a list of stations (ordered by state)

### which report precipitation data. This is the most complete list of stations I have been

### able to find. You can start by selecting the stations of interest to you from this master

### station list. Note, however that the shefstns.tbl.master file may not be complete, and

### you may need to supplement this table with stations of your own.

### If you must decode SHEF reports from more than 4700 stations, you will have to create

### two station tables, and run two copies of the DCSHEF decoder from two entries in the

### LDM pattern action file. Again, you must use separate file names for each copy of

### DCSHEF. For example, I may put all the stations reporting SHEF data from Colorado

### in shefstnsco.tbl and decode that data into YYMMDD_shefco.gem. And, I may put all

### the stations reporting SHEF data from Wyoming in shefstnswy.tbl and decode the data

### into YYMMDD_shefwy.gem.

### Finally, there are some HADS stations that are not in the shefstns.tbl.master file. You

### may want to check the HADS list at http://hsp.nws.noaa.gov/oh/hads/hsas/hsas.htm,

### and add any stations of interest to your shefstns.tbl file. Simply follow the existing

### format shown in shefstns.tbl.master.

### Units:


### SHEF data is stored in the output GEMPAK data file in English units. These units are

### the default for SHEF data, and are listed with the parameter names in Table 1 of the

### SHEF Users Guide and in the file $GEMTBL/pack/SHEFparms.txt. If the units

### function "DUS" is used in a SHEF report, that means the data values are reported in S.I.

### units. DCSHEF will then convert the data values from S.I. units into English units and

### store them in the output GEMPAK data file.

### Data:

### Once the SHEF data is decoded into an output GEMPAK data file, you can view the

### data using any surface data plotting program. You can create maps using SFMAP. You

### can create meteograms using SFGRAM. You can even do an objective analysis on the

### data using OABSFC to create gridded data. (Be sure that the available SHEF data

### provides enough spatial coverage so that the objective analysis is valid.) You can list

### the SHEF data using the SFLIST program.

### SHEF data can also be viewed in GARP. To configure GARP to view SHEF data, you

### must add a few keys to the $NAWIPS/garp/config/Garp_defaults file. For example:

### sfc_keys : "surf,shb,shef" sfc_labels : "Hourly/METAR,Ship/Buoy,SHEF" sfc_tables :

### "sfcparms.lst,shbparms.lst,shefparms.lst" shef_dir : $METDAT/gempak/shef

### shef_parms: ";TAIR;TADR;TNIR;TXIR;PPPR;PPQR;STIM"

### Your "shef_parms" list will contain those data you routinely want plotted in GARP.

### You should put your complete list of SHEF parameters (from your shef.pack file) in a

### $NAWIPS/garp/tables/shefparms.lst file. (You can use the $NAWIPS/garp/tables/

### sfcparms.lst file as a sample.)

### Performance:

### DCSHEF is a standard GEMPAK decoder. It does quite a bit of string manipulation,

### and like other GEMPAK decoders, can be a little slow at times. In the case of METAR

### or upper air decoding, this is not a big deal. However, because of the overwhelming

### volume of data in SHEF, this can become an issue.

### For most SOOs, who only receive reports from a few states, the performance of

### DCSHEF will be fine. However, if your office receives SHEF data from the entire

### country, you may run into slow decoding. For example, it took over 20 minutes to

### decode all the 12Z SHEF data on the NWS DD+ data feed. (The DD+ data feed

### includes most of the reports from the entire US.)

### If you see slow decoding of the SHEF data, you might consider restricting the amount

### of data sent to DCSHEF by modifying the pattern in your pqact.conf file. For example,

### you might change the (...)(RR.) pattern to (NMC|SEA)RR(M|A|S).


### Command line options:

### DCSHEF uses a number of command line options. They are:

### -v N Set the verbosity level to "N". As N goes up, more verbose

```
logging occurs. The default is "0". Level 1 will report
stations not decoded. Level 2 will report stations &
parameters not decoded. Levels 3, 4, & 5 will include
Level 1 & 2 messages, along with additional debugging
messages.
```
### -c curtim "curtim" is the pseudo-current time. This is a complete

```
GEMPAK format date/time (YYMMDD/HHMM) and is used as the
system time when decoding archive data. The default value
for this variable is the system time.
```
### -b nhours "nhours" is the number of hours prior to the current time.

```
Only reports within this time period are decoded into
the data file. The default value for this variable is
24 hours.
```
### -d decoder_log "decoder_log" is the name and path of the decoder

```
log file. The default value is dcshef.log
```
### -t time_out "time_out" is the time-out value in seconds for the

```
decoder. If no data is received within the specified
time limit, the program will exit. The default time-
out value is 600 seconds.
```
### -h Print the usage statement and exit

### -p prmfil Set the parameter packing table to "prmfil". The default

```
value is $GEMTBL/pack/shef.pack.
```
### -s stntbl Set the station table to "stntbl". The default value

```
is $GEMTBL/stns/shefstns.tbl.
```
### -a iadstn Define the maximum number of additional stations (beyond

```
those in the station table specified in "-s stntbl") that
can be added to the data file as "iadstn". The default
value is 0.
```
### -m maxtim Set the maximum number of times in a single data file

```
to "maxtim". The default value is 100.
```
### -g gemtbldir Set the path to the GEMPAK tables ($GEMTBL). This

```
is required when running the decoder from the LDM.
This definition can also be used for the parameter
table and station table entries.
```

### Limitations:

### DCSHEF will decode most SHEF reports. There are a few exceptions:

### 1) .E reports with time increments of less than 15 minutes will only decode those data

### values at unique valid times. (See the "Times" section for more details.)

### 2) In the same report, parameter names that differ only in the 5-7th character are not

### distinguishable.

### 3) More than one date override function in the data or header section of .B reports can

### not be handled.

### 4) Data qualifier (DQ groups), duration information (DV groups), and creation date

### information (DC groups) are not decoded.

### Log: PB/NWS 7/29/98 PB/NWS 8/20/98 Bug fix for reports with comments at end of

### line. PB/NWS 9/02/98 VERSION 2: Added code to append data to existing

```
reports in GEMPAK data file.
```
### PB/NWS 11/9/98 VERSION 2.1: Fixed bug in B reports with leading

```
slash
```
### II. Usage Guide ----------------------

### 1) Set up the list of parameters to decode in the parameter packing file. A sample

### packing file is included in the distribution in the file $GEMTBL/pack/shef.pack. You

### will find the complete list of valid SHEF parameters in the file $GEMTBL/pack/

### SHEFparm.txt, or in the on-line version of the SHEF Users Guide: http://

### hsp.nws.noaa.gov/hrl/shef/contents.htm See the "Parameters" section above for more

### information.

### 2) Set up a list of stations to decode in the station table file. A master list of stations is

### included in the distribution in the file $GEMTBL/stns/shefstns.tbl.master. You can

### copy up to 4700 stations into your station table. (default name $GEMTBL/stns/

### shefstns.tbl) See the "Stations" section above for more information.

### 3) Configure the pattern action file, pqact.conf, of your LDM to run DCSHEF upon

### receipt of a SHEF report. Here is an example entry (watch the TABS):

### AFOS ^(...)RR.(..)

```
PIPE dcshef -v 0
-d /metdat/gempak/shef/dcshef.log
-g /usr1/nawips/gempak/tables
-p shef.pack
-s shefstns.tbl
/metdat/gempak/shef/YYMMDD_shef.gem
```

## 6.25 DCSTORM (Unidata)

### DCSTORM decodes Severe Storm reports from the Storm Prediction Center (SPC)

### from a real-time data feed, or from a file fed to the program through standard input, and

### writes the data to a GEMPAK ship format file. Bulletins decoded are hourly NWUS22

### KWNS (formerly WWUS60 KMKC) products.

### The program is controlled by inputs to the command line.

### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dcstorm [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcstorm -c YYMMDD/HHNN [options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### User Controled Input Default

#### -------------------- -------

```
Parameter file sels.pack
Station table none
Maxstns 9999
```
## 6.26 DCSUOMI (Unidata)

### DCSUOMI is a decoder for use with the NetCDF format SUOMINET data files from

### Unavco/Unidata. DCSUOMI places the individual GPS & Metstation reports into

### Gempak surface format files.

### In order to read the NetCDF file from standard input, a temporary file is created on disk

### consiting of the NetCDF product. Upon completion, the temporary file is removed, or

### if desired, the "-n netcdf_file" option may be used to store the input file in addition to

### the GEMPAK format file.


### The following list provides the surface file naming of decoded SUOMINET

### parameters:

### NetCDF name GEMPAK name pwv PWVM pwv_err PWVE wet_delay DELW

### model_dry_delay DELD total_delay DELT final_dry_delay DELF pifact PIFC pres

### PRES temperature TMPC rh RELH met_flag MFLG

### The program is controlled by inputs to the command line.

### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dcacars [options] output_file
```
### When decoding existing files, the input file name can be specified using the "-f

### filename" argument.

```
dcsuomi -f input_file [options] output_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the observations within the NetCDF file to replace the following

### characters.

```
YYYY Year with century
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### Command line options specific to DCSUOMI:

```
-f input_file Read NetCDF file from disk instead of STDIN
-n output_netcdf file Save NetCDF file from STDIN to disk
```
### The following ancillary tables are used:

```
$GEMTBL/pack/suomi.pack Packing file
```
## 6.27 DCSVRL

### DCSVRL decodes severe local storm reports (tornado and severe thunderstorm watch

### reports) from a real-time data feed, or from a file fed to the program through standard

### input, and writes the data to an ASCII file. The program is controlled by inputs to the

### command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:


```
dcsvrl [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcsvrl -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

```
|Type|Start_time|End_time|Watch_number|Corr_flag
County_station_table_information_1
County_station_table_information_2
...
County_station_table_information_N
```
### Where: Type is TS (severe thunderstorm) or TN (tornado)

```
Start_time and End_time are full GEMPAK date/time strings
Watch_number is the watch number
Corr_flag is a flag indicating a correction (0 or 1)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The County_station_table_information is read using the FORTRAN format

### (A8,1X,I6,1X,A32,1X,A2,1X,A2,1X,F9.2,1X,F9.2,1X,F9.2,1X,I2,1X,A20). The

### county information is the same as in the GEMPAK station tables.

## 6.28 DCTAF

### DCTAF decodes raw TAF (terminal aerodrome forecast) reports from a real-time data

### feed, or from a file fed to the program through standard input, and writes the data to a

### GEMPAK surface file. The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dctaf [options] output_file
```

### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcatf -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY or YYYY Year
MM Month number
DD Day
HH Hour
NN Minute
```
## 6.29 DCTROP (Unidata)

### DCTROP decodes Hurricane/Tropical storm reports reports from a real-time data feed,

### or from a file fed to the program through standard input, and writes the data to a

### GEMPAK ship format file. Bulletins decoded are: WTNT4[1-5] (atlantic), WTPZ4[1-

### 5] (east pacific), WTPA4[1-5] (central pacific), and WTPN3[1-5] (west pacific).

### The program is controlled by inputs to the command line.

### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dctrop [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dctrop -c YYMMDD/HHNN [options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
@@ Insert the Storm Name/number
```

### User Controled Input Default

#### -------------------- -------

```
Parameter file tropic.pack
Station table none
Maxstns 3999
```
## 6.30 DCUAIR

### DCUAIR decodes upper air sounding data from a real-time data feed, or from a file fed

### to the program through standard input, and writes the data to a GEMPAK sounding file.

### The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcuair [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcuair -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY or YYYY Year
MM Month number
DD Day
HH Hour
NN Minute
```
### The dcuair decoder may be used to decode dropsonde data. If this is done, it is

### advisable to run a separate instance of the decoder from that used to decode standard

### upper air data. In this case,

```
a) the maximum number of stations to add should be set to
at least 50 on the command line ( -a 50 ), since all
dropsonde reports are treated as additional stations
b) the maximum number of times should be set to 24 on the
command line ( -m 24 ), to create hourly entries instead of
3-hourly entries
c) only upper air bulletins having WMO bulletin headers
beginning with UZ should be sent to the decoder
```

## 6.31 DCWARN

### DCWARN decodes flash flood, tornado and severe thunderstorm warning reports from

### a real-time data feed, or from a file fed to the program through standard input, and

### writes the data to an ASCII file. The program is controlled by inputs to the command

### line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcwarn [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcwarn -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

```
|Type|Start_time|End_time|Orig_station|Corr_flag
County_station_table_information_1
County_station_table_information_2
...
County_station_table_information_N
```
### Where: Type is SVR (thunderstorm), TOR (tornado) or FFW (flash flood)

```
Start_time and End_time are full GEMPAK date/time strings
Orig_station is the WFO that issued the warning
Corr_flag is a flag indicating a correction (0 or 1)
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The County_station_table_information is read using the FORTRAN format

### (A8,1X,I6,1X,A32,1X,A2,1X,A2,1X,F9.2,1X,F9.2,1X,F9.2,1X,I2,1X,A20). The

### county information is the same as in the GEMPAK station tables.


## 6.32 DCWATCH (Unidata)

### DCWATCH decodes WWUS40 format Severe Thunderstorm and Tornado watch box

### bulletins from a real-time data feed, or from a file fed to the program through standard

### input, and writes the data to a GEMPAK ship format file. The program is controlled by

### inputs to the command line.

### The inputs are program options, and the output file name or template. For example, for

### real-time data feeds:

```
dcwatch [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must be also be specified. For example:

```
dcwatch -c YYMMDD/HHNN [options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YY Year without the century
MM Month number
DD Day
HH Hour
NN Minute
```
```
User Controled parameters Suggested Default:
------------------------- -----------------
Parameter file watch.pack
Station table sfworld.tbl
Maxtim 1500
```
## 6.33 DCWCN

### DCWCN decodes watch county notification reports from a real-time data feed, or from

### a file fed to the program through standard input, and writes the data to an ASCII file.

### The program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
DCWCN [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
DCWCN -c YYMMDD/HHNN [other_options] output_file < input_file
```

### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### PROGRAM METHODOLOGY FOR TIME STRINGS

### When decoding WCNs, assumptions on time varibles are made for those cases where

### the watch start time is '000000T0000Z'.

### For 'NEW' reports, the watch starting and ending times are decoded from the VTEC

### line.

### For 'CON' reports, where the watch starting time is '000000T0000Z' in the VTEC line,

### the start time is set equal to the bulletin issue time and the ending time is decoded from

### the VTEC line.

### For 'CAN' reports, where the watch starting time is '000000T0000Z' in the VTEC line,

### the ending time is decoded from the VTEC line and the starting time is set equal to the

### ending time.

### OUTPUT FORMATING

### The format of the output ASCII file is as follows:

### |Watch|Issue|Start|Stop|Watch |Bulletin |Significance|Corr|Cancel|Test |type |time |time

### |time|number|originator|code |flag|flag |flag

### County/marine zone table entries, one per line, formatted as shown in the example

### below (county/marine zone information lines should appear just as they do in

### $GEMTBL/stns/mzcntys.tbl)..............................

### where:

### Watch type is TSM thunderstorm watch

```
TOR tornado watch
```
### Issue time is YYMMDD/HHMM - GEMPAK date/time format - time of the

```
of the WCN bulletin
```
### Start time is YYMMDD/HHMM - GEMPAK date/time format - Start time

```
of the WCN bulletin
```
### Stop time is YYMMDD/HHMM - GEMPAK date/time format - End time of


```
of the WCN bulletin
```
### Bulletin originator is KALY, KBGM, etc. from 2nd field of 1st line of bulletin

### Watch number is the decoded watch number

### Significance code A Watch

### Correction flag 0 Not a correction

```
1 A correction
```
### Cancel flag 0 Not a cancellation or expired report

```
1 A cancellation or expired report
```
### Test flag 0 Not a test report

```
1 A test report
```
### Example ------- |TOR|020807/1701|020807/1716|020807/2100|0002|KLSX|A|0|0|1

### ILC121 17121 Marion IL US 38.65 -88.92 0.00 0 LSX ILC051 17051 Fayette IL US

### 39.00 -89.03 0.00 0 LSX ILC189 17189 Washington IL US 38.35 -89.41 0.00 0 LSX

### LHZ361 743610 LHZ361 LH US 45.65 -83.88 0.00 0 APX ANZ335 673350 ANZ335

### AN US 41.04 -73.35 0.00 0 OKX ANZ338 673380 ANZ338 AN US 40.54 -74.08 0.00

### 0 OKX

## 6.34 DCWOU

### DCWOU decodes watch outline update reports from a real-time data feed, or from a

### file fed to the program through standard input, and writes the data to an ASCII file. The

### program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
DCWOU [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
DCWOU -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.


```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

### |Watch|Issue|Start|Stop|Watch |Bulletin |Active|Time|Corr/test|Cancel |type |time |time

### |time|number|originator|WFOs |zone|flag | flag

### County/marine zone table entries, one per line, formatted as shown in the example

### below (county/marine zone information lines should appear just as they do in

### $GEMTBL/stns/mzcntys.tbl)..............................

### where:

### Watch type is TSM thunderstorm watch

```
TOR tornado watch
```
### Issue time is YYMMDD/HHMM - GEMPAK date/time format - Issue time

```
of the WOU bulletin
```
### Start time is YYMMDD/HHMM - GEMPAK date/time format - Start time

```
of the WOU bulletin
```
### Stop time is YYMMDD/HHMM - GEMPAK date/time format - End time of

```
the WOU bulletin
```
### Bulletin originator is KALY, KBGM, etc. from 2nd field of 1st line of bulletin

### Watch number is the decoded watch number

### Active WFOs are the WFOs that the watch affects Time zone is the time zone where

### the watch has been issued Corr/test flag is 0 Not a correction; not a test

```
1 A correction; not a test
2 Not a correction; a test
3 A correction; a test
```
### Cancel flag is 0 Not a cancellation

```
1 A cancellation
```
### Example -------

### |TOR|021008/1121|021008/1130|021008/

### 1700|102|KWNS|BMX;OHX;MEG;PAH;LMK;APX;OKX|CST|0|0 ALC077 1077

### Lauderdale AL US 34.91 -87.65 0.00 0 BMX INC129 18129 Posey IN US 38.02 -87.88

### 0.00 0 PAH KYC031 21031 Butler KY US 37.21 -86.69 0.00 0 LMK MSC093 28093

### Marshall MS US 34.77 -89.50 0.00 0 MEG TNC147 47147 Robertson TN US 36.53 -

### 86.87 0.00 0 OHX LHZ361 743610 LHZ361 LH US 45.65 -83.88 0.00 0 APX ANZ335


### 673350 ANZ335 AN US 41.04 -73.35 0.00 0 OKX ANZ338 673380 ANZ338 AN US

### 40.54 -74.08 0.00 0 OKX

## 6.35 DCWSTM

### DCWSTM decodes winter storm reports from a real-time data feed, or from a file fed

### to the program through standard input, and writes the data to an ASCII file. The

### program is controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcwstm [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcwstm -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```
### The format of the output ASCII file is as follows:

### |Message|Start|Stop|Bulletin |Weather|Corr/test|Cancel |type |time |time|originator|type

### |flag | flag

### Zone table entries, one per line, formatted as shown in the example below (zone

### information lines should appear just as they do in $GEMTBL/stns/zones.tbl)......

### ........................

### where:

### Message type is WRN warning

```
WTC watch
ADV advisory
```
### Start time is YYMMDD/HHMM - GEMPAK date/time format Stop time is

### YYMMDD/HHMM - GEMPAK date/time format


### Bulletin originator is KALY, KBGM, etc. from 2nd field of 1st line of bulletin

### Weather type is SNOW snow

```
ICE ice
SL sleet
FZRA freezing rain
SLFZ sleet/freezing rain
```
### Corr/test flag is 0 Not a correction; not a test

```
1 A correction; not a test
2 Not a correction; a test
3 A correction; a test
```
### Cancel flag is 0 Not a cancellation

```
1 A cancellation
```
### Example -------

### |ADV|020425/0913|020425/1600|KBOX| |0|0 MAZ005 210050 Western_Middlesex

### MA US 4252 -7136 0 0 BOX MAZ006 210060 Western_Essex MA US 4270 -7100 0

### 0 BOX NHZ012 290120 Hillsborough NH US 4293 -7173 0 0 BOX

## 6.36 DCWTCH

### DCWTCH decodes tornado and severe thunderstorm watch box reports and watch

### status reports from a real-time data feed, or from a file fed to the program through

### standard input, and writes the data to an ASCII file. The program is controlled by inputs

### to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcwtch [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcwtch -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```

### The format of the output ASCII file is as follows:

### |Type|Start_time|End_time|Watch_number|Corr_flag|Cancel_flag

```
Lat1 Lon1
Lat2 Lon2
Lat3 Lon3
Lat4 Lon4
```
### Where: Type is TS (thunderstorm), TN (tornado) , RP (replaced) or ST (status)

```
Start_time and End_time are full GEMPAK date/time strings
Watch_number is the number assigned to the watch
Corr_flag is a flag indicating a correction (0 or 1)
Cancel_flag is a flag indicating a cancellation (0 or 1),
or, for type ST, the number of points which follow
```
### Extra spaces may appear anywhere in this line of information, except in the first

### character position. The first character must be a bar (|).

### The latitude and longitude values are read using the FORTRAN format (2F9.2). Types

### TS and TN will always have four points; the number of points for type ST may vary.

## 6.37 DCXMOS

### DCXMOS decodes Global Forecast System - Extended (GFSX) Model Output

### Statistics data from a real-time data feed, or from a file fed to the program through

### standard input, and writes the data to a GEMPAK surface file. The program is

### controlled by inputs to the command line.

### The inputs are program options and the output file name or template. For example, for

### real-time data feeds:

```
dcxmos [options] output_file
```
### If running the program interactively with standard input, the -c option must be used.

### The input file must also be specified. For example:

```
dcxmos -c YYMMDD/HHNN [other_options] output_file < input_file
```
### A template may be used to specify the output file name. The file name template uses

### the date and time of the bulletin or report to replace the following characters.

```
YYYY or YY Year with or without the century
MM Month number
DD Day
HH Hour
NN Minute
```

### Note that for data from Hawaii (WMO bulletin header FEPA20), parameter PP12

### (probability of precipitation fcst in a 12-hr period) is valid for periods of 18-30, 30-42,

### ..., 186-198 hours after 0000 UTC. For all other sites, PP12 is valid for periods of 12-

### 24, 24-36, ..., 180-192 hours after 0000 UTC.



