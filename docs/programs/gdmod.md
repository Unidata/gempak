# GDMOD

GDMOD moves grids from one GEMPAK grid file to another.


### Input Parameters
 
    GDFILE    Grid file
    GDOUTF    Output grid file
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GFUNC     Scalar grid
    GVCORD    Grid vertical coordinate
    GPACK     Packing type/number of bits
    GRDHDR    Grid Header Flags
 
 
### Program Description
 
GDMOD moves grids from an input grid file to an output
file.  The input grid file name is specified in GDFILE.  The
output grid file name is specified in GDOUTF.  The input and
output grid files must have the same navigation information.

GDATTIM allows several different input options.  If LIST is
entered, a list of dates/times will be provided after running
the program.  Upon choosing one of these times, a data search
will be conducted.  Entering ALL will allow the program to match
data pertaining to all of the dates/times in the file.

GLEVEL also has several options unique to matching grid headers.
Entering ALL will match data pertaining to all of the levels in
the file.  If two levels are entered separated by a dash, this
gives all of the levels within that range, including the bounding
levels.  Entering MAN will match and copy all of the mandatory
pressure levels into the output file.

GVCORD controls the vertical coordinate search.  One vertical
coordinate can be entered for the search, or ALL may be entered
which will match all of the vertical coordinates in the file.

GFUNC specifies the grid parameter name. If one parameter is
entered, the program searches for that parameter only.  If ALL
is entered, the program will match all of the parameters in its
search.  Entering several parameters separated by semicolons
will match those parameters only.

The output grids will be packed using the information in
GPACK.

 
### Examples
 
1.  Adds grids for lifted index value for the layer between
    1000 millibars and 500 millibars from model file
    $MODEL/ngm/ngm_96100200, to file NEW.GRD. Only grids at
    the analysis time are added.  Pack the data using 16 bits
    for each data value.

        GDFILE  =  $MODEL/ngm/ngm_96100200
        GDOUTF  =  new.grd
        GDATTIM =  961002/0000F000
        GLEVEL  =  500:1000
        GFUNC   =  lift
        GVCORD  =  pres
        GPACK   =  16/grib


2.  Copy grids with the temperature in Kelvin for all of the
    pressure levels from TODAY.GRD to DEC.GRD.  Do not pack
    any data.

        GDFILE  =  today.grd
        GDOUTF  =  dec.grd
        GDATTIM =  961002/0000F000
        GLEVEL  =  all
        GFUNC   =  tmpk
        GVCORD  =  pres
        GPACK   =

3.  For all of the times in the grid file, copy grids with the
    u- and v-components of the wind using sigma coordinates
    matching the 8967 level from OLD.GRD to NEW.GRD.  Do not
    pack any data.

        GDFILE  =  old.grd
        GDOUTF  =  new.grd
        GDATTIM =  all
        GLEVEL  =  8967
        GFUNC   =  urel;vrel
        GVCORD  =  sgma
        GPACK   =

4.  Add grids from the latest GFS model file to OUT.GRD from
    1000 to 500 millibars and the 250-millibar levels matching
    all possible parameters. A list of grid times is displayed
    for user selection.  Do not pack any data.

        GDFILE  =  gfs
        GDOUTF  =  out.grd
        GDATTIM =  list
        GLEVEL  =  1000-500;250
        GFUNC   =  all
        GVCORD  =  pres
        GPACK   =


### Error Messages
 
    [GDMOD  -1]     Fatal error initializing TAE.
    [GDMOD  -2]     Fatal error reading TAE parameters.
    [GDMOD  -3]     Invalid grid range entered.
    [GDMOD  -4]     Grid number ... is invalid.
    [GDMOD  -5]     There are no grids in file ....
    [GDMOD  -6]     No valid grids in list.
    [GDMOD  -7]     Error reading input grid.
    [GDMOD  -8]     Error writing output grid.
    [GDMOD  -9]     Error opening files.
    [GDMOD -10]     Files contain different navigations.
    [GDMOD -11]     Too many grids to transfer.
    [GDMOD -12]     Cannot parse user input.
    [GDMOD -13]     Cannot match user input.
