# GDDELT

GDDELT deletes grids from GEMPAK grid files.


### Input Parameters
 
    GDFILE    Grid file
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
 
 
### Program Description

GDDELT deletes grids from GEMPAK grid files.

The user can specify a particular grid by making appropriate
inputs for GVCORD, GLEVEL, GDATTIM, and GFUNC.

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


### Examples
 
1.  Delete the temperature in Kelvin at the 500-millibar level after
    choosing from a list of times within NGM.GRD.

        GDFILE   =  ngm.grd
        GVCORD   =  pres
        GLEVEL   =  500
        GDATTIM  =  list
        GFUNC    =  tmpk

2.  Delete the relative humidity value at 950 millibars from SFC.GRD.

        GDFILE   =  sfc.grd
        GVCORD   =  pres
        GLEVEL   =  950
        GDATTIM  =  961031/0000f000
        GFUNC    =  relh

3.  Delete all of the relative wind values on the sigma coordinate from all levels from TODAY.GRD.

        GDFILE   =  today.grd
        GVCORD   =  sgma
        GLEVEL   =  all
        GDATTIM  =  all
        GFUNC    =  urel;vrel

4.  Delete the 500-millibar height grid for forecast hour 00
    from the file MRF.GRD.

        GDFILE   =  mrf.grd
        GVCORD   =  pres
        GLEVEL   =  500
        GDATTIM  =  f00
        GFUNC    =  hght

### Error Messages
 
    [GDDELT  -1]    Fatal error initializing TAE.
    [GDDELT  -2]    Fatal error reading TAE parameters.
    [GDDELT  -3]    Invalid grid range entered.
    [GDDELT  -4]    Grid number ... is invalid.
    [GDDELT  -5]    There are no grids in file ....
    [GDDELT  -6]    No valid grids in list.
    [GDDELT  -7]    Input grid ... cannot be found.
    [GDDELT  -8]    Grid file ... not exist.
