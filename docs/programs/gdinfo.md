# GDINFO

GDINFO lists information about GEMPAK grid files.

### Input Parameters
 
    GDFILE    Grid file
    LSTALL    Full list flag
    OUTPUT    Output device/filename
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
 
 

### Program Description
 
GDINFO lists information about GEMPAK grid files.

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

The navigation information and grid analysis information will
be listed.  If requested, the grids in the file will also be
listed.  If `LSTALL = YES`, all the grids in the file will be
listed.  If `LSTALL = NO`, only the navigation and analysis
information will be displayed.

 
### Examples
 
List all of the latest NGM Model information to the screen for
the 500-millibar temperature in Kelvin from the 00 time period.

	GDFILE	 =  ngm
	LSTALL	 =  YES
	OUTPUT	 =  t
	GDATTIM  =  f00
	GLEVEL   =  500
	GFUNC    =  TMPK
	GVCORD   =  PRES

### Error Messages
 
    [GDINFO  -1]    Fatal error initializing TAE.
    [GDINFO  -2]    Fatal error reading TAE parameters.
