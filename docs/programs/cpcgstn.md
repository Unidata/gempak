# CPCGSTN

CPCGSTN searches for stations located inside specified areas
from a Vector Graphics File.

### Input Parameters
 
    VGFILE    Vgfile | scale file | attribute file | filter
    DATTIM    Date/time
    STNFIL    Station information file
    OUTPUT    Output device/filename
 
 

### Program Description
 
CPCGSTN lists all stations in a given station table, and
indicates which stations are located inside areas drawn
in the Vector Graphics File. The areas must be drawn using
the "tick mark" special line. The file may contain multiple
areas that do not overlap.

If the given polygon is not closed, then the appropriate
portion of the U.S. border is used to construct a closed
polygon.

The DATTIM is only used to fill in the format of the output
listing. In this way the output listing may be used as
input to SFEDIT.

The output includes the station number, GEMPAK time, the flag
indicating if the station is inside (1) or outside (0) of the
polygon and the polygon color.


### Examples
 
1.  Identify the stations located in the polygons in the VG file
    vgf.vgf. Use the current system time and the station table
`stns_ll90.tbl`. The output is displayed to the terminal.
    
        VGFILE   = vgf.vgf
        DATTIM   = last
        STNFIL   = stns_ll90.tbl
        OUTPUT   = t

2.  As in example 1, except the output is written to the file
    data.dat.

        VGFILE   = vgf.vgf
        DATTIM   = last
        STNFIL   = stns_ll90.tbl
        OUTPUT   = f/data.dat

### Example Output

	PARM = FLAG;ICLR
    
      STN    YYMMDD/HHMM    FLAG    ICLR
    69002    010831/1822       1      17
    69007    010831/1822       1      17
    69008    010831/1822       1      17
    69012    010831/1822       0       0
    69013    010831/1822       0       0
    69014    010831/1822       0       0
    69016    010831/1822       0       0
    69017    010831/1822       0       0
    69019    010831/1822       0       0
    70026    010831/1822       0       0
    .... ....
    .... ....


### Error Messages
 
    [CPCGSTN  -1]   Fatal error initializing TAE.
    [CPCGSTN  -2]   Fatal error reading TAE parameters.
    [CPCGSTN  -3]   Error initializing GEMPLT.
