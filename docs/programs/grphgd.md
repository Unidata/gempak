# GRPHGD

GRPHGD generates a grid either from contours drawn via NMAP product generation,
or from a provided VGF file.

### Input Parameters
 
    GDOUTF    Output grid file
    GUESS     Guess file
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
    MAXGRD    Maximum number of grids
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    ANLYSS    Grid analysis block
    CNTRFL    Contour file
    GDATTIM   Grid date/time
    GFUNC     Scalar grid
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    KEYCOL    Key color
    KEYLINE   Key line (LINE or SPLN/subtyp)
    OLKDAY    Day of extended outlook
    GGLIMS    Grid value limitations and control
    HISTGRD   Toggle writing history grid (y/n)
    BOUNDS    BOUNDS name|<key>key_name|mask_flag
    TYPE      GDPLOT2 function processing type
    GAMMA     Convergence parameter
    SEARCH    Search radius/Extrapolation
    NPASS     Number of passes
    QCNTL     Quality control threshold
    GUESFUN   Guess grid
    CATMAP    s1=x;s2=y;...sN=z
    DISCRETE  a-b=x;c-d=y;e-f=z
    DLINES    [yes/no][;yes/no] | epsilon
    GGVGF     VGF file
    EDGEOPTS  Option to specify boundary conditions.
 
 

### Program Description
 
GRPHGD generates a grid from contours drawn via NMAP product generation.
A two-step process is involved:

1) Contours must be drawn via NMAP and the coordinates saved,
2) The saved coordinates are passed into GRPHGD for
   conversion into gridded data.

Alternatively, the user can specify the VGF file name, containing contour
information, thus by-passing NMAP procedre described below.

#### NMAP Procedure

To generate contours, invoke NMAP and enter product generation.
Contours are defined as a simple solid line grouped with a numeric
text value using 'LABEL' grouping.  The label may be located anywhere.
Any number of equal-valued labels may be grouped with any number of lines.
When drawing, labeling and grouping have been completed, a text file
containing the geographical contour coordinates must be created.  Click
'PROD' then 'GRAPH GRID'.  A popup will appear with a listing of all
properly grouped contours and their associated value and coordinates.
Click 'SAVE' to save this text to the ASCII file CNTRFL.  This is the
file which will be passed into GRPHGD.

Note that NMAP now invokes the GRPHGD program directly when the user
selects "MAKE GRID".

#### GRPHGD Procedure

To generate a gridded field from contour data, use the program GRPHGD.
The following parameters are used:

	GDOUTF   = Output grid file
	PROJ     = Map projection/angles/margins|dr
	GRDAREA  = Area covered by grid
	KXKY     = Number of grid points in x;y
	MAXGRD   = Maximum number of grids
	CPYFIL   = File to be copied
	ANLYSS   = Grid analysis block
	CNTRFL   = Contour file (ASCII data generated via NMAP)
	GDATTIM  = Grid date/time
	GFUNC    = GEMPAK parameter name
	GLEVEL   = GEMPAK vertical level value
	GVCORD   = GEMPAK vertical coordinate
	KEYCOL   = Key color
	KEYLINE  = Key line (LINE or SPLN/subtyp)
	OLKDAY   = Day of extended outlook
	BOUNDS   = Bound area(s) to exclude or include
	TYPE     = function processing type
    GAMMA    = Convergence parameter
    SEARCH   = Search radius/Extrapolation
    NPASS    = Number of passes
    QCNTL    = Quality control threshold
    GUESFUN  = Guess grid
    CATMAP   = s1=x;s2=y;...sN=z
    DISCRETE = a-b=x;c-d=y;e-f=z
    DLINES   = Directional lines
    GGVGF    = VGF file
    EDGEOPTS = FALSE

Several parameters (PROJ, GRDAREA, KXKY, MAXGRD, CPYFIL, ANLYSS) are
intended to be used identically as in the programs GDCFIL and NAGRIB.
These define the grid (and gridfile) to create the data.
The grid is written to file GDOUTF with parameter name GFUNC valid
at GDATTIM.  GLEVEL and GVCORD may be assigned accordingly.
KEYCOL indicates which lines actually get processed; =0 processes all
lines, =n processes only lines with color n. BOUNDS determines whether
to assign missing values to gridpoints inside (BOUNDS=...|...|false) or
outside (BOUNDS=...|...|true) a bounded area. Up to ten bounds areas
may be specified by separating with '+'. KEYLINE is similar to KEYCOL
but is for line type.  It indicates which lines get processed; =0
all line types are processed, =m/n, where m is the vgtype and n is
the subtyp of vgtype m.

TYPE specifiies the processing type that is used similar to that of GDPLOT2
function. Valid TYPEs are 'c' for contour line processing, 'b' for wind
barb processing, and 'a' for wind arrow processing.

The parameters GAMMA, SEARCH, NPASS, QCNTL and GUESFUN apply to analysis
of point-value wind vector information (TYPEs 'b' or 'a').

CATMAP allows for mapping of alphanumeric contour labels to be mapped to
numeric-only values prior to the GRPHGD analysis. For instance, the contour
label 'LOW' could be mapped to the value '1', 'MED' to '2', etc.

OLKDAY indicates the day of the extended outlook to process.

DISCRETE allows for the discrete representation of values on the final grid.
For instance, all values inside a closed contour with value '0.5' could be
assigned the value '0.5'.

DLINES specifies that the direction of the contour line (the order of points
which may be depicted with an arrowed line) will be used to determine the
final grid point values in ambiguous cases. For instance, if only one
contour line is drawn, the situation is ambiguous as to which side of the
line should have values greater (or lesser) than the contour value. The
convention is that GREATER VALUES ARE ALWAYS TO THE RIGHT OF THE LINE.
DLINES also specifies if the left of line value is smaller. If the
DISCRETE option is enabled, then DLINES is automatically TRUE for right of
line.

GGVGF specifies the name of VGF file that contains the contour information.
If GGVGF is not specified, the CNTRFL parameter must be specified and exist.
If GGVGF is specified, there is no need to go through NMAP Procedure,
described above.  The grid file would be generated from the specified VGF
file.  Also, the info file would be generated as well.
If both GGVGF and CNTRFL are specified, the contour information is gotten
from the specified VGF file, and written out to the specified .info file.

EDGEOPTS specifies whether or not to access the contours beyond the grid
domain. Input TRUE or FALSE.

The GRPHGD contour line algorithm

GRPHGD loosely follows an algorithm originally described in TDL OFFICE
NOTE 92-14 "The Systematic Interpolative Radial Search (SIRS) -- A
Method to Compute Gridpoint Values from Contours".  Some changes
were incorporated to improve data consistency, to increase execution
efficiency and for added capability:

The program performs several methodologies to assign values at gridpoints:
1) bounds check w/ appropriate missing value assignments,
2) exact value assignment when a contour lies directly on a gridpoint,
3) radial search/assignment for grid points between different valued
   contours,
4) grid points within closed contours, with and without relative
   minima/maxima,
5) grid points surrounded by like-valued contours, without
   relative minima/maxima,
6) a weighted search and assignment where neither 1) thru 5) could be
   applied.  A 9-point smoother is applied as a final step.

The GRPHGD wind barb/arrow algorithm

Wind barbs and arrows are analyzed using the GEMPAK Objective Analysis
Barnes algorithm.


### Examples
 
1.  Create a grid from the NMAP ASCII file qpf.info.  Use the same grid
as found in the RUC2 model.  Identify the grid as 'prcp' valid
at 980903/1200.

	    GDOUTF   = qpf.98090312
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   = 10
        CPYFIL   = $MODEL/ruc2/ruc2_98090312
        ANLYSS   =
        CNTRFL   = qpf.info
        GDATTIM  = 980903/1200
        GFUNC    = prcp

2.  Same as example 1, except assign missing values outside the SSA
    bounded area HPC050, i.e., assign values only to those grid points
    inside the bounds area.

        GDOUTF   = qpf.98090312
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   = 10
        CPYFIL   = $MODEL/ruc2/ruc2_98090312
        ANLYSS   =
        CNTRFL   = qpf.info
        GDATTIM  = 980903/1200
        GFUNC    = prcp
        BOUNDS   = SSA|<AREA>HPC050|true

3.  Same as example 2, except that the countour information is processed
    directy from gpf.vgf file.

        GDOUTF   = qpf.98090312
        PROJ     =
        GRDAREA  =
        KXKY     =
        MAXGRD   = 10
        CPYFIL   = $MODEL/ruc2/ruc2_98090312
        ANLYSS   =
        CNTRFL   =
        GDATTIM  = 980903/1200
        GFUNC    = prcp
        BOUNDS   = SSA|<AREA>HPC050|true
        GGVGF   =  qpf.vgf

### Error Messages
 
    [GRPHGD  +5]    WARNING.  visib reset to TRUE by rule.
    [GRPHGD  +4]    WARNING.  Maximum number of lines exceeded.
    [GRPHGD  +3]    WARNING.  Maximum number of intersections exceeded (...).
    [GRPHGD  +2]    WARNING.  This grid dimension is too large.
    [GRPHGD  +1]    WARNING.  This grid is too large.
    [GRPHGD  -1]    Fatal error initializing TAE.
    [GRPHGD  -2]    Fatal error reading TAE parameters.
    [GRPHGD  -3]    Fatal error initializing GEMPLT.
    [GRPHGD  -4]    Navigation information is invalid.
    [GRPHGD  -5]    Grid area ... is invalid.
    [GRPHGD  -6]    Grid size is invalid.
    [GRPHGD  -7]    The grid file name may not be blank.
    [GRPHGD  -8]    Cannot create file ....
    [GRPHGD  -9]    Grid name ... cannot be found in grid table.
    [GRPHGD -11]    Cannot open file ....
    [GRPHGD -12]    Cannot convert value from file ....
    [GRPHGD -13]    File ... does not exist.
    [GRPHGD -14]    The control file name may not be blank.
    [GRPHGD -15]    Boundary condition can not be set due to open line(s).
    [GRPHGD ]
