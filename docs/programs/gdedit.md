# GDEDIT

GDEDIT reads grids from a sequential edit file and adds them
to a GEMPAK grid file.

### Input Parameters
 
    GDEFIL    Grid edit file
    GDFILE    Grid file
    GPACK     Packing type/number of bits
 
 

### Program Description
 
GDEDIT reads grids from a sequential text edit file and adds
them to a GEMPAK grid file.  The input edit file can be
created using a text editor or using the OUTPUT = F option
in GDLIST.

Certain header information must be included before the grid
data.  Required information includes the grid size and the
grid identifier.  Scaling information may also be included.

The grid size must be included on a line:

	COLUMNS: 1 33		ROWS: 1 14

The edit grid must begin at column = 1 and row = 1 and
include the entire grid.  WARNING: the edit grid size must
match the size specified in the grid file.  Otherwise, a
fatal error is generated.

The grid identifier is found on a line such as:

	921027/1200F48		850	PRES	DIVWND

The first line containing a slash ( / ) is assumed to be a
grid identifier.  The order of values is:  time 1, time 2
(optional), level 1, level 2 (optional), vertical
coordinate, parameter name.

If the data were scaled before being listed, scaling
information must be included.

 This is of the form:

	Scale factor: 10**5

The scaling information is found by searching for "factor"
and "**".

Data follow this header information.  The rows are assumed
to read from the last (top) to the first.  Each row of
data contains:

	ROW 14	0.27  . . .

The word ROW and the row number must be included.  The row
number is NOT checked to see that this is the correct row.
Data for each row may wrap to the next line.

An example of a grid edit file follows:

	Grid file: SMALL.GRD
	GRID:	TIME1	     TIME2	LEVEL1	LEVEL2	VCORD	PARM
		921025/0000		0		PRES	TMPC
	AREA:	DSET			GRID SIZE:	7	6
	COLUMNS:	1	7	ROWS:	1	6

	Scale factor : 10**0


	COLUMN:	  1	  2	  3	  4	  5	  6	  7

	ROW  6	-12.81	-12.40	-11.98	-11.30	-10.82	-11.18	-12.25
	ROW  5	 -9.89	 -9.90	 -9.41	 -8.26	 -7.00	 -6.43	 -6.80
	ROW  4	 -2.14	 -2.94	 -3.66	 -3.43	 -2.36	 -1.29	 -0.92
	ROW  3	  4.12	  4.25	  3.23	  2.35	  2.63	  3.51	  3.98
	ROW  2	  6.39	  7.35	  7.03	  6.29	  6.31	  6.85	  7.14
	ROW  1	  7.26	  7.95	  7.80	  7.57	  7.87	  8.49	  8.96


### Examples
 
1.  Add the grids in the edit file GDLIST.FIL to the grid
   file 25DEC12Z.GRD.  Do not pack the data.

        GDEFIL  =  gdlist.fil
        GDFILE  =  25dec12z.grd
        GPACK   =

2. Add the grid in TMPC.FIL to the grid file NMC.GRD.  Pack
   the data using the GRIB format.  Select the number of
   bits so that precision to two digits after the decimal
   place will be retained.

       GDEFIL  =  tmpc.fil
       GDFILE  =  nmc.grd
       GPACK   =  +2/dec


### Error Messages
 
    [GDEDIT  -1]    Fatal error initializing TAE.
    [GDEDIT  -2]    Fatal error reading TAE parameters.
    [GDEDIT  -3]    Valid grid identifier not found.
    [GDEDIT  -4]    Grid size cannot be determined.
    [GDEDIT  -5]    The grid size in the edit file is incorrect.
    [GDEDIT  -6]    Error reading grid data.
    [GDEDIT  -7]    The edit file ... is invalid.
    [GDEDIT  -8]    The output grid file ... is invalid.
