# SFMOD

SFMOD moves selected surface data from an input surface file to an
output surface file.


### Input Parameters
 
    SFFILE    Surface data file
    SFOUTF    Output surface file
    DATTIM    Date/time
    DATOUT    Output date/time
    AREA      Data area
    SFPARM    Surface parameter list
 
 
### Program Description
 
SFMOD takes data from an existing GEMPAK surface file,
SFFILE, and writes the data into another existing surface
file, SFOUTF.  This program can be used to subset the
original dataset by time and/or stations and/or parameters.

This program will not create a new surface file.  The GEMPAK
program, SFCFIL, can be used to create a surface file.  If the
parameters in the output file are not the same as the
parameters in the input file, the required parameter
conversions will be done.

If a requested station or time is not in the output file, it
will be added to the file if there is room.  The number of
times and stations that can be included in a file is specified
by TIMSTN in SFCFIL.

The parameter DATOUT can be used to set different times in the
output file from those times listed with the parameter DATTIM.

The parameter SFPARM can be used to specify which parameters
in the output file are to be written.


### Examples
 
1.  Put the data for all stations at the latest time in file
    TEST.SFC into a file called LAST.SFC.

        SFFILE  =  test.sfc
        SFOUTF  =  last.sfc
        DATTIM  =  last
        AREA    =  dset

2.  Put the data for stations in Maryland and Virginia for
    all times into the output data set.

        SFFILE  =  test.sfc
        SFOUTF  =  last.sfc
        DATTIM  =  all
        AREA    =  @md/@va

3.  Put the data for stations in Maryland for 1100 - 1400 UTC
    into the output file with the new times from 1500 - 1800 UTC.
Write only the temperature and dewpoint data to the output
file.

        SFFILE   = test.sfc
        SFOUTF   = last.sfc
        DATTIM   = 940513/1100;940513/1200;940513/1300;940513/1400
        DATOUT   = 0513/1500;0513/1600;0513/1700;0513/1800
        AREA     = @md
        SFPARM   = tmpf;dwpf


### Error Messages
 
    [SFMOD  +4]     Parameter ... is not in output file.
    [SFMOD  +3]     Some stations were not added to file.
    [SFMOD  +2]     Character parameter cannot be used:  ...
    [SFMOD  +1]     Parameter ... cannot be calculated.
    [SFMOD  -1]     Fatal error initializing TAE.
    [SFMOD  -2]     Fatal error reading TAE parameters.
    [SFMOD  -3]     There are no parameters to be computed.
    [SFMOD  -4]     Time ... cannot be added.
    [SFMOD  -5]     Number of input and output date/time are not equal.
