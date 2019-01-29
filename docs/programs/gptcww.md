# GPTCWW

GPTCWW generates the track error watch/warn plot for the TPC, using
breakpoint information contained in a VGF file.


### Input Parameters
 
    STRMID    Storm id/Adv num/Device/Num fcst days/Text/Track/Sz1;Sz2/Lt1;Lt2/L
 
 
### Program Description
 
GPTCWW will plot the track error watch/warn plot for each tropical
system specified.  The system(s) to be plotted may be specified in
one of two ways:

1) A storm identifier and advisory number may be explicitly
    specified using the STRMID parameter.

2) Storm identifiers and advisory numbers may be gotten by
    checking the storm history file against the contents of
    the storm advisory directory.  Only the latest advisory
    for each storm in the directory (which is not found in the
    history file) will be processed.  This method of running is
    specified by leaving the storm id and/or advisory number
    blank in the STRMID parameter, and is the default mode.
    See the help for STRMID for further details.

The track error watch/warn plot displays the current location and
the forecasted track for the tropical system.  It also shows the
track error envelope.  Active hurricane watches and warnings and
tropical storm watches and warnings, if any, will be displayed.
The watch/warning information is gotten from a local VG file of
TCA elements created in product generation.

The program can generate XW, GIF, or PostScript files, depending
on the value of the third field of the STRMID parameter.  The
default is GIF files.  See the help for STRMID for further details.

The program will display either a 3 day or a 5 day forecast,
depending on the value of the fourth field of the STRMID
parameter.  See the help for STRMID for further details.

(GPTCWW does not use the fifth field of the STRMID parameter.)

 
### Examples
 
1.  Use the history file to determine which new advisories are to
be plotted.  GIF files will be created.  A 5 day forecast
is displayed.

	    STRMID	 =

2.  Plot the graphic for advisory number 8 of storm al892001.
    Display the graphic in XW only.  A 5 day forecast is displayed.

	    STRMID	 =  al892001/8/xw

3.  Plot the graphic for advisory number 1 of storm al882001 in an
XW window.  A 3 day forecast is displayed.

	    STRMID	 =  al882001/1/xw/3

4.  Plot the graphic for advisory number 20 of storm ep1900 (which
could also have been specified as ep192000).  A GIF file will be
    created.  A 5 day forecast is displayed.

	    STRMID   =  ep1900/20/gf

5.  Plot the graphic for advisory number 30 of storm al132004.
    Display the graphic in XW only.  Display a 5 day forecast.
    Do not plot the forecasted track.  Have cyclone's forecasted
    position up to and including 72 hours marked in size 3, and
    beyond that marked in size 1.2

        STRMID	=  al132004/30/xw/5//N/3;1.2

6.  Plot the graphic for advisory number 10 of storm al172004.
    Display the graphic in XW only.  Display a 5 day forecast.
    Plot the forecasted track up to and including 72 hours with
long-dashed red line of width 4, and beyond 72 hours with
short-dashed magenta line of width 1.  Do not plot the
    scale legend box.

        STRMID	=  al172004/10/xw/5////5;2/4;1/2;7/N

### Error Messages
 
    [GPTCWW  4]  Error in opening breakpoint VGF file.
    [GPTCWW  3]  Too few points in track error table.
    [GPTCWW  2]  Cannot change ... in color table.
    [GPTCWW  1]    No new storms to process.
    [GPTCWW  -1]    Fatal error initializing TAE.
    [GPTCWW  -2]    Fatal error reading TAE parameters.
    [GPTCWW  -3]    Error in reading advisory.
    [GPTCWW  -5]    Error in opening history_tc file.
    [GPTCWW  -6]    No advisories found in directory.
    [GPTCWW  -7]    Error in opening breakpoint file.
    [GPTCWW  -8]    Error in opening average track error table.
    [GPTCWW  -9]    Error in opening breakpoint station plotting table.
    [GPTCWW -10]    Invalid storm identifier in advisory message.
    [GPTCWW -12]    Error in opening file name table.
    [GPTCWW -13]    Error reading public advisory.
