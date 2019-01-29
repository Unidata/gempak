# GPTPC

GPTPC generates four hurricane graphics for the TPC.  They are:

	(1)  Wind swath plot
	(2)  Strike probability plot (Atlantic storms only)
	(3)  Wind intensity graph
	(4)  Wind speed table


### Input Parameters
 
    STRMID    Storm id/Adv num/Device/Num fcst days/Text/Track/Sz1;Sz2/Lt1;Lt2/L
 
 
### Program Description
 
GPTPC will plot up to four graphics for each tropical system
specified.  The system(s) to be plotted may be specified in one
of two ways:

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

The wind swath plot shows the cumulative radius of the 34 knot
(tropical storm force) and the 64 knot (hurricane force) winds
over the life of the storm.

The strike probability plot displays contours of the strike
probability percentage using ranges of 10 to 19 percent, 20 to
49 percent, 50 to 99 percent, and 100 percent.

The wind intensity graph shows maximum one-minute wind speed
forecast and probabilities in miles per hour vs. forecast time,
out to 72 hours.  Category information is also displayed.

The wind speed table shows the wind speed forecast expressed as a
percentage probability of reaching a given strength and category,
out to 72 hours.

The program can generate XW, GIF, or PostScript files, depending
on the value of the third field of the STRMID parameter.  The
default is GIF files.  See the help for STRMID for further details.
(GPTPC does not use the fourth or fifth fields of the STRMID
parameter.)

 
### Examples
 
1.  Use the history file to determine which new advisories are to
be plotted.  GIF files will be created.

	    STRMID	 =

2.  Plot the graphics for advisory number 8 of storm al892001.
    Display the graphics in XW only, prompting the user to select
which plots will be displayed and when to move on to the next
plot.

	    STRMID	 =  al892001/8/test

3.  Plot the graphics for advisory number 1 of storm al882001.
Each of the four graphics will be displayed in its own XW
window (the windows are tiled and may be raised as desired).
Processing is automatic.

	    STRMID	 =  al882001/1/xw

4.  Plot the graphics for advisory number 20 of storm ep1900 (which
could also have been specified as ep192000).  GIF files will be
    created.

	    STRMID   =  ep1900/20/gf

### Error Messages
 
    [GPTPC  2]   Cannot change ... in color table.
    [GPTPC  1]     No new storms to process.
    [GPTPC  -1]     Fatal error initializing TAE.
    [GPTPC  -2]     Fatal error reading TAE parameters.
    [GPTPC  -3]     Error in reading advisory.
    [GPTPC  -4]     Error in reading strike probability grid.
    [GPTPC  -5]     Error in opening history file.
    [GPTPC  -6]     No advisories found in directory.
    [GPTPC  -9]     Error in contour fill.
    [GPTPC -10]     Error in coordinates for wind intensity plot.
    [GPTPC -11]     Error in opening intensity probability table.
    [GPTPC -12]     Error in opening file name table.
    [GPTPC -13]     Could not retrieve local date/time.
