# SFGRAM

SFGRAM draws a meteorogram for surface data.

### Input Parameters
 
    SFFILE    Surface data file
    DATTIM    Date/time
    STATION   Stations
    TRACE1    Parms/colors/range/witness
    TRACE2    Parms/colors/range/witness
    TRACE3    Parms/colors/range/witness
    TRACE4    Parms/colors/range/witness
    TRACE5    Parms/colors/range/witness
    NTRACE    Number of traces
    TAXIS     Time1-time2-tinc;lbl;gln;tck
    BORDER    Background color/type/width
    MARKER    Marker color/type/size/width/hw
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
 
 

### Program Description
 
SFGRAM draws a time series plot for surface data.  Up to
five traces may be drawn in a single plot.  NTRACE determines
the number of traces to draw.  If NTRACE is 5, the plot area
will be divided into fifths; if NTRACE is 4, the plot area will
be divided into fourths, etc.  If STATION is a single station,
all the traces will be for that station.  If STATION is a
list of stations, TRACE1 will plot data from the first station,
TRACE2 will plot data from the second station, etc.

Each TRACE parameter contains specifications for the parameters
to be plotted in the corresponding trace.  The format for
each trace is:

parms/colors/range/witness!parms/colors/range/witness

The parameters before the ! will be labeled on the left of the
plot; those after the ! will be labeled on the right.
Any GEMPAK surface parameter may be entered.  Real valued
parameters will be drawn as a graph.  Character valued
parameters will be rotated 90 degrees and written on the plot.
Symbols are plotted for the symbol parameters.  For GUST and
GUMS, the character, G, will be plotted.

Up to four parameters may be plotted along each axis.  The
parameters must be separated using semicolons.  Character
and symbols may only be plotted in positions 1, 2 or 3,
where position 1 is at the bottom of the trace, position 2
is in the middle and position 3 is at the top.

Each parameter name may be followed by a colon, the size or
line dash type, a second colon, and the width.  For example,
WSYM:.5:5 will draw weather symbols half the default size with
a line width of 5.  TMPF:3 will plot a temperature line using
dash pattern 3.

The colors for the parameters must also be separated using
semicolons.  If a single number is entered, all parameters
are drawn in that color.  If a zero is entered, the current
default color is used.

The range specifies the scaling of the y-axis.  The format
is:
    start;stop;increment

Note that, in this program, the parts of range must be
separated using semicolons.  If no range is given, it
is selected using the data values.  If start = stop,
then the y-axis range is from the mean of the data minus
this value to the mean plus the start = stop value.  If
the start or the stop value (or both) is preceded by a
+ sign, then the y-axis range is determined by that value
unless the data actually goes past it, in which case, the
y-axis is extended to plot all of the data.

Witness lines may be specified for each TRACE.  These are
horizontal dotted lines.  A list of y values, separated by
semicolons, may be entered.  Alternatively, if WITNESS =
YES, a witness line will be centered on the plot.

The time axis is specified in TAXIS as a minimum time, a
maximum time, and a time increment separated with dashes.
If any or all of the parts of TAXIS is blank, reasonable
values will be selected by the program.


### Examples
 
1.	Plot the meteorogram for PWM for all the times available
in the data set.  Plot the time axis from 19/03 to
20/09.  TRACE1 plots TMPF and DWPF in colors 2 and 3.
TRACE2 plots PMSL in color 4.  TRACE3 plots the wind
barbs and gusts in knots.  TRACE4 plots the visibility
in miles in color 7.  TRACE5 plots the cloud cover codes
at the bottom and the weather symbols at the top in color
6.
    
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        DATTIM	 =  all
        STATION	 =  pwm
        TRACE1	 =  tmpf;dwpf:3/2;3
        TRACE2	 =  pmsl/4
        TRACE3	 =  gust;brbk
        TRACE4	 =  vsby/7
        TRACE5	 =  clds;;wsym/6
        NTRACE	 =  5
        TAXIS	 =  19/03-20/09
        BORDER	 =  1
        MARKER	 =  3/12
        TITLE	 =  1
        CLEAR	 =  y
        DEVICE	 =  xw
        PANEL	 =  0
        TEXT	 =  1///sw

2.	Plot TMPF and DWPF for stations PWM, BWI and HAT.  TMPF
is plotted with a solid line using color 2 and DWPF is
plotted with a dashed line using color 3.
    
        SFFILE	 =  $GEMPAK/data/hrcbob.sfc
        DATTIM	 =  all
        STATION	 =  pwm;bwi;hat
        TRACE1	 =  tmpf;dwpf:3/2;3
        TRACE2	 =  tmpf;dwpf:3/2;3
        TRACE3	 =  tmpf;dwpf:3/2;3
        TRACE4	 =
        TRACE5	 =
        NTRACE	 =  3
        TAXIS	 =  19/03-20/09
        BORDER	 =  1
        MARKER	 =  3/12
        TITLE	 =  1
        CLEAR	 =  y
        DEVICE	 =  xw
        PANEL	 =  0
        TEXT	 =  1///sw

### Error Messages
 
    [SFGRAM  +3]    Hardware text cannot generally be rotated.
    [SFGRAM  -1]    Fatal error initializing TAE.
    [SFGRAM  -2]    Fatal error reading TAE parameters.
    [SFGRAM  -3]    Fatal error initializing GEMPLT.
    [SFGRAM  -4]    No stations have been entered.
    [SFGRAM  -5]    ... cannot be plotted in position 4.
    [SFGRAM  -6]    The time range along the x axis is 0.
    [SFGRAM  -7]    The parameter ... cannot be computed.
    [SFGRAM  -8]    The graph coordinates are invalid.
    [SFGRAM  -9]    There is no data at station ....
    [SFGRAM -10]    Station ... is invalid.
    [SFGRAM -11]    There are no times in the file.
    [SFGRAM -12]    The time ... is invalid.
    [SFGRAM -14]    The file ... cannot be opened.
    [SFGRAM -15]    There are no parameters specified.
    [SFGRAM -16]    Error in specifying TAXIS.
    [SFGRAM -17]    No valid time or file found.
