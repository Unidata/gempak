# GPSCAT

GPSCAT draws scatterometer wave height and wind maps, latitude/longitude
lines and various controllable image and graphic customizations

### Program Description

GPSCAT draws a map and/or latitude/longitude lines for a specified 
graphics area.  Plots may be drawn in any GEMPAK projection and 
may be overlaid on images.  Unlike GPMAP, GPSCAT can not display 
various types of graphics products such as AWIPS, AFOS, watches, 
etc.

Images are sampled to correspond to the geographic area specified
by GAREA.

The parameter SCAT sets the type of data (ASCT_HI, QSCT_HI, SGWH2).
The parameter SCTTIM sets the ending time. SCTMIN sets the time
(in minutes) to proceed back from SCTTIM (default LAST).

SPDINT sets the speed intervals for scatterometer winds.

COLR1 and COLR2 are two color ranges to use for scatterometer wind 
display.

MARKER defines the wind barb/arrow attributes, while SCTTYP defines
the timestamp ttributes and various plotting-related flags.


### Examples

1.  Draw an orthographic projection of North America using 
ASCT_HI winds colored for magnitude. Plot the last 6 hours of data.

        MAP	     =  1
        GAREA    =  0;-105;0;-105
        PROJ     =  ort/50;-105;0
        IMCBAR	 =
        LATLON	 =  2/10/1/1/15;15
        PANEL	 =  0
        TITLE	 =  1
        TEXT	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw
        LINE	 =
        SCAT     =  ASCT_HI
        SCTTIM   =  last
        SCTMIN   =  360
        COLR1    =  30;6;26;24;21;23;5;17;8;14
        COLR2    =  31;31;31;31;31;31;31;31;31;31
        SPDINT   =  6;12;18;24;30;36;42;48;54;60
        MARKER   =  .2;.4;1;5
        SCTTYP   =  0|0|0|0|n|n|n
        TRAK1    =  
        TRAKE    =  
        TRAK2    = 

2.	Draw a world wide mercator projection for OSCT_HI winds for the
last 36 hours. Mark time stamps every 60 minutes with color 31
and line width 1.

        MAP      =  1
        GAREA    =  -80;-180;80;180
        PROJ     =  mer
        IMCBAR   =
        LATLON   =  2/10/1/1/15;15
        PANEL    =  0
        TITLE    =  1
        TEXT     =  1
        CLEAR    =  yes
        DEVICE   =  xw
        LINE     =
        SCAT     =  OSCT_HI
        SCTTIM   =  last
        SCTMIN   =  2200
        COLR1    =  30;6;26;24;21;23;5;17;8;14
        COLR2    =  31;31;31;31;31;31;31;31;31;31
        SPDINT   =  6;12;18;24;30;36;42;48;54;60
        MARKER   =  .2;.4;1;5
        SCTTYP   =  0|60|31|1|Y|Y|Y|Y
        TRAK1    =
        TRAKE    =
        TRAK2    =
