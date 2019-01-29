# GPRCM

GPRCM is a version of GPMAP that plots Radar Coded Message (RCM)
bulletins.

### Input Parameters
 
    MAP       Map color/dash/width/filter flag
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    RCMFIL    RCM data file
    RADTIM    Radar composite current/dattim
    RADDUR    Radar time window (minutes prior to RADTIM)
    DITHER    dither level
    MESO      meso color/marker/size/width/hw;filter
    TVS       tvs color/marker/size/width/hw;filter
    CNTR      Centroid barb color/size/width;filter
    MAXTOP    Maxtop color;filter
    RADINFO   Radar status color
    ECHO      Plot radar echoes (0,1,2)
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    LUTFIL    Enhancement lookup table filename
    IMBAR     Color/ornt/anch/x;y/ln;wd/freq|text_info
    OUTPUT    Output device/filename
 
 

### Program Description
 
GPRCM is derived from the standard GEMPAK program GPMAP.
The behavior is similar to gpmap, with the addition of
reading Radar Coded Message (RCM) bulletins and optionally
plotting radar echoes and/or annotations from the remarks
included within the RCM bulletin.

RCM bulletins contain digitized reflectivity values derived
from the 2.2 x 2.2 nmi NIDS Composite reflectivity product
over the radar area of coverage out to 248 nmi based on grid squares
approximately 5.4 nmi on a side. The reflectivity intensity
value for each grid box is determined by assigning the maximum
value of all Composite Reflectivity boxes whose centers
are contained within the grid square. The RCM message
comprises 9 intesity categories as follows:

    Code                       Color Plotted
     0    ND
     1    15-30 dBZ             23
     2    30-40 dBZ             22
     3    40-45 dBZ             21
     4    45-50 dBZ             20
     5    50-55 dBZ             17
     6    >= 55 dBZ             15
     7    data beyond 124 nmi that are above threshold
     8    data beyond 124 nmi that are below threshold

RCM data within a user specified time range are plotted.

RCMFIL is the RCM data file. Bulletins with radar observation times
in the RCM file will be plotted if the observation time is within
the range specified by the starting and ending times.

RADTIM (current or dattim) specifies the ending time for accepted data.
RADDUR is the number of minutes prior to the ending time to begin
accepting data.

DITHER is used to specify the plotting behavior of the reflectivity
intensities. A value of 0 specifies no dithering, the grid box is
completely filled. Values 1-4 increase the number of dither points
per grid box, approaching an increasingly opaque value. Dither
can be used to overlay image data with radar reflectivity in
order to simulate transparency. A dither value >= 5 will outline
the grid box.

MESO specifies the color/marker/size/width/hw for mesocyclones to be
marked. A color value of 0 is used to turn off mesocyclone plotting.
Marker attributes behave as MARKER for other plotting programs.

TVS specifies the color/marker/size/width/hw for tornado vortex signatures
to be marked. A color value of 0 is used to turn off TVS plotting.

CNTR specifies the color/size/width for Cell Centroid movement barbs (knots).
A value of 0 is used to turn off centroid plotting.

MAXTOP specifies the color for cell top annotations (feet).
A value of 0 is used to turn off cell top annotations.

RADINFO specifies the color for radar site operational status annotations.
NA, data not available; OM, out for maintenance; NE, no echoes.

ECHO specifies whether to plot the grid box intesities. A value of 0
specifies no plotting of echoes; 1 specifies plotting only those
echoes from radars in precipitation mode; 2 specifies plotting
of echoes from radars in both precipitation and clear air mode.

RCM summary information is directed to OUTPUT. Summary information
includes the station ID, reorting time, operational mode, and
mesocyclone and tornado vortex signature locations (if present).

GPRCM utilizes the `nexrad.tbl` station table to determine the radar
location (not all RCM bulletins contain radar location as of Nov. 1999).
The radars listed in the station table are also used to determine
which locations are missing (Not Available: NA).


### Examples
 
1.  Draw a Lambert Conformal map of the US.
    Display RCM data from the file rcm.dat. Plot radar observations
    for the past 60 minutes. Display the echo intensities (no dithering),
    maxtop values in yellow, and radar status in red. Do not plot mesocyclon
    or TVS locations. Plot the cell centroid motion barbs in white.
    Use echoes from radar sites operating in precipitation mode only. Use
    the default CLRBAR positioning.

         GAREA    = us
         PROJ     = str/90;-105;0
         SATFIL   =
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 1/-1/Radar Summary
         TEXT     = 1.3/22/1/hw
         CLEAR    = yes
         DEVICE   = xw
         RCMFIL   = rcm.dat
         RADTIM   = current
         RADDUR   = 60
         DITHER   = 0
         MESO     = 0
         TVS      = 0
         CNTR     = 1/1/1
         MAXTOP   = 5
         RADINFO  = 2
         ECHO     = 1
         CLRBAR   = 1
         LUTFIL   =

2.  Overlay the IR satellite imagery with RCM data for the US west coast.
    Use a DITHER value of 2 to for moderate transparency. Plot the radar
    CLRBAR horizontally above title. Satellite CLRBAR will be vertical along
    the left side of the image.

         GAREA    = ca-
         PROJ     = sat
         SATFIL   = $SAT/GOES-10/4km/IR/IR_991108_1600
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 1/-1/Satellite and Radar Composite
         TEXT     = 1/22/1/hw
         CLEAR    = yes
         DEVICE   = xw
         RCMFIL   = rcm.dat
         RADTIM   = current
         RADDUR   = 60
         DITHER   = 2
         MESO     = 0
         TVS      = 0
         CNTR     = 0
         MAXTOP   = 0
         RADINFO  = 0
         ECHO     = 1
         CLRBAR   = 1/h/cc/.46;.06
         LUTFIL   = default


### Error Messages
 
    [GPRCM  -1]     Fatal error initializing TAE.
    [GPRCM  -2]     Fatal error reading TAE parameters.
    [GPRCM  -3]     Fatal error initializing GEMPLT.
