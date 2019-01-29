# GPTRAJ

GPTRAJ computes trajectories for gridded data files.

### Input Parameters
 
    MAP       Map color/dash/width/filter flag
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    LUTFIL    Enhancement lookup table filename
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    GDFILE    Grid file
    GVECT     Vector grid
    GPOINT    Grid point
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GDATTIM   Grid date/time
    TSTEP     Time step (minutes)
    MARKER    Marker color/type/size/width/hw
    LINE      Color/type/width/label/smth/fltr
    RTRAJ     Back/Reverse trajectory flag
 
 

### Program Description
 
GPTRAJ computes horizontal trajectories from user supplied vector
functions on constant grid level/coordinates.

Trajectories are computed using a user specified GPOINT as the
starting or ending location (see RTRAJ). The advected GPOINT
location is determined by interpolating the gridded vector
components to the trajectory location using the nearest data time.
The trajectory location is updated iteratively through the forecast
times using TSTEP over the time range specified in GDATTIM.

TSTEP is the time step of integration (in minutes).

GPOINT may be specified using the traditional methods for
denoting a single point, or may be intered as a semicolon
separated lat/lon pair of min/max/increment.

MARKER is used to define the marker characteristics used to locate
the trajectory point at times matching the available grid fields.

LINE is used to define the line type which connects the computed
trajectory locations.

RTRAJ is a logical flag to specify whether a reverse/back trajectory
will be computed. If RTRAJ is NO, a forward trajectory is computed.
If RTRAJ is YES, a backwards trajectory will be computed from GPOINT.

The direction of the last data point is marked with a directional arrow.


### Examples
 
1.  Draw a Lambert Conformal map of the US.
    Using the most recent ETA model output, display a trajectory
    starting at DEN using the 10m wind for the time range F024-F060.

         MAP      = 1
         GAREA    = uslcc
         PROJ     = lcc
         SATFIL   =
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 1/-1/Trajectory from DEN
         TEXT     = 1/22/1/hw
         CLEAR    = yes
         DEVICE   = xw
         GDFILE   = eta
         GVECT    = wnd
         GPOINT   = den
         GLEVEL   = 10
         GVCORD   = hght
         GDATTIM  = f024-f060
         MARKER   = 6/15/6/1
         LINE     = 7
         RTRAJ    = no
         TSTEP    = 1

2.  Repeat the map above using all forecast times in
    the ETA model run for a back trajectory.

         MAP      = 1
         GAREA    = uslcc
         PROJ     = lcc
         SATFIL   =
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 1/-1/Backward Trajectory for DEN
         TEXT     = 1/22/1/hw
         CLEAR    = yes
         DEVICE   = xw
         GDFILE   = eta
         GVECT    = wnd
         GPOINT   = den
         GLEVEL   = 10
         GVCORD   = hght
         GDATTIM  = fall
         MARKER   = 6/15/6/1
         LINE     = 7
         RTRAJ    = yes
         TSTEP    = 1

3.  Display all 700mb backward trajectories for points ending at
    lat/lon pairs every 10 degrees across the domain using all
    forecast times in the ETA model run. Set the time step to
    30 minutes.

         MAP      = 1
         GAREA    = uslcc
         PROJ     = lcc
         SATFIL   =
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 1/-1
         TEXT     = 1/22/1/hw
         CLEAR    = yes
         DEVICE   = xw
         GDFILE   = eta
         GVECT    = wnd
         GPOINT   = 30/50/10;-120/-70/10
         GLEVEL   = 700
         GVCORD   = pres
         GDATTIM  = fall
         MARKER   = 6/15/6/1
         LINE     = 7
         RTRAJ    = yes
         TSTEP    = 30


### Error Messages
 
    [GPTRAJ  -1]    Fatal error initializing TAE.
    [GPTRAJ  -2]    Fatal error reading TAE parameters.
    [GPTRAJ  -3]    Fatal error initializing GEMPLT.
