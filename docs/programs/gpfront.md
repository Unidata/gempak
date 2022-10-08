# GPFRONT

GPFRONT is a version of GPMAP that plots map symbols interpreted
from the ASUS1 bulletins. Forecast front positions from FSUS2
bulletins can be plotted by specifying the forecast hour desired
from the bulletin (typically a bulletin contains 12 and 24 hour
forecast positions).

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
    ASFIL     ASUS1 front data file (FSUS2 forecast)
    ASHR      valid hour (used for forecast hour)
    ASATT     H-L symbol Size/WX symbol flag/Frontal Pip Size
    LUTFIL    Enhancement lookup table filename
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
 
 

### Program Description
 
GPFRONT is derived from the standard GEMPAK program GPMAP.
The behaviour is similar to gpmap, with the addition of
plotting map symbols defined in an ASUS bulletin, with the
map area specified. Satellite and radar data may be used
as backgrounds. ASFIL specifies the name or file template
of a front file. For ASUS1 bulletins which contain a single
analysis time, ASHR is ignored. For FSUS2 bulletins that
contain forecast positions for 12 and 24 hour progs,
ASHR should be the desired forecast hour (eg 12 or 24).
ASATT can be used to specify the scaling of H/L symbols drawn
(default is 1.0), High and Low values flag (default is Y),
and frontal pip size (default as same as weather symbols).


### Examples
 
1.  Draw a Lambert Conformal map of the US.
    Display frontal position from the file 98032016_frt.wmo.

         GAREA    = uslcc
         PROJ     = lcc
         SATFIL   =
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 2/-1/Frontal Positions
         TEXT     = 3/31/2/hw
         CLEAR    = yes
         DEVICE   = xw
         ASFIL    = 98032016_frt.wmo
         ASHR     =
         ASATT    =
         LUTFIL   = default
         IMCBAR   =


### Error Messages
 
    [GPFRONT  -1]   Fatal error initializing TAE.
    [GPFRONT  -2]   Fatal error reading TAE parameters.
    [GPFRONT  -3]   Fatal error initializing GEMPLT.
    [GPFRONT  -4]   Frontal position file not found
