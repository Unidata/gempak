# GPVAD

GPVAD plots the NEXRAD Level III VAD wind profile.

### Input Parameters
 
    RADFIL    Radar image filename(s)
    RADTIM    Radar composite current/dattim
    WIND      Wind symbol/siz/wdth/typ/hdsz
    TITLE     Title color/line/title
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    CLEAR     Clear screen flag
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    OUTPUT    Output device/filename
 
 

### Program Description
 
GPVAD plots the NEXRAD Level III wind profile product.

RADFIL is the NEXRAD Level III input file. A template may
be specified, with an optional site name and product type
(eg NEXRIII|DDC|NWV). If a site name is provided, it will
be used to replace the %SITE% alias in the template name.
If a product type is provided, it will be used to replace
the %PROD% alias in the template string if present. NVW
is the only valid VAD product identifier at present.
The NEXRIII template is provided for NEXRAD Level III files.

RADTIM is a valid GEMPAK date/time string or abbreviation.
A time range may be specified.

CLRBAR specifies the attributes used to plot the RMS ranges
of the VAD winds.

WIND can be used to select the type and plot size of barbs or
arrows for the vectors in the VAD display.

TEXT can be used to control the attributes used to plot the text
symbols contained in the VAD product file.

Colors for the axes and wind vectors drawn by GPVAD are specified
in the gpvad.config file. If the file does not exist, a default
set of colors will be used. The config file will be searched for
in the standard locations, including the current working directory
in order to allow the user to tailor colors for the individual
application. `$GEMTBL/unidata/gpvad.config` is provided.


### Examples
 
1. Plot the VAD profile for radar location FFC using the NEXRIII
template for the most recent time. Use wind barbs, and plot the
RMS bar along the altitude axis.

        RADFIL   = NEXRIII|FFC|NVW
        RADTIM   = last
        WIND     = bk1
        TITLE    = 1/-2/VAD DISPLAY ~
        PANEL    = 0
        DEVICE   = XW
        CLEAR    = y
        TEXT     = 1/1/1/hw
        CLRBAR   = 1/v/cl/.05;.5/.3;.01


2. Plot an animated VAD profile series for radar location FFC
using the NEXRIII template and specifying the time range
from 21Z to 23Z today.

        RADFIL   = NEXRIII|FFC|NVW
        RADTIM   = 2200-2300
        WIND     = bk1
        TITLE    = 1/-2/VAD DISPLAY ~
        PANEL    = 0
        DEVICE   = XW
        CLEAR    = y
        TEXT     = 1/1/1/hw
        CLRBAR   = 1/v/cl/.05;.5/.3;.01


### Error Messages
 
    [GPVAD  +2]     NEXRIII template not found using ...
    [GPVAD  -1]     Fatal error initializing TAE.
    [GPVAD  -2]     Fatal error reading TAE parameters.
    [GPVAD  -3]     GEMPLT initialization error.
