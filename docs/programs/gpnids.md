# GPNIDS

GPNIDS plots NEXRAD Level III products.

### Input Parameters
 
    RADFIL    Radar image filename(s)
    RADTIM    Radar composite current/dattim
    TITLE     Title color/line/title
    PANEL     Panel loc/color/dash/width/regn
    DEVICE    Device|name|x size;y size|color type
    CLEAR     Clear screen flag
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    COLORS    Color list
    WIND      Wind symbol/siz/wdth/typ/hdsz
    LINE      Color/type/width/label/smth/fltr
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    GAREA     Graphics area
    MAP       Map color/dash/width/filter flag
    LATLON    Line color/dash/width/freq/inc/label/format
    OUTPUT    Output device/filename
 
 

### Program Description
 
GPNIDS plots NEXRAD Level III products.

RADFIL is the NEXRAD Level III input file. A template may
be specified, with an optional site name and product type
(eg NEXRIII|DDC|NWV). If a site name is provided, it will
be used to replace the %SITE% alias in the template name.
If a product type is provided, it will be used to replace
the %PROD% alias in the template string if present. Products
will br plotted in graph or map coordinates depending on
the type of product. For VAD wind profile products (NVW) the
display is graph coordinates, while raster and radial images,
and overlay products (eg mesocyclone, storm track, tvs etc) are
displayed in map coordinates. The NEXRIII template is provided
for NEXRAD Level III files.

RADTIM is a valid GEMPAK date/time string or abbreviation.
A time range may be specified.

CLRBAR specifies the attributes used to plot the RMS ranges
of the VAD winds.

IMCBAR displays the coresponding image calibration bar for
raster and radial products.

WIND can be used to select the type and plot size of barbs or
arrows for the vectors in the VAD display.

TEXT can be used to control the attributes used to plot the text
symbols contained in the NIDS product file.

COLORS provides the color used for the TEXT symbols.
Colors for the axes and wind vectors for VAD products (NVW) are
specified in the gpvad.config file. If the file does not exist,
a default set of colors will be used. The config file will be searched
for in the standard locations, including the current working directory
in order to allow the user to tailor colors for the individual
application. $GEMTBL/unidata/gpvad.config is provided.

LINE is used to control the attributes of lines displayed
in the products.

OUTPUT is used to direct display of the tablular and alphanumeric
portions of the NIDS products.

 
### Examples
 
1.	Plot the VAD profile for radar location FFC using the NEXRIII
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
        MAP      = 0

2.	Plot an animated VAD profile series for radar location FFC
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
        MAP      = 0

3.  Plot the Composite Reflectivity product for SJT and display the
    text storm attribute table to the terminal.

        RADFIL   = NEXRIII|SJT|NCR
        RADTIM   = last
        WIND     =
        TITLE    = 1/-2
        PANEL    = 0
        DEVICE   = xw
        CLEAR    = Y
        TEXT     = 1/2/2/hw
        CLRBAR   =
        IMCBAR   = 5/v/LL/.005;.6/.4;.01
        OUTPUT   = t
        GAREA    = dset
        COLORS   = 7
        MAP      = 6
        LATLON   = 0
        LINE     =

4.  Overlay the Storm Tracking Information product for SJT on the
    composite reflectivity display in #3.

        RADFIL   = NEXRIII|SJT|STI
        RADTIM   = last
        TITLE    = 1/-2
        PANEL    = 0
        DEVICE   = xw
        CLEAR    = n
        TEXT     = 1/2/2/hw
        COLORS   = 7
        LINE     = 31/1/4
        WIND     =
        CLRBAR   =
        IMCBAR   = 5/v/LL/.005;.6/.4;.01
        GAREA    = dset
        MAP      = 6
        LATLON   = 0
        OUTPUT   = t


### Error Messages
 
    [GPNIDS  +2]    NEXRIII template not found using ...
    [GPNIDS  -1]    Fatal error initializing TAE.
    [GPNIDS  -2]    Fatal error reading TAE parameters.
    [GPNIDS  -3]    GEMPLT initialization error.
