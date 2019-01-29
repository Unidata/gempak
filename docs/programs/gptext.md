# GPTEXT

GPTEXT draws the contents of a text file to the output device.

### Input Parameters
 
    PANEL     Panel loc/color/dash/width/regn
    COLORS    Color list
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    TXTFIL    Text filename or LOGO|size|mode
    TXTLOC    Text location
    COLUMN    Number of columns
 
 

### Program Description
 
GPTEXT draws text read from an ASCII text file to the
output device driver. If the input file is LOGO, the NOAA
logo will be plotted.  The logo can be plotted in either
full color or monochrome. The NOAA logo is also plotted if
"NOAA" is entered for the file name. The NWS logo is plotted
if "NWS" is entered.

The text location TXTLOC is given as the X and Y values
in the Normalized coordinate system or Map coordinate
system if a '#' is prefixed before the X and Y values.
To use map coordinates, the map projection must be
defined by another GEMPAK application such as GPMAP.

COLUMN is the number of columns the text will be divided
into within a panel.


### Examples
 
1.  Draw the contents of the text file mytext.txt to the XW
device.  Display the text at the location .1;.8.  Display
in a single column.
    
        PANEL	 =  0
        COLORS   =  1
        TEXT	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw
        TXTFIL	 =  mytext.txt
        TXTLOC	 =  .1;.8
        COLUMN	 =  1

### Error Messages
 
    [GPTEXT  -1]    Fatal error initializing TAE.
    [GPTEXT  -2]    Fatal error reading TAE parameters.
    [GPTEXT  -3]    Fatal error initializing GEMPLT.
    [GPTEXT  -4]    File ... does not exist.
    [GPTEXT  -5]    Map projection is not set.
