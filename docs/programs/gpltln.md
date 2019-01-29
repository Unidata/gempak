# GPLTLN

GPLTLN draws a map, latitude/longitude lines with a selected marker,
and various image and graphic products.

### Input Parameters
 
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    LAT       Latitude
    LON       Longitude
    PANEL     Panel loc/color/dash/width/regn
    MARKER    Marker color/type/size/width/hw
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
    VGFILE    Vgfile | scale file | attribute file | filter
    PLUS      Plus sign size/width
 
 

### Program Description
 
GPLTLN draws a map and/or latitude/longitude lines in every 10
degrees with a selected marker for a specified graphics area.
A plus sign is placed at the latitude/longitude intersections.
Plots may be drawn in any GEMPAK projection and may be overlaid
on images.  GPLTLN also post-processes NMAP Vector Graphics Files
(VGFs) to create products in any format supported by GEMPAK device
drivers.

Images are animated if more than one image file is specified in
SATFIL or RADFIL.  Images are sampled to correspond to the
geographic area specified by GAREA.  The geographic area may be
defined graphically by setting GAREA using the CURSOR command.
This command allows for interactive zooming of images and the
corresponding map and latitude/longitude plots.

GPLTLN can post-process NMAP VGFs to create products in different
formats including AFOS, AWIPS Redbook, GIF, TIFF, 6 bit FAX, and
PostScript.  The VGFILE variable specifies the input VGF and the
DEVICE variable specifies the desired output format.  VGF object
attributes may be modified when creating products by specifying
a table (attribute file) in the VGFILE variable.  This table uses
the same format as the `$GEMTBL/pgen/setting.tbl` to alter object
attributes.  For example, the color, width, and smoothing flag
attributes can be changed for a solid line by setting them in the
attribute file.  The table, `$GEMTBL/pgen/uattribd.tbl`, provides a
template for this capability.  Refer to this table for additional
details.  GPLTLN can also scale attributes for an entire class of
objects by specifying a table (scale file) in the VGFILE
variable.  Refer to the scale.fax table in `$GEMTBL/pgen` for
additional information.


### Examples
 
1.  Draw a Polar Stereographic map of the Northern Hemisphere.
Draw the map in color 1. Draw lat/lon lines with marker 15
in color 3. Place a plus sign on lat/lon intersections every
10 degrees in size 1 and width 3. The color is same with
the marker.
    
        MAP	     =  1
        GAREA	 =  -10;-130;-10;50
        PROJ	 =  str/90;-80;0
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        LAT	     =
        LON	     =
        PANEL	 =  0
        MARKER   =  3/15
        TITLE	 =  1
        TEXT	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw
        LUTFIL   =
        STNPLT   =
        VGFILE	 =
        PLUS	 =  1/3


### Error Messages
 
    [GPLTLN  -1]    Fatal error initializing TAE.
    [GPLTLN  -2]    Fatal error reading TAE parameters.
    [GPLTLN  -3]    Fatal error initializing GEMPLT.
