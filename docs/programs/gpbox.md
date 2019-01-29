# GPBOX

GPBOX draws a box around a region.


### Input Parameters
 
    LINE      Color/type/width/label/smth/fltr/scflg
    REGION    Region type
    DEVICE    Device|name|x size;y size|color type
 
 
### Program Description
 
This program draws a box around a region on the current
graphics device.  REGION can be specified as VIEW, PLOT, or
DEVICE.  VIEW is the view region of the graphics device.
The view region may be changed in GEMPAK programs using the
parameter PANEL.  The PLOT region is the area used for the
data plot, excluding margins.  DEVICE is the entire device
space.

Note that a box may also be drawn around the VIEW region
using the PANEL variable.


### Examples
 
1. Draw a solid line of width 1 around the view region on the
   xw device.

       LINE    =  1
       REGION  =  view
       DEVICE  =  xw

2. Draw a line around the device region in color 2, using line
   type 3 and width 5.

       LINE    =  2/3/5
       REGION  =  device


### Error Messages
 
    [GPBOX  -1]     Fatal error initializing TAE.
    [GPBOX  -2]     Fatal error in reading the TAE parameters.
    [GPBOX  -3]     Fatal error initializing GEMPLT.
