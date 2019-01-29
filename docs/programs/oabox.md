# OABOX

OABOX draws a box around an objective analysis region.


### Input Parameters
 
    LINE      Color/type/width/label/smth/fltr/scflg
    DEVICE    Device|name|x size;y size|color type
    REGION    Region type
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    GDFILE    Grid file
 
 
### Program Description
 
This program draws a box around a region defined for the
objective analysis programs.  REGION can be specified as
GRID, DATA, or EXTEND.  The default is GRID.

The GRID region is the area used for the objective analysis.
The DATA region is the area over which data for the
analysis is extracted.  The EXTEND region is the grid
area extended in each direction for the first pass analysis.


### Examples
 
1.  Draw a solid line around the grid area in color 1 using
    a solid line of width 1.

        LINE    =  1
        REGION  =  grid
        GDFILE  =  sample.grd
        DEVICE  =  xw
        GAREA   =  us
        PROJ    =  mer


2.  Draw a line around the data region using color 2, line type
    3, and line width 5.

        LINE    =  2/3/5
        REGION  =  data


### Error Messages
 
    [OABOX  -1]     Fatal error initializing TAE.
    [OABOX  -2]     Fatal error reading TAE parameters.
    [OABOX  -3]     Fatal error initializing GEMPLT.
    [OABOX  -4]     No box will be drawn since the color is 0.
    [OABOX  -5]     Invalid input for REGION.
