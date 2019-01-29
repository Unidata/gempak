# GPCOLOR

GPCOLOR changes the colors on a color device.


### Input Parameters
 
    COLORS    Color list
    DEVICE    Device|name|x size;y size|color type
 
 
### Program Description
 
This program changes the color components of color numbers
on color devices where the components can be set.  See the
help information on COLORS for more information.


### Examples
 
1.  Set color number 4 to violet.

        COLORS  =  4=violet

2.  Set color number 2 to yellow by specifying the red, green,
    and blue components.

        COLORS  =  2=1:1:0


### Error Messages
 
    [GPCOLOR  -1]   Fatal error initializing TAE.
    [GPCOLOR  -2]   Fatal error reading TAE parameters.
    [GPCOLOR  -3]   Fatal error initializing GEMPLT.
