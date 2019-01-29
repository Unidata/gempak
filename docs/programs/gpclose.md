# GPCLOSE

GPCLOSE closes the specified graphics output window or file.

### Input Parameters
 
    DEVICE    Device|name|x size;y size|color type
 

### Program Description
 
GPCLOSE closes the graphics output window or file
specified in the DEVICE parameter.

 
### Examples
 
1. Close the window named "window2".

	    DEVICE	= xw|window2

### Error Messages
 
    [GPCLOSE  1]   Cannot close window. Blank name is invalid.
    [GPCLOSE  -1]   Fatal error initializing TAE.
    [GPCLOSE  -3]   Fatal error initializing GEMPLT.
