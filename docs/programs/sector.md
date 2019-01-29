# SECTOR

SECTOR subsets a GINI satellite image in area and pixel resolution.

### Input Parameters
 
    SATFIL    Satellite image filename(s)
    OUTFIL    Output satellite image filename
    GAREA     Graphics area
    PIXRES    Resolution of output image
 
 

### Program Description
 
SECTOR subsets a GINI satellite image in area and pixel
resolution.  The input satellite file MUST be an AWIPS/GINI
file.  The new file is created with an updated header based
on the subset area or the new number of pixels and lines.

If GAREA is DSET the entire image is used, otherwise a subset
specified by GAREA is obtained from the image.  PIXRES is the
input for how many pixels and lines to include in the new
image. If PIXRES is 4, then every 4th pixel and line will be
used in the new image.


### Examples
 
1.  Create a subset image over Kansas, that uses all pixels.
    
        SATFIL	 =  IR_960723_1200
        OUTFIL	 =  IR_960723_1200_ks
        GAREA	 =  ks
        PIXRES   =  1

2.  Create a new image that is for the entire area but uses
every 4th pixel and line.

        SATFIL	 =  IR_960723_1200
        OUTFIL	 =  IR_960723_1200_4
        GAREA	 =  dset
        PIXRES   =  4

### Error Messages
 
    [SECTOR  -1]    Fatal error initializing TAE.
    [SECTOR  -2]    Fatal error reading TAE parameters.
    [SECTOR  -3]    Error initializing GEMPLT.
    [SECTOR  -4]    Input file is not a GINI image.
    [SECTOR  -5]    File ... does not exist.
    [SECTOR  -6]    Too few points in the requested image.
    [SECTOR  -7]    Requested image has no points from the original.
    [SECTOR  -8]    Bad bounds for the requested image.
    [SECTOR  -9]    Pixel resolution, ..., is invalid
