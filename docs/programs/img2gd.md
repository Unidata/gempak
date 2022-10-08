# IMG2GD

IMG2GD converts an image to a grid. The grid navigation specified may be
different than that of the source image.


### Input Parameters
 
    PROJ      Map projection/angles/margins|drop flag
    GRDAREA   Area covered by grid
    KXKY      Number of grid points in x;y
    GRDNAM    Grid parameter name
    GDFILE    Grid file
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GDATTIM   Grid date/time
    CPYFIL    Grid file whose navigation is to be used in new grid file | subare
    MAXGRD    Maximum number of grids
    IMGFIL    Image filename(s)
    IMGTIM    image file current/dattim
    CALIMG    Flag to use calibrated values or pixel values
 
 
### Program Description
 
IMG2GD converts an image to a grid.

GDFILE specifies the output grid file. If the file does not already
exist, the file is created using the grid defined by CPYFIL,
or if CPYFIL is not defined then by PROJ, GRDAREA, and KXKY.

CPYFIL may provide either an existing grid file to read the projection
information from, or a grid number (#nnn) defined in `grdnav.tbl`.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if
the output file does not already exist, and CPYFIL is blank.

GRDNAM is the name of the output grid.

GVCORD is the grid coordinate of the output grid.

GDATTIM is the date/time of the output grid.

IMGFIL is the source image file name or template to be copnverted
to a grid.

IMGTIM is the time to use for IMGFIL templates.

CALIMG determines whether to use the supplied calibration information
of the image for gridded values (YES), or use the raw pixel values of
the image (NO).

