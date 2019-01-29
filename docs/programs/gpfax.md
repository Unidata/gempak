# GPFAX

GPFAX creates a Postscript, GIF, or TIFF file, or an X Windows display
from a 6-bit fax file.


### Input Parameters
 
    DEVICE    Device|name|x size;y size|color type
    FAXFIL    6-bit FAX product filename
 
 
### Program Description
 
GPFAX reads a 6-bit FAX file and creates an output product in
either the PostScript, GIF, or TIFF format.  The program can also
display the 6-bit FAX product as image using XW device driver.

Only one 6-bit FAX product can be processed at a time.  If there
are multiple products in a single file, then only the first
product is read.

The output product size, specified in the DEVICE parameter, should be
properly set to correctly scale the FAX product.  For the GIF and
TIFF devices, the xsize and ysize should be obtained from the
entries in the table `$GEMTBL/pgen/faxprod.tbl` that correspond to
the input FAX product identification number.  For example, the
x and y dimensions for the 411X FAX product are 1728 and 864,
respectively.  These are obtained from the fourth and fifth columns
corresponding to the 411X entry in the table.

For the TIFF device driver, the output file is always named as
FAX.tiff.  The output file must be renamed, otherwise it will be
overwritten by subsequent executions of the program.


### Examples
 
1.  Create a GIF file of 5-Day MRF MOS Mean chart, (6-bit FAX ID 411X).

        DEVICE = gif|411X.gif|1728;864
        FAXFIL = 411X.6bt

2.  Create a TIFF file of the four panel low-level significant weather
    chart, (6-bit FAX ID 501X).
        
        DEVICE = tiff||1728;1133
        FAXFIL = 501X.6bt

### Error Messages
 
    [GPFAX  -1]     Fatal error.
    [GPFAX  -3]     Fatal error initializing GEMPLT.
