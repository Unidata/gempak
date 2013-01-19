#include "gifcmn.h"
#include "imgdef.h"

#define MIN_SCALE	0.0001  /* minimum for rmxysc */


void wsatim ( char *imgnam, int *xispace0, int *yispace0,
			int *xispace1, int *yispace1, int *ist,
			int *inum, int *iret )
/************************************************************************
 * wsatim								*
 *									*
 * This subroutine displays satellite images for the GIF driver.	*
 * It is capable of displaying raw satellite images and remapped images.*
 *									*
 * wsatim ( imgnam, xispace0, yispace0, xispace1, yispace1,		*
 *	    ist, inum, iret )						*
 *									*
 * Input parameters:							*
 *	*imgnam		char		Name of image file		*
 *	*xispace0	int		Left of image in plot coord	*
 *	*yispace0	int		Top of image in plot coord	*
 *	*xispace1	int		Right of image in plot coord	*
 *	*yispace1	int		Bottom of image in plot coord	*
 *	*ist		int		Starting point of color table	*
 *	*inum		int		Number of colors		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL  = normal return	*
 *                      G_NIMGFL  = cannot open image file              *
 *                      G_NFILENM = file name is too long (xslutf)      *
 *                      G_NIMGCOL = not enough image colors      	*
 *			G_NMEMRY  = Memory allocation failure		*
 *			G_NIMCORD = invalid image coordinates		* 
 **									*
 * Log:									*
 * G. Krueger/EAI	12/93	Modify xrest -> xsatim			*
 * G. Krueger/EAI	02/93	Added simplified color alloc scheme	*
 * S. Jacobs/NMC	 3/94	Added std image max/min values		*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window, multi-pixmap		*
 * C. Lin/EAI	         8/94	Take clrsalloc into xwcmn.h		*
 * J. Cowie/COMET	 8/94	Added byte swapping for image headers	*
 * D. Himes/COMET	 8/94	Return error code if xcaloc fails	*
 * S. Jacobs/NMC	 8/94	Updated calls to MV_SWP4 to send arrays	*
 * S. Jacobs/NMC	 9/94	Added more optional debug prints	*
 * J. Cowie/COMET	11/94	Handle radar, use default LUT file	*
 * S. Jacobs/NMC	 1/95	Changed to use a radar colors array	*
 * J. Cowie/COMET	 1/95	Fixed index color mapping rounding bug	*
 * J. Cowie/COMET	 1/95	Added image subsetting			*
 * C. Lin/EAI            3/95   Added clearing pixmap                   *
 *                              Used calloc() to clear image_data       *
 *                              Error code; error handling              *
 *                              Use ColorBanks structure                *
 *                              Bug fix: all malloc() calls             *
 * C. Lin/EAI            3/95   Take out normal coordinates input.      *
 *				Rewrite the program.   			*
 * C. Lin/EAI            9/95   Add G_ZEROCB check			*
 * J. Cowie/COMET	10/95	Changed to use icbank value		*
 * J. Nielsen-Gammon/TAMU2/96	Disabled pixmap clearing		*
 * J. Nielsen-Gammon/TAMU12/96	Modified for GIF driver			*
 * J. Nielsen-Gammon/TAMU9/98	Gempak 5.4 version			*
 * T. Lee/GSC		 7/00	Renamed x*.c to w*.c			*
 * R. Curtis/EAI	 8/00   Updated for NetCDF files                *
 * S. Chiswell/Unidata	11/00	Updated for ZLIB compressed NIDS files	*
 * R. Tian/SAIC		05/02	Modified to display fax image		*
 * D.W.Plummer/NCEP	 3/03	Changes for 10-bit GVAR imagery		*
 * S. Danz/AWC 		11/03	Changed to not clear background data	*
 * A. Hardy/NCEP	 4/04	Modified to display 10-bit VIS imagery	*
 * T. Piper/SAIC	10/05	Dynamically allocate coltrans & rowtrans*
 * T. Piper/SAIC	1/06	Change *lenfil from long int to int	*
 * T. Piper/SAIC	08/06	Moved byte swapping to crarea		*
 * T. Piper/SAIC        11/06   Restore coltrans[0] back to original    *
 * T. Piper/SAIC	03/07	Fixed flaw in row processing		*
 * X. Guo/CWS           04/10   Added codes to support 94 product       *
 * X. Guo/CWS           05/10   Added IFHINIDS to process 8 bit product *
 ***********************************************************************/
{
    int                 *coltrans, drange, element_size, ier, ii;
    int                 imghght, imgwdth, indx, isGVARRAW, iwdth;
    int                 linestart, ncolors, *rowtrans, ximage0;
    int                 ximage1, xloc, yimage0, yimage1, yloc;
    char                dev[] = "GIF";
    float               *fpix, *tmpk;
    size_t              imgsize;
    double              dbltmp, doffset, imgratio, plotratio, ratio, sf_hght, sf_wdth;
    unsigned char       datamap[256], *ddptr, *dptr;
    unsigned int        col, linesize, newdim, *pix;
    unsigned int        plothght, plotwdth, remainder, row, xhght, xwdth;

/*---------------------------------------------------------------------*/
/*
 *  Check the input for valid bounds.  Then set the image dimensions.
 */
    if (( imbot <= imtop ) || ( imrght <= imleft ) ||
        ( *xispace1 <= *xispace0) || ( *yispace0 <= *yispace1 )) {
        *iret = G_NIMCORD;
        return;
    }
    imgwdth = (imrght - imleft) + 1;
    imghght = (imbot - imtop) + 1;
/*
 *  Initialize the image data arrays.
 */
    csinit ( &ier );
/*
 *  Read image data file if this is a new file.
 */
    if  ( strcmp ( lastimg.filename, imgnam ) != 0 ) {
/*
 *  Allocate image data space.
 */
	imgsize = (size_t)(imnpix * imnlin * imdpth);

	if  ( ( imgData != (unsigned char *)NULL ) && 
	      ( imgsize <= lastimg.imgsize ) ) {
	    memset ( (unsigned char *)imgData, 0, imgsize );
	}
	else {
	    if  ( imgData != (unsigned char *)NULL ) {
	        free ( imgData );
	    }
	    imgData = (unsigned char *) calloc ( imgsize, 
				     sizeof ( unsigned char ) );
	    if  ( imgData == (unsigned char *)NULL ) {
	        *iret = G_NMEMRY;
	        return;
	    }
        }
/*
 *  Get the image data for the type of image file format.
 */
	switch ( imftyp ) {
	  case IFAREA:  /* Area file */
	    crarea ( imgnam, &ier );
	    break;
		
	  case IFGINI:  /* AWIPS GINI files */
	    crgini ( imgnam, &ier );
	    break;

	  case IFNIDS:  /* NIDS radar files */
          case IFHINIDS: /*Higher Resolution NIDS radar files*/
            crnids (imgnam, &ier );
            break;
	   
	  case IFNOWR:  /* WSI NOWRAD radar files */
	    crnowr ( imgnam, &ier );
	    break;
	    
	  case IFNCDF:  /* NetCDF files */
	    crncdf ( imgnam, &ier );
	    break;

	  case IFNEXZ:  /* ZLIB NEXRAD files */
	    crnexz ( imgnam, &ier );
	    break;

          case IFNFAX:  /* 6-bit FAX product files */
            crnfax ( dev, imgnam, &ier );
               	break;

	  case IFNEX2:  /* NEXRAD Level2 files */
	    crnex2 ( imgnam, &ier );
	    break;

	  default:  /* Error in format */
	    ier = G_NIMGFMT;
	    break;
	}
/*
 *  Check for error.  Set this file as the 'last' one if OK.
 */
	if ( ier != G_NORMAL ) {
	    *iret = ier;
	    return;
	}
	strcpy ( lastimg.filename, imgnam );
	lastimg.imgsize = imgsize;
    }
/*
 *  Check image data range.
 */
    if  ( immxpx == immnpx ) {
        *iret = G_BADPXV;
        return;
    }
/*
 *  Request image area.  Set image scaling.
 */
    imgratio = (double)imghght / (double)imgwdth;
    if  ( ( !G_DIFF(rmxysc, 1.0F) ) && ( rmxysc > MNSCAL ) ) {
	imgratio /= (double)rmxysc;
    }
/*
 *  Compute the plot area.
 */
    plotwdth  = (*xispace1 - *xispace0) + 1; 
    plothght  = (*yispace0 - *yispace1) + 1;
    plotratio = (double)plothght / (double)plotwdth; 
/*
 *  Calculate final image size.
 *  ( ximage0, ximage1, yimage0, yimage1 )
 */
    if  ( plotratio > imgratio ) { 
/*
 *  If the height to width of the space available for the image
 *  is greater than the height to width of the image, width is
 *  the limiting factor, and scale the height to keep the
 *  appropriate aspect ratio.
 */
	ximage0 = *xispace0;
	ximage1 = *xispace1;
/*
 *  Center the image in the plot area.
 */
	newdim    = (unsigned int)((double)plotwdth * imgratio);
	remainder = plothght - newdim;
	yimage0   = G_NINT ( (double)*yispace0 - (double)remainder / 2.0 );
	yimage1   = G_NINT ( (double)*yispace1 + (double)remainder / 2.0 );
    }
    else {
/*
 *  Otherwise, if the height to width of the space available for
 *  the image is less than the height to width of the image,
 *  height is the limiting factor, and scale the width to keep
 *  the appropriate aspect ratio.
 */
	yimage0 = *yispace0;
	yimage1 = *yispace1;
/*
 *  Center the image in the plot area.
 */
        newdim    = (unsigned int)((double)plothght / imgratio);
        remainder = plotwdth - newdim;
        ximage0   = G_NINT ( (double)*xispace0 + (double)remainder / 2.0 );
        ximage1   = G_NINT ( (double)*xispace1 - (double)remainder / 2.0 );
    }
/*
 *  Final image size.
 */
    xwdth = (unsigned int)((ximage1 - ximage0) + 1);
    xhght = (unsigned int)((yimage0 - yimage1) + 1);
/*
 *  Construct the mapping from the image data to the final color
 *  pixel index -- datamap[].
 */
    isGVARRAW = G_FALSE;
    if ( strcmp ( cmstyp, "GVAR" ) == 0 &&
         strcmp ( cmcalb,  "RAW" ) == 0  )  isGVARRAW = G_TRUE;
/*
 * Compute image data range.
 */
    if ( isGVARRAW != G_TRUE ) {
        dbltmp = pow (2.0, (double)(imdpth * 8) );
    }
    else {
/*
 *  Treat GVAR RAW data as 0-255.
 *  (10-bit GVAR count scaled to 8-bit brightness temperature)
 */
        dbltmp = pow(2.0, 8.0);
    }
    drange = (int)dbltmp;
    ncolors = *inum;
    ratio   = (double)(ncolors - 1) / (double)(immxpx - immnpx);
/*
 *  Compute offset for data mapping.
 */
    doffset =  (double)immnpx * ratio - 0.5;

    if  ( imdpth < 2 ||  isGVARRAW == G_TRUE ) {
/*
 *  One byte data.
 */
	for ( ii = 0; ii < drange; ii++ ) {
	    if  ( ii < immnpx )
		datamap[ii] = 0;
	    else if  ( ii > immxpx )
		datamap[ii] = *ist + ncolors - 1;
	    else {
	        if ( ! isGVARRAW == G_TRUE)
		    indx = (int)((double)ii * ratio - doffset);
	        else
		    indx = (int)((double)(ii*(ncolors-1)) / (double)drange - doffset);
	        datamap[ii] = *ist + indx;
	    }
	}
    }
/*
 *  Fill the column translation array with incremental indices
 *  into the original image columns with respect to the previous
 *  column.  Off-image values are set to -1.
 */
    G_MALLOC(coltrans, int, xwdth, "coltrans");
    sf_wdth = (double)(imgwdth - 1) / (double)(xwdth - 1);
    for ( col = 0; col < xwdth; col++ ) {
	coltrans[col] = (imleft - 1) + (int)(sf_wdth * (double)col + 0.5);
	if ( coltrans[col] < 0 || coltrans[col] >= imnpix ) {
	    coltrans[col] = -1;
	}
	else if ( col != 0 && coltrans[col-1] != -1 ) {
	    coltrans[col] -= (imleft - 1) +
				(int)(sf_wdth*(double)(col-1) + 0.5);
	}
    }
/*
 *  Fill the row translation array with indices into the
 *  original image rows.  Set off-image values to -1.
 */
    G_MALLOC(rowtrans, int, xhght, "rowtrans");
    sf_hght = (double)(imghght - 1) / (double)(xhght -1);
    for ( row = 0; row < xhght; row++ ) {
	rowtrans[row] = (imtop - 1) + (int)(sf_hght * (double)row + 0.5);
	if  ( rowtrans[row] < 0  || rowtrans[row] >= imnlin ) {
	    rowtrans[row] = -1;
	}
    }
/*
 *  Construct the final image for display.
 */
    *iret = G_NORMAL;
    element_size = imdpth;
    linesize = (unsigned int)(imnpix * element_size); 
/*
 *  Allocate memory for one row/line of pixel data.
 */
    G_MALLOC(pix, unsigned int, xwdth, "pix");
    if ( imdpth == 2 && isGVARRAW == G_TRUE ) {
	G_MALLOC(tmpk, float, xwdth, "tmpk");
	G_MALLOC(fpix, float, xwdth, "fpix");
        }
/*
 *  Process each row/line of the image.
 */
	yloc = yimage1 - 1;
	for ( row = 0; row < xhght; row++ ) {
	    yloc++;
	    if  ( rowtrans[row] != -1 ) {
/*
 *  Process a row/line of data.
 *  Calculate the starting byte in image for the line.
 */
		linestart = rowtrans[row] * linesize;
		dptr = &imgData[linestart];

		for ( col = 0; col < xwdth; col++) {
		    if  ( coltrans[col] == -1 ) {
		        pix[col] = 0;
		    }
		    else {
/*
 *  Adjust the data pointer to the correct column.
 */
			dptr += coltrans[col] * element_size;
			if  ( imdpth == 1 ) {
			    pix[col] = *dptr;
			}
                        else {
/*
 *  Move unsigned char '*dptr' into unsigned int '*pix'.
 */
			    pix[col] = 0;
			    ddptr = dptr;
			    for ( ii = 0; ii < imdpth; ii++ ) {
				pix[col] = pix[col] << 8;
				pix[col] += *ddptr;
				ddptr++;
			    }
			}
			if ( isGVARRAW == G_TRUE ) {
/*
 *  Bit pattern for 10-bit GVAR RAW looks like
 *  |0|x|x|x|x|x|x|x|x|x|x|0|0|0|0|0|
 *  so must shift 5 bits to the right.
 */
			    pix[col] = pix[col] >> 5;
			}
		    }
		}
/*
 *  Convert GVAR RAW counts to Effective Temperatures, then
 *  convert Effective Temperatures to Brightness Temperatures.
 */
	if ( isGVARRAW == G_TRUE ) {
		    iwdth = (int)xwdth;
		    im_gvtota ( &iwdth, pix, tmpk, &ier );
		    if ( ier == 0 ) im_ttob ( &iwdth, tmpk, fpix, &ier );
		    if ( ier == 0 ) {
			for ( col = 0; col < xwdth; col++) {
			    if ( ERMISS(fpix[col]) )
				pix[col] = 0;
			    else
				pix[col] = G_NINT( fpix[col] );
			}
		    }
		    else {
			for ( col = 0; col < xwdth;col++) {
			    pix[col] = G_NINT ( (double)pix[col] *
				(double)drange /(double)(immxpx - immnpx) );
			}
		    }
		}
/*
 *  Convert image data values to pixel values.
 */
		xloc = ximage0 - 1;
		for ( col = 0; col < xwdth; col++) {
		    xloc++;
		    if ( ( coltrans[col] !=  -1 ) && ( rowtrans[row] !=  -1 ) ) {
/*
 *  Before we draw, verify we are not trying to draw in the background color as that shouldn't
 *  be needed:
 *  If CLEAR=Y, the image is cleared before we get here.
 *  If CLEAR=N, then we wouldn't want to erase what is under portions of the image where there
 *              is no data.
 */
			if (imdpth < 2  ||  isGVARRAW == G_TRUE ) {
			    if (LUT[datamap[pix[col]]] != CurrentBGColorIndex)
			        gdImageSetPixel (Current_Im, xloc, yloc, LUT[datamap[pix[col]]]);
			}
			else if ((int)pix[col] <= immnpx) {
			    if (LUT[*ist] != CurrentBGColorIndex)
				gdImageSetPixel (Current_Im, xloc, yloc, LUT[*ist]);
			}
			else if ((int)pix[col] >= immxpx) {
			    if (LUT[*ist+ncolors-1] != CurrentBGColorIndex)
				gdImageSetPixel (Current_Im, xloc, yloc, LUT[*ist+ncolors-1]);
			}
			else {
			    indx = *ist + pix[col] * ratio - doffset; 
			if (LUT[indx] != CurrentBGColorIndex)
			    gdImageSetPixel (Current_Im, xloc, yloc, LUT[indx]);
			}
		    }
	    }  /*  End of column/pixel processing  */
	    }
	}  /*  End of row/line processing  */

	G_FREE ( coltrans, int );
	G_FREE ( rowtrans, int );
	G_FREE ( pix, unsigned int );
	if ( imdpth == 2 && isGVARRAW == G_TRUE ) {
	    G_FREE( tmpk, float );
	    G_FREE( fpix, float );
	}
}
