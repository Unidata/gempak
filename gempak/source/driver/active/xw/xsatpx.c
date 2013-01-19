#include "xwcmn.h"
#include "imgdef.h"

static int *imgDpy = NULL;
static size_t DpySize = 0;
static unsigned int last_xhght = 0;
static unsigned int last_xwdth = 0;
static char filNam[FILE_FULLSZ] = { "" };

void xsatpx ( float *dx, float *dy, int *offx, int *offy, char *imgnam,
		int *xispace0, int *yispace0, int *xispace1,
		int *yispace1, int *iarea, int *mode, int *ipix,
		float *dxo, float *dyo, int *iret )
/************************************************************************
 * xsatpx								*
 *									*
 * This subroutine gets a pixel value on an images file for the X 	*
 * window driver.							*
 *									*
 * xsatpx ( dx, dy, offx, offy, imgnam, xispace0, yispace0, 		*
 * 	    xispace1, yispace1, iarea, mode, ipix, dxo, dyo, iret)	*
 *									*
 * Input parameters:							*
 *	*dx		float		x-coordinate			*
 *	*dy		float		y-coordinate			*
 *	*offx		int		Offset X on original image	*
 *	*offy		int		Offset Y on original image	*
 *	*imgnam		char		Name of image file		*
 *	*xispace0	int		Left of image in plot coord	*
 *	*yispace0	int		Bottom of image in plot coord	*
 *	*xispace1	int		Right of image in plot coord	*
 *	*yispace1	int		Top of image in plot coord	*
 *	*iarea		int		Pixel area indicator		*
 *	*mode		int		Pixel mode indicator		*
 *									*
 * Output parameters:							*
 *	*ipix		int		Pixel value			*
 *	*dxo		float		Device X at offset location	*
 *	*dyo		float		Device Y at offset location	*
 *	*iret		int		Return code			*
 *					G_NORMAL  = normal return	*
 *			G_NIMGFL  = cannot open image file		*
 *			G_NCLRAL  = color allocation failure (xcaloc)	*
 *			G_NFILENM = file name is too long (xslutf)	*
 *			G_NIMGCOL = not enough image colors		*
 *			G_NMEMRY  = memory allocation failure		*
 *			G_NIMCORD = invalid image coordinates		*
 *			G_NIMGFMT = invalid image format		*
 *			G_BADPXV  = bad min/max pixel values		*
 **									*
 * Log:									*
 * T. Lee/GSC		 9/99	Created					*
 * T. Lee/GSC		 9/99	Checked image bounds			*
 * S. Law/GSC		10/99	Added loop changes in gemwindow		*
 * T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
 * D.W.Plummer/NCEP	 2/03	Changes for 10-bit GVAR imagery		*
 * D.W.Plummer/NCEP	 4/03	Added static filname variable filNam	*
 * D.W.Plummer/NCEP	 6/04	Added offsets and (dxo,dyo)		*
 * T. Piper/SAIC	08/06	Moved byte swapping to crarea		*
 * T. Piper/SAIC	09/06	Removed parameter lenfil		*
 * T. Piper/SAIC        11/06   Restore coltrans[0] back to original    *
 * X. Guo/CWS           04/10   Added codes to support 94 product       *
 * X. Guo/CWS           05/10   Added IFHINIDS to process 8 bit product *
 ***********************************************************************/
{
    int			*coltrans, *ct, element_size, ic, icol, icount;
    int			ier, ii, imgcol, imghght, *imgptr, imgrow;
    int			imgwdth, indx, ir, irow, isGVARRAW, ix, iy;
    int			jj, *jpix, kk, linestart, max_pix, mode_buf;
    int			mode_index[256], mode_pix, newDpy, pix_value;
    int			*rowtrans, ximage0, ximage1, yimage0, yimage1;
    size_t		dpysize, imgsize;
    double		imgratio, plotratio, sf_hght, sf_wdth;
    unsigned char	*ddptr, *dptr;
    unsigned int	col, linesize, newdim, pix;
    unsigned int	plothght, plotwdth, remainder, row, xhght, xwdth;

    Window_str		*cwin;
    winloop_t	 *cloop;

/*---------------------------------------------------------------------*/

    *ipix = IMISSD;
    *dxo  = RMISSD;
    *dyo  = RMISSD;
/*
 *  Check the input for valid bounds.  Then set the image dimensions.
 */
    if (( imbot <= imtop ) || ( imrght <= imleft ) ||
	( *xispace1 <= *xispace0) || ( *yispace0 <= *yispace1 )) {
        *iret = G_NIMCORD;
        return;
    }
    imgwdth = (imrght - imleft) + 1;
    imghght = ( imbot  - imtop  ) + 1;
/*
 *  Get the current window information.
 */
    cwin	= &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);
/*
 *  Check pixel location.
 */
    xwdth = cloop->pxm_wdth;  
    xhght = cloop->pxm_hght;  
    *iret = G_NORMAL;

    if ( *dx < 0.0F || *dx > (float)xwdth || *dy < 0.0F || *dy > (float)xhght ) {
	return; 
    }
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
                crnids ( imgnam, &ier );
	    break;

          case IFHINIDS: /*Higher Resolution NIDS radar files*/
               crnids (imgnam, &ier );
            break;
	  case IFNOWR:  /* WSI NOWRAD radar files */
	    crnowr ( imgnam, &ier );
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

    isGVARRAW = G_FALSE;
    if ( strcmp ( cmstyp, "GVAR" ) == 0 &&
         strcmp ( cmcalb,  "RAW" ) == 0  )  isGVARRAW = G_TRUE;

    newDpy = G_FALSE;
    if ( strcmp(imgnam,filNam) != 0 )  newDpy = G_TRUE;
    strcpy ( filNam, imgnam );
    if ( xwdth != last_xwdth || xhght != last_xhght )  newDpy = G_TRUE;
    last_xwdth = xwdth;
    last_xhght = xhght;
/*
 *  Fill the column translation array with incremental indices
 *  into the original image columns with respect to the previous
 *  column.  Off-image values are set to -1.
 */
    G_MALLOC(coltrans, int, xwdth, "coltrans");
    G_MALLOC(ct, int, xwdth, "ct");
    sf_wdth = (double)(imgwdth - 1) / (double)(xwdth - 1);
    ix = G_NINT (1.0F / sf_wdth);
    if ( ix < 1 )  ix = 1;
    for ( col = 0; col < xwdth; col++ ) {
	coltrans[col] = (imleft - 1) + (int)(sf_wdth * (double)col + 0.5);
	ct[col] = coltrans[col];
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
    iy = G_NINT (1.0F / sf_hght);
    if ( iy < 1 )  iy = 1; 
    for ( row = 0; row < xhght; row++ ) {
	rowtrans[row] = (imtop - 1) + (int)(sf_hght * (double)row + 0.5);
	if  ( rowtrans[row] < 0  || rowtrans[row] >= imnlin ) {
	    rowtrans[row] = -1;
	}
    }
/*
 *  Construct the final image.  Return if the point is off the image.
 */
    dpysize = xhght * xwdth;
/*
 *  Compute image row and column based on device input center (*dx,*dy) 
 *  and image grid offsets (*offx,*offy).
 *  Compute location at offset (*dxo,*dyo).
 */
    imgcol = G_NINT((double)(imleft-1)+sf_wdth*((*dx)-ximage0)) + *offx;
    imgrow = G_NINT((double)(imtop -1)+sf_hght*((*dy)-yimage1)) + *offy;
    *dxo = (imgcol-((double)(imleft-1))) / sf_wdth + ximage0;
    *dyo = (imgrow-((double)(imtop -1))) / sf_hght + yimage1;
/*
 *  Determine (icol,irow) for indexing into imgDpy array.
 */
    icol = IMISSD;
    for ( col = 0; col < xwdth; col++ ) {
	if ( imgcol <= ct[col] )  {
	    icol = (int)col; break;
	}
    }
    G_FREE ( ct, int );
    irow = IMISSD;
    for ( row = 0; row < xhght; row++ ) {
	if ( imgrow <= rowtrans[row] )  {
	    irow = (int)row; break;
	}
    }
    icount = irow * xwdth + icol;
    if ( *iarea && ( icol < 0 || icol >= (int)xwdth || 
	 irow < 0 || irow >= (int)xhght ||
	 icount >= (int)dpysize ) )  {
	G_FREE ( coltrans, int );
	G_FREE ( rowtrans, int );
	return;
    }

    if ( ( imgDpy == (int *)NULL ) || (dpysize > DpySize) ) {
	G_FREE ( imgDpy, int );
	G_CALLOC(imgDpy, int, (int)dpysize, "imgDpy");
	if ( imgDpy == (int *)NULL ) {
	G_FREE ( coltrans, int );
	G_FREE ( rowtrans, int );
	*iret = G_NMEMRY;
	return;
    }
	DpySize = dpysize;
        newDpy = G_TRUE;
    }
    imgptr = imgDpy;

    element_size = imdpth;
    linesize = (unsigned int)(imnpix * element_size);

    if ( newDpy == G_TRUE ) {
/*
 *  Process each row/line of the image.
 */
	for ( row = 0; row < xhght; row++ ) {
	    if  ( rowtrans[row] == -1 ) {
/*
 *  Off image value is set to missing.
 */
		for ( col = 0; col < xwdth; col++) { 
		    *imgptr = IMISSD;
		    imgptr++;
		}
		continue;  /* finish the row */
	    }
	    else if ( ( row != (unsigned int)0 ) &&
		      ( rowtrans[row] == rowtrans[row -1] ) ) {
/*
 *  Replicate the row/line.
 */
		for ( col = 0; col < xwdth; col++) {
		    *imgptr =  *(imgptr - xwdth);
		    imgptr++;
		}
		continue;  /* finish the row */
	    }
/*
 *  Process a row/line of data.
 *  Calculate the starting byte in image for the line.
 */
	    linestart = rowtrans[row] * linesize;
	    dptr = &imgData[linestart];

	    for ( col = 0; col < xwdth; col++) {
		if  ( coltrans[col] == -1 ) {
		    *imgptr = IMISSD;
		}
		else {
/*
 *  Adjust the data pointer to the correct column.
 */
		    dptr += coltrans[col] * element_size;
		    if  ( imdpth == 1 ) {
		        pix = *dptr;
		    }
		    else {
/*
 *  Move unsigned char '*dptr' into unsigned int 'pix'. 
 */
			pix = 0;
			ddptr = dptr;
			for ( ii = 0; ii < imdpth; ii++ ) {
			    pix = pix << 8;
			    pix += *ddptr;
			    ddptr++;
			}
		    }
		    if ( isGVARRAW == G_TRUE ) {
/*
 *  Bit pattern for 10-bit GVAR RAW looks like
 *  |0|x|x|x|x|x|x|x|x|x|x|0|0|0|0|0|
 *  so must shift 5 bits to the right.
 *  Note that we will be computing/returning
 *  the 10-bit value.
 */
			pix = pix >> 5;
		    }
/*
 *  Convert image data value to pixel value.
 */
		    if ( imdpth < 2  ||  isGVARRAW == G_TRUE ) {
			*imgptr = (int)pix;
		    }
		    else {
			if ((int)pix <= immnpx) {
			    *imgptr = IMISSD;
			}
			else if ((int)pix >= immxpx) {
			    *imgptr = IMISSD;
			}
			else {
			    *imgptr = (int)pix;
			}
		    }
		}
		imgptr++;
	    }
	}  /*  End of row/line processing  */
    }

    G_FREE ( coltrans, int );
    G_FREE ( rowtrans, int );
    G_MALLOC ( jpix, int, (int)dpysize, "jpix"); 
/*
 *  Get the pixel value.
 */
    if ( *iarea ) { 
	ic = *iarea * ix;
	ir = *iarea * iy;
	kk = 0;
	for ( jj = (irow - ir); jj <= (irow + ir); jj+= iy) {
	    for ( ii = (icol - ic); ii <= (icol + ic); ii+= ix ) {
		icount = jj * xwdth + ii;
		if ( ii > 0 && jj > 0 && ii < (int)xwdth && jj < (int)xhght && 
		     icount < (int)dpysize ) {
		    jpix[kk] = imgDpy[icount];
		    kk++;
		}
	    }
	}
    }
    else {
	indx = imgrow*linesize + imgcol*imdpth;
	*ipix = (int) imgData[indx];
	G_FREE ( jpix, int );
	return;
    }
/*
 *  Find the maximum and mode pixel values.
 */
    for ( ii = 0; ii <= 255; ii++ ) {
	mode_index[ii] = 0;
    }

    mode_buf = -1;
    mode_pix = 0;
    max_pix  = 0;

    for ( ii = 0; ii < kk; ii++ ) {
	pix_value = jpix[ii];
	mode_index[pix_value]++;
	max_pix = G_MAX ( pix_value, max_pix );

	if ( mode_index[pix_value] >= mode_buf ) {
	    if ( mode_index[pix_value] > mode_buf ) {
		mode_buf = mode_index[pix_value];
		mode_pix = pix_value;
	}
    else {
		if ( pix_value > mode_pix ) mode_pix = pix_value;
	    }
	}
    }

    G_FREE ( jpix, int );

/*
 *  Output the pixel value.
 */

    if ( *mode ) 
	*ipix = mode_pix; 
    else
	*ipix = max_pix;

}
