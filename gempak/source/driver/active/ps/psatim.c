#include "pscmn.h"
#include "imgdef.h"

static void prtfax ( FILE *flun, int xsz, int ysz, float prtx, 
                     float prty, int *iret);

void psatim ( char *imgnam, int *xispace0, int *yispace0, 
				int *xispace1, int *yispace1, int *iret )
/************************************************************************
 * psatim								*
 *									*
 * This subroutine writes satellite images to the PostScript file.	*
 * It is capable of displaying raw satellite images and remapped images.*
 *									*
 * psatim ( imgnam, xispace0, yispace0, xispace1, yispace1,iret)	*
 *									*
 * Input parameters:							*
 *	*imgnam		char		Name of image file		*
 *	*xispace0	int		Left of image in plot coord	*
 *	*yispace0	int		Top of image in plot coord	*
 *	*xispace1	int		Right of image in plot coord	*
 *	*yispace1	int		Bottom of image in plot coord	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL  = normal return	*
 *					G_NIMGFL  = cannot open img file*
 *					G_NMEMRY  = Memory alloc failure*
 *					G_NIMCORD = invalid image coord	*
 *					G_BADPXV  = bad min/max pxl vals*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/96	Copied from XSATIM			*
 * S. Jacobs/NCEP	 4/97	Removed #define IMGDEF; Changed imgDpy	*
 *				to imgDpy and reworked malloc		*
 * S. Jacobs/NCEP	 6/99	Added NetCDF file type			*
 * R. Curtis/EAI	 8/00   Updated for NetCDF files                *
 * S. Chiswell/Unidata	11/00	Updated for ZLIB compressed NIDS files	*
 * R. Tian/SAIC		05/02	Updated for fax image			*
 * D.W.Plummer/NCEP	 3/03	Changes for 10-bit GVAR imagery		*
 * A. Hardy/NCEP	 4/04	Modified to display 10-bit VIS imagery	*
 * T. Piper/SAIC	10/05	Dynamically allocate coltrans & rowtrans*
 * T. Piper/SAIC	08/06	Moved byte swapping to crarea		*
 * T. Piper/SAIC        11/06   Restore coltrans[0] back to original    *
 * X. Guo/CWS		04/10   Added codes to support 94 product       *
 * X. Guo/CWS           05/10   Added IFHINIDS to process 8 bit product *
 ***********************************************************************/
{
    int                 *coltrans, drange, element_size, ier, ierr;
    int                 ii, imghght, imgwdth, indx, isGVARRAW;
    int                 iwdth, lenp, linestart, ncolors, *rowtrans;
    int                 ximage0, ximage1, yimage0, yimage1;
    char                buff[80], dev[] = "PS", hdrstr[1024];
    float               *fpix, *tmpk;
    size_t              dpysize, imgsize;
    double              dbltmp, doffset, imgratio, plotratio, ratio, sf_hght, sf_wdth;
    unsigned char       *ddptr, *dptr, *imgptr;
    unsigned char       background, datamap[256], *imgDpy;
    unsigned int        col, jj, linesize, newdim, *pix, plothght, plotwdth;
    unsigned int        remainder, row, xhght, xwdth;

/*---------------------------------------------------------------------*/
/*
 *  Check the input for valid bounds.  Then set the image dimensions.
 */
    if  ( ( imtop <= imbot ) || ( imrght <= imleft ) ||
          ( *xispace1 <= *xispace0 ) || ( *yispace1 <= *yispace0 ) ) {
        *iret = G_NIMCORD;
        return;
    }
    imgwdth = (imrght - imleft) + 1;
    imghght = (imtop - imbot) + 1;
/*
 *  Make sure that plot file is open.  Put terminal in vector mode.
 */
    if  ( ! opnfil ) { 
	psopen ( iret );
	if  ( *iret != G_NORMAL )  return;
    }
    psplot = G_TRUE; 
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
               crnexbz (imgnam, &ier );
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
	    prtfax ( flun, imnpix, imnlin, xsize, ysize, &ier);
            return;

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
    plothght  = (*yispace1 - *yispace0) + 1;
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
    xwdth = G_NINT ( (double)((ximage1 - ximage0) + 1) / 32.0 );
    xhght = G_NINT ( (double)((yimage1 - yimage0) + 1) / 32.0 );
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
    ncolors = clrbank[imbank].ncolr;
    ratio   = (double)(ncolors - 1) / (double)(immxpx - immnpx);
/*
 *  Compute offset for data mapping.
 */
    doffset = (double)immnpx * ratio - 0.5;

    if  ( imdpth < 2 ||  isGVARRAW == G_TRUE ) {
/*
 *  One byte data.
 */
	for ( ii = 0; ii < drange; ii++ ) {
	    if  ( ii < immnpx )
		datamap[ii] = 0;
	    else if  ( ii > immxpx )
		datamap[ii] = ncolors - 1;
	    else {
	        if ( ! isGVARRAW == G_TRUE)
		    indx = (int)((double)ii * ratio - doffset);
	        else
		    indx = (int)( (double)(ii * (ncolors-1)) / (double)drange - doffset);
		datamap[ii] = indx;
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
	rowtrans[row] = (imbot - 1) + (int)(sf_hght * (double)row + 0.5);
	if  ( rowtrans[row] < 0  || rowtrans[row] >= imnlin ) {
	    rowtrans[row] = -1;
        }
    }
/*
 *  Construct the final image for display.
 */
    dpysize = xhght * xwdth;
    G_CALLOC(imgDpy, unsigned char, (int)dpysize, "imgDpy");
    if ( imgDpy == (unsigned char *)NULL ) {
	G_FREE ( coltrans, int );
	G_FREE ( rowtrans, int );
	*iret = G_NMEMRY;
	return;
    }
    imgptr = imgDpy;

    background = 255;
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
    for ( row = 0; row < xhght; row++ ) {
	if  ( rowtrans[row] == -1 ) {
/*
 *  Off image value is set to background. 
 */
	    for ( col = 0; col < xwdth; col++ )
		*imgptr++ = background;
		continue;  /* finish the row */
	}
	else if ( ( row != (unsigned int)0 ) &&
		( rowtrans[row] == rowtrans[row -1] ) ) {
/*
*  Replicate the row/line.
 */
            memcpy(imgptr, imgptr - xwdth, xwdth);
            imgptr = imgptr + xwdth;
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
        for ( col = 0; col < xwdth; col++) {

	    if  ( coltrans[col] == -1 ) {
                *imgptr++ = background;
            }
            else {
		if  ( imdpth < 2 ||  isGVARRAW == G_TRUE )
                        *imgptr++ = datamap[pix[col]];
                else {
                    if  ( (int)pix[col] <= immnpx )
                        *imgptr++ = 0;
                    else if  ( (int)pix[col] >= immxpx )
                        *imgptr++ = ncolors -1;
                    else {
                        indx = pix[col] * ratio - doffset;
                        *imgptr++ = indx;
                    }
                }
            }
	}
    }  /*  End of row/line processing  */

    G_FREE ( coltrans, int );
    G_FREE ( rowtrans, int );
    G_FREE ( pix, unsigned int );
    if ( imdpth == 2 && isGVARRAW == G_TRUE ) {
       G_FREE( tmpk, float );
       G_FREE( fpix, float );
    }
/*
 *  Write the image header information.
 */
    strcpy ( hdrstr, "gsave\n" );
    strcat ( hdrstr, "% Set the scaling for the image.\n" );
    sprintf ( buff, "%d %d translate\n", *xispace0, *yispace0 );
    strcat ( hdrstr, buff );
    sprintf ( buff, "%d %d scale\n", xwdth*32, xhght*32 );
    strcat ( hdrstr, buff );
    strcat ( hdrstr, "% Drop the image.\n" );
    sprintf ( buff, "/oneline { %6d string } def\n", 3*xwdth );
    strcat ( hdrstr, buff );
    sprintf ( buff, " %5d %5d 8 [ %5d 0 0 %6d 0 %5d ]\n",
		  xwdth, xhght, xwdth, -1*xhght, xhght );
    strcat ( hdrstr, buff );
    strcat ( hdrstr, " { currentfile oneline readhexstring pop }\n");
    strcat ( hdrstr, " false 3 colorimage\n" );

    lenp = strlen ( hdrstr );
    cfl_writ ( flun, lenp, (unsigned char *)hdrstr, &ierr );
/*
 *  Write the image to the output file.
 */
    for ( imgptr = imgDpy, jj = 1; jj <= dpysize; imgptr++, jj++ ) {
 	fprintf ( flun, "%02x%02x%02x",
	      clrbank[imbank].color[*imgptr].krgun,
	      clrbank[imbank].color[*imgptr].kggun,
	      clrbank[imbank].color[*imgptr].kbgun );
	if  ( jj%xwdth == 0 )  fprintf ( flun, "\n" );
    }
/*
 *  Reset the scaling for the plot.
 */
    strcpy ( hdrstr, "grestore\n" );

    lenp = strlen ( hdrstr );
    cfl_writ ( flun, lenp, (unsigned char *)hdrstr, &ierr );

    G_FREE ( imgDpy, unsigned char );
}

/*=====================================================================*/

static void prtfax ( FILE *flun, int xsz, int ysz, float prtx, 
                                                   float prty, int *iret)
/************************************************************************
 * prtfax								*
 *									*
 * This subroutine writes 6-bit FAX image to the PostScript file.	*
 *									*
 * prtfax ( flun, xsz, ysz, prtx, prty, iret )				*
 *									*
 * Input parameters:							*
 *	*flun		FILE		Point to PostScript file	*
 *	xsz		int		Number of pixels		*
 *	ysz		int		Number of lines			*
 *	prtx		float		Paper X size in inch		*
 *	prty		float		Paper Y size in inch		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL  = normal return	*
 **									*
 * Log:									*
 * R. Tian/SAIC		05/02		Development			*
 ***********************************************************************/
{
    char 	prolog[2048], bufout[120];
    float 	scalex, scaley, scale_fact;
    float	limitx, limity;
    int		lenp;
    int		ii;
/*---------------------------------------------------------------------*/
	
    *iret = G_NORMAL;
/*
 *  Write the image header information.
 */
    strcpy (prolog, "gsave\n" );
    strcat (prolog, "% Set the scaling for the image.\n" );
/*
 *  Calculate translate and scale factor. The magic number '32' comes
 *  from the common header. 
 */
    if  ( xsz > ysz ) {  /* if landscape mode */
	if ( prtx < prty ) {
	    limitx = (prty - 0.5) * 32;
	    limity = (prtx - 0.5) * 32;
            scalex = prty*100/xsz;
            scaley = prtx*100/ysz;
            if (scalex > scaley)
                scale_fact = scaley;
            else
                scale_fact = scalex;
	    scalex = (xsz*scale_fact/100-0.5)*32;
	    scaley = (ysz*scale_fact/100-0.5)*32;
	    scalex = scalex < limitx ? scalex : limitx;
	    scaley = scaley < limity ? scaley : limity;
            sprintf ( bufout,
           " %6.2f inch .25 inch translate 90 rotate %6.2f inch %6.2f inch scale\n",
                      scaley, scalex, scaley );
	} else {
	    limitx = (prtx - 0.5) * 32;
	    limity = (prty - 0.5) * 32;
            scalex = prtx*100/xsz;
            scaley = prty*100/ysz;
            if (scalex > scaley)
                scale_fact = scaley;
            else
                scale_fact = scalex;
	    scalex = (xsz*scale_fact/100-0.5)*32;
	    scaley = (ysz*scale_fact/100-0.5)*32;
	    scalex = scalex < limitx ? scalex : limitx;
	    scaley = scaley < limity ? scaley : limity;
            sprintf ( bufout,
             " .25 inch .25 inch translate %6.2f inch %6.2f inch  scale\n",
                      scalex, scaley );
	}
    }
    else {  /* if portrait mode */
	if ( prtx > prty ) {
	    limitx = (prty - 0.5) * 32;
	    limity = (prtx - 0.5) * 32;
            scalex = prty*100/xsz;
            scaley = prtx*100/ysz;
            if (scalex > scaley)
                scale_fact = scaley;
            else
                scale_fact = scalex;
	    scalex = (xsz*scale_fact/100-0.5)*32;
	    scaley = (ysz*scale_fact/100-0.5)*32;
	    scalex = scalex < limitx ? scalex : limitx;
	    scaley = scaley < limity ? scaley : limity;
            sprintf ( bufout,
           " %6.2f inch .25 inch translate 90 rotate %6.2f inch %6.2f inch scale\n",
                      scaley, scalex, scaley );
	} else {
	    limitx = (prtx - 0.5) * 32;
	    limity = (prty - 0.5) * 32;
            scalex = prtx*100/xsz;
            scaley = prty*100/ysz;
            if (scalex > scaley)
                scale_fact = scaley;
            else
                scale_fact = scalex;
	    scalex = (xsz*scale_fact/100-0.5)*32;
	    scaley = (ysz*scale_fact/100-0.5)*32;
	    scalex = scalex < limitx ? scalex : limitx;
	    scaley = scaley < limity ? scaley : limity;
            sprintf ( bufout,
             " .25 inch .25 inch translate %6.2f inch %6.2f inch  scale\n",
                      scalex, scaley );
	}
    }

    strcat (prolog, bufout);
    strcat (prolog, "% Drop the image.\n" );
    sprintf(bufout, "/picstr %d string def \n", xsz/8);
    strcat (prolog, bufout);
    strcat (prolog, "\n/imagefax \n");
    strcat (prolog, "   {\n");
    sprintf(bufout, "%d %d true [%d 0 0 -%d 0 %d ]\n", xsz, ysz, xsz, ysz, ysz);
    strcat (prolog, bufout);
    strcat (prolog, "      { currentfile picstr readhexstring pop }\n");
    strcat (prolog,"       imagemask\n");
    strcat (prolog, "    } def \n");
    strcat (prolog, "  imagefax\n");
    lenp = strlen ( prolog );
    cfl_writ ( flun, lenp, (unsigned char *)prolog, iret );
/* 
 *  Write the image data out to the file. 
 */
    for (ii = 0; ii < xsz*ysz/8; ii++) {
	fprintf ( flun, "%02x", (imgData[ii] & 0xff) );	
	if ( ii%(xsz/8) == 0 ) { 
            fprintf ( flun, "\n" );
	}
    }
    fprintf ( flun, "\n" );
}
