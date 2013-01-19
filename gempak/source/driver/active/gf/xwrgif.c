#include "xwcmn.h"

int WriteGIF (	FILE		*fp,
		unsigned char	*pic,
		int		 w,
		int		 h,
		unsigned char	*rmap,
		unsigned char	*gmap,
		unsigned char	*bmap,
		int		 numcols,
		int		 colorstyle );

void xwrgif ( int *iret )
/************************************************************************
 * xwrgif								*
 *									*
 * This subroutine writes out a gif file from the current graphics 	*
 * window.								*
 *									*
 * xwrgif ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * P.Neilley/NCAR        5/95   Adopted from xendd.c in gf driver       *
 * J. Cowie/COMET	11/95	Strip off padding bytes from each line,	*
 *				free image structure			*
 * S. Jacobs/NCEP	 4/96	Changed calling sequence		*
 * S. Jacobs/NCEP	 4/96	Added mxcol to find the number of	*
 *				color entries				*
 * M. Linda/GSC		 6/96	Removed a left-over 'free' command.	*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		01/00	cwin->pixmaps -> cwin->pxms[curr_loop]	*
 * A. Hardy/GSC          2/01   Modified output file name		*
 * S. Chiswell/Unidata	 8/02	Modified for 16/24 bit depth		*
 * S. Jacobs/NCEP	 5/03	Fixed 16 bit error for line padding	*
 * T. Piper/SAIC	11/03	Check image->depth prior to destroying	*
 ***********************************************************************/
{
    XColor		colors[256];
    XImage		*image ;
    char		*tp1, *tp2 ;
    Window_str		*cwin;
    Pixmap		pxmap;
    int			ipxm, i, j, rbytes;
    char		tmpfil[200];
    unsigned char	xred[256], xgreen[256], xblue[256];
    unsigned char	*imgdatptr;

    FILE		*fp;
    int			icnt, exist, ier, ierr, mxcol, linbyte, nBpp;
    long		ilen;
    char		newfil[72], dummy[133];

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *	Set the number of colors in the color map.
     */
    mxcol = gemvis->map_entries;

    /*
     *	Store filename from input parameter, PNeilley 5/95
     */

    strcpy ( tmpfil, gemwindow[current_window].name );

    /*
     *	Write error message if the file cannot be opened.
     */
    icnt  = 0;
    exist = G_TRUE;
    while ( exist && icnt < 1000 )
    {
	if ( icnt == 0 ) 
	    sprintf ( newfil, "%s", tmpfil );
	else
	    sprintf ( newfil, "%s.%03d", tmpfil, icnt );

	cfl_inqr ( newfil, NULL, &ilen, dummy, &ierr );
	if  ( ierr != 0 )  exist = G_FALSE;
	icnt++;
    }

    fp = cfl_wopn ( newfil, &ier );
    if ( ier != G_NORMAL )  
    {
	*iret = -1;
	return;
    }

    for ( i = 0; i < MAX_WINDOW; i++ )
    {

	if  ( gemwindow[i].name[0] != '\0' )
	{

	    cwin  = &(gemwindow[i]);
	    ipxm  = cwin->curpxm[cwin->curr_loop];
	    pxmap = cwin->pxms[cwin->curr_loop][ipxm];

	    image = XGetImage( gemdisplay, pxmap, 0, 0,
			       (unsigned int)cwin->width,
			       (unsigned int)cwin->height,
			       AllPlanes, ZPixmap );

	    /*
	     *   Remove any padding bytes at the end of each line
	     */
	    nBpp = image->bits_per_pixel / 8;
	    linbyte = image->width * nBpp;

	    if ( image->bytes_per_line > linbyte )
	    {
		tp1 = tp2 = image->data;
		rbytes = image->bytes_per_line - linbyte;
		for ( j = 0; j < image->height; j++ )
	   	{
		    memcpy ( tp1, tp2, (size_t)linbyte);
		    tp1 += linbyte ;
		    tp2 += linbyte + rbytes ;
		}
	    }
	    

	    if(image->depth <= 8) {
	       /* 
	        *	Allocate The XColor Array Used With The Colormap
	        */
	       for( j=0; j<mxcol; j++ ) {
		   colors[j].pixel = (Pixel)j;
		   colors[j].flags = DoRed | DoGreen | DoBlue;
	       }

	       imgdatptr = (unsigned char *)image->data;
            }
            else {
		/*
		 * for 16/24 bit data, create an indexed pixmap
		 */
		int istart=0, negflg = 0, ivalue, ii, jj, inc=0, ifc;
		int nbanks,ir,il;
		unsigned char uctmp;

		nbanks = ColorBanks.nbank;

		for(ii = 0; ii < nbanks; ii++) {

		    if(!allocflag[ii]) continue;

		    for(jj = 0; jj<ColorBanks.banks[ii]; jj++) {
		        colors[inc].pixel = ColorBanks.colrs[ii][jj];
		        colors[inc].flags = DoRed | DoGreen | DoBlue;
		        inc++;
		    }
		}

		mxcol = inc;

		imgdatptr = (unsigned char *)malloc((size_t)(image->width * image->height));

	        for(ii = 0; ii < (image->width * image->height); ii++) {

		    if (image->bitmap_bit_order == LSBFirst) {
			/* need to flip bytes */
			il = 0; ir = nBpp - 1;
			while (il < ir) {
			    uctmp = image->data[istart + il]; 
			    image->data[istart + il] = image->data[istart + ir];
			    image->data[istart + ir] = uctmp;
			    il++;
			    ir--;
			}
		    }

		    mv_btoi ( (unsigned char *)(image->data),
		   	     &istart, &nBpp, &negflg, &ivalue, &ier);
		    istart += nBpp;


		    /* find value */
		    jj = 0; ifc = -1;
		    do {
			if ((Pixel)ivalue == colors[jj].pixel) 
			    ifc = jj;
			else
			    jj++;
		    } while ( ( jj < mxcol )&&(ifc == -1) );

		    if (ifc == -1)  ifc = 0;

		    imgdatptr[ii] = (unsigned char)ifc;
		}	
	    }

	    XQueryColors( gemdisplay, gemmap, colors, mxcol );

	    for( j=0; j<mxcol; j++ )
	    {
		xred[j] = (unsigned char)(colors[j].red >> 8);
		xblue[j] = (unsigned char)(colors[j].blue >> 8);
		xgreen[j] = (unsigned char)(colors[j].green >> 8);
	    }

	    WriteGIF( fp, imgdatptr, cwin->width,
		      cwin->height, xred, xgreen,
		      xblue, mxcol, 3 );

	    if(image->depth > 8) free(imgdatptr);
	    XDestroyImage ( image );
	}
    }
        cfl_clos (fp, iret);
}

