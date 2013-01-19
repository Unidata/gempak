#include "nccmn.h"

void mline ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * mline								*
 *									*
 * This subroutine draws lines to the metafile.				*
 *									*
 * mline  ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	ix[]		int		X coordinates 			*
 *	iy[]		int		Y coordinates 			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int		Return code                     *
 *				  G_NMDATA = Too much data for metafile	*
 **                                                                     *
 * Log:									*
 * C. Lin/EAI		10/92						*
 * C. Lin/EAI		11/92	Add Header Information			*
 * C. Lin/EAI		 4/93	Add internal buffer			*
 * A. Chang/EAI          1/94	Modified to buffered I/O        	*
 * A. Chang/EAI          4/94						*
 * A. Chang/EAI          4/94	Elimated scaling code	        	*
 * S. Jacobs/NMC	 6/94	General clean up			*
 * L. Williams/EAI	 7/94	Reformat header				*
 * M. Linda/GSC		 6/96	Corrected check for too many points	*
 * A. Hardy/GSC	 	12/98	Change mcirc to mdots                   *
 ***********************************************************************/
{
	short	buffer[2048];
	short	colbuf[2];
	short	widbuf[2];
	short	nbyte;
	int	numbyte;
	int	i, k, ibyt, ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Check for too many points.  Point count must fit into a short.
 */
	numbyte = (*np) * 2 * NUM_BTSZ;

	if ( numbyte > MAX_TWOBT ) {
	    *iret = G_NMDATA;
	    return;
	}

/*
 *	Open the file, if necessary.
 */
	if ( ! opnfil ) {
	    mopen ( &ier );
	    if ( ier != G_NORMAL ) {
		*iret = ier;
		return;
	    }
	}

/*
 *	Set line color.
 */
	if ( lcolor_req != lcolor_set ) {
	    lcolor_set = lcolor_req;

	    nbyte = 1 * NUM_BTSZ;
	    colbuf[0] = LINE_COLOR | nbyte;
	    colbuf[1] = (short) lcolor_set;
	    colbuf[1] <<= 8;

	    fwrite ( colbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

/*
 *	Set line width.
 */

	if  ( lwidth_req != lwidth_set ) { 
	    lwidth_set = lwidth_req;

	    nbyte = 1 * NUM_BTSZ;
	    widbuf[0] = LINE_WIDTH | nbyte;
	    widbuf[1] = (short) lwidth_set;

	    fwrite ( widbuf, NUM_BTSZ, 2, meta_fp );

	    byte_count += NUM_BTSZ * 2;
	}

	if ( ( *np == 2 ) &&
	     ( ( ix[0] == ix[1] ) && ( iy[0] == iy[1] ) ) ) {
/*
 *	    Draw a circle.
 */
	    mdots ( ix, iy, &lwidth_set, &ier );

	}
	else {
/*
 *	    Draw a line.
 */
	    if ( numbyte < 31 ) {
	        buffer[0] = LINE | (short) numbyte;
	        ibyt = 1;
	    }
	    else {
	        buffer[0] = LINE | LONG_FORM;
	        buffer[1] = (short) numbyte;
	        ibyt = 2;
	    }

	    k = ibyt;
	    for ( i = 0; i < (*np); i++ ) {
	        buffer[k] = (short) ix[i];
	        k++;

	        buffer[k] = (short) iy[i];
	        k++;

	        if ( k >= 2048 ) {
		    fwrite ( buffer, NUM_BTSZ, k, meta_fp );
		    k = 0;
	        }
	    }

	    if ( k > 0 ) {
	        fwrite ( buffer, NUM_BTSZ, k, meta_fp );
	    }

	    byte_count += ( numbyte + ibyt * NUM_BTSZ );
	}

}
