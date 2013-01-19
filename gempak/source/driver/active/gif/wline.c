#include "gifcmn.h"

void wline ( int *npt, int *xpt, int *ypt, int *iret )
/************************************************************************
 * wline								*
 *									*
 * This function draws a line to a GIF image. This is essentially an	*
 * unfilled and unclosed polygon.					*
 *									*
 * wline ( npt, xpt, ypt, iret )					*
 *                                                                      *
 * Input parameters:                                         		*
 *  *npt		int     no of points                            *
 *  *xpt		int     x coordinates                           *
 *  *ypt 		int     y coordinates                           *
 *									*
 * Output parameter:                                                   *
 *  *iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * Dan Austin		 5/96						*
 * T. Lee/GSC		 7/00	Cleaned up				*
 ***********************************************************************/
{
    int ii;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Continue for two or more points.
     */

    if ( *npt >= 2 ) { 
	
	/*
	 * Draw a series of line segments.
	 */

	for ( ii = 0; ii < *npt-1; ii++) {
	    gdImageLine (Current_Im, xpt[ii], ypt[ii], 
			 xpt[ii+1], ypt[ii+1], CurrentColorIndex);
	}
    }
}
