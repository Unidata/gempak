#include "xwcmn.h"


void xline ( int *npt, int *ix, int *iy, int *iret )
/************************************************************************
 * xline								*
 *									*
 * This subroutine draws line segments to an X window.			*
 *									*
 * xline  ( npt, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*npt		int		Number of points		*
 *	*ix		int		X coordinates			*
 *	*iy 		int		Y coordidnates			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C Call for X device driver		*
 * M. desJardins/NMC	12/91	xdrwpt-->xline				*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	multi-window, multi-pixmap		*
 * C. Lin/EAI	         2/95	add break;				*
 * M. Linda/GSC		 6/96	Added test for 'points' buffer overflow	*
 * M. Linda/GSC		 8/96	Fixed test for 'points' buffer overflow	*
 * A. Hardy/GSC		11/98   Renamed xcirc to xdot                   *
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ii, jj, kk, plot, lp, ipxm, ier;
    XPoint	points [128];
    Pixmap	gempixmap;  
    GC		gemgc;      
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    plot  = G_FALSE;

    cwin      = &(gemwindow[current_window]);
    ipxm      = gemwindow[current_window].curpxm[cwin->curr_loop]; 
    gemgc     = gemwindow[current_window].gc; 

    lp = cwin->curr_loop;	
    gempixmap = cwin->pxms[lp][ipxm];

    /*
     * Return for fewer than two points.
     */

    if  ( *npt < 2 )  return;

    /*
     * Check that this is not a single point.
     */

    for ( jj = 1; jj < *npt; jj++ ) {
	if  ( ( ix[jj] != ix[jj-1] ) || ( iy[jj] != iy[jj-1] ) ) {
	    plot = G_TRUE;
	    break;
	}
    }

    if  ( ( plot ) || ( line_width <= 0 ) ) {

	/*
	 * Draw a line.
	 */

	kk = 0;
	for ( ii = 0; ii < *npt; ii++ ) {
	    points [kk].x = ix [ii];
	    points [kk].y = iy [ii];
	    kk++;

	    if ( kk >= 128 ) {
		XDrawLines ( gemdisplay, gempixmap, gemgc, points, kk,
			     CoordModeOrigin );
		kk = 0;
		ii--;
	    }
	}

	if ( kk > 0 ) {
	    XDrawLines ( gemdisplay, gempixmap, gemgc, points, kk,
			 CoordModeOrigin );
	}
    }
    else if ( line_width <= 2 ) {

	/*
	 * Draw a point.
	 */

	xpoint ( &ix[0], &iy[0], &ier );

    }
    else {

	/*
	 * Draw a filled circle.
	 */
	xdot ( &ix[0], &iy[0], (int*)&line_width, &ier );

    }

}
