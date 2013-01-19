#include "xwcmn.h"
#include "stipple.h"

void xfill ( int *npt, int *ix, int *iy, int *iret )
/************************************************************************
 * xfill								*
 *									*
 * This subroutine draws a filled polygon.				*
 *									*
 * xfill ( npt, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*npt		int		Number of points		*
 *	*ix 		int		X coordinates			*
 *	*iy 		int		Y coordinates			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * M. desJardins/NMC	01/92	GEMPAK 5.1				*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI		 7/94	multi-window, multi-pixmap		*
 * M. Linda/GSC		 2/97	points[128] to points[LLMXPT]   	*
 * S. Jacobs/NCEP	 3/98	Added fill patterns			*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * J. Wu/SAIC		06/05	remove reference to LLMXPT		*
 ***********************************************************************/
{
    int		ii, lp;
    int		npts;
    int		ipxm;
    XPoint	*points;
    Pixmap	gempixmap, stipple;
    GC		gemgc;
    Window	gwin;
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret	= G_NORMAL;
    cwin	= &(gemwindow[current_window]);
    ipxm	= cwin->curpxm[cwin->curr_loop];
    gemgc	= cwin->gc;
    gwin	= cwin->window;
    lp		= cwin->curr_loop;
    gempixmap	= cwin->pxms[lp][ipxm];

    /*
     *	Return for fewer than three points.
     */
    if ( *npt <= 2 ) return;

    /*
     *	Allocate memory.
     */
    npts = *npt;
    G_MALLOC ( points, XPoint, npts, "XFILL" );

    /*
     *	Move the ix and iy arrays into the point structure.
     */
    for ( ii = 0; ii < npts; ii++ ) {
	points [ii].x = ix [ii];
	points [ii].y = iy [ii];
    }

    /*
     *	Set the fill type.
     */
    if  ( kfillt == 1 )  {
	XSetFillStyle ( gemdisplay, gemgc, FillSolid );
    }
    else {
	stipple = 
	    XCreatePixmapFromBitmapData (gemdisplay,
					 gwin, (char *)stipple_bits[kfillt-1],
					 STIPPLE_WIDTH, STIPPLE_HEIGHT, 
					 1, 0, 1 );
	XSetStipple ( gemdisplay, gemgc, stipple );
	XSetFillStyle ( gemdisplay, gemgc, FillStippled );
    }

    XFillPolygon ( gemdisplay, gempixmap, gemgc, points, npts,
		   Complex, CoordModeOrigin );

    if  ( kfillt != 1 )  {
	XSetFillStyle ( gemdisplay, gemgc, FillSolid );
	if  ( stipple != (Pixmap)NULL )  {
	    XFreePixmap ( gemdisplay, stipple );
	}
    }

    G_FREE ( points, XPoint );
}
