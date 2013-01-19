#include  "xwcmn.h"

void xpoint  ( int *ix, int *iy, int *iret )
/************************************************************************
 * xpoint								*
 *									*
 * This subroutine draws a single point in an X window.  It should	*
 * be used to plot dots when the line width is 1.			*
 *									*
 * xpoint  ( ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*ix		int		X coordinate			*
 *	*iy		int		Y coordinate			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * M. desJardins/NMC	01/92	GEMPAK 5.1				*
 * J. Whistler/SSAI	04/92	Draw to pixmap instead of window (sj)	*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	multi-window, multi-pixmap		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ipxm, lp;      
    Pixmap	gempixmap; 
    GC		gemgc;     
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin	= &(gemwindow[current_window]);
    lp		= cwin->curr_loop;
    ipxm	= gemwindow[current_window].curpxm[lp]; 
    gemgc	= gemwindow[current_window].gc; 

    gempixmap = cwin->pxms[lp][ipxm];

    /*
     * Draw point.
     */

    XDrawPoint  ( gemdisplay, gempixmap, gemgc, *ix, *iy );

}
