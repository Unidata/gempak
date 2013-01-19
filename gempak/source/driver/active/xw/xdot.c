#include "xwcmn.h"

void xdot ( int *ix, int *iy, int *ilwid, int *iret )
/************************************************************************
 * xdot									*
 *									*
 * This subroutine draws circles.  It is used to draw round dots in an	*
 * X window.								*
 *									*
 * xdot ( ix, iy, ilwid, iret )						*
 *									*
 * Input parameters:							*
 *	*ix		int		X coordinate			*
 *	*iy		int		Y coordinate			*
 *	*ilwid		int		Current line width		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *              			G_NORMAL = normal return.       *
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for XW device driver		*
 * M. desJardins/NMC	12/91	xfcirc --> xcirc; center circle		*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	multi-window, multi-pixmap		*
 * A. Hardy/GSC		11/98   Renamed xcirc to xdot                   *
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ixnew, iynew, ipxm, lp;
    Window_str	*cwin;
    GC		gemgc; 
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    cwin = &(gemwindow[current_window]);

    /*
     * Transform x and y to the upper left corner of bounding
     * rectangle.
     */
    ixnew = *ix - *ilwid / 2;
    iynew = *iy - *ilwid / 2;

    /*
     * Fill in entire circle.  Note that start and end angles of
     * arc are in 64ths of degrees.
     */
    ipxm  = gemwindow[current_window].curpxm[cwin->curr_loop]; 
    gemgc = gemwindow[current_window].gc;

    lp = cwin->curr_loop;
    XFillArc (gemdisplay, cwin->pxms[lp][ipxm], gemgc,
	      ixnew, iynew, *ilwid, *ilwid, 0, 360*64); 
}
