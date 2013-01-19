#include "xwcmn.h"

void xclrpxm ( int *ipxm, int *iret )
/************************************************************************
 * xclrpxm								*
 *									*
 * This subroutine clears the specified pixmap. 			*
 *									*
 * xclrpxm  ( ipxm, iret )						*
 *									*
 * Input parameters:							*
 *	*ipxm		int	pixmap index				*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *			    G_NORMAL = normal return (no size change)	*
 **									*
 * Log:									*
 * C. Lin/EAI	         6/97						*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ityp, lp, ier;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/


    *iret = G_NORMAL;

    /*
     * Get the current window information.
     */
    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);

    /*
     * First, set the background to the current color.
     */
    ityp = 0;
    xscolr ( &ityp, &ibkcol, &ier );

    /*
     * Make X call to clear window.  The background color has been
     * set as the current foreground color.
     */
    lp = cwin->curr_loop;

    if (cwin->pxms[lp][*ipxm] != (Pixmap) NULL) {

	XFillRectangle (gemdisplay, cwin->pxms[lp][*ipxm], cwin->gc, 
			0, 0, cloop->pxm_wdth, cloop->pxm_hght); 
    }

    /*
     * Now, reset the current foreground color.
     */
    ityp = 0;
    xscolr( &ityp, &ifrcol, &ier );

}
