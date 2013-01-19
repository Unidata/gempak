#include "xwcmn.h"

void xclpnl ( int *ix1, int *iy1, int *ix2, int *iy2, int *iret )
/************************************************************************
 * xclpnl								*
 *									*
 * This subroutine clears a panel in the current X Window.		*
 *									*
 * xclpnl  ( ix1, iy1, ix2, iy2, iret )					*
 *									*
 * Input parameters:							*
 *	*ix1		int		First X value			*
 *	*iy1		int		First Y value			*
 *	*ix2		int		Second X value			*
 *	*iy2		int		Second Y value			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 2/97	Copied from XCLEAR			*
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		 ityp, ier, ipxm, lp;
    unsigned int ixdif, iydif;
    GC		 gemgc; 
    Window_str	 *cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *  Get the current window information.
     */
    cwin  = &(gemwindow[current_window]);
    ipxm  = cwin->curpxm[cwin->curr_loop]; 
    gemgc = cwin->gc; 

    /*
     *  First, set the background to the current color.
     */
    ityp = 0;
    xscolr ( &ityp, &ibkcol, &ier );

    /*
     *  Make X call to clear the panel.  The background color
     *  has been set as the current foreground color.
     */
    ixdif = *ix2 - *ix1;
    iydif = *iy1 - *iy2;

    lp = cwin->curr_loop;
    XFillRectangle (gemdisplay, cwin->pxms[lp][ipxm],
		    gemgc, *ix1, *iy2, ixdif, iydif); 

    XSync ( gemdisplay, False );

    /*
     *  Now, reset the current foreground color.
     */
    ityp = 0;
    xscolr ( &ityp, &ifrcol, &ier );
}
