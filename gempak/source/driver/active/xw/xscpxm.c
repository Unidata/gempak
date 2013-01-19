#include "xwcmn.h"


void xscpxm ( Pixel ipxm, int *iret )
/************************************************************************
 * xscpxm								*
 *									*
 * This subroutine sets the current pixmap pointer to the specified     *
 * pixmap index 'ipxm'.  Unlike xg2pxm, this routine does not change	*
 * the displayed pixmap.  						*
 * 									*
 * xscpxm( ipxm, iret )							*
 *									*
 * Input parameters:							*
 *	ipxm		Pixel		index to the pixmap		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 G_NORMAL = Normal return	*
 *					 -1 = invalid pixmap index	*
 **									*
 * Log:									*
 * E. Safford/GSC	09/99	initial coding                          *
 * E. Safford/GSC	10/99	remove check on number of pixmaps       *
 * E. Safford/GSC	12/99	clean up				*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 ***********************************************************************/
{
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin  = &(gemwindow[current_window]);

    if ( ipxm > (Pixel)MAX_PIXMAP ) {
	*iret = -1;
        return;
    }

    cwin->curpxm[cwin->curr_loop] = (int)ipxm;
}
