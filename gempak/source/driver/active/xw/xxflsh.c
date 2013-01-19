#include "xwcmn.h"

void xxflsh ( int *raise, int *iret )
/************************************************************************
 * xxflsh								*
 *									*
 * This subroutine flushes the buffers in the X window. It should only	*
 * be called from HEPLOT or its equivalent.				*
 *									*
 * xxflsh ( raise, iret )						*
 *									*
 * Input parameters:							*
 *	*raise		int		Raise flag 			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * M. desJardins/NMC	01/92	xflush-->xxflsh				*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window, multi-pixmap		*
 * C. Lin/EAI		 8/94	Changed calling seq. to add raise flag	*
 * C. Lin/EAI		 6/97	Consider 'S' coordinates		*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		01/00	XCopyArea -> xpxm2win			*
 ***********************************************************************/
{
    int		ipxm, ier;
    Window	gwin;

    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    cwin	= &(gemwindow[current_window]);
    gwin	= cwin->window;
    ipxm	= cwin->curpxm[cwin->curr_loop];

    if  ( *raise )  {
	XMapRaised ( gemdisplay, gwin );
    }
    else {
	XMapWindow ( gemdisplay, gwin );
    }

    xpxm2win (ipxm, current_window, &ier);
}
