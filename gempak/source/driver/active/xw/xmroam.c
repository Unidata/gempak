#include "xwcmn.h"

void xmroam_setPos ( int xx, int yy )
/************************************************************************
 * xmroam_setPos							*
 *									*
 * This subroutine is intended to be used by GUI application in mouse   *
 * track mode of roaming. It roams the current window (screen )  to     *
 * the specified position (upper left of the window) in 'D' coordinate  *
 * system.   								*
 *									*
 * xmroam_setPos ( xx, yy )						*
 *									*
 * Input parameters:							*
 *	xx	int		upper left x coordinate			*
 *	yy	int		upper left y coordinate			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	         8/97						*
 * S. Law/GSC		10/99	renamed from xmroam and added loop	*
 *				changes in gemwindow			*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		01/00	XCopyArea -> xpxm2win			*
 ***********************************************************************/
{
    int		xoffset_save, yoffset_save, ipxm, iret;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    /*
     * Get the current window information.
     */
    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);
    ipxm  = cwin->curpxm[cwin->curr_loop];

    xoffset_save = cloop->xoffset;
    yoffset_save = cloop->yoffset;

    if (cloop->roamflg) {

	cloop->xoffset = xx;
	cloop->yoffset = yy;

	if ( xarea() == -1 ) {
	    cloop->xoffset = xoffset_save; 
	    cloop->yoffset = yoffset_save; 
	}

	xpxm2win (ipxm, current_window, &iret);
    }
}

/*=====================================================================*/

void xmroam_getPos ( int *xx, int *yy )
/************************************************************************
 * xmroam_getPos							*
 *									*
 * This subroutine retrieves the current tracking position.		*
 *									*
 * xmroam_getPos (xx, yy)						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*xx	int		upper left x coordinate			*
 *	*yy	int		upper left y coordinate			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		10/99	initial coding				*
 ***********************************************************************/
{
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    /*
     * Get the current window information.
     */
    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);

    *xx = cloop->pxm_x;
    *yy = cloop->pxm_y;
}
