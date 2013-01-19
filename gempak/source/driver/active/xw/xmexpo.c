#include "xwcmn.h"

void xmexpo ( XEvent *event )
/************************************************************************
 * xmexpo								*
 *									*
 * This subroutine handles expose events for the motif window.		*
 *									*
 * xmexpo( event )							*
 *									*
 * Input parameters:							*
 *	*event		XEvent		event structure			*
 * Output parameters:							*
 **									*
 * Log:									*
 * C. Lin/EAI            5/96	from xxenvt.c 				*
 * C. Lin/EAI            6/97	use copy area info 			*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h                  *
 * S. Law/GSC		01/00	XCopyArea -> xpxm2win			*
 ***********************************************************************/
{
    int		ii, iret;
/*---------------------------------------------------------------------*/

    if(event->xexpose.count == 0) {

	/*
	 * Only redraw the areas of the window exposed.
	 */
	for (ii = 0; ii < MAX_WINDOW; ii++) {
	    if (event->xany.window == gemwindow[ii].window) { 
		break;
	    }
	}

	xpxm2win (-1, ii, &iret);
    }
}
