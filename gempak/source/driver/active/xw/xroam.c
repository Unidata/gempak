#include "xwcmn.h"

void xroam ( int *ityp, float *xx, float *yy, int *ix, int *iy, 
							int *iret )
/************************************************************************
 * xroam								*
 *									*
 * This subroutine roams the current window to the specified position   *
 * in any coordinate system except 'S'. The base point of the roam can  *
 * be upper left of the screen or the center of the screen.             *
 *									*
 * xroam  (ityp, xx, yy, ix, iy, iret)					*
 *									*
 * Input parameters:							*
 *      *ityp   int             the base point of roam          	*
 *                                        0 = upper left screen corner  *
 *                                        1 = center of the screen      *
 *                                        2 = delta_x, delta_y      	*
 *	*xx	float		upper left x coordinate			*
 *	*yy	float		upper left y coordinate			*
 *									*
 * Output parameters:							*
 *	*ix	int		upper left x coordinate			*
 *	*iy	int		upper left y coordinate			*
 *	*iret		int		Return code			*
 *			    G_NORMAL = normal return (no size change)	*
 **									*
 * Log:									*
 * C. Lin/EAI	         6/97						*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		01/00	XCopyArea -> xpxm2win			*
 ***********************************************************************/
{
    int		xpos, ypos, cx, cy, dx, dy, ipxm, ier;
    int		xoffset_save, yoffset_save;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/


    *iret = G_NORMAL;

    xpos = (int) *xx;
    ypos = (int) *yy;

    /*
     * Get the current window information.
     */
    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);
    ipxm  = cwin->curpxm[cwin->curr_loop];

    xoffset_save = cloop->xoffset;
    yoffset_save = cloop->yoffset;

    if (cloop->roamflg) {

	switch ( *ityp ) {

	  case 0:
	    cloop->xoffset = xpos;
	    cloop->yoffset = ypos;

	    break;

	  case 1:

	    /*
	     * calculate the current center position
	     * of the window
	     */
	    cx = cwin->width/2  + cloop->xoffset;
	    cy = cwin->height/2 + cloop->yoffset;

	    /*
	     * calculate the dx, dy
	     */
	    dx = xpos - cx;
	    dy = ypos - cy;

	    cloop->xoffset += dx;
	    cloop->yoffset += dy;

	    break;

	  case 2:
	    cloop->xoffset += xpos;
	    cloop->yoffset += ypos;

	    break;

	  default:
	    cloop->xoffset = xpos;
	    cloop->yoffset = ypos;

	    break;
	}

	XClearWindow( gemdisplay, cwin->window );

	if ( xarea() == -1 ) {
	    *iret = G_NROAM;
	    cloop->xoffset = xoffset_save; 
	    cloop->yoffset = yoffset_save; 
	}

	xpxm2win (ipxm, current_window, &ier);
    }

    *ix = cloop->xoffset;
    *iy = cloop->yoffset;

}
