#include "xwcmn.h"

void xpxm2win ( int src, int dest, int *iret )
/************************************************************************
 * xpxm2win								*
 *									*
 * This subroutine copies the pixmap at index src to the gemwindow at	*
 * index dest and makes src the current pixmap.  If either src or dest	*
 * is -1, then the current pixmap/window is used.			*
 *									*
 * xpxm2win (src, dest, iret)						*
 *									*
 * Input parameters:							*
 *	src		int	index to the pixmap			*
 *	dest		int	index to the window			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				 G_NORMAL = Normal return		*
 *				 -1 = invalid pixmap index		*
 *				 -2 = invalid window index		*
 **									*
 * Log:									*
 * S. Law/GSC		01/00	initial coding				*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * E. Safford/GSC	05/01	use dest window, not current window     *
 ***********************************************************************/
{
    Window_str	*dwin;
    winloop_t	*dloop;
    Pixmap	dpix;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;


    if (dest == -1) {
	dest = current_window;
    }
    else if (dest < 0 || dest >= MAX_WINDOW) {
	*iret = -2;
	return;
    }


    dwin  = &(gemwindow[dest]);
    dloop = &(dwin->loop[dwin->curr_loop]);

    if (src == -1) {
        src = dwin->curpxm[dwin->curr_loop]; 
    }
    else {
	xscpxm (src, iret);
	if (*iret < G_NORMAL) { 
	    return;
	}
    }

    if (*iret == G_NORMAL) {

	dpix = dwin->pxms[dwin->curr_loop][src];

	if (dpix != (XID)0 && dwin->window != (XID)0) {
	    XCopyArea (gemdisplay, dpix, dwin->window,
		       dwin->gc, dloop->pxm_x, dloop->pxm_y, 
		       dwin->area_w, dwin->area_h,
		       dwin->win_x, dwin->win_y);
	}

        XFlush(gemdisplay);
    }

}
