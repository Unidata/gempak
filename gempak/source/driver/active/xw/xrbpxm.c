#include "xwcmn.h"

void xrbpxm ( int idx, int lp )
/************************************************************************
 * xrbpxm 								*
 * 									*
 * This function rebuilds a specific pixmap in the current window's     *
 * array of pixmaps.  The pixmap is first freed if (if it had been      *
 * created) and then created.  The net result is a pixmap that is       *
 * correctly sized to the designated loop.                              *
 *									*
 *  NOTE:  This routine is only for use in nmap2.  			*
 *									*
 * void xrbpxm ( idx, lp )						*
 *									*
 * Input parameters:							*
 *      idx	int	index of pixmap					*
 *	lp	int	loop to which the pixmap belongs		*
 *									*
 * Output parameters:							*
 *		NONE							*
 **									*
 * Log:									*
 * E. Safford/GSC	11/99	initial coding 				*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 ***********************************************************************/
{
Window_str	*cwin;
winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin = &gemwindow[current_window];
    cloop = &(cwin->loop[lp]);


    if (cwin->pxms[lp][idx] != (Pixmap) NULL) {
        XFreePixmap   ( gemdisplay, cwin->pxms[lp][idx] );
	cwin->pxms[lp][idx] = (Pixmap) NULL;
	cwin->npxms--;
    }

    cwin->pxms[lp][idx] = XCreatePixmap ( gemdisplay, root, 
    			cloop->pxm_wdth, cloop->pxm_hght, cwin->depth);

    cwin->npxms++;

}
