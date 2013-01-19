#include "xwcmn.h"

void xpgrestlp ( void )
/************************************************************************
 * xpgrestlp								*
 * 									*
 * This function copies the master pixmaps for the current loop into	*
 * the loop's pixmap array.						*
 *									*
 * void xpgrestlp ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/GSC	12/99	initial coding        			*
 * S. Law/GSC		01/00	added check for pxms before copying	*
 * S. Law/GSC		01/00	added _pgpalwIsUp flag			*
 * E. Safford/GSC	03/00	don't set _pgpalwIsUp value		*
 * S. Law/GSC		06/00	changed to use current loop		*
 ***********************************************************************/
{
    int 	ii, lp;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin   = &gemwindow[current_window];
    cloop  = &(cwin->loop[cwin->curr_loop]);
    lp     = cwin->curr_loop;

    /*
     *  Copy any existing master pixmaps to the loop's pixmap array.
     */
    for (ii=0; ii < MAX_PIXMAP; ii++) {
        if (cwin->mstr[ii] != (Pixmap) NULL &&
	    cwin->pxms[lp][ii] != (Pixmap) NULL) {
            XCopyArea (gemdisplay,
	       cwin->mstr[ii], cwin->pxms[lp][ii],
	       cwin->gc, 0, 0, 
	       cloop->pxm_wdth, cloop->pxm_hght,
	       0, 0);
        }
	else {
	    break;
	}
    }
}
