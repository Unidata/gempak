#include "xwcmn.h"

void xputpxms ( Pixmap *pixmaps )
/************************************************************************
 * xputpxms								*
 * 									*
 * This function put (copy ) each of the input source pixmaps into the  *
 * corresponding pixmaps in the current window. It assumes that the     *
 * input source pixmaps have the same width, height, and more important *
 * depth, with the pixmaps saved in the current window structure.  	*
 *									*
 *    NOTE:  With the 12/99 change to xwcmn.h, this routine should only *
 * be used by nmap.  Nmap2 now uses xpgrestlp.				*
 *								 	*
 * void xputpxms (pixmaps)						*
 *									*
 * Input parameters:							*
 *      *pixmaps             Pixmap        array of source pixmaps      *
 *									*
 * Output parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * C. Lin/EAI		 8/97						*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * S. Law/GSC		01/00	added _pgpalwIsUp flag			*
 * E. Safford/GSC	03/00	moved _pgpalwIsUp to explicit set func  *
 * S. Law/GSC		08/00	cwin->pixmaps -> cwin->pxms[lp]		*
 ***********************************************************************/
{
    int 	ii, lp;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin	= &gemwindow[current_window];
    lp		= cwin->curr_loop;
    cloop	= &(cwin->loop[lp]);

    /*
     *  Copy each of the saved frames to the current window.
     */
    for ( ii = 0; ii < cwin->npxms; ii++ ) {
	if ( pixmaps[ii] ) {
	    XCopyArea (gemdisplay,
		       pixmaps[ii], *(cwin->pxms[lp] + ii),
		       cwin->gc, 0, 0, 
		       cloop->pxm_wdth, cloop->pxm_hght,
		       0, 0);
	}
    }
}
