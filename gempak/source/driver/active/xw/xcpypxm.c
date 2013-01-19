#include "xwcmn.h"

void xcpypxm ( int from, int to, int *iret )
/************************************************************************
 * xcpypxm								*
 * 									*
 * This function copies the source pixmap into the destination pixmap.  *
 * The input parameters are the pixmap index numbers in the 		*
 * current_window display.  This routine should only be used by        	*
 * product generation in nmap.						*
 *									*
 * void xcpypxm ( from, to, iret )					*
 *									*
 * Input parameters:							*
 *      from	int	index of source pixmap				*
 *	to	int	index of destination pixmap			*
 *									*
 * Output parameters:							*
 *	*iret	int	return value					*
 *				  0 = normal				*
 *				 -1 = invalid pixmap index		*
 **									*
 * Log:									*
 * E. Safford/GSC	02/99	initial coding 				*
 * E. Safford/GSC	03/99	add copy of master if in product gen.	*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * S. Law/GSC		01/00	added _pgpalwIsUp flag			*
 * S. Law/GSC		08/00	pixmaps/master -> pxms[lp]/mstr		*
 ***********************************************************************/
{
    int		lp;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin = &gemwindow[current_window];
    cloop = &(cwin->loop[cwin->curr_loop]);
    lp = cwin->curr_loop;    

    /*
     *  Copy each of the saved frames to the current window.
     */
 
    if ( cwin->pxms[lp][from] && cwin->pxms[lp][to] ) {  

	XCopyArea (gemdisplay, 
		   cwin->pxms[lp][from],
		   cwin->pxms[lp][to],
		   cwin->gc, 0, 0,
		   cloop->pxm_wdth,
		   cloop->pxm_hght, 0, 0);

	if (_pgpalwIsUp) {
	    XCopyArea (gemdisplay, 
		       cwin->mstr[from],
		       cwin->mstr[to],
		       cwin->gc, 0, 0,
		       cloop->pxm_wdth,
		       cloop->pxm_hght, 0, 0);
	}

	*iret = G_NORMAL;
    }
    else {
        *iret = -1;
    }
}
