#include "xwcmn.h"

void xcpypxm2 ( int lp, int from, int to, int *iret)
/************************************************************************
 * xcpypxm2								*
 * 									*
 * This function copies the source pixmap into the destination pixmap.  *
 * The input parameters are the pixmap index numbers in the 		*
 * specified loop.  This routine should only be used by nmap2.        	*
 *									*
 * void xcpypxm2 ( lp, from, to, iret )					*
 *									*
 * Input parameters:							*
 *	lp	int	loop containing the from and to pixmaps		*
 *      from	int	index of source pixmap				*
 *	to	int	index of destination pixmap			*
 *									*
 * Output parameters:							*
 *	*iret	int	return value					*
 *				  0 = normal				*
 *				 -1 = invalid pixmap index		*
 **									*
 * Log:									*
 * E. Safford/GSC	12/99	copied from xcpypxm                  	*
 * S. Law/GSC		01/00	added _pgpalwIsUp flag			*
 * S. Law/GSC		06/00	mstr array only for current loop	*
 * E. Safford/SAIC	08/01	check & create pxm if to || from is NULL*
 * J. Wu/SAIC		12/01	cast cwin->mstr[] for comparison   	*
 ***********************************************************************/
{
    int		xdpth;
    Window_str	*cwin;
    winloop_t	 *cloop;
/*---------------------------------------------------------------------*/


    cwin = &gemwindow[current_window];
    cloop = &(cwin->loop[lp]);
    xdpth = cwin->depth;

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

  	if (_pgpalwIsUp && cwin->curr_loop == lp) {

	    if ( (void*)cwin->mstr[from] == NULL) { 
	        cwin->mstr[from] = XCreatePixmap(gemdisplay,
                                    cwin->window,
			            cloop->pxm_wdth,
			            cloop->pxm_hght,
			            xdpth);
	    }
	
	    if ( (void*)cwin->mstr[to] == NULL) { 
	        cwin->mstr[to] = XCreatePixmap(gemdisplay,
                                    cwin->window,
			            cloop->pxm_wdth,
			            cloop->pxm_hght,
			            xdpth);
	    }

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
