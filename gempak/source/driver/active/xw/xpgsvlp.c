#include "xwcmn.h"

void xpgsvlp ( int *iret )
/************************************************************************
 * xpgsvlp                                                     		*
 *                                                                      *
 * This function saves the pxms of the current loop into the mstr array	*
 *  (in gemwindow).							*
 *									*
 * This routine is only for use in nmap2.              			*
 *                                                                      *
 * void xpgsvlp (iret)							*
 *                                                                      *
 * Input parameters:                                                    *
 *			NONE						*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int	0 = normal, -1 = error, no copy done	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/99	copied from xpgsvfrm			*
 * E. Safford/GSC	12/99	clean up				*
 * E. Safford/GSC	01/00	remove print statement			*
 * S. Law/GSC		01/00	added _pgpalwIsUp flag			*
 * E. Safford/GSC	03/00	don't set _pgpalwIsUp			*
 * S. Law/GSC		06/00	changed to use current loop		*
 ***********************************************************************/
{
    int		xdpth, ii, lp;		/* pixel depth of X window */

    Window_str	*cwin;
    winloop_t	*cloop; 
/*---------------------------------------------------------------------*/

    *iret     = 0;

    cwin      = &(gemwindow[0]); 
    lp        = cwin->curr_loop;
    cloop     = &(cwin->loop[lp]); 
    xdpth     = cwin->depth;

    for (ii=0; ii < MAX_PIXMAP; ii++) {
	/*
	 *  if the master exists then delete it
	 */
	if (cwin->mstr[ii] != (Pixmap) NULL) {
	    XFreePixmap(gemdisplay, cwin->mstr[ii]);
	    cwin->mstr[ii] = (Pixmap)NULL;
	}

	if ( cwin->pxms[lp][ii] != (Pixmap) NULL ) {
	    /*
	     *  create the master to the correct dimensions
	     */
	    cwin->mstr[ii] = XCreatePixmap(gemdisplay,
					   cwin->window, 
					   cloop->pxm_wdth, 
					   cloop->pxm_hght, 
					   xdpth);

	    /*
	     *  copy the pixmaps to the master copy
	     */
            XCopyArea (gemdisplay,
	       cwin->pxms[lp][ii],
	       cwin->mstr[ii],
	       cwin->gc,
	       0, 0, 
	       cloop->pxm_wdth, 
	       cloop->pxm_hght, 0, 0);
	}
	else {
	    break;
	}
    }
}

