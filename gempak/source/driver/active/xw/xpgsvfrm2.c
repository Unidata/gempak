#include "xwcmn.h"

void xpgsvfrm2 ( int fn )
/************************************************************************
 * xpgsvfrm2                                                  		*
 *                                                                      *
 * This function saves the specified pixmap frame of the current loop	*
 * into the corresponding master pixmap.				*
 *                                                                      *
 * void xpgsvfrm2 (fn)  						*
 *                                                                      *
 * Input parameters:                                                    *
 *      fn              int             Frame # to save                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	12/99	copied from xpgsvfrm.c               	*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		06/00	changed to use current loop		*
 ***********************************************************************/
{
    int		ipxm, xdpth, lp;

    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin  = &(gemwindow[0]); 
    cloop = &(cwin->loop[cwin->curr_loop]);
    lp    = cwin->curr_loop;
    xdpth = cwin->depth;

    /*
     *  If the master exists, delete it.
     */
    if ( cwin->mstr[fn] != (Pixmap) NULL ) {
	    XFreePixmap(gemdisplay, cwin->mstr[fn]);
	    cwin->mstr[fn] = (Pixmap)NULL;
    }


    /*
     *  Now create mstr and copy the pxms into it.  Since mstr was 
     *  deleted above, the dimensions must be correct for the pixmaps 
     *  in this loop.
     */
    cwin->mstr[fn] = XCreatePixmap(gemdisplay,
				 cwin->window, 
				 cloop->pxm_wdth, 
				 cloop->pxm_hght, 
				 xdpth);

    XCopyArea (gemdisplay,
	       cwin->pxms[lp][fn],
	       cwin->mstr[fn],
	       cwin->gc,
	       0, 0, 
	       cloop->pxm_wdth, 
	       cloop->pxm_hght, 0, 0);
    
    ipxm = cwin->curpxm[cwin->curr_loop];
    cwin->curpxm[cwin->curr_loop] = fn;

    cwin->curpxm[cwin->curr_loop] = ipxm;
}

