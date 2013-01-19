#include "xwcmn.h"

void xpgsvfrm ( int fn )
/************************************************************************
 * xpgsvfrm                                                   		*
 *                                                                      *
 * This function saves frames into backup pixmaps.       		*
 *                                                                      *
 * void xpgsvfrm ( fn )  						*
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
 * E. Wehner/EAI         4/97    Created                         	*
 * C. Lin/EAI            8/97    Use pixmap size instead of window size *
 *				 add XFreePixmap and checking		*
 * E. Wehner/EAi	 9/97	 Remove graphics info record		*
 * C. Lin/EAI	        10/97	 rename from NxmDraw_saveframe, cleanup	*
 * C. Lin/EAI	        01/98	 recompute range record for frame 0 only*
 * I. Durham/GSC	 5/98	 changed underscore decl. to an include	*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		01/00	added _pgpalwIsUp flag			*
 * E. Safford/GSC	03/00	don't set _pgpalwIsUp value		*
 * S. Law/GSC		08/00	pixmaps/master -> pxms[lp]/mstr		*
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		11/01	remove unnecessary crg_init call	*
 * J. Wu/SAIC		12/01	add _pgLayer in cvg_load() call		*
 * J. Wu/SAIC		12/01	replace cvg_load() with cvg_redraw()	*
 ***********************************************************************/
{
    int		xdpth;		/* pixel depth of X window */
    int		ipxm, istat, lp;

    static int	wdth_save = 0; /* last size of master pixmaps */
    static int	hght_save = 0;

    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin	= &(gemwindow[0]); 
    lp		= cwin->curr_loop;
    cloop	= &(cwin->loop[lp]);
    xdpth	= cwin->depth;

    if ( (wdth_save != cloop->pxm_wdth) ||
	 (hght_save != cloop->pxm_hght) ) {

	if ( cwin->mstr[fn] ) {
	    XFreePixmap(gemdisplay, cwin->mstr[fn]);
	    cwin->mstr[fn] = (Pixmap)NULL;
	}

    }

    if (cwin->mstr[fn] == (Pixmap)0) {

	cwin->mstr[fn] = XCreatePixmap(gemdisplay,
				       cwin->window, 
				       cloop->pxm_wdth, 
				       cloop->pxm_hght, 
				       xdpth);

	wdth_save = cloop->pxm_wdth;
	hght_save = cloop->pxm_hght;
    }

    XCopyArea (gemdisplay,
	       *(cwin->pxms[lp] + fn),
	       cwin->mstr[fn],
	       cwin->gc,
	       0, 0, 
	       cloop->pxm_wdth, 
	       cloop->pxm_hght, 0, 0);
    
    ipxm = cwin->curpxm[cwin->curr_loop];
    cwin->curpxm[cwin->curr_loop] = fn;

    if ( fn == 0 ) crg_rebuild( );
    
    cvg_redraw(NULL, &istat);

    cwin->curpxm[cwin->curr_loop] = ipxm;
}
