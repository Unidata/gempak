#include "xwcmn.h"

void xpgpaste ( float llx, float lly, float urx, float ury, int *iret )
/************************************************************************
 * xpgpaste  								*
 *                                                                      *
 * 	This function uses X calls to refresh a section of a pixmap	*
 *	from a master copy of the pixmap.				*
 *                                                                      *
 * xpgpaste( llx, lly, urx, ury, iret)					*
 *                                                                      *
 * Input parameters:                                                    *
 *	llx	float		Lower left X				*
 *	lly	float		Lower left Y				*
 *	urx	float		Upper right X				*
 *	ury	float		Upper right Y				*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret	int		Return code				*
 *                      NULL                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97   Created.                                *
 * E. Wehner/EAi	 9/97	Removed graphics info record		*
 * C. Lin/EAI	        10/97	Rename from drwpaste, cleanup		*
 * E. Safford/GSC	12/98	new refresh design			*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * E. Safford/GSC	05/99	update only the current loop		*
 * S. Law/GSC		06/00	mstr array only for current loop	*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * S. Jacobs/NCEP	 9/00	Added check for empty pixmaps		*
 ***********************************************************************/
{
    int 	ii, lp;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    *iret = 0;

    cwin	= &gemwindow[0];
    lp		= cwin->curr_loop;
    cloop	= &(cwin->loop[lp]);

    /*
     * Refresh the pixmap out of original data ... Increase the bounding
     * box by the size of the markers to make sure we delete them too.
     */
    XClearArea(gemdisplay, cwin->window,
                    (int)llx - cloop->xoffset, 
		    (int)lly - cloop->yoffset,
                    (unsigned int)(urx - llx), 
		    (unsigned int)(ury-lly),
                                 False);

    for ( ii=0; ii < MAX_PIXMAP; ii++ ) {
	/*
	 *  copy from the mstr to the pxms if mstr exists
	 */
	if ( ( cwin->mstr[ii] != (Pixmap) NULL ) ||
	     ( cwin->pxms[lp][ii] != (Pixmap) NULL ) ) {

	    XCopyArea (gemdisplay, 
		       cwin->mstr[ii],
		       cwin->pxms[lp][ii],
		       cwin->gc,
		       (int)llx, (int)lly,
		       (unsigned int)(urx - llx),
		       (unsigned int)(ury - lly),
		       (int)llx, (int)lly);

	    cwin->xw_rfrsh[lp][ii] = TRUE;
	}
	else {
	    break;
	} 	
    }
}
