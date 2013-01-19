#include "xwcmn.h"

void xsplot ( int *iret )
/************************************************************************
 * xsplot								*
 *									*
 * This subroutine increases the pixmap pointer by 1 and creates the  	*
 * next pixmap if the incr_pxmCnt is set to TRUE (the default).		*
 *									*
 * xsplot  ( iret )							*
 *									*
 * Input parameters:							*
 *			None						*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 G_NORMAL = Normal return	*
 *					 G_NMAXFR = Too many frames	*
 **									*
 * Log:									*
 * C. Lin/EAI	         7/94	Multi-window & Multi-pixmap		*
 * C. Lin/EAI	         8/94	Added call to XCLEAR			*
 * C. Lin/EAI	         6/97	Use pixmap size,  use cwin		*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	02/99	use incr_pxmCnt to control pxm advance  *
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	cwin->pixmaps -> cwin->pxms[lp]		*
 * E. Safford/GSC	08/00	fix npxms increment error		*
 ***********************************************************************/
{
    int		ipxm, xwdth, xhght, xdpth, lp, ier;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    if ( gemwindow[current_window].npxms < MAX_PIXMAP ) {

	cwin	= &(gemwindow[current_window]);
	lp	= cwin->curr_loop;
	cloop	= &(cwin->loop[lp]);
	ipxm	= cwin->curpxm[lp];

	if (!cwin->nmap2 && cwin->incr_pxmCnt) {
	    /* 
	     *  advance the curpxm index
	     */
	    (cwin->curpxm[lp])++;
	    ipxm = cwin->curpxm[lp];
        }


	xwdth = cloop->pxm_wdth;
	xhght = cloop->pxm_hght;
	xdpth = cwin->depth;

	if (cwin->pxms[lp][ipxm] == (Pixmap) NULL) {
	    cwin->pxms[lp][ipxm] = XCreatePixmap(gemdisplay, root, xwdth,
						 xhght, xdpth); 	
	    xclrpxm( &ipxm, &ier);

        }

	if (cwin->nmap2 || cwin->incr_pxmCnt) {
	    cwin->npxms++;
	}


    }
    else {
	/*
	 * number of pixmaps exceeds maximum
	 */
	*iret = G_NMAXFR; 
    }

}
