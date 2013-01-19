#include "xwgui.h"


void xenanm ( int *iret )
/************************************************************************
 * xenanm								*
 *									*
 * This subroutine marks the end of an animation by freeing all the 	*
 * extra pixmaps.							*
 *									*
 * xenanm  ( iret )							*
 *									*
 * Input parameters:							*
 *			None						*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *			G_NORMAL = normal return 			*
 **									*
 * Log:									*
 * C. Lin/EAI	         7/94	Multi-window & Multi-pixmap		*
 * C. Lin/EAI	         2/95	add break; clean up the comments	*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	12/99	update for nmap2			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	cwin->pixmaps -> cwin->pxms[lp]		*
 * S. Jacobs/NCEP	 9/00	Added free of master pixmaps		*
 * T. Piper/GSC		 3/01	Removed xwcmn.h, in xwgui.h		*
 ***********************************************************************/
{
    int		jj, start, lp;
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;
    cwin =  &(gemwindow[current_window]);
    lp = cwin->curr_loop;    

    if ( !cwin->nmap2 ) {
	start = (cwin->curpxm[cwin->curr_loop]) + 1;

	/*
	 * Delete all extra pixmaps.
	 */
	for (jj = start; jj < MAX_PIXMAP; jj++) {

	    if  (cwin->pxms[lp][jj] != (Pixmap) NULL) {
		XFreePixmap (gemdisplay, cwin->pxms[lp][jj]);
		cwin->pxms[lp][jj] = (Pixmap) NULL;

		if  (cwin->mstr[jj] != (Pixmap) NULL) {
		    XFreePixmap (gemdisplay, cwin->mstr[jj]);
		    cwin->mstr[jj] = (Pixmap) NULL;
		}

		XSync (gemdisplay, False);
	    }
	    else {
		break;
	    }

	}

    }
}
