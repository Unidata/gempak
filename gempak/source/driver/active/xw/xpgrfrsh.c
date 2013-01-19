#include "xwcmn.h"

void xpgrfrsh ( void )
/************************************************************************
 * xpgrfrsh                                                             *
 *                                                                      *
 * This function refresh counter to refresh the VGF elements on all     *
 * frames.								*
 *                                                                      *
 * void xpgrfrsh()                                  			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         X/97   Created.                                *
 * C. Lin/EAI           10/97   Rename from NxmRefresh, cleanup         *
 * E. Safford/GSC	12/98	new refresh strategy			*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ii, lp;
    Window_str	*cwin;
/*---------------------------------------------------------------------*/

    cwin = &(gemwindow[current_window]);

    /* 
     *  set to refresh the VGF elements on all screens 
     */
    for (lp=0; lp < MAX_LOOP; lp++) {
	for (ii=0; ii < MAX_PIXMAP; ii++) {
	    if (cwin->pxms[lp][ii] != (Pixmap) NULL) {
		cwin->xw_rfrsh[lp][ii] = TRUE;
	    }
	    else {
		break;
	    }
	}
    }
}
