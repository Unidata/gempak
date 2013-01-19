#include "xwcmn.h"

void xmotifw ( Window win_id, char win_name[], GC win_gc, int width, 
				int height, int depth, int *iret )
/************************************************************************
 * xmotifw								*
 *									*
 * This subroutine registers one x/motif window created by not using    *
 * GEMPAK functions.  The graphics context along with basic window      *
 * is filled into the WINDOW structure defined in the GEMPAK xw driver.	*
 * One pixmap is created in this subroutine.  If the window had been    *
 * opened, i.e., it is already in use, this subroutine will just        *
 * return G_NWUSED.          						*
 *									*
 * xmotifw  ( win_id, win_name, win_gc, width, height, depth, iret ) 	*
 *									*
 * Input parameters:							*
 *      win_id          Window          window identifier               *
 *	win_name[]	char		window name		        *
 *	win_gc          GC              window graphics context         *
 *      width           int             window width                    *
 *      height          int             window height                   *
 *      depth           int             window depth                    *
 *                                                                      *
 * Output parameters:							*
 *                                                                      *
 *	*iret		int		Return code			*
 *			G_NORMAL = normal return			*
 *									*
 *                      G_NIWNAM = invalid window name (too long)       *
 *			G_NWUSED = window is already open (in use)	*
 **									*
 * Log:									*
 * D. Plummer/NMC	 7/94						*
 * C. Lin/EAI		 8/94	Clean up				*
 * C. Lin/EAI		 8/94	Added calls to clear the pixmap		*
 * C. Lin/EAI		 2/95	Error handling				*
 * C. Lin/EAI		 6/97	Add roam				*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. SAfford/GSC	12/99	update for new xwcmn.h & initalize pxms	*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ii, jj, ipxm, lp, ier;
    char	wname[WNAME_LEN];

    Window_str	*cwin;
/*---------------------------------------------------------------------*/
    if ( (win_name) && (strlen(win_name) > (size_t)WNAME_LEN) ) {
	*iret = G_NIWNAM;
	return;
    }

    *iret = G_NORMAL;

    if (win_name != NULL) {
	strcpy( wname, win_name);
    }
    else {
	strcpy (wname, DEFAULT_WNAME);
    }

    /*
     * check to see if this window has been used.
     */

    for ( ii = 0; ii < MAX_WINDOW; ii++) {
	if ( strcmp( wname, gemwindow[ii].name ) == 0 ) {
	    *iret = G_NWUSED;
	    return;
	}
    }

    /*
     * find the first unused window for it.
     */

    for ( ii = 0; ii < MAX_WINDOW; ii++) {

	if ( gemwindow[ii].name[0] == '\0' ) {

	    current_window = ii;

	    strcpy( gemwindow[ii].name, wname ) ;

	    cwin = &(gemwindow[ii]);

	    cwin->window	= win_id ;
	    cwin->gc		= win_gc ;
	    cwin->width		= width ;
	    cwin->height	= height ;
	    cwin->depth		= depth ;
	    cwin->npxms		= 1 ;
	    cwin->curpxm[0]	= 0 ;

	    cwin->area_w	= width;
	    cwin->area_h	= height;
	    cwin->win_x		= 0;
	    cwin->win_y		= 0;


	    for (lp = 0; lp < MAX_LOOP; lp++) {
		for (jj = 0; jj < MAX_PIXMAP; jj++) {
		    cwin->pxms[lp][jj] = (Pixmap)NULL;
		    cwin->curpxm[lp] = 0 ;
		}
	    }
	    cwin->pxms[cwin->curr_loop][0] = 
		XCreatePixmap (gemdisplay, root, width, height, depth);

	    for (jj = 0; jj < MAX_LOOP; jj++) {
		cwin->loop[jj].pxm_wdth	= width ;
		cwin->loop[jj].pxm_hght	= height ;
		cwin->loop[jj].pxm_x	= 0;
		cwin->loop[jj].pxm_y	= 0;
		cwin->loop[jj].xoffset	= 0 ;
		cwin->loop[jj].yoffset	= 0 ;
		cwin->loop[jj].roamflg	= 0;
	    }

	    ipxm = 0;
	    xclrpxm(&ipxm, &ier);

	    break;
	}
    }

}
