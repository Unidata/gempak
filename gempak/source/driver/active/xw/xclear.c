#include "xwcmn.h"

void xclear ( int *idwdth, int *idhght, int *iswdth, int *ishght, int *iret )
/************************************************************************
 * xclear								*
 *									*
 * This subroutine clears the current X Window.				*
 *									*
 * xclear  ( idwdth, idhght, iswdth, ishght, iret )			*
 *									*
 * Output parameters:							*
 *	*idwdth		int	device	Width  (pixmap width)		*
 *	*idhght		int	device	Height (pixmap height)		*
 *	*iswdth		int	window	Width  (window width)		*
 *	*ishght		int	window	Height (window height)		*
 *	*iret		int		Return code			*
 *			    G_NORMAL = normal return (no size change)	*
 *			    G_NWSIZE = size change			*
 *			    G_NMEMRY = not enough memory		*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * M. desJardins/NMC	01/91	GEMPAK 5.1				*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	multi-window, multi-pixmap		*
 * C. Lin/EAI	         2/97	make (w,h,b,d) unsigned int for ANSI C	*
 * C. Lin/EAI	         6/97	add 'S' coordinates (roam)		*
 *				add window size in calling sequence	*
 *				call xclrpxm()				*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	10/99	modify for use with nmap2		*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	cwin->pixmaps -> cwin->pxms[lp]		*
 ***********************************************************************/
{
    int		 xx, yy, jj, lp, ier;
    unsigned int ww, hh, bb, dd, xwdth, xhght;

    int		 ipxm; 
    Window	 gwin; 

    Window_str	 *cwin;
    winloop_t	 *cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Get the current window information.
     */
    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);
    
    gwin  = cwin->window; 
    xwdth = cwin->width; 
    xhght = cwin->height; 
    ipxm  = cwin->curpxm[cwin->curr_loop]; 
    lp    = cwin->curr_loop;


    /*
     * If this is the first pixmap, check for a resize.
     */
    if  (ipxm == 0 || cwin->nmap2) {

	/*
	 * Get the current window geometry.
	 */
	XGetGeometry (gemdisplay, gwin, &root, &xx, &yy,
		       &ww, &hh, &bb, &dd); 

	/*
	 * Compare the current size with the old size.
	 */
	if  ( ( xwdth != ww ) || ( xhght != hh ) ) {

	    xwdth  = ww;
	    xhght  = hh;

	    *iret = G_NWSIZE;

	    if (!cloop->roamflg) {
	
		/*
		 * roam off: new pixmap 
		 */
		cwin->width	= xwdth; 
		cwin->height	= xhght; 
		cloop->pxm_wdth	= xwdth; 
		cloop->pxm_hght	= xhght; 
		cloop->xoffset	= 0; 
		cloop->yoffset	= 0; 
		cloop->pxm_x	= 0; 
		cloop->pxm_y	= 0; 
		cwin->area_w	= xwdth; 
		cwin->area_h	= xhght; 
		cwin->win_x	= 0; 
		cwin->win_y	= 0; 

		if (!cwin->nmap2) {
		    /*
		     * Delete all of the old pixmaps
		     */
		    for ( jj = 0 ; jj < MAX_PIXMAP; jj++ ) {
			if  (cwin->pxms[lp][jj] != (Pixmap)NULL) {
			    XFreePixmap (gemdisplay, cwin->pxms[lp][jj]);
			    cwin->pxms[lp][jj] = (Pixmap)NULL;
			    XSync( gemdisplay, False );
			}
		    }

		    /*
		     * Create a new pixmap.
		     */
		    cwin->pxms[lp][ipxm] = 
			XCreatePixmap (gemdisplay, root, cloop->pxm_wdth, 
				       cloop->pxm_hght, cwin->depth);
		    cwin->npxms = 1;
		}
	    }
	    else {

		/*
		 * roam is on, reset the window size only
		 */
		cwin->width  = xwdth;
		cwin->height = xhght;

		xarea();
	    }
	}
    }

    if (cwin->nmap2) {


	if  ( cwin->pxms[lp][ipxm] != (Pixmap)NULL ) {
	    XFreePixmap ( gemdisplay, cwin->pxms[lp][ipxm] );
	    cwin->pxms[lp][ipxm] = (Pixmap)NULL;
	    XSync( gemdisplay, False );

	    cwin->npxms--;
	}

	cwin->pxms[lp][ipxm] = 
	    XCreatePixmap( gemdisplay, root, cloop->pxm_wdth, 
			   cloop->pxm_hght, cwin->depth );
	cwin->npxms++;
    }

    xclrpxm(&ipxm, &ier);

    /*
     * Set the pixmap size to return.
     */
    *idwdth = cloop->pxm_wdth;
    *idhght = cloop->pxm_hght;
    *iswdth = cwin->width;
    *ishght = cwin->height;

}
