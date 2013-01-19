#include "xwgui.h"

void xsroam ( int *roamflg, int *ipwdth, int *iphght, int *idwdth, 
						int *idhght, int *iret )
/************************************************************************
 * xsroam								*
 *									*
 * This subroutine sets the roaming for current window in X window      *
 * driver. This function will not keep the original contents of the old *
 * pixmaps should the new requested size be different from the original *
 * size.								*
 *									*
 * xsroam  ( roamflg, ipwdth, iphght, idwdth, idhght, iret )		*
 *									*
 * Input parameters:							*
 *	*roamflg	int	roam flag				*
 *	*ipwdth		int	pixmap width				*
 *	*iphght		int	pixmap height				*
 *									*
 * Output parameters:							*
 *	*idwdth		int	device	Width  (window width)		*
 *	*idhght		int	device  window	Height (window height)	*
 *	*iret		int		Return code			*
 *			    G_NORMAL = normal return (no size change)	*
 *			    G_NWSIZE = size change			*
 **									*
 * Log:									*
 * C. Lin/EAI	         6/97						*
 * S. Jacobs/NCEP	12/98	Fixed cast of NULL for LINUX		*
 * E. Safford/GSC	03/99	Delete master pxmps when prod gen is up *
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		06/00	mstr array only for current loop	*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * H. Zeng/EAI          04/01   replaced _lpIdx with _lp                *
 ***********************************************************************/
{
    char	newpxm;
    int		jj, ier, xwdth, xhght, xdpth, npxm, fpxm, lpxm; 

    Window_str	*cwin;
    winloop_t	*cloop;
    Boolean	in_view;
/*---------------------------------------------------------------------*/


    *iret = G_NORMAL;

    /*
     * Get the current window information.
     */
    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[_lp]);
    xwdth = cwin->width; 
    xhght = cwin->height; 
    xdpth = cwin->depth; 

    /* 
     *  check if current loop is actually being viewed
     */
    in_view = (_lp == _viewedLoop);    	
				

    newpxm = 0;
    if ( *roamflg == 1 ) {
	cloop->roamflg = 1;

	/* 
	 * roam on: pixmap size is the same as requested
	 */
	if ( cloop->pxm_wdth != *ipwdth ||
	     cloop->pxm_hght != *iphght ) {
	    newpxm = 1;
	    cloop->pxm_wdth = *ipwdth;
	    cloop->pxm_hght = *iphght;
	}	 
    }
    else {
	cloop->roamflg = 0;

	/*
	 * roam off: pixmap size will be the same with window size
	 */
	if ( cloop->pxm_wdth != xwdth ||
	     cloop->pxm_hght != xhght ) {
	    newpxm = 1;
	    cloop->pxm_wdth = xwdth;
	    cloop->pxm_hght = xhght;
	}
    }

    /*
     * create new pixmaps if needed 
     */
    if  ( newpxm ) { 

	*iret = G_NWSIZE;

	fpxm = _fstPxm[_lp];
	npxm = _numPxm[_lp];
	lpxm = fpxm + npxm + 1; 	/* add one for hidden pixmap */


	/*
	 * Delete all old pixmaps
	 */
	for (jj = fpxm; jj < lpxm; jj++) {

	    if (cwin->pxms[_lp][jj] != (Pixmap) NULL) {
		XFreePixmap (gemdisplay, cwin->pxms[_lp][jj]);
		cwin->pxms[_lp][jj] = (Pixmap) NULL;
		cwin->npxms--;
	    }
	    if ( in_view && (cwin->mstr[jj] != (Pixmap) NULL) ) {
		XFreePixmap (gemdisplay, cwin->mstr[jj]);
		cwin->mstr[jj] = (Pixmap) NULL;
	    }
	}

	XSync( gemdisplay, False );


	/*
	 * Create a new pixmap.
	 */
	cwin->pxms[_lp][fpxm] =  
	    XCreatePixmap ( gemdisplay, root,
			    cloop->pxm_wdth, cloop->pxm_hght, xdpth );

	cwin->npxms++; 
	cwin->curpxm[cwin->curr_loop] = fpxm; 
	
	xclrpxm( &fpxm, &ier );

	cloop->xoffset	= 0;
	cloop->yoffset	= 0;

	cloop->pxm_x	= 0;
	cloop->pxm_y	= 0;
	cwin->area_w	= cwin->width;
	cwin->area_h	= cwin->height;
	cwin->win_x	= 0;
	cwin->win_y	= 0;
    }

    *idwdth = cloop->pxm_wdth;
    *idhght = cloop->pxm_hght;

}
