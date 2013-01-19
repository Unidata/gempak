#include "xwcmn.h"

int xarea ( void )
/************************************************************************
 * xarea								*
 *									*
 * This subroutine computes the information needed to copy from pixmap  *
 * to current window. The parameteres computed are source(pixmap) upper *
 * left x,y coordinates, the destination (window) upper left corner,    *
 * the width and height of the copy area. This function is an XW driver *
 * internal function.        						*
 *									*
 * int xarea ( )							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 * xarea	int		0 - normal, -1 - window out of bounds	*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	         6/97						*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 ***********************************************************************/
{
    int		src_x, src_y, width, height, dest_x, dest_y;
    int		xtmp, ytmp;
    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    cwin  = &(gemwindow[current_window]);
    cloop = &(cwin->loop[cwin->curr_loop]);

    xtmp = cloop->xoffset + cwin->width;
    ytmp = cloop->yoffset + cwin->height;

    if (xtmp > 0 && ytmp > 0 ) {
	if ( cloop->xoffset >= 0 && cloop->yoffset >= 0 ) {
	    src_x  = cloop->xoffset; 
	    src_y  = cloop->yoffset; 
	    dest_x = 0; 
	    dest_y = 0;

	    if ( xtmp > cloop->pxm_wdth )
		width = cloop->pxm_wdth - cloop->xoffset;
	    else
		width = cwin->width;    

	    if ( ytmp > cloop->pxm_hght )
		height = cloop->pxm_hght - cloop->yoffset;
	    else
		height = cwin->height;    
	}
	else if ( cloop->xoffset < 0 && cloop->yoffset < 0) {
	    src_x  = 0; 
	    src_y  = 0; 
	    dest_x = -(cloop->xoffset); 
	    dest_y = -(cloop->yoffset);

	    if ( xtmp > cloop->pxm_wdth )
		width = cloop->pxm_wdth;
	    else
		width = xtmp;    

	    if ( ytmp > cloop->pxm_hght )
		height = cloop->pxm_hght;
	    else
		height = ytmp;    
	}
	else if ( cloop->xoffset < 0 ) { /* cloop->yoffset > 0 */
	    src_x  = 0; 
	    src_y  = cloop->yoffset; 
	    dest_x = -(cloop->xoffset); 
	    dest_y = 0;

	    if ( xtmp > cloop->pxm_wdth )
		width = cloop->pxm_wdth;
	    else
		width = xtmp;    

	    if ( ytmp > cloop->pxm_hght )
		height = cloop->pxm_hght - cloop->yoffset;
	    else
		height = cwin->height;    

	}
	else { /* cloop->xoffset > 0, cloop->yoffset < 0 */
	    src_x  = cloop->xoffset; 
	    src_y  = 0; 
	    dest_x = 0; 
	    dest_y = -(cloop->yoffset);

	    if ( xtmp > cloop->pxm_wdth )
		width = cloop->pxm_wdth - cloop->xoffset;
	    else
		width = cwin->width;    

	    if ( ytmp > cloop->pxm_hght )
		height = cloop->pxm_hght;
	    else
		height = ytmp;    
	}

	/*
	 * save the information into window structure
	 */
	cloop->pxm_x  = src_x;
	cloop->pxm_y  = src_y;
	cwin->area_w = width;
	cwin->area_h = height;
	cwin->win_x  = dest_x;
	cwin->win_y  = dest_y;

	return(0);
    }
    else {
	return(-1);
    }
}
