#include "xwcmn.h"

void xxevnt ( int *iret )
/************************************************************************
 * xxevnt								*
 *									*
 * This subroutine handles expose events for the X window device driver.*
 *									*
 * xxevnt  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * M. desJardins/NMC	 1/92						*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window & multi-pixmap		*
 * D. Himes/Comet        8/94	Don't set the drivers notion of 	*
 *                           	the "active window" to last     	*
 *                           	window that received an exposed 	*
 *                           	event.  Take out check for No-  	*
 *                           	Expose event as it was disabled 	*
 *                           	in xopenw().                    	*
 * D. Himes/Comet        8/94	Only redraw areas that need it. 	*
 * C. Lin/EAI            2/95	clean up. 				*
 * C. Lin/EAI            6/97	consider 'S' coordinates. 		*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	updated for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 ***********************************************************************/
{
    int		ii, nevent, ipxm, xoffset, yoffset, lp;
    XEvent	gemevent;
    GC		gemgc;
    Pixmap	gempixmap;
    Window	gwin;

    Window_str	*cwin;
    winloop_t	*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * Get the number of events.
     */

    nevent = XEventsQueued ( gemdisplay, QueuedAfterReading );

    /*
     * Loop through checking for expose events.
     */

    while ( nevent-- ) {

	/*
	 * Get the next event.
	 */

	XNextEvent ( gemdisplay, &gemevent );

	/*
	 * Check for the type of event.  
	 * Only expose events being
	 *	    processed now.
	 */

	switch ( gemevent.type ) {
	  case Expose:

	    /*
	     * Only redraw the areas of the window exposed.
	     */

	    gwin = gemevent.xany.window;

	    for ( ii = 0; ii < MAX_WINDOW; ii++ ) {
		if ( gwin == gemwindow[ii].window ) {
		    break;
		}
	    }

	    /*
	     * Check to see that we matched on the window so
	     * we don't jump off the end of the gemwindow array.
	     */

	    if  ( ii >= MAX_WINDOW ) break;

	    cwin	= &(gemwindow[ii]);
	    lp		= cwin->curr_loop;
	    cloop	= &(cwin->loop[lp]);
	    ipxm	= cwin->curpxm[lp];
	    gemgc	= cwin->gc;
	    gempixmap	= cwin->pxms[lp][ipxm];
	    xoffset	= cloop->xoffset;
	    yoffset	= cloop->yoffset;

	    {
		XExposeEvent *ev = &gemevent.xexpose;
		XCopyArea ( gemdisplay, gempixmap, gwin,
			    gemgc, ev->x+xoffset, ev->y+yoffset, 
			    ev->width, ev->height, 
			    ev->x, ev->y );
	    }

	    break;  /*  End case Expose:  */

	  default:    

	    /*
	     * Shouldn't get here, but..
	     */

	    break;  /* default */

	} /* end switch */
    }

    /*
     * Only flush the queued X commands once when we're done
     * processing events.  Note that the xflush is needed because
     * of the way we are handling events.  We never go back into
     * the xevent loop with no events pending (which is what normally
     * causes an xflush to occur.
     */

    XFlush ( gemdisplay ); 
}
