#include "xwcmn.h"
#include "color.h"
#include "cross.h"


typedef struct{
	int start_x;
	int start_y; 
	int last_x; 
	int last_y;
	GC gc;
} rubber_band_data;

/*
 *	Private functions
 */
void start_rubber_band ( int type, int xx, int yy );
void track_rubber_band ( int type, int xx, int yy );
void end_rubber_band ( int type );

rubber_band_data _rbdata = {0,0,0,0,NULL};
XGCValues	 _values;
int              _rubber_color;
			/* color index for the final box/line/point */
/************************************************************************
 * xgtpnt.c	                                                        *
 *                                                                      *
 * This module gets point(s) from the screen by using mouse.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void xgtpnt ( int *ityp, int *ix, int *iy, int *iret )
/************************************************************************
 * xgtpnt								*
 *									*
 * This subroutine gets point(s) from the screen by using mouse.	*
 *  The selected point/line/box will be drawn into the current pixmap   *
 *  of the current window.  						*
 *									*
 * xgtpnt  ( ityp, ix, iy, iret )					*
 *									*
 * Input parameters:							*
 *	*ityp		int	        Type of rubber band		*
 *					   1 = point,        NP = 1	*
 *					   2 = line,         NP = 2	*
 *					   3 = corner box,   NP = 2	*
 *					   4 = center box,   NP = 2	*
 *									*
 * Output parameters:							*
 *	*ix 		int 		X coordinates of the points	*
 *	*iy 		int		Y coordinates of the points	*
 *	*iret		int		Return code			*
 *					   G_NORMAL   = normal		*
 *					   G_NGRAFCOL = not enough clrs	*
 **									*
 * Log:									*
 * C. Lin/EAI 		 6/93						*
 * S. Jacobs/EAI	 6/93	Clean up				*
 * S. Jacobs/EAI	 7/93	Added cursor change			*
 * C. Lin/EAI 		 7/93   Add rubber band line			*
 * S. Jacobs/EAI	 9/93	Added ITYP				*
 * C. Lin/EAI 		 9/93   Add rectangle rubber band 		*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	Multi-window & multi-pixmap		*
 * C. Lin/EAI	         8/94	Change the selection method		*
 * C. Lin/EAI	         8/94	Ruber band colors 	        	*
 * C. Lin/EAI	         3/95	Use ColorBanks structure; Error handling*
 * C. Lin/EAI	         8/95	Solve the rubber-banding foreground	*
 *				color problem; Made the state variables	*
 *				unique by adding _.			*
 * C. Lin/EAI		 8/97	Add offsets to get 'D' coord when	*
 *				drawing into pixmap			*
 * S. Jacobs/NCEP	 6/98	Removed NP from calling sequence	*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * M. Li/GSC		08/00	added xw_gxor				*
 * M. Li/GSC		08/00	Simplified xw_gxor			*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * T. Piper/SAIC	03/02	plugged memory leak with _rbdata.gc	*
 * J. Wu/SAIC		06/03	activate keyboard input at the end	*
 * E. Safford/SAIC	11/03	remove the request for a final XEvent	*
 * M. Li/SAIC		11/07	added center box mode			*
 ***********************************************************************/
{
    int		nevent, count, np, *xx, *yy, dx, dy, ier;
    int         ipxm, lp, xoffset, yoffset;

    XColor	fchclr, bchclr, cred; 
    Pixmap	shape, mask;

    Cursor      curs;      
    XEvent      gemevent; 
    Window      gwin;    

    unsigned long   forecolor, backcolor;

    Window_str	*cwin;
    winloop_t	*cloop;
    Pixmap	gempixmap;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     * check the type of rubber band 
     */
    if  ( ( *ityp > 4) || ( *ityp < 1 ) )  return;

    /*
     * Set the number of points for the given type.
     */
    if  ( *ityp == 1 )
	np = 1;
    else
	np = 2;

    /*
     * check to see if there are enough graphic colors
     */
    if (ColorBanks.banks[GraphCid] < 2){
	*iret = G_NGRAFCOL;
	return;
    }

    count = 0;

    cwin  = &(gemwindow[current_window]);
    cloop = &(cwin->loop[cwin->curr_loop]); 

    gwin        =  cwin->window;
    ipxm        =  cwin->curpxm[cwin->curr_loop];
    lp = cwin->curr_loop;
    gempixmap = cwin->pxms[lp][ipxm];

    xw_gxor(&_rubber_color, &ier);	
		
    /*	
     * set the foreground color and background color
     */
    forecolor = ColorBanks.colrs[GraphCid][_rubber_color];
    backcolor = ColorBanks.colrs[GraphCid][0] ;
    _values.foreground = backcolor ^ forecolor;
    _values.background = 0;
    _values.function   = GXxor;

    _values.line_width = 3 ;

	if ( _rbdata.gc != NULL ) free (_rbdata.gc);
    _rbdata.gc = 
	XCreateGC ( gemdisplay, gwin, 
		    GCForeground | GCBackground | GCFunction | GCLineWidth, 
		    &_values); 

    /*
     * Set the cursor to a gray crosshair shape.
     */
    shape = XCreatePixmapFromBitmapData ( gemdisplay, gwin,
					  (char *)cross_bits, CROSS_WIDTH, 
					  CROSS_HEIGHT, 1, 0, 1 ); 
    mask  = XCreatePixmapFromBitmapData ( gemdisplay, gwin,
					  (char *)crossmask_bits, 
					  CROSSMASK_WIDTH, 
					  CROSSMASK_HEIGHT, 1, 0, 1 ); 

    fchclr.red = 65535;
    fchclr.blue = 65535;
    fchclr.green = 65535;
    fchclr.flags = DoRed | DoBlue | DoGreen;

    bchclr.red = 0;
    bchclr.blue = 0;
    bchclr.green = 0;
    bchclr.flags = DoRed | DoBlue | DoGreen;

    curs  = XCreatePixmapCursor( gemdisplay, shape, mask, &fchclr,
				 &bchclr, CROSS_X_HOT, CROSS_Y_HOT );

    XDefineCursor  ( gemdisplay, gwin, curs );

    /*
     * Allocate enough memory for the arrays.
     */
    xx = (int *)malloc( (size_t)np * sizeof(int) * 3);
    yy = (int *)malloc( (size_t)np * sizeof(int) * 3);

    /*
     * Get the number of events.
     */
    XSelectInput ( gemdisplay, gwin, 
		   ButtonPressMask | PointerMotionMask
		   | ButtonReleaseMask ); 

    nevent = XEventsQueued ( gemdisplay, QueuedAfterReading );

    /*
     * Loop through checking for button events.
     */
    while ( ( nevent-- > 0 ) || ( count < np ) ) {

	/*
	 * Get the next event.
	 */
	XNextEvent ( gemdisplay, &gemevent );

	/*
	 * Check for the type of event.  Only button press events
	 * being processed now.
	 */
	switch ( gemevent.type ) {

	  case ButtonPress:
	    /*
	     * Check that ButtonPress is in the gemwindow.
	     */
	    if  ( ( gemevent.xbutton.display == gemdisplay ) &&
		  ( gemevent.xbutton.window  == gwin ) &&
		  ( gemevent.xbutton.button  == Button1 ) ) {
     
		xx[count] = gemevent.xbutton.x;
		yy[count] = gemevent.xbutton.y;

		if ( count == 0 ) {
		    start_rubber_band( *ityp, xx[0], yy[0] );
		    count++;
		}
	    }
	    break;

	  case ButtonRelease:
	    /*
	     * Check that ButtonRelease is in the gemwindow.
	     */
	    if  ( ( gemevent.xbutton.display == gemdisplay ) &&
		  ( gemevent.xbutton.window  == gwin ) &&
		  ( gemevent.xbutton.button  == Button1 ) ) {

		xx[count] = gemevent.xbutton.x;
		yy[count] = gemevent.xbutton.y;

		end_rubber_band( *ityp );
		count++;
	    }

	    break;

	  case MotionNotify:
   	    
	    if  ( ( gemevent.xmotion.display == gemdisplay ) &&
		  ( gemevent.xmotion.window  ==  gwin ) ) { 

		if ( count > 0 ) {
		    track_rubber_band( *ityp, gemevent.xmotion.x,
				       gemevent.xmotion.y);
		}
	    }

	    break;
	}
    }
    

    /*
     * Take the last np points
     */
    if ( *ityp == 1 ) {
	ix[0] = xx[count-1];
	iy[0] = yy[count-1];
    }
    else if ( *ityp == 2 ) {
	ix[0] = xx[count-2];
	iy[0] = yy[count-2];

	ix[1] = xx[count-1];
	iy[1] = yy[count-1];
    }
    else if ( *ityp == 3 ) {
	ix[0] =  G_MIN( xx[count - 1], xx[count - 2] );
	iy[0] =  G_MAX( yy[count - 1], yy[count - 2] );

	ix[1] =  G_MAX( xx[count - 1], xx[count - 2] );
	iy[1] =  G_MIN( yy[count - 1], yy[count - 2] );
    }
    else if ( *ityp == 4 ) {
	dx = G_ABS(xx[count - 1] - xx[count - 2] );
	dy = G_ABS(yy[count - 1] - yy[count - 2] );

        ix[0] =  xx[count - 2] - dx;
	iy[0] =  yy[count - 2] + dy;

	ix[1] =  xx[count - 2] + dx;
	iy[1] =  yy[count - 2] - dx;
    }

    /*
     *	Free the allocated memory.
     */
    free ( xx );
    free ( yy );

    /*
     * If one point is needed, draw the point into pixmap.
     */
    xoffset = cloop->xoffset;
    yoffset = cloop->yoffset;

    if ( *ityp == 1 ) {

	_values.foreground = ColorBanks.colrs[GraphCid][_rubber_color];
	_values.function   = GXcopy;

	XChangeGC ( gemdisplay, _rbdata.gc, 
		    GCForeground | GCFunction,
		    &_values);

	XDrawLine ( gemdisplay, gempixmap, 
		    _rbdata.gc,
		    ix[0] - 4 + xoffset, iy[0] - 4 + yoffset,
		    ix[0] + 4 + xoffset, iy[0] + 4 + yoffset);

	XDrawLine ( gemdisplay, gempixmap, 
		    _rbdata.gc,
		    ix[0] - 4 + xoffset, iy[0] + 4 + yoffset,
		    ix[0] + 4 + xoffset, iy[0] - 4 + yoffset );
    }
		

    /*
     * Reset the cursor to the customary red arrow.
     */
    curs = XCreateFontCursor  ( gemdisplay, XC_top_left_arrow );
    XDefineCursor  ( gemdisplay, gwin, curs );
    cred.red = 65535;
    cred.blue = 0;
    cred.green = 0;
    cred.flags = DoRed | DoBlue | DoGreen;
    XRecolorCursor ( gemdisplay, curs, &cred, &cred );

    /*
     * Reset the event selection.
     * Note: a full event selection is more appropriate here. While all
     *       other events are accepted normally after the call of this
     *       function in nmap_zoomw.c, the keyboard events are still
     *       ignored "somehow" and cannot be restored immediately via
     *       XSelectInput & XmProcessTraversal - which is supposed to work.
     *       
     *       To get around this problem, we simulate an EnterNotify event
     *       & send it to the main drawing window to gurantee the immediate
     *       activation of the keyboard. Since the pointer will always be
     *       set in the main drawing window after zooming, this mechanism
     *       should have no other side-effects.
     *       - J. Wu, 06/2003.
     *
     * Note further:  Requesting the event from XNextEvent means that we'll
     *       wait until an X event occurs before returning from this function.
     *       That is, we won't start the zoom until a mouse move, button press,
     *       etc occurs.  The gemevent variable still has a valid event in it
     *       (the 2nd mouse button press event) so just send that to reset 
     *       the keyboard events.
     *       - E. Safford 11/2003. 
     */
    XSelectInput( gemdisplay, gwin, ButtonPressMask | ButtonReleaseMask |
                        ExposureMask | PointerMotionMask | KeyPressMask |
			KeyReleaseMask | KeymapStateMask |
			EnterWindowMask | LeaveWindowMask);

    gemevent.type = EnterNotify; 
    XSendEvent ( gemdisplay, gwin, True, EnterWindowMask, &gemevent );	

}

/*======================================================================*/

void start_rubber_band ( int type, int xx, int yy )
/************************************************************************
 * start_rubber_band                                                    *
 *                                                                      *
 * This routine ...							*
 *									*
 * start_rubber_band( type, xx, yy )					*
 *									*
 * Input parameters:                                                    *
 *      type           int              Type of rubber band             *
 *                                         1 = point,        NP = 1     *
 *                                         2 = line,         NP = 2     *
 *                                         3 = corner box,   NP = 2     *
 *					   4 = center box,   NP = 2	*
 *                                                                      *
 * Output parameters:                                                   *
 *	xx         int             X coordinate of the point		*
 *	yy         int             Y coordinate of the point    	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		11/07	added center box mode			*
 ***********************************************************************/
 {
    Window  gwin;
/*---------------------------------------------------------------------*/

    gwin = gemwindow[current_window].window; 

    _rbdata.last_x = _rbdata.start_x = xx;
    _rbdata.last_y = _rbdata.start_y = yy;

    switch ( type ) {

      case 1: 
      case 2: 
	XDrawLine ( gemdisplay, gwin, _rbdata.gc,
		    _rbdata.start_x, _rbdata.start_y,
		    _rbdata.last_x, _rbdata.last_y); 
	break;

      case 3:
      case 4:
	XDrawRectangle ( gemdisplay, gwin, _rbdata.gc,
			 _rbdata.start_x, _rbdata.start_y, 0, 0);
	break;
    }

}

/*=====================================================================*/

void track_rubber_band ( int type, int xx, int yy )
/************************************************************************
 * track_rubber_band                                                    *
 *                                                                      *
 * This routine ...                                                     *
 *                                                                      *
 * track_rubber_band( type, xx, yy )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *     type           int               Type of rubber band             *
 *                                         1 = point,        NP = 1     *
 *                                         2 = line,         NP = 2     *
 *                                         3 = corner box,   NP = 2     *
 *					   4 = center box    NP = 2	*
 *                                                                      *
 * Output parameters:                                                   *
 *      xx         int             X coordinate of the point            *
 *      yy         int             Y coordinate of the point            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		11/07	added center box mode			*
 ***********************************************************************/
{
    Window gwin; 
/*---------------------------------------------------------------------*/

    gwin = gemwindow[current_window].window; 

    switch ( type ) {

      case 1:
      case 2:
	XDrawLine( gemdisplay, gwin, _rbdata.gc, _rbdata.start_x,
		   _rbdata.start_y, _rbdata.last_x, _rbdata.last_y);
	break;

      case 3:
	XDrawRectangle( gemdisplay, gwin, _rbdata.gc, 
			G_MIN(_rbdata.start_x, _rbdata.last_x),
			G_MIN(_rbdata.start_y, _rbdata.last_y), 
			(unsigned int)G_ABS((float)(_rbdata.last_x - _rbdata.start_x)),
			(unsigned int)G_ABS((float)(_rbdata.last_y - _rbdata.start_y)) );
	break;

      case 4:
	XDrawRectangle( gemdisplay, gwin, _rbdata.gc, 
			_rbdata.start_x - (int)G_ABS((float)(_rbdata.last_x - _rbdata.start_x)),
			_rbdata.start_y - (int)G_ABS((float)(_rbdata.last_y - _rbdata.start_y)),
			(unsigned int)G_ABS((float)((_rbdata.last_x - _rbdata.start_x) * 2.0) ),
			(unsigned int)G_ABS((float)((_rbdata.last_y - _rbdata.start_y) * 2.0) ) );
	break;
    }

    _rbdata.last_x = xx;
    _rbdata.last_y = yy;

    if ( ( _rbdata.start_x == _rbdata.last_x ) ||
	 (	_rbdata.start_y == _rbdata.last_y ) ) 
	_values.line_style = LineOnOffDash;
    else
	_values.line_style = LineSolid;

    XChangeGC ( gemdisplay, _rbdata.gc, 
		GCBackground | GCForeground
		| GCLineStyle | GCFunction,
		&_values);


    switch ( type ) {

      case 1:
      case 2:
	XDrawLine( gemdisplay, gwin, _rbdata.gc, _rbdata.start_x,
		   _rbdata.start_y, _rbdata.last_x, _rbdata.last_y);
	break;

      case 3:
	XDrawRectangle( gemdisplay, gwin, _rbdata.gc, 
                        G_MIN(_rbdata.start_x, _rbdata.last_x),
                        G_MIN(_rbdata.start_y, _rbdata.last_y), 
                        (unsigned int)G_ABS((float)(_rbdata.last_x - _rbdata.start_x)),
                        (unsigned int)G_ABS((float)(_rbdata.last_y - _rbdata.start_y)) );
	break;

      case 4:
	XDrawRectangle( gemdisplay, gwin, _rbdata.gc, 
			_rbdata.start_x - (int)G_ABS((float)(_rbdata.last_x - _rbdata.start_x)),
			_rbdata.start_y - (int)G_ABS((float)(_rbdata.last_y - _rbdata.start_y)),
			(unsigned int)G_ABS((float)((_rbdata.last_x - _rbdata.start_x) * 2.0) ),
			(unsigned int)G_ABS((float)((_rbdata.last_y - _rbdata.start_y) * 2.0) ) );
	break;
    }
} 

/*======================================================================*/

void end_rubber_band ( int type )
/************************************************************************
 * end_rubber_band      	                                        *
 *                                                                      *
 * This routine ...                                                     *
 *                                                                      *
 * end_rubber_band( type )      		                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      type           int              Type of rubber band             *
 *                                         1 = point,        NP = 1     *
 *                                         2 = line,         NP = 2     *
 *                                         3 = corner box,   NP = 2     *
 *					   4 = center box,   NP = 2	*
 *                                                                      *
 * Output parameters:                                                   *
 *	None.								*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * M. Li/SAIC		11/07	added center box mode			*
 ***********************************************************************/
{
    int		ipxm, lp, xoffset, yoffset;
    Window_str	*cwin;
    winloop_t	*cloop;
    Pixmap	gempixmap;
/*---------------------------------------------------------------------*/

    cwin  = &(gemwindow[current_window]); 
    cloop = &(cwin->loop[cwin->curr_loop]);
    ipxm    = cwin->curpxm[cwin->curr_loop]; 
    lp = cwin->curr_loop;
    gempixmap = cwin->pxms[lp][ipxm];

    xoffset = cloop->xoffset;
    yoffset = cloop->yoffset;

    _values.foreground = ColorBanks.colrs[GraphCid][_rubber_color];
    _values.function   = GXcopy;

    XChangeGC ( gemdisplay, _rbdata.gc, 
		GCForeground | GCFunction,
		&_values);

    switch ( type ) {

      case 1:
      case 2:
	/*
	 * Draw the line into pixmap.
	 */
	XDrawLine( gemdisplay, gempixmap, 
		   _rbdata.gc, _rbdata.start_x + xoffset,
		   _rbdata.start_y + yoffset, 
		   _rbdata.last_x + xoffset,
		   _rbdata.last_y + yoffset); 
	break;

      case 3:
	/*
	 * Draw the box into pixmap.
	 */
	XDrawRectangle( gemdisplay, gempixmap, _rbdata.gc, 
                        G_MIN(_rbdata.start_x, _rbdata.last_x) + xoffset,
                        G_MIN(_rbdata.start_y, _rbdata.last_y) + yoffset,
                        (unsigned int)G_ABS((float)(_rbdata.last_x - _rbdata.start_x)),
                        (unsigned int)G_ABS((float)(_rbdata.last_y - _rbdata.start_y)) );
	break;

      case 4:
	XDrawRectangle( gemdisplay, gempixmap, _rbdata.gc, 
			_rbdata.start_x - (int)G_ABS((float)(_rbdata.last_x - _rbdata.start_x))
					+ xoffset,
			_rbdata.start_y - (int)G_ABS((float)(_rbdata.last_y - _rbdata.start_y))
					+ yoffset,
			(unsigned int)G_ABS((float)((_rbdata.last_x - _rbdata.start_x) * 2.0) ),
			(unsigned int)G_ABS((float)((_rbdata.last_y - _rbdata.start_y) * 2.0) ) );
        break;
    }
}
