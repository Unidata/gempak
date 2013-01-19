#include "xwcmn.h"

void xopenw ( char win_name[], int win_index, float xsize, 
		float ysize, int *ixsize, int *iysize, int *iret )
/************************************************************************
 * xopenw								*
 *									*
 * This subroutine opens one xw window and sets the initial		*
 * graphics context along with basic window attributes.			*
 *									*
 * xopenw ( win_name, win_index, xsize, ysize, ixsize, iysize, iret )	*
 *									*
 * Input parameters:							*
 *	win_name[]	char		window name			*
 *	win_index	int		window index			*
 *	xsize		float		Right edge of window		*
 *	ysize		float		Bottom edge of window		*
 *									*
 * Output parameters:							*
 *	*ixsize		int		Right edge of window		*
 *	*iysize		int		Bottom edge of window		*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * C. Lin/EAI		 7/94	Multi-window & Multi-pixmap		*
 * D. Himes/COMET	 8/94	Turn off NoExpose events in the GC.	*
 *				Caused many unnecessary events to	*
 *				be processed in xxevnt()		*
 * C. Lin/EAI		 2/95	Take out app/gemwindow[].app_context	*
 * M. Linda/GSC		 3/96	Added ixsize, iysize			*
 * S. Jacobs/NCEP	 8/96	Removed the offset of 27 from gemhint.y	*
 * C. Lin/EAI		 2/97	make (xwdth, xhght, xbord, xdpth)       *
 *				unsigned int for ANSI C compiler	*
 * C. Lin/EAI		 6/97	set pxm_wdth, pxm_hght, xoffset, yoffset*
 *				roamflg, copy area info			*
 *				call xclrpxm(), clean up		*
 * D.W.Plummer/NCEP	10/98	change fill rule to 'EvenOddRule'	*
 * S. Law/GSC		10/99	added loop changes in gemwindow		*
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	changed curpxm to an array		*
 * S. Law/GSC		08/00	removed nmap vs nmap2 references	*
 * T. Piper/SAIC	02/05	Replaced XsetStandardProperties wth	*
 *							XSetWMProperties*
 ***********************************************************************/
{
    int			dhght, dwdth, gemscreen, xpos, ypos, ier;
    unsigned int	xwdth, xhght, xbord, xdpth;

    Cursor		curs;
    Window		gwin;
    GC			gemgc;

    XGCValues		 values;
    XTextProperty	 window_name;
    XSizeHints		 *gemhint;
    XWMHints		 *wm_hints;
    XClassHint		 *class_hints;
    XSetWindowAttributes gemxswa;
    XColor		 cred;

    Window_str      	*cwin;
    winloop_t		*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    current_window = win_index;

    gemhint = XAllocSizeHints();
    wm_hints = XAllocWMHints();
    class_hints = XAllocClassHint();
    cwin  = &(gemwindow[current_window]);
    cloop = &(cwin->loop[cwin->curr_loop]);

    strcpy(cwin->name, win_name);

    gemscreen = DefaultScreen( (XtPointer)gemdisplay );

/*
 * Determine window height and width.
 */

    dwdth = DisplayWidth( (XtPointer)gemdisplay, gemscreen );
    dhght = DisplayHeight( (XtPointer)gemdisplay, gemscreen );

    if ( G_ABS ( xsize - RMISSD ) < RDIFFD ) 
	gemhint->width = 0.7F * (float)dwdth ;
    else if ( ( xsize > 0.0F ) && ( xsize <= 1.0F ) ) 
	gemhint->width = (float)dwdth * xsize ;
    else if ( xsize < 100.0F ) gemhint->width = 100 ;
    else if ( xsize > (float)dwdth ) gemhint->width = dwdth ;
    else gemhint->width = (int)xsize ;

    if ( G_ABS ( ysize - RMISSD ) < RDIFFD ) 
	gemhint->height = 0.7F * (float)dhght ;
    else if ( ( ysize > 0.0F ) && ( ysize <= 1.0F ) )
	gemhint->height = (float) dhght * ysize ;
    else if ( ysize < 100.0F ) gemhint->height = 100 ;
    else if ( ysize > (float)dhght ) gemhint->height = dhght ;
    else gemhint->height = (int)ysize ;

    if ( gemhint->width  < 100 ) gemhint->width  = 100 ;
    if ( gemhint->height < 100 ) gemhint->height = 100 ;

    /*
     * Determine window location.
     */

    gemhint->x = dwdth - ( gemhint->width ) - ( current_window * 30 ) - 20;
    if ( gemhint->x < 0 ) gemhint->x = 0;

    gemhint->y = ( current_window * 30 );

    gemhint->flags  = USPosition | USSize;

    /*
     * Create the window and set standard properties and attributes.
     */

    gwin = XCreateSimpleWindow( gemdisplay, root, gemhint->x, gemhint->y, 
				gemhint->width, gemhint->height, 5, 
				WhitePixel ( (XtPointer)gemdisplay, gemscreen ),
				BlackPixel ( (XtPointer)gemdisplay, gemscreen ) );

    cwin->window = gwin;

    class_hints->res_name = win_name;
    class_hints->res_class = win_name;
    wm_hints->initial_state = NormalState;
    wm_hints->input = True;
    wm_hints->flags = StateHint | InputHint;

    XStringListToTextProperty(&win_name, 1, &window_name);
    XSetWMProperties( gemdisplay, gwin, &window_name, &window_name,
			NULL, 0, gemhint, wm_hints, class_hints ); 
    XFree(gemhint);
    XFree(wm_hints);
    XFree(class_hints);

    gemxswa.bit_gravity = CenterGravity;

    XChangeWindowAttributes (gemdisplay, gwin, (CWBitGravity), &gemxswa );

    /*
     * Get the geometry and window size information.
     */
    XGetGeometry( gemdisplay, gwin, &root, &xpos,
			   &ypos, &xwdth, &xhght, &xbord, &xdpth );

    cwin->width  = xwdth;
    cwin->height = xhght;
    cwin->depth  = xdpth;

    /*
     * Create graphics contexts.
     */
    gemgc = XCreateGC( gemdisplay, gwin, 0, 0 );

    /*
     * Turn of NoExpose and GraphicsExpose events.  They
     * don't seem to be needed and were causing many events
     * to seen in xxevent().
     */
    values.graphics_exposures = False;
    XChangeGC( gemdisplay, gemgc, GCGraphicsExposures, &values);

    cwin->gc = gemgc; 

    /*
     * Set backgound colors.
     */
    XSetBackground( gemdisplay, gemgc, 
		    BlackPixel ( (XtPointer)gemdisplay, gemscreen ) ) ;

    /*
     * Set fill rule.
     */
    XSetFillRule ( gemdisplay, gemgc, EvenOddRule );

    /*
     * Create one pixmap.
     */
    	cwin->pxms[cwin->curr_loop][0] = 
	XCreatePixmap(gemdisplay, root, xwdth, xhght, xdpth);

    cwin->curpxm[cwin->curr_loop] = 0;
    cloop->pxm_wdth	= xwdth;
    cloop->pxm_hght	= xhght;
    cloop->roamflg	= 0;
    cloop->xoffset	= 0;
    cloop->yoffset	= 0;

    cloop->pxm_x	= 0;
    cloop->pxm_y	= 0;
    cwin->area_w	= xwdth;
    cwin->area_h	= xhght;
    cwin->win_x		= 0;
    cwin->win_y		= 0;

    /*
     * return device size
     */
    *ixsize = xwdth;
    *iysize = xhght; 

    /* 
     * clear the pixmap, 
     */
    xclrpxm(&(cwin->curpxm[cwin->curr_loop]), &ier);

    /*
     * Select the events to be processed.
     */
    XSelectInput ( gemdisplay, gwin, ExposureMask );

    /*
     * Set the cursor to be the customary red arrow.
     */
    curs = XCreateFontCursor ( gemdisplay, XC_top_left_arrow );
    XDefineCursor ( gemdisplay, gwin, curs );
    cred.red	= 65535;
    cred.blue	= 0;
    cred.green	= 0;
    cred.flags	= DoRed | DoBlue | DoGreen;
    XRecolorCursor ( gemdisplay, curs, &cred, &cred );
    XFreeCursor(gemdisplay, curs);
}
