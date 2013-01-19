#include "xwcmn.h"

void gfopenw ( char win_name[], int win_index, float xsize, 
			float ysize, int *ixsize, int *iysize, int *iret )
/************************************************************************
 * gfopenw								*
 *									*
 * This subroutine opens one xw window and sets the initial		*
 * graphics context along with basic window attributes.			*
 *									*
 * gfopenw ( win_name, win_index, xsize, ysize, ixsize, iysize, iret )	*
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
 * A. Hardy/GSC          2/01   Copied from the XW driver and           *
 *                              removed size limitations                *
 * T. Piper/SAIC	02/08	Renamed gf/xopenw to gfopenw		*
 ***********************************************************************/
{
    int			dhght, dwdth, gemscreen, xpos, ypos, ier;
    unsigned int	xwdth, xhght, xbord, xdpth;
    char		gemname [WNAME_LEN];

    Cursor		curs;
    Window		gwin;
    GC			gemgc;

    XGCValues		 values;
    XSizeHints		 gemhint;
    XSetWindowAttributes gemxswa;
    XColor		 cred;

    Window_str      	*cwin;
    winloop_t		*cloop;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    current_window = win_index;

    cwin  = &(gemwindow[current_window]);
    cloop = &(cwin->loop[cwin->curr_loop]);

    strcpy(cwin->name, win_name);

    strcpy(gemname, win_name);

    gemscreen = DefaultScreen( (XtPointer)gemdisplay );

/*
 * Determine window height and width.
 */
    dwdth = DisplayWidth( (XtPointer)gemdisplay, gemscreen );
    dhght = DisplayHeight( (XtPointer)gemdisplay, gemscreen );

    if ( G_ABS ( xsize - RMISSD ) < RDIFFD ) 
	gemhint.width = 0.7 * (float) dwdth ;
    else if ( ( xsize > 0.0 ) && ( xsize <= 1.0 ) ) 
	gemhint.width = (float) dwdth * xsize ;
    else if ( xsize < 100.0 ) gemhint.width = 100 ;
    else gemhint.width = (int) xsize ;

    if ( G_ABS ( ysize - RMISSD ) < RDIFFD ) 
	gemhint.height = 0.7 * (float) dhght ;
    else if ( ( ysize > 0.0 ) && ( ysize <= 1.0 ) )
	gemhint.height = (float) dhght * ysize ;
    else if ( ysize < 100.0 ) gemhint.height = 100 ;
    else gemhint.height = (int) ysize ;

    if ( gemhint.width  < 100 ) gemhint.width  = 100 ;
    if ( gemhint.height < 100 ) gemhint.height = 100 ;

/*
 * Determine window location.
 */
    gemhint.x = dwdth - ( gemhint.width ) - ( current_window * 30 ) - 20;
    if ( gemhint.x < 0 ) gemhint.x = 0;

    gemhint.y = ( current_window * 30 );

    gemhint.flags  = USPosition | USSize;

/*
 * Create the window and set standard properties and attributes.
 */
    gwin = XCreateSimpleWindow( gemdisplay, root, gemhint.x, gemhint.y, 
				gemhint.width, gemhint.height, 5, 
				WhitePixel ( (XtPointer)gemdisplay, gemscreen ),
				BlackPixel ( (XtPointer)gemdisplay, gemscreen ) );

    cwin->window = gwin;

    XSetStandardProperties( gemdisplay, gwin, gemname, gemname, None,
			    NULL, 0, &gemhint ); 

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

}
