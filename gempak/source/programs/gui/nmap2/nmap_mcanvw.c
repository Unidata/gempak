#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"

#define	MIN_CANVAS_WIDTH	( 800 )
#define	MIN_CANVAS_HEIGHT	( 300 )

/*
 *  drawing area of the main window
 */
static Widget   _drawingW;


/* 
 * virtual function variables  -- state functions 
 */
static XtEventHandler _pressFunc;	/* function called on "press" */
static XtEventHandler _dragFunc;	/* function called on "drag" */
static XtEventHandler _dropFunc;	/* function called on "drop" */


static float	_lastX;
static float	_lastY;

static Boolean  _dynActiveFlag;		/* dynamics active flag */


/*
 * Private callback functions and event handler
 */
void mcanvw_exposeCb (   Widget, XtPointer,
			 XmDrawingAreaCallbackStruct *call );
void mcanvw_resizeCb ( Widget, XtPointer, XtPointer );

static void mcanvw_evntHandler ( Widget, XtPointer, XEvent*, Boolean* );

/************************************************************************
 * nmap_mcanv.c								*
 *									*
 * This module creates the drawing area of main window and defines the	*
 * callback functions for nmap.						*
 *									*
 * CONTENTS:                                                            *
 *	mcanvw_create()		creates the drawing area.		*
 *	mcanvw_rgstr()		register the canvas as a GEMPAK window.	*
 *	mcanvw_setCursor()	sets the cursor inside drawing widget	*
 *	mcanvw_setLocDspl()	sets the latlon for the locator text	*
 *									*
 *	mcanvw_setPressFunc()	callback for pressing mouse button	*
 *	mcanvw_setDragFunc()	callback for dragging mouse		*
 *	mcanvw_setDropFunc()	callback for releasing mouse button	*
 *	mcanvw_setDynamicFunc()	set callbacks for 'Arm Dynamic' state.	*
 *									*
 *	mcanvw_disarmPress()	Disarms callback for pressing mouse btn	*
 *	mcanvw_disarmDrag()	Disarms callback for dragging mouse btn	*
 *	mcanvw_disarmDrop()	Disarms callback for releasing mouse btn*
 *	mcanvw_disarmDynamic()	Disarms callback for mouse btn motion	*
 *									*
 *	mcanvw_getDrawingW()	get the drawing widget ID.		*
 *	mcanvw_getDims()	get the drawing area dimensions		*
 *	mcanvw_getCurPos()	get current (last) cursor position	*
 *	mcanvw_getDynActFlg()	get the dynActFlag.			*
 *	mcanvw_getDpth()	return the DefaultDepth of the display	*
 *									*
 *	mcanvw_exposeCb()	EXPOSE callback function for canvas.	*
 *	mcanvw_resizeCb()	resize callback function for canvas.	*
 *	mcanvw_evntHandler()	event handler for canvas.		*
 ***********************************************************************/

/*=====================================================================*/

Widget mcanvw_create ( Widget parent )
/************************************************************************
 * mcanvw_create                                              		*
 *                                                                      *
 * This function creates the canvas drawing area of the main window.    *
 *                                                                      *
 * Widget mcanvw_create(parent)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent form widget ID                          *
 *                                                                      *
 * Output parameters:                                                   *
 * mcanvw_create	Widget      drawing widget ID                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96  						*
 * I. Durham/GSC   	05/98	Changed underscore decl. to an include	*
 * E. Safford/GSC	06/01	leave _drawingW unmanaged initially	*
 * E. Safford/GSC	07/01	add min width & height to avoid crashes *
 *				  from undersized resource file settings*
 * T. Piper/SAIC	05/03	removed unnecessary xwcmn.h		*
 * B. Yin/SAIC		03/05	removed the default arrow key behavior  *
 * T. Piper/SAIC	07/07	restored back setting XtTranslatuions	*
 ***********************************************************************/
{
int		width, height;
XtTranslations	xlations;
/*---------------------------------------------------------------------*/
/*
 * create canvas drawing area
 */
        _drawingW = XtVaCreateWidget("mainw_canvas",
                        xmDrawingAreaWidgetClass, parent,
                        XmNresizable,             TRUE,
			XmNtopAttachment,	  XmATTACH_FORM,
			XmNleftAttachment,	  XmATTACH_FORM,
			XmNrightAttachment,	  XmATTACH_FORM,
			XmNtraversalOn,		  FALSE,
                        NULL);

        mcanvw_getDims( &width, &height);

	if (width < MIN_CANVAS_WIDTH) {
	XtVaSetValues(_drawingW, XmNwidth, MIN_CANVAS_WIDTH, NULL);
	}

	if (height < MIN_CANVAS_HEIGHT) {
	XtVaSetValues(_drawingW, XmNheight, MIN_CANVAS_HEIGHT, NULL);
	}

        XtAddCallback(_drawingW, XmNexposeCallback,
				(XtCallbackProc)mcanvw_exposeCb, NULL);
	XtAddCallback(_drawingW, XmNresizeCallback, mcanvw_resizeCb, NULL);
        XtAddEventHandler(_drawingW, 
			PointerMotionMask | LeaveWindowMask | 
			EnterWindowMask, 
                        False, (XtEventHandler)mcanvw_evntHandler, NULL);

/*
 *  To make the arrow keys work for selecting TCA segments,
 *  we need turn on the traversal and remove the default arrow
 *  key behavior.
 */
    	XtVaGetValues ( _drawingW, XmNtranslations, &xlations, NULL );
   	XtVaSetValues ( _drawingW, XmNtraversalOn, TRUE , NULL );
   	XtUninstallTranslations( _drawingW );
   	XtVaSetValues ( _drawingW, XmNtranslations, xlations, NULL );

	return(_drawingW);
}

/*=====================================================================*/

void mcanvw_rgstr ( void )
/************************************************************************
 * mcanvw_rgstr                                                      	*
 *                                                                      *
 * This function registers the canvas widget of the main window as      *
 * a GEMPAK window.                      				*
 *                                                                      *
 * void mcanvw_rgstr()                                               	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      4/96                                                 *
 * T. Piper/SAIC        07/03   replaced gmpk_rgstr with NxmGmpkRgstr   *
 ***********************************************************************/
{
    char wname[10];
/*---------------------------------------------------------------------*/
    strcpy(wname, MCANVW_NAME);
    NxmGmpkRgstr(_drawingW, wname, NULL);
}

/*=====================================================================*/

void mcanvw_setCursor ( int cursor )
/************************************************************************
 * mcanvw_setCursor							*
 *									*
 * This function changes cursor in the main window drawing area.	*
 *									*
 * void mcanvw_setCursor (cursor)					* 
 *									*
 * Input parameters:                                                    *
 *	cursor		int	which type of cursor     		*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	initial coding				*
 * H. Zeng/EAI          04/00   changed cursor change funtion           *
 * H. Zeng/EAI          04/00   changed "combo" to "cursor"             *
 ***********************************************************************/
{
    static int	curr_cursor = -99;
/*---------------------------------------------------------------------*/

    if (cursor == curr_cursor) return;

    switch (cursor) {
      case CURS_DEFAULT:		/* default red arrow */
	NxmCursor_setCursor(_drawingW, CURS_DEFAULT);
	break;

      case CURS_POINT_SELECT:		/* busy signal */
	NxmCursor_setCursor(_drawingW, CURS_POINT_SELECT);
	break;

      case CURS_BUSY:		        /* cross hair */
	NxmCursor_setCursor(_drawingW, CURS_BUSY);
	break;
    }
    curr_cursor = cursor;
}

/*=====================================================================*/

void mcanvw_setLocDspl ( void )
/************************************************************************
 * mcanvw_setLocDspl							*
 *									*
 * This function finds the latitude and longitude based on _lastX/Y and	*
 * passes the information to mbotw_updateLocDspl.			*
 *									*
 * void mcanvw_setLocDspl ()						* 
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/00	moved from mcanvw_evntHandler		*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * H. Zeng/SAIC		01/07	changed to use mbotw_updateLocDspl	*	
 ***********************************************************************/
{
    int		np, ier;
    float	lat, lon;
/*---------------------------------------------------------------------*/
/*
 *  get lat/lon 
 */
    np = 1;
    gtrans (sys_S, sys_M, &np, &_lastX, &_lastY, &lat, &lon, &ier, 
            strlen(sys_S), strlen(sys_M) );

/*
 *  display the location info. on the bottom of the main window 
 *  based on the lat/lon provided.
 */
    mbotw_updateLocDspl (lat, lon);
}

/*=====================================================================*/

void mcanvw_setPressFunc ( XtEventHandler func, int cursor )
/************************************************************************
 * mcanvw_setPressFunc							*
 *									*
 * Associates a callback function with the click of a mouse button.	*
 *									*
 * void mcanvw_setPressFunc (func, cursor)				*
 *									*
 * Input parameters:							*
 *	func	XtEventHandler	Pointer to Evevnt Handler function	*
 *	cursor	int		which type of cursor     		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	04/97	Created					*
 * E. Wehner/EAi	09/97	stop passing grinfo record		*
 * C. Lin/EAI		10/97	rename from NxmMouArmPress, cleanup	*
 * S. Law/GSC		04/99	added client_data to event handler	*
 * S. Law/GSC		09/99	added cursor parameter			*
 ***********************************************************************/
{
    _dynActiveFlag = FALSE;

    if (_pressFunc) {
        mcanvw_disarmPress();
    }

    _pressFunc = func;

    mcanvw_setCursor (cursor);

/*
 *  Activate callbacks.
 */
    if (func) {
        XtAddEventHandler(_drawingW, ButtonPressMask, FALSE,
			  func, (XtPointer) MCANVW_PRESS);
    }
}

/*=====================================================================*/

void mcanvw_setDragFunc ( XtEventHandler func, int cursor )
/************************************************************************
 * mcanvw_setDragFunc							*
 *									*
 * This function associates a callback with a mouse drag.		*
 *									*
 * void mcanvw_setDragFunc (func, cursor )				*
 *									*
 * Input parameters:							*
 *	func	XtEventHandler	Pointer to Event Handler function	*
 *	cursor	int		which type of cursor    		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE                                            *
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	04/97	Created					*
 * E. Wehner/EAi	09/97	stop passing grinfo record		*
 * C. Lin/EAI		10/97	rename from NxmMouDrag, cleanup		*
 * S. Law/GSC		04/99	added client_data to event handler	*
 * S. Law/GSC		09/99	added cursor parameter			*
 ***********************************************************************/
{
    _dynActiveFlag = FALSE;

    if (_dragFunc) {
        mcanvw_disarmDrag();
    }

    _dragFunc = func;

    mcanvw_setCursor (cursor);

/*
 *  Activate callbacks.
 */
    if (func) {
        XtAddEventHandler(_drawingW, ButtonMotionMask, FALSE,
			  func, (XtPointer) MCANVW_DRAG);
    }
}

/*=====================================================================*/

void mcanvw_setDropFunc ( XtEventHandler func, int cursor )
/************************************************************************
 * mcanvw_setDropFunc							*
 *									*
 * Activates a callback function based on the release of a mouse	*
 * button.								*
 *									*
 * void mcanvw_setDropFunc ( func, cursor)				*
 *									*
 * Input parameters:							*
 *	func	XtEventHandler	Pointer to Event Handler function	*
 *	cursor	int		which type of cursor     		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	04/97	Created					*
 * E. Wehner/EAi	09/97	stop passing grinfo record		*
 * C. Lin/EAi		10/97	rename from NxmMouDrop, cleanup		*
 * S. Law/GSC		04/99	added client_data to event handler	*
 * S. Law/GSC		09/99	added cursor parameter			*
 ***********************************************************************/
{
    _dynActiveFlag = FALSE;

    if (_dropFunc) {
        mcanvw_disarmDrop();
    }

    _dropFunc = func;

    mcanvw_setCursor (cursor);

/*
 *  Activate callbacks.
 */
    if (func) {
        XtAddEventHandler(_drawingW, ButtonReleaseMask, FALSE, 
			  func, (XtPointer) MCANVW_DROP);
    }
}

/*=====================================================================*/

void mcanvw_setDynamicFunc ( XtEventHandler start_func,
			     XtEventHandler drag_func,
			     XtEventHandler drop_func, int cursor )
/************************************************************************
 * mcanvw_setDynamicFunc						*
 *									*
 * This function associates a callback function with the mouse motion	*
 * action.								*
 *									*
 * void mcanvw_setDynamicFunc (start_func, drag_func, drop_func, cursor)*
 *									*
 * Input parameters:							*
 *	start_func	XtEventHandler	function for MB press		*
 *	drag_func	XtEventHandler	function for MB in motion	*
 *	drop_func	XtEventHandler	function for MB release		*
 *	cursor	        int	which type of cursor     		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAi		10/97	based on NxmMouArmDynamic		*
 * E. Safford/GSC	04/99	fix irix6 compiler warnings & clean up	*
 * S. Law/GSC		04/99	removed redundant checks		*
 * S. Law/GSC		09/99	added cursor parameter			*
 ***********************************************************************/
{
/*
 *  Activate callbacks.
 */
    mcanvw_setPressFunc((XtEventHandler)start_func, cursor);
    mcanvw_setDragFunc((XtEventHandler)drag_func, cursor);
    mcanvw_setDropFunc((XtEventHandler)drop_func, cursor);
}

/*=====================================================================*/

void mcanvw_setDynActFlag ( Boolean dynflg )
/************************************************************************
 * mcanvw_setDynActFlag                                                 *
 *                                                                      *
 * This function sets the dynamics active flag.                 	*
 *                                                                      *
 * void mcanvw_setDynActFlag( dynflg )                                  *
 *                                                                      *
 * Input parameters:                                                    *
 * 	dynflg	Boolean		dynamic act flag			*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI    10/97                                         		*
 ***********************************************************************/
{
    _dynActiveFlag = dynflg;
}

/*=====================================================================*/

void mcanvw_disarmPress ( void )
/************************************************************************
 * mcanvw_disarmPress							*
 *									*
 * This function disarms the call back that is associated with a mouse	*
 * action.								*
 *									*
 * void mcanvw_disarmPress ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	04/97	Created					*
 * E. Wehner/EAi	09/97	stop passing grinfo record		*
 * C. Lin/EAI		10/97	rename from NxmMouDisarmPress, cleanup	*
 * S. Law/GSC		04/99	added client_data to event handler	*
 ***********************************************************************/
{
    _dynActiveFlag = FALSE;

    if (_pressFunc) {
	XtRemoveEventHandler(_drawingW, ButtonPressMask, FALSE,
			     (XtEventHandler)_pressFunc, 
			     (XtPointer) MCANVW_PRESS);
    }
    _pressFunc = NULL;
}

/*=====================================================================*/

void mcanvw_disarmDrag ( void )
/************************************************************************
 * mcanvw_disarmDrag							*
 *									*
 * This function removes a callback for dragging the mouse.		*
 *									*
 * void mcanvw_disarmDrag ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	04/97	Created					*
 * E. Wehner/EAi	09/97	stop passing grinfo record		*
 * C. Lin/EAI		10/97	rename from NxmMouDisarmDrag, cleanup	*
 * S. Law/GSC		04/99	added client_data to event handler	*
 ***********************************************************************/
{
    _dynActiveFlag = FALSE;

    if  (_dragFunc) {
	XtRemoveEventHandler(_drawingW, ButtonMotionMask, FALSE,
			     (XtEventHandler)_dragFunc, 
			     (XtPointer) MCANVW_DRAG);
    }
    _dragFunc = NULL;
}

/*=====================================================================*/

void mcanvw_disarmDrop ( void )
/************************************************************************
 * mcanvw_disarmDrop							*
 *									*
 * Removes callback for releasing a mouse button.			*
 *									*
 * void mcanvw_disarmDrop ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	04/97	Created					*
 * E. Wehner/EAi	09/97	stop passing grinfo record		*
 * C. Lin/EAI		10/97	rename from NxmMouDisarmDrop, cleanup	*
 * S. Law/GSC		04/99	added client_data to event handler	*
 ***********************************************************************/
{
    _dynActiveFlag = FALSE;
    if (_dropFunc) {
        XtRemoveEventHandler(_drawingW, ButtonReleaseMask, FALSE,
			     (XtEventHandler)_dropFunc, 
			     (XtPointer) MCANVW_DROP);
    }
    _dropFunc = NULL;
}

/*=====================================================================*/

void mcanvw_disarmDynamic ( void )
/************************************************************************
 * mcanvw_disarmDynamic                                                 *
 *                                                                      *
 * This function removes callbacks associated with mouse motion.	*
 *                                                                      *
 * void mcanvw_disarmDynamic()         					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAi      10/97     						*
 ***********************************************************************/
{
    mcanvw_disarmPress();
    mcanvw_disarmDrag();
    mcanvw_disarmDrop();
}

/*=====================================================================*/

Widget mcanvw_getDrawingW ( void )
/************************************************************************
 * mcanvw_getDrawingW							*
 *                                                                      *
 * This function get the drawing widget ID.      			*
 *                                                                      *
 * Widget mcanvw_getDrawingW()                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * mcanvw_getDrawingW	Widget     drawing widget ID.                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 ***********************************************************************/
{
    return(_drawingW);
}

/*=====================================================================*/

void mcanvw_getDims ( int *width, int *height )
/************************************************************************
 * mcanvw_getDims							*
 *                                                                      *
 * This function gets the dimensions for the main canvas Widget.	*
 *                                                                      *
 * void mcanvw_getDims( width, height)                             	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*width		int	width of _drawingW			*
 *	*height		int	height of _drawingW			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	06/01 	initial coding                          *
 ***********************************************************************/
{
    Dimension	hght, wdth;
/*---------------------------------------------------------------------*/
    XtVaGetValues(_drawingW, XmNheight, &hght, XmNwidth, &wdth, NULL);
    *width  = (int)wdth;
    *height = (int)hght;
}

/*=====================================================================*/

void mcanvw_getCurPos ( float *xx,  float *yy )
/************************************************************************
 * mcanvw_getCurPos							*
 *                                                                      *
 * This function returns the current (last) cursor position.   		*
 *                                                                      *
 * void mcanvw_getCurPos( xx, yy) 	                            	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*xx		float	last x postion    			*
 *	*yy		float	last y position    			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/01 	initial coding                          *
 ***********************************************************************/
{
    *xx = _lastX;
    *yy = _lastY;
}

/*=====================================================================*/

Boolean mcanvw_getDynActFlag ( void )
/************************************************************************
 * mcanvw_getDynActFlag							*
 *                                                                      *
 * This function gets the dynamic act flag.      			*
 *                                                                      *
 * Boolean mcanvw_getDynActFlag()                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * mcanvw_getDynActFlag	Boolean    drawing widget ID.                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      9/96                                                 *
 ***********************************************************************/
{
    return(_dynActiveFlag);
}

/*=====================================================================*/

int mcanvw_getDpth ( void )
/************************************************************************
 * mcanvw_getDpth							*
 *                                                                      *
 * This function returns the DefaultDepth of the display.		*
 *                                                                      *
 * Boolean mcanvw_getDynActFlag()                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		none							*
 * return:                                                              *
 * 		int		DefaultDepth of the display		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	02/04	intial coding                           *
 ***********************************************************************/
{
   static int 	xdpth = -1;
/*---------------------------------------------------------------------*/
   if ( xdpth < 0 ) { 
   	xdpth = DefaultDepth( XtDisplay( _drawingW ), 
		DefaultScreen( XtDisplay( _drawingW ) ));
   }
   return( xdpth );
}

/*=====================================================================*/
/* ARGSUSED */
void mcanvw_exposeCb ( Widget wid, XtPointer clnt, 
				XmDrawingAreaCallbackStruct *call )
/************************************************************************
 * mcanvw_exposeCb                                                  	*
 *                                                                      *
 * Callback function for expose event of main window drawing area.      *
 *                                                                      *
 * void mcanvw_exposeCb (wid, clnt, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid	Widget     			widget ID		*
 *	clnt	XtPointer			not used		*
 *	*call	XmDrawingAreaCallbackStruct	callback data struct	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	04/96                                           *
 * S. Law/GSC		12/99	added call to seekw_refresh		*
 * E. Safford/GSC	01/01	add call to pggst_redrawGhost		*
 * T. Piper/SAIC        12/04   added aodtw_refresh & cldhgtw_refresh   *
 ***********************************************************************/
{
    XEvent *event;
/*---------------------------------------------------------------------*/
    event = call->event;

    if ( event->xexpose.count == 0 ) {

	xmexpo(event);

        if ( pgpalw_isUp() && _dynActiveFlag ) {
	    pggst_enableGhost();
	    pggst_redrawGhost();
  	}
	aodtw_refresh(FALSE);
	cldhgtw_refresh(FALSE);
	seekw_refresh();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mcanvw_resizeCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * mcanvw_resizeCb							*
 *									*
 * Callback function for resize event of main window drawing area.	*
 *									*
 * void mcanvw_resizeCb (wid, clnt, cbs)				*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	not used			*
 *	cbs		XtPointer	callback data struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI      	04/96                                           *
 * E. Safford/GSC	12/99	update for new xwcmn.h			*
 * S. Law/GSC		01/00	replaced with call to xpxm2win		*
 * E. Safford/GSC	01/01	reload all data & base maps on resize	*
 * H. Zeng/EAI          05/01   added call to NxmBusy_setStopBtn()      *
 * E. Safford/GSC	07/01	added pghdlb_deselectAll()              *
 ***********************************************************************/
{
    int	d1, d2, d3, d4, lp, ier;
/*---------------------------------------------------------------------*/

    if ( pgpalw_isUp() ) {
        pghdlb_deselectAll();
    }

    xclear (&d1, &d2, &d3, &d4, &ier); 

    NxmBusy_setStopBtn(0);
    for (lp=0; lp<MAX_LOOP; lp++) {
	loop_setDataChngd (lp, TRUE);
    }
    dataw_loadData();
    NxmBusy_setStopBtn(1);
}

/*=====================================================================*/
/* ARGSUSED */
static void mcanvw_evntHandler ( Widget	wid, XtPointer clnt,
				 XEvent	*event, Boolean	*cont_to_disp )
/************************************************************************
 * mcanvw_evntHandler							*
 *									*
 * Event handler for main window drawing area.				*
 *									*
 * static void mcanvw_evntHandler(wid, clnt, event, cont_to_disp)	*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data (not used)		*
 *	*event		XEvent		event data struct		*
 *									*
 * Output parameters:							*
 *	*cont_to_disp	Boolean		continue to dispatch flag	*	
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/96						*
 * G. Krueger/EAI	09/98	Added ghost veiling			*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * S. Law/GSC		06/00	changed to use mcanvw_setLatlon		*
 * E. Safford/SAIC	01/02	add complete param list 		*
 * H. Zeng/SAIC		01/07	added multi. locators display		*
 * H. Zeng/SAIC		03/07	added call to XtGrabKeyboard()		*
 * T. Piper/SAIC	05/07	removed XtGrabKeyboard() calls		*
 ***********************************************************************/
{

    switch (event->type) {
      case EnterNotify:
	XmProcessTraversal (wid, XmTRAVERSE_CURRENT);
	pggst_enableGhost ();
        mbotw_startLocDspl();

      case MotionNotify:
/*
 * get device coordinates
 */
	_lastX = (float) event->xmotion.x;
	_lastY = (float) event->xmotion.y;

	mcanvw_setLocDspl ();
	break;

      case LeaveNotify:
/*
 * clear out the locator info area and Ghosting.
 */
	mbotw_endLocDspl ();
	pggst_disableGhost ();
	break;
    }
}

/*=====================================================================*/

void mcanvw_catchCtrl ( void )
/************************************************************************
 * mcanvw_catchCtrl							*
 *									*
 * This is a dummy function to catch a Ctrl-Btn1Dwn key combination.	*
 * In Motif 2.1 this key combination is a known fatal bug.  The Motif   *
 * libraries released by most linux implementations contain a vendor    *
 * supplied fix, but Redhat does not yet include such a patch.  This    *
 * routine is linked to the Ctrl Btn1Dwn combination in the Nmap        *
 * resource file.  It is registered for the drawing canvas of the main  *
 * nmap2 window.  In order to completely avoid the bug, every widget    *
 * would have to trap this key combination.  Linking every widget to a  *
 * dummy file seemed like too much effort.  The likely point of failure *
 * is within the main canvas, so that is patched by this function.	*
 *									*
 * void mcanvw_catchCtrl ( void )                               	*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	01/02	initial "coding"       			*
 ***********************************************************************/
{
    return;
}
