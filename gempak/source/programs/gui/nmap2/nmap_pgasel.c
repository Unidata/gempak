#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "pgprm.h"
#include "hints.h"
#include "drwids.h"
#include "vgstruct.h"
#include "proto_xw.h"


#define		BEGIN_DRAG 	15.0F
#define		DRAG_PTS	5
#define		STNMDL	        10

static float	_dragX[DRAG_PTS];
static float	_dragY[DRAG_PTS];
static float	_dragStartX, _dragStartY;

static Boolean  _startSel = FALSE;

static int	_irregPt;      /* irregular area points */
static int	_maxAllowPts   = (MAXPTS - 1); /* functional limit */
static float	_xPtsBuf[MAXPTS];
static float	_yPtsBuf[MAXPTS];

/*
 *  Private Callback Functions
 */
static void pgasel_eventHdl    ( Widget, XtPointer, XEvent*, Boolean* );
static void pgasel_dragEh      ( Widget, XtPointer, XEvent*, Boolean* ); 
static void pgasel_dropEh      ( Widget, XtPointer, XEvent*, Boolean* );
static void pgasel_irregStartEh( Widget, XtPointer, XEvent*, Boolean* );
static void pgasel_irregDragEh ( Widget, XtPointer, XEvent*, Boolean* ); 
static void pgasel_irregDropEh ( Widget, XtPointer, XEvent*, Boolean* );

/*
 *  Private functions for single/multiple selections.
 */
static void (*_selectSingleFunc) ( float lat, float lon); 
static void (*_selectAreaFunc) ( int np, const float lat[], const float lon[]); 
static void (*_selectDoneFunc) ( void ); 
    
/************************************************************************
 * nmap_pgasel.c                                                        *
 *									*
 * This module contains the general utility event_handling/callback     *
 * functions that are used for area selection which includes single and *
 * multiple selection by drag or shift-click in product generation.     *
 * This is used in multi-select, group, number edit, or	list creating/  *
 * editing operations.  It is upon the caller to provide proper         *
 * post-selection processing functions.					*
 *									*
 * This module was renamed from nmap_pgmsel.c.				*
 *									*
 *									*
 * CONTENTS:                                                            *
 * pgasel_start()	register post-selection processing functions	*
 * 									*
 * pgasel_eventHdl()	  set up drag/drop event handlers		*
 * pgasel_dragEh()	  event handler for drag			*
 * pgasel_dropEh()	  event handler for drop			*
 * pgasel_irregStartEh()  event handler for press of shift-click area selection*
 * pgasel_irregDragEh()   event handler for drag of shift-click area selection	*
 * pgasel_irregDropEh()   event handler for drop of shift-click area selection	*
 ***********************************************************************/
     
/*=====================================================================*/

void pgasel_start ( void (*_selSingleFunc)( float lat, float lon),
	void (*_selAreaFunc)( int np, const float flat[], const float flon[]),
	void (*_selDoneFunc)( void ) )
/************************************************************************
 * pgasel_start 							*
 *									*
 * This function registers the pass-in functions so that they can be 	*
 * called for the described events (single selection, area drag		*
 * selection, and selection done). 					*
 *                                                                      *
 * Single Selection - Defined by a single button 1 press->release.	*
 *		      Used to select a single object.			*
 * Area Selection   - Defined by drawing:				*
 *                    (1) a drag box 					*
 *                           begin  - a button 1 press			*
 *			     follow - motionNotify			*
 *			     end    - a button 1 release		*  
 *                    (2) a closed figure				*
 *			     begin - a Shift & button 1 press		*
 *                           follw - more button 1 presses, each press	*
 *			             drops a new point			*
 *			     end   - a button 2 press			*
 *                    Used to select multiple objects contained within	*
 *                    a closed figure.					*
 * Selection Done   - Defined by a button 2 press.			*
 *		      Used to exit the selection.			*
 *                                                                      *
 * pgasel_start ( _selSingleFunc, _selAreaFunc, _selDoneFunc )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*_selSingleFunc()	void	called for single selection 	*
 *	*_selAreaFunc()		void	called for drag or shift-click	*
 *	*_selDoneFunc()		void	called when selection is done	*
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                        			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/04	initial coding				*
 * E. Safford/SAIC	07/04	pgmsel_start -> pgasel_start		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
/*
 *  Register post-selection processing functions.
 */
    _selectSingleFunc = _selSingleFunc;     
    _selectAreaFunc   = _selAreaFunc; 
    _selectDoneFunc   = _selDoneFunc;     
    
/*
 *  Set up event handler.
 */
    mcanvw_setPressFunc ( (XtEventHandler)&pgasel_eventHdl, CURS_DEFAULT );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgasel_eventHdl ( Widget w, XtPointer clnt, 
				  XEvent *event, Boolean *ctdr )
/************************************************************************
 * pgasel_eventHdl 							*
 *									*
 * This function sets up the callbacks for doing a single selection	*
 * (button 1 press-release) or area selection (buton 1 press-drag or 	*
 * shift+click).  The functions defined for single/area	selections	*
 * will be used to process results, if they are not NULL functions.	*  
 *                                                                      *
 * static pgasel_eventHdl ( w, clnt, event, ctdr )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*continue	Boolean                                         *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/04	revised from pgdsel_start		*
 ***********************************************************************/
{
    int		dummy, ier, xoff, yoff;
    float	distance, xx, yy, xlat, xlon;
/*---------------------------------------------------------------------*/

    xgtoff ( &xoff, &yoff, &ier );
    xlat = (float)(event->xbutton.x + xoff);
    xlon = (float)(event->xbutton.y + yoff);
       
/*
 * First check if it is irregular area selection (shift+click).
 */
    if ( event->type == ButtonPress && event->xbutton.button == Button1 
                                    && event->xbutton.state == ShiftMask ) {

	_maxAllowPts = MAXPTS - 1;
	pggst_veilGhost ( TRUE );

        _irregPt = 0;   
	pggst_clearGhost ( TRUE );
        pggst_setLineAttr ( 0, TRUE );	

        _xPtsBuf[0] = (float)(event->xbutton.x + xoff);
        _yPtsBuf[0] = (float)(event->xbutton.y + yoff);
        _xPtsBuf[1] = (float)(event->xbutton.x + xoff);
        _yPtsBuf[1] = (float)(event->xbutton.y + yoff);

	pggst_addGhostPts ( 2, _xPtsBuf, _yPtsBuf, &ier ); 
	pggst_drawGhost ( GST_NEW_LINE );
      
        mcanvw_disarmDynamic ();
        mcanvw_setDynamicFunc ( (XtEventHandler)&pgasel_irregStartEh, 
				(XtEventHandler)&pgasel_irregDragEh,
                                (XtEventHandler)&pgasel_irregDropEh,  CURS_DEFAULT );
	mcanvw_setDynActFlag ( TRUE ); 
	mbotw_mouseSet ( LMHINT_NEXT, MMHINT_DONE );
      
        return;

    }

/*
 * If not using irregular area selection, check if it is press-release
 * or rectangular box selection.
 */
    if ( event->type == ButtonPress && event->xbutton.button == Button1 ) {
	
	_dragStartX = (float) (event->xbutton.x + xoff);
	_dragStartY = (float) (event->xbutton.y + yoff); 
        _startSel = TRUE;

	mcanvw_setDragFunc ( &pgasel_eventHdl, CURS_DEFAULT );
	mcanvw_setDropFunc ( &pgasel_eventHdl, CURS_DEFAULT );
    }


    if ( _startSel ) {
        
	if ( event->type == MotionNotify ) {

/*
 *  This is second or later call in a selection.  Compare the 
 *  distance from current cursor position to start position.  If it
 *  exceeds tolerance, decide this is a drag operation and start _dragCb.
 */
	    xx = (float) (event->xbutton.x + xoff);
	    yy = (float) (event->xbutton.y + yoff);

	    cgr_dist ( 1, &_dragStartX, &_dragStartY, 
					xx, yy, &distance, &dummy, &ier );

	    if ( distance >= BEGIN_DRAG ) {
                
		mcanvw_disarmDrag ();
	        mcanvw_disarmDrop ();

	        _dragX[0] = _dragX[3] = _dragX[4] = _dragStartX;
	        _dragY[0] = _dragY[1] = _dragY[4] = _dragStartY;
	        _dragX[1] = _dragX[2] = (float) (event->xbutton.x + xoff); 
	        _dragY[3] = _dragY[2] = (float) (event->xbutton.y + yoff);

    	        pggst_clearGhost ( TRUE );
                pggst_addGhostPts ( 5, _dragX, _dragY, &ier );
	        pggst_drawGhost ( GST_NORMAL );

	        mcanvw_setDragFunc ( (XtEventHandler)&pgasel_dragEh, CURS_DEFAULT );
	        mcanvw_setDropFunc ( (XtEventHandler)&pgasel_dropEh, CURS_DEFAULT );
	    }
        }
        else if ( event->type == ButtonRelease ) { 

/*
 *  This indicates a single selection by mouse press-release.
 *  Process it using _selectSingleFunc().
 */ 
  	    _startSel = FALSE;
	    
	    if ( _selectSingleFunc != NULL ) {
	        _selectSingleFunc ( xlat, xlon );
	    }
	}

    } 
    else if ( event->type == ButtonPress && 
    	      event->xbutton.button == Button2 ) {        
/*
 *  This indicates a intent to exit the selection.
 *  Process it using _selectDoneFunc().
 */ 
	if ( _selectDoneFunc != NULL ) {
	    _selectDoneFunc ();
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgasel_irregStartEh ( Widget w, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * pgasel_irregStartEh                                                  *
 *                                                                      *
 * Internal callback of button press for irregular area selection       *
 *                                                                      *
 * static void pgasel_irregStartiEh ( w, clnt, event, ctdr )            *
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/04	copy from pgdsel_irregStart		*
 ***********************************************************************/
{
    int   	ier, xoff, yoff;
    float	xx, yy;
    char	mesg[] = {"Maximum points exceeded."};
/*---------------------------------------------------------------------*/
/*
 * Make sure event is triggered from left button.
 */
    if ( event->xbutton.button == Button1 ) {

	xgtoff ( &xoff, &yoff, &ier );
	xx = (float) (event->xbutton.x + xoff); 
	yy = (float) (event->xbutton.y + yoff); 
    
	if ( _irregPt + 2  >= _maxAllowPts ) {
	    NxmWarn_show ( w, mesg );
	    mbotw_mouseSet ( LMHINT_ERROR, MMHINT_DONE );
	}
	else {
	    _irregPt++;
	    _xPtsBuf[_irregPt +1] = _xPtsBuf[_irregPt] = xx;
	    _yPtsBuf[_irregPt +1] = _yPtsBuf[_irregPt] = yy;

	    pggst_drawGhost ( GST_NEW_LINE );
	    pggst_replaceGhostPts ( 1, &xx, &yy, &ier );
	    pggst_addGhostPts ( 1, &xx, &yy, &ier );
	    pggst_drawGhost ( GST_NEW_LINE );
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgasel_dragEh ( Widget w, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * pgasel_dragEh       							*
 *									*
 * This function handles a drag in multi-selct by drag.                 *
 *                                                                      *
 * static pgasel_dragEh ( w, clnt, event, ctdr )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/04	copy from pgdsel_dragCb			*
 ***********************************************************************/
{
    int	ier, xoff, yoff;
/*---------------------------------------------------------------------*/

    pggst_drawGhost ( GST_NORMAL );

    xgtoff ( &xoff, &yoff, &ier );
    _dragX[1] = _dragX[2] = (float) (event->xbutton.x + xoff);
    _dragY[3] = _dragY[2] = (float) (event->xbutton.y + yoff);

    pggst_replaceGhostPts ( 5, _dragX, _dragY, &ier );
    pggst_drawGhost ( GST_NORMAL );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgasel_irregDragEh ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgasel_irregDragEh                                                   *
 *                                                                      *
 * Callback of left button drag for irregular area selection.           *
 *                                                                      *
 * static void pgasel_irregDragEh ( w, clnt, event, ctdr )            	*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/04	copy from pgdsel_irregDrag		*
 ***********************************************************************/
{
    int		ier, xoff, yoff;
    float	xx, yy;
/*---------------------------------------------------------------------*/

    xgtoff ( &xoff, &yoff, &ier );
    xx = (float) (event->xbutton.x + xoff); 
    yy = (float) (event->xbutton.y + yoff); 

/*
 *  Erase the previous line
 */
    pggst_drawGhost ( GST_NEW_LINE );

/*
 *  Load the new point(s)
 */
    _xPtsBuf[_irregPt + 1] = xx;
    _yPtsBuf[_irregPt + 1] = yy;

    pggst_replaceGhostPts ( 1, &xx, &yy, &ier );	

/*
 *  Draw the new ghost line
 */
    pggst_drawGhost ( GST_NEW_LINE );
}

/*=====================================================================*/
/* ARGSUSED */
static void pgasel_dropEh ( Widget w, XtPointer clnt, XEvent *event,
						Boolean *ctdr )
/************************************************************************
 * pgasel_dropEh							*
 *									*
 * This function handles a drop in multi-select by drag.                *
 *                                                                      *
 * static pgasel_dropEh ( w, clnt, event, ctdr )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      NULL                                            *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		04/04	modified from pgdsel_dropCb		*
 ***********************************************************************/
{
    int	np = DRAG_PTS;
/*---------------------------------------------------------------------*/
/*
 *  Erase ghost.
 */
    pggst_drawGhost ( GST_NORMAL );		/* erase ghost */
    pggst_clearGhost ( TRUE );

/*
 *  Reset event handler.
 */
    mcanvw_setDynActFlag ( False );
    mcanvw_disarmDynamic ();

    mcanvw_setPressFunc ( (XtEventHandler)&pgasel_eventHdl, CURS_DEFAULT );
    
    _startSel = FALSE;

/*
 *  Process the area selection using _selectAreaFunc().
 */ 
    if ( _selectAreaFunc != NULL ) {
        _selectAreaFunc ( np, _dragX, _dragY );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgasel_irregDropEh ( Widget w, XtPointer clnt, XEvent *event,
							Boolean *ctdr )
/************************************************************************
 * pgasel_irregDropEh							*
 *									*
 * Callback of left button drop for irregular area selection		*
 *									*
 * static void pgasel_irregDropEh (w, clnt, event, ctdr)		*
 *									*
 * Input parameters:							*
 *	w		Widget		Widget that activated callback	*
 *	clnt		XtPointer	Pointer to client data		*
 *	*event		XEvent		Event that triggered callback	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		04/04	modified from pgdsel_irregDrop		*
 ***********************************************************************/
{
/*
 * Make sure event is triggered from middle button.
 */
    if ( event->xbutton.button == Button2 ) {

/*
 *  Erase the previous line
 */
        pggst_touchGhost ();
        pggst_drawGhost ( GST_NORMAL );

        _irregPt++;
        		
        pggst_clearGhost ( TRUE );

/*
 *  Reset the event handler.
 */
	mcanvw_setDynActFlag ( False );
        mcanvw_disarmDynamic ();
	mcanvw_setPressFunc ( (XtEventHandler)&pgasel_eventHdl, CURS_DEFAULT );	
        
/*
 *  Add final point to make _xPtsBuf&_yPtsBuf a polygon and
 *  process the area selection using _selectAreaFunc().
 */
	if ( _irregPt >= 3 ) {
            _xPtsBuf[_irregPt] = _xPtsBuf[0];
            _yPtsBuf[_irregPt] = _yPtsBuf[0];
            _irregPt++;	
        
	    if ( _selectAreaFunc != NULL ) {
	        _selectAreaFunc ( _irregPt, _xPtsBuf,  _yPtsBuf );
	    }
	}	
    }  
}
