#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

extern XtAppContext     _appContext;			 /* app context */
static XtIntervalId     _timeOutId = (XtIntervalId)NULL; /* id for timeout */
static long		_interval  = 500L;		 /* 0.5 sec default */

/*
 * structure for position the popup menu
 */
typedef struct {
    Widget 		menu;
    XButtonPressedEvent event;
} pos_t;
static pos_t pos;

/*
 * Private functions
 */
static void NxmPushBtnMenu_poseCb   ( Widget, XtPointer, XtPointer );
static void NxmPushBtnMenu_poseIt   ( XtPointer, XtIntervalId );
static void NxmPushBtnMenu_unPoseCb ( Widget, XtPointer, XtPointer );

/************************************************************************
 * NxmPushBtnMenu.c                                                     *
 *                                                                      *
 * This module contains functions that create a popup menu attached to	*
 * a push button. It works in this way: when the push button is 	*
 * clicked, there is no menu popped up; when the push button is pressed	*
 * and held for a certain amount of time, a menu is popped up right	*
 * below the push button.						* 
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 * NxmPushBtnMenu_create()        create a menu attached to a push btn	*
 * NxmPushBtnMenu_poseCb()	  push button arm callback		*
 * NxmPushBtnMenu_poseIt()	  pop up the menu			*
 * NxmPushBtnMenu_unPoseCb()	  btn disarm and focus leave callback	*
 ***********************************************************************/

/*=====================================================================*/

void NxmPushBtnMenu_create ( Widget pushb, long interval, char *items[],
                             int nitems, XtCallbackProc callback )
/************************************************************************
 * NxmPushBtnMenu_create                                                *
 *                                                                      *
 * This function creates a popup menu attached to a push button.        *
 *                                                                      *
 * NxmPushBtnMenu_create(pushb, interval, items, nitems, callback)      *
 *                                                                      *
 * Input parameters:                                                    *
 *  pushb               Widget       the push button id                 *
 *  interval		long	     elapsed time to pop up menu	*
 *  *items[]            char         menu items name 		        *
 *  nitems              int          number of items in the menu        *
 *  *callback()         void         callback function for the menu     *
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/02						*
 ***********************************************************************/
{
    long	ii;
    Widget	menu_pane, menu_item;
    Widget	parent;
/*---------------------------------------------------------------------*/
/*
 * make sure at least 0.5 second interval.
 */
    if(interval > _interval) {
	_interval = interval;
    }

/*
 * the menu and push button will have the same parent.
 */
    parent = XtParent(pushb);

/*
 * create the pop up menu.
 */
    menu_pane = XmCreatePopupMenu(parent, "a_menu_pane", NULL, 0);

    for(ii = 0; ii < nitems; ++ii) {
	menu_item = XmCreatePushButtonGadget(menu_pane, items[ii], NULL, 0);
        XtManageChild(menu_item);
	XtAddCallback(menu_item, XmNactivateCallback,
                      (XtCallbackProc)callback,(XtPointer)ii);
    }

/*
 * attach the menu to the push button.
 */
    XtAddCallback(pushb, XmNarmCallback, 
                  (XtCallbackProc)NxmPushBtnMenu_poseCb, menu_pane);
    XtAddCallback(pushb, XmNdisarmCallback,
                  (XtCallbackProc)NxmPushBtnMenu_unPoseCb, menu_pane);
    XtAddEventHandler(pushb, LeaveWindowMask, FALSE,
                      (XtEventHandler)NxmPushBtnMenu_unPoseCb, menu_pane);
}

/*=====================================================================*/

static void NxmPushBtnMenu_poseCb(Widget w, XtPointer clnt, XtPointer call)
/************************************************************************
 * NxmPushBtnMenu_poseCb                                                *
 *                                                                      *
 * This is the push button arm callback.				* 
 *                                                                      *
 * NxmPushBtnMenu_poseCb(w, clnt, call)					*
 *                                                                      *
 * Input parameters:                                                    *
 *  w			Widget       widget ID                          *
 *  clnt		XtPointer    client data			*
 *  call		XtPointer    callback structure			*
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/02						*
 ***********************************************************************/
{
    XRectangle 		displayrect;
    XButtonPressedEvent *event;
/*---------------------------------------------------------------------*/
/*
 * gets the event.
 */
    event = (XButtonPressedEvent *)
            (((XmPushButtonCallbackStruct *)call)->event);

/*
 * computes the position for the popup menu.
 */
    XmWidgetGetDisplayRect(w, &displayrect);
    pos.menu = (Widget)clnt;
    pos.event.x_root = event->x_root - event->x;  /* x of btn orig from root */
    pos.event.y_root = event->y_root - event->y + /* y of btn orig from root */
                       displayrect.height +       /* btn height */
                       8;			  /* extra gap */

    _timeOutId = XtAppAddTimeOut(_appContext, _interval,
                           (XtTimerCallbackProc)NxmPushBtnMenu_poseIt,
                           (XtPointer)&pos);
}

/*=====================================================================*/
/* ARGSUSED */
static void NxmPushBtnMenu_poseIt(XtPointer clnt, XtIntervalId id)
/************************************************************************
 * NxmPushBtnMenu_poseIt                                                *
 *                                                                      *
 * This function actually pops up the menu.				*
 *                                                                      *
 * NxmPushBtnMenu_poseIti ( clnt, id )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  clnt		XtPointer    client data			*
 *  id                  XtIntervalId timeout id 			*
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/02						*
 ***********************************************************************/
{
    pos_t 	*pos_p;
    Widget	popup;
/*---------------------------------------------------------------------*/

    pos_p = (pos_t *)clnt;
    popup = pos_p->menu;

    XmMenuPosition(popup, &(pos_p->event));
    XtManageChild(popup);
}

/*=====================================================================*/
/* ARGSUSED */
static void NxmPushBtnMenu_unPoseCb(Widget w, XtPointer clnt, XtPointer call)
/************************************************************************
 * NxmPushBtnMenu_unPoseCb                                              *
 *                                                                      *
 * This is the push button disarm and focus leave callback.		*
 *                                                                      *
 * NxmPushBtnMenu_unPoseCb(w, clnt, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  w			Widget       widget ID                          *
 *  clnt        	XtPointer    client data			*
 *  call	        XtPointer    callback structure			*
 *                                                                      *
 * Output parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 * Return parameters:                                                   *
 *          NONE                                                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		10/02						*
 ***********************************************************************/
{
    if (_timeOutId != (XtIntervalId)NULL) {
        XtRemoveTimeOut(_timeOutId);
        _timeOutId = (XtIntervalId)NULL;
    }
}
