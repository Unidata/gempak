#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


/************************************************************************
 * NxmClose.c								*
 *									*
 * This module handles popup close functions.				*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmClose_menuReset()	Ties the CLOSE item to a function.	*
 *   NxmClose_menuRmEntry()	Removes CLOSE item from WM menu.	*
 *   NxmClose_popupCb()		Closes a popup widget with CLOSE button	*
 ***********************************************************************/

/*=====================================================================*/

void NxmClose_menuReset ( Widget shell, XtCallbackProc func, XtPointer call )
/************************************************************************
 * NxmClose_menuReset							*
 *									*
 * This function ties the close item of the window manager menu to the	*
 * specified function.							*
 *									*
 * void NxmClose_menuReset(shell, func, call) 				*
 *									*
 * Input parameters:							*
 *	shell	Widget		widget ID of the window			*
 *	func	XtCallbackProc	callback function for the menu		*
 *	call	XtPointer	input data to the function		*
 *									*
 * Output parameters:							*
 *	    NONE							*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    08/95						*
 * G. Krueger/EAI   09/97  NxmMCloseReset -> NxmMclose_menuReset; Clean	*
 ***********************************************************************/
{
Atom	WM_DELETE_WINDOW;
/*---------------------------------------------------------------------*/

	XtVaSetValues(shell,
		XmNdeleteResponse, XmDO_NOTHING,
		NULL);
	WM_DELETE_WINDOW = XInternAtom(XtDisplay(shell),
		"WM_DELETE_WINDOW", False);
	XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW,
		func, call);
}

/*=====================================================================*/

void NxmClose_menuRmEntry ( Widget shell )
/************************************************************************
 * NxmClose_menuRmEntry 						*
 *									*
 * This function takes out the close item from the window manager menu. *
 *									*
 * void NxmClose_menuRmEntry(shell)					*
 *									*
 * Input parameters:							*
 *	shell		Widget	   widget ID of the window		*
 *									*
 * Output parameters:							*
 *	    NONE							*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    08/95						*
 * G. Krueger/EAI   09/97  NxmMcloseNone -> NxmClose_menuRmEntry	*
 ***********************************************************************/
{
long func;
/*---------------------------------------------------------------------*/

    XtVaGetValues(shell, XmNmwmFunctions, &func, NULL);
    func &= ~MWM_FUNC_CLOSE;
    XtVaSetValues(shell, XmNmwmFunctions, func, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmClose_popupCb ( Widget wdgt, Widget popup, XtPointer call )
/************************************************************************
 * NxmClose_popupCb							*
 *									*
 * This function closes the input popup widget.  It is used for the 	*
 * close button.							*
 *									*
 * void NxmClose_popupCb(wdgt, popup, call)				*
 *									*
 * Input parameters:							*
 *	wdgt	Widget	   widget ID of the close button		*
 *	popup	Widget	   widget ID of the popup to be closed		*
 *	call	XtPointer  Data from the widget				*
 *									*
 * Output parameters:							*
 *	    NONE							*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI   09/97  Added comment block				*
 ***********************************************************************/
{
    XtUnmanageChild( popup );
}
