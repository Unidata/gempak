#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

void _NxmWarn_ok_cb ( Widget, XtPointer, XtPointer );

/************************************************************************
 * NxmWarn.c								*
 *									*
 * This module handles warnings.					*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmWarn_show()	Create a WARNING dialog.			*
 *   _NxmWarn_ok_cb()	Destroy the WARNING dialog.			*
 ***********************************************************************/

/*=====================================================================*/

void NxmWarn_show ( Widget parent, char *message )
/************************************************************************
 * NxmWarn_show								*
 *									*
 * This function displays a warning message.				*
 *									*
 * void NxmWarn_show( parent, message ) 				*
 *									*
 * Input parameters:							*
 *	parent		Widget	ID of parent widget.			*
 *	*message	char 	Warning message to be displayed		*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    05/94						*
 * C. Lin/EAI	    06/96  add XmUpdateDisplay				*
 * C. Lin/EAI	    11/96  add XmDIALOG_SYSTEM_MODAL			*
 * C. Lin/EAI	    12/96  manage the parent if it is not managed	*
 * G. Krueger/EAI   09/97  NxmWarning->NxmWarn_show; Clean up		*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
    Cardinal	argcnt;
    Arg		args[10];
    XmString	xmwarning, xmtitle;
    Widget	warning, child;
/*---------------------------------------------------------------------*/

    xmwarning = XmStringCreateLtoR(message, XmFONTLIST_DEFAULT_TAG);
    xmtitle = XmStringCreateLocalized ("Warning");
    argcnt = 0;
    XtSetArg(args[argcnt], XmNdialogType, XmDIALOG_WARNING); argcnt++;
    XtSetArg(args[argcnt], XmNmessageAlignment,
	     XmALIGNMENT_CENTER); argcnt++;
    XtSetArg(args[argcnt], XmNmessageString, xmwarning); argcnt++;
    XtSetArg(args[argcnt], XmNdialogStyle, 
	     XmDIALOG_FULL_APPLICATION_MODAL); argcnt++;
    XtSetArg(args[argcnt], XmNdialogTitle, xmtitle); argcnt++;

    warning = XmCreateWarningDialog( parent, "Warning",	args, argcnt);

    XtAddCallback(warning, XmNokCallback, _NxmWarn_ok_cb, NULL);

    XmStringFree (xmwarning);
    XmStringFree (xmtitle);

    child = XmMessageBoxGetChild(warning, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(child);

    child = XmMessageBoxGetChild(warning, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild(child);

    if (!XtIsManaged(parent)) XtManageChild(parent);

    XtManageChild(warning);
    XmUpdateDisplay(warning);
}

/*=====================================================================*/
/* ARGSUSED */
void _NxmWarn_ok_cb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * _NxmWarn_ok_cb							*
 *									*
 * This function destroys a warning dialog.				*
 *									*
 * void _NxmWarn_ok_cb( wdgt, clnt, call )	 			*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		ID of widget to be destroyed		*
 *	clnt	XtPointer	Data from NxmWarn_show			*
 *	call	XtPointer	Data from the widget			*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * G. Krueger/EAI   08/97  NxmWarningCallback->NxmWarn_ok_cb; Clean up	*
 ***********************************************************************/
{
    XtDestroyWidget(wdgt);
}
