#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


Widget NxmExit_create ( Widget parent, char *title, char *message, 
				void (*ok_cb)(Widget,XtPointer,XtPointer), 
				void (*cancel_cb)(Widget,XtPointer,XtPointer) )
/************************************************************************
 * NxmExit_create							*
 *									*
 * This function creates an EXIT confirmation dialog.			*
 *									*
 * Widget NxmExit_create( parent, title, message, ok_cb, cancel_cb )	*
 *									*
 * Input parameters:							*
 *	parent		Widget	parent widget.				*
 *	*title		char	dialog title.				*
 *	*message	char	message displayed in dialog.		*
 *	*ok_cb()	void	callback function of OK button.		*
 *      *cancel_cb()    void    callback function of CANCEL button.     *
 *									*
 * Return parameters:							*
 *	NxmExit_create	Widget  The dialog widget			*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    05/94						*
 * C. Lin/EAI	    10/96	add MODAL dialog style			*
 * G. Krueger/EAI   09/97	NxmExitDialog->NxmExit_create; Cleanup	*
 * S. Law/GSC		07/00	set dialog title			*
 * H. Zeng/EAI      02/01       added cancel callback                   *
 ***********************************************************************/
{
    Widget	dialog, button;
    Cardinal	argcnt;
    Arg		args[10];
    XmString	xmstr, xmtitle;
/*---------------------------------------------------------------------*/

    argcnt = 0;
    xmstr = XmStringCreateLtoR(message, XmFONTLIST_DEFAULT_TAG);
    XtSetArg(args[argcnt], XmNmessageString, xmstr); argcnt++;
    xmtitle = XmStringCreateLocalized(title);
    XtSetArg(args[argcnt], XmNdialogTitle, xmtitle); argcnt++;
    XtSetArg(args[argcnt], XmNnoResize, True); argcnt++;
    XtSetArg(args[argcnt], XmNdialogStyle, XmDIALOG_SYSTEM_MODAL); argcnt++;

    dialog = XmCreateQuestionDialog(parent, "Quit", args, argcnt);

    XmStringFree (xmstr);
    XmStringFree (xmtitle);

    XtAddCallback(dialog, XmNokCallback, ok_cb, NULL);

    if( cancel_cb != NULL ) {
        XtAddCallback(dialog, XmNcancelCallback, cancel_cb, NULL);
    }

    button = XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);

    XtUnmanageChild(button);

    XtManageChild(dialog);

    return(dialog);
}
