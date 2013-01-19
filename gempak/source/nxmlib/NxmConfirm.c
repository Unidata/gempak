#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


/*
 *  Private functions
 */
void _NxmConfirm_cancelCb ( Widget, XtPointer (clnt)(void), XtPointer );


/************************************************************************
 * NxmConfirm.c 							*
 *									*
 * This module handles confirmation of user actions.			*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmConfirm_show()		Displays confirmation message widget	*
 *   _NxmConfirm_cancelCb()	Destroys confirmation message widget	*
 ***********************************************************************/

/*=====================================================================*/

void NxmConfirm_show ( Widget parent, char *message, XtCallbackProc func_ok, 
			XtCallbackProc func_cancel, XtPointer clnt, int *iret ) 
/************************************************************************
 * NxmConfirm_show 							*
 *									*
 * This function displays a confirmation message widget.		*
 *									*
 * void NxmConfirm_show ( parent, message, func_ok, func_cancel, clnt,	*
 *		     						iret )	*
 *									*
 * Input parameters:							*
 *	parent		    Widget	ID of parent widget		*
 *	*message	    char	Warning message to be displayed *
 *	*func_ok()	    void	Function to be executed 	*
 *	*func_cancel()      void	Function to execute on cancel	*
 *					 = NULL -> no function		*
 *	clnt		    XtPointer	Client data			*
 *									*
 * Output parameters:							*
 *	*iret		    int		Return code			*
 *									*
 **									*
 * Log: 								*
 * D. Keiser/GSC	 6/97						*
 * G. Krueger/EAI	10/97	NxmConfirm->NxmConfirm_show; Cleanup	*
 * G. Krueger/EAI	 6/98	Added optional cancel function callback	*
 * E. Safford/GSC	04/99	set iret to 0				*
 * S. Law/GSC		07/00	added XmStringFree calls		*
 ***********************************************************************/
{
    Cardinal	argcnt;
    Arg		args[10];
    XmString	xmconfirm, xmtitle;
    Widget	confirm, child;
/*---------------------------------------------------------------------*/

    *iret = 0;
    xmconfirm = XmStringCreateLtoR ( message, XmFONTLIST_DEFAULT_TAG );
    argcnt = 0;
    XtSetArg ( args[argcnt], XmNdialogType, XmDIALOG_QUESTION );argcnt++;
    XtSetArg ( args[argcnt], XmNmessageAlignment, 
	       XmALIGNMENT_CENTER ); argcnt++;
    XtSetArg ( args[argcnt], XmNmessageString, xmconfirm ); argcnt++;
    XtSetArg ( args[argcnt], XmNdialogStyle,
	       XmDIALOG_FULL_APPLICATION_MODAL ); argcnt++;
    xmtitle = XmStringCreateLocalized ("Confirm");
    XtSetArg(args[argcnt], XmNdialogTitle, xmtitle); argcnt++;

    confirm = XmCreateQuestionDialog ( parent, "dialog", args, argcnt );

    XtAddCallback ( confirm, XmNokCallback, func_ok, clnt );
    XtAddCallback ( confirm, XmNcancelCallback, 
		    (XtCallbackProc)_NxmConfirm_cancelCb,
		    (XtPointer)func_cancel );

    child = XmMessageBoxGetChild ( confirm, XmDIALOG_HELP_BUTTON );
    XtUnmanageChild ( child );

    XmStringFree (xmconfirm);
    XmStringFree (xmtitle);

    if (!XtIsManaged(parent)) XtManageChild ( parent );

    XtManageChild ( confirm );
    XmUpdateDisplay ( confirm );

}

/*=====================================================================*/
/* ARGSUSED */
void _NxmConfirm_cancelCb ( Widget wdgt, XtPointer (clnt)(void), 
							XtPointer call )
/************************************************************************
 * _NxmConfirm_cancelCb 						*
 *									*
 * This function destroys a confirmation message widget.  This is used	*
 * with NxmConfirm_show's CANCEL button widget. 			*
 *									*
 * void _NxmConfirm_cancelCb ( wdgt, clnt, call )			*
 *									*
 * Input parameters:							*
 *	wdgt		Widget		ID of widget to be destroyed	*
 *	clnt()		XtPointer	Optional callback function	*
 *	call		XtPointer	Data from the widget		*
 *									*
 * Output parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * D. Keiser/GSC	 6/97						*
 * G. Krueger/EAI	10/97	_NxmConfirmCancelCallback ->		*
 *				_NxmConfirm_cancelCb; Add header 	*
 * G. Krueger/EAI	 6/98	Added optional cancel function callback	*
 ***********************************************************************/
{
    XtDestroyWidget(wdgt);
    if ( clnt != NULL ) clnt();
}
