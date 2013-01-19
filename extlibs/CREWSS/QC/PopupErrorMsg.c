#include "gui.h"


void Create_Popup_ErrorMsg(void)
{
   int n;
   Arg args[10];
/*---------------------------------------------------------------------*/
   n=0;
   XtSetArg(args[n], XmNheight, 100);n++;
   XtSetArg(args[n], XmNwidth,  500);n++;
/* XtSetArg(args[n], XmNx, 600);n++;
   XtSetArg(args[n], XmNy, 200);n++;
*/
   ErrorMsgShell = XmCreateDialogShell( toplevel, "Message", args, n);
   ErrorMsg = XmCreateBulletinBoard( ErrorMsgShell, "Message", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 0);n++;
   XtSetArg(args[n], XmNy, 0);n++;
   XtSetArg(args[n], XmNwidth,  500);n++;
   XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Error text"));n++;
   XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING);n++;
   ErrorMsgLabel =XmCreateLabel(ErrorMsg, "error text", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 240);n++;
   XtSetArg(args[n], XmNy, 55);n++;
   ErrorMsgDismiss = XmCreatePushButton( ErrorMsg, "Dismiss", args, n);
   XtAddCallback( ErrorMsgDismiss, XmNactivateCallback, (XtCallbackProc)ErrorMsgExitCb,  NULL);

   XtManageChild(ErrorMsgLabel);
   XtManageChild(ErrorMsgDismiss);

}

/*=====================================================================*/

void PopupErrorMsg( char *ErrorText)
/************************************************************************
 *                                                                      *
 * PopupErrorMsg                                                        *
 *   This callback routine pops up the ErrorMsg widget and displays     *
 *   the text.                                                          *
 *                                                                      *
 ***********************************************************************/

{

   Turn_Off_Select_Group(); /* Select Group mode is on, turn it off */

/* XtVaSetValues( ErrorMsg, XmNx,600 , XmNy, 200, NULL);  */
   XtVaSetValues( ErrorMsg, XmNx,600 , XmNy, 200, XmNwidth,  500, NULL);  
   XtVaSetValues( ErrorMsgLabel, XmNlabelString, XmStringCreateLocalized(ErrorText));
   XtManageChild(ErrorMsg);
   XtManageChild(ErrorMsgLabel);
   XtManageChild(ErrorMsgDismiss);


}

/*=====================================================================*/

/* ARGSUSED */
void ErrorMsgExitCb(Widget wid, XtPointer client, XtPointer call)
{

   XtUnmanageChild(ErrorMsg);

}
