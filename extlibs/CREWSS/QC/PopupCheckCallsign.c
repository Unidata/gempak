#include "gui.h"

/************************************************************************
 *                                                                      *
 * Create_Popup_CheckCallsign                                           *
 *   This routine creates the popup dialog for the CheckCallsign        *
 *   function.                                                          *
 *                                                                      *
 ***********************************************************************/

void Create_Popup_CheckCallsign(void)
{
   int n;
   Arg args[6];

   n=0;
   XtSetArg(args[n], XmNheight, 100);n++;
   XtSetArg(args[n], XmNwidth,  360);n++;
   XtSetArg(args[n], XmNx, 500);n++;
   XtSetArg(args[n], XmNy, 0);n++;
   CheckCallsignShell = XmCreateDialogShell( toplevel, "CheckCallsign", args, n);
   CheckCallsign = XmCreateBulletinBoard( CheckCallsignShell, "CheckCallsign", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 20);n++;
   XtSetArg(args[n], XmNy, 10);n++;
   XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Enter callsign (all CAPS):"));n++;
   CheckCallsignLabel =XmCreateLabel(CheckCallsign, " callsign label", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 230);n++;
   XtSetArg(args[n], XmNy,  0);n++;
   XtSetArg(args[n], XmNheight, 40);n++;
   XtSetArg(args[n], XmNwidth,  100);n++;
   CheckCallsignText = XmCreateTextField(CheckCallsign, "CallsignText", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 100);n++;
   XtSetArg(args[n], XmNy, 65);n++;
   CheckCallsignGo = XmCreatePushButton( CheckCallsign, "Go", args, n);
   XtAddCallback( CheckCallsignGo, XmNactivateCallback,
				(XtCallbackProc)GetCheckCallsignCb, NULL);

   n=0;
   XtSetArg(args[n], XmNx, 175);n++;
   XtSetArg(args[n], XmNy, 65);n++;
   CheckCallsignDismiss = XmCreatePushButton( CheckCallsign, "Dismiss", args, n);
   XtAddCallback( CheckCallsignDismiss, XmNactivateCallback,
				(XtCallbackProc)CheckCallsignExitCb, NULL);

   XtManageChild(CheckCallsignLabel);
   XtManageChild(CheckCallsignText);
   XtManageChild(CheckCallsignGo);
   XtManageChild(CheckCallsignDismiss);
}

/*=====================================================================*/
/*ARGSUSED*/
void PopupCheckCallsignCb ( Widget widget, int tag, 
				XmPushButtonCallbackStruct *cb_data)
/************************************************************************
 *                                                                      *
 * PopupCheckCallsignCb                                                 *
 *   This callback routine manages the CheckCallsign widget.            *
 *                                                                      *
 ***********************************************************************/
{

   Turn_Off_Select_Group(); /* Select Group mode is on, turn it off */

   if (SaveObsChanges()) {
      XtVaSetValues( CheckCallsign, XmNx,500 , XmNy, 0 , NULL);
      XtManageChild(CheckCallsign);
      XtManageChild(CheckCallsignLabel);
      XmTextFieldSetString(CheckCallsignText, "");
      XtManageChild(CheckCallsignText);
      XtManageChild(CheckCallsignDismiss);
      XtManageChild(CheckCallsignGo);
   }


}

/*=====================================================================*/
/* ARGSUSED */
void GetCheckCallsignCb ( Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 *                                                                      *
 * GetCheckCallsignCb                                                   *
 *   This routine retrieves the requested callsign, gets the latest     *
 *   obs received for that callsign, displays it, and displays the      *
 *   Cruise/History for that callsign.                                  *
 *                                                                      *
 ***********************************************************************/
{
  char *callsign;
  int  Latest;

  callsign = XmTextFieldGetString(CheckCallsignText);
  if (strlen(callsign) > (size_t)0) {
     XtUnmanageChild(CheckCallsign);
     Latest = Get_Latest(callsign);
     if (Latest >= 0) {
        Previous_View_Obs = Current_View_Obs;
        Previous_Pri_Obs = Current_Pri_Obs;
        Current_View_Obs = Latest;     
        DisplayObsValues(Current_View_Obs);
     } else {
        ClearFields(); 
     }
     HistoryCruise(callsign); 
  }
}

/*=====================================================================*/
/* ARGSUSED */
void CheckCallsignExitCb (Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 *                                                                      *
 * CheckCallsignExit                                                    *
 *    This routine unmanages the CheckCallsign dialog box.              *
 *                                                                      *
 ***********************************************************************/
{

  XtUnmanageChild(CheckCallsign);
  XtUnmanageChild(CheckCallsignLabel);
  XtUnmanageChild(CheckCallsignText);
  XtUnmanageChild(CheckCallsignGo);
  XtUnmanageChild(CheckCallsignDismiss);

}
