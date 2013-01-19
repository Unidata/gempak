#include "gui.h"


void Create_Popup_DateCycle(void) 
{

  int n;
  Arg args[6];
/* Create a popup dialog for GetDateCycle */

   n=0;
   XtSetArg(args[n], XmNheight, 190);n++;
   XtSetArg(args[n], XmNwidth,  250);n++;
   XtSetArg(args[n], XmNx, 500);n++;
   XtSetArg(args[n], XmNy, 0);n++;
   DateCycleShell = XmCreateDialogShell( toplevel, "Choose date and cycle", args, n);
   DateCycle = XmCreateBulletinBoard( DateCycleShell, "DateCycle", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 0);n++;
   XtSetArg(args[n], XmNy, 20);n++;
   XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Enter date:"));n++;
   DateLabel =XmCreateLabel(DateCycle, "date label", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 130);n++;
   XtSetArg(args[n], XmNy,  20);n++;
   XtSetArg(args[n], XmNheight, 40);n++;
   XtSetArg(args[n], XmNwidth,  100);n++;
   DateText = XmCreateTextField(DateCycle, "DateText", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 0);n++;
   XtSetArg(args[n], XmNy, 80);n++;
   XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("Enter cycle:"));n++;
   CycleLabel =XmCreateLabel(DateCycle, "time label", args, n);

   n=0;
   XtSetArg(args[n], XmNx, 130);n++;
   XtSetArg(args[n], XmNy,  80);n++;
   XtSetArg(args[n], XmNheight, 40);n++;
   XtSetArg(args[n], XmNwidth,  100);n++;
   CycleText = XmCreateTextField(DateCycle, "CycleText", args, n);


   n=0;
   XtSetArg(args[n], XmNx, 75);n++;
   XtSetArg(args[n], XmNy, 145);n++;
   DateCycleGo = XmCreatePushButton( DateCycle, "Go", args, n);
   XtAddCallback( DateCycleGo, XmNactivateCallback, (XtCallbackProc)GetDateCycle,  NULL);

   n=0;
   XtSetArg(args[n], XmNx, 125);n++;
   XtSetArg(args[n], XmNy, 145);n++;
   DateCycleDismiss = XmCreatePushButton( DateCycle, "Dismiss", args, n);
   XtAddCallback( DateCycleDismiss, XmNactivateCallback, (XtCallbackProc)DateCycleExitCb, NULL);


   XtManageChild(DateLabel);
   XtManageChild(DateText);
   XtManageChild(CycleLabel);
   XtManageChild(CycleText);
   XtManageChild(DateCycleGo);
   XtManageChild(DateCycleDismiss);
}

/*=====================================================================*/
/*ARGSUSED*/
void PopupGetDateCycleCb ( Widget widget, int tag,
				XmPushButtonCallbackStruct *cb_data)
/************************************************************************
 *                                                                      *
 * PopupGetDateCycleCb                                                  *
 *   This callback routine manages the GetDateCycle widget.             *
 *                                                                      *
 ***********************************************************************/
{
  char Today_Str[9], This_Cycle[3];
  struct tm *today;
  time_t t;

   Turn_Off_Select_Group(); /* Select Group mode is on, turn it off */

   if (SaveObsChanges()) {
      t = time((time_t *) NULL);
      today = gmtime(&t);
      if (today->tm_hour < 6)                          strcpy(This_Cycle,"00");
      if ((today->tm_hour >= 6)  && (today->tm_hour < 12)) strcpy(This_Cycle,"06");
      if ((today->tm_hour >= 12) && (today->tm_hour < 18)) strcpy(This_Cycle,"12");
      if (today->tm_hour >= 18)                        strcpy(This_Cycle,"18");

      strftime(Today_Str, 9, "%Y%m%d", today);

      XmTextFieldSetString( DateText, Today_Str );
      XmTextFieldSetString( CycleText, This_Cycle );

      XtVaSetValues( DateCycle, XmNx,500 , XmNy, 0 , NULL);
      XtManageChild(DateLabel);
      XtManageChild(DateText);
      XtManageChild(CycleLabel);
      XtManageChild(CycleText);
      XtManageChild(DateCycleGo);
      XtManageChild(DateCycleDismiss);
      XtManageChild(DateCycle);
   }

}

/*=====================================================================*/

void  GetDateCycle(void)
/* Retrieve the date and cycle.  Insert default current date and cycle into text window. */
{
  char     *Date, *Cycle;

  Date = XmTextFieldGetString(DateText);
  Cycle = XmTextFieldGetString(CycleText);

  if ((strlen(Date)==(size_t)8) && (strlen(Cycle) == (size_t)2) ) {
     XtUnmanageChild(DateCycle);
     Reload(Date, Cycle);
  }
}

/*=====================================================================*/
/* ARGSUSED */
void  DateCycleExitCb(Widget wid, XtPointer client, XtPointer call)
{

      XtUnmanageChild(DateLabel);
      XtUnmanageChild(DateText);
      XtUnmanageChild(CycleLabel);
      XtUnmanageChild(CycleText);
      XtUnmanageChild(DateCycleGo);
      XtUnmanageChild(DateCycleDismiss);
      XtUnmanageChild(DateCycle);
}
