#include "gui.h"

/*  ARGSUSED  */
void PopupHistoryCb(Widget wid, XtPointer client, XtPointer call )
/************************************************************************
 *                                                                      *
 * PopupHistory                                                         *
 *   This callback routine manages the History widget                   *
 *                                                                      *
 ***********************************************************************/
{

   HistoryCruise(callsign);

}

/*=====================================================================*/

void HistoryCruise( char *callsign )
/************************************************************************
 *                                                                      *
 * HistoryCruise                                                        *
 *   This routine creates the history text page and cruise plot         *
 *   for input callsign.                                                *
 **									*
 * Log:									*
 * T. Piper/SAIC	 1/06	Replaced XtIsWidget with If != NULL	*
 ***********************************************************************/
{
    int		nn;
    Arg		args[10];
    char	HistoryTitle[25];

/*---------------------------------------------------------------------*/

   if (SaveObsChanges()) {
      XtManageChild(HistoryExit);
  
      if (HistoryText != NULL) XtDestroyWidget(HistoryText);

     /* Create the History text widget (destroy and recreate each time to clear the page)*/

      XtVaSetValues( History, XmNheight, HistoryHeight, NULL);
      XtVaSetValues( History, XmNx, 465, XmNy, MenubarHeight, NULL);

      XmUpdateDisplay( History );
      XtManageChild( History );


      nn=0;
      XtSetArg(args[nn], XmNheight, HistoryHeight-50);nn++;  
      XtSetArg(args[nn], XmNwidth, HistoryWidth-20);nn++;
      XtSetArg(args[nn], XmNx, 0);nn++;
      XtSetArg(args[nn], XmNy, 50);nn++;
      XtSetArg(args[nn], XmNscrollBarPlacement, XmBOTTOM_LEFT);nn++;
      XtSetArg(args[nn], XmNeditMode, XmMULTI_LINE_EDIT);nn++;

      HistoryText = XmCreateScrolledText(History, "HistoryText", args, nn);
      XtManageChild(HistoryText);
    
      sprintf(HistoryTitle, "History for %s", callsign); 
      MakeHistoryFile( MenuButtons[9], callsign );   /* button for history/cruise to highlight. */
      nn = PrintHistory(callsign);       
      if (nn > 0) 
         CruisePlot(); 
      else
         Replot(1);
      unlink( History_Filename );
      XtVaSetValues( HistoryLabel, XmNlabelString, XmStringCreateLocalized(HistoryTitle), NULL);
      XmUpdateDisplay( HistoryLabel );
   }
}

/*=================================================================================*/
/* ARGSUSED */
void CloseHistoryCb(Widget wid, XtPointer client, XtPointer call) 
{
   XtUnmanageChild(History); 
   ReFocusCb(NULL, NULL, NULL); 
}
