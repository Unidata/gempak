#include "gui.h"


Display      *AppDisplay;            /* Display for map window        */
char   helpfile[128];
Widget ForecastMenu;
Widget ModelValMenu;
XEvent Current_Event;
int    MenubarHeight;
int    ObsDisplayHeight;
Widget Map, BB, MW, toplevel;


/*=====================================================================*/

Widget Create_Menubar ( Widget parent )
{
Widget	menubar, ForecastButtons[6], ModelValButtons[6];

int	n, i, xpos;
Arg     args[10];
char    MenuLabels[13][20];
/*static  char    helpfile[128]; */

/*---------------------------------------------------------------------*/

   strcpy( MenuLabels[0], "Exit" );
   strcpy( MenuLabels[1], "Select" );
   strcpy( MenuLabels[2], "Process" );
   strcpy( MenuLabels[3], "UnView" );
   strcpy( MenuLabels[4], "Zoom" );
   strcpy( MenuLabels[5], "World Map" );
   strcpy( MenuLabels[6], "Curr Obs" );
   strcpy( MenuLabels[7], "Fcst Fields" );
   strcpy( MenuLabels[8], "Model Value" );
   strcpy( MenuLabels[9], "History/Cruise Plot" );
   strcpy( MenuLabels[10], "Check Callsign" );
   strcpy( MenuLabels[11], "Date/Cycle" );
   strcpy( MenuLabels[12], "Submit" );

/*
 * Create the menu bar
 */

   n=0;
   XtSetArg(args[n], XmNheight, MenubarHeight); n++;
   XtSetArg(args[n], XmNwidth, 1200); n++;
   XtSetArg(args[n], XmNx, 0); n++;
   XtSetArg(args[n], XmNy, 0); n++;
   XtSetArg(args[n], XmNmarginHeight, 0); n++;
   XtSetArg(args[n], XmNmarginWidth, 0); n++;   
   XtSetArg(args[n], XmNbackground, White.pixel); n++;
   menubar = XmCreateBulletinBoard( parent, "Menubar", args, n);
   XtManageChild(menubar);


/*
 * Create pushbuttons  -- spacing notes: 6 for each char, 35 b/t labels. 
 */

   n=0;
   xpos = 10;
   XtSetArg(args[n], XmNy, 5); n++;
   XtSetArg(args[n], XmNbackground, White.pixel); n++;
   XtSetArg(args[n], XmNforeground, Black.pixel); n++;

   for (i=0; i<13; i++) {
      XtSetArg(args[3], XmNx, xpos);
      MenuButtons[i] = XmCreatePushButton(menubar, MenuLabels[i], args, 4);
      xpos = xpos + strlen(MenuLabels[i])*6 + 35;
   }

/* Create popup menu for Contour button */

   ForecastMenu = XmCreatePopupMenu(menubar, "Forecast Plots", NULL, 0);
   XtSetArg(args[0], XmNlabelString, XmStringCreateLocalized("Forecast Plots Menu"));

   ForecastButtons[0] = XmCreateLabel(ForecastMenu, "title", args, 1);
   ForecastButtons[1] = XmCreateSeparator(ForecastMenu, "sep", NULL, 0);
   ForecastButtons[2] = XmCreatePushButton(ForecastMenu, "Contour SLP", NULL, 0);
   XtAddCallback( ForecastButtons[2], XmNactivateCallback, (XtCallbackProc) ContourSLPCb, NULL); 
   ForecastButtons[3] = XmCreatePushButton(ForecastMenu, "Contour SST", NULL, 0);
   XtAddCallback( ForecastButtons[3], XmNactivateCallback, (XtCallbackProc) ContourSSTCb, NULL); 
   ForecastButtons[4] = XmCreatePushButton(ForecastMenu, "Contour Air Temp", NULL, 0);
   XtAddCallback( ForecastButtons[4], XmNactivateCallback, (XtCallbackProc) ContourTempCb, NULL); 
   ForecastButtons[5] = XmCreatePushButton(ForecastMenu, "Wind Vectors", NULL, 0);
   XtAddCallback( ForecastButtons[5], XmNactivateCallback, (XtCallbackProc) WindVectorsCb, NULL); 

   XtManageChildren(ForecastButtons, 6);

/* Create a Scrolled Window for the Ship History */

   n=0;
   XtSetArg(args[n], XmNheight, HistoryHeight);n++;
   XtSetArg(args[n], XmNwidth, HistoryWidth);n++;
   XtSetArg(args[n], XmNx, 470);n++;
   XtSetArg(args[n], XmNy, MenubarHeight);n++;

   History = XmCreateBulletinBoard( BB, "History", args, n);
   n=0;
   XtSetArg(args[n], XmNheight, 30);n++;
   XtSetArg(args[n], XmNwidth, 80);n++;
   XtSetArg(args[n], XmNx, 0);n++;
   XtSetArg(args[n], XmNy, 0);n++;
   HistoryExit = XmCreatePushButton( History, "Dismiss", args, n);
   XtAddCallback( HistoryExit, XmNactivateCallback, (XtCallbackProc) CloseHistoryCb, NULL); 

   n=0;
   XtSetArg(args[n], XmNheight, 30);n++;
   XtSetArg(args[n], XmNwidth, 150);n++;
   XtSetArg(args[n], XmNx, (HistoryWidth-150)/2);n++;
   XtSetArg(args[n], XmNy, 0);n++;
   XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized("callsign"));n++;
   XtSetArg(args[n], XmNforeground, White.pixel);n++;
   HistoryLabel =XmCreateLabel(History, "History Label", args, n);

   XtManageChild(HistoryExit);
   XtManageChild(HistoryLabel);

/* Create a popup dialog for CheckCallsign */

   Create_Popup_CheckCallsign();

/* Create a popup dialog for GetDateCycle */

   Create_Popup_DateCycle();

/* Create popup menu for Model Value button */

   ModelValMenu = XmCreatePopupMenu(menubar, "ModelVal", NULL, 0);
   XtSetArg(args[0], XmNlabelString, XmStringCreateLocalized("Display Model Values"));

   ModelValButtons[0] = XmCreateLabel(ModelValMenu, "title", args, 1);
   ModelValButtons[1] = XmCreateSeparator(ModelValMenu, "sep", NULL, 0);
   ModelValButtons[2] = XmCreatePushButton(ModelValMenu, "SLP", NULL, 0);
   XtAddCallback( ModelValButtons[2], XmNactivateCallback, (XtCallbackProc) DisplaySLPValCb, NULL);   
   ModelValButtons[3] = XmCreatePushButton(ModelValMenu, "SST", NULL, 0); 
   XtAddCallback( ModelValButtons[3], XmNactivateCallback, (XtCallbackProc) DisplaySSTValCb, NULL);   
   ModelValButtons[4] = XmCreatePushButton(ModelValMenu, "Air Temp", NULL, 0);
   XtAddCallback(ModelValButtons[4], XmNactivateCallback, (XtCallbackProc) DisplayTempValCb, NULL);   
   ModelValButtons[5] = XmCreatePushButton(ModelValMenu, "Wind Speed", NULL, 0);
   XtAddCallback(ModelValButtons[5], XmNactivateCallback, (XtCallbackProc) DisplayWindSpdValCb, NULL);   
   XtManageChildren(ModelValButtons, 6);
/* NxmHelpPopupCreate( parent, "HelpDialog", "HelpText", 20, 75); */
   XtAddCallback( MenuButtons[0],  XmNactivateCallback, (XtCallbackProc) ExitCb,          NULL);
   XtAddCallback( MenuButtons[1],  XmNactivateCallback, (XtCallbackProc) SelectGroupCb,   NULL); 
   XtAddCallback( MenuButtons[2],  XmNactivateCallback, (XtCallbackProc) ProcessGroupCb,  NULL); 
   XtAddCallback( MenuButtons[3],  XmNactivateCallback, (XtCallbackProc) UnViewGroupCb,   NULL); 
   XtAddCallback( MenuButtons[4],  XmNactivateCallback, (XtCallbackProc) ZoomCb,          NULL);
   XtAddCallback( MenuButtons[5],  XmNactivateCallback, (XtCallbackProc) WorldMapCb,      NULL);
   XtAddCallback( MenuButtons[6],  XmNactivateCallback, (XtCallbackProc) ReFocusCb,       NULL); 
   XtAddCallback( MenuButtons[7],  XmNactivateCallback, (XtCallbackProc) PopupForecastCb, NULL);
   XtAddCallback( MenuButtons[8],  XmNactivateCallback, (XtCallbackProc) PopupModelValCb, NULL); 
   XtAddCallback( MenuButtons[9],  XmNactivateCallback, (XtCallbackProc) PopupHistoryCb,  NULL); 
   XtAddCallback( MenuButtons[10], XmNactivateCallback, (XtCallbackProc) PopupCheckCallsignCb, NULL); 
   XtAddCallback( MenuButtons[11], XmNactivateCallback, (XtCallbackProc) PopupGetDateCycleCb, NULL); 
   XtAddCallback( MenuButtons[12], XmNactivateCallback, (XtCallbackProc) SubmitQCFlagsCb, NULL); 

   XtManageChildren(MenuButtons, 13);

   return (menubar);
}

/*=====================================================================*/
/*ARGSUSED*/
void PopupForecastCb ( Widget widget, int tag,
				XmPushButtonCallbackStruct *cb_data)
/************************************************************************
 *                                                                      *
 * PopupForecastCb                                                      *
 *   This callback routine pops up the ForecastMenu                     *
 *                                                                      *
 ***********************************************************************/
{
   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */
   XmMenuPosition(ForecastMenu,  (XButtonPressedEvent *) &Current_Event);
   XtManageChild(ForecastMenu);

}

/*=====================================================================*/
/*ARGSUSED*/
void PopupModelValCb ( Widget widget, int tag,
				XmPushButtonCallbackStruct *cb_data)
/************************************************************************
 *                                                                      *
 * PopupModelValCb                                                      *
 *   This callback routine pops up the ModelValMenu                     *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 ***********************************************************************/
{
   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */
   XmMenuPosition(ModelValMenu,  (XButtonPressedEvent *) &Current_Event);
   XtManageChild(ModelValMenu);

}
