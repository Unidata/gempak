#include "gui.h"


void Create_Obs_Windows( Widget parent )
/************************************************************************
 * Create_Obs_Windows							*
 *									*
 * This function creates the Observation Window.	         	*
 *									*
 *   - label widgets for heading and parameter labels,                  *
 *   - editable text widgets for display and editing of some            *
 *     observation parameters,                                          * 
 *   - label widgets for display of noneditable observation parameters, *
 *   - label widgets for display of model values.                       *
 *   - label widgets for heading and flag labels,                       *
 *   - radio buttons for flags.                                         * 
 *									*
 * Create_Obs_Windows ( parent )			        	*
 *									*
 ************************************************************************/
{
Widget          ObsFrame, w, Line;
int             i, n;
Arg             args[10];
char            HeadingStrs[7][10];
char            LabelStrs[16][20];


/*---------------------------------------------------------------------*/

/*
 * Create the Observations Display window and the Flag Window
 */

   n=0;
   XtSetArg(args[n], XmNbackground, White.pixel); n++; 
   XtSetArg(args[n], XmNshadowThickness, 5); n++;
   XtSetArg(args[n], XmNshadowType, XmSHADOW_ETCHED_IN);n++;
   XtSetArg(args[n], XmNy, MenubarHeight); n++;

   XtSetArg(args[n], XmNheight, ObsWindowHeight); n++;
   XtSetArg(args[n], XmNwidth, WindowWidth); n++;
   XtSetArg(args[n], XmNx, 0); n++;
   ObsFrame = XmCreateFrame( parent, "obsframe", args, n);

   n = 0;
   XtSetArg(args[n], XmNbackground, White.pixel); n++; 
    XtSetArg(args[n], XmNresizePolicy, XmRESIZE_NONE); n++;
   ObsWindow = XmCreateDrawingArea( ObsFrame, "ObsWindow", args, n);

   XtManageChild( ObsFrame );
   XtManageChild( ObsWindow );

   XtSetArg(args[n], XmNbackground, Black.pixel); n++;           /* line b/t obs display and flags */
   XtSetArg(args[n], XmNheight, ObsWindowHeight-30); n++;        /* I couldn't get XmSeparator to work */
   XtSetArg(args[n], XmNwidth, 2); n++;
   XtSetArg(args[n], XmNx, 700); n++;
   XtSetArg(args[n], XmNy, 0); n++;
   XtSetArg(args[n], XmNforeground, Red.pixel);    n++;
   Line = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild( Line );

/*
 * Write the heading and labels for the observation window
 */

   strcpy ( LabelStrs[0], "Date               ");
   strcpy ( LabelStrs[1], "Time (Z)           ");
   strcpy ( LabelStrs[2], "Callsign           ");
   strcpy ( LabelStrs[3], "Lat                ");
   strcpy ( LabelStrs[4], "Lon                ");
   strcpy ( LabelStrs[5], "SLP (mb)           ");
   strcpy ( LabelStrs[6], "Air Temp (C)       ");
   strcpy ( LabelStrs[7], "Wind Spd (kts)     ");
   strcpy ( LabelStrs[8], "Wind Dir (deg)     ");
   strcpy ( LabelStrs[9], "SST (C)            ");
   strcpy ( LabelStrs[10], "WMO Wx code       ");
   strcpy ( LabelStrs[11],"Dew point (C)      ");
   strcpy ( LabelStrs[12], "Visibility        ");
   strcpy ( LabelStrs[13], "3-hr Press Chg    ");
   strcpy ( LabelStrs[14], "Ice Accretion (cm)");
   strcpy ( LabelStrs[15], "Wave Period (sec) ");
   strcpy ( HeadingStrs[0],"Parameter");
   strcpy ( HeadingStrs[1],"Observed");
   strcpy ( HeadingStrs[2],"Model");
   strcpy ( HeadingStrs[3],"Diff");
   strcpy ( HeadingStrs[4],"Edited");
   strcpy ( HeadingStrs[5],"Reject");
   strcpy ( HeadingStrs[6],"Accept");

   n=0;
   XtSetArg(args[0], XmNbackground, White.pixel);      
   XtSetArg(args[1], XmNheight, 20); 
   XtSetArg(args[2], XmNx, 0);
   XtSetArg(args[3], XmNy, 0);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[0]));
   XtSetArg(args[5], XmNforeground, Brown.pixel);   
   n = 6;
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 135);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[1]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 225);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[2]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 290);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[3]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 360);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[4]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 460);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[0]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 590);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[1]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 0);
   XtSetArg(args[5], XmNforeground, Black.pixel);
   for (i=0; i<10; i++) {
      XtSetArg(args[3], XmNy, (i*29)+35);          /* was 27 45 */
      XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(LabelStrs[i]));
      ParamLabelWidgets[i] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(ParamLabelWidgets[i]);
   };

   XtSetArg(args[2], XmNx, 460);
   for (i=0; i<6; i++) {
      XtSetArg(args[3], XmNy, (i*27)+45);
      XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(LabelStrs[i+10]));
      ParamLabelWidgets[i+10] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(ParamLabelWidgets[i+10]);
   };

/*
 * Write the heading and flag labels for the flags 
 */

   n = 6;
   XtSetArg(args[0], XmNbackground, White.pixel);      
   XtSetArg(args[1], XmNheight, 20);
   XtSetArg(args[2], XmNx, 750);
   XtSetArg(args[3], XmNy, 0);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[0]));
   XtSetArg(args[5], XmNforeground, Brown.pixel);
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 890);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[5]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 975);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[6]));
   w = XmCreateLabel(ObsWindow, "", args, n);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 750);
   XtSetArg(args[5], XmNforeground, Black.pixel);
   for (i=0; i<5; i++) {
      XtSetArg(args[3], XmNy, ((i)*20)+53); 
      XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(LabelStrs[i+5]));
      FlagLabelWidgets[i] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(FlagLabelWidgets[i]);
   };



/*
 * Create the label widgets for obs values, and editable ones for qc-able values.
 * (for model & noneditable obs values) on the observation window
 */

   n=7;
   XtSetArg(args[1], XmNheight, 30);
   XtSetArg(args[2], XmNwidth, 45);
   XtSetArg(args[3], XmNforeground, Black.pixel);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(" "));
   XtSetArg(args[5], XmNx, 135);          /* was 125 */

/* date/time */

   XtSetArg(args[6], XmNy, 35);
   DateWidget = XmCreateLabel(ObsWindow, "", args, n);
   XtSetArg(args[6], XmNy, 64);
   TimeWidget = XmCreateLabel(ObsWindow, "", args, n);

   XtManageChild(DateWidget);
   XtManageChild(TimeWidget);

/* observation value label widgets.  Display in two columns. */ 

   
   for (i=0; i<8; i++) {
      XtSetArg(args[6], XmNy, (i*29)+91);          /* was 27 98 */
      ObsValWidgets[i] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(ObsValWidgets[i]);
   };

   n=7;
   XtSetArg(args[5], XmNx, 610);        /* was 600 */
   for (i=0; i<6; i++) {
      XtSetArg(args[6], XmNy, (i*27)+45);   
      ObsValWidgets[i+8] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(ObsValWidgets[i+8]);
   };

/* model values, to be displayed for the qc-able obs data values in the left column:
   SLP, SST, air temp, wind speed, and wind direction */

   n=8;
   XtSetArg(args[2], XmNwidth, 55);
   XtSetArg(args[5], XmNx, 210);   /* was 190 */
   for (i=0; i<5; i++) {
      XtSetArg(args[7], XmNy, (i*29)+177);
      ModelValWidgets[i] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(ModelValWidgets[i]);
   };


/* Difference values for obs - model */

   n=8;
   XtSetArg(args[2], XmNwidth, 55 );
   XtSetArg(args[5], XmNx, 280);           /* was 260 */
   for (i=0; i<5; i++) {
      XtSetArg(args[7], XmNy, (i*29)+177);
      ObsDiffWidgets[i] = XmCreateLabel(ObsWindow, "", args, n);
      XtManageChild(ObsDiffWidgets[i]);
   };


/* editable widgets for qc-able obs values, in left column. */

   n=8;
   XtSetArg(args[1], XmNheight, 34); /* was 29 */
   XtSetArg(args[2], XmNwidth, 90 ); /* was 80 */
   XtSetArg(args[7], XmNeditable, True);
   XtSetArg(args[5], XmNx, 355);           
   for (i=0; i<8; i++) {
      XtSetArg(args[7], XmNy, (i*30)+85);
      ObsEditValWidgets[i] = XmCreateTextField(ObsWindow, "              ", args, n);
      XtManageChild(ObsEditValWidgets[i]);
   };

/* Create the model value display area for the ModelValue button. */
/* Don't manage it, it appears only when active.                  */

   n=0;
   XtSetArg(args[n], XmNx, 450);n++;
   XtSetArg(args[n], XmNy, ObsWindowHeight-45);n++; /* was 50 */
   XtSetArg(args[n], XmNlabelString,XmStringCreateLocalized(" "));n++;
   XtSetArg(args[n], XmNheight, 30); n++;
   XtSetArg(args[n], XmNwidth, 150); n++;
   XtSetArg(args[n], XmNborderWidth, 2); n++;
   XtSetArg(args[n], XmNborderColor, Red.pixel); n++;

   ModelValueLabel = XmCreateLabel(ObsWindow, "Model Value", args, n);


/* Create the ShipName label to be under the ObsPlotWindow, above the ModelValueLabel */

   n=0;
   XtSetArg(args[n], XmNbackground, White.pixel); n++;
   XtSetArg(args[n], XmNforeground, Black.pixel); n++;
   XtSetArg(args[n], XmNx, 470);n++; 
   /* XtSetArg(args[n], XmNx, 450);n++;  */
   XtSetArg(args[n], XmNy, ObsWindowHeight-40);n++;  /* was -70 */
   XtSetArg(args[n], XmNlabelString,XmStringCreateLocalized(""));n++;
   XtSetArg(args[n], XmNheight, 35); n++;
   XtSetArg(args[n], XmNwidth, 200); n++;
/* XtSetArg(args[n], XmNalignment, "XmALIGNMENT_BEGINNING"); n++; */
/* XtSetArg(args[n], XmNwidth, 175); n++; */

   ShipNameWidget = XmCreateLabel(ObsWindow, "Ship Name", args, n);
   XtManageChild(ShipNameWidget);


/*
 * Create the buttons for the flags window
 */

   n = 9;
   XtSetArg(args[2], XmNforeground, Black.pixel);
   XtSetArg(args[3], XmNbackground, White.pixel);
  
   for (i=0; i<5; i++) {                   
      XtSetArg(args[1], XmNx, 905);
      XtSetArg(args[0], XmNy, (i*20)+55); /* was (i*30)+55); */
      XtSetArg(args[4], XmNselectColor, Red.pixel);
      FlagButtonWidgets[i][0] = XmCreateToggleButton(ObsWindow, "", args, 5);
      XtAddCallback(FlagButtonWidgets[i][0] , XmNvalueChangedCallback, 
                    (XtCallbackProc)ToggleFlagCb, (XtPointer) (i*2));
      XtSetArg(args[1], XmNx, 980);
      XtSetArg(args[4], XmNselectColor, Green.pixel);
      FlagButtonWidgets[i][1] = XmCreateToggleButton(ObsWindow, "", args, 5);
      XtAddCallback(FlagButtonWidgets[i][1] , XmNvalueChangedCallback, 
                    (XtCallbackProc)ToggleFlagCb, (XtPointer) ((i*2)+1)); 
      XtManageChildren(FlagButtonWidgets[i], 2);
   }

/*
 * Create the accept all, reject all, duplicate, and next obs buttons
 * on the flags window.
*/
   XtSetArg(args[0], XmNx, 1040);
   XtSetArg(args[1], XmNy,  35);
   XtSetArg(args[2], XmNbackground, White.pixel);
   XtSetArg(args[3], XmNforeground, Black.pixel);
   AcceptAllWidget = XmCreatePushButton(ObsWindow, "Accept All", args, 4);
   XtAddCallback(AcceptAllWidget, XmNactivateCallback, (XtCallbackProc)SetAllFlagsCb, (XtPointer)1); 

   XtSetArg(args[1], XmNy, 70);
   RejectAllWidget = XmCreatePushButton(ObsWindow, "Reject All", args, 4);
   XtAddCallback(RejectAllWidget, XmNactivateCallback, (XtCallbackProc)SetAllFlagsCb, (XtPointer)0);

   XtSetArg(args[1], XmNy, 105);
   ClearAllWidget = XmCreatePushButton(ObsWindow, "Clear All", args, 4);
   XtAddCallback(ClearAllWidget, XmNactivateCallback, (XtCallbackProc)SetAllFlagsCb, (XtPointer)2);


   XtSetArg(args[1], XmNy, 140);
   DupWidget = XmCreatePushButton(ObsWindow, "Duplicate", args, 4);
   XtAddCallback(DupWidget, XmNactivateCallback, (XtCallbackProc)SetDupFlagCb, (XtPointer)1); 


   XtSetArg(args[1], XmNy, 175);
   NextObsWidget = XmCreatePushButton(ObsWindow, "Next Obs", args, 4);
   XtAddCallback( NextObsWidget, XmNactivateCallback, (XtCallbackProc)ProcessGroupCb, NULL); 

/* add the bookmark and return to bookmark buttons */

   XtSetArg(args[1], XmNy, 210);
   BookmarkWidget = XmCreatePushButton(ObsWindow, "No Bookmark Set", args, 4);
   XtAddCallback( BookmarkWidget, XmNactivateCallback, (XtCallbackProc)BookmarkCb, NULL);


   XtSetArg(args[1], XmNy, 245);
   ReturnWidget = XmCreatePushButton(ObsWindow, "Return to Mark", args, 4);
   XtAddCallback( ReturnWidget, XmNactivateCallback, (XtCallbackProc)ReturnToBookmarkCb, NULL);


   XtManageChild(AcceptAllWidget);
   XtManageChild(RejectAllWidget);
   XtManageChild(ClearAllWidget);
   XtManageChild(DupWidget);
   XtManageChild(NextObsWidget);
   XtManageChild(BookmarkWidget);
   XtManageChild(ReturnWidget);

}
