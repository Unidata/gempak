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
int             i, nargs;
Arg             args[12];
char            HeadingStrs[7][10];
char            LabelStrs[16][20];

/*---------------------------------------------------------------------*/

/*
 * Create the Observations Display window and the Flag Window
 */

   nargs = 0;
   XtSetArg(args[nargs], XmNbackground, White.pixel); nargs++; 
   XtSetArg(args[nargs], XmNshadowThickness, 5); nargs++;
   XtSetArg(args[nargs], XmNshadowType, XmSHADOW_ETCHED_IN);nargs++;
   XtSetArg(args[nargs], XmNy, MenubarHeight); nargs++;

   XtSetArg(args[nargs], XmNheight, ObsWindowHeight); nargs++;
   XtSetArg(args[nargs], XmNwidth, WindowWidth); nargs++;
   XtSetArg(args[nargs], XmNx, 0); nargs++;
   ObsFrame = XmCreateFrame( parent, "obsframe", args, nargs);

   nargs = 0;
   XtSetArg(args[nargs], XmNbackground, White.pixel); nargs++; 
    XtSetArg(args[nargs], XmNresizePolicy, XmRESIZE_NONE); nargs++;
   ObsWindow = XmCreateDrawingArea( ObsFrame, "ObsWindow", args, nargs);

   XtManageChild( ObsFrame );
   XtManageChild( ObsWindow );

   XtSetArg(args[nargs], XmNbackground, Black.pixel); nargs++;           /* line b/t obs display and flags */
   XtSetArg(args[nargs], XmNheight, ObsWindowHeight-30); nargs++;        /* I couldn't get XmSeparator to work */
   XtSetArg(args[nargs], XmNwidth, 2); nargs++;
   XtSetArg(args[nargs], XmNx, 700); nargs++;
   XtSetArg(args[nargs], XmNy, 0); nargs++;
   XtSetArg(args[nargs], XmNforeground, Red.pixel);    nargs++;
   Line = XmCreateLabel(ObsWindow, "", args, nargs);
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

   XtSetArg(args[0], XmNbackground, White.pixel);      
   XtSetArg(args[1], XmNheight, 20); 
   XtSetArg(args[2], XmNx, 0);
   XtSetArg(args[3], XmNy, 0);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[0]));
   XtSetArg(args[5], XmNforeground, Brown.pixel);   

   nargs = 6;
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 135);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[1]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 225);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[2]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 290);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[3]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 360);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[4]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 460);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[0]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 590);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[1]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 0);
   XtSetArg(args[5], XmNforeground, Black.pixel);
   for (i=0; i<10; i++) {
      XtSetArg(args[3], XmNy, (i*29)+35);          /* was 27 45 */
      XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(LabelStrs[i]));
      ParamLabelWidgets[i] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(ParamLabelWidgets[i]);
   };

   XtSetArg(args[2], XmNx, 460);
   for (i=0; i<6; i++) {
      XtSetArg(args[3], XmNy, (i*27)+45);
      XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(LabelStrs[i+10]));
      ParamLabelWidgets[i+10] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(ParamLabelWidgets[i+10]);
   };

/*
 * Write the heading and flag labels for the flags 
 */

   XtSetArg(args[0], XmNbackground, White.pixel);      
   XtSetArg(args[1], XmNheight, 20);
   XtSetArg(args[2], XmNx, 750);
   XtSetArg(args[3], XmNy, 0);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[0]));
   XtSetArg(args[5], XmNforeground, Brown.pixel);
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 890);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[5]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);
   XtSetArg(args[2], XmNx, 975);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(HeadingStrs[6]));
   w = XmCreateLabel(ObsWindow, "", args, nargs);
   XtManageChild(w);

   XtSetArg(args[2], XmNx, 750);
   XtSetArg(args[5], XmNforeground, Black.pixel);
   for (i=0; i<5; i++) {
      XtSetArg(args[3], XmNy, ((i)*20)+53); 
      XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(LabelStrs[i+5]));
      FlagLabelWidgets[i] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(FlagLabelWidgets[i]);
   };



/*
 * Create the label widgets for obs values, and editable ones for qc-able values.
 * (for model & noneditable obs values) on the observation window
 */

   nargs = 7;
   XtSetArg(args[1], XmNheight, 30);
   XtSetArg(args[2], XmNwidth, 45);
   XtSetArg(args[3], XmNforeground, Black.pixel);
   XtSetArg(args[4], XmNlabelString,XmStringCreateLocalized(" "));
   XtSetArg(args[5], XmNx, 135);          /* was 125 */

/* date/time */

   XtSetArg(args[6], XmNy, 35);
   DateWidget = XmCreateLabel(ObsWindow, "", args, nargs);
   XtSetArg(args[6], XmNy, 64);
   TimeWidget = XmCreateLabel(ObsWindow, "", args, nargs);

   XtManageChild(DateWidget);
   XtManageChild(TimeWidget);

/* observation value label widgets.  Display in two columns. */ 

   
   for (i=0; i<8; i++) {
      XtSetArg(args[6], XmNy, (i*29)+91);          /* was 27 98 */
      ObsValWidgets[i] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(ObsValWidgets[i]);
   };

   XtSetArg(args[5], XmNx, 610);        /* was 600 */
   for (i=0; i<6; i++) {
      XtSetArg(args[6], XmNy, (i*27)+45);   
      ObsValWidgets[i+8] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(ObsValWidgets[i+8]);
   };

/* model values, to be displayed for the qc-able obs data values in the left column:
   SLP, SST, air temp, wind speed, and wind direction */

   nargs = 8;
   XtSetArg(args[2], XmNwidth, 55);
   XtSetArg(args[5], XmNx, 210);   /* was 190 */
   for (i=0; i<5; i++) {
      XtSetArg(args[7], XmNy, (i*29)+177);
      ModelValWidgets[i] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(ModelValWidgets[i]);
   };


/* Difference values for obs - model */

   XtSetArg(args[2], XmNwidth, 55 );
   XtSetArg(args[5], XmNx, 280);           /* was 260 */
   for (i=0; i<5; i++) {
      XtSetArg(args[7], XmNy, (i*29)+177);
      ObsDiffWidgets[i] = XmCreateLabel(ObsWindow, "", args, nargs);
      XtManageChild(ObsDiffWidgets[i]);
   };



/* editable widgets for qc-able obs values, in left column. */

   XtSetArg(args[1], XmNheight, 34); /* was 29 */
   XtSetArg(args[2], XmNwidth, 90 ); /* was 80 */
   XtSetArg(args[7], XmNeditable, True);
   XtSetArg(args[5], XmNx, 355);           
   for (i=0; i<8; i++) {
      XtSetArg(args[7], XmNy, (i*30)+85);
      ObsEditValWidgets[i] = XmCreateTextField(ObsWindow, "              ", args, nargs);
      XtManageChild(ObsEditValWidgets[i]);
   };

/* Create the model value display area for the ModelValue button. */
/* Don't manage it, it appears only when active.                  */

   nargs = 0;
   XtSetArg(args[nargs], XmNx, 450);nargs++;
   XtSetArg(args[nargs], XmNy, ObsWindowHeight-45);nargs++; /* was 50 */
   XtSetArg(args[nargs], XmNlabelString,XmStringCreateLocalized(" "));nargs++;
   XtSetArg(args[nargs], XmNheight, 30); nargs++;
   XtSetArg(args[nargs], XmNwidth, 150); nargs++;
   XtSetArg(args[nargs], XmNborderWidth, 2); nargs++;
   XtSetArg(args[nargs], XmNborderColor, Red.pixel); nargs++;

   ModelValueLabel = XmCreateLabel(ObsWindow, "Model Value", args, nargs);



/* Create the ShipName label to be under the ObsPlotWindow, above the ModelValueLabel */

   nargs = 0;
   XtSetArg(args[nargs], XmNbackground, White.pixel); nargs++;
   XtSetArg(args[nargs], XmNforeground, Black.pixel); nargs++;
   XtSetArg(args[nargs], XmNx, 470);nargs++; 
/* XtSetArg(args[nargs], XmNx, 450);nargs++;  */
   XtSetArg(args[nargs], XmNy, ObsWindowHeight-40);nargs++;  /* was -70 */
   XtSetArg(args[nargs], XmNlabelString,XmStringCreateLocalized(""));nargs++;
   XtSetArg(args[nargs], XmNheight, 35); nargs++;
   XtSetArg(args[nargs], XmNwidth, 200); nargs++;
/* XtSetArg(args[nargs], XmNalignment, "XmALIGNMENT_BEGINNING"); nargs++; */
/* XtSetArg(args[nargs], XmNwidth, 175); nargs++; */
   ShipNameWidget = XmCreateLabel(ObsWindow, "Ship Name", args, nargs);
   XtManageChild(ShipNameWidget);



/*
 * Create the buttons for the flags window
 */

   XtSetArg(args[2], XmNforeground, Black.pixel);
   XtSetArg(args[3], XmNbackground, White.pixel);
   XtSetArg(args[5], XmNindicatorSize, 16);
   XtSetArg(args[7], XmNspacing, 0);
   XtSetArg(args[8], XmNheight, 20);
   XtSetArg(args[9], XmNwidth,  19);
   XtSetArg(args[10], XmNmarginHeight, 0);
   XtSetArg(args[11], XmNmarginWidth, 0);
   for (i=0; i<5; i++) {                   
      XtSetArg(args[0], XmNy, (i*20)+50); /* was (i*30)+55); */
      XtSetArg(args[1], XmNx, 900);
      XtSetArg(args[4], XmNselectColor, Red.pixel);
      XtSetArg(args[6], XmNhighlightColor, Red.pixel);
      FlagButtonWidgets[i][0] = XmCreateToggleButton(ObsWindow, "", args, 12);
      XtAddCallback(FlagButtonWidgets[i][0] , XmNvalueChangedCallback, 
                    (XtCallbackProc)ToggleFlagCb, (XtPointer) (i*2));
      XtSetArg(args[1], XmNx, 980);
      XtSetArg(args[4], XmNselectColor, Green.pixel);
      XtSetArg(args[6], XmNhighlightColor, Green.pixel);
      FlagButtonWidgets[i][1] = XmCreateToggleButton(ObsWindow, "", args, 12);
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
   XtAddCallback(AcceptAllWidget , XmNactivateCallback, (XtCallbackProc)SetAllFlagsCb, (XtPointer) 1); 

   XtSetArg(args[1], XmNy, 70);
   RejectAllWidget = XmCreatePushButton(ObsWindow, "Reject All", args, 4);
   XtAddCallback(RejectAllWidget , XmNactivateCallback, (XtCallbackProc)SetAllFlagsCb, (XtPointer) 0);

   XtSetArg(args[1], XmNy, 105);
   ClearAllWidget = XmCreatePushButton(ObsWindow, "Clear All", args, 4);
   XtAddCallback(ClearAllWidget , XmNactivateCallback, (XtCallbackProc)SetAllFlagsCb, (XtPointer) 2);


   XtSetArg(args[1], XmNy, 140);
   DupWidget = XmCreatePushButton(ObsWindow, "Duplicate", args, 4);
   XtAddCallback(DupWidget , XmNactivateCallback,  (XtCallbackProc)SetDupFlagCb,  (XtPointer) 1); 


   XtSetArg(args[1], XmNy, 175);
   NextObsWidget = XmCreatePushButton(ObsWindow, "Next Obs", args, 4);
   XtAddCallback( NextObsWidget, XmNactivateCallback, (XtCallbackProc)ProcessGroupCb,  NULL); 

/* add the bookmark and return to bookmark buttons */

   XtSetArg(args[1], XmNy, 210);
   BookmarkWidget = XmCreatePushButton(ObsWindow, "No Bookmark Set", args, 4);
   XtAddCallback( BookmarkWidget, XmNactivateCallback, (XtCallbackProc)BookmarkCb,  NULL);


   XtSetArg(args[1], XmNy, 245);
   ReturnWidget = XmCreatePushButton(ObsWindow, "Return to Mark", args, 4);
   XtAddCallback( ReturnWidget, XmNactivateCallback, (XtCallbackProc)ReturnToBookmarkCb,  NULL);


   XtManageChild(AcceptAllWidget);
   XtManageChild(RejectAllWidget);
   XtManageChild(ClearAllWidget);
   XtManageChild(DupWidget);
   XtManageChild(NextObsWidget);
   XtManageChild(BookmarkWidget);
   XtManageChild(ReturnWidget);

}
