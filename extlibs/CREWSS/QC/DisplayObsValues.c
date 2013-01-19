#include "gui.h"


void DisplayObsValues (int Obs_To_Display )
/************************************************************************
 *   DisplayObsValues                                                   *
 *                                                                      *
 *  Display the obs and model values on Obs Window.                     *
 *  Display flags set in Flags window.                                  *
 *  Plot obs in ObsPlotWindow.                                          *
 *                                                                      *
 * Modified: Paula Freeman 7/28/04 Only 14 ObsValWidgets were created,  *
 *                                  so only  fill in 0-13               *
 *                                                                      *
 ***********************************************************************/
{
int      i;
float    diff;
char     ModelValStrs[5][15];     /* Model value strings for display        */
char     ObsDiffStrs[5][15];      /* Obs - model strings for display        */
char     dummystr[9];
float    out_lat, out_lon;
int      moved;
char     shipname[25];
/*---------------------------------------------------------------------*/

   /* printf("DisplayObsValues: %d %d %s %s \n", Current_Pri_Obs, Current_View_Obs, callsign, obstime);  */
   for (i=0; i<5; i++) strcpy( ModelValStrs[i], " ");
   for (i=0; i<12; i++) strcpy( ObsValStrs[i], " ");

/*
 * Get the obs values, replaced with the edited values, if edited, and put in curr_ variables.
 */  

   Edited_Obs( &curr_lat, &curr_lon, &curr_winddir, &curr_windspd, &curr_airtemp, &curr_sst, &curr_slp);
      
/*
 * Plot the obs in the observation window.        
 */

    PlotObsWindowCb(NULL, NULL, NULL);

/*
 *  Clear display strings for obs val, model val, and difference values.
 */

   for (i=0; i<9; i++) 
      ObsValStrs[0][i]=callsign[i];
   for (i=0; i<5; i++)
      strcpy(ObsDiffStrs[i], "");
   for (i=3; i<14; i++) 
      strcpy(ObsValStrs[i], "");
   for (i=0; i<5; i++)
      strcpy(ModelValStrs[i], "");


/* Fill in the obs date and time */

   XtVaSetValues(DateWidget, XmNlabelString, XmStringCreateLocalized(obsdate), NULL);
   XtVaSetValues(TimeWidget, XmNlabelString, XmStringCreateLocalized(obstime), NULL);

/* Fill in obs value fields (using original, not edited values ). */
/* Leave blank if data is missing.                                */

   if (lat > 0) {
      sprintf( ObsValStrs[1], "%6.2f N", lat);        
   } else {
      sprintf( ObsValStrs[1], "%6.2f S", -lat);
   }

   if (lon > 180) {
      sprintf( ObsValStrs[2], "%6.2f W", 360-lon);
   } else {
      sprintf( ObsValStrs[2], "%6.2f E", lon);
   }
   if (!null_data(slp))        sprintf( ObsValStrs[3], "%5.1f",  slp); 
   if (!null_data(airtemp))    sprintf( ObsValStrs[4], "%5.1f",  airtemp);
   if (!null_data(windspd))    sprintf( ObsValStrs[5], "%5.1f",  windspd);
   if (!null_data(winddir))    sprintf( ObsValStrs[6], "%5.1f",  winddir);
   if (!null_data(sst))        sprintf( ObsValStrs[7], "%5.1f",  sst);
   if (!null_data(wx))         sprintf( ObsValStrs[8], "%6d",    wx);
   if (!null_data(dewp))       sprintf( ObsValStrs[9], "%5.1f",  dewp);
   if (!null_data(vis))        sprintf( ObsValStrs[10],"%6d",   vis); 
   if (!null_data(presschg3hr))sprintf( ObsValStrs[11],"%5.1f", presschg3hr);
   if (!null_data(ice))        sprintf( ObsValStrs[12], "%6d",   ice);
   if (!null_data(waveper))    sprintf( ObsValStrs[13], "%6d",   waveper);

   /* for (i=0; i<15; i++) { Only 14 ObsValWidgets were created, so only
                             fill in 0-13   PJF 7/28/04  */
   for (i=0; i<14; i++) {
      XtVaSetValues(ObsValWidgets[i], XmNlabelString,
                    XmStringCreateLocalized( ObsValStrs[i] ), NULL);
   };


/* Fill in model values.  Don't display if the location has been changed (moved) */

   moved = Edited_Loc( &out_lat, &out_lon, lat, lon, Edited_Value[1], Edited_Value[2]);

   if (!moved) {
      sprintf( ModelValStrs[0], "%5.1f",  model_slp);
      sprintf( ModelValStrs[1], "%5.1f",  model_airtemp);
      sprintf( ModelValStrs[2], "%5.1f",  model_windspd);
      sprintf( ModelValStrs[3], "%5.1f",  model_winddir);
      sprintf( ModelValStrs[4], "%5.1f",  model_sst);

   for (i=0; i<5; i++){
      XtVaSetValues(ModelValWidgets[i], XmNlabelString,
                    XmStringCreateLocalized( ModelValStrs[i] ), NULL);
   };

/* Display the difference b/t obs and model values. */

      if (!null_data(slp)) sprintf( ObsDiffStrs[0], "%5.1f",  slp - model_slp);
      if (!null_data(airtemp)) sprintf( ObsDiffStrs[1], "%5.1f",  airtemp - model_airtemp);
      if (!null_data(windspd)) sprintf( ObsDiffStrs[2], "%5.1f",  windspd - model_windspd);
      if (!null_data(winddir)) {
         diff = abs(winddir - model_winddir);
         if (diff > 180) diff = 360 - diff;
         sprintf( ObsDiffStrs[3], "%5.1f",  diff);
      }
      if (!null_data(sst)) sprintf( ObsDiffStrs[4], "%5.1f",  sst - model_sst);
   }

   for (i=0; i<5; i++){
      XtVaSetValues(ObsDiffWidgets[i], XmNlabelString,
                    XmStringCreateLocalized( ObsDiffStrs[i] ), NULL);
   };

/* Fill in the obs edit windows.  If the obs has been edited, display  */
/* the edited value so use the curr_ values, except for callsign       */

   /* printf("display flags and edits for %s %s \n", callsign, obstime);  */

   if (QC_Flags[0]==3)                                           /* handle callsign separately. */
      XmTextFieldSetString(ObsEditValWidgets[0], edit_callsign); /* because it's a string value */
   else                                                          /* so not in float Edited_Value */
      XmTextFieldSetString(ObsEditValWidgets[0], ObsValStrs[0]);
   
   if (curr_lat > 0.) {                     /* lat was edited, display hem format */
      sprintf( dummystr, "%6.2f N", curr_lat);
   } else {
      sprintf( dummystr, "%6.2f S", -curr_lat);
   }
   XmTextFieldSetString(ObsEditValWidgets[1], dummystr);

   if (curr_lon > 180.) {                     /* lon was edited, display hem format */
      sprintf( dummystr, "%6.2f W", 360-curr_lon);
   } else {
      sprintf( dummystr, "%6.2f E", curr_lon);
   }
   XmTextFieldSetString(ObsEditValWidgets[2], dummystr);

   if(!null_data(curr_slp)) sprintf( dummystr, "%5.1f", curr_slp); else strcpy(dummystr,"");
   XmTextFieldSetString(ObsEditValWidgets[3], dummystr);
   if(!null_data(curr_airtemp)) sprintf( dummystr, "%5.1f", curr_airtemp); else strcpy(dummystr,"");
   XmTextFieldSetString(ObsEditValWidgets[4], dummystr);
   if(!null_data(curr_windspd)) sprintf( dummystr, "%5.1f", curr_windspd); else strcpy(dummystr,"");
   XmTextFieldSetString(ObsEditValWidgets[5], dummystr);
   if(!null_data(curr_winddir)) sprintf( dummystr, "%5.1f", curr_winddir); else strcpy(dummystr,"");
   XmTextFieldSetString(ObsEditValWidgets[6], dummystr);
   if(!null_data(curr_sst)) sprintf( dummystr, "%5.1f", curr_sst); else strcpy(dummystr,"");
   XmTextFieldSetString(ObsEditValWidgets[7], dummystr);


   /* clear the edit flags */

   for (i=0; i<5; i++) {
         FlagButtonValues[i][0] = False;                   /* clear accept and reject buttons */
         FlagButtonValues[i][1] = False;
         XmToggleButtonSetState(FlagButtonWidgets[i][0], False, False);
         XmToggleButtonSetState(FlagButtonWidgets[i][1], False, False);
   }


/*
 * If any accept/reject flags already set, update the toggle buttons.
 */

   for (i=0; i<5; i++) {                       /* params 0-2 can be changed, but not accepted or */
                                               /* rejected, so don't check QC_Flags[0-2]         */
      /* printf("Check QC_Flag[%d] which is FlagButtonValue[%d]\n", i+3, i); */
      if (QC_Flags[i+3]==1) {               
         FlagButtonValues[i][1] = True;                        /* set accept */
         FlagButtonValues[i][0] = False;      
         XmToggleButtonSetState(FlagButtonWidgets[i][1], True, False);
      } else if (QC_Flags[i+3]==2) {
         FlagButtonValues[i][0] = True;                         /* set reject */
         FlagButtonValues[i][1] = False;
         XmToggleButtonSetState(FlagButtonWidgets[i][0], True, False);
      }
   }

   /* The Callsign_Index value for each obs points into the Callsign array for the callsign, and the
       ShipNames_Index array for the record in the Ship_Names file for the full
       ship name.  Confused yet?  
    */

   Get_Ship_Name( ShipNames_File, ShipNames_Index[Callsign_Index[Obs_To_Display]], shipname);
   XtVaSetValues(ShipNameWidget, XmNlabelString, XmStringCreateLocalized(shipname), NULL);


/* Highlight the labels for the suspect parameters */

   Highlight_Labels(moved);


}
