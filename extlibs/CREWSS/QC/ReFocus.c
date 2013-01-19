#include "gui.h"

/* ARGSUSED */
void ReFocusCb(Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 * ReFocusCb                                                              *
 *                                                                      *
 * This function rezooms in on the current observation and redisplays   *
 * it in the Obs windows.                                               *
 *                                                                      *
 ************************************************************************/

{
   float out_lat, out_lon;
   int CurrObs;

   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */
   if (SaveObsChanges()) {

      if (Current_View_Obs >= 0) {
         CurrObs = Current_View_Obs;
      } else if ((Current_Pri_Obs >= 0) && (Current_Pri_Obs < (int)NumObs)) {
         CurrObs = Current_Pri_Obs; 
      } else
         CurrObs = -1;


/*
    * Remove the obs from the list it was on for the current map.
    * We'll add it back to a list when we see if it's been qc'ed.
    */

     /* printf("In Refocus: CurrObs=%d callsign:%s\n", CurrObs, callsign); */

     if (CurrObs >= 0) {
        DisplayObsValues(CurrObs);    
        Edited_Loc( &out_lat, &out_lon, lat, lon, Edited_Value[1], Edited_Value[2]);
        AutoZoom( out_lat, out_lon );  
     }
   }

}
