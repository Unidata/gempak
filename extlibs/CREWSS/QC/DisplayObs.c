#include "gui.h"


void DisplayObs( int Obs_To_Display  )
/************************************************************************
 *   DisplayObs                                                         *
 *                                                                      *
 *   Display the current observation values and model values, zoom      *
 *   in on the chosen obs and plot the station plot.                    *
 *                                                                      *
 ***********************************************************************/
{
int      ret;
/*---------------------------------------------------------------------*/

   
   if( (Previous_View_Obs >= 0) || (Previous_Pri_Obs >= 0)) Replot_Curr_Wx_Obs( GKSWkid, QCed_Color);
      
/*
 * Plot the obs in the observation window.        
 */

    ReadObs(Obs_To_Display);

/*
 *  Display the obs and model values on Obs Window.
 *  Display flags set in Flags window.
 *  Plot obs in ObsPlotWindow.
 */

    DisplayObsValues(Obs_To_Display);


/*
 *  Zoom to a box centered on the obs
 *
 * If already zoomed, and the obs is in the box, then don't rezoom.
 * Check for previous view and priority obs, if on the screen, redraw
 * in appropriate color.  Leave any windbarbs and/or contours up.
 * Plot the current obs as highlighted.
 */

    /* printf("DisplayObs: Previous_View_Obs=%d and Previous_Pri_Obs=%d\n", Previous_View_Obs, Previous_Pri_Obs);  */

   NhlDataToNDC( Mapid, Lon, Lat, NumObs, X, Y, NULL, NULL, &ret, &missing);
   if ( AutoZoomed && !NewMap && 
	(X[Obs_To_Display] < 1.) && (X[Obs_To_Display] > 0.) && 
	(Y[Obs_To_Display] < 1.) && (Y[Obs_To_Display] > 0.)) {


     Plot_One_Wx_Obs( GKSWkid, Obs_To_Display, NoZoom, Current_Obs_Color);      /* reads obs from database & plots */

   } else {

/* redraw the screen.  Remove and destroy any contours and vectors */

     if (Contour_SLP_On) {
        Contour_SLP_On = False;
        NhlRemoveOverlay(Mapid,Contour_SLP_id,-1);
        NhlDestroy(Contour_SLP_id);
     }
     if (Contour_SST_On) {
        Contour_SST_On = False;
        NhlRemoveOverlay(Mapid,Contour_SST_id,-1);
        NhlDestroy(Contour_SST_id);
     }
     if (Contour_Temp_On) {
        Contour_Temp_On = False;
        NhlRemoveOverlay(Mapid,Contour_Temp_id,-1);
        NhlDestroy(Contour_Temp_id);
     }
     if (Wind_Vectors_On) {
        Wind_Vectors_On = False;
        NhlRemoveOverlay(Mapid,Wind_id,-1);
        NhlDestroy(Wind_id);
     }

      AutoZoom( Lat[Obs_To_Display], Lon[Obs_To_Display] ); 
      NhlUpdateWorkstation(Wkid);
   }


/* if a parameter is suspect, overlay a contour or wind vectors, if not already up */

   if (suspect_slp && Contour_SLP_On != True)
      ContourSLPCb(NULL, NULL, NULL);
   else if (suspect_airtemp && Contour_Temp_On != True)
      ContourTempCb(NULL, NULL, NULL);
   else if ( (suspect_windspd || suspect_winddir) && Wind_Vectors_On != True)
      WindVectorsCb(NULL, NULL, NULL);
   else if (suspect_sst && Contour_SST_On != True)
      ContourSSTCb(NULL, NULL, NULL);

/*
 * Update the Legend with current obs counts.
 */

   Update_Legend();

}
