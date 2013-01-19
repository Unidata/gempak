#include "gui.h"


/*ARGSUSED*/
void PlotObsWindowCb(Widget wid, XtPointer client, XtPointer cbs)
/************************************************************************
 * PlotObsWindowCb                                                      *
 *                                                                      *
 * Redraw the station plot in the obs window.                           *
 *                                                                      *
 ***********************************************************************/
{

   float Barb_Size, Char_Distance;
   int   Color;

/*---------------------------------------------------------------------*/

/*
 * Plot the current obs in the observation window.
 */

   if ( (Current_Pri_Obs >= 0) || (Current_View_Obs >= 0) ) {
       setup_station_plot(&GKSObsPlotWkid, &ZoomIn, &curr_lat, &curr_lon);
/*     Barb_Size =  .4;  Char_Distance = 4.; */
       Barb_Size =  .32;  Char_Distance = 3.5; 
       Color=1;
       plot_one_station( &GKSObsPlotWkid, &Barb_Size, &Char_Distance, 
			 &Color, &curr_lat, &curr_lon, &cloud, &vis, 
			 &curr_winddir, &curr_windspd, &curr_airtemp,
			 &curr_sst, &curr_slp );
       deactivate_low_level( &GKSObsPlotWkid);
   }
}
