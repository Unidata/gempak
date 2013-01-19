#include "gui.h"


Widget Create_Obs_Plot_Window( Widget parent )
/************************************************************************
 * Create_Obs_Plot_Window                                               *
 *                                                                      *
 * This function creates the observation plot window                    *
 *                                                                      *
 * Create_Obs_Plot_Window (parent)                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *   parent            Widget                                           *
 *                                                                      *
 * Output parameters:                                                   *
 *   Obs_Plot_Window   Widget       window where obs will be plotted    *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   P. Freeman  NCEP  2/99                                             *
 ************************************************************************/
{
int       nargs;
Arg       args[10];

/*---------------------------------------------------------------------*/
/*
 * Create the observation plot window
 */

   nargs=0;
   XtSetArg(args[nargs], XmNheight, 120); nargs++;
   XtSetArg(args[nargs], XmNwidth, 120); nargs++;
   XtSetArg(args[nargs], XmNx, 500); nargs++;
   XtSetArg(args[nargs], XmNy, ObsWindowHeight-145); nargs++;   /* was 150 */
   ObsPlotWindow = XmCreateDrawingArea(parent, "ObsPlotWindow", args, nargs);

   XtManageChild( ObsPlotWindow );
   XtAddCallback( ObsPlotWindow, XmNexposeCallback, (XtCallbackProc)PlotObsWindowCb, NULL ); 

   return( ObsPlotWindow );

}
