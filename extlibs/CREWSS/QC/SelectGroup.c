#include "gui.h"


/*ARGSUSED*/
void SelectGroupCb( Widget button, XtPointer data, XtPointer cb_data )
/************************************************************************
 * SelectGroupCb
 *                                                                      *
 * This routine is the callback for the Select Group button.            *
 * It toggles on Select_Group_Mode, resetting the event handler on the  *
 * map to the Select_Next_Group routine, allowing the user to select    *
 * priority groups with rubberband boxes.  When the Select Group        *
 * button is pushed again, it toggles off Select_Group_Screen, and      *
 * resets the event handler on the Map to ViewObs.                      *
 *                                                                      *
 * SelectGroupCb( w, data, cb_data )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          The input widget                *
 *      data            XtPointer       The input data for the widget   *
 *                                                                      *
 * Output parameters:                                                   *
 *      cb_data         XtPointer      The callback data for the widget *
 **                                                                     *
 * P. Freeman  NCEP  4/99                                               *
 ***********************************************************************/
{

      if (Select_Group_Screen)  {
         Turn_Off_Select_Group();
         ProcessGroupCb(NULL, NULL, NULL);
      } else {
         WorldMapCb(NULL, NULL, NULL);
         Select_Group_Screen = True;
         XtVaSetValues( button, XmNforeground, Red.pixel, NULL); /* change button color to red */
         XmUpdateDisplay( button );
         XtRemoveEventHandler( Map, ButtonPress, False, (XtEventHandler)ViewObs , NULL);
         XtAddEventHandler( Map, ButtonPress, False, (XtEventHandler)Select_Next_Group, NULL);
      }

}

/*=================================================================================*/

void Turn_Off_Select_Group ( void )
{

      if (Select_Group_Screen)  {
         Select_Group_Screen = False;
         XtVaSetValues( MenuButtons[1], XmNforeground, Black.pixel, NULL); /* change button color back to black */
         XmUpdateDisplay( MenuButtons[1] );
         XtRemoveEventHandler( Map, ButtonPress, False, (XtEventHandler)Select_Next_Group , NULL);
         XtAddEventHandler( Map, ButtonPress, False, (XtEventHandler)ViewObs, NULL);
      }
}

/*===============================================================================*/

void Select_Next_Group(void)
/************************************************************************
 * Select_Next_Group                                                    *
 *                                                                      *
 * This routine allows the user to select another group.  It is the     *
 * event handler for button clicks when Select_Group_Screen is on.      *
 * It allows the user to select a group of obs, identifying the group   *
 * to be next in priority order.                                        *
 *                                                                      *
 * P. Freeman  NCEP  4/99                                               *
 ***********************************************************************/
{

int           badbox;
float         x1_ndc, x2_ndc, y1_ndc, y2_ndc;
int           Return;
size_t	      ii, jj;
/*
 *  Return to the world map.
 */

   if (Zoomed) WorldMapCb(NULL, NULL, NULL);

/*
 * Save the changes from the current obs and turn off contours and vectors.
 */

   if (SaveObsChanges()) {

/*
 *  Retrieve the user-selected boundry box 
 */

      badbox = Get_Box( 1, &x1_ndc, &x2_ndc, &y1_ndc, &y2_ndc);
      if (! badbox) {

/*
 * Find each observation in the box, and mark it with the current
 * priority order number.
 */

         NhlDataToNDC( Mapid, Lon, Lat, NumObs, X, Y, NULL, NULL, &Return, &missing);

         NumPri++;
      
         for (ii=0, jj=0; ii<NumObs; ii++){
            if ( (X[ii] > x1_ndc) && (X[ii] < x2_ndc) &&
                 (Y[ii] > y2_ndc) && (Y[ii] < y1_ndc) &&
                 (Priority[ii] == -1) ) {
                Priority[ii] = NumPri;
                jj++;
            }
         }
         /* printf("SelectObs: %i obs selected\n", j); */

         if (jj == (size_t)0) NumPri--;      /* if none found, go back to previous priority number */

      }
      Replot(0);
   }

}
