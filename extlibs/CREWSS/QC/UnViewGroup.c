#include "gui.h"


/*ARGSUSED*/
void UnViewGroupCb( Widget w, XtPointer data, XtPointer cb_data )
/************************************************************************
 * UnViewGroupCb                                                        *
 *                                                                      *
 * This routine is the callback for the UnView button.                  *
 * Unview a group of observations so they can be reviewed.              *
 * Do not undo any changes.  Select group of observations by accepting  *
 * map boundry points via a rubberband box selection.                   *
 *                                                                      *
 ***********************************************************************/
{

int        k, badbox;
float      x1_ndc, x2_ndc, y1_ndc, y2_ndc;
int        Return;
size_t     ii, jj;


/*---------------------------------------------------------------------*/

   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */


/*
 * Save the changes from the current obs and turn off contours and vectors.
 */

   if (SaveObsChanges()) {

      XtVaSetValues( w, XmNforeground, Red.pixel, NULL); /* change button color to red */
      XmUpdateDisplay( w );

/*
 *  Retrieve the user-selected boundry box
 */

      badbox = Get_Box( 0, &x1_ndc, &x2_ndc, &y1_ndc, &y2_ndc);


/*
 * Find each observation in the box, unprioritize it. Restore the suspect field from
 * the suspect flags in the obs file.
 */

      if (!badbox) {
         NhlDataToNDC( Mapid, Lon, Lat, NumObs, X, Y, NULL, NULL, &Return, &missing);

         for (ii=0, jj=0; ii<NumObs; ii++){                      /* for each obs */
         if ( (X[ii] > x1_ndc) && (X[ii] < x2_ndc) &&       /* if its in the box */
                 (Y[ii] > y2_ndc) && (Y[ii] < y1_ndc) ) {      
               for (k=0; k<NumOnMap; k++)                    /* then find it in the Diplayed list */
                  if (*(Viewed + (k*2)) == (int)ii) {
                     *(Viewed + k*2 + 1) = False;  /* and mark it as undisplayed */
                     jj++;
                  }
            }
         }
         Replot(0);
      }

      XtVaSetValues( w, XmNforeground, Black.pixel, NULL); /* change button color to black */
      XmUpdateDisplay( w );
   }
}
