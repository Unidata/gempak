#include "gui.h"

/* ARGSUSED */
void ProcessGroupCb(Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 *                                                                      *
 * ProcessGroup                                                         *
 *   This callback routine displays the next suspect obs in priority    *
 *   order.                                                             *
 *                                                                      *
 ***********************************************************************/
{

/* 
 * If history is up, get rid of it
 */

   XtUnmanageChild(History);


   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */

    if (SaveObsChanges()) {
/*
 * Save the changes from the current obs and turn off contours and vectors.
 */

       Previous_View_Obs = Current_View_Obs;
       Previous_Pri_Obs = Current_Pri_Obs;
       /* printf("In ProcessGroupCb: Previous_View_Obs = %d\n", Previous_View_Obs);
       printf("                 Previous_Pri_Obs = %d\n", Previous_Pri_Obs); */


       ClearFields();
       Current_View_Obs = -1;                 /* make sure DisplayObs doesn't highlight the latest ViewObs */


/*     Only destroy and remove the contours if we end up redrawing the screen. */


/*
    * Get the index for the next observation with the current priority
    * or the first for the next priority.
    */

     if (NumPri >= 0) {

        Get_Next_Pri_Obs(&Current_Pri);

/*
 *   Display the next observation
 */

        if (Current_Pri_Obs < (int)NumObs) {
            DisplayObs( Current_Pri_Obs );
        } else
           WorldMapCb(NULL, NULL, NULL);

     }
  }

}

/*=====================================================================*/

int Get_Next_Pri_Obs(int *Current_Pri)
/***********************************************************************
* This routine gets the index to the next priority order observation.  *
* The remainder of the Priority array is searched for the next obs     *
* with the current priority order.  If none are found, and this is not *
* the last priority number, then begin again at the beginning of the   *
* array searching for the next obs with the next priority number.      *
***********************************************************************/
{

int Next_Pri_Obs;

   /* printf("Get_Next_Pri_Obs: Current_Pri=%d NumPri=%d NumObs=%d\n", *Current_Pri, NumPri, NumObs); */


/*
 *   Look through remainder of array looking for next obs with
 *   priority equal to the current priority (Current_Pri_Obs) that is
 *   also suspect (Suspect=1)
 */
   Next_Pri_Obs = Current_Pri_Obs + 1;

   if (*Current_Pri == -1) *Current_Pri = 0;

   if (*Current_Pri <= NumPri) {
      while ( (Next_Pri_Obs < (int)NumObs) &&
             ((Priority[Next_Pri_Obs] != *Current_Pri) ||
              (Suspect[Next_Pri_Obs] != 1) ) ) {
         Next_Pri_Obs++;
      }
   }

/*
 *   If got to the end of array, then go to next priority, if any,
 *   and start again and beginning of array looking for first obs
 *   with next priority
 */
   if (Next_Pri_Obs >= (int)NumObs) {
      if (*Current_Pri < NumPri) {
         (*Current_Pri)++;
         Next_Pri_Obs = 0;
         while ( (Next_Pri_Obs < (int)NumObs) &&
                ((Priority[Next_Pri_Obs] != *Current_Pri) ||
                 (Suspect[Next_Pri_Obs] != 1) ) )
             Next_Pri_Obs++;
      }
   }
/*
 *   Set Current_Pri_Obs to point to next obs (or NumObs+1 if found none)
 *   If found return true else return false.
 */

   Current_Pri_Obs = Next_Pri_Obs; 

   if (Next_Pri_Obs < (int)NumObs) {
      return(1);
   };
   return(0);
}
