#include "gui.h"

/*ARGSUSED*/
void SetAllFlagsCb ( Widget widget, int AorR, XtPointer cb_data)
/**************************************************************************
 *                                                                        *
 * SetAllFlagsCb                                                          *
 *   This the callback for the AcceptAll and RejectAll buttons.           *
 *   Set all the accept flags (AorR = 0), or reject flags (AorR=1),       *
 *   or clear all flags ((AorR = 3).                                      *
 *                                                                        *
 *************************************************************************/
{
int i, j;

/* if AorR ==2, then clear all flags and buttons */

   if (AorR ==2) {
      for (i=0; i<5; i++)
          for (j=0; j<2; j++){
             FlagButtonValues[i][j] = False;
             XmToggleButtonSetState(FlagButtonWidgets[i][j], False, False);
          }
   } else {


/* else, if AorR == True, then set all accept flags and clear reject */
/*       else                  set all reject flags and clear accept */

      for (i=0; i<5; i++) 
         for (j=0; j<2; j++) 
            if (j != AorR) { 
               FlagButtonValues[i][j] = False;
               XmToggleButtonSetState(FlagButtonWidgets[i][j], False, False);
            } else {
               FlagButtonValues[i][AorR] = True;
               XmToggleButtonSetState(FlagButtonWidgets[i][AorR], True, False);
            }
   } 
}
