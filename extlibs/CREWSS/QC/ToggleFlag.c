#include "gui.h"

/*ARGSUSED*/
void ToggleFlagCb( Widget widget, int WhichFlag, XtPointer cb_data)
/************************************************************************
 *                                                                      *
 * ToggleFlagCb                                                         *
 *   This callback routine is called when one of the flag buttons is    *
 *   toggled.  WhichFlag identifies which parameter (row) and which     *
 *   which flag (col) was toggled.                                      *
 *      Row = WhichFlag/2                                               *
 *      Col = WhichFlag - (2*Row)                                       *
 *   To act like a radio button, clear out the other flag, and set the  *
 *   value of the FlagButtonValues array to reflect the current state   *
 *   of the button (it was toggled before invoking this routine).       *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 ***********************************************************************/
{
int Row, Col;

   Row = WhichFlag/2;
   Col = WhichFlag % 2;
   /* printf("In ToggleFlag, Row = %d, Col = %d\n", Row, Col); */

   if (Col==0) {                       /* act like radio button, make sure other button is off */
      XmToggleButtonSetState(FlagButtonWidgets[Row][1], False, False);
      FlagButtonValues[Row][1] = False; 
   } else {
      XmToggleButtonSetState(FlagButtonWidgets[Row][0], False, False);
      FlagButtonValues[Row][0] = False;
   }

   if (XmToggleButtonGetState(FlagButtonWidgets[Row][Col])) {      /* set state of values to reflect button */
      FlagButtonValues[Row][Col] = True;
   } else {
      FlagButtonValues[Row][Col] = False;
   }

}
