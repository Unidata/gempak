#include "gui.h"


/*ARGSUSED*/
void SetDupFlagCb( Widget widget, int which, XtPointer cb_data)
/************************************************************************
 *                                                                      *
 * SetDupFlagCb                                                         *
 *   This is the callback for the Dup button.                           *
 *   Toggle the duplicate flag. If set, make the button red.            *
 *                                                                      *
 ***********************************************************************/
{
   if (duplicate)
      duplicate = False;
   else
      duplicate = True;

   if (duplicate)
      XtVaSetValues(widget,  XmNforeground, Red.pixel, XmNlabelString, XmStringCreateLocalized("Dup Flag Set"), NULL);
   else if (suspect_dup)
      XtVaSetValues(widget, XmNforeground, Red.pixel, XmNlabelString, XmStringCreateLocalized("Duplicate??"),NULL);
   else
      XtVaSetValues(widget, XmNforeground, Black.pixel, XmNlabelString, XmStringCreateLocalized("Duplicate"), NULL);

   XmUpdateDisplay( ObsWindow );

}
