#include "geminc.h"
#include "naltxt.h"

/*=====================================================================*/

void AddToList ( Widget widget, char string[], int position )
/*
 * AddToList
 */
{
XmString	motif_string; 

/*---------------------------------------------------------------------*/

    motif_string = XmStringCreateLocalized( string );

    XmListAddItemUnselected( widget, motif_string , position );

    XmStringFree( motif_string );

}
