#include "geminc.h"
#include "cpgcmn.h"

extern XmStringCharSet char_set;

void vtitle ( Widget w, char *label )
/************************************************************************
 * VTITLE								*
 *									*
 * This function places a dialog title on the requested widget.		*
 *									*
 * VTITLE  ( W, LABEL )							*
 *									*
 * Input parameters:							*
 * w		Widget		Widget (dialog) to attach label to	*
 * label	char *		String to use as the label		*
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * E. Wehner/EAi	6/96		Created				*
 * J. Wu/GSC		5/01		free XmStrings			*
***********************************************************************/
{
    Arg al[10];
    Cardinal ac;
    XmString xmstr;
    
    ac = 0;
    xmstr = XmStringCreate(label, char_set);
    XtSetArg(al[ac], XmNdialogTitle, xmstr); 
    ac++;
    
    XtSetValues(w, al,ac);
    XmStringFree( xmstr);
}
