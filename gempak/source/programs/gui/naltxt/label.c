#include "geminc.h"
#include "naltxt.h"


Widget CreateLabelWidget ( Widget parent, char name[], char message[], 
							Arg *args, int n )
/************************************************************************
 * CreateLabelWidget							*
 *									*
 * Widget CreateLabelWidget(parent, name, message, args, n)		*
 *									*
 * Input parameters:							*
 *	parent		Widget						*
 *	name[]		char						*
 *	message[]	char						*
 *	*args		Arg						*
 *	n		int						*
 **									*
 ***********************************************************************/
{
  Widget label_widget;
  XmString label_text;
/*---------------------------------------------------------------------*/
/*
 * Convert message to XmString
 */
    label_text = XmStringCreateLocalized( message );

/*
 * Set label_text         
 */
    XtSetArg( args[n], XmNlabelString, label_text ); n++;

/*
 * Create label widget
 */
    label_widget = XmCreateLabel( parent, name, args, n );

/*
 * Free the string
 */
    XmStringFree( label_text);

    return( label_widget);

}
