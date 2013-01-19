#include "geminc.h"
#include "Nxm.h"
#include "naltxt.h"

void unviewerror_callback ( Widget, XtPointer, XtPointer );

Widget	viewfile_widget;
Widget	viewerror_widget;
Widget	viewlabel_widget;
Boolean	viewerror_up = False;
/************************************************************************
 * viewfile.c								*
 * 									*
 * Functions defined:							*
 *	unviewerror_callback						*
 *	CreateViewText							*
 *	FillViewText							*
 *									*
 * Motif routines to manage scrolled text window			*
 ***********************************************************************/

/*=====================================================================*/
/* ARGSUSED */
void unviewerror_callback ( Widget wdgt, XtPointer clnt, 
						XtPointer call )
/************************************************************************
 * unviewerror_callback							*
 *									*
 *	This does something.						*
 *									*
 * void unviewerror_callback( wdgt, clnt, call)				*
 *									*
 * Input parameters:							*
 *	wdgt		Widget						*
 *	clnt		XtPointer					*
 *	call		XtPointer					*
 **									*
 ***********************************************************************/
{
    if ( viewerror_up == True ) {
	XtUnmanageChild( viewerror_widget );
	viewerror_up = False;
    }
}

/*=====================================================================*/

Widget CreateViewText ( Widget parent )
/************************************************************************
 * CreateViewText							*
 *									*
 * Widget CreateViewText( parent )					*
 *									*
 * Input parameters:							*
 * parent	Widget							*
 *									*
 * Output parameters:							*
 * CreateViewText	Widget						*
 *									*
 **									*
 ***********************************************************************/
{
	Arg	args[20];
	int	n;
/*---------------------------------------------------------------------*/
/*
 * Create a label widget to hold filename 
 */
	n = 0;
	viewlabel_widget = CreateLabelWidget( parent, "filename",
					"View File", args, n);
	viewfile_widget = CreateScrolledText( parent, "viewfile");

	n = 0;
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	viewerror_widget = XmCreateErrorDialog( parent, "viewerror",
						args, n);
	XtAddCallback( viewerror_widget, XmNokCallback,
			(XtCallbackProc)unviewerror_callback, NULL);
	XtUnmanageChild( XmMessageBoxGetChild( viewerror_widget,
					XmDIALOG_HELP_BUTTON));
	XtUnmanageChild( XmMessageBoxGetChild( viewerror_widget,
					XmDIALOG_CANCEL_BUTTON));
	return( viewfile_widget );
}

/*=====================================================================*/

void FillViewText ( char filename[] )
/************************************************************************
 * FillViewText								*
 *									*
 * void FillViewText( filename )					*
 *									*
 * Input parameters:							*
 *	filename[]	char						*
 **									*
 ***********************************************************************/
{

	Boolean	status;
	Arg	args[10];
	int	n;
	char	string[400];
	XmString	motif_string;
/*---------------------------------------------------------------------*/


	status = FillWidgetWithFile( viewfile_widget, filename);
	if (status == False ) {
		sprintf( string, "Error in loading [%s]", filename);
		motif_string = XmStringCreateLocalized ("Error in loading file");
		n = 0;
		XtSetArg( args[n], XmNmessageString, motif_string); n++;
		XtSetValues( viewerror_widget, args, n);
		XmStringFree( motif_string);

		if (viewerror_up == False) {
			XtManageChild( viewerror_widget );
			viewerror_up = True;
			}
		 NxmLabel_setStr ( viewlabel_widget, string );
		} /* end of if */
	else	
		NxmLabel_setStr ( viewlabel_widget, filename);
}
