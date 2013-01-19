#include "geminc.h"
#include "gemprm.h"

Widget CreateScrolledText ( Widget parent, char name[] );
Boolean FillWidgetWithFile ( Widget widget, char filename[] );

/************************************************************************
 * text.c								*
 *									*
 * 	text widgets functions						*
 *									*
 * CONTENTS:								*
 *	CreateScrolledText						*
 *	FillWidgetWithFile						*
 *									*
 ***********************************************************************/

/*=====================================================================*/

Widget CreateScrolledText ( Widget parent, char name[] )
/************************************************************************
 * CreateScrolledText							*
 *									*
 * Widget  CreateScrolledText( parent, name )				*
 *									*
 * Input parameters:							*
 *	parent		Widget						*
 *	name[]		char						*
 *									*
 * Output parameters:							*
 *	CreateScrolledText	Widget					*
 **									*
 ************************************************************************/
{

	Widget	text_widget;
	Arg	args[10];
	int	n;

/*---------------------------------------------------------------------*/

	n = 0;
	XtSetArg( args[n], XmNrows , 24 ); n++;
	XtSetArg( args[n], XmNcolumns, 80 ); n++;
	XtSetArg( args[n], XmNscrollHorizontal , True ); n++;
	XtSetArg( args[n], XmNscrollVertical , True ); n++;
	XtSetArg( args[n], XmNeditable , False ); n++;
	XtSetArg( args[n], XmNeditMode , XmMULTI_LINE_EDIT ); n++;

	text_widget = XmCreateScrolledText( parent, name, args, n);
	XtManageChild( text_widget );
	return( text_widget );

}

/*===========================================================================*/

Boolean FillWidgetWithFile ( Widget widget, char filename[] )
/************************************************************************
 * FillWidgetWithFile                                              	*
 *                                                                      *
 * Boolean FillWidgetWithFile( widget, filename )                 	*
 *                                                                      *
 * Input parameters:                                                    *
 *       widget          Widget                                         *
 *       filename[]      char                                           *
 *									*
 * Output parameters:							*
 *	FillWidgetWithFile	Boolean					*
 **                                                                     *
 * Log:									*
 * S. Jacobs/NCEP	 3/04	Removed unprintable characters		*
 ***********************************************************************/
{

	FILE	*fp;
	struct stat	file_info;
	char	*buffer;
	long	bytes_read;
	int	ier;

/*---------------------------------------------------------------------*/

	if ( (fp = fopen(filename, "r")) == NULL ) return( False );

	if ( stat(filename, &file_info ) != 0 ) {
		fclose(fp);
		return( False );
		} 

	buffer = (char *) XtMalloc( file_info.st_size + 5);
	if ( buffer == (char *) NULL) {
		fclose( fp );
		return( False );
		}
	bytes_read = (long) fread( buffer, 1, file_info.st_size, fp);
	fclose(fp);

	buffer[file_info.st_size-1] = '\0';

	cst_unpr ( buffer, buffer, &ier );

	if ( bytes_read < file_info.st_size) {
		XtFree( buffer );
		return( False );
		}

	XmTextSetString( widget, buffer );
	XtFree( buffer );
	return( True );

}
