#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


/************************************************************************
 * NxmTxtIn.c								*
 *									*
 * This module produces a label widget and a text field input as        *
 * composit input widget.						*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmTxtIn_create() Create a PROMPT dialog box.			*
 ***********************************************************************/

/***********************************************************************/

Widget NxmTxtIn_create ( Widget parent, char *labelstr, int nc, 
							Widget *textw ) 
/************************************************************************
 * NxmTxtIn_create							*
 *									*
 * This function creates a row column widget holding a label and a text *
 * field widget.							*
 *									*
 * Widget NxmTxtIn_create(parent, labelstr, nc, textw )			*
 *									*
 * Input parameters:							*
 *	parent		Widget	 ID of parent widget.			*
 *	*labelstr	char     label string				*
 *	nc		int      number of columns of text field widget	*
 *									*
 * Output parameters:							*
 *	*textw		Widget	 text widget				*
 *									*
 * Return parameters:							*
 *	  The row column widget						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	       12/97						*
 ***********************************************************************/
{
Widget	    rowcol;
/*---------------------------------------------------------------------*/

	rowcol = XtVaCreateWidget("rowcol",
		xmRowColumnWidgetClass, parent,
                XmNnumColumns,          1,
                XmNorientation,         XmHORIZONTAL,
                XmNradioBehavior,       False,
                XmNpacking,             XmPACK_TIGHT,
                NULL );

	XtCreateManagedWidget(labelstr,
                xmLabelWidgetClass, rowcol,
                NULL, 0);

	*textw = XtVaCreateManagedWidget("text",
                xmTextFieldWidgetClass, rowcol,
                XmNcolumns,             nc,
                NULL );

	
	XtManageChild(rowcol);

        return( rowcol );
}
