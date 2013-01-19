#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

static Widget	_promptTxtW;
static Widget	_promptW;
static char	_promptStr[256]; 

void _NxmPrompt_ok_cb ( Widget, 
			long (*callback)(Widget,XtPointer,XtPointer), 
			XtPointer );


/************************************************************************
 * NxmPrompt.c								*
 *									*
 * This module produces prompt boxes.					*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmPrompt_create() Create a PROMPT dialog box.			*
 *   _NxmPrompt_ok_cb()	OK button handler				*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmPrompt_create ( Widget parent, char *title, 
			  char *prompt_string, XtCallbackProc callback ) 
/************************************************************************
 * NxmPrompt_create							*
 *									*
 * This function prompts the user for more input.			*
 *									*
 * Widget NxmPrompt_create( parent, title, prompt_string, callback )	*
 *									*
 * Input parameters:							*
 *	parent		Widget	 ID of parent widget.			*
 *	*title		char	Title to be displayed on prompt box.	*
 *	*prompt_string	char	String to prompt user for more input.	*
 *	callback	XtCallbackProc	 Callback function of OK button.*
 *									*
 * Return parameters:							*
 *	 NxmPrompt_create	Widget  The prompt dialog widget	*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	       94						*
 * G. Krueger/EAI   11/97  Added headers; Cleanup			*
 *			   NxmPromptPopupCreate->NxmPrompt_create	*
 ***********************************************************************/
{
Widget	    button, rowcol;

/*---------------------------------------------------------------------*/
        _promptW = XmCreateFormDialog(parent, title, NULL, 0);
        XtVaSetValues(XtParent(_promptW), XmNtitle, title, NULL);

        rowcol = XtVaCreateManagedWidget("rowcol",
		xmRowColumnWidgetClass, _promptW,
                XmNtopAttachment,       XmATTACH_POSITION,
                XmNtopPosition,         5,
                XmNleftAttachment,      XmATTACH_POSITION,
                XmNleftPosition,        5,
                XmNrightAttachment,     XmATTACH_POSITION,
                XmNrightPosition,       90,
                XmNnumColumns,          1,
                XmNorientation,         XmHORIZONTAL,
                XmNradioBehavior,       False,
                XmNpacking,             XmPACK_TIGHT,
                NULL );

         XtCreateManagedWidget(prompt_string,
                xmLabelWidgetClass, rowcol,
                NULL, 0);

         _promptTxtW = XtVaCreateManagedWidget("popuptext",
                xmTextFieldWidgetClass, rowcol,
                XmNcolumns,        25,
                NULL );

	rowcol = XtVaCreateManagedWidget("rowcol",
                xmRowColumnWidgetClass, _promptW,
                XmNtopAttachment,       XmATTACH_POSITION,
                XmNtopPosition,         55,
                XmNleftAttachment,      XmATTACH_POSITION,
                XmNleftPosition,        5,
                XmNrightAttachment,     XmATTACH_POSITION,
                XmNrightPosition,       95,
                XmNbottomAttachment,    XmATTACH_POSITION,
                XmNbottomPosition,      95,
                XmNnumColumns,          1,
                XmNorientation,         XmHORIZONTAL,
                XmNradioBehavior,       False,
                XmNspacing,             300,
                NULL );

	button = XtVaCreateManagedWidget("  OK  ",
		xmPushButtonWidgetClass, rowcol,
		XmNshadowThickness,      3,
		NULL );

        XtAddCallback(button, XmNactivateCallback,
                (XtCallbackProc)_NxmPrompt_ok_cb, (XtPointer)callback );

	button = XtVaCreateManagedWidget(" CLOSE ",
		xmPushButtonWidgetClass, rowcol,
		XmNshadowThickness,      3,
		NULL );

        XtAddCallback(button, XmNactivateCallback,
                (XtCallbackProc)NxmClose_popupCb, _promptW );

        return ( _promptW );
}

/*=====================================================================*/
/* ARGSUSED */
void _NxmPrompt_ok_cb ( Widget w, long (*callback)(Widget,XtPointer,XtPointer),
							XtPointer call )
/************************************************************************
 * _NxmPrompt_ok_cb							*
 *									*
 * This function accepts the user's response to the prompt.		*
 *									*
 * void _NxmPrompt_ok_cb( w, callback, call )				*
 *									*
 * Input parameters:							*
 *	w		Widget	   Widget ID of the close button.	*
 *	*callback()	int	   Callback function of the OK button.	*
 *	call		XtPointer  Data from the widget.		*
 *									*
 * Output parameters:							*
 *	    NONE							*
 *									*
 * Return parameters:							*
 *	    NONE							*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	       94						*
 * G. Krueger/EAI   11/97  Added header block				*
 ***********************************************************************/
{
int	status;
char	*text;
/*---------------------------------------------------------------------*/
	text = XmTextFieldGetString(_promptTxtW);
	strcpy( _promptStr, text );
	XtFree(text);

	if ( callback != NULL )
		status = callback(w, _promptStr, NULL);

	if ( status )
	    XtUnmanageChild(_promptW);
}
