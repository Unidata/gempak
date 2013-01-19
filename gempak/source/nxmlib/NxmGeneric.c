#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

#define WIDGET_HEIGHT	 25
#define WIDGET_OFFSET	 15

static Widget	_genButtons[4];
static int	_genNButtons;
static Boolean	_buttonReady = False;

/*
 *  Private functions
 */
void NxmGeneric_buttonCb ( Widget, 
	void (*callback)(Widget,long,XmToggleButtonCallbackStruct*),
        XmToggleButtonCallbackStruct *cbs );


/************************************************************************
 * NxmGeneric.c								*
 *									*
 * This module displays a generic popup dialog box.			*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmGeneric_show()		Create a generic dialog.		*
 *   NxmGeneric_buttonCb()	Destroy the generic dialog.		*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmGeneric_show ( Widget parent, char *title, char *message, 
			int  numBtns,  char *button[],
			XtCallbackProc callback )
/************************************************************************
 * NxmGeneric_show							*
 *									*
 * This function creates a generic popup dialog.  A button will be	*
 * created for each button label until a NULL label is found.  The	*
 * calling routine is responsible for popdown and/or destruction of	*
 * the created widget.							*
 *									*
 * Widget NxmGeneric_show (parent, title, message, button0, button1,	*
 *			 button2, button3, orient, which, callback)	*
 *									*
 * Input parameters:							*
 *	parent		Widget	ID of parent widget.			*
 *	*title		char	widget title bar label			*
 *	*message	char	message to be displayed			*
 *	numBtns		int	number of buttons to display		*
 *	*button[]	char	array of button strings			*
 *	callback	XtCallbackProc	button callback function	*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *			Widget	ID of created widget			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/99	initial coding				*
 * S. Law/GSC		07/00	removed XmStringCreate call		*
 * E. Safford/SAIC	04/04	use push buttons not radio buttons and  *
 *				  simplify greatly -- rm vert alignment *
 ***********************************************************************/
{
    Widget		label, button_pane;
    Widget		mainw;
/*---------------------------------------------------------------------*/

    mainw = XmCreateFormDialog (parent, title, NULL, 0);

    label = XtVaCreateManagedWidget (message, 
			xmLabelWidgetClass, 	mainw,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		WIDGET_OFFSET,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		WIDGET_OFFSET,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNrightOffset,		WIDGET_OFFSET,
			NULL);


    button_pane  = (Widget) NxmCtlBtn_create( mainw, 1, "",
    				     numBtns, button,
				     (XtCallbackProc)callback, NULL );
    XtVaSetValues( button_pane,
			 XmNtopAttachment,	XmATTACH_WIDGET,
 		         XmNtopWidget,		label,
			 XmNtopOffset,		WIDGET_OFFSET,
			 XmNleftAttachment,	XmATTACH_FORM,
			 XmNleftOffset,		WIDGET_OFFSET,
			 XmNrightAttachment,	XmATTACH_FORM,
			 XmNrightOffset,	WIDGET_OFFSET,
			 XmNbottomAttachment,	XmATTACH_FORM,
			 XmNbottomOffset,	WIDGET_OFFSET,   
			 NULL );

    _buttonReady = True;

    XtManageChild (button_pane);
    XtManageChild (mainw);
    XmUpdateDisplay (mainw);

    return (mainw);
}

/*=====================================================================*/

void NxmGeneric_buttonCb ( Widget wid, 
				void (*callback)(Widget, long, 
				XmToggleButtonCallbackStruct *cbs), 
				XmToggleButtonCallbackStruct *cbs )
/************************************************************************
 * NxmGeneric_buttonCb							*
 *									*
 * This function destroys the generic dialog.				*
 *									*
 * void NxmGeneric_buttonCb (wid, callback, cbs)	 		*
 *									*
 * Input parameters:							*
 *	wid		Widget		ID of widget to be destroyed	*
 *	*callback()	void		callback function for buttons	*
 *	*cbs		XmToggleButtonCallbackStruct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		02/99	initial coding				*
 ***********************************************************************/
{
    long ii;
/*---------------------------------------------------------------------*/

    if (_buttonReady && cbs->set) {
	_buttonReady = False;

	for (ii = 0; ii < _genNButtons; ii++) {
	    if (_genButtons[ii] == wid) break;
	}

	if (ii != _genNButtons) {
	    callback (wid, ii, cbs);
	}
    }
}
