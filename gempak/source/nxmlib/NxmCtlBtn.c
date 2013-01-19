#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


/************************************************************************
 * NxmCtlBtn.c								*
 *									*
 * This module handles action (control) buttons such as ACCEPT or	*
 * CANCEL.								*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmCtlBtn_create() Create a series of control buttons from a list	*
 ***********************************************************************/

/***********************************************************************/

Widget NxmCtlBtn_create ( Widget parent, char spread_flag, char *name,
			int n, char *btnstr[], XtCallbackProc callback,
			WidgetList btnw )
/************************************************************************
 * NxmCtlBtn_create							*
 *									*
 * This function creates control buttons in a row. If the 'spread_flag' *
 * is set, the container widget will be 'FORM WIDGET' and all the	*
 * buttons will be evenly spread out; otherwise a ROWCOLUMN widget is	*
 * used. It returns a form or a rowcolumn  widget holding all the	*
 * pushbutton widgets. If input parameter 'btnw' is not NULL, it sets	*
 * the widget ID of each button in 'btnw' as a WidgetList.		*
 *									*
 * Widget NxmCtlBtn_create( parent, spread_flag, name, n, btnstr,	*
 *			    callback, btnw )				*
 *									*
 * Input parameters:							*
 *	parent		Widget	    parent widget.			*
 *	spread_flag	char	    0 - not spread, 1 - evenly spreaded *
 *	*name		char	    name for the form widget.		*
 *	n		int	    # of push-buttons.			*
 *	*btnstr[]	char	    names of each push-buttons. 	*
 *	(*callback)()	void	    callback function of pushbuttons.	*
 *									*
 * Input/Output parameters:						*
 *	btnw		WidgetList  widget ID for each pushbutton.	*
 *									*
 * Return parameters:							*
 *	  The form widget						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    04/96						*
 * G. Krueger/EAI   10/97	NxmControlBtn->NxmCtlBtn_create; Clean	*
 * T. Piper/GSC		6/01	Freed button				*
 ***********************************************************************/
{
Widget	     rc_form;
WidgetList   button;
long	     ii;
XmString     xmstr;
/*---------------------------------------------------------------------*/

	if ( spread_flag ) {
	    rc_form = XtVaCreateWidget(name,
			xmFormWidgetClass, parent,
			XmNfractionBase,   3*n+1,
			NULL);
	}
	else {
	    rc_form = XtVaCreateWidget(name,
			xmRowColumnWidgetClass, parent,
			XmNorientation, 	XmHORIZONTAL,
			NULL);
	}

	button = (WidgetList) XtMalloc((size_t)n * sizeof(Widget));

	for (ii = 0; ii < n; ii++) {

		xmstr = XmStringCreateLocalized(btnstr[ii]);
		button[ii] = XtVaCreateManagedWidget("pushb",
			xmPushButtonWidgetClass, rc_form,
			XmNlabelString, 	 xmstr,
			NULL);
		if (spread_flag) {
		    XtVaSetValues( button[ii],
			XmNleftAttachment,	 XmATTACH_POSITION,
			XmNleftPosition,	 3*ii+1,
			XmNrightAttachment,	 XmATTACH_POSITION,
			XmNrightPosition,	 3*ii+3,
			NULL);
		}

		if (btnw != NULL) {
		     btnw[ii] = button[ii];
		}

		if (callback)
		     XtAddCallback(button[ii], XmNactivateCallback,
					callback, (XtPointer)ii);
		XmStringFree(xmstr);
	}
	XtFree((XtPointer)button);
	XtManageChild(rc_form);

	return(rc_form);

}
