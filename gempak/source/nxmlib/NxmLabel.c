#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"


/************************************************************************
 * NxmLabel.c								*
 *									*
 * This module handles labelling functions.				*
 *									*
 * CONTENTS:								*
 *									*
 *   NxmLabel_setStr()		Sets the label string resource. 	*
 *   NxmLabel_createFrameLbl()	Sets the label for a frame widget.	*
 ***********************************************************************/

/*=====================================================================*/

void NxmLabel_setStr ( Widget w, char *label )
/************************************************************************
 * NxmLabel_setStr							*
 *									*
 * This function sets the label string resource.			*
 *									*
 * void NxmLabel_setStr(w, label)					*
 *									*
 * Input parameters:							*
 *	w		Widget	Widget ID.				*
 *	*label		char	label string.				*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	    04/96						*
 * G. Krueger/EAI   10/97	NxmSetLabel->NxmLabel_setStr; Cleanup	*
 ***********************************************************************/
{
XmString xmstr;
/*---------------------------------------------------------------------*/

	xmstr = XmStringCreateLocalized(label);
	XtVaSetValues(w, XmNlabelString, xmstr, NULL);
	XmStringFree(xmstr);

}

/*=====================================================================*/

void NxmLabel_getStr ( Widget w, char label[] )
/************************************************************************
 * NxmLabel_getStr							*
 *									*
 * This function gets the label string resource for the widget.		*
 *									*
 * void NxmLabel_getStr(w, label)					*
 *									*
 * Input parameters:							*
 *	w		Widget	Widget ID.				*
 *									*
 * Output parameters:							*
 *	label[]		char	label string.				*
 **									*
 * Log: 								*
 * E. Safford/GSC	10/99						*
 ***********************************************************************/
{
XmString xmstr;
char	 *str;
/*---------------------------------------------------------------------*/

    XtVaGetValues(w, XmNlabelString, &xmstr, NULL);
    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &str);
    XmStringFree(xmstr);

    strcpy (label, str);
    XtFree(str);
}

/*=====================================================================*/

Widget NxmLabel_createFrameLbl ( char *label_str, Widget parent_pane, 
							Widget frame_pane )
 /***********************************************************************
 * NxmLabel_createFrameLbl 						*
 *									*
 * This function sets the label for a frame widget			*
 *									*
 * Widget NxmLabel_createFrameLbl(label_str, parent_pane, frame_pane)	*
 *									*
 * Input parameters:							*
 *	*label_str	char	labeling string 			*
 *	parent_pane	Widget	parent pane widget			*
 *	frame_pane	Widget	parent frame widget			*
 *									*
 * Return parameters:							*
 * NxmLabel_createFrameLbl	Widget	  The label widget		*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   	09/96						*
 * G. Krueger/EAI  	10/97   NxmFrameLabel->NxmLabel_createFrameLbl;	*
 *			  		Cleanup				*
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       * 
 * T. Piper/SAIC	03/05	Removed XmVERSION/XmREVISION check	*
 ***********************************************************************/
{
Widget		labelw;
XmString	str;
/*---------------------------------------------------------------------*/

    str = XmStringCreateLocalized(label_str);

    labelw = XtVaCreateManagedWidget ("lb",
		xmLabelGadgetClass,	   frame_pane,
		XmNlabelString, 	   str,
		XmNchildType,		   XmFRAME_TITLE_CHILD,
		XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
		NULL);

    XmStringFree(str);

    return(labelw);

}
