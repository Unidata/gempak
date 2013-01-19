#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "panelstr.h"
#include "Nxm.h"

extern	Widget	ggroup_listW;
Widget		group_listW;

Widget	fileLabelW;

void create_gpctrB ( Widget parent );
void SelGroup_Pushb_Callback ( Widget, long, XtPointer );


/************************************************************************
*	SELECT_GROUP.C							*
*									*
*   Module to take care of creating group selection panel.		*
*									*
*   Log:								*
*   Chien Lin/EAI      10/92						*
*   G. Krueger/EAI	9/97	Updated NxmWarning to NxmWarn_Show	*
*   G. Krueger/EAI     11/97	Renamed NxmHelp functions		*
*   I. Durham/GSC	5/98	Changed call for underscore		*
*   J. Wu/GSC		5/01	free XmStrings				*
************************************************************************/

void create_selectgroup ( Widget parent )
{
Widget	group_pane, form, frame, rowcol, bb;
XmString	xmstr;
/*---------------------------------------------------------------------*/
/* Create a popup shell & list widget for the file list. */
	group_select_toplevel = XmCreateBulletinBoardDialog(parent,
			"groupselect",NULL, 0);

	group_pane = XtVaCreateManagedWidget("group_pane",
				xmPanedWindowWidgetClass,
				group_select_toplevel,
				XmNsashWidth,  1,
				XmNsashHeight, 1,
				NULL);
	form = XtVaCreateManagedWidget("ListGroupForm",
				xmFormWidgetClass, group_pane,
		NULL );

	bb = XtVaCreateManagedWidget("bb1",
		xmBulletinBoardWidgetClass, form,
		NULL );

	xmstr = XmStringCreateLocalized("File:");
	fileLabelW = XtVaCreateManagedWidget ("fileLb",
		xmLabelWidgetClass, bb,
		XmNlabelString, xmstr,
		NULL );
	XmStringFree( xmstr );
	
	frame = XtVaCreateManagedWidget("List_Group",
		xmFrameWidgetClass, form,
		XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget,	  bb,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL );

	group_listW = create_grouplist(frame);

	bb = XtVaCreateManagedWidget("bb2",
		xmBulletinBoardWidgetClass, form,
		XmNtopAttachment, XmATTACH_WIDGET,
				XmNtopWidget,	  frame,
		NULL );

	rowcol = XtVaCreateManagedWidget("Model_Group",
		xmRowColumnWidgetClass, bb,
		XmNnumColumns, 1,
		XmNorientation, XmHORIZONTAL,
		XmNradioBehavior, False,
		NULL );

	create_gpctrB(rowcol);

	MultipanelFrame = XtVaCreateManagedWidget("MultiPanel",
				xmFrameWidgetClass, group_pane,
				NULL );
	create_multipanel(MultipanelFrame);
}

/*=====================================================================*/

void create_gpctrB ( Widget parent )
{
char		    *labels[] = { "Clear Screen", "Help","Close" };
Arg		args[4];
long		ii;
Cardinal	argcnt;
Widget		pushbutton;
/*---------------------------------------------------------------------*/

    for( ii = 0; ii< (long)XtNumber(labels); ii++) {
	argcnt = 0;
	XtSetArg(args[argcnt], XmNshadowThickness,3);  argcnt++;
	pushbutton = XtCreateManagedWidget(labels[ii],
			xmPushButtonWidgetClass,
			parent,
			args,
			argcnt);

	XtAddCallback(pushbutton, XmNactivateCallback,
			(XtCallbackProc)SelGroup_Pushb_Callback,
			(XtPointer)ii);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void SelGroup_Pushb_Callback ( Widget w, long which, XtPointer call )
{
int		status = 0;
/*---------------------------------------------------------------------*/

    switch (which) {

	case	0:     /* CLEAR SCREEN */
	    ClearAreas(NULL, NULL, NULL);
	    status = 0;
	    break;

	case	1:	/* HELP */
	    NxmHelp_helpBtnCb( NULL, 6, NULL );
	    status = 0;
	    break;

	case	2:	/* CANCEL */
	    status = 1;
	    break;

	default:
	    status = 1;
	    break;
    }

    if (status)
	XtUnmanageChild(group_select_toplevel);
}

/*=====================================================================*/
/* ARGSUSED */
void ClearAreas ( Widget w, XtPointer clnt, XtPointer call )
{
int	i;
int	iret;

/*---------------------------------------------------------------------*/

    if ( !NxmPrt_isPrtFlgSet() ) {
	    for(i = 0; i < PixmapData.old_pixmap_no; i ++ ) {
		if  ( i == 0 )	{
		    gstanm ( &iret );
		}
		else {
		    gsplot ( &iret );
		}
		gclear ( &iret );
	    }
	    genanm ( &iret );

	    geplot ( &iret );

	    PixmapData.current_pixmap = 0;
	    display_pixmap();
    }
}

/*=====================================================================*/

void ok_select ( Widget widget )
{
int	selct, iret;

/*---------------------------------------------------------------------*/

	XtVaGetValues(group_listW,
			XmNselectedItemCount, &selct,
			NULL);

	if ( selct == 0 ) {

		NxmWarn_show( widget, " Please Select a Group. ");
	}
	else {
		if ( !MpanelMode || ViewFrame == 1 )
		    gclear(&iret);

		GroupLoadFlag = 1;
		ViewFrame = 0;
                strcpy (panelSrc.group_name[CurrentPanel],GroupList[SelectGroupNo-1].groupname);
		load_group(SelectGroupNo);
	}
}

/*=====================================================================*/

void delete_grouplist ( void )
{
    XmListDeleteAllItems(group_listW);
    XmListDeleteAllItems(ggroup_listW);
}
