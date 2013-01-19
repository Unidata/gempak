#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "Nxm.h"
extern Widget group_listW;

/************************************************************************
*	MANGROUP.C							*
*									*
*   Module to take care of creating mannul-groups			*
*									*
*   Log:								*
*   Chien Lin/EAI      10/92						*
*   Chien Lin/EAI	8/93	ntrans1 				*
*   S. Wang/GSC 	1/97	clean up				*
*   G. Krueger/EAI	9/97	Changed NxmWarning -> NxmWarn_show	*
*   G. Krueger/EAI     11/97	Renamed NxmHelp functions		*
*   J. Wu/GSC 		5/01	free XmStrings				*
************************************************************************/

#define 	VISIBLE_LIST_ITEMS   10

long		SearchMode;
Widget		gframe_listW, ggroup_listW;
Widget		group_nameW, filenameW;
Widget		Inc_textsW[3];
Widget		save_group_popup;
Widget		search_nameW, modeW[2];
XmString	*frameNameXm;

extern Widget	frame_listW, model_select_toplevel;


/*
 * Private functions
 */
Widget	create_buttons (   Widget parent );
void 	create_ctrB (      Widget parent );
void 	create_groupname ( Widget parent );
void 	create_incTxt (    Widget parent );
void 	create_modeTgl (   Widget parent );
void 	create_searchCtrB( Widget parent );
void 	create_search (    Widget parent );
void	create_searchTxt ( Widget parent );
Widget 	create_savegroup ( Widget parent, char *title, char *prompt_string,
			   Widget *textw, void (*callback)(Widget,long,XtPointer) );
void 	Mode_Callback ( Widget, long, XtPointer );
void 	CtrB_Callback ( Widget, long, XtPointer );
void 	ClearInc_Callback ( Widget, XtPointer, XtPointer );
void 	GpDelete_Callback ( Widget, XtPointer,
		 	  XmListCallbackStruct* );
void 	LoadSave_Callback ( Widget, long, XtPointer );
int 	get_group ( Widget widget );
void 	change_group ( int groupno );



void create_mangroup ( Widget parent )
/************************************************************************
 *
 *	This subroutione creates the manual group widgets.
 *
 *		create_mangroup  ( parent )
 *
 *	Input parameters:
 *		parent	Wiget		parent widget id
 *
 ***********************************************************************/
{
Arg			args[10];
Cardinal		argcnt;
Widget			group_pane, form, frame[3];
Widget			rowcol, model_button;

/*----------------------------------------------------------------------*/

/* Create a popup shell & list widget for the file list. */

	group_panel_toplevel = XmCreateBulletinBoardDialog(parent,
			"group_panel", args, 0);

	argcnt = 0;
	XtSetArg(args[argcnt], XmNsashWidth, 1); argcnt++;
	XtSetArg(args[argcnt], XmNsashHeight, 1); argcnt++;
	group_pane = XtCreateManagedWidget("group_pane",
						xmPanedWindowWidgetClass,
						group_panel_toplevel,
						args,
						argcnt);

	argcnt = 0;
	form = XtCreateManagedWidget("group_form",
						xmFormWidgetClass,
						group_pane,
						NULL,
						argcnt);

	frame[0] = XtVaCreateManagedWidget("Inc_Group",
				  xmFrameWidgetClass, form,
				  XmNtopAttachment,    XmATTACH_FORM,
				  XmNleftAttachment,	 XmATTACH_FORM,
				  XmNbottomAttachment,	   XmATTACH_POSITION,
				  XmNbottomPosition,	80,
				  NULL );

	create_search(frame[0]);

	frame[1] = XtVaCreateManagedWidget("FListFrame",
				  xmFrameWidgetClass, form,
				  XmNtopAttachment,    XmATTACH_FORM,
				  XmNleftAttachment,	XmATTACH_WIDGET,
				  XmNleftWidget,    frame[0],
				  XmNbottomAttachment,	   XmATTACH_POSITION,
				  XmNbottomPosition,	80,
				  NULL );

	gframe_listW = create_framelist(frame[1], 1);

	frame[2] = XtVaCreateManagedWidget("FListGroup",
				  xmFrameWidgetClass, form,
				  XmNtopAttachment,    XmATTACH_FORM,
				  XmNleftAttachment,	XmATTACH_WIDGET,
				  XmNleftWidget,	frame[1],
				  XmNrightAttachment,	 XmATTACH_FORM,
				  XmNbottomAttachment,	   XmATTACH_POSITION,
				  XmNbottomPosition,	80,
				  NULL );

	ggroup_listW = create_grouplist(frame[2]);

	rowcol = XtVaCreateManagedWidget("GroupNameRc",
				  xmRowColumnWidgetClass, form,
				  XmNtopAttachment,    XmATTACH_POSITION,
				  XmNleftAttachment,	XmATTACH_POSITION,
				  XmNnumColumns, 1,
				  XmNorientation, XmHORIZONTAL,
				  XmNradioBehavior, False,
				  XmNpacking, XmPACK_TIGHT,
				  NULL );

	create_groupname(rowcol);
	model_button = create_buttons(form);
	create_model_selection(model_button);

	rowcol = XtVaCreateManagedWidget("ManualGroupCtrbRc",
				  xmRowColumnWidgetClass, group_pane,
				  XmNnumColumns, 1,
				  XmNorientation, XmHORIZONTAL,
				  XmNradioBehavior, False,
				  NULL );

	create_ctrB(rowcol);
}

/*=====================================================================*/

Widget create_buttons ( Widget parent )
{
Widget	button, model_button;
XmString	xmstr;

/*---------------------------------------------------------------------*/

	xmstr = XmStringCreateLocalized("Add to List");
	button = XtVaCreateManagedWidget ("AddGroupListB",
		xmPushButtonWidgetClass, parent,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNshadowThickness, 3,
		NULL );
	XmStringFree( xmstr );
	
	XtAddCallback(button, XmNactivateCallback,
		(XtCallbackProc)AddGpList_Callback, 0);

	xmstr = XmStringCreateLocalized("Add & Display");
	button = XtVaCreateManagedWidget ("AddDisplayB",
		xmPushButtonWidgetClass, parent,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNshadowThickness, 3,
		NULL );
	XmStringFree( xmstr );

	XtAddCallback(button, XmNactivateCallback,
		(XtCallbackProc)AddGpList_Callback, (XtPointer)1);

	xmstr = XmStringCreateLocalized("Delete Group");
	button = XtVaCreateManagedWidget ("DeleteGroupB",
		xmPushButtonWidgetClass, parent,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNshadowThickness, 3,
		NULL );
	XmStringFree( xmstr );

	XtAddCallback(button, XmNactivateCallback,
		(XtCallbackProc)GpDelete_Callback, NULL);

	xmstr = XmStringCreateLocalized("Load Group List");
	model_button = XtVaCreateManagedWidget ("LoadGroupListB",
		xmPushButtonWidgetClass, parent,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNshadowThickness, 3,
		NULL );
	XmStringFree( xmstr );

	XtAddCallback(model_button, XmNactivateCallback,
		(XtCallbackProc)LoadSave_Callback, (XtPointer)0);

	xmstr = XmStringCreateLocalized("Save Group List");
	button = XtVaCreateManagedWidget ("SaveGroupListB",
		xmPushButtonWidgetClass, parent,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNshadowThickness, 3,
		NULL );
	XmStringFree( xmstr );

	XtAddCallback(button, XmNactivateCallback,
		(XtCallbackProc)LoadSave_Callback, (XtPointer)1);

	save_group_popup = create_savegroup(button,
				"Group Name", "File Name",
				&filenameW, Save_Popup_Callback);

	return(model_button);

}

/*=====================================================================*/

void create_search ( Widget parent )
{
Widget		    form;

/*---------------------------------------------------------------------*/


	form = XtVaCreateManagedWidget ("SearchForm",
				   xmFormWidgetClass,
				   parent,
				   NULL );

	create_modeTgl( form );
	create_incTxt( form );
	create_searchTxt( form );
	create_searchCtrB( form );
}

/*=====================================================================*/

void create_modeTgl ( Widget parent )
/************************************************************************
 *
 *	This subroutione creates the search mode toggle button widgets.
 *
 *		CREATE_MODETGL	( PARENT )
 *
 *	Input parameters:
 *		PARENT	Wiget		parent widget
 *
 ***********************************************************************/
{
long	ii;
Widget	rowcol;
char	*labels[] = { "Incremental Search", "String Search" };

/*---------------------------------------------------------------------*/

	rowcol = XtVaCreateManagedWidget ("Mode",
				   xmRowColumnWidgetClass,
				   parent,
				   XmNtopAttachment, XmATTACH_POSITION,
				   XmNtopPosition, 2,
				   XmNleftAttachment, XmATTACH_POSITION,
				   XmNleftPosition, 1,
				   XmNnumColumns, 2,
				   XmNorientation, XmHORIZONTAL,
				   XmNradioBehavior, True,
				   XmNspacing,	 5,
				   NULL );

	for ( ii = 0; ii < (long)XtNumber(labels); ii++ ) {
		modeW[ii] = XtVaCreateManagedWidget(labels[ii],
				  xmToggleButtonWidgetClass, rowcol,
				  NULL );

		XtAddCallback(modeW[ii], XmNarmCallback,
			   (XtCallbackProc)Mode_Callback, (XtPointer)ii);
	}

/*
 * initialize search mode to mode0
 */
	setMode0();

}

/*=====================================================================*/
/* ARGSUSED */
void Mode_Callback ( Widget w, long which, XtPointer call )
{
    SearchMode = which;
}

/*=====================================================================*/

void setMode0 ( void )
{
    SearchMode = 0;
    XmToggleButtonSetState(modeW[0], True, True);
}

/***********************************************************************/

void setMode1 ( void )
{
    SearchMode = 1;
    XmToggleButtonSetState(modeW[1], True, True);
}

/*=====================================================================*/

void create_incTxt ( Widget parent )
/************************************************************************
 *
 * This subroutione creates the incremental selection button widgets.
 *
 *		CREATE_INCTXT  ( PARENT )
 *
 *	Input parameters:
 *		PARENT	Wiget		parent widget
 *
 ***********************************************************************/
{
Cardinal	    ii;
Widget		    rowcol;
char		    *labels[] = { "Start No.", "Increment", "End No." };
String		    Inc_def[] = { "1", "1", " " };

/*---------------------------------------------------------------------*/

	rowcol = XtVaCreateManagedWidget ("IncRow",
				   xmRowColumnWidgetClass,
				   parent,
				   XmNtopAttachment, XmATTACH_POSITION,
				   XmNtopPosition, 30,
				   XmNleftAttachment, XmATTACH_POSITION,
				   XmNleftPosition, 1,
				   XmNpacking,	  XmPACK_COLUMN,
				   XmNnumColumns, 3,
				   XmNorientation, XmHORIZONTAL,
				   XmNradioBehavior, False,
				   NULL );

	for ( ii = 0; ii < XtNumber(labels); ii++ ) {
		XtVaCreateManagedWidget(labels[ii],
				  xmLabelWidgetClass, rowcol,
				  XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
				  XmNalignment,  XmALIGNMENT_END,
				  NULL );

		Inc_textsW[ii] = XtVaCreateManagedWidget("Inc_textW",
				  xmTextWidgetClass, rowcol,
				  XmNcolumns,  4,
				  XmNvalue,  Inc_def[ii],
				  NULL );
	}
}

/*========================================================================*/

void create_searchTxt ( Widget parent )
{
Widget	rowcol;

/*------------------------------------------------------------------------*/
/*
 * create search text field
 */
	rowcol = XtVaCreateManagedWidget ("Search",
				  xmRowColumnWidgetClass, parent,
				  XmNtopAttachment, XmATTACH_POSITION,
				  XmNtopPosition, 70,
				  XmNleftAttachment, XmATTACH_POSITION,
				  XmNleftPosition, 1,
				  XmNnumColumns, 1,
				  XmNorientation, XmHORIZONTAL,
				  XmNradioBehavior, False,
				  XmNpacking, XmPACK_TIGHT,
				  NULL );

	XtCreateManagedWidget("Search ", xmLabelWidgetClass,
				  rowcol, NULL, 0);

	search_nameW = XtVaCreateManagedWidget("search_nameW",
				  xmTextWidgetClass, rowcol,
				  XmNcolumns,  10,
				  NULL );

}

/*========================================================================*/

Widget create_framelist ( Widget parent, int selectpolicy )
/************************************************************************
 *
 *	This subroutione creates the frame selection list widget.
 *
 *		CREATE_FRAMELIST  ( PARENT )
 *
 *	Input parameters:
 *		PARENT		Widget		parent widget
 *		selectpolicy	int	 0 --- Single, 1 --- multiple
 ***********************************************************************/
{
Arg	    args[15];
Cardinal    argcnt;
Widget	    form, list;

/*---------------------------------------------------------------------*/

	form = XtVaCreateManagedWidget ("ListForm",
				   xmFormWidgetClass,
				   parent,
				   NULL );

	XtVaCreateManagedWidget ("List of Frames",
				   xmLabelWidgetClass,
				   form,
				   XmNtopAttachment,   XmATTACH_POSITION,
				   XmNtopPosition,     2,
				   XmNleftAttachment,  XmATTACH_POSITION,
				   XmNleftPosition,    25,
				   XmNrightAttachment, XmATTACH_POSITION,
				   XmNrightPosition,   75,
				   NULL );

	argcnt = 0;
	XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_POSITION);
	argcnt++;
	XtSetArg(args[argcnt], XmNtopPosition,15);
	argcnt++;
	XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM);
	argcnt++;
	XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM);
	argcnt++;
	XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_FORM);
	argcnt++;

	if (selectpolicy){
	   XtSetArg(args[argcnt], XmNselectionPolicy,
					XmMULTIPLE_SELECT);
	   argcnt++;
	}
	else {
	   XtSetArg(args[argcnt], XmNselectionPolicy,
					XmSINGLE_SELECT);
	   argcnt++;
	}

	list = XmCreateScrolledList(form,"frame_list",args,argcnt);
	XtManageChild(list);

	return ( list );
}

/*=====================================================================*/

void create_groupname ( Widget parent )
/************************************************************************
 *
 *		CREATE_GROUPNAME  ( PARENT )
 *
 *	This subroutione creates the group name textfield widget.
 *
 *	Input parameters:
 *		PARENT	Wiget		parent widget
 *
 ***********************************************************************/
{
Arg		args[4];
Cardinal	argcnt;

/*---------------------------------------------------------------------*/

	argcnt = 0;
	XtSetArg(args[argcnt], XmNshadowThickness,3);
	argcnt++;

	XtCreateManagedWidget("Group Name ",
		xmLabelWidgetClass, parent,
		args, argcnt);

	group_nameW = XtVaCreateManagedWidget("group_nameW",
				  xmTextWidgetClass, parent,
				  NULL );
}

/*=====================================================================*/

Widget create_savegroup ( Widget parent, char *title, char *prompt_string,
			Widget *textw, void (*callback)(Widget,long,XtPointer) )
{
char		*labels[] = { "  OK  ", " Close " };
long		ii;
Cardinal	argcnt;
Arg		args[4];
Widget		popup, pushbutton, rowcol;

/*---------------------------------------------------------------------*/

	popup = XmCreateFormDialog(parent, title,
					NULL, 0);

	XtVaSetValues(XtParent(popup), XmNtitle, title, NULL);

	rowcol = XtVaCreateManagedWidget("rowcol",
				  xmRowColumnWidgetClass, popup,
				  XmNtopAttachment,    XmATTACH_POSITION,
				  XmNtopPosition,    5,
				  XmNleftAttachment,	XmATTACH_POSITION,
				  XmNleftPosition,    5,
				  XmNrightAttachment,	 XmATTACH_POSITION,
				  XmNrightPosition,    90,
				  XmNnumColumns, 1,
				  XmNorientation, XmHORIZONTAL,
				  XmNradioBehavior, False,
				  XmNpacking, XmPACK_TIGHT,
				  NULL );

	 XtCreateManagedWidget(prompt_string,
					  xmLabelWidgetClass,
					  rowcol,
					  NULL,
					  0);

	*textw = XtVaCreateManagedWidget("popuptext",
				  xmTextWidgetClass, rowcol,
				  XmNcolumns,  25,
				  NULL );

	rowcol = XtVaCreateManagedWidget("rowcol",
				  xmRowColumnWidgetClass, popup,
				  XmNtopAttachment,    XmATTACH_POSITION,
				  XmNtopPosition,    55,
				  XmNleftAttachment,	XmATTACH_POSITION,
				  XmNleftPosition,    5,
				  XmNrightAttachment,	 XmATTACH_POSITION,
				  XmNrightPosition,    95,
				  XmNbottomAttachment,	  XmATTACH_POSITION,
				  XmNbottomPosition,	95,
				  XmNnumColumns, 1,
				  XmNorientation, XmHORIZONTAL,
				  XmNradioBehavior, False,
				  XmNspacing, 300,
				  NULL );


	for ( ii = 0; ii < (long)XtNumber(labels); ii++ ) {
		argcnt = 0;
		XtSetArg(args[argcnt], XmNshadowThickness,3);  argcnt++;
		pushbutton = XtCreateManagedWidget(labels[ii],
					  xmPushButtonWidgetClass,
					  rowcol,
					  args,
					  argcnt);

		XtAddCallback(pushbutton, XmNactivateCallback,
			   (XtCallbackProc)callback, (XtPointer)ii );
	}

	return ( popup );
}

/*=====================================================================*/

void create_ctrB ( Widget parent )
 /***********************************************************************
 *
 *		CREATE_CTRB  ( PARENT )
 *
 *	This subroutione creates the control button widgets
 *		for group creation panel.
 *
 *	Input parameters:
 *		PARENT	Wiget		parent widget
 ***********************************************************************/
{
char		*labels[] = { " Help ", " Close " };
long		ii;
Widget		pushbutton;

/*---------------------------------------------------------------------*/

	for ( ii = 0; ii < (long)XtNumber(labels); ii++ ) {
		pushbutton = XtVaCreateManagedWidget(labels[ii],
				xmPushButtonWidgetClass, parent,
				XmNshadowThickness,3,
				NULL);

		XtAddCallback(pushbutton, XmNactivateCallback,
				(XtCallbackProc)CtrB_Callback,
					(XtPointer)ii);
	}
}

/*=====================================================================*/

void create_searchCtrB ( Widget parent )
 /***********************************************************************
 *
 *		CREATE_SEARCHCTRB  ( PARENT )
 *
 *	This subroutione creates the control button widgets
 *		for search panel.
 *
 *	Input parameters:
 *		PARENT	Wiget		parent widget
 ***********************************************************************/
{
Widget	button;

/*---------------------------------------------------------------------*/
/*
 * create control buttons
 */

	button = XtVaCreateManagedWidget (" Apply ",
				   xmPushButtonWidgetClass,
				   parent,
				   XmNtopAttachment, XmATTACH_POSITION,
				   XmNtopPosition, 87,
				   XmNleftAttachment, XmATTACH_POSITION,
				   XmNleftPosition, 10,
				   XmNshadowThickness, 3,
				   NULL );

	XtAddCallback(button, XmNactivateCallback,
			(XtCallbackProc)Search_Callback, NULL);

	button = XtVaCreateManagedWidget (" Clear ",
				   xmPushButtonWidgetClass,
				   parent,
				   XmNtopAttachment, XmATTACH_POSITION,
				   XmNtopPosition, 87,
				   XmNleftAttachment, XmATTACH_POSITION,
				   XmNleftPosition, 60,
				   XmNshadowThickness, 3,
				   NULL );

	XtAddCallback(button, XmNactivateCallback,
			(XtCallbackProc)ClearInc_Callback, NULL);

}

/*=====================================================================*/
/* ARGSUSED */
void CtrB_Callback ( Widget w, long which, XtPointer call )
/************************************************************************
 *
 *	Callback function of control buttons for group creation panel.
 *
 *		ctrB_Callback ( w, which, call )
 *
 ***********************************************************************/
{
int		status = 0;

/*---------------------------------------------------------------------*/

	switch (which) {

	case	0:	/* HELP */
		NxmHelp_helpBtnCb( NULL, 7, NULL );
		status = 0;
		break;

	case	1:	/* CANCEL */
		status = 1;
		break;

	default:
		status = 1;
		break;
	}

	if (status) {
		XmListDeselectAllItems(gframe_listW);
		XmTextSetString(group_nameW, "");
		XtUnmanageChild(group_panel_toplevel);
	}

}

/*=====================================================================*/
/* ARGSUSED */
void Search_Callback ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 *
 *	Callback function of Incremental selection apply button.
 *
 *	Search_Callback(w, clnt, call)
 *
 ***********************************************************************/
{
int	inc[3], ii;
char	text[20];
XmString	comptext;

/*--------------------------------------------------------------------*/

	if (SearchMode == 0) {

/*
 * clear frame list.
 */
		XmListDeselectAllItems(gframe_listW);

		for ( ii = 0; ii < 3; ii++ ) {
			get_text(Inc_textsW[ii], &text[0]);
			inc[ii] = atoi(text);
		}

/*
 * check the number of frames.
 */
		if( inc[2] > FrameNo ) {
		    add_IncNo();
		    inc[2] = FrameNo;
		}

		for ( ii = inc[0]-1; ii <= inc[2]-1; ii += inc[1] ) {
			if ( ii >= 0 )
			XmListSelectItem( gframe_listW, frameNameXm[ii], False);
		}
	}
	else {
		get_text(search_nameW, &text[0]);

		if ( text[0] != (char)NULL ) {
			comptext = XmStringCreate(text,
					XmFONTLIST_DEFAULT_TAG);
			
			for ( ii = 0; ii < FrameNo; ii++ ) {
			   if( XmStringHasSubstring(frameNameXm[ii], comptext)
				== True )
			   XmListSelectItem( gframe_listW,
						frameNameXm[ii], False);
			}
			
			XmStringFree( comptext );
		}
	}
}

/*=====================================================================*/
/* ARGSUSED */
void ClearInc_Callback ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 *
 *	Callback function of clear frame selection button.
 *
 *		ClearInc_Callback  ( W, DATA, CALL )
 *
 ***********************************************************************/
{
    XmListDeselectAllItems(gframe_listW);
}

/*=====================================================================*/
/* ARGSUSED */
void AddGpList_Callback ( Widget w, long which, XtPointer call )
/************************************************************************
 *
 *	Callback function of add group list.
 *
 *		AddGpList_Callback  ( W, which, CALL )
 *
 ***********************************************************************/
{

/*--------------------------------------------------------------------*/

	switch( which ) {
	    case 0:		/* add to group list */
		if ( get_group(w) ) {
			XmListDeselectAllItems(gframe_listW);
			XmTextSetString(group_nameW, "");
		}
		break;

	    case 1:		/* add to group list and display */
		if (get_group(w)) {
			MpanelMode = 0;
			SelectGroupNo = GroupNo;
			GroupLoadFlag = 1;

			XmListDeselectAllItems(gframe_listW);
			XmTextSetString(group_nameW, "");
			XtUnmanageChild(group_panel_toplevel);

			load_group(GroupNo);
		}
		break;
	}
}

/*=====================================================================*/

int get_group ( Widget widget )
/************************************************************************
 *									*
 *	This subroutione gets the frame selection for the		*
 *		specified group.					*
 *									*
 *		GET_GROUP  ( WIDGET )					*
 *									*
 * Input parameters:							*
 *	WIDGET		Widget		 parent widget			*
 **									*
 * Log:									*
 * T. Piper/SAIC	10/04	Changed MAX_PIXMAP to MAX_FRAME_GROUP	*
 ***********************************************************************/
{
int		framecnt, ii, status;
XmStringTable	selectedframes;
char		text[40], message[256];

/*---------------------------------------------------------------------*/

	get_text(group_nameW, &text[0]);

	if ( *text ) {

		strcpy( GroupList[GroupNo].groupname,text );

		XtVaGetValues(gframe_listW,
				XmNselectedItemCount, &framecnt,
				XmNselectedItems,     &selectedframes,
				NULL);

		if ( framecnt == 0 ) {
			NxmWarn_show( widget, "Please Select Frames." );
			status = 0;
		}

		else {
			if ( framecnt > MAX_FRAME_GROUP ) {
			    sprintf(message,
			    "The number of frames in this group exceeds the limit of %d.",
				 MAX_FRAME_GROUP);
			    NxmWarn_show( widget, message );
			    status = 0;
			}
			else {
			    GroupList[GroupNo].frame_num = framecnt;

			    for ( ii = 0; ii < framecnt; ii++ ) {
				GroupList[GroupNo].frames[ii] =
					XmListItemPos(gframe_listW,
						selectedframes[ii]);
			    }

			    GroupNo ++;
			    status = 1;
			    add_grouplist();
			 }
			 
		 }
	}		/* end of if */
	else	{
		status = 0;
		NxmWarn_show( widget, "Please Input the Group Name.");
	}

	return (status);
}

/*=====================================================================*/

void add_framelist ( char **title )
 /***********************************************************************
 *
 *
 *	This subroution fills in the frame list widget.
 *
 *		ADD_FRAMELIST  ( title )
 *
 *	Input parameters:
 *		TITLE	 char **  pointer to frame title string.
 *
 ***********************************************************************/
{
int		ii;
char		name[80];

/*---------------------------------------------------------------------*/

	if ( FrameNo == 0 ) {
		printf(" No Metafile Loaded\n");
		return;
	}
	else {
		frameNameXm = (XmString *)XtMalloc(sizeof(XmString)*(size_t)FrameNo);

		for ( ii = 0; ii < FrameNo; ii++ ) {
			sprintf(name,"[%d]  %s\n",ii+1,title[ii]);
			frameNameXm[ii] = XmStringCreate(name,
					XmFONTLIST_DEFAULT_TAG);
		}

		XtVaSetValues(frame_listW,
				XmNitems,	frameNameXm,
				XmNitemCount,	FrameNo,
				NULL);

		XtVaSetValues(gframe_listW,
                                XmNitems,       frameNameXm,
                                XmNitemCount,   FrameNo,
                                NULL);
	}
}

/*=====================================================================*/

void add_IncNo ( void )
/************************************************************************
 *
 *	This subroutione writes the end of the frame number to
 *		the corresponding widget.
 *
 *		ADD_INCNO  ( )
 *
 ***********************************************************************/
{
char	      name[5];

/*---------------------------------------------------------------------*/

	sprintf(name,"%d",FrameNo);
	XtVaSetValues(Inc_textsW[2],
			XmNvalue,	name,
			NULL);
}

/*====================================================================*/

void delete_framelist ( void )
 /***********************************************************************
 *
 *	This subroutione deletes the items in frame list widget.
 *
 *		DELETE_FRAMELIST  ( )
 *
 ***********************************************************************/
{
	int ii;

/*--------------------------------------------------------------------*/

	XmListDeleteAllItems(frame_listW);
	XmListDeleteAllItems(gframe_listW);
	for ( ii = 0; ii < FrameNo; ii++ ) {
	    XmStringFree(frameNameXm[ii]);
	}
	XtFree((XtPointer)frameNameXm);
}

/*====================================================================*/
/* ARGSUSED */
void GpDelete_Callback ( Widget widget, XtPointer clnt, 
					XmListCallbackStruct *list_data )
{
int nselect;

/*------------------------------------------------------------------*/
/*
 * return when no selection was made
 */
	XtVaGetValues(ggroup_listW, XmNselectedItemCount,
			&nselect, NULL);
	if ( nselect == 0 )
	     return;

	if (SelectGroupNo) {
		XmListDeletePos(group_listW,SelectGroupNo);
		XmListDeletePos(ggroup_listW,SelectGroupNo);
		change_group(SelectGroupNo);
		GroupNo --;
	}
}

/*====================================================================*/

void change_group ( int groupno )
{
int	ii, jj;

/*---------------------------------------------------------------------*/

	for ( ii = groupno-1; ii < (GroupNo - 1); ii++ ) {
		strcpy(GroupList[ii].groupname, GroupList[ii+1].groupname);
		GroupList[ii].frame_num = GroupList[ii+1].frame_num;
		for ( jj = 0; jj < GroupList[ii+1].frame_num; jj++ )
		      GroupList[ii].frames[jj] = GroupList[ii+1].frames[jj];
	}
}

/*====================================================================*/
/* ARGSUSED */
void LoadSave_Callback ( Widget w, long which, XtPointer call )
{
char	groupfile[256] = "\0";

/*--------------------------------------------------------------------*/

	switch ( which ) {

	    case 0: /* Load Group List */
		XtManageChild(model_select_toplevel);
		break;

	    case 1: /* Save Group List */

		if (MetaFile[0] != '\0') {

		(char *)strccpy(groupfile,MetaFile,'.');

		XtVaSetValues(filenameW,
			XmNvalue, groupfile,
			NULL);

		}

		XtManageChild(save_group_popup);
		break;

	}
}

/*====================================================================*/

int get_text ( Widget widget, char *text )
/************************************************************************
*								   	*
*   Miscellaneous utility subroutines.				   	*
*								   	*
*   Log:							   	*
*   Chien Lin/EAI      10/92					   	*
* T. Piper/SAIC		12/01	freed XmTextGetString			*
************************************************************************/
{
	int status = 0;
	char	*tmp;

/*----------------------------------------------------------------------*/

	if ( widget ) {
		strcpy (text, tmp = XmTextGetString(widget));
		XtFree(tmp);
		status = 1;
	}
	else
		status = 0;

	return(status);
}

/*====================================================================*/

char *strccpy ( char *t, char *s, char c )
{
        char *p;

        p = strrchr ( s, '/' );
	if ( p )
        	while ( p > s ) *t++ = *s++;

        p = strrchr ( s, c );
        if ( p )
                while ( s < p ) *t++ = *s++;
        else
                while ( *s ) *t++ = *s++;

        if (*t)
                *t = '\0';

        return(s);
}
