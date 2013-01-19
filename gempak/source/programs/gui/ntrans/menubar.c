#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "Nxm.h"
extern Widget	model_toplevel, local_toplevel;

/************************************************************************
* menubar.c						   		*
*								   	*
*   Module to take care of creating toplevel menubar for ntrans.   	*
*								   	*
*   Log:							   	*
* Chien Lin/EAI		02/93					   	*
* S. Wang/GSC		01/97	code upgrade and clean up	   	*
* S. Wang/GSC		10/97	add stop button disable option	   	*
* T. Piper/SAIC		01/04	removed DWELL_TABLE define		*
* S. Jacobs/NCEP	03/04	Replaced NULL array items for menus	*
* C. Bailey/HPC		05/05	Add Procedures menu to top menu bar	*
************************************************************************/

/*
 * Private functions 
 */
#ifdef Linux
void Quit_OK_Callback	( Widget, XtPointer, XtPointer ) __attribute__((noreturn));
#else
void Quit_OK_Callback	( Widget, XtPointer, XtPointer );
#endif
void create_topmenu	( Widget parent, Pixel parent_bk );
void Exit_Callback	( Widget, XtPointer, XtPointer );
void FileMenu_Callback  ( Widget, long, XtPointer );
void GroupMenu_Callback ( Widget, long, XtPointer );
void OptionMenu_Callback( Widget, long, XtPointer );
void ViewMenu_Callback  ( Widget, long, XtPointer );
void Procedure_Callback ( Widget, long, XtPointer );


WidgetList	file_mb;
WidgetList	subfile_buttons;
extern Widget	frame_select_toplevel;

/*
 * The last NULL item in the arrays is necessary for use in the
 * function NxmMenuPulldownBuild. Do NOT remove it from the 
 * declarations below.
 */

static _NXMmenuItem   sub_filemenu[] = {
  { "Models", &xmCascadeButtonGadgetClass, 'M', NULL,
	NULL, NULL, 0, NULL, NULL},
  { "Model Lists", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>M",
	"Ctrl + M", (XtCallbackProc)FileMenu_Callback, 0, NULL, NULL},
  { "Local Products", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>L",
	"Ctrl + L", (XtCallbackProc)FileMenu_Callback, 5, NULL, NULL},
  { "User-Defined", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>F",
	"Ctrl + F", (XtCallbackProc)FileMenu_Callback, 1, NULL, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

static _NXMmenuItem   file_menu[] = {
  { "Open", &xmCascadeButtonGadgetClass, 'O', NULL,
	NULL, NULL, 0, sub_filemenu, NULL},
  { "Update Model", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>U",
	"Ctrl + U", (XtCallbackProc)FileMenu_Callback, 4, NULL, NULL},
  { "Print", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>P",
	"Ctrl + P", (XtCallbackProc)FileMenu_Callback, 2, NULL, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

static _NXMmenuItem   arrange_menu[] = {
  { "Auto-group", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>A",
	"Ctrl + A", (XtCallbackProc)GroupMenu_Callback, 0, NULL, NULL},
  { "Manual-group", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>G",
	"Ctrl + G", (XtCallbackProc)GroupMenu_Callback, 1, NULL, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

static _NXMmenuItem   view_menu[] = {
  { "Select Group", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>S",
	"Ctrl + S", (XtCallbackProc)ViewMenu_Callback, 0, NULL, NULL},
  { "Select Frame", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>V",
	"Ctrl + V", (XtCallbackProc)ViewMenu_Callback, 1, NULL, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

static _NXMmenuItem   subOption_menu[] = {
  { "Enable", &xmPushButtonGadgetClass, 0, NULL, NULL,
	(XtCallbackProc)OptionMenu_Callback, 1, NULL, NULL},
  { "Disable", &xmPushButtonGadgetClass, 0, NULL, NULL,
	(XtCallbackProc)OptionMenu_Callback, 2, NULL, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

static _NXMmenuItem   option_menu[] = {
  { "Dwell Rate", &xmPushButtonGadgetClass, 0, "Ctrl<KeyPress>D",
	"Ctrl + D", (XtCallbackProc)OptionMenu_Callback, 0, NULL, NULL},
  { "Stop Button", &xmCascadeButtonGadgetClass, 0, NULL, NULL, NULL,
	(int)NULL, subOption_menu, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

static _NXMmenuItem   procedure_menu[] = {
  { "Restore Procedure File", &xmPushButtonGadgetClass, 0, NULL,
	NULL, (XtCallbackProc)Procedure_Callback, 0, NULL, NULL},
  { "Save Procedure File", &xmPushButtonGadgetClass, 0, NULL,
	NULL, (XtCallbackProc)Procedure_Callback, 1, NULL, NULL},
  {NULL, NULL, 0, NULL, NULL, NULL, 0, NULL, NULL}
};

/*=====================================================================*/

void create_top_menubar ( Widget parent )
 /***********************************************************************
 *									*
 * create_top_menubar							*
 *									*
 *	This function creates the menubar widgets.			*
 *									*
 * void create_top_menubar(parent)					*
 *									*
 * Input parameters:							*
 *	parent	Widget		parent widget				*
 *									*
 **   Log:								*
 *   Chien Lin/EAI	02/93						*
 *   S. Wang/GSC	01/97	rewrite 				*
 *   S. Law/GSC		02/99	removed dwell_panel from loop list	*
 *   E. Safford/GSC	07/01	make Arrange & View insensitive to start*
 ***********************************************************************/
{
    Widget	buttonrc, stop_rc;
    Pixel	background;

/*---------------------------------------------------------------------*/

    menubar_form = XtVaCreateWidget("top_menubar_form",
				    xmFormWidgetClass,  parent,
				    XmNtopAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,  XmATTACH_FORM,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL);
    XtVaGetValues(parent, XmNbackground, &background, NULL);

    create_topmenu(menubar_form, background);

/*
 * create dwell popup widget
 */
    dwell_panel = NxmDwell_popupCreate( toplevel_form, "DwellPanel");

    buttonrc = NxmAnimationPanelCreate( menubar_form, "AnimationButtons",
				       "yellow", "black", menu_b, 2,
				       (XtCallbackProc)NxmLoopButtonCallback, displayPixmap);

    NxmLoopbuttonSensitive( False );

    stop_rc = XtVaCreateWidget( "stop_rc",
			       xmRowColumnWidgetClass, menubar_form,
			       XmNtopAttachment,	XmATTACH_FORM,
			       XmNrightAttachment,	XmATTACH_FORM,
			       XmNbottomAttachment,	XmATTACH_FORM,
			       XmNorientation,		XmHORIZONTAL,
			       NULL);

/*
 * create stop/busy buttons
 */
    NxmBusy_createBtns(stop_rc);

/*
 * initialize STOP button as insensitive
 */
    NxmBusy_setStopBtn(0);

    XtManageChild(stop_rc);

    XtVaSetValues(buttonrc,
		  XmNbackground,	background,
		  XmNtopAttachment,	XmATTACH_FORM,
		  XmNrightAttachment,	XmATTACH_WIDGET,
		  XmNrightWidget,	stop_rc,
		  XmNrightOffset,	30,
		  XmNorientation,	XmHORIZONTAL,
		  NULL);

    XtVaSetValues(parent, XmNbackground, background, NULL);

    XtManageChild(menubar_form);

    XtSetSensitive(menu_b[1], False);
    XtSetSensitive(menu_b[2], False);

}

/*=====================================================================*/
/* ARGSUSED */
void create_topmenu ( Widget parent, Pixel parent_bk )
/************************************************************************
 *									*
 * create_topmenu							*
 *									*
 * this function create the top menu bar				*
 *									*
 * void create_topmenu(parent, parent_bk)				*
 *									*
 * Input parameters:							*
 * parent		Widget						*
 * parent_bk		Pixel						*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   1/97 						*
 * G. Krueger/EAI  8/97 NxmExitDialog->NxmExit_create			*
 * G. Krueger/EAI  9/97 Changed NxmWarning -> NxmWarn_show		*
 * G. Krueger/EAI 11/97 Renamed NxmHelp functions			*
 ***********************************************************************/
{
Arg	    args[15];
Cardinal    argcnt;
Widget	    menubar, button;

/*---------------------------------------------------------------------*/

	argcnt = 0;
	XtSetArg(args[argcnt],XmNtopAttachment,  XmATTACH_FORM); argcnt++;
	XtSetArg(args[argcnt],XmNleftAttachment, XmATTACH_FORM); argcnt++;

	menubar = XmCreateMenuBar(menubar_form, "topmenubar",
						args, argcnt);
	XtVaSetValues(menubar, XmNbackground, parent_bk, NULL);

	file_mb = (WidgetList) XtMalloc( XtNumber(file_menu) * sizeof(Widget) );
	subfile_buttons = (WidgetList) XtMalloc( XtNumber(sub_filemenu) *
					sizeof(Widget) );

	file_menu[0].sub_buttons = subfile_buttons;

	menu_b[0]=NxmMenuPulldownBuild(menubar, file_mb,
				"File", 'F',  file_menu);
	menu_b[1]=NxmMenuPulldownBuild(menubar, NULL,
				"Arrange", 'A', arrange_menu);
	menu_b[2]=NxmMenuPulldownBuild(menubar, NULL,
				"View", 'V',  view_menu);
	menu_b[3]=NxmMenuPulldownBuild(menubar, NULL,
				"Option", 'O',	option_menu);
	menu_b[4]=NxmMenuPulldownBuild(menubar, NULL,
				"Procedures", 'P', procedure_menu);


	button = XmCreateCascadeButton(menubar, "Help", NULL, 0);
	XtVaSetValues(button, XmNbackground, parent_bk, NULL);

	XtAddCallback(button, XmNactivateCallback, 
			(XtCallbackProc)NxmHelp_helpBtnCb,
			(XtPointer)02);
	XtManageChild(button);

	button = XmCreateCascadeButton(menubar, "Exit", NULL, 0);
	XtVaSetValues(button, XmNbackground, parent_bk, NULL);

	XtAddCallback(button, XmNactivateCallback, Exit_Callback, NULL);
	XtManageChild(button);

	XtManageChild(menubar);

/*
 * create some popup windows of the menu
 */
	create_mangroup(toplevel_form);
	create_selectframe(toplevel_form);
	create_model_menu( subfile_buttons[0] );
	npfw_create(toplevel_form);

	XtSetSensitive(menu_b[1], False);
	XtSetSensitive(menu_b[2], False);
	XtSetSensitive(file_mb[1], False);
	XtSetSensitive(file_mb[2], False);

}

/*=====================================================================*/
/* ARGSUSED */
void Exit_Callback ( Widget w, XtPointer clnt, XtPointer call )
{
char message[50];

/*---------------------------------------------------------------------*/

	strcpy(message, "OK to Exit from ntrans?");
	NxmExit_create(w, "Exit Confirmation", message, 
				Quit_OK_Callback, NULL);
}

/*=====================================================================*/
/* ARGSUSED */
void Quit_OK_Callback ( Widget w, XtPointer clnt, XtPointer call ) 
/************************************************************************
* T. Piper/SAIC         07/03   replaced gemdisplay with XtDisplay()    *
************************************************************************/
{
	Close_Meta();

	XFlush(XtDisplay(DrawingW));

	XtCloseDisplay(XtDisplay(DrawingW));
	exit(0);
}

/*=====================================================================*/
/* ARGSUSED */
void FileMenu_Callback ( Widget w, long which, XtPointer call )
{
    switch (which) {
	case 0:     /* OPEN MODELS*/
	    OpenModel = 1;
	    XtManageChild(model_toplevel);
	    break;
	case 1:     /* OPEN User_Defined Files*/
	    XtManageChild(file_select_toplevel);
	    break;
	case 2:      /* Print */
	    NxmPrt_prtWPopup();
	    break;
	case 4:      /* Update Model */
	    create_model_menu( subfile_buttons[0] );
	    if ( model_toplevel )
		XtDestroyWidget( model_toplevel );
	    create_models(toplevel_form);
	    break;
	case 5:      /* Local Products */
	    if ( local_toplevel != NULL )
		XtManageChild(local_toplevel);
	    else
		NxmWarn_show(DrawingW, "No Local Products.");
	    break;
    }
}

/*==========================================================================*/
/* ARGSUSED */
void Procedure_Callback ( Widget w, long which, XtPointer call )
{
    switch (which) {
        case 0:     /* Restore Procedure File */
	    npfw_popup(0);
            break;
        case 1:     /* Save Procedure File */
            npfw_popup(1);
            break;
    }
}

/*==========================================================================*/
/* ARGSUSED */
void GroupMenu_Callback ( Widget w, long which, XtPointer call ) 
{
    switch (which) {
        case 0:     /* Auto-GROUPING */
            autogroup();
            break;
        case 1:     /* Manual-GROUPING */
            XtManageChild(group_panel_toplevel);
            break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void ViewMenu_Callback ( Widget w, long which, XtPointer call )
{
    switch (which) {
	case 0:     /* Select Group */
	    XtManageChild(group_select_toplevel);
	    break;
	case 1:     /* Select Frame */
	    XtManageChild(frame_select_toplevel);
	    break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void OptionMenu_Callback ( Widget w, long which, XtPointer call )
{
    switch ( which ) {
	case 0:
	    XtManageChild(dwell_panel);
	    break;
	case 1:
	    NxmBusy_setStopBtn(1);
	    break;
	case 2:
	    NxmBusy_setStopBtn(0);
	    break;
    }
}
