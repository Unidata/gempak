#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "nmap_data.h"


#define NAMEPROJSHOW	5     /* number of shown projection choices */
#define Min(a, b)	( (a) < (b) ) ? (a) : (b)     /* maximum of a and b */

typedef struct {
	char	name[100];	 /* name of the projection item */
	char	gmpkname[20];	 /* projection name in GEMPAK */
	char	parm[100];	 /* optional parameter names */
}proj_t;

proj_t	 _projItem[] = {
  { "Default (DEF)", "DEF", {(char)NULL} },
  { "North Polar Stereographic (NPS)", "NPS", {(char)NULL} },
  { "NH Lambert Conic Conformal (LCC)", "LCC", {(char)NULL} },
  { "Cylindrical Equidistant (CED)", "CED", {(char)NULL} },
  { "Mercator (MER)", "MER", {(char)NULL} },
  { "Polar Stereographic (STR)", "STR", "/90.00;-105.00;0.00" },
  { "South Polar Stereographic (SPS)", "SPS", {(char)NULL} },
  { "SH Lambert Conic Conformal (SCC)", "SCC",{(char)NULL} },
  { "Modified Cylindrical Equidistant (MCD)", "MCD", {(char)NULL} },
  { "Universal Transverse Mercator (UTM)", "UTM", {(char)NULL} },
  { "Azimuthal Equidistant (AED)", "AED", "/90.00;-105.00;0.00" },
  { "North Orthographic (NOR)", "NOR", {(char)NULL} },
  { "South Orthographic (SOR)", "SOR", {(char)NULL} },
  { "Lambert Equal Area (LEA)", "LEA", "/90.00;-105.00;0.00" },
  { "Transverse Mercator (TVM) ", "TVM", {(char)NULL} },
  { "Orthographic (ORT)", "ORT", "/90.00;-105.00;0.00" },
  { "Gnomonic (GNO)", "GNO", "/90.00;-105.00;0.00" },
  { "Satellite Image (SAT) ", "SAT", {(char)NULL}  },
  { "Radar Image (RAD) ", "RAD", {(char)NULL}  },
};


char	_gareaEdit[MAX_STR]; 
char	_gareaCopy[MAX_STR]; 

char	_projEdit[MAX_STR];  
char	_projCopy[MAX_STR];

int	_projOptInx;		/* index of optional munu	*/
char	_curDef[73];		/* content of current DEF projection */


Widget	_toggleBt[NAMEPROJSHOW];/* PROJ toggle buttons		*/
Widget	_defaultBt;		/* PROJ default button		*/
Widget	_defaultConLabel;	/* PROJ default content label	*/
Widget	_defaultConFrame;	/* PROJ default content frame	*/
Widget	_othersBt;		/* PROJ others button		*/
Widget	_projOption;		/* PROJ optional menu button	*/
WidgetList  _optButton; 	/* option menu buttons		*/

Widget _mapCustomW;		/* map custom definition popup window  */
Widget _projTextW;		/* PROJ variable text frield widget    */
Widget _gareaTextW;		/* GAREA variable text frield widget   */


/*
 * Private functions
 */

void mpcstw_ctlBtnCb 	( Widget, long, XtPointer );
void mpcstw_defConCb 	( Widget, XtPointer, XtPointer );
void mpcstw_gareaCreate ( Widget parent );
void mpcstw_gareaDisplay ( void );
void mpcstw_getDef 	( void );
void mpcstw_projBtSnstv ( Boolean state );
void mpcstw_projCreate 	( Widget parent );
void mpcstw_projDefaultCb ( Widget, XtPointer, XtPointer );
void mpcstw_projInit 	( char *proj );
void mpcstw_projOptionCb ( Widget, long, XtPointer );
void mpcstw_projOthersCb ( Widget, XtPointer, XtPointer );
void mpcstw_projToggleCb ( Widget, long, XtPointer );
void mpcstw_resetDefCon ( void );
void mpcstw_textCb 	( Widget, long, XtPointer );
static void mpcstw_updtMapDvr ( void );

/************************************************************************
 * nmap_mpcstw.c							*
 *									*
 * This module creates the custom map selection popup window and	*
 * defines the callback functions for nmap.				*
 *									*
 * CONTENTS:								*
 *   mpcstw_create()	   creates the custom map definition popup.	*
 *   mpcstw_projCreate()   creates the PROJ definition area		*
 *   mpcstw_gareaCreate()  creates the GAREA definition area		*
 *   mpcstw_popup()	   open the popup window			*
 *   mpcstw_isUp()	   check whether the popup window is up 	*
 *   mpcstw_projBtSnstv()  set the projection area sensitivity		*
 *   mpcstw_resetDefCon()  reset the DEF proj content			*
 *   mpcstw_projInit()	   proj selection initialization		*
 *   mpcstw_popdown()	   close the popup window			*
 *   mpcstw_gareaDisplay() initialize garea text display		*
 *   mpcstw_projToggleCb() callback function for proj toggle buttons	*
 *   mpcstw_projDefaultCb()callback function for proj default button	*
 *   mpcstw_projOthersCb() callback function for proj others button	*
 *   mpcstw_projOptionCb() callback function for option menu		*
 *   mpcstw_textCb()	   callback function for proj/garea textfield	*
 *   mpcstw_defConCb()	   callback for DEF proj content button 	*
 *   mpcstw_ctlBtnCb()	   callback function for control buttons	*
 ***********************************************************************/

/*=====================================================================*/

Widget mpcstw_create ( Widget parent )
/************************************************************************
 * mpcstw_create							*
 *									*
 * This function creates the custom map definition popup window.	*
 *									*
 * Widget mpcstw_create(parent)						*
 *									*
 * Input parameters:							*
 *  parent	 Widget  parent form widget ID				*
 *									*
 * Output parameters:							*
 * mpcstw_create	Widget	map selection popup window widget ID	*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	   04/96						*
 * G. Krueger/EAI  10/97	NxmControlBtn->NxmCtlBtn_create 	*
 * I. Durham/GSC   05/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP  12/98	Fixed cast of NULL for LINUX		*
 ***********************************************************************/
{
Widget pane, form;
char   *btnstr[]={ "Accept", "Help", "Cancel" };

/*---------------------------------------------------------------------*/

	_optButton = (WidgetList)XtMalloc( XtNumber(_projItem) *
					sizeof(Widget));

/*
 * create dialog shell
 */
	_mapCustomW = XmCreateFormDialog(parent, "mpcstw_popup",
			NULL,	0);

	XtVaSetValues(_mapCustomW, XmNnoResize, True, NULL);

	XtVaSetValues(XtParent(_mapCustomW),
			XmNtitle, "Customized Map Definition",
			NULL);

/*
 * create a pane widget
 */
	pane = XtVaCreateWidget("mpcstw_pane",
			xmPanedWindowWidgetClass, _mapCustomW,
			XmNsashWidth,		  1,
			XmNsashHeight,		  1,
			NULL);

/*
 * create the projection & garea
 */
	mpcstw_gareaCreate(pane);
	mpcstw_projCreate(pane);
	XtManageChild(pane);

/*
 * create control buttons
 */
	form = NxmCtlBtn_create(pane, 1, "mpcstw_ctlBtn", XtNumber(btnstr),
				btnstr, (XtCallbackProc)mpcstw_ctlBtnCb, NULL);

	XtVaSetValues(form, XmNmarginHeight, 15, NULL);

	XtManageChild(pane);

	return(_mapCustomW);

}

/*=====================================================================*/

void mpcstw_projCreate ( Widget parent )
/************************************************************************
 * mpcstw_projCreate							*
 *									*
 * This function creates proj area					*
 *									*
 * void mpcstw_projCreate(parent)					*
 *									*
 * Input parameters:							*
 *  parent	 Widget  parent form widget ID				*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI		04/96						*
 * S. Wang/GSC		09/96	Add others selections			*
 * S. Wang/GSC		10/96	Add DEF selection and content		*
 * G. Krueger/EAI	10/97	NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
int	 nn;
long	ii;
Widget	 frame, rc0, rc, pane_fr;
Widget	 menu, rowcol;
XmString xmstr ;
Arg	 args[5] ;

/*---------------------------------------------------------------------*/

	frame = XtVaCreateWidget("mpcstw_projFrame",
			xmFrameWidgetClass, parent,
			NULL);

	pane_fr = XtVaCreateManagedWidget("mpcstw_projFrame",
			xmPanedWindowWidgetClass, frame,
			XmNsashWidth,		  1,
			XmNsashHeight,		  1,
			NULL);

	NxmLabel_createFrameLbl("Projection", pane_fr, frame);

/*
 * create a parent row column widget for projection selection
 * panel
 */
	rc0 = XtVaCreateWidget("rc",
			xmRowColumnWidgetClass, pane_fr,
			XmNorientation, 	XmVERTICAL,
			NULL);

/*
 * create GEMPAK PROJ input string area
 */
	rc = XtVaCreateWidget("mpcstw_projGEMPAKRc",
			xmRowColumnWidgetClass, rc0,
			XmNorientation, 	XmHORIZONTAL,
			NULL);
	XtVaCreateManagedWidget("PROJ",
			xmLabelGadgetClass, rc,
			NULL);
	_projTextW = XtVaCreateManagedWidget("txt",
			xmTextFieldWidgetClass,     rc,
			XmNcolumns,		    35,
			XmNmarginHeight,	    0,
			NULL);
	XtAddCallback(_projTextW, XmNactivateCallback,
			(XtCallbackProc)mpcstw_textCb, (XtPointer)1);

	XtManageChild(rc);

	rc = XtVaCreateWidget("mpcstw_projRc",
			xmRowColumnWidgetClass, rc0,
			XmNorientation, 	XmVERTICAL,
			XmNradioBehavior,	True,
			XmNspacing,		0,
			NULL);
/*
 * create default projection button and label
 */
	rowcol = XtVaCreateManagedWidget( "orc",
		xmRowColumnWidgetClass, rc,
		XmNorientation, 	XmHORIZONTAL,
		XmNradioBehavior,	True,
		XmNspacing,		60,
		NULL );

	_defaultBt = XtVaCreateManagedWidget( _projItem[0].name,
			xmToggleButtonWidgetClass, rowcol,
			XmNradioBehavior,	   True,
			NULL );
	XtAddCallback( _defaultBt, XmNarmCallback,
			(XtCallbackProc)mpcstw_projDefaultCb,
			(XtPointer)0 );

/*
 * create default projection show content button
 */
	_defaultConFrame = XtVaCreateManagedWidget("mpcstw_defFrame",
			    xmFrameWidgetClass,  rowcol,
			    XmNshadowType,	 XmSHADOW_OUT,
			    XmNshadowThickness,  2,
			    NULL);
	xmstr = XmStringCreateLocalized("	 DEF Content   ");
	_defaultConLabel = XtVaCreateManagedWidget( "mpcstw_defLabel",
			    xmLabelWidgetClass,  _defaultConFrame,
			    XmNlabelString,	 xmstr,
			    XmNrecomputeSize,	 False,
			    XmNheight,		 4,
			    XmNwidth,		 85,
			    NULL);
	XmStringFree(xmstr);
	XtAddEventHandler( _defaultConLabel,
			ButtonReleaseMask,	FALSE,
			(XtEventHandler)mpcstw_defConCb,
			(XtPointer)0);

/*
 * create commonly used projection radio button group
 */
	for ( ii = 1; ii < NAMEPROJSHOW; ii++ ) {
		_toggleBt[ii] = XtVaCreateManagedWidget( _projItem[ii].name,
				xmToggleButtonWidgetClass, rc,
				XmNmarginHeight,	   0,
				NULL );
		XtAddCallback( _toggleBt[ii], XmNarmCallback,
			(XtCallbackProc)mpcstw_projToggleCb,
			(XtPointer)ii );
	}

	XtManageChild(rc);

	rowcol = XtVaCreateManagedWidget("orc",
		xmRowColumnWidgetClass, rc,
		XmNorientation, 	XmHORIZONTAL,
		XmNradioBehavior,	TRUE,
		XmNpacking,		XmPACK_TIGHT,
		NULL);

	_othersBt = XtVaCreateManagedWidget("Others:",
			xmToggleButtonWidgetClass, rowcol,
			NULL );

	XtAddCallback(_othersBt, XmNarmCallback,
			(XtCallbackProc)mpcstw_projOthersCb,
			(XtPointer)NAMEPROJSHOW);


/*
 * create other projection option menu
 */
	menu = XmCreatePulldownMenu(rowcol, "mpcstw_projOptMenu", NULL, 0);

	nn = 0;
	xmstr = XmStringCreateLocalized("");

	XtSetArg(args[nn], XmNsubMenuId, menu); nn++;
	XtSetArg(args[nn], XmNlabelString, xmstr); nn++;

	_projOption = XmCreateOptionMenu(rowcol, "mpcstw_projOpt", args, nn);
	XmStringFree(xmstr);

	for (ii = NAMEPROJSHOW; ii < (long)XtNumber(_projItem); ii++) {
		_optButton[ii] = XtVaCreateManagedWidget(_projItem[ii].name,
			xmPushButtonGadgetClass, menu,
			NULL );

		XtAddCallback(_optButton[ii], XmNactivateCallback,
			(XtCallbackProc)mpcstw_projOptionCb,
			(XtPointer)ii);
	}

	_projOptInx = NAMEPROJSHOW;

	XtManageChild(_projOption);

	XtManageChild(rc0);
	XtManageChild(frame);
}

/*=====================================================================*/

void mpcstw_gareaCreate ( Widget parent )
/************************************************************************
 * mpcstw_gareaCreate							*
 *									*
 * This function creates garea definition area				*
 *									*
 * void mpcstw_gareaCreate(parent)					*
 *									*
 * Input parameters:							*
 *  parent	 Widget  parent form widget ID				*
 *									*
 * Output parameters:							*
 *			NULL						*
 *									*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI		04/96						*
 * G. Krueger/EAI	10/97  NxmFrameLabel->NxmLabel_createFrameLbl	*
 ***********************************************************************/
{
Widget	 frame, rc, pane_fr;
/*---------------------------------------------------------------------*/

	frame = XtVaCreateWidget("mpcstw_gareaFrame",
			xmFrameWidgetClass,	parent,
			NULL);

	pane_fr = XtVaCreateManagedWidget("mpcstw_gareaFrame",
			xmPanedWindowWidgetClass, frame,
			XmNsashWidth,		  1,
			XmNsashHeight,		  1,
			NULL);

	NxmLabel_createFrameLbl("Graphic Area", pane_fr, frame);

/*
 * create GEMPAK GAREA input string area
 */
	rc = XtVaCreateWidget("mpcstw_gareaGEMPAKRc",
			xmRowColumnWidgetClass, pane_fr,
			XmNorientation, 	XmHORIZONTAL,
			XmNmarginWidth, 	10,
			XmNmarginHeight,	10,
			NULL);

	XtVaCreateManagedWidget("GAREA",
			xmLabelGadgetClass,	rc,
			NULL);

	_gareaTextW = XtVaCreateManagedWidget("txt",
			xmTextFieldWidgetClass,     rc,
			XmNcolumns,		    35,
			XmNmarginHeight,	    0,
			NULL);
	XtAddCallback(_gareaTextW, XmNactivateCallback,
			(XtCallbackProc)mpcstw_textCb, (XtPointer)1);

	XtManageChild(rc);

	XtManageChild(frame);
}

/*=====================================================================*/

void mpcstw_popup ( char *proj, char *garea )
/************************************************************************
 * mpcstw_popup 							*
 *									*
 * This function pops up the map customization window.			*
 *									*
 * void mpcstw_popup(proj, garea)					*
 *									*
 * Input parameters:							*
 *     proj	char *	PROJ variable					*
 *     garea	char *	GAREA variable					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 ***********************************************************************/
{
    strcpy (_projEdit,  proj);
    strcpy( _projCopy,  proj );

    strcpy (_gareaEdit, garea);
    strcpy( _gareaCopy, garea );

    XtManageChild(_mapCustomW);

    mpcstw_gareaDisplay();

    if ( strcmp( _projCopy, "SAT" ) == 0 ) {
	mpcstw_projBtSnstv(False);
    }

/*
 * initialize PROJ definition area
 */
    mpcstw_projInit( _projEdit );
    XmTextSetString( _projTextW, _projEdit );
}

/*=====================================================================*/

int mpcstw_isUp ( void )
/************************************************************************
 * mpcstw_isUp								*
 *									*
 * This function pops up the map customization window.			*
 *									*
 * int mpcstw_isUp()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * mpcstw_isUp		int		0 -- false, 1 -- true		*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI		10/96						*
 ***********************************************************************/
{
    return(XtIsManaged(_mapCustomW));
}

/*=====================================================================*/

void mpcstw_projBtSnstv ( Boolean state )
/************************************************************************
 * mpcstw_projBtSnstv							*
 *									*
 * This is a function.							*
 *									*
 * void mpcstw_projBtSnstv( state )					*
 *									*
 * Input Parameters:							*
 *	state		Boolean		State of button			*
 **									*
 * Log:									*
 ***********************************************************************/
{
int	i;
/*---------------------------------------------------------------------*/

    for ( i = 1; i < NAMEPROJSHOW; i++ )
	XtSetSensitive(_toggleBt[i], state);
    XtSetSensitive(_othersBt, state);
    XtSetSensitive(_defaultBt, state);
    XtSetSensitive(_projOption, state);
    XtSetSensitive(_projTextW, state);
    XtSetSensitive(_defaultConFrame, state);

}

/*=====================================================================*/

void mpcstw_projInit ( char *proj )
/************************************************************************
 * mpcstw_projInit							*
 *									*
 * This function initialize the proj definition area			*
 *									*
 * void mpcstw_projInit( proj ) 					*
 *									*
 * Input parameters:							*
 *     proj	char *	PROJ variable					*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 ***********************************************************************/
{
int	i;
/*---------------------------------------------------------------------*/

	if ( strncmp( _projItem[0].gmpkname, proj, 3) == 0 ) {
	    XmToggleButtonSetState( _defaultBt,
					TRUE, FALSE );
	    XtSetSensitive(_defaultConFrame, True);
	}
	else {
	    for ( i=1; i < NAMEPROJSHOW ; i++ ) {
		if ( strncmp( _projItem[i].gmpkname, proj, 3) == 0 ) {
			XmToggleButtonSetState( _toggleBt[i],
					TRUE, FALSE );
			return;
		}
	    }
	    for ( i= NAMEPROJSHOW; i < (int)XtNumber(_projItem); i++ ) {
		if ( strncmp(_projItem[i].gmpkname, proj, 3) == 0 ) {
			XmToggleButtonSetState( _othersBt,
					TRUE, FALSE );
			mpcstw_projOptionCb( _optButton[i], i, NULL );
		}
	    }
	}
}

/*=====================================================================*/

void mpcstw_popdown ( void )
/************************************************************************
 * mpcstw_popdown							*
 *									*
 * This function pops down the data selection popup window.		*
 *									*
 * void mpcstw_popdown()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI		04/96						*
 * S. Wang/GSC		09/96	 add clean up before exit		*
 ***********************************************************************/
{
int	i;
/*---------------------------------------------------------------------*/

/*
 * unset all buttons
 */
	XmToggleButtonSetState(_defaultBt, FALSE, FALSE);
	mpcstw_resetDefCon();
	for ( i =1; i<NAMEPROJSHOW; i++ )
	    XmToggleButtonSetState( _toggleBt[i], FALSE, FALSE );
	XmToggleButtonSetState(_othersBt, FALSE, FALSE);

	if ( strcmp( _projCopy, "SAT" ) == 0 )
		mpcstw_projBtSnstv(True);

	XtUnmanageChild(_mapCustomW);
}

/*=====================================================================*/

void mpcstw_gareaDisplay ( void )
/************************************************************************
 * mpcstw_gareaDisplay							*
 *									*
 * This function initialize garea text display				*
 *									*
 * void mpcstw_gareaDisplay()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		10/96						*
 * T. Piper/SAIC	12/07	Replaced for loop with while loop to 	*
 *				prevent segmentation fault.		*
 ***********************************************************************/
{
char	str_copy[100];
char	*str_dot;
char	*str_single;
char	str_display[100];
char	str_singles[4][100];
int	ii, ival, len;
/*---------------------------------------------------------------------*/

    strcpy(str_copy, _gareaEdit);

    if ( strchr(str_copy,';') == NULL )
	strcpy( str_display, _gareaEdit );
    else {
	str_display[0] = '\0';
	str_single = strtok( str_copy, ";" );
	strcpy( str_singles[0], str_single);

	ival = 1;
	while ( (str_single = strtok( NULL, ";" )) != NULL ) {
	    strcpy( str_singles[ival], str_single);
	    ival++;
	}

	for ( ii=0; ii<ival; ii++ ) {
	    if ( (str_dot = strchr(str_singles[ii] ,'.') )
						!= NULL ) {
		str_dot++;
		for (len=0;isdigit(*(str_dot+len))!=0;len++)
		;
		*( str_dot+(Min(2,len)) ) = '\0';
	    }
	    strcat(str_display, str_singles[ii]);

	    if ( ii != 3 )
		strcat(str_display, ";");
	}
    }
    XmTextSetString( _gareaTextW, str_display );
}

/*=====================================================================*/

void mpcstw_getDef ( void )
/************************************************************************
 * mpcstw_getDef							*
 *									*
 * This function initialize garea text display				*
 *									*
 * void mpcstw_getDef() 						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *		       NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		10/96						*
 ***********************************************************************/
{
    strcpy(_curDef, "AAA");
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_projToggleCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mpcstw_projToggleCb							*
 *									*
 * This is the callback for the proj toggle buttons.			*
 *									*
 * void mpcstw_projToggleCb ( w, which, call )				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	which	long		client data				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *		       NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 ***********************************************************************/
{
    XmToggleButtonSetState(_defaultBt, FALSE, FALSE);
    mpcstw_resetDefCon();
    XmToggleButtonSetState(_othersBt, FALSE, FALSE);
    XmTextSetString(_projTextW, _projItem[which].gmpkname );
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_projOptionCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mpcstw_projOptionCb							*
 *									*
 * This is the callback for proj option menus.				*
 *									*
 * void mpcstw_projOptionCb(w, which, call)				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	which	long		client data				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 ***********************************************************************/
{
char	temp[100];
/*---------------------------------------------------------------------*/

    _projOptInx = (int)which ;
    strcpy( temp, _projItem[which].gmpkname );
    strcat( temp, _projItem[which].parm );
    XmTextSetString( _projTextW, temp ) ;
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_projOthersCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * mpcstw_projOthersCb							*
 *									*
 * This is the callback for the proj Others button			*
 *									*
 * void mpcstw_projOthersCb(w, which, call)				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	clnt	XtPointer	client data				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NULL						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 ***********************************************************************/
{
int	i;
char	temp[100];
/*---------------------------------------------------------------------*/

    for ( i =1;i<NAMEPROJSHOW;i++ )
	XmToggleButtonSetState( _toggleBt[i], FALSE, FALSE );
    XmToggleButtonSetState( _defaultBt, FALSE, FALSE );
    mpcstw_resetDefCon();

    strcpy( temp, _projItem[_projOptInx].gmpkname );
    strcat(temp, _projItem[_projOptInx].parm);
    XmTextSetString( _projTextW, temp );
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_projDefaultCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * mpcstw_projDefaultCb 						*
 *									*
 * This is the callback for the Default proj button			*
 *									*
 * void mpcstw_projDefaultCb(w, clnt, call)				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	clnt	XtPointer	client data				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 ***********************************************************************/
{
int	i;
/*---------------------------------------------------------------------*/

    for ( i =1; i<NAMEPROJSHOW; i++ )
	XmToggleButtonSetState( _toggleBt[i], FALSE, FALSE );
    XmToggleButtonSetState(_othersBt, FALSE, FALSE);

    XmTextSetString( _projTextW, _projItem[0].gmpkname );
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_textCb ( Widget w, long flag, XtPointer call )
/************************************************************************
 * mpcstw_textCb							*
 *									*
 * This is the callback for the proj Text field widget			*
 *									*
 * void mpcstw_textCb(w, flag, call)					*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	flag	long		= 1 draw map, = 0 do not draw map	*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	09/96	projTextCb and gareaTextCb			*
 * C. Lin/EAI	11/96	combine projTextCb and gareaTextCb together	*
 * C. Lin/EAI	02/97	bug fix in freeing string 'text'		*
 * C. Lin/EAI	07/97	call new mapw module				*
 * S. Jacobs/NCEP	10/99	Added current loop to setting map proj	*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop   	*
 * M. Li/GSC		03/01	removed mapw_getDEFproj and mapw_setMap	*
 * E. Safford/GSC	05/01	added mpcstw_updtMapDvr  		*
 * J. Wu/GSC       	05/01	free XmStrings    			*
 ***********************************************************************/
{
int	      	ignore;
char	      	*text;
unsigned char 	shadow_char;
XmString      	str_def_con;
/*---------------------------------------------------------------------*/
/*
 * get the PROJ input
 */
    text = XmTextFieldGetString(_projTextW);

    if ( (strlen(text)==(size_t)0) || (strcmp( _projCopy, "SAT" ) == 0) ) {
	strcpy( _projEdit, _projCopy);
	XmTextSetString( _projTextW, _projEdit );
    }
    else {
	strcpy( _projEdit, text );
    }

    if ( text ) XtFree(text);

/*
 * get the GAREA input
 */
    text = XmTextFieldGetString(_gareaTextW);

    if ( strlen(text)==(size_t)0 ) {
	strcpy( _gareaEdit, _gareaCopy );
	XmTextSetString( _gareaTextW, _gareaEdit );
    }
    else {
	strcpy( _gareaEdit, text );
    }

    XtVaGetValues(_defaultConFrame, XmNshadowType, &shadow_char, NULL);

    if ( shadow_char == XmSHADOW_IN ) {

	str_def_con = XmStringCreateLocalized( _curDef );
	XtVaSetValues(_defaultConLabel,
			XmNlabelString,  str_def_con,
			NULL);
        XmStringFree ( str_def_con );
    }

    if ( text ) XtFree(text);

    mpcstw_updtMapDvr( );

    if ( flag == 1 ) {
	gclear( &ignore );
	mapw_redrawMap();
    }
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_defConCb ( Widget w, XtPointer clnt, XtPointer call )
/************************************************************************
 * mpcstw_defConCb							*
 *									*
 * This is the callback for DEF proj content button			*
 *									*
 * void mpcstw_defConCb(w, data, call)					*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	clnt	XtPointer	client data				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 * C. Lin/EAI		02/97	add checking if text is NULL		*
 * C. Lin/EAI		07/97	call new mapw module			*
 * M. Li/GSC		03/01	removed mapw_getDEFproj			*
 ***********************************************************************/
{
unsigned char	 shadow_char;
char		 *text;
XmString	 str_def_con;

/*---------------------------------------------------------------------*/

	if ( XmToggleButtonGetState(_defaultBt) == FALSE )
		return;

	XtVaGetValues( _defaultConFrame,
		XmNshadowType, &shadow_char,
		NULL);

	if ( shadow_char == XmSHADOW_OUT ) {

/*
 * show the content of current DEF projection
 */
		text = XmTextFieldGetString(_gareaTextW) ;
		if ( strlen(text) != (size_t)0 ) {
		    XtFree(text);
		    str_def_con = XmStringCreateLocalized( _curDef );

		    XtVaSetValues(w,
				XmNlabelString,  str_def_con,
				NULL);
		    XmStringFree(str_def_con);
		    XtVaSetValues(_defaultConFrame,
				XmNshadowType,	 XmSHADOW_IN,
				NULL);
		}
	}
    else
	mpcstw_resetDefCon();
}

/*=====================================================================*/
/* ARGSUSED */
void mpcstw_ctlBtnCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * mpcstw_ctlBtnCb							*
 *									*
 * This is the callback for the control buttons 			*
 *									*
 * void mpcstw_ctlBtnCb(w, which, call) 				*
 *									*
 * Input parameters:							*
 *	w	Widget		calling widget ID			*
 *	which	long		client data				*
 *	call	XtPointer	never used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		09/96						*
 * C. Lin/EAI		07/97	call new mapw module			*
 * G. Krueger/EAI	11/97	Renamed NxmHelp functions		*
 * S. Jacobs/NCEP	10/99	Added current loop to setting map proj	*
 * E. Safford/GSC	10/99	dataw_getCurLoop -> loop_getCurLoop   	*
 * M. Li/GSC		03/01	removed mapw_setMap			*
 ***********************************************************************/
{
int		ignore;
/*---------------------------------------------------------------------*/

	switch(which) {

	    case 0:				/* Accept */

		mpcstw_textCb( NULL, 0, NULL );
		mpcstw_popdown();

		mpcstw_updtMapDvr( );

/*
 * draws map
 */
		gclear( &ignore );
		mapw_redrawMap();
		break;

	    case 1:			/* Help   */
		NxmHelp_helpBtnCb(w, 4, NULL);
		break;

	    case 2:			/* Cancel */
		mpcstw_popdown();
		break;
	}
}

/*=====================================================================*/

void mpcstw_resetDefCon ( void )
/************************************************************************
 * mpcstw_resetDefCon							*
 *									*
 * This function reset the DEF content label				*
 *									*
 * void mpcstw_resetDefCon()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC		10/96						*
 ***********************************************************************/
{
char	  char_init[] = "DEF Content";
XmString  str;
/*--------------------------------------------------------------------*/

    str = XmStringCreateLocalized(char_init);

    XtVaSetValues( _defaultConLabel,
		 XmNlabelString,  str,
		 NULL );
    XtVaSetValues( _defaultConFrame,
		 XmNshadowType,   XmSHADOW_OUT,
		 NULL );

    XmStringFree(str);
}

/*=====================================================================*/

static void mpcstw_updtMapDvr ( void )
/************************************************************************
 * mpcstw_updtMapDvr							*
 *									*
 * This function uses the current _projEdit and _gareaEdit values to	*
 * update the nmp (map) driver.						*
 *									*
 * mpcstw_updtMapDvr ()							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *									*
 **									*
 * Log: 								*
 * E. Safford/GSC	05/01	initial coding				*
 * T. Piper/SAIC	04/05	updated for nmp_smapattr CSC		*
 ***********************************************************************/
{
int		lp, ier;
nmpstr_t	map, proj, garea[2];
/*--------------------------------------------------------------------*/

    lp = loop_getCurLoop();
    nmp_gmapattr( lp, map, proj, garea, &ier);

    strcpy (proj, _projEdit);
    strcpy (garea[0], _gareaEdit);

    nmp_smapattr (lp, map, proj, garea, FALSE, &ier);	
}
