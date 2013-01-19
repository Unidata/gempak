#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "nmap_data.h"

#define SFC		"SFC"
#define APPLY_COLOR	"yellow"

#define MAX_AREATYPSTR ( 25 )
#define MAX_FCSTHRSTR  (  9 )
#define MAX_TAGSTR     (  9 )
#define MAX_ISSOPTSTR  (  5 )
#define MAX_SEQNUM     (300 )
#define MAX_DSCPSTR    ( 12 )
#define NCHKBOX	       (  5 )
#define NR2S	       ( 10 )
#define NUM_SIGMET_REF ( 10 )
#define MAX_NOSIG_REF  (  5 )
#define ON	       ( True )
#define OFF	       ( False )
#define BLACK	       ( 32 )

/*
 *  structure for user input field in panel 2
 */
typedef struct {		 
	Widget	label;
	Widget	text;
} userInput_t;

/*
 *  structure for the popup dialogs in panel 2
 */
typedef struct {		 
	int	nBtns;
	Widget*	btns;
        Widget  popupDlg;
} popup_t;

/*
 *  structure for widgets in panel 2
 *  the array size MAXTBLNAME is required by setOptMenu(...) 
 */
typedef struct {
	int			nInputField;
 	int			nDesc;
	int			nCheckbox;
	int			nPopup;
	char			haz [ MAXTBLNAME ];
	Widget			form;
	userInput_t		*inputField;
	struct	optMenuStrc	*descStrc;
	Widget			*checkbox;
	Widget			*checkboxLabel;
	Widget			*popupLabel;
	popup_t			*popup;
	Widget			typeText;
} panel2_t;

/*
 *  structure for widgets in the warning dialog for top/bottom.
 */
typedef struct {
	Widget		flightLevel;
	Widget		fzlLabel;
	Widget		fzlLevel;
} levels_t;
 

static VG_DBStruct		_gfaElm;	/* Current GFA element */

static	int			_nAreatyp;
static	char			_areaTyp[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_areaTypStrc;

static	int			_nFcsthr;
static	char			_fcsthr[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_fcsthrStrc;

static	int			(*_nTags)[MAXNOPT];
static	char			(*_tags)[MAXNOPT][MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	(*_tagStrc)[MAXNOPT];

static	int			_nDesks;
static	char			_desks[MAXNOPT][MAXTBLNAME];
static	struct	optMenuStrc	_deskStrc;

static  int                     _nIssopt;
static  char                    _issopt[MAXNOPT][MAXTBLNAME];
static  struct  optMenuStrc     _issoptStrc;

static	Widget	_hazForm;
static	Widget	_editPane;
static	Widget	_gfaForm;
static	Widget	_fromText;
static	Widget	_colorPb;
static	Widget	_txtMovePb;
static	Widget	_cntlForm;
static	Widget	_fromLinePb;
static  Widget	_fcsthrText;
static  Widget	_curTypeDlg = NULL;		/* Current (IFR) type add/remove dialog */
static  Widget	_ifrWarningW = NULL;		/* Incomplete IFR warning window */
static  Widget	_typeTextW   = NULL;		/* Text field in the IFR warning window */
static  Widget  _mtobscWarningW = NULL;         /* Incomplete IFR warning window */
static  Widget  _mtobscTextW = NULL;            /* Text field in the MTOBSC warning window */

static	panel2_t	*_currentPanel2;
static	WidgetList	_ctlButtons;

static	int	_areaTypeIndex;
static	int	_currFcsthr;

static	char	_topStr[10]	= "";
static	char	_bottomStr[10]	= "";
static  char    _fcsthrStr[10]  = "";
static  char    _tmpFcsthrStr[10]  = "";

static	int	_attrColor	= 4;

static Boolean	_waitFlag	= False;
static Boolean	_txtActive	= False;

static Boolean	_addMode	= False;	/* add or edit mode flag */
static Boolean	_textDrawn	= True;		/* attr box drawn or not */


static panel2_t	*_panel2;
static int	_nPanel2 	= 0;

static int	_FLIndex	= 0;		/* array index of FL input in panel2 */
static int	_FZLIndex	= 0;		/* array index of FZL input in panel2 */

static XtCallbackProc 	_editCbFunc;

static Boolean _okOnWarning 	= False;
static Boolean _cancelOnWarning = False;

/*
 *  private functions -- callback
 */
static void pggfaw_addFcstHrColon	( Widget, XtPointer, XtPointer );
static void pggfaw_areatypPbCb		( Widget,      long, XtPointer );
static void pggfaw_changedFLCb		( Widget, XtPointer, XtPointer );
static void pggfaw_cigChkBoxCb		( Widget, XtPointer, XtPointer );
static void pggfaw_ctlBtnCb		( Widget,      long, XtPointer );
static void pggfaw_deskCb		( Widget,      long, XtPointer );
static void pggfaw_fcsthrPbCb		( Widget,      long, XtPointer );
static void pggfaw_svfilePbCb		( Widget, XtPointer, XtPointer );
static void pggfaw_txtMoveCb		( Widget, XtPointer, XtPointer );
static void pggfaw_fillFLCb		( Widget, XtPointer, XtPointer );
static void pggfaw_optPbCb		( Widget, XtPointer, XtPointer );
static void pggfaw_fcsthrTxtCb		( Widget, XtPointer, XtPointer );
static void pggfaw_txtDragEh		( Widget, XtPointer, XEvent*, Boolean* );
static void pggfaw_txtPressEh		( Widget, XtPointer, XEvent*, Boolean* );
static void pggfaw_fillFzlCb		( Widget, XtPointer, XtPointer );
static void pggfaw_tagCb		( Widget,      long, XtPointer );
static void pggfaw_ifrWarningBtnCb	( Widget, XtPointer, XtPointer );
static void pggfaw_verifyFLCb		( Widget, XtPointer, XtPointer );
static void pggfaw_verifyFzlCb		( Widget, XtPointer, XtPointer );
static void pggfaw_vrfyFcstHrCb		( Widget, XtPointer, XtPointer );
static void pggfaw_warningBtnCb		( Widget, XtPointer, XtPointer );
static void pggfaw_mtobscWarningBtnCb	( Widget, XtPointer, XtPointer );
static void pggfaw_popupTypeDlgCb	( Widget, XtPointer, XtPointer );
static void pggfaw_closeTypeDlgCb	( Widget, XtPointer, XtPointer );
static void pggfaw_setTypeTextCb	( Widget, XtPointer, XtPointer );
static void pggfaw_colorZeroCb 		( Widget, XtPointer, XtPointer );


/*
 * private functions -- action
 */
static void pggfaw_fillStrArray ( int max_strarr, int max_string, char origstr[],
				  char strarr[][MAXTBLNAME] );
static void pggfaw_getGuiInfo ( int *iret );
static void pggfaw_setText ( void );
static void pggfaw_setAddMode ( Boolean inAddMode );
static void pggfaw_cleanup ( void );
static void pggfaw_createPanel1 ( void );
static void pggfaw_createPanel2 ( void );
static void pggfaw_getPanel2Attr ( VG_DBStruct *el );
static void pggfaw_setPanel2Attr ( VG_DBStruct *el ); 
static void pggfaw_showWarning ( Widget parent );

static void pggfa_createOptionMenu ( Widget parent, int nbuttons,
				XtPointer pstart_item, char label_str[], 
				XtCallbackProc callback, Widget *form, 
				Widget *label, Widget *menu, Widget pbs[],
                                int posX, int posY,
				char *btnstrs[] );
                                
static void pggfaw_setFromLinePb ( Boolean inFlag );
static void pggfaw_initIssueType ( void ) ;
static void pggfaw_setApplyBtn   ( Boolean enable );
static void pggfaw_initDueTo 	 ( void ) ;

static void pggfaw_setFzl( Boolean value );
static int pggfaw_getNextTag ( void );
static void pggfaw_addTag( int tag );
static void pggfaw_setTagMenu( char tag[] );
static void pggfaw_setDfltTag( char fcsthr[] );
static Boolean pggfaw_tagInUse( char vgfname[], FILE *fptr, char tag[], char fcsthr[] );
static void pggfaw_getTagsForDesk ( char vgfname[], FILE *fptr, char desk[], 
				    char haz[], int **tags, int *ntags );
static void pggfaw_setAllTags ( void );
static int pggfaw_getCategoryType ( void );
static int pggfaw_getCurrentSubtype ( void );
static void pggfaw_resetForSubtype( int *iret );
static void pggfaw_showIfrWarning ( Widget parent );
static void pggfaw_showMtobscWarning ( Widget parent );

/************************************************************************
 * nmap_pggfaw.c							*
 *									*
 * This module defines everything for GFA (Airmet) creation/editing.	*
 *									*
 * CONTENTS:								*
 *	pggfaw_create()		creates the popup window		*
 *	pggfaw_popup()		manages the popup window		*
 *	pggfaw_popdown()	unmanages the popup window		*
 *									*
 *	pggfaw_getAttr()	gets the current attributes		*
 *	pggfaw_isTxtActive	checks if in move text mode		*
 *	pggfaw_isUp()		queries whether the window is up	*
 *	pggfaw_saveNew()	saves a new GFA element			*
 *	pggfaw_setAttr()	sets the attibutes			*
 *	pggfaw_setFrom()	sets the from line			*
 *      pggfaw_reorderGFA()	re-order the points in a CW direction	*
 *									*
 *	pggfaw_areatypPbCb()	callback for area menu buttons		*
 *	pggfaw_ctlBtnCb()	callback for control buttons		*
 *	pggfaw_fcsthrPbCb()	callback for forecast hour menu buttons	*
 *	pggfaw_fcsthrTxtCb()	callback for forecast hour text field	*
 *      pggfaw_svfilePbCb()     callback for saving from line text	*
 *      pggfaw_txtMoveCb()      callback for "Move Text" button		*
 *	pggfaw_optPbCb 		callback for option menu		*
 *	pggfaw_fillFLCb		callback for 'top/botton' text field	*
 *									*
 *      pggfaw_txtDragEh()      event handler for dragging attr. box	*
 *	pggfaw_txtPressEh()	event handler for placing attr. box	*
 *	pggfaw_vrfyFcstHrCb()	verify the forecast hour text input	*
 *	pggfaw_addFcstHrColon()	add a colon to forecast hour text input	*
 *									*
 *	pggfaw_fillStrArray()	fills an array of strings		*
 *	pggfaw_getGuiInfo()	reads GFA infomation table		*
 *	pggfaw_setText		sets attr. box ghosting/event handling	*
 *	pggfaw_setAddMode	sets GFA window mode to Add or Edit	*
 *	pggfaw_setType		sets GFA area type to the new type	*
 *	pggfaw_setHour		sets GFA forecast hour to the new hour	*
 *	pggfaw_cleanup		cleans up if quit before placing box	*
 *	pggfaw_getGuiInfo 	read GUI infomation from a table	*
 *	pggfaw_createPanel1 	create the top panel			*
 *	pggfaw_createPanel2 	create the second panel			*
 *	pggfaw_getPanel2Attr 	get attribute in the hazard panel	*
 *	pggfaw_setPanel2Attr 	set attribute in the hazard panel	*
 ***********************************************************************/

/*=====================================================================*/

void pggfaw_create ( Widget parent )
/************************************************************************
 * pggfaw_create							*
 *									*
 * This function creates a GFA (Airmet) creating/editing window.	*
 *									*
 * void pggfaw_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	add "Save" for the from line text	*
 * J. Wu/SAIC		03/04	add non-convetive sigmet		*
 * J. Wu/SAIC		04/04	add text input for forecast hour	*
 * J. Wu/SAIC		06/04	add GFA_GFA subtype			*
 * E. Safford/SAIC	07/04	make top and bottom fields 3 chars long *
 *				  make frame (_frame) global		*
 * J. Wu/SAIC		08/04	add hazard # for GFA_GFA		*
 * J. Wu/SAIC		09/04	remove airmet & non-convective sigmet	*
 * J. Wu/SAIC		10/04	initialize _gfaElm			*
 * B. Yin/SAIC		10/04	re-write for the new GFA GUI		*
 * B. Yin/SAIC		04/05	remove the from line dialog 		*
 * T. Piper/SAIC	09/05	made ii and nn long			*
 * L. Hinson/AWC        09/05   Updated Pane to be oriented Horizontally*
 *                                instead of vertically                 *
 *                              Updated Control Buttons to be oriented  *
 *                                Vertically instead of horizontally    *
 ***********************************************************************/
{
    int		ier;
    long	ii, nn;
    char	*ctlstrs[] = { "Apply", "Cancel" };
    XmString	xmstr;
    
/*---------------------------------------------------------------------*/
/*
 *  Load GUI information from "gfa_temp.tbl".
 */
    pggfaw_getGuiInfo ( &ier );

    if ( ier != 0 ) return;
    
/*
 *  Create main form dialog window.
 */
    _gfaForm = XmCreateFormDialog ( parent, "pggfaw_popup", NULL, 0 );

    xmstr = XmStringCreateLocalized("GFA Create / Edit");

    XtVaSetValues ( _gfaForm,
		XmNnoResize,			TRUE,
		XmNautoUnmanage,		FALSE,
		XmNdialogTitle,			xmstr,
		XmNx,				50,
		XmNy,				100,
		NULL );

    XmStringFree(xmstr);

/*
 *  Create pane area and forms.
 */
    _editPane = XtVaCreateManagedWidget ( "gfaw_pane",
			xmPanedWindowWidgetClass, 	_gfaForm,
                        XmNmarginHeight,                10,
                        XmNmarginWidth,                 10,
                        XmNspacing,                     20,
			XmNsashWidth,			1,
			XmNsashHeight,	 		1,
                        XmNorientation,       XmHORIZONTAL, 
			NULL );

    pggfaw_createPanel1 ( );
    pggfaw_createPanel2 ( );

/*
 *  Control buttons
 */
    nn = XtNumber ( ctlstrs );
    _cntlForm = XtVaCreateWidget ( "gfa_cntl_form",
		xmFormWidgetClass,		_editPane,
                XmNfractionBase,                nn * 50,
		NULL );

    _ctlButtons = (WidgetList)XtMalloc((size_t)nn*sizeof(Widget));
    for ( ii = 0; ii < nn; ii++ ) {
        _ctlButtons[ii] = XtVaCreateManagedWidget(ctlstrs[ii],
                xmPushButtonWidgetClass,        _cntlForm,
                XmNtopAttachment, XmATTACH_POSITION,
                XmNtopPosition, ((ii * 50) + 10),
                XmNbottomPosition,(((ii + 1) * 50) - 10),
                NULL ); 
                
	XtAddCallback ( _ctlButtons[ ii ], XmNactivateCallback,
		(XtCallbackProc)pggfaw_ctlBtnCb, (XtPointer) ii );
    }
    
/*
 *  Initialize _gfaElm
 */
    _gfaElm.elem.gfa.info.nblocks = 0;

}

/*=====================================================================*/

void pggfaw_popup ( VG_DBStruct *el, XtCallbackProc callback )
/************************************************************************
 * pggfaw_popup								*
 *									*
 * This function manages the GFA popup.					*
 *									*
 * void pggfaw_popup ( el, callback )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	current element			*
 *	callback	XtCallbackProc	edit callback structure		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	change to include more object types	*
 * J. Wu/SAIC		06/04	add GFA_GFA subtype			*
 * J. Wu/SAIC		08/04	add hazard number for GFA_GFA		*
 * J. Wu/SAIC		08/04	link with layer & filter control	*
 * J. Wu/SAIC		09/04	remove airmet & non-convective sigmet	*
 * J. Wu/SAIC		10/04	access attr. using cvg_get/setFld()	*
 * B. Yin/SAIC		10/04	re-write for the new GFA GUI		*
 * E. Safford/SAIC	12/04	initialize _category for new GFA elms   *
 * E. Safford/SAIC	01/05	initialize sigmet ref info              *
 * B. Yin/SAIC		04/05	remove the sigmet ref text widget	*
 * B. Yin/SAIC		05/05	initialize 'from line' text to empty	*
 * H. Zeng/SAIC		09/05	removed call to pggfaw_updateSigRefBtn  *
 * B. Yin/SAIC		11/05	removed GFA subtype			*
 * B. Yin/SAIC		12/05	call the set routine to set fromline btn*
 * B. Yin/SAIC		01/06	removed the part that sets line width	*
 * B. Yin/SAIC		01/06	set the issue type to 'nrml' for new GFA*
 * B. Yin/SAIC		01/06	use pggfaw_setApplyBtn to disable Apply *
 * E. Safford/SAIC	01/06	call pggfaw_initDueTo()			*
 * B. Yin/SAIC		06/06	set the tag menu			*
 * J. Wu/SAIC		06/06	remove matching fcsthr with filter win.	*
 * B. Yin/SAIC		07/06	get hazard color from the setting table.*
 ***********************************************************************/
{
    int		subtype, ier;
    VG_DBStruct	newel;
    XmString	xmstr;
    Boolean	inAddMode = False;
    char	curLayerName[MXFLSZ];
/*---------------------------------------------------------------------*/
 
    pggfaw_popdown ();

    pggfaw_setAllTags();

    if ( el == NULL ) {	/* Add new elements */

	inAddMode = True;
        pggfaw_setAddMode ( inAddMode );
	
	newel.hdr.vg_class = CLASS_MET;
	newel.hdr.vg_type  = GFA_ELM;
	
/*
 *  Initialize the number of blocks to 0.
 */
	newel.elem.gfa.info.nblocks  = 0;
	newel.elem.gfa.info.npts  = 0;

/*
 *  Build a new GFA element and propagate its settings.
 */
	pggfaw_getAttr ( &newel );

	pggfaw_setAttr ( &newel );        
	
/*
 *  Disable 'Show From Line' and 'Move Text' buttons
 */
        XtSetSensitive ( _txtMovePb, False );        
        XtSetSensitive ( _fromLinePb, False );        

/*
 *  Don't show 'Apply' and 'Cancel' buttons for add mode 
 */
        if ( XtIsManaged ( _cntlForm ) ) {
	   XtUnmanageChild ( _cntlForm );
        }
                
/*
 *  Match the current layer name with area type.
 */
        if ( pglayrw_isUp() ) {
            pglayrw_getLayerName ( curLayerName );
            pggfaw_setType ( curLayerName );    
        }
        
/*
 *  Retrieve attribute settings.
 */
        subtype = pggfaw_getCurrentSubtype();
	ces_get ( subtype, &newel, &ier );
        
	_attrColor = newel.hdr.maj_col;	
        XtVaSetValues ( _colorPb,
		XmNbackground,		NxmColrP_getColorPixel ( _attrColor ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( _attrColor ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( _attrColor ),
		NULL );

/*
 *  Set From Line to blank
 */
    	XtVaSetValues ( _fromText, XmNvalue, "", NULL );

	pggfaw_initIssueType();
        pggfaw_initDueTo();

/*
 *  Free block memory.
 */
        cvg_freeElPtr ( &newel );
	
    }
    else {	/* Edit elements */
	if (callback != NULL) _editCbFunc = callback;
    
        pggfaw_setAddMode ( inAddMode );

	pggfaw_setAttr ( el );
        
	pggfaw_setFrom ( el->elem.gfa.info.npts, el->elem.gfa.latlon, 
    		     &(el->elem.gfa.latlon[el->elem.gfa.info.npts]) );
	
/*
 *  Enable 'Show From Line' and 'Move Text' buttons
 */
        XtSetSensitive ( _txtMovePb, True );        
	pggfaw_setFromLinePb ( True );

/*
 *  Show 'Apply' and 'Cancel' buttons for add mode 
 */
        if ( !XtIsManaged ( _cntlForm ) ) {
	   XtManageChild ( _cntlForm );
        }
    }
                         
/*
 *  Disable 'Apply' button
 */
    pggfaw_setApplyBtn( False );
    
/*
 *  Manage GFA form.
 */
    pggfaw_createPanel2 ( );
    XtManageChild ( _gfaForm );
            
/*
 *  Set window title.
 */
    if ( inAddMode ) { 
        xmstr = XmStringCreateLocalized( "GFA Create" );	
    }
    else {
        xmstr = XmStringCreateLocalized( "GFA Edit" );	
    }
    
    XtVaSetValues ( _gfaForm, XmNdialogTitle, xmstr, NULL );
    XmStringFree(xmstr);
}

/*=====================================================================*/

void pggfaw_popdown ( void )
/************************************************************************
 * pggfaw_popdown							*
 *									*
 * This function unmanages the GFA popup.				*
 *									*
 * void pggfaw_popdown ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	pop down _svfileForm			*
 * E. Safford/SAIC	08/04	fix move text problem (5b-48, P2)	*
 * J. Wu/SAIC		09/04	clean up for abnormal switching		*
 * B. Yin/SAIC		04/05	remove from line dialog			*
 * B. Yin/SAIC		03/06	close IFR add/remove type dialog	*
  ***********************************************************************/
{
/*---------------------------------------------------------------------*/

   if ( XtIsManaged ( _gfaForm ) ) {
	NxmClrW_popdown();
	XtUnmanageChild ( _gfaForm );

/*
 *  Close current type edit dialog (for IFR).
 */
	if ( ( _curTypeDlg != NULL ) && XtIsManaged ( _curTypeDlg ) ) {
  	   XtUnmanageChild ( _curTypeDlg );
	}
    }

/*
 *  Clean up if the user switches loop/layer/filter/action before
 *  placing the attribute box.
 */
    pggfaw_cleanup ();
}

/*=====================================================================*/

Boolean pggfaw_isUp ( void )
/************************************************************************
 * pggfaw_isUp								*
 *									*
 * Specify whether the GFA dialog is managed or not.			*
 *									*
 * Boolean pggfaw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pggfaw_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged (_gfaForm) );
}

/*=====================================================================*/

static void pggfaw_fillStrArray ( int max_strarr, int max_string,
               char origstr[], char strarr[][MAXTBLNAME] )
/************************************************************************
 * pggfaw_fillStrArray							*
 *									*
 * This function copies a string of multiple names delimited by a ';'	*
 * to an array of strings.						*
 *									*
 * static void pggfaw_fillStrArray (max_strarr, max_string, origstr,	*
 *				    strarr )				*
 *									*
 * Input parameters:							*
 *	max_strarr	int	maximum array size			*
 *	max_string	int	maximum string length			*
 *	origstr[]	char	the original string			*
 *									*
 * Output parameters:							*
 *	strarr[][MAXTBLNAME]	char	the array of strings		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * B. Yin/SAIC		10/04	removed a parameter(current array index)*
 ***********************************************************************/
{
    int		last, ii;
    char	*ptr;
/*---------------------------------------------------------------------*/

    ptr = strtok ( origstr, ";" );
    last = max_string - 1;

    ii = 0;
    while ( (ptr != (char *)NULL) && (ii < max_strarr) ) {

	strncpy ( strarr[ii], ptr, (size_t)last );
	strarr[ii][strlen(ptr)] = '\0';

	ptr = strtok ( NULL, ";" );	
	ii++;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_areatypPbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pggfaw_areatypPbCb							*
 *									*
 * Callback function for area type menu buttons.			*
 *									*
 * static void pggfaw_areatypPbCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	store area type for current subtype	*
 * J. Wu/SAIC		06/04	adjust for GFA_GFA subtype		*
 * E. Safford/SAIC	07/04	add un/manage of _frame to fix 778:P2   *
 * J. Wu/SAIC		08/04	use _descriptionForm & rm _frame	*
 * J. Wu/SAIC		08/04	link with layer control window		*
 * J. Wu/SAIC		09/04	use only GFA description menu		*
 * B. Yin/SAIC          10/04   re-write for the new GFA GUI            *
 * E. Safford/SAIC	01/05	enable/disable sigmet ref controls	*
 * H. Zeng/SAIC		09/05	removed call to pggfaw_setSigRefState()	*
 * B. Yin/SAIC		11/05	changed _areaTypeIndex from array to int*
 * B. Yin/SAIC		01/06	use pggfaw_setApplyBtn to enable Apply 	*
 * E. Safford/SAIC	01/06	call pggfaw_initDueTo()			*
 * B. Yin/SAIC		06/06	set the tag menu for the hazard		*
 * B. Yin/SAIC		07/06	reset the subtype			*
 ***********************************************************************/
{
    int mm, nn, ier;
/*---------------------------------------------------------------------*/
    _areaTypStrc.current = (int)which;        
    _areaTypeIndex = (int)which;

    XtVaSetValues ( _areaTypStrc.menu, 
		XmNmenuHistory,		_areaTypStrc.pb[which], 
		NULL );

/*
 *  Set the tag menu for the hazard.
 */
    for ( mm = 0; mm < _nDesks; mm++ ) {

       for ( nn = 0; nn < _nAreatyp; nn++ ) {

	   if ( nn == which && mm == _deskStrc.current ) {

	      XtManageChild (  _tagStrc[ mm ][ nn ].form );

	   }
	   else {

	      XtUnmanageChild ( _tagStrc[ mm ][ nn ].form );

	   }
       }
    }

    pggfaw_setDfltTag( _fcsthr[ _currFcsthr ] );
    
/*
 *  Create the panel for the hazard
 */
    pggfaw_createPanel2();

/*
 *  Enable 'Apply' button
 */
    pggfaw_setApplyBtn( True );

/*
 *   Match the layer name to the new type if the layering is active.
 */
    if ( pglayrw_isUp () ) {        
	pglayrw_matchLayer ( _areaTyp[which] );
    }

/*
 *  Set the DUE_TO menu to the first item if we're adding
 *  new hazards.
 */
    if( strcasecmp( _areaTyp[ which ], "TURB" ) == 0 || 
        strcasecmp( _areaTyp[ which ], "TURB-HI" ) == 0 ||
	strcasecmp( _areaTyp[ which ], "TURB-LO" ) == 0 ) {

        if( pggfaw_isAddMode() ) {
            pggfaw_initDueTo();
        }
    }
    pggfaw_resetForSubtype( &ier );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_fcsthrPbCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pggfaw_fcsthrPbCb							*
 *									*
 * Callback function for forecast hour menu buttons.			*
 *									*
 * static void pggfaw_fcsthrPbCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	cbs	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	store forecast hour for current subtype	*
 * J. Wu/SAIC		04/04	add _fcsthrText for text input		*
 * J. Wu/SAIC		08/04	link with filter control window		*
 * B. Yin/SAIC          10/04   re-write for the new GFA GUI            *
 * B. Yin/SAIC          11/04   add back the 'Other' option             *
 * B. Yin/SAIC		11/05	chg _currFcsthr from array to int	*
 * B. Yin/SAIC		01/06	use pggfaw_setApplyBtn to enable Apply 	*
 * B. Yin/SAIC		06/06	set the default tag for the hour	*
 * B. Yin/SAIC		07/06	reset the subtype			*
 * M. Li/SAIC		03/07	No action when 'Other' is empty		*
 ***********************************************************************/
{
    int		ier;
    char	*tmpStr;
    XmString	xmStr;
/*---------------------------------------------------------------------*/

    _fcsthrStrc.current = (int)which;
    _currFcsthr = (int)which;

    XtVaSetValues ( _fcsthrStrc.menu, 
		XmNmenuHistory,	_fcsthrStrc.pb[which], 
		NULL );

/*
 *  Manage the text field if user select the last option - "Other".
 */
    XtVaGetValues ( _fcsthrStrc.pb[which], XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

    if ( strcasecmp ( tmpStr, "other" ) == 0  ) {
        XtManageChild ( _fcsthrText );
    }
    else {
        XtUnmanageChild ( _fcsthrText );
    }

    pggfaw_setDfltTag ( tmpStr );

    XmStringFree ( xmStr );
    XtFree ( tmpStr );

/*
 *   Match the new hour to filter time if the filtering is active.
 */
    if ( pgfilterw_isUp () && strcasecmp (_fcsthr[which], "other") ) {
	pgfilterw_turnOnTime ( _fcsthr[which] );
        
    }

/*
 *  Enable 'Apply' button
 */
    pggfaw_setApplyBtn( True );
    pggfaw_resetForSubtype( &ier );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_txtMoveCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_txtMoveCb							*
 *									*
 * Callback function for "Move Text" button.				*
 *									*
 * static void pggfaw_txtMoveCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client-data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 ***********************************************************************/
{
    int		el_location, num, ier;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    _txtActive = True;
    XtSetSensitive ( _txtMovePb, False );

/*
 *  Put the handbar on the attribute text. The current active element
 *  is still the same GFA element, though.
 */ 	    
    el_location = pgactv_getElmLoc ();                        
    cvg_rdrec ( cvg_getworkfile(), el_location, &el, &ier );

    crg_getinx ( el_location, &num, &ier);
    pghdlb_deselectEl ( num, TRUE );
    
    pghdlb_select ( &el, el_location );
        
/*
 *  Set up event handling to move the text box around.
 */ 	        
    pggfaw_setText ();
    
/*
 *  Free block memory.
 */
    cvg_freeElPtr ( &el );
}

/*=====================================================================*/

Boolean pggfaw_isTxtActive ( void )
/************************************************************************
 * pggfaw_isTxtActive							*
 *									*
 * Check whether in the "Move Text" mode.				*
 *									*
 * Boolean pggfaw_isTxtActive ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pggfaw_isTxtActive	Boolean		Is/is not active	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 ***********************************************************************/
{
    return ( _txtActive );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_ctlBtnCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pggfaw_ctlBtnCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * static void pggfaw_ctlBtnCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		long		which button			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	save from line text to a file		*
 * J. Wu/SAIC		03/04	make unique filename from subtypes	*
 * J. Wu/SAIC		06/04	add subtype GFA_GFA			*
 * E. Safford/SAIC	08/04	mk AIRMET file name format TANGO_#, etc	*
 * J. Wu/SAIC		08/04	add hazard # to GFA file name format 	*
 * J. Wu/SAIC		09/04	remove airmet & non-convetive sigmet 	*
 * J. Wu/SAIC		10/04	remove call to pggfaw_popdown() 	*
 * B. Yin/SAIC          10/04   re-write for the new GFA GUI            *
 * B. Yin/SAIC          03/05   snap GFA if its hour is a time span     *
 * B. Yin/SAIC          06/05   check if the top/bottom field is empty  *
 * B. Yin/SAIC		06/05   Modified pggfaw_okToDraw		*
 * E. Safford/SAIC	06/05  	fix call to _editCbFunc			*
 * T. Piper/SAIC	09/05	made which long				*
 * B. Yin/SAIC		01/06	use pggfaw_setApplyBtn to disable Apply *
 * B. Yin/SAIC		06/06	set tag menu				*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * B. Yin/SAIC		10/06	set tag menu to current GFA's tag	*
 * E. Safford/SAIC	06/07   check error code from pgvgf_saveNewElm	*
 ***********************************************************************/
{
    int 	ier, np, location;
    float	llx, lly, urx, ury;
    char	tagStr[ 32 ];
/*---------------------------------------------------------------------*/


    switch ( which ) {

      case 0:		/* Apply */

        if ( !pggfaw_okToDraw ( True ) ) break;

        pggfaw_setApplyBtn( False );

	if ( _editCbFunc ) {
	   _editCbFunc ( NULL, (XtPointer)which, NULL );
	}
	else {

    	    np = _gfaElm.elem.gfa.info.npts;

    	    location = pgactv_getElmLoc ();
            pgutls_prepNew ( location, &_gfaElm, &llx, &lly, &urx, &ury, &ier );    

    	    pggfaw_getAttr ( &_gfaElm );
    	    pggfaw_checkHours ( &_gfaElm );

	    np = _gfaElm.elem.gfa.info.npts;

	    pgundo_newStep ();
	
	    pgvgf_saveNewElm ( NULL, sys_M, &_gfaElm, np, &_gfaElm.elem.gfa.latlon[ 0 ],
	    			&_gfaElm.elem.gfa.latlon[ np ], TRUE, &location, &ier );    
            if( ier < 0 ) {
            pgundo_storeThisLoc ( location, UNDO_ADD, &ier );
            pgundo_endStep ();
            pgvgf_dsp ( location, &ier );    
    	    pgutls_redraw ( location, &_gfaElm, &ier );
            } 
	}

/*
 *  Set the tag menu to current GFA's tag.
 */
	cvg_freeElPtr( &_gfaElm );
        location = pgactv_getElmLoc ();                        
        cvg_rdrec ( cvg_getworkfile(), location, &_gfaElm, &ier );

	cvg_getFld ( &_gfaElm, TAG_GFA_TAG, tagStr, &ier );
	pggfaw_addTag( atoi(tagStr) );

	pggfaw_setAllTags();

	break;

      case 1:		/* Cancel */
    	
	if ( _editCbFunc ) {

	   _editCbFunc ( NULL, (XtPointer)which, NULL );

	}
	else {

           pgevt_unsetOper ( FALSE );

           mcanvw_setPressFunc ( (XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT );

           mbotw_mouseSet ( LMHINT_SELECT, MMHINT_TOSELECTOPER );

	}
	break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_svfilePbCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_svfilePbCb							*
 *									*
 * Callback function for the control buttons in the window to save	*
 * the from line text.							*
 *									*
 * static void pggfaw_svfilePbCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*	
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		03/04	initial coding 				*
 * B. Yin/SAIC          10/04   re-write for the new GFA GUI            *
 * B. Yin/SAIC          04/05   remove cancel case		        *
 * E. Safford/SAIC	04/05	add Tag to the file name format		*
 * E. Safford/SAIC	07/05	rm sequence from the file name format	*
 * L. Hinson/AWC        09/20   Set SAVE From Line Button to insensitive*
 *                                after file saved                      *
 * B. Yin/SAIC		11/05	chg _areaTyp from 3 dimension to 2 dim	*
 * B. Yin/SAIC		06/06	change tag and desk menu		*
 ***********************************************************************/
{
    int		ier, pos;
    char        fname[ 128 ], *from_text, tagStr[ 32 ];
    FILE        *fp;
/*---------------------------------------------------------------------*/
/*
 *  Build the file name string
 */
    cst_rmst ( _tags[ _deskStrc.current ][ _areaTypStrc.current ][ _tagStrc[ _deskStrc.current ][ _areaTypStrc.current ].current],
    	       "*", &pos, tagStr, &ier );

    sprintf ( fname, "GFA_%s_%s%s.from", _areaTyp[ _areaTypeIndex ], 
	      tagStr, _desks[ _deskStrc.current ] );

    fp = fopen ( fname, "w" );

    from_text = XmTextGetString ( _fromText );
    fputs ( from_text , fp );

    XtFree ( from_text );

    fclose ( fp );
    XtSetSensitive( _fromLinePb, False);
}

/*=====================================================================*/

void pggfaw_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pggfaw_getAttr							*
 *									*
 * This routine returns the current values of the GFA element.		*
 *									*
 * void pggfaw_getAttr ( el )						*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	current element			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	adapt to more subtypes			*
 * J. Wu/SAIC		04/04	add text input for forecast hour	*
 * J. Wu/SAIC		06/04	add GFA_GFA subtype			*
 * J. Wu/SAIC		08/04	add hazard number for GFA_GFA		*
 * E. Safford/SAIC	08/04	change top & bottom to char arrays	*
 * J. Wu/SAIC		09/04	remove airmet & non-convetive sigmet 	*
 * J. Wu/SAIC		10/04	access attr. using cvg_get/setFld()	*
 * B. Yin/SAIC		11/04	re-write for the new GFA GUI		*
 * B. Yin/SAIC		11/04	use pre-defined gfa tags to set attr.	*
 * B. Yin/SAIC		11/04	add back forecast hour text input box.	*
 * B. Yin/SAIC		11/04	set the initial value of tag menu	*
 * E. Safford/SAIC	12/04	set category				*
 * E. Safford/SAIC	01/05	get sigmet reference string		*
 * B. Yin/SAIC		04/05	remove the sigmet text widget		*
 * E. Safford/SAIC	07/05	rm sequence number          		*
 * H. Zeng/SAIC		09/05	removed sigmet attr			*
 * B. Yin/SAIC		11/05	removed _subtype			*
 * B. Yin/SAIC		12/05	removed the part that sets line width.	*
 *				line width is set in pggfaw_checkHours()*
 * B. Yin/SAIC          02/06   Call pggfaw_isClosed to determine if    *
 *                              the GFA is closed or not.               *
 * B. Yin/SAIC		06/06	change tag menu				*
 * B. Yin/SAIC		01/07	set the freezing level ranges.		*
 * E. Safford/SAIc	04/07	fix memory leak				*
 * L. Hinson/AWC        06/08   Set the tag TAG_GFA_CYCLE               *
 ***********************************************************************/
{
    int		ii, ier, iret, curDesk, curHaz, curTag, pos;
    char        value[32], *tmpStr, tag[32], ranges[ STD_STRLEN ];

    XmString	xmStr;
    Widget	wPulldown, wPushButton;
    char day[8], cycle[8];
/*---------------------------------------------------------------------*/
/*
 *  Initialize the output element.
 */
    cvg_freeElPtr( el );

    el->hdr.vg_class	= CLASS_MET;
    el->hdr.vg_type	= GFA_ELM;
    el->hdr.smooth	= 0;
    el->hdr.closed      = pggfaw_isClosed() ? 1 : 0;
    el->hdr.filled	= 0;	/* 2 = solid, 3 = dashed lines, 4 = lines */
    el->hdr.maj_col	= _attrColor;
    el->hdr.min_col	= _attrColor;	

    el->elem.gfa.info.nblocks = 0;
    el->elem.gfa.info.npts = _gfaElm.elem.gfa.info.npts;
        
    cvg_setFld ( el, TAG_GFA_AREATYPE, _areaTyp[_areaTypeIndex],
    		 &ier );
    
/*
 *  Get forecast hour
 */
    XtVaGetValues ( _fcsthrStrc.menu, 
   		    XmNsubMenuId, &wPulldown, NULL );
    XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

    XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

    if ( strcasecmp ( tmpStr, "other" ) == 0 ) {
       cvg_setFld ( el, TAG_GFA_FCSTHR, _fcsthrStr, &ier );
    }
    else {
    cvg_setFld ( el, TAG_GFA_FCSTHR, 
	                 _fcsthr[_currFcsthr], &ier );
    }
    pgcycle_getCycle ( day, cycle, &iret);
    /* Set the Cycle as a tag in VGF File*/
    cvg_setFld ( el, TAG_GFA_CYCLE,
                         cycle, &ier );
    XtFree ( tmpStr );
    XmStringFree( xmStr );
			 
/*
 *  Get tag
 */   
    curDesk = _deskStrc.current;
    curHaz  = _areaTypStrc.current;
    curTag  = _tagStrc[ curDesk ][ curHaz ].current;

    if ( strcasecmp ( _tags[ curDesk ][ curHaz ][ curTag ],
    		      "new" ) == 0 ) {

       sprintf( value, "%d%s", pggfaw_getNextTag(), _desks[ curDesk ] );
       cvg_setFld ( el, TAG_GFA_TAG, value, &ier );

        }
    else {

/* read menu string */
	if ( strchr( _tags[ curDesk ][ curHaz ][ curTag ], '*' ) ) {
		
           cst_rmst( _tags[ curDesk ][ curHaz ][ curTag ],
	             "*", &pos, tag, &ier );

        }
	else {

	   strcpy( tag, _tags[ curDesk ][ curHaz ][ curTag ] );

    }
	
	sprintf( value, "%s%s", tag, _desks[ curDesk ] );
	cvg_setFld ( el, TAG_GFA_TAG, value, &ier ); 

    }

/* 
 * Get issue status 
 */
    XtVaGetValues ( _issoptStrc.menu, 
   		    XmNsubMenuId, &wPulldown, NULL );
    XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

    XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

    if ( strlen ( tmpStr ) > (size_t)0 ) {
        for ( ii = 0; ii < _nIssopt; ii++ ) {
            if ( strcmp ( tmpStr, _issopt[ii] ) == 0 ) {
                cvg_setFld ( el, TAG_GFA_STATUS, tmpStr, &ier );
                break;
            }
        }
    }

    if ( ii >= _nIssopt ) {
       cvg_setFld ( el, TAG_GFA_STATUS, _issopt[0], &ier );
    }
	
    XtFree ( tmpStr );
    XmStringFree( xmStr );    

/*
 *  Get text position. 
 */
    cvg_getFld ( &_gfaElm, TAG_GFA_LAT, value, &ier );
    if ( ier == 0 ) {
        cvg_setFld ( el, TAG_GFA_LAT, value, &ier );    
    }

    cvg_getFld ( &_gfaElm, TAG_GFA_LON, value, &ier );    
    if ( ier == 0 ) {
        cvg_setFld ( el, TAG_GFA_LON, value, &ier );
    }

/*
 *  Get freezing level ranges
 */
    if ( strcasecmp( _areaTyp[_areaTypeIndex], "fzlvl" ) == 0 ) {
	
       cvg_getFld ( &_gfaElm, TAG_GFA_FZLRANGE, ranges, &ier );

/*
 * In case there is no range infor in _gfaElm, for instance,
 * creating a new FZLVL element, get ranges from the work file.
*/
       if ( ier != 0 || strlen( ranges ) == 0 ) {

          pggfaw_getFzlRangesFromFile( cvg_getworkfile(), ranges );

       }

       cvg_setFld ( el, TAG_GFA_FZLRANGE, ranges, &ier );

    }

/*
 *  Get the hazard panel information
 */
    pggfaw_getPanel2Attr ( el );
}
    
/*=====================================================================*/

void pggfaw_setAttr ( VG_DBStruct *el )
/************************************************************************
 * pggfaw_setAttr							*
 *									*
 * This function sets the attributes as in the given el.		*
 *									*
 * void pggfaw_setAttr ( el )						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	the current elememt		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	adapt to more subtypes			*
 * J. Wu/SAIC		03/04	save off menu selections for retrieval	*
 * J. Wu/SAIC		04/04	add text input for forecast hour	*
 * J. Wu/SAIC		06/04	add subtype GFA_GFA			*
 * J. Wu/SAIC		07/04	update description for subtype GFA_GFA	*
 * J. Wu/SAIC		08/04	format top/bottom level to 3 digits	*
 * J. Wu/SAIC		08/04	update hazard number for GFA_GFA	*
 * E. Safford/SAIC	08/04	change top & bottom to char arrays	*
 * J. Wu/SAIC		09/04	remove airmet & non-convetive sigmet 	*
 * J. Wu/SAIC		10/04	access attr. using cvg_get/setFld()	*
 * B. Yin/SAIC		11/04	re-write for the new GFA GUI		*
 * B. Yin/SAIC		11/04	fixed the text location bug		*
 *				disable opt menu when uncheck 'refer to'*
 *				added back forecast hour text input box *
 * E. Safford/SAIC	12/04	save off _category			*
 * E. Safford/SAIC	01/05	set sigmet reference string		*
 * B. Yin/SAIC		03/05	set number of points			*
 * B. Yin/SAIC		04/05	remove the sigmet ref text widget	*
 * E. Safford/SAIC	07/05	rm sequence (update) number		*
 * H. Zeng/SAIC		09/05	removed sigmet attr			*
 * B. Yin/SAIC		10/05	handle cases when tag is not in gfa.tbl *
 * L. Hinson/AWC        10/05   Added Issue Options menu setup code     *
 *                              Drop Check Box Issue Status Code        *
 *                              Drop call to pggfaw_updateSigRefBtn     *
 * B. Yin/SAIC		11/05	removed _subtype			*
 * B. Yin/SAIC		06/06	change tag menu				*
 * J. Wu/SAIC		06/06	remove verification when editing a GFA	*
 *				with its fcst hr string length > 5	*
 * B. Yin/SAIC		09/06	call pggfaw_setDfltTag to set tag	*
 * B. Yin/SAIC		01/07	set the freezing level ranges		*
 ***********************************************************************/
{
    int			ier, ii, areatyp, issoptIndex;
    char		temp_str[10];
    char                value[32], ranges[ STD_STRLEN ];

    Boolean             fcsthrInMenu = False;
/*---------------------------------------------------------------------*/
/*
 *  Set the area type menu options 
 */

    cvg_getFld ( el, TAG_GFA_AREATYPE, value, &ier );
    areatyp = 0;
    for ( ii = 0; ii < MAXNOPT; ii++ ) {
	if ( strcmp ( value, _areaTyp[ ii ] ) == 0 ) {
           areatyp = ii;
	   break;
	}
    }
    
    pgutls_setOptMenu ( _areaTyp[areatyp],
    			_areaTyp, _nAreatyp, &_areaTypStrc );    
    _areaTypeIndex = _areaTypStrc.current ;
        
/*
 *  Set Status
 */
    cvg_getFld ( el, TAG_GFA_STATUS, value, &ier );

    for ( ii = 0; ii < MAXNOPT; ii++ ) {
	if ( strcmp ( value, _issopt[ ii ] ) == 0 ) {
           issoptIndex = ii;
	   break;
	}
    }

    pgutls_setOptMenu ( _issopt[ issoptIndex ], _issopt, 
    			_nIssopt, &_issoptStrc ); 
/*
 *  Set forecast hour menu options 
 */
    cvg_getFld ( el, TAG_GFA_FCSTHR, value, &ier );
    pgutls_setOptMenu ( value, _fcsthr, _nFcsthr, &_fcsthrStrc );
    _currFcsthr = _fcsthrStrc.current;
    
    if ( strlen (value) > (size_t)0 ) {
        for ( ii = 0; ii < _nFcsthr; ii++ ) {
            if ( strcmp ( value, _fcsthr[ii] ) == 0 ) {
                fcsthrInMenu = True;
                break;
            }
        }
    }

    if ( !fcsthrInMenu ) {

/*
 *  We may end up with hour string as "1:15-3", which could violate
 *  the verification rules for the forecast hour.  If so, remove 
 *  the verification callbacks.
 */	 
	 if ( strlen( value ) > 5  )  {
	     XtRemoveAllCallbacks ( _fcsthrText, XmNmodifyVerifyCallback );
             XtRemoveAllCallbacks ( _fcsthrText, XmNvalueChangedCallback );
	 }
	 else {
             XtAddCallback ( _fcsthrText, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)pggfaw_vrfyFcstHrCb, NULL );
             XtAddCallback ( _fcsthrText, XmNvalueChangedCallback, 
      		    (XtCallbackProc)pggfaw_addFcstHrColon, NULL );	 
	 }
	 
       pgutls_setOptMenu ( "Other",
    			_fcsthr, _nFcsthr, &_fcsthrStrc );

       _currFcsthr = _fcsthrStrc.current;

       strcpy ( _fcsthrStr, value );
       XmTextSetString ( _fcsthrText,  _fcsthrStr );

       XtManageChild ( _fcsthrText );

    }
    else {
       XtUnmanageChild ( _fcsthrText );
    }

/*
 *  Set tag
 */
    if ( _addMode ) {

/* 
 *   Set the default tag and the desk.
 */
       pggfaw_setDfltTag( _fcsthr[ _currFcsthr ] );
       pgutls_setOptMenu ( _desks[ _deskStrc.current ], _desks, _nDesks, &_deskStrc );

    }
    else {

/* 
 *  set menu from the loading element. 
 */
	cvg_getFld ( el, TAG_GFA_TAG, value, &ier );
	pggfaw_setTagMenu( value );

    }

/*
 *  Set top/bottom text and color
 */
    _topStr[0] = '\0';
    cvg_getFld ( el, TAG_GFA_TOP, value, &ier );    
    if ( cvg_getFlghtLvl( value ) > 0 )  {
        sprintf ( temp_str, "%s", value );
        cst_padString ( temp_str, '0', 0, 3, _topStr );
    }

    _bottomStr[0] = '\0';
    cvg_getFld ( el, TAG_GFA_BOTTOM, value, &ier );    
    if( strcmp( value, SFC ) == 0 ) {
        sprintf ( temp_str, "%s", value );
        strcpy( _bottomStr, temp_str );
    }
    else if ( cvg_getFlghtLvl( value ) > 0 ) {
        sprintf ( temp_str, "%s", value );
        cst_padString ( temp_str, '0', 0, 3, _bottomStr );
    }

    _attrColor = el->hdr.maj_col;
    XtVaSetValues ( _colorPb,
		XmNbackground,		NxmColrP_getColorPixel ( _attrColor ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( _attrColor ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( _attrColor ),
		NULL );
  
/*
 *  Set text position. 
 */
    _gfaElm.hdr.vg_type = GFA_ELM;
    cvg_getFld ( el, TAG_GFA_LAT, value, &ier );
    if ( ier == 0 ) {
        cvg_setFld ( &_gfaElm, TAG_GFA_LAT, value, &ier );
    }
    
    cvg_getFld ( el, TAG_GFA_LON, value, &ier );
    if ( ier == 0 ) {
        cvg_setFld ( &_gfaElm, TAG_GFA_LON, value, &ier );
    }
    
/*
 *  Set freezing level ranges
 */
    cvg_getFld ( el, TAG_GFA_FZLRANGE, ranges, &ier );

    if ( ier == 0 ) {

        cvg_setFld ( &_gfaElm, TAG_GFA_FZLRANGE, ranges, &ier );

    }

/*
 *  Set the hazard panel information
 */
    pggfaw_setPanel2Attr ( el );

    _gfaElm.elem.gfa.info.npts = el->elem.gfa.info.npts;

}

/*=====================================================================*/

void pggfaw_setFrom ( int np, float *lat, float *lon )
/************************************************************************
 * pggfaw_setFrom							*
 *									*
 * This function sets the from line based on the new coordinates.	*
 *									*
 * void pggfaw_setFrom ( np, lat, lon )					*
 *									*
 * Input parameters:							*
 *	np		int	number of points			*
 *	*lat		float	points of latitude			*
 *	*lon		float	points of longitude			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * B. Yin/SAIC		10/05	change clo_from separetor type to 4(TO)	*
 * B. Yin/SAIC		12/05	set fromline to blank for open gfa	*
 * D.W.Plummer/NCEP	11/06	Replace SIGINTL_ELM with GFA_ELM	*
 ***********************************************************************/
{
    int			ier;
    char		fmline[15*MAXPTS];
/*---------------------------------------------------------------------*/
/*
 *  Save the latitude and longitude values
 */
    strcpy ( fmline, " " );
    if ( pggfaw_isClosed () ) {
       if ( np > 0 ) {
	  clo_from ( GFA_ELM, SIGTYP_AREA, np, 4, lat, lon, 100, 
 			fmline, &ier);
       }
    }

    XtVaSetValues ( _fromText, XmNvalue, fmline, NULL );
}

/*=====================================================================*/

void pggfaw_reorderGFA ( int areaType, int npts, 
			 float *lat, float *lon, int *iret )
/************************************************************************
 * pggfaw_reorderGFA							*
 *									*
 * This function reorders the points of a GFA element in a CW direction *
 * unless the subtype is GFA_HAZARD_FZLVL_SFC or GFA_HAZARD_FZLVL.	* 
 * For all FZLVLs there is no change to point order.              	*
 *									*
 * If an error is encountered there will be no effort made to reorder   *
 * the points.								*
 *									*
 * void pggfaw_reorderGFA ( areaType, inPts, *inLat, *inLon, *outLats, 	*
 *			 *outLons, *iret )				*
 *									*
 * Input parameters:							*
 *	areatype	int	GFA element's area type (hazard #)	*
 *	npts		int	number of input points			*
 *									*
 * Input/Output parameters:						*
 *	*lat		float	input points lat coordinates		*
 *	*lon		float	input points lon coordinates		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code				*
 *				   0 = normal				*
 *				  -1 = missing/null lat    		*
 *				  -2 = missing/null lon			*
 *				  -3 = error on clo_reorder		*
 *				   1 = FZLVL areaType			*
 **									*
 * Log:									*
 * E. Safford/SAIC	01/07	initial coding				*
 ***********************************************************************/
{
    int		ier, *indx = NULL, ii;
    float	*tmpLat, *tmpLon;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if( !lat ) {
	*iret = -1;
        return;
    }

    if( !lon ) {
	*iret = -2;
        return;
    }


    if( areaType == GFA_HAZARD_FZLVL || 
    	areaType == GFA_HAZARD_FZLVL_SFC ) {
        *iret = 1;
	return;
    }


    G_MALLOC( tmpLat, float, npts+1, "PGGFAW_REORDERGFA tmpLat" );
    G_MALLOC( tmpLon, float, npts+1, "PGGFAW_REORDERGFA tmpLon" );

    for( ii=0; ii<npts; ii++ ) {
	tmpLat[ii] = lat[ii];
	tmpLon[ii] = lon[ii];
    }

    G_MALLOC( indx, int, npts+1, "PGGFAW_REORDERGFA indx" ); 

    clo_reorder( npts, lat, lon, indx, &ier );
   
    if( ier >= 0 ) {

        for( ii=0; ii<npts; ii++ ) {
            lat[ii] = tmpLat[ indx[ii] ];
            lon[ii] = tmpLon[ indx[ii] ];
        }
    }
    else {
	*iret = -3;
    }

    G_FREE( tmpLat, float );
    G_FREE( tmpLon, float );

    G_FREE( indx, int );
}

/*=====================================================================*/

void pggfaw_saveNew ( int np, float *lats, float *lons )
/************************************************************************
 * pggfaw_saveNew							*
 *									*
 * This function saves a new GFA element.				*
 *									*
 * void pggfaw_saveNew ( np, lats, lons )				*
 *									*
 * Input parameters:							*
 *	np	int		number of points			*
 *	*lats	float		latitudes				*
 *	*lons	float		longitudes				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		10/04	access attr. using cvg_get/setFld()	*
 * B. Yin/SAIC		03/05	snap GFAs if its hour is a time span	*
 * B. Yin/SAIC		12/05	draw open GFAs with 2 points		*
 * B. Yin/SAIC		06/06	add the new tag into the tag menu	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * S. Danz/AWC          10/06   Establish the arrow endpoint*
 * E. Safford/SAIC	01/07	add call to pggfaw_reorderGFA()		*
 * E. Safford/SAIC	01/07	reset np after call to pggfaw_checkHours*
 * E. Safford/SAIC	04/07	fix memory leak                         *
 ***********************************************************************/
{
    int		ii, ier, loc, areaType, one = 1;
    char	tag[ 32 ], haz[ 32 ];
    char	value[32];
    float       x_cntr, y_cntr, area, c_lat, c_lon;
    float	xp[MAXPTS], yp[MAXPTS];
/*---------------------------------------------------------------------*/
/*
 *  Do not draw any GFA element with less than 3 points.
 */
    if ( ( np < 3 && pggfaw_isClosed() ) ||
         ( np < 2 && !pggfaw_isClosed() ) ) {
        pggst_clearGhost ( TRUE );
        geplot ( &ier );
	pgnew_setArmDynamic ();
        return;
    }  

/*
 *  Build the current GFA element with proper header, group,
 *  attribute, and location.
 */
    pgutls_initHdr ( &_gfaElm.hdr );
    
    pgnew_getGrpInfo ( &_gfaElm.hdr.grptyp, &_gfaElm.hdr.grpnum );       
    
    pggfaw_getAttr ( &_gfaElm );
        
    _gfaElm.elem.gfa.info.npts = np;

/*
 *   Reorder the lat/lon points into a clockwise orientation.
 */
    strcpy( haz, _areaTyp[ _areaTypeIndex ] );
    areaType = pggfaw_getHazardType( haz );

    pggfaw_reorderGFA ( areaType, np, lats, lons, &ier );
    if( ier < 0 ) {
	return;
    }

    for (ii = 0; ii < np; ii++) {
        _gfaElm.elem.gfa.latlon[ii]    = lats[ii];
        _gfaElm.elem.gfa.latlon[ii+np] = lons[ii];
    }

/*
 *  Check the forecast hour. If it's a smear or outlook, snap the GFA.
 */
    pggfaw_checkHours ( &_gfaElm );
  
/*
 *  Note that np can change inside the pgsmear_snapEl, which is called
 *  by pggfaw_checkHours.  Ok, I agree that this side effect is dreadful.  
 *  This note will mark this landmine for clean up when we restructure 
 *  the code.
 */ 
    np = _gfaElm.elem.gfa.info.npts;

/*
 *  Set the from line.
 */
    pggfaw_setFrom ( np, &_gfaElm.elem.gfa.latlon[0],
                         &_gfaElm.elem.gfa.latlon[np] );

/*
 *  Set the arrow endpoint
 */
    cvg_todev( &_gfaElm, &np, xp, yp, &ier );
    if ( pggfaw_isClosed() ) {
       cgr_centroid( xp, yp, &np, &x_cntr, &y_cntr, &area, &ier );
    } else {
       x_cntr = xp[ 0 ];
       y_cntr = yp[ 0 ];
    }
    gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
                strlen(sys_D), strlen(sys_M) 
            );

    sprintf ( value, "%7.2f", c_lat );
    cvg_setFld ( &_gfaElm, TAG_GFA_ARROW_LAT, value, &ier );
    sprintf ( value, "%7.2f", c_lon );
    cvg_setFld ( &_gfaElm, TAG_GFA_ARROW_LON, value, &ier );

/*
 *  Suppress the attribute box with an invalid location. 
 *  Cdsgfa will not draw the attribute box if the "lat"
 *  is less than -90.0F.
 */
    if (cvg_plenabled()) {

/* 
 * If placement is enabled, start from the centroid
 * odds are it will be placed, but if its not its 
 * a good place to start from
 */
        sprintf ( value, "%7.2f", c_lat );
        cvg_setFld ( &_gfaElm, TAG_GFA_LAT, value, &ier );
        sprintf ( value, "%7.2f", c_lon );
        cvg_setFld ( &_gfaElm, TAG_GFA_LON, value, &ier );
    } else {
        cvg_setFld ( &_gfaElm, TAG_GFA_LAT, "-999.0", &ier );
        cvg_setFld ( &_gfaElm, TAG_GFA_LON, "-999.0", &ier );
    }

/*
 *  Add the new tag into the tag menu.
 */
    cvg_getFld ( &_gfaElm, TAG_GFA_TAG, tag, &ier );
    pggfaw_addTag ( atoi( tag ) );

/*
 *  Save and plot the GFA frame. The attribute text box
 *  will be added later.
 */
    pgvgf_saveNewElm ( NULL, sys_M, &_gfaElm, np, &_gfaElm.elem.gfa.latlon[ 0 ], 
    		       &_gfaElm.elem.gfa.latlon[ np ], TRUE, &loc, &ier );
    
    pgvgf_dsp ( loc, &ier );
    geplot ( &ier );

/*
 * Ask CVG if the box for the element was placed, if so we don't have
 * to worry about doing it ourselves
 */
    if (cvg_placed(loc, &_gfaElm) != TRUE) {

/*
 *  Set the current GFA element active so we can save the 
 *  text box location into it later.
 */
        pgactv_setActvElm ( &_gfaElm, loc );

/*
 *  Set up the ghosting & event handling to catch the plot
 *  location for the GFA attribute text box. 
 */
        pggfaw_setText ();
    } else {
        pgundo_newStep ();
        pgundo_storeThisLoc ( loc, UNDO_ADD, &ier );
        pgundo_endStep ();

/*
 *  Enable 'Show From Line' and 'Move Text' buttons
 */
        XtVaSetValues ( _txtMovePb, XmNsensitive, True, NULL );
        pggfaw_setFromLinePb ( True );
    }

    cvg_freeElPtr( &_gfaElm );
}

/*=====================================================================*/

static void pggfaw_setText ( void )
/************************************************************************
 * pggfaw_setText							*
 *									*
 * Sets the ghosting and event handling for the GFA attribute text box.	*
 *									*
 * static  void pggfaw_setText ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		03/04	adapt to more subtypes			*
 * J. Wu/SAIC		09/04	set _textDrawn flag to False		*
 * J. Wu/SAIC		09/04	remove airmet & non-convective sigmet	*
 * B. Yin/SAIC		10/04	re-write for the new GFA GUI		*
 * B. Yin/SAIC		11/05	remove _subtype				*
 * B. Yin/SAIC		12/05	call the set routine to set fromline btn*
 * S. Danz/AWC		03/06	use cds_gfatxt to create temp text box  *
 * L. Hinson/AWC        12/06   Check for ier status of 3, after call   *
 *                              to cds_gfatxt. If true, return; This    *
 *                              indicates a NIL condition exists in the *
 *                              text layout string and we want to bypass*
 *                              ghosting labels and rendering them.     *
 ***********************************************************************/
{
    int		ntxt = 1, np, ier;
    float	xx, yy;
    VG_DBStruct txt_el; 
/*---------------------------------------------------------------------*/
/*
 *  Set the text flag - the box has not been drawn yet.
 */
    _textDrawn = False;
    
/*
 *  Build a text element for ghosting box.
 */
    cds_gfatxt(&_gfaElm, &txt_el, &ier);
    if (ier == 3) {  /* If ier set to 3, indicative of */
	return;      /* NIL condition in textLayoutStr */
    }		     /* return */
    
    np = _gfaElm.elem.gfa.info.npts;    
    txt_el.elem.spt.info.lat = _gfaElm.elem.gfa.latlon[np-1];
    txt_el.elem.spt.info.lon = _gfaElm.elem.gfa.latlon[2*np-1];

    pgtxt_setLabelValue ( txt_el.elem.spt.text );
    
/*
 *  Do initialize "offset"s; otherwise you won't get the
 *  ghosting box.
 */
    txt_el.elem.spt.info.offset_x = 0;
    txt_el.elem.spt.info.offset_y = 0;

/*
 *  Set up event handling to catch the plot location.
 */
    _waitFlag = TRUE;

    mcanvw_setDynamicFunc ( (XtEventHandler)&pggfaw_txtPressEh, 
    			    (XtEventHandler)&pggfaw_txtDragEh, 
			    (XtEventHandler)NULL, CURS_DEFAULT );

    pggst_veilGhost (TRUE);
    
    pggst_setText ( &txt_el );

    gtrans ( sys_M, sys_D, &ntxt, &(txt_el.elem.spt.info.lat), 
	    &(txt_el.elem.spt.info.lon), &xx, &yy, 
	    &ier, strlen (sys_M), strlen (sys_D));

    mcanvw_setDynActFlag ( TRUE );
    
    mbotw_mouseSet ( LMHINT_PUT, MMHINT_DONE );

    _waitFlag = FALSE;

/*
 *  Enable 'Show From Line' and 'Move Text' buttons
 */
    XtVaSetValues ( _txtMovePb, XmNsensitive, True, NULL );
    pggfaw_setFromLinePb ( True );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_txtPressEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pggfaw_txtPressEh							*
 *									*
 * Press event handler for placing the GFA attribute text box.		*
 *									*
 * static void pggfaw_txtPressEh (wid, clnt, event, ctdr )		*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget				*
 *	clnt		XtPointer	not used			*
 *	*event		XEvent		event				*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 * J. Wu/SAIC		09/04	Set the _textDrawn flag to True		*
 * J. Wu/SAIC		10/04	access attr. using cvg_get/setFld()	*
 * J. Wu/SAIC		12/04	update undo/redo seq. when moving text	*
 * B. Yin/SAIC		08/06	fix bug (open a vgf then mv txt)	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * E. Safford/SAIC	04/07	fix memory leak				*
 ***********************************************************************/
{
    int		ntxt = 1, np, ier, xoff, yoff, ii, location, num;
    char	value[32];
    float	xx, yy, lats[MAXPTS], lons[MAXPTS], newlat, newlon;
    float	llx, lly, urx, ury;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    if ( _waitFlag ) return;

/*
 *  Erase the ghost line
 *  Note: Must pass in an element with VG class set to 0 to reset
 *        the _textFlag in "pggst" package to False. 
 */
    el.hdr.vg_class = 0;
    pggst_setText ( &el );
    pggst_clearGhost ( TRUE );

/*
 *  Prepare to save/draw the attribute box.
 */
    location = pgactv_getElmLoc ();
    pgutls_prepNew ( location, &_gfaElm, &llx, &lly, &urx, &ury, &ier );    

/*
 *  Catch the GFA attribute box's final location
 */
    xgtoff ( &xoff, &yoff, &ier );
    xx = (float) ( event->xbutton.x + xoff );
    yy = (float) ( event->xbutton.y + yoff );

    gtrans ( sys_D, sys_M, &ntxt, &xx, &yy, &newlat, &newlon, 
	     &ier, strlen (sys_D), strlen (sys_M) );
    
    if ( event->xbutton.button == Button1 ||
         ( event->xbutton.button == Button2 && !_txtActive ) ) {    
    	 sprintf ( value, "%7.2f", newlat );
	 cvg_setFld ( &_gfaElm, TAG_GFA_LAT, value, &ier );
    	 
	 sprintf ( value, "%7.2f", newlon );
	 cvg_setFld ( &_gfaElm, TAG_GFA_LON, value, &ier );
    }
        
/*
 *  Save and plot the new element 
 */
    np = _gfaElm.elem.gfa.info.npts;    
    for ( ii = 0; ii < np; ii++ ) {
        lats[ii] = _gfaElm.elem.gfa.latlon[ii];
	lons[ii] = _gfaElm.elem.gfa.latlon[ii+np];
    }    

    if ( event->xbutton.button == Button1 && _txtActive ) {
	pgundo_newStep ();
	
/*
 *  If the attribute box has not been drawn, do not 
 *  add the location to UNDO list. 
 */
	if ( _textDrawn ) pgundo_storeThisLoc ( location, UNDO_DEL, &ier );
        
	pgvgf_saveNewElm ( NULL, sys_M, &_gfaElm, np, lats, lons, TRUE, 
		&location, &ier );    
        pgundo_storeThisLoc ( location, UNDO_ADD, &ier );
        pgundo_endStep ();
    }
    else {
	pgundo_newStep ();
	if ( _txtActive ) pgundo_storeThisLoc ( location, UNDO_DEL, &ier );
        
	pgvgf_saveNewElm ( NULL, sys_M, &_gfaElm, np, lats, lons, TRUE,
		&location, &ier );    
        pgundo_storeThisLoc ( location, UNDO_ADD, &ier );         
    }
    
    pgvgf_dsp ( location, &ier );       
    cvg_freeElPtr( &_gfaElm );
    pgutls_redraw ( location, &_gfaElm, &ier );

/*
 *  Set the text flag - the box has been drawn.
 */
    _textDrawn = True;

/*
 *  Reset to draw a new one or keep editing
 */
    if ( event->xbutton.button == Button1 ) {        
		
	if ( _txtActive ) {   
            pggfaw_setText ();
        }
        else {
            pghdlb_deselectAll ();
	    pgnew_setArmDynamic ();
        }
        	        
    }
    else if ( event->xbutton.button == Button2 ) {      
	
	if ( _txtActive ) {            
	    _txtActive = False;
            XtSetSensitive ( _txtMovePb, True );
            
	    crg_getinx ( location, &num, &ier);
	    pghdlb_deselectEl ( num, TRUE );
            pghdlb_select ( &_gfaElm, location );	    
	    
	    mcanvw_disarmDynamic ();
	    mcanvw_setPressFunc ( (XtEventHandler)&pgevt_selectHdl, CURS_DEFAULT ); 
	    mbotw_mouseSet ( LMHINT_MOVEPOINT, MMHINT_DONE );	    
	}
	else {
	    pgnew_setArmDynamic ();	
	}	
    }     
    cvg_freeElPtr( &_gfaElm );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_txtDragEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pggfaw_txtDragEh							*
 *									*
 * Drag event handler for placing the GFA attribute text.		*
 *									*
 * static void pggfafw_txtDragEh ( wid, clnt, event, ctdr  )		*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget				*
 *	clnt		XtPointer	not used			*
 *	*event		XEvent		event				*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 ***********************************************************************/
{
    int		ier, xoff, yoff;
    float	xx[1], yy[1];
/*---------------------------------------------------------------------*/

    if ( _waitFlag ) return;

/*
 *  Erase the ghost line
 */
    pggst_clearGhost ( TRUE );
    
/*
 *  Add new ghostpoint
 */ 
    xgtoff ( &xoff, &yoff, &ier );
    xx[0] = (float) ( event->xbutton.x + xoff );
    yy[0] = (float) ( event->xbutton.y + yoff );
    pggst_addGhostPts ( 1, xx, yy, &ier );
    
/*
 *  Redraw the ghost line
 */
    pggst_drawGhost ( GST_NORMAL );
}

/*=====================================================================*/

static void pggfaw_setAddMode ( Boolean inAddMode )
/************************************************************************
 * pggfaw_setAddMode							*
 *									*
 * This function sets the GFA window to the new given mode.		*
 *									*
 * static void pggfaw_setAddMode ( inAddMode )				*
 *									*
 * Input parameters:							*
 *	inAddMode	Boolean		New mode (Add or Edit)		*
 *									*
 * Output parameters:							*
 *	None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 ***********************************************************************/
{
    _addMode = inAddMode;    
}

/*=====================================================================*/

Boolean pggfaw_isAddMode ( void )
/************************************************************************
 * pggfaw_isAddMode							*
 *									*
 * This function queries if the GFA window is in the "Add" mode.	*
 *									*
 * Boolean pggfaw_isAddMode ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *    None								*
 *									*
 * Return parameters:							*
 *    pggfaw_isAddMode() Boolean	Is/Is not in "Add" mode		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 ***********************************************************************/
{
    return ( _addMode );    
}

/*=====================================================================*/

void pggfaw_setType ( const char *newType )
/************************************************************************
 * pggfaw_setType							*
 *									*
 * This function sets the current area type in the menu to the new type	*
 *									*
 * void pggfaw_setType ( newType )					*
 *									*
 * Input parameters:							*
 *    *newType		char	New type to be set			*
 *									*
 * Output parameters:							*
 *    None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 * J. Wu/SAIC		09/04	remove airmet & non-convetive sigmet 	*
 * E. Safford/SAIC	11/04	remove the nDescription update		*
 * B. Yin/SAIC		11/05	remove _subtype				*
 * B. Yin/SAIC		06/06	set the tag number menu			*
 ***********************************************************************/
{
    int 	ii, mm, nn;
/*---------------------------------------------------------------------*/
    
    for ( ii = 0; ii < _nAreatyp; ii++ ) {
        	
/*
 *  If a match found, set to the new type. 
 */
	if ( strcasecmp ( newType, _areaTyp[ii] ) == 0 ) {

	    pgutls_setOptMenu ( _areaTyp[ii], _areaTyp,
	                        _nAreatyp, &_areaTypStrc );
            _areaTypeIndex = _areaTypStrc.current ;
	    
/*
 *  Set the tag number menu.
 */
            for ( mm = 0; mm < _nDesks; mm++ ) {

               for ( nn = 0; nn < _nAreatyp; nn++ ) {

	           if ( nn == _areaTypeIndex && mm == _deskStrc.current ) {

	              XtManageChild (  _tagStrc[ mm ][ nn ].form );

	           }
	           else {

	              XtUnmanageChild ( _tagStrc[ mm ][ nn ].form );
	           }
               }
            }
	    break;
	}
    }
}

/*=====================================================================*/

void pggfaw_setHour ( const char *newHour )
/************************************************************************
 * pggfaw_setHour							*
 *									*
 * This function sets the current forecast hour to the new hour.	*
 *									*
 * void pggfaw_setHour ( newHour )					*
 *									*
 * Input parameters:							*
 *    *newHour		char	New hour to be set			*
 *									*
 * Output parameters:							*
 *    None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 * B. Yin/SAIC		11/05	remove _subtype				*
 * B. Yin/SAIC		09/06	call pggfaw_setDfltTag to set tag	*
 * M. Li/SAIC		03/07	Strip '+' in newHour			*
 * M. Li/SAIC		03/07	Unmanage 'Other' entry			*
 * E. Safford/SAIC	06/07	fix source/dest overlap in timecmp	*
 ***********************************************************************/
{
    int 	ii, len;
    char	timecmp[10];
/*---------------------------------------------------------------------*/
  
    strcpy ( timecmp, newHour );
    len = strlen ( timecmp );
    if ( timecmp[len-1] == '+' ) {
        timecmp[ len-1 ] = '\0';
    }
    
    for ( ii = 0; ii < _nFcsthr; ii++ ) {
        
/*
 *  If a match found, set the forecast hour to the new hour. 
 */
        if ( strcasecmp ( timecmp,  _fcsthr[ii] ) == 0 ) {
	    pgutls_setOptMenu ( _fcsthr[ii],
    			        _fcsthr,
				_nFcsthr,
				&_fcsthrStrc );
            _currFcsthr = _fcsthrStrc.current;
	    pggfaw_setDfltTag( _fcsthr[ _currFcsthr ] );

	    if ( XtIsManaged(_fcsthrText) ) XtUnmanageChild ( _fcsthrText );
	    
            break;
        }
    }    	
}

/*=====================================================================*/

static void pggfaw_cleanup ( void )
/************************************************************************
 * pggfaw_cleanup							*
 *									*
 * This function does cleanup if the user switches loop, layer, filter,	*
 * or actions before placing the attribute box.  If the user is drawing	*
 * a new GFA element, the drawing will be completed and the box will be	*
 * placed in the center of the polygon.	If the user is editing a GFA	*
 * element, the box will remain at the last-placed location. 		*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *    None								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		09/04	initial coding				*
 * J. Wu/SAIC		10/04	access attr. using cvg_get/setFld()	*
 * B. Yin/SAIC		12/05	get GFA center point only for closed gfa*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		np, ier, one=1, loc;
    char	value[32];
    float	xp[MAXPTS], yp[MAXPTS], lat, lon;
    float	xc=0.0F, yc=0.0F, area=0.0F;
    float	llx, lly, urx, ury;
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
/*
 *  Activate "Move Text" button.
 */
    if ( _txtActive )  {
        _txtActive = False;
        XtSetSensitive ( _txtMovePb, True );        
    }

/*
 *  Clear the ghost attribute text box.
 *  Note: pggst_clearGhost() only clears the array of ghost point. 
 *        In order to inhibit the actual drawing of a ghost text
 *        box, the _textFlag in "pggst" package must be set to FALSE.
 *        However, we cannot manipulate this flag directly. So we have
 *        to make it happen by passing in an element with its VG class
 *        set to 0. 
 */
    el.hdr.vg_class = 0;
    pggst_setText ( &el );
    pggst_clearGhost ( TRUE );

/*
 *  If the user is adding a new GFA element but switches to something
 *  else (switching layer, changing loop, changing filter, or clicking
 *  other buttons) before place the attribute box, the drawing will be
 *  automatically completed by placing the box in the center of the GFA.
 */
    if ( _addMode && !_textDrawn ) {
        
        loc = pgactv_getElmLoc ();                        
	
	if ( loc >= 0 ) {
            
	    pgutls_prepNew ( loc, &el, &llx, &lly, &urx, &ury, &ier );    
	    
	    cvg_getFld ( &el, TAG_GFA_LAT, value, &ier );        
	    if ( ier == 0 && el.hdr.vg_type == GFA_ELM &&
	                     atof(value) < -90.0F ) {	   
                cvg_todev( &el, &np, xp, yp, &ier );

                if ( pggfaw_isClosed() ) {
                   cgr_centroid( xp, yp, &np, &xc, &yc, &area, &ier );
                }
                else {
                   xc = xp[ 0 ];
                   yc = yp[ 0 ];
                }

		if ( ier >= 0 ) {
                    gtrans( sys_D, sys_M, &one, &xc, &yc, &lat,
		            &lon, &ier, strlen(sys_D), strlen(sys_M) );
                    if ( ier >= 0 ) {
                        sprintf ( value, "%7.2f", lat );
			cvg_setFld ( &el, TAG_GFA_LAT, value, &ier );
			
                        sprintf ( value, "%7.2f", lon );
			cvg_setFld ( &el, TAG_GFA_LON, value, &ier );
		    }
		}
	    }
            
	    pgvgf_saveNewElm ( NULL, sys_M, &el, np, 
	                       &el.elem.gfa.latlon[0],
			       &el.elem.gfa.latlon[np], TRUE, &loc, &ier );    
            pgundo_storeThisLoc ( loc, UNDO_ADD, &ier );
            
/*
 *  free the blocks (pgutls_prepNew calls cvg_rdrec).
 */
	    cvg_freeElPtr ( &el );  

    	    pgvgf_dsp ( loc, &ier );        	    
	    pgutls_redraw ( loc, &el, &ier ); 	    	    
	    
/*
 *  free the blocks (pgutls_redraw calls cvg_rdrec)..
 */
	    cvg_freeElPtr ( &el );  
        
	}	
    }
        
/*
 *  Update the flag.
 */
    _textDrawn = True;    

/*
 *  free the blocks and reset.
 */
    cvg_freeElPtr ( &_gfaElm );  
    _gfaElm.elem.gfa.info.nblocks = 0;
    _gfaElm.elem.gfa.info.npts = 0;
}

/*=====================================================================*/

static void pggfaw_createPanel1 ( void )
/************************************************************************
 * pggfaw_createPanel1                                                  *
 *                                                                      *
 * This routine creates the top panel of the GFA dialog box.            *
 *                                                                      *
 * void pggfaw_createPanel1 ( )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                                                                      *
 *      		None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC  	10/04   Created                                 *
 * B. Yin/SAIC  	11/04   Added the 'NEW' check box               *
 *                      	Added forecast hour 'Other' option      *
 * B. Yin/SAIC  	11/04   Set 'RefToSig" to OFF as default        *
 * E. Safford/SAIC	12/04	change Seq/Upd to Upd			*
 * E. Safford/SAIC	01/05	change sigmet ref to buttons/text fld	*
 * B. Yin/SAIC		04/05	remove the sigmet text widget		*
 *				add the from line widget		*
 * E. Safford/SAIC	07/05	rm the update (sequence) number widgets *
 * H. Zeng/SAIC		09/05	removed all sigmet ref btns		*
 * T. Piper/SAIC	09/05	made ii long				*
 * L. Hinson/AWC        09/05   Revised GUI layout - Align Hazard, Tag, *
 *                              Forecast Hour Widgets in top row.       *
 *                              Convert Issue Status Check Buttons to   *
 *                                option push button and put in top row *
 *                              Align, Move Text/Save From Line to upper*
 *                                right portion of widget, stacked vert.*
 *                              Shorten width of From Line widget to    *
 *                                make room for color selection widget  *
 *                              Dropped SIGMET Reference Buttons        *  
 * B. Yin/SAIC		12/05	call the set routine to set fromline btn*
 * B. Yin/SAIC		04/06	reduced the from line field by 1 column *
 * J. Wu/SAIC		05/06	position _fcsthrText under fcst hr menu	*
 * B. Yin/SAIC		06/06	split tag menu to tag# and desk		*
 * J. Wu/SAIC		06/06	move fcsthr verification to gfaw_setAttr*
 * B. Yin/SAIC		07/07	move NxmClrW_popup to pggfaw_colorZeroCb*	
 ***********************************************************************/
{
    int 	mm, nn, oneRow = 40;
    Widget      form1;
    Arg    	args[10];
/*---------------------------------------------------------------------*/

    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, _editPane, NULL );

/*
 *  Create hazard pulldown menu
 */
    _areaTypStrc.current = 0;

    pggfa_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_areaTypStrc.current, "Hazard:",
			(XtCallbackProc)pggfaw_areatypPbCb, &_areaTypStrc.form, 
			&_areaTypStrc.label, &_areaTypStrc.menu, 
			_areaTypStrc.pb, 0, -5, NULL );
    
/*
 *  Forcast Hour pulldown menu
 */     
    _fcsthrStrc.current = 0;
    pggfa_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_fcsthrStrc.current, "Fcst Hr:",
			(XtCallbackProc)pggfaw_fcsthrPbCb, &_fcsthrStrc.form, 
			&_fcsthrStrc.label, &_fcsthrStrc.menu, 
			_fcsthrStrc.pb, 120, -5, NULL );

/*
 *  Create tag pulldown menu
 */
    _tagStrc = (struct optMenuStrc(*)[ MAXNOPT ] )
       		malloc( _nDesks * sizeof( struct optMenuStrc [ MAXNOPT ] ));


    for ( nn = 0; nn < _nDesks; nn++ ) {

        for ( mm = 0; mm < _nAreatyp; mm++ ) {

        _tagStrc[ nn ][ mm ].current = 0;

	pggfa_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_tagStrc[ nn ][ mm ].current, "Tag:",
			(XtCallbackProc)pggfaw_tagCb, &_tagStrc[ nn ][ mm ].form, 
			&_tagStrc[ nn ][ mm ].label, &_tagStrc[ nn ][ mm ].menu, 
			_tagStrc[ nn ][ mm ].pb, 0, -5, NULL );

        pgutls_setOptMenu ( _tags[ nn ][ mm ][ 0 ], 
       			    _tags[ nn ][ mm ], 
    			    _nTags[ nn ][ mm ], 
			    &_tagStrc[ nn ][ mm ] );

        XtVaSetValues ( _tagStrc[ nn ][ mm ].form, 
    			XmNx,		  205,
    			XmNwidth,	  75,
                	NULL );
	}
    }
 
/*
 *  Create desk pulldown menu
 */
    _deskStrc.current = 0;
    pggfa_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_deskStrc.current, "Desk:",
			(XtCallbackProc)pggfaw_deskCb, &_deskStrc.form, 
			&_deskStrc.label, &_deskStrc.menu, 
			_deskStrc.pb, 280, -5, NULL );

/* Issue Buttons pulldown menu */
    _issoptStrc.current = 0;
    pggfa_createOptionMenu( form1, MAXNOPT,
                          (XtPointer)&_issoptStrc.current, "Issue Type:",
                          (XtCallbackProc)pggfaw_optPbCb, &_issoptStrc.form,
                          &_issoptStrc.label, &_issoptStrc.menu,
                          _issoptStrc.pb, 340, -5, NULL);   
		
    _fcsthrText = XtVaCreateManagedWidget ( "fsct_text",
    		    xmTextWidgetClass,		form1,
		    XmNcolumns,			4,
		    XmNmaxLength,		4,
		    XmNeditable,		TRUE,
		    XmNvalue,			"",
		    XmNtopAttachment,		XmATTACH_FORM,
		    XmNtopOffset,		oneRow + 5,
		    XmNleftAttachment,		XmATTACH_FORM,
		    XmNleftOffset,		130,
		    NULL );

    XtAddCallback ( _fcsthrText, XmNlosingFocusCallback, 
      		    (XtCallbackProc)pggfaw_fcsthrTxtCb, NULL );					       

/*
 *  Text move button
 */
    _txtMovePb = XtVaCreateManagedWidget ( "Move Text",
		xmPushButtonWidgetClass,	form1,
		XmNx,				435,
		XmNy,				-2,
		XmNheight,			30,
		XmNwidth,			150,
		NULL ); 

    XtAddCallback ( _txtMovePb, XmNactivateCallback,
		    (XtCallbackProc)pggfaw_txtMoveCb, (XtPointer) NULL );
    
/*
 *  'Save From Line' button
 */
    _fromLinePb = XtVaCreateManagedWidget ( "Save From Line",
		xmPushButtonWidgetClass,	form1,
		XmNx,				435,
		XmNy,				25,
		XmNheight,			30,
		XmNwidth,			150,
		NULL );


    XtAddCallback ( _fromLinePb, XmNactivateCallback,
	 	    (XtCallbackProc)pggfaw_svfilePbCb, (XtPointer) NULL );
 
    pggfaw_setFromLinePb( True );

/*
 *  From line text 
 */
    nn = 0;
    XtSetArg( args[nn], XmNrows,		  1 ); 		nn++;
    XtSetArg( args[nn], XmNcolumns,		  56 ); 	nn++;
    XtSetArg( args[nn], XmNeditable, 		  False );	nn++;
    XtSetArg( args[nn], XmNvisualPolicy, 	  XmCONSTANT );	nn++;
    XtSetArg( args[nn], XmNscrollingPolicy,	  XmAUTOMATIC); nn++; 
    XtSetArg( args[nn], XmNscrollBarDisplayPolicy,XmAS_NEEDED); nn++;
    XtSetArg( args[nn], XmNcursorPositionVisible, False ); 	nn++;
    XtSetArg( args[nn], XmNx,			  3 );		nn++;
    XtSetArg( args[nn], XmNy,			  60 );	nn++; 


    _fromText = XmCreateScrolledText( form1, "fromText", args, nn );

    XtManageChild( _fromText );
    
    
    _colorPb = XtVaCreateManagedWidget ( "",
		xmPushButtonWidgetClass,	form1,
		XmNy,				60,
		XmNx,				556,
		XmNwidth,			25,
		XmNheight,			20,
		NULL );  


    XtAddCallback ( _colorPb, XmNactivateCallback,  
		    (XtCallbackProc)pggfaw_colorZeroCb, NULL );
    XtAddCallback ( _colorPb, XmNactivateCallback, 
		    (XtCallbackProc)pggfaw_optPbCb, NULL );
}

/*=====================================================================*/

static void pggfa_createOptionMenu ( Widget parent, int nbuttons,
				XtPointer pstart_item, char label_str[], 
				XtCallbackProc callback, Widget *form, 
				Widget *label, Widget *menu, Widget pbs[],
                                int posX, int posY,
				char *btnstrs[] )
/************************************************************************
 * pggfa_createOptionMenu						* 
 *									*
 * This function creates an option menu.				*
 *									*
 * static void pgutls_createOptionMenu (parent, nbuttons, pstart_item,	*
 *				 label_str, callback, form, label,	*
 *				 menu, pbs, posX, posY,btnstrs)	        *
 *									*
 * Input parameters:							*
 *	parent		Widget	the parent widget			*
 *	nbuttons	int	number of menu buttons			*
 *	pstart_item	XtPointer  pointer to starting menu item	*
 *	label_str[]	char	label string for label			*
 *									*
 * Ouput parameters:							*
 *	callback	XtCallbackPro	push button callback function	*
 *	*form		Widget	the underlying form			*
 *	*label		Widget	the side label				*
 *	*menu		Widget	the option menu				*
 *	pbs[]		Widget	the menu push buttons			*
 *      posX            int     the position in the X                   *
 *      posY            int     the position in the Y                   *
 *	*btnstrs[]	char	the menu push button labels		*
 *									*
 **									*
 * Log:									*
 *   L. Hinson/AWC      9/05    Initial Coding Hybrid from              *
 *                              pgutls_createOptionMenu &               *
 *                              pgtca_createOptMenu                     *
 ***********************************************************************/
{
    int		*start_item;
    long	ii;
    XmString	xmstr;
    Widget	pdmenu;
/*---------------------------------------------------------------------*/

    start_item = (int *) pstart_item;

   *form = 
	(Widget) XtVaCreateManagedWidget ("create_omen_form",
					  xmFormWidgetClass,	parent,
					  NULL);

    pdmenu = XmCreatePulldownMenu (*form, "omen_pdmenu", NULL, 0);
    *menu  = XmCreateOptionMenu   (*form, "omen_optmenu", NULL, 0);
    
    XtVaSetValues ( XmOptionButtonGadget( *menu ), 
                XmNalignment,           XmALIGNMENT_BEGINNING,
                NULL );
                
    XtVaSetValues ( XmOptionLabelGadget( *menu ), 
                XmNalignment,           XmALIGNMENT_BEGINNING,
                NULL );


    if (btnstrs == NULL) {
	for (ii = 0; ii < nbuttons; ii++) {
	    pbs[ii] = 
		XtVaCreateManagedWidget ("omen_pb",
					 xmPushButtonWidgetClass, pdmenu,
					 XmNuserData, pstart_item,
					 NULL);

	    if (callback) {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       callback, (XtPointer) ii);
	    }
	    else {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       (XtCallbackProc)pgutls_optPbCb, (XtPointer) ii);
	    }
	}
    }
    else {
	for (ii = 0; ii < nbuttons; ii++) {
	    pbs[ii] = 
		XtVaCreateManagedWidget (btnstrs[ii],
					 xmPushButtonWidgetClass, pdmenu,
					 XmNuserData, pstart_item,
					 NULL);

	    if (callback) {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       callback, (XtPointer) ii);
	    }
	    else {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       (XtCallbackProc)pgutls_optPbCb, (XtPointer) ii);
	    }
	}
    }

    xmstr = XmStringCreateLocalized ( label_str );
    XtVaSetValues (*menu, 
		   XmNlabelString,	xmstr,	
		   XmNsubMenuId,	pdmenu,
		   XmNmenuHistory,	pbs[*start_item], 
                   XmNorientation,      XmVERTICAL,
                   XmNx,                posX,
                   XmNy,                posY,
		   NULL);

    XmStringFree (xmstr);

    XtManageChild (*menu);
}

/*=====================================================================*/

static void pggfaw_createPanel2 ( void )
/************************************************************************
 * pggfaw_createPanel2                                                  *
 *                                                                      *
 * This routine creates the middle panel of the GFA dialog box.         *
 *                                                                      *
 * void pggfaw_createPanel2 ( )                                         *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                                                                      *
 *      		None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC  	10/04   Created                                 *
 * B. Yin/SAIC  	11/04   Set current haz panel in blank case     *
 * B. Yin/SAIC  	11/04   Disabled 'From Line' & 'Move Text' btns *
 * B. Yin/SAIC  	06/05   Added a callback to check top/bottom    *
 * L. Hinson/AWC        09/05   Fix problems with enabling panel2 when  *
 *                               nDesc is 0 but nInputField is > 0      *
 *                              Reset Text Widget to NULL when adding   *
 *                                new Airmets (_addMode is True)        *
 *                              Fix GUI Alignment on Input and Descript *
 *                                Widgets                               *
 * B. Yin/SAIC		11/05	Remove _subtype				*
 * B. Yin/SAIC		04/06	Set the text field widget to NULL	*
 * B. Yin/SAIC		04/06	Added FZL input field for ICE.   	*
 * B. Yin/SAIC		06/06	Set null panel2 for hazards w/o panel2 	*
 * B. Yin/SAIC		09/06	Set the current IFR type dialog		*
 * B. Yin/SAIC          06/07   Create text fields for MTOBSC/IFR types *
 ***********************************************************************/
{
    int         ii, jj, ier, inputIndex, pdIndex, checkboxIndex;
    int         nLabel, nChoices, oneRow = 36, rows, cols, nn, popupIndex;
    char        typeStr[ 32 ], desc[ MAXTBLNAME ], choice[ STD_STRLEN ];
    char        *tmpStr, desc1[ MAXTBLNAME ];
                                                                                               
    Arg         args[ 10 ];
    Widget      typePane, typeRowCol;
    Widget      addTypeBtn;
    Widget      wPulldown, wPushButton;
    XmString    xmStr;
/*---------------------------------------------------------------------*/

    if ( !_hazForm )  {
       _hazForm = XtVaCreateWidget ( "hazForm", xmFormWidgetClass, 
				     _editPane, NULL );
    }
	
    if ( XtIsManaged ( _hazForm ) ) {
       XtUnmanageChild ( _hazForm );
    }

    if ( _currentPanel2 && XtIsManaged ( _currentPanel2->form ) ) {
       XtUnmanageChild ( _currentPanel2->form ); 
    }

/*
 *  Close current type edit dialog (for IFR).
 */
    if ( ( _curTypeDlg != NULL ) && XtIsManaged ( _curTypeDlg ) ) {
  	   XtUnmanageChild ( _curTypeDlg );
    }

    if ( _addMode ) {

        XtSetSensitive ( _txtMovePb,  False );        
        XtSetSensitive ( _fromLinePb, False );        

    }

/*
 *  If the panel for the hazard exists, bring up the panel.
 */
    for ( ii = 0; ii < _nPanel2; ii++ ) {

	XtVaGetValues ( _areaTypStrc.menu, XmNsubMenuId, &wPulldown, NULL );
        XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

	XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

	XmStringFree ( xmStr );

 	if ( strcasecmp ( tmpStr, _panel2[ ii ].haz ) == 0 ) {

	    if ( _panel2[ ii ].nDesc 	   != 0 || 
	    	 _panel2[ ii ].nInputField != 0 ||
                 _panel2[ ii ].nCheckbox   != 0 || 
		 _panel2[ ii ].nPopup      != 0  ) {

    	       _currentPanel2 = &_panel2[ ii ];

               if (_addMode) {

                 for (jj = 0;jj < _panel2 [ ii ].nInputField; jj++) {

                   XtVaSetValues( _panel2[ ii ].inputField[ jj ].text,XmNvalue,"",NULL);

		   if ( ( strcasecmp ( _panel2[ ii ].haz, "ICE" ) == 0 ) && 
		   	( jj == _FZLIndex ) ) {

			pggfaw_setFzl ( OFF );

		   }
                 }
               }

	       XtManageChild ( _panel2[ ii ].form );
	       XtManageChild ( _hazForm );

	    }
	    else {
		_currentPanel2 = NULL;
	    }

	    XtFree ( tmpStr );
	    return;
	}
	XtFree ( tmpStr );
    }

/*
 * If the panel for the hazard does not exist, create the panel.
 */
    if ( _nPanel2 == 0 ) {
       G_MALLOC ( _panel2, panel2_t, _nPanel2 + 1, "pggfa middle panel" );
    }
    else {
       G_REALLOC ( _panel2, panel2_t, _nPanel2 + 1, "pggfa middle panel" );
    }

    strcpy ( _panel2[ _nPanel2 ].haz, 
    	     _areaTyp[ _areaTypStrc.current ] );

    _panel2[ _nPanel2 ].nInputField 	= 0;
    _panel2[ _nPanel2 ].nDesc 		= 0;
    _panel2[ _nPanel2 ].nCheckbox 	= 0;
    _panel2[ _nPanel2 ].nPopup 		= 0;

    _panel2[ _nPanel2 ].checkbox 	= NULL; 
    _panel2[ _nPanel2 ].checkboxLabel 	= NULL; 
    _panel2[ _nPanel2 ].popup 		= NULL;
    _panel2[ _nPanel2 ].popupLabel 	= NULL; 

    _panel2[ _nPanel2 ].form = XtVaCreateManagedWidget ( "panel2",
                        	xmFormWidgetClass, _hazForm, NULL );

    _currentPanel2 = &_panel2[ _nPanel2 ];

/*
 *  Read description infomation
 *  Create labels and pulldown menu (or user input fields).
 */
    ctb_gfagndesc ( _areaTyp[ _areaTypStrc.current ], 
    		    &nLabel, &ier );

    if ( ( ier == 0 ) && ( nLabel != 0 ) ) {

       for ( ii = 0, rows = 0; ii < nLabel; ii++, rows++ ) {

	   ctb_gfagdesc ( _areaTyp[ _areaTypStrc.current ], 
	   		  &ii, typeStr, desc1, &nChoices, &ier );      
		
           if ( ier != 0 ) { rows--; continue;}

/*
 *  Replace all "_" with " " in the desc string.
 */
	   strcpy( desc, desc1 );
	   while ( strstr ( desc1, "_" ) ) {

	         cst_rpst ( desc1, "_", " ", desc, &ier );
		 strcpy( desc1, desc );

	   }

	   strcat ( desc, ":" );

	   if ( strcasecmp ( typeStr, "userinput" ) == 0 ) {   /*user input*/
		
		inputIndex = _panel2[ _nPanel2 ].nInputField;

		if (  inputIndex == 0 ) {

		   G_MALLOC ( _panel2[ _nPanel2 ].inputField, userInput_t, 
			      inputIndex + 1, "gfa gui panel2" );

		}
		else {
			
		   G_REALLOC ( _panel2[ _nPanel2 ].inputField, userInput_t,
			       inputIndex + 1, "gfa gui panel2" );

		}
	      
	        _panel2[ _nPanel2 ].inputField [ inputIndex ].label = 
			XtVaCreateManagedWidget ( desc,
                	  	xmLabelWidgetClass,	_panel2[ _nPanel2 ].form,
                	  	XmNtopAttachment,	XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow + 5,
                	  	XmNleftAttachment,	XmATTACH_FORM,
                	  	XmNleftOffset,		0,
                	  	NULL );

    		_panel2[ _nPanel2 ].inputField [ inputIndex ].text = 
			XtVaCreateManagedWidget ( "bottom_text",
                		xmTextWidgetClass,      _panel2[ _nPanel2 ].form,
                		XmNmaxLength,           nChoices,
                		XmNeditable,            TRUE,
                		XmNvalue,               "",
                		XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow,
                		XmNleftAttachment,      XmATTACH_FORM,
                		XmNleftOffset,          120,
				XmNwidth,		225,
                    		XmNrecomputeSize,       False,
                		NULL );
	
    		XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text,
				 XmNmodifyVerifyCallback, 
		    		(XtCallbackProc)pggfaw_optPbCb, (XtPointer) NULL );

		if ( strcasecmp ( desc, "top/bottom:" ) == 0 ) {
    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNlosingFocusCallback, 
		    		   (XtCallbackProc)pggfaw_fillFLCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
		   		   XmNvalueChangedCallback, 
    		   		   (XtCallbackProc)pggfaw_changedFLCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNmodifyVerifyCallback, 
		    		   (XtCallbackProc)pggfaw_verifyFLCb, NULL );

		   if ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) {

			_FLIndex = inputIndex;

		   }

		}

		else if ( strcasecmp ( desc, "fzl top/bottom:" ) == 0 ) {

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNlosingFocusCallback, 
		    		   (XtCallbackProc)pggfaw_fillFzlCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNmodifyVerifyCallback, 
		    		   (XtCallbackProc)pggfaw_verifyFzlCb, NULL );

		   if ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) {

			_FZLIndex = inputIndex;

		   }

		   pggfaw_setFzl( OFF );

		}

		_panel2[ _nPanel2 ].nInputField++;

	   }

	   else if ( strcasecmp ( typeStr, "checkbox" ) == 0 ) {  /* check box */
		
		checkboxIndex = _panel2[ _nPanel2 ].nCheckbox;

		G_REALLOC ( _panel2[ _nPanel2 ].checkbox, Widget,
			    checkboxIndex + 1, "gfa gui panel2" );
		G_REALLOC ( _panel2[ _nPanel2 ].checkboxLabel, Widget,
			    checkboxIndex + 1, "gfa gui panel2" );

/*
 *   Create the toggle button label.
 */
		_panel2[ _nPanel2 ].checkboxLabel[ checkboxIndex ] = 
			XtVaCreateManagedWidget ( desc,
                       		xmLabelGadgetClass, 	_panel2[ _nPanel2 ].form,
		       		XmNmarginLeft,	   	0,
		       		XmNmarginRight,     	10,
				XmNtopAttachment,	XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow,
				XmNleftAttachment,	XmATTACH_FORM,
		       		NULL );

/*
 *   Create the toggle button
 */
	        _panel2[ _nPanel2 ].checkbox [ checkboxIndex ] = 
		    	XtVaCreateManagedWidget( " ",
                               	xmToggleButtonWidgetClass, _panel2[ _nPanel2 ].form,
				XmNtopAttachment,	XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow - 3,
				XmNleftAttachment,	XmATTACH_WIDGET,
                		XmNleftWidget,		_panel2[ _nPanel2 ].checkboxLabel[ checkboxIndex ],
				XmNmarginWidth,		5,
				XmNhighlightThickness,	0,
                               	NULL);

 		XtAddCallback ( _panel2[ _nPanel2 ].checkbox [ checkboxIndex ], 
				XmNvalueChangedCallback,
				(XtCallbackProc)pggfaw_setTypeTextCb, 
				(XtPointer) NULL );

                XtManageChild ( _panel2[ _nPanel2 ].checkbox [ checkboxIndex ] ); 

		_panel2[ _nPanel2 ].nCheckbox++;

	   }

	   else if ( strcasecmp ( typeStr, "popup" ) == 0 ) {  /* popup menu */
		
		popupIndex = _panel2[ _nPanel2 ].nPopup;

		G_REALLOC ( _panel2[ _nPanel2 ].popup, popup_t,
			    popupIndex + 1, "gfa gui panel2" );
		G_REALLOC ( _panel2[ _nPanel2 ].popupLabel, Widget,
			    popupIndex + 1, "gfa gui panel2" );

/*
 *  Create label for the push button that brings up the popup dialog.
 */
		_panel2[ _nPanel2 ].popupLabel[ popupIndex ] = 
			XtVaCreateManagedWidget ( desc,
                       		xmLabelGadgetClass, 	_panel2[ _nPanel2 ].form,
		       		XmNmarginLeft,	   	0,
		       		XmNmarginRight,     	10,
				XmNtopAttachment,	XmATTACH_FORM,
                                XmNtopOffset,           (rows == 0) ? 5 : rows * oneRow - 3,
				XmNleftAttachment,	XmATTACH_FORM,
		       		NULL );

/*
 *  Create the push button that brings up the popup dialog.
 */
                addTypeBtn = XtVaCreateManagedWidget( "Add/Remove Types",
                               	xmPushButtonWidgetClass, _panel2[ _nPanel2 ].form,
				XmNtopAttachment,	XmATTACH_FORM,
                                XmNtopOffset,           (rows == 0) ? 0 : rows * oneRow - 8,
				XmNleftAttachment,	XmATTACH_WIDGET,
                		XmNleftWidget,		_panel2[ _nPanel2 ].popupLabel[ popupIndex ],
				XmNmarginWidth,		5,
				NULL);

/*
 *  Create the popup dialog widget.
 */
    		xmStr = XmStringCreateLocalized ( "Types" );

                _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg =
                                XmCreateFormDialog ( _panel2[ _nPanel2 ].form, "popup",
                                                NULL, 0 );
                                                                                               
                _curTypeDlg = _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg;
                                                                                               
                XtVaSetValues ( _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg,
                                XmNnoResize,                    TRUE,
                                XmNdialogTitle,                 xmStr,
                                NULL );

       		XmStringFree(xmStr);

		XtAddCallback ( addTypeBtn, XmNactivateCallback,
				(XtCallbackProc)pggfaw_popupTypeDlgCb, 
                                (XtPointer) (_panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg) );

/*
 *  Create pane to hold all widgets in the dialog.
 */
    		typePane = XtVaCreateManagedWidget ( "gfaw_popup",
                                xmPanedWindowWidgetClass,       _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg,
                        	XmNmarginHeight,                1,
                        	XmNmarginWidth,                 1,
                        	XmNspacing,                     2,
				XmNsashWidth,			1,
				XmNsashHeight,	 		1, 
				XmNseparatorOn,			False,
                        	XmNorientation,       		XmVERTICAL, 
				NULL );

/*
 *  Create a RowColumn widget to hold all toggle buttons.
 */
    		typeRowCol = XmCreateRowColumn ( typePane, "typeRowCol", NULL, 0 );
    		XtManageChild( typeRowCol );

/* 
 *  Create all toggle buttons in the popup dialog windows.
 */
		_panel2[ _nPanel2 ].popup[ popupIndex ].btns  = 
			(Widget*) XtMalloc ((size_t)( nChoices + 1 ) * sizeof (Widget));

		_panel2[ _nPanel2 ].popup[ popupIndex ].nBtns = 0;
		    
	     	for ( jj = 0; jj < nChoices; jj++ ) {
				
	            ctb_gfagdc ( _areaTyp[ _areaTypStrc.current ], 
		    		 &ii, &jj, choice, &ier );

                    _panel2[ _nPanel2 ].popup[ popupIndex ].btns[ jj ] = 
		    		XtVaCreateManagedWidget ( choice,
                                	xmToggleButtonWidgetClass, typeRowCol,
                                	NULL);

 		    XtAddCallback ( _panel2[ _nPanel2 ].popup[ popupIndex ].btns[ jj ], 
		    		    XmNvalueChangedCallback,
				    (XtCallbackProc) pggfaw_setTypeTextCb, 
				    (XtPointer) NULL );

		    _panel2[ _nPanel2 ].popup[ popupIndex ].nBtns++;

		}
		
/*
 *  Create the 'Close' button in the popup dialog.
 */
                _panel2[ _nPanel2 ].popup[ popupIndex ].btns[ nChoices ] =
			XtVaCreateManagedWidget ( "Close", xmPushButtonWidgetClass, 
						 typePane, NULL);

 		XtAddCallback ( _panel2[ _nPanel2 ].popup[ popupIndex ].btns[nChoices], 
				XmNactivateCallback,
				(XtCallbackProc)pggfaw_closeTypeDlgCb, 
                                (XtPointer) _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg );

		_panel2[ _nPanel2 ].nPopup++;

/*
 * Add the type text field
 */
                cols = strlen( desc ) + strlen("Add/Remove Types");

                nn = 0;
                XtSetArg( args[nn], XmNrows,              1 );          nn++;
                XtSetArg( args[nn], XmNcolumns,           cols );       nn++;
                XtSetArg( args[nn], XmNeditable,          False );      nn++;
                XtSetArg( args[nn], XmNvisualPolicy,      XmCONSTANT ); nn++;
                XtSetArg( args[nn], XmNscrollingPolicy,   XmAUTOMATIC); nn++;
                XtSetArg( args[nn], XmNscrollBarDisplayPolicy, XmAS_NEEDED); nn++;
                XtSetArg( args[nn], XmNcursorPositionVisible, False );  nn++;
                XtSetArg( args[nn], XmNx,                         3 );  nn++;
                XtSetArg( args[nn], XmNy,    (rows+1) * oneRow - 5 );   nn++;
                                                                                                                 
                _panel2[ _nPanel2 ].typeText =
                        XmCreateScrolledText( _panel2[ _nPanel2 ].form,
                                              "fromText", args, nn );
                                                                                                                 
                XtManageChild( _panel2[ _nPanel2 ].typeText );
                                                                                                                 
                rows += 2;

	   }

	   else {					/* pull down menu */

		pdIndex = _panel2[ _nPanel2 ].nDesc;

                if ( pdIndex == 0 ) {
                                                                                
                   G_MALLOC ( _panel2[ _nPanel2 ].descStrc, struct optMenuStrc,
                              pdIndex + 1, "gfa gui panel2" );
                }
                                                                           
                else {
                                                                                 
                   G_REALLOC ( _panel2[ _nPanel2 ].descStrc, struct optMenuStrc,
                               pdIndex + 1, "gfa gui panel2" );
                }

    	      	_panel2[ _nPanel2 ].descStrc[ pdIndex ].current = 0;
                 
              	pgutls_createOptionMenu ( _panel2[ _nPanel2 ].form, MAXNOPT, 
			(XtPointer)&_panel2[ _nPanel2 ].descStrc[ pdIndex ].current, 
			desc, (XtCallbackProc)pggfaw_optPbCb, 
			&_panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
			&_panel2[ _nPanel2 ].descStrc[ pdIndex ].label, 
			&_panel2[ _nPanel2 ].descStrc[ pdIndex ].menu, 
		         _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb, NULL );


             	XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
			XmNtopAttachment,	XmATTACH_FORM,
                        XmNtopOffset,           rows * oneRow,
                    	XmNrecomputeSize,       False,
			NULL );

            	XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		110,
			NULL );


             	XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].label,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		5,
			NULL );
   

	     	for ( jj = 0; jj < nChoices; jj++ ) {
	            ctb_gfagdc( _areaTyp[ _areaTypStrc.current ], 
		    		&ii, &jj, choice, &ier);


	            xmStr = XmStringCreateLocalized ( choice );

	            XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ], 
			      XmNlabelString, 	xmStr, 
                    	      XmNrecomputeSize,       True,
			      NULL );

	            XmStringFree ( xmStr );


                    XtManageChild ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ] ); 
	    	}

            	for (; jj < MAXNOPT; jj++) {

	          if ( XtIsManaged ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ] ) ) {
	             XtUnmanageChild ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ] );
                  }
                }
	    	_panel2[ _nPanel2 ].nDesc++;
	   }
       }				/* loop to create user input or pulldown menu */
    } 

/* 
 *  Display the second panel
 */
    if ( _panel2[ _nPanel2 ].nDesc 	 != 0 || 
    	 _panel2[ _nPanel2 ].nInputField != 0 ||
         _panel2[ _nPanel2 ].nCheckbox 	 != 0 || 
	 _panel2[ _nPanel2 ].nPopup 	 != 0 ) {

       XtManageChild ( _panel2[ _nPanel2 ].form);

       if ( !XtIsManaged ( _hazForm ) ) 
          XtManageChild ( _hazForm );

    }
    _nPanel2++;
}

/*=====================================================================*/

static void pggfaw_getGuiInfo ( int *iret )
/************************************************************************
 * pggfaw_getGuiInfo							*
 *									*
 * This rountine reads in the GFA GUI infomation from a table.		*
 *									*
 * static void pggfaw_getGuiInfo ( iret )				*
 *									*
 * Input parameters:							*
 *	None								*
 *									*
 * Output parameters:							*
 *	*iret		int		Return value			*
 *				 	 0: normal return		*
 *				 	-1: no table/data		*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		10/04	Created					*
 * L. Hinson/AWC        09/05   Add call to ctb_gfagiss to get issue    *
 *                              status values from gfa.tbl              *
 * B. Yin/SAIC		11/05	Remove _subtype				*
 * B. Yin/SAIC		06/06	Read tag and desk menu items		*
 ***********************************************************************/
{
    int		ntags, ii, jj, ier;
    char	str[ STD_STRLEN * 2 ];
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Open the information table. If not found, return -1.
 */
    ctb_gfard ( iret );

    if ( *iret == 0 ) {
	
/* 
 * Read labels. 
 */
       ctb_gfaghaz ( ";", &_nAreatyp, str, &ier );
       pggfaw_fillStrArray ( MAXNOPT, MAX_AREATYPSTR, str, _areaTyp );		     

       ctb_gfagdesk ( ";", &_nDesks, str, &ier );
       pggfaw_fillStrArray ( MAXNOPT, MAX_TAGSTR, str, _desks);

       ctb_gfagtag ( ";", &ntags, str, &ier );

       _tags = (char(*)[ MAXNOPT ][ MAXNOPT ][ MAXTBLNAME ])
       		malloc( _nDesks * sizeof( char[ MAXNOPT ][ MAXNOPT ][ MAXTBLNAME ] ));

       _nTags = (int(*)[ MAXNOPT ] )
       		malloc( _nDesks * sizeof( int[ MAXNOPT ] ));

       for ( ii = 0; ii < _nDesks; ii++ ) {

           for ( jj = 0; jj < _nAreatyp; jj++ ) {

               pggfaw_fillStrArray ( MAXNOPT, MAX_TAGSTR, str, _tags[ ii ][jj] );
	       _nTags[ ii ][ jj ] = ntags;

	   }
       }
       
       ctb_gfagiss ( ";", &_nIssopt, str, &ier );
       pggfaw_fillStrArray ( MAXNOPT, MAX_ISSOPTSTR, str, _issopt );
   
       ctb_gfagfhr ( ";", &_nFcsthr, str, &ier );
       pggfaw_fillStrArray ( MAXNOPT, MAX_FCSTHRSTR, str, _fcsthr );       
   
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_fillFLCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_fillFLCb							*
 *									*
 * Callback function is used to fill the flight levels.			*
 *									*
 * static void pggfaw_fillFLCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		10/04		Created				*
 * B. Yin/SAIC		11/04		Enhanced validation function	*
 * B. Yin/SAIC		11/04		Enabled strings starting with /	*
 * B. Yin/SAIC		11/04		Fixed bug when input single '/'	*
 * B. Yin/SAIC		 6/05		Removed the validation section	*
 * L. Hinson/AWC        09/05           Changed Padstring call to append*
 *                                        zeros instead of prepending   *
 * B. Yin/SAIC		04/06		Enable FZL input if FL is FZL	*
 ***********************************************************************/
{
    char	*ptext = NULL, *pstr, tmpStr[ 8 ], *hazType;
    levels_t	*levels;
    
    Widget 	wPulldown, wPushButton;
    XmString	xmStr;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );

    if ( ptext && strlen ( ptext ) > (size_t)0 ) {

        if ( ( pstr = strtok ( ptext, "/" ) ) ) {

	    cst_padString ( pstr, '0', 1, 3, _topStr ); 

	}

        if ( ( pstr = strtok ( NULL, "/" ) ) ) {
  
 	   if ( ( strcasecmp ( pstr, "s" )   == 0 ) ||
	        ( strcasecmp ( pstr, "sf" )  == 0 ) ||		
	        ( strcasecmp ( pstr, "sfc" ) == 0 ) ) {
	      strcpy ( _bottomStr, "SFC" );
	   }
	   else if ( ( strcasecmp ( pstr, "f" )   == 0 ) ||
	   	     ( strcasecmp ( pstr, "fz" )  == 0 ) ||		
		     ( strcasecmp ( pstr, "fzl" ) == 0 ) ) {
	      strcpy ( _bottomStr, "FZL" );
	   }
	   else {
              cst_padString ( pstr, '0', 1, 3, _bottomStr );
	   }

 	}
	else {

/*
 *  Check if the hazard type is ice
 */
	  XtVaGetValues ( _areaTypStrc.menu, XmNsubMenuId, &wPulldown, NULL );
	  XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

	  XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
	  XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &hazType );

	  if ( strcasecmp ( hazType, "ice" ) == 0 ) {
	     
	     strcpy ( _bottomStr, "FZL" );
	  }
	  else {
	     strcpy ( _bottomStr, "SFC" );
	  }

          XmStringFree ( xmStr );
	  XtFree ( hazType );

	}
    }    
    else {
	strcpy ( _topStr,    "" );
        strcpy ( _bottomStr, "" );
    }

    sprintf ( tmpStr, "%s/%s", _topStr, _bottomStr );

    if ( strlen ( tmpStr ) > (size_t)1 ) {
       XtVaSetValues ( wid, XmNvalue, tmpStr, NULL );
    }

    if ( ptext ) XtFree ( ptext );

    if ( strcasecmp( _currentPanel2->haz, "ICE" ) == 0 ) {

	pggfaw_setFzl( strcasecmp( _bottomStr, "FZL" ) == 0 );

	if ( clnt ) {

	   levels = ( levels_t*)clnt;

	   if ( strcasecmp( _bottomStr, "FZL" ) == 0 ) {
              XtVaSetValues ( levels->fzlLabel, XmNsensitive, True, NULL );
              XtVaSetValues ( levels->fzlLevel, XmNsensitive, True, NULL );
	   }
	   else {
              XtVaSetValues ( levels->fzlLabel, XmNsensitive, False, NULL );
              XtVaSetValues ( levels->fzlLevel, XmNsensitive, False, NULL );
	   }
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_optPbCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_optPbCb                                                       *
 *                                                                      *
 * Callback function for option menu push buttons.                      *
 *                                                                      *
 * void pggfaw_optPbCb ( wid, clnt, cbs )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid		Widget          widget ID                       *
 *	clnt		XtPointer	client data			*     
 *      cbs		XtPointer       not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           10/04   Created		                *
 * B. Yin/SAIC		 01/06	 use pggfaw_setApplyBtn to enable Apply *
 * B. Yin/SAIC		 07/06	 reset the subtype			*
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/
    pggfaw_resetForSubtype( &ier );
    pggfaw_setApplyBtn( True );
}

/*=====================================================================*/

static void pggfaw_getPanel2Attr ( VG_DBStruct *el )
/************************************************************************
 * pggfaw_getPanel2Attr							*
 *									*
 * This routine gets the GFA attributes in panel2 and saves to 		*
 * an element.								*
 *									*
 * void pggfaw_getPanelsAttr ( el )					*
 *									*
 * Input parameters:							*
 *			NONE						*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	current element			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		11/04	Created					*
 * B. Yin/SAIC		11/04	Fixed the top/bottom input bug		*
 * L. Hinson/AWC        10/05   Add Exception Logic for MOD Severity    *
 *                              when not defined for Turbulence/Icing   *
 * B. Yin/SAIC		03/06	Save the type info for IFR		*
 * B. Yin/SAIC		04/06	Add fzl top/bottom for ICE.		*
 * J. Wu/SAIC           06/08   Get types from checkboxes               *
 ***********************************************************************/
{
    int		ii, ier, len;
    char	tag[ MAXTBLNAME ], *value, *tmpStr;
    char	value1[ MAXTBLNAME ], *value2, *col1, *col2;
    char        tmpStr2[ STD_STRLEN ], typeTextStr[ STD_STRLEN ];

    Widget	wPulldown, wPushButton;
    XmString    xmStr;
    
/*---------------------------------------------------------------------*/
/*
 *  Find the current hazard panel
 */
    if ( !_currentPanel2 ) return;
     
/*
 *  Loop over all option menu and user input fields
 *  Write the attribute tag and value to el
 */
    for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

	XtVaGetValues ( _currentPanel2->descStrc[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

/*
 *  Remove ":"
 */
	tmpStr[ strlen ( tmpStr ) - 1 ] = '\0';

	sprintf ( tag, "<%s>", tmpStr );

	XtFree ( tmpStr );
	XmStringFree( xmStr );
	
	XtVaGetValues ( _currentPanel2->descStrc[ ii ].menu, 
			XmNsubMenuId, &wPulldown, NULL );
        XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

	XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &value );

	cvg_setFld ( el, tag, value, &ier );
	
	XtFree ( value );
	XmStringFree( xmStr );
    }

/* Check that Severity has been set for Turbulence/Icing */
    cvg_getFld(el, "<Severity>", tmpStr2, &ier);
    if (ier != 0)
      if (strncasecmp(_currentPanel2->haz,"TURB",4) == 0 ||
         strncasecmp(_currentPanel2->haz,"ICE",4) == 0)
         cvg_setFld(el,"<Severity>","MOD",&ier);
    
     

    for ( ii = 0; ii < _currentPanel2->nInputField; ii++ ) {

	XtVaGetValues ( _currentPanel2->inputField[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &col1 );

/*
 *  Remove ":"
 */
	col1[ strlen ( col1 ) - 1 ] = '\0';

	XtVaGetValues ( _currentPanel2->inputField[ ii ].text, 
			XmNvalue, &value, NULL );

	if ( ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) &&
	     ( ii == _FZLIndex ) ) {

	   if ( strlen ( value ) != 0 ) {

	      strcpy ( value1, value );
	      value2 = strstr ( value1, "/" );
	      value2[ 0 ] = '\0';
	      value2++;

	      cvg_setFld ( el, TAG_GFA_FZL_TOP, value1, &ier );
	      cvg_setFld ( el, TAG_GFA_FZL_BOTTOM, value2, &ier );
	   }

	   XtFree ( col1 );
	   XtFree ( value );
	   XmStringFree( xmStr );

	   continue;
	}

/*
 *  Check if the input filed is in "xxx/xxx" format
 */
	col2 = strstr ( col1, "/" );

	if ( !col2 ) {

	   sprintf ( tag, "<%s>", col1 );
	   cvg_setFld ( el, tag, value, &ier );

	}
	else {

	   col2[ 0 ] = '\0';
	   col2++;
	   strcpy ( value1, value );
	   value2 = strstr ( value1, "/" );

	   if ( !value2 ) {

	      if ( strcasecmp ( col1, "top" ) == 0 ) {
		 strcpy ( tag, TAG_GFA_TOP );
	      }
	      else {
	         sprintf ( tag, "<%s>", col1 );
	      }

	      cvg_setFld ( el, tag, value1, &ier );

	      if ( strcasecmp ( col2, "bottom" ) == 0 ) {
		 strcpy ( tag, TAG_GFA_BOTTOM );
	      }
	      else {
	         sprintf ( tag, "<%s>", col2 );
	      }

	      cvg_setFld ( el, tag, "", &ier );
	   }
	   else {

	      value2[ 0 ] = '\0';
	      value2++;

	      if ( strcasecmp ( col1, "top" ) == 0 ) {
		 strcpy ( tag, TAG_GFA_TOP );
	      }
	      else {
	         sprintf ( tag, "<%s>", col1 );
	      }

	      cvg_setFld ( el, tag, value1, &ier );

	      if ( strcasecmp ( col2, "bottom" ) == 0 ) {
		 strcpy ( tag, TAG_GFA_BOTTOM );
	      }
	      else {
	         sprintf ( tag, "<%s>", col2 );
	      }

	      cvg_setFld ( el, tag, value2, &ier );

	   }

	   XtFree ( col1 );
	   XtFree ( value );
	   XmStringFree( xmStr );
	}
    }

/*
 * save the type info for hazards (listed with checkboxes and/or popups)
 */
    if ( _currentPanel2->nCheckbox > 0 ||
         _currentPanel2->nPopup > 0 ) {

        if ( _currentPanel2->nPopup > 0 ) {
            value = XmTextGetString( _currentPanel2->typeText );
            cvg_setFld ( el, "<Type>", value, &ier );
            XtFree ( value );
        }
        else {
            typeTextStr[0] = '\0';
            for ( ii = 0; ii < _currentPanel2->nCheckbox; ii++ ) {

                if (  XmToggleButtonGetState ( _currentPanel2->checkbox[ ii ] ) ) {

                    XtVaGetValues ( _currentPanel2->checkboxLabel[ ii ], XmNlabelString,
                           &xmStr, NULL );
                    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

                    strcat ( typeTextStr, tmpStr );

                    XtFree ( tmpStr );
                    XmStringFree ( xmStr );
                }
            }

            len = strlen( typeTextStr );
            if ( len > 0 && typeTextStr[ len - 1 ] == ':' ) {
                typeTextStr[ len - 1 ] = '\0';
            }

            cvg_setFld ( el, "<Type>", typeTextStr, &ier );

        }
    }
}

/*=====================================================================*/

static void pggfaw_setPanel2Attr ( VG_DBStruct *el )
/************************************************************************
 * pggfaw_setPanel2Attr							*
 *									*
 * This routine sets the GFA attributes in panel2 from a GFA element. 	*
 *									*
 * void pggfaw_setPanelsAttr ( el )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	current element			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		11/04	Created					*
 * B. Yin/SAIC		11/04	Set Top/Bottom to "" if not in element	*
 * B. Yin/SAIC		03/06	Set Toggle buttons in the popup dialog	*
 * B. Yin/SAIC		04/06	Check text field before set its value	*
 * B. Yin/SAIC		04/06	Add fzl top/bottom for ICE.		*
 * E. Safford/SAIC	07/06	rm initial '/' on labelStr		*
 * B. Yin/SAIC          07/07   check nPopup before setting typeText    *
 * J. Wu/SAIC           06/08   Set checkboxes if they exist            *
 ***********************************************************************/
{
    int		ii, jj, len, ier;
    char	tag[ MAXTBLNAME ], *tmpStr, typeText[ STD_STRLEN ];
    char	value[ MAXTBLNAME ], col1[ MAXTBLNAME ], *col2;
    char	labelStr[ STD_STRLEN ];

    XmString    xmStr;
/*---------------------------------------------------------------------*/
/*
 *  Find the current hazard panel
 */
    pggfaw_createPanel2 ( );
    if ( !_currentPanel2 ) return;
     
/*
 *  Loop over all option menu and user input fields
 *  Read the attribute values from el
 */
    for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

	XtVaGetValues ( _currentPanel2->descStrc[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

/*
 *  Remove ":"
 */
	tmpStr[ strlen ( tmpStr ) - 1 ] = '\0';

	sprintf ( tag, "<%s>", tmpStr );

	XtFree ( tmpStr );
	XmStringFree( xmStr );

	cvg_getFld ( el, tag, value, &ier );

	for ( jj = 0; jj < MAXNOPT; jj++ ) {
	    
	    XtVaGetValues ( _currentPanel2->descStrc[ ii ].pb[ jj ],
			    XmNlabelString, &xmStr, NULL );
	    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

	    if ( strcmp ( tmpStr, value ) == 0 ) {

	       XtVaSetValues( _currentPanel2->descStrc[ ii ].menu,
		              XmNmenuHistory, 
			      _currentPanel2->descStrc[ ii ].pb[ jj ],
			      NULL );
	       break;
	    }
	    XtFree ( tmpStr );
	    XmStringFree( xmStr );
	}
    } 

    for ( ii = 0; ii < _currentPanel2->nInputField; ii++ ) {

	if ( ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) &&
	     ( ii == _FZLIndex ) ) {

	   cvg_getFld ( el, TAG_GFA_FZL_TOP, value, &ier );
	   cvg_getFld ( el, TAG_GFA_FZL_BOTTOM, col1, &ier );

	   if ( ( strlen( value ) != 0 ) && ( strlen( col1 ) != 0 ) ) {
	      
	      sprintf ( value, "%s/%s", value, col1 );

              XtVaSetValues( _currentPanel2->inputField[ ii ].text, 
	   		     XmNvalue, value, NULL );

	      pggfaw_setFzl( ON );

	   }
	   else {
	      pggfaw_setFzl( OFF );
	   }
	   continue;
	}

	XtVaGetValues ( _currentPanel2->inputField[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

/*
 *  Remove ":"
 */
	tmpStr[ strlen ( tmpStr ) - 1 ] = '\0';

/*
 *  Check if the input filed is in "xxx/xxx" format
 */
	strcpy ( col1, tmpStr );
	col2 = strstr ( col1, "/" );

	if ( !col2 ) {

	   sprintf ( tag, "<%s>", col1 );
	   cvg_getFld ( el, tag, value, &ier );

	   if ( strlen ( value ) > (size_t)0 )
              XtVaSetValues( _currentPanel2->inputField[ ii ].text, 
	     		     XmNvalue, value, NULL );

	}
	else {

	   col2[ 0 ] = '\0';
	   col2++;


	   if ( strcasecmp ( col1, "top" ) == 0 ) {
	      strcpy ( tag, TAG_GFA_TOP );
	   }
	   else {
	      sprintf ( tag, "<%s>", col1 );
	   }

	   cvg_getFld ( el, tag, value, &ier );

	   len = strlen ( value );
	   value[ len ] = '/';


	   if ( strcasecmp ( col2, "bottom" ) == 0 ) {
	      strcpy ( tag, TAG_GFA_BOTTOM );
	   }
	   else {
	      sprintf ( tag, "<%s>", col2 );
	   }

	   cvg_getFld ( el, tag, &value[ len + 1 ], &ier );

	   if ( strlen ( value ) > (size_t)1 ) {
              XtVaSetValues( _currentPanel2->inputField[ ii ].text, 
	   		     XmNvalue, value, NULL );
	   }
	   else {
              XtVaSetValues( _currentPanel2->inputField[ ii ].text, 
	      	   	     XmNvalue, "", NULL );
    	   }
       }
       XtFree ( tmpStr );
       XmStringFree( xmStr );
    }       

/*
 *  Get the 'Type' info and set all checkboxes in the 2nd panel
 *  and all toggle buttons in the popup dialog window.
 */
    cvg_getFld ( el, "<Type>", typeText, &ier );
    if ( ier == 0  ) {
        if ( ( _currentPanel2->nPopup != 0 ) &&
             ( _currentPanel2->typeText != NULL ) ) {

            XmTextSetString ( _currentPanel2->typeText, typeText );
        }

        if ( ( _currentPanel2->nCheckbox == 0 ) &&
             ( _currentPanel2->nPopup    == 0 ) ) {
            typeText[ 0 ] = '\0';
        }
    }
    else {

        typeText[ 0 ] = '\0';

    }

    for ( ii = 0; ii < _currentPanel2->nCheckbox; ii++ ) {

        XtVaGetValues ( _currentPanel2->checkboxLabel[ ii ], XmNlabelString,
                        &xmStr, NULL );
        XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

/*
 *  Remove ":"
 */
	tmpStr[ strlen( tmpStr ) - 1 ] = '\0';

	if ( strstr( typeText, tmpStr ) ) {
	   XmToggleButtonSetState( _currentPanel2->checkbox[ ii ], True, False );
	}
	else {
	   XmToggleButtonSetState( _currentPanel2->checkbox[ ii ], False, False );
	}
	XtFree ( tmpStr );
        XmStringFree( xmStr );
    }

    for ( ii = 0; ii < _currentPanel2->nPopup; ii++ ) {
	for ( jj = 0; jj < _currentPanel2->popup[ ii ].nBtns; jj++ ) {
		
	    XtVaGetValues ( _currentPanel2->popup[ ii ].btns[ jj ], XmNlabelString, 
			    &xmStr, NULL );

	    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
	    sprintf( labelStr, "%s", tmpStr );

	    if ( strstr( typeText, labelStr ) ) {

	       XmToggleButtonSetState ( _currentPanel2->popup[ ii ].btns[ jj ], 
	       				True, False );
	    }
	    else {
	       XmToggleButtonSetState ( _currentPanel2->popup[ ii ].btns[ jj ], 
	       				False, False );
	    }
	    XtFree ( tmpStr );
            XmStringFree( xmStr );
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_vrfyFcstHrCb ( Widget text_w, XtPointer clnt,
							XtPointer cbs )
/************************************************************************
 * pggfaw_vrfyFcstHrCb                                                  *
 *                                                                      *
 * Verify the contents of the forecast hour text widget as the user is  *
 * entering data.  The input must be in the format of "H:MM", where 1st	*
 * char 'H' must be 0 to 5, 2nd char must be ':', 3rd char must be 0, 1,*
 * 3, or 4, and the 4th char must be 0 or 5. Any other characters will  *
 * force the "doit" flag of the cbs structure be set to "FALSE", which  *
 * signals the text widget to NOT display the data and SOUND the alarm  *
 * bell (a single beep).  Note that the bell may be switched off by     *
 * setting the text widget's XmNaudibleWarning value, which is "TRUE"   *
 * by default.                                                          *
 *                                                                      *
 * Note: This function should be linked to the text widget via the      *
 *       XmNmodifyVerifyCallback event, which fires before the          *
 *       XmNvalueChangedCallback and XmNlosingFocusCallback events.     *
 *                                                                      *
 * static void pggfaw_vrfyFcstHrCb ( text_w, clnt, cbs )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      text_w          Widget    text widget                           *
 *      clnt		XtPointer Widget's event data (not used here)	*
 *      cbs       XtPointer callback structure                 	*
 *                                                                      *
 * Output parameters:                                                   *
 *                      None.                                           *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           04/04   initial coding                          *
 * J. Wu/SAIC           04/04   Limit the input as "xxx-xxx"            *
 * J. Wu/SAIC           06/04   adjust the check for '-'                *
 * E. Safford/SAIC      09/04   add ARGSUSED                            *
 * B. Yin/SAIC		01/06	use pggfaw_setApplyBtn to enable Apply  *
 * J. Wu/SAIC           05/06   force format of "H:MM" for specials	*
 ***********************************************************************/
{
    int         ii;
    char        *text, newText[ 6 ], cval;
    
    XmTextVerifyCallbackStruct *call_struct =
                                (XmTextVerifyCallbackStruct *) cbs;
/*---------------------------------------------------------------------*/

    if ( call_struct->text->ptr == NULL ) {
        return;
    }

/*
 *  Get the current contents of the forecast hour text 
 */
    XtVaGetValues ( text_w, XmNvalue, &text, NULL );
    
/*
 *  Construct the resulting text
 */
    newText [ 0 ] = '\0';
    strncat ( newText, text, call_struct->startPos );	
    strcat ( newText, call_struct->text->ptr );
    strcat ( newText, &text[ call_struct->endPos ] );

/*  
 *  Verify the resulting text:
 *      First char shoud be 0 to 5; 
 *      2nd char must be ':';
 *      3rd char should be 0, 1, 3, or 4;
 *      Fourth char should be 0 if 3rd char is 0 or 3;
 *                            5 if 3rd char is 1 or 4;
 */     	
    strcpy ( _tmpFcsthrStr, text );
    for ( ii = 0; ii < (int)strlen( newText ); ii++ ) {
        
	cval = newText[ ii ];

	if ( ( ii == 0 && cval != '0' && cval != '1' &&
	                  cval != '2' && cval != '3' &&
			  cval != '4' && cval != '5' )	||
             ( ii == 1 && newText[ ii ] != ':' )        ||
             ( ii == 2 && cval != '0' && cval != '1' &&
			  cval != '3' && cval != '4' )	||
             ( ii == 3 && ( ( ( newText[ 2 ] == '0' ||
	                        newText[ 2 ] == '3' ) && 
				cval != '0' ) ||
                            ( ( newText[ 2 ] == '1' || 
	                        newText[ 2 ] == '4' ) && 
			        cval != '5' ) ) ) )  {

            call_struct->doit = False;
            break;
	}			
    }	
    XtFree( text );
    pggfaw_setApplyBtn( True );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_fcsthrTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_fcsthrTxtCb                                                   *
 *                                                                      *
 * Callback function for the optional forecast hour text. This text     *
 * is available only when the forecast hour menu selection is the       *
 * last option "Other".                                                 *
 *                                                                      *
 * static void pggfaw_fcsthrTxtCb ( wid, clnt, cbs )                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid		Widget		Widget ID                       *
 *	clnt		XtPointer	not used                        *
 *	cbs		XtPointer	callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           04/04   initial coding                          *
 * J. Wu/SAIC           04/04   remove the ending '-' if it exists      *
 * J. Wu/SAIC           08/04   link with layer control window          *
 * J. Wu/SAIC           05/06   add auto-complete for special inputs	*
 * M. Li/SAIC		03/07	check for invalid _fcsthrStr		*
 ***********************************************************************/
{
    int		slen;
    char	*ptext = NULL;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );

    if ( ptext ) {	
	strcpy ( _fcsthrStr, ptext );
        
	slen = (int)strlen( ptext );
	if ( slen == 1 ) {
	    strcat ( _fcsthrStr, ":00" );	    	
	}
	else if ( slen == 2 ) {
	    strcat ( _fcsthrStr, "00" );	    
	}
	else if ( slen == 3 ) {
	    if ( ptext[2] == '0' || ptext[2] == '3' ) {
	        strcat ( _fcsthrStr, "0" );	    	    
	    }
	    else {
	        strcat ( _fcsthrStr, "5" );	    
	    }
	}

	if (strlen(_fcsthrStr) <= 0) return;
	
	XtVaSetValues ( wid,
	    		XmNvalue,		_fcsthrStr, 
			XmNcursorPosition,	4,
			NULL );
		
	XtFree ( ptext );

/*
 *   Match the new hour to filter time if the filtering is active.
 */
        if ( pgfilterw_isUp () ) {
            pgfilterw_turnOnTime ( _fcsthrStr );
        }
    }
}

/*=====================================================================*/

void pggfaw_checkHours ( VG_DBStruct *el )
/************************************************************************
 * pggfaw_checkHours                                                    *
 *                                                                      *
 * This routine checks the GFA forecast hours,  sets the GFA category   *
 * (smear/outlook/snapshot), and snaps GFA if it's a smear or outlook.	*
 *                                                                      *
 * void pggfaw_checkHours ( el ) 			                *
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	current element			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		03/05	Created					*
 * E. Safford/SAIC      06/06   allow snap to make el smaller           *
 * B. Yin/SAIC		05/05	Added single hour smear			*
 * J. Wu/SAIC		10/05	Added single hour outlooks 9-9 & 12-12	*
 * B. Yin/SAIC		11/05	Changed GFA category to subtype		*
 * B. Yin/SAIC		12/05  	Keep the GFA subtype if the hours don't	*
 *				change in the edit mode			*
 * B. Yin/SAIC		07/06	Call pggfaw_makeSubtype to get subtype	*
 * J. Wu/SAIC		01/08	Removed hard-coded checks for AIRM/OTLK	*
 * J. Wu/SAIC		02/08	Fixed a bug in checking OTLK		*
 ***********************************************************************/
{
    int		ier, subtype, minu;    
    char	hours[ 32 ], subtypeStr[ 32 ], level[ 32 ], haz[ 32 ];
    char	secondHr[ 32 ], *cptr;
    Boolean	isAIRM = False, isOTLK = False;
/*---------------------------------------------------------------------*/

    level[ 0 ] = '\0';
    cvg_getFld ( el, "Level", level, &ier );

    strcpy ( haz, _areaTyp[ _areaTypeIndex ] );

    if ( ( strcasecmp( haz, "fzlvl" ) == 0 ) && ( ier == 0 ) &&
    	 ( strcasecmp( level, "sfc" ) == 0 ) ) {

	strcat ( haz, "_SFC" );

    }

    hours[ 0 ] = '\0';
    cvg_getFld ( el, TAG_GFA_FCSTHR, hours, &ier );

    /*
     *  If "hours" contains a "-" and the hour after "-" > 6 hours,  it is an OUTLOOK.
     *  If "hours" contains a "-" and the hour after "-" <= 6 hours,  it is an AIRMET.
     */
    if ( ier == 0 ) {
        if ( strchr( hours, '-' ) ) {
            
	    strcpy ( secondHr, hours );	    
            cptr = strtok( secondHr, "-" );            
	    cptr = strtok ( NULL, "-" );	
	    minu = 0;
	    if ( cptr ) {
                minu = atoi ( cptr ) * 60;
                cptr = strtok ( NULL, ":" );	
                if ( cptr ) {
	            minu += atoi( cptr );
                }
	    }
	    
	    if ( minu > 360 ) {
	        isOTLK =  True;   
	    }
	    else {
	        isAIRM =  True;   	    
	    }   
	}
    }
        

    /*
     *  Now set the subtype.
     */
    if ( isAIRM ) {

       pggfaw_makeSubtype ( pggfaw_getHazardType( haz ),
       			    GFA_USER_SMEAR, &subtype, &ier );

       if ( ier < 0 ) subtype = 99;

       sprintf( subtypeStr, "%d", subtype );
       cvg_setFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

       pgsmear_snapEl ( FALSE, el, &ier );

    }
    else if ( isOTLK ) {

       pggfaw_makeSubtype ( pggfaw_getHazardType( haz ),
       			    GFA_USER_OUTLOOK, &subtype, &ier );

       if ( ier < 0 ) subtype = 99;

       sprintf( subtypeStr, "%d", subtype );
       cvg_setFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

       pgsmear_snapEl ( FALSE, el, &ier );

    }
    else {

       pggfaw_makeSubtype ( pggfaw_getHazardType( haz ),
       			    GFA_SNAPSHOT, &subtype, &ier );

       if ( ier < 0 ) subtype = 99;

       sprintf( subtypeStr, "%d", subtype );
       cvg_setFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

    }

/*
 *  Get GFA line width
 */
    pggfaw_getLineWidth ( el );
}

/*=====================================================================*/

/* ARGSUSED */
static void pggfaw_verifyFLCb ( Widget wid, XtPointer clnt, 
							XtPointer cbs ) 
/************************************************************************
 * pggfaw_verifyFLCb                                                    *
 *                                                                      *
 * This routine checks the validation of top/bottom flight levels.      *
 *                                                                      *
 * void pggfaw_verifyFLCb ( wid, clnt, cbs ) 	        		*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid		Widget	  top/bottom text field widget		*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	cbs	XtPointer callback structure				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		05/05	Created					*
 * T. Piper/SAIC	09/05	made ii size_t				*
 ***********************************************************************/
{
    size_t	ii;
    char	*text, newText[ 8 ], *strPtr, *hazType;
    Boolean	ice;

    Widget 	wPulldown, wPushButton;
    XmString	xmStr;

    XmTextVerifyCallbackStruct *call_struct = 
				(XmTextVerifyCallbackStruct *) cbs;
/*---------------------------------------------------------------------*/

    if ( call_struct->text->ptr == NULL ) {
	return;
    }

/*
 *  Check if the hazard type is ice
 */
    XtVaGetValues ( _areaTypStrc.menu, XmNsubMenuId, &wPulldown, NULL );
    XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

    XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &hazType );

    ice = ! strcasecmp ( hazType, "ice" );
    
    XmStringFree ( xmStr );
    XtFree ( hazType );

/*
 *  Get the current contents of the top/bottom widget 
 */
    XtVaGetValues ( wid, XmNvalue, &text, NULL );

/*
 *  Check the validation
 */
    if ( ( call_struct->text->length + (int)strlen ( text ) - 
         ( call_struct->endPos - call_struct->startPos ) ) > 7 ) {
       
       call_struct->doit = False;

    }
    else  {

/*
 *  Construct the resulting text
 */
       newText [ 0 ] = '\0';
       strncat ( newText, text, call_struct->startPos );	
       strcat ( newText, call_struct->text->ptr );
       strcat ( newText, &text[ call_struct->endPos ] );

       if ( strlen ( newText ) > (size_t)0 ) {

/*
 *  No spaces are allowed
 */
          if ( strchr ( newText, ' ' ) ) {

	     call_struct->doit = False;

          }
          else if ( isdigit ( newText[ 0 ] ) == 0 ) { /* first chr is not digit */

	     call_struct->doit = False;

	  }
	  else if ( strchr ( newText, '/' ) != strrchr ( newText, '/' ) ) { /* more than 1 '/' */

	     call_struct->doit = False;

     	  }

          else { 
          
/* 
 *  Check first column
 */
	     strPtr = strtok ( newText, "/" );

             if ( strlen ( strPtr ) > (size_t)3 ) {

		call_struct->doit = False;

	     }
	     else {

		for ( ii = 1; ii < strlen ( strPtr ); ii++ ) {
		    
		    if ( isdigit ( strPtr[ ii ] ) == 0 ) call_struct->doit = False;
		}
	     }

/*
 *  Check second column
 */
	     if ( ( strPtr = strtok ( NULL, "/" ) ) ) {

		if ( strlen ( strPtr ) > (size_t)3 ) {
		      
	           call_struct->doit = False;
		}
		else if ( !( strcasecmp ( strPtr, "s" )   == 0 ||
			     strcasecmp ( strPtr, "sf" )  == 0 ||
			     strcasecmp ( strPtr, "sfc" ) == 0 ||
			     ( ( strcasecmp ( strPtr, "f" )   == 0  ||
			         strcasecmp ( strPtr, "fz" )   == 0 ||
			         strcasecmp ( strPtr, "fzl" ) == 0 ) && ice ) ) ) {

		     for ( ii = 0; ii < strlen ( strPtr ); ii++ ) {
				    
		         if ( isdigit ( strPtr[ ii ] ) == 0 ) call_struct->doit = False;
		     }
		}
	     }
	  }
       }
    }
    XtFree ( text );
}

/*=====================================================================*/

Boolean pggfaw_okToDraw( Boolean showDlg )
/************************************************************************
 * pggfaw_okToDraw                                                      *
 *                                                                      *
 * This routine checks if top/bottom field is empty. 			*
 *                                                                      *
 * Boolean pggfaw_okToDraw ( )				 	        *
 *                                                                      *
 * Input parameters:                                                    *
 * 	showDlg		Boolean		show the warning dialog if true	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		06/05	Created					*
 * B. Yin/SAIC		06/05	Added a parameter to show the dialog	*
 * E. Safford/SAIC	11/05	use main window as parent for           *
 *				  pggfaw_showWarning			*
 * B. Yin/SAIC		04/06	check the FZL top/bottom field for ICE. *
 * B. Yin/SAIC		06/06	return True if no panel2		*
 * B. Yin/SAIC		09/06	check IFR types 			*
 * B. Yin/SAIC		09/06	Free text	 			*
 * B. Yin/SAIC          06/07   check MT OBSC types                     *
 ***********************************************************************/
{
    int		ii;
    char	*label, *value = NULL, *text;

    XmString    xmStr;
/*---------------------------------------------------------------------*/

    if ( !_currentPanel2 ) return True;

/*
 *  Check if there is a top/bottom field
 */
    for ( ii = 0; ii < _currentPanel2->nInputField; ii++ ) {

	XtVaGetValues ( _currentPanel2->inputField[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &label );

	XtVaGetValues ( _currentPanel2->inputField[ ii ].text, 
			   XmNvalue, &value, NULL );

	if ( strncasecmp ( label, "top/bottom", 10 ) == 0 ) {
	
	   if ( !value || ( strlen ( value ) == (size_t)0 ) ) {

		XtFree ( value );
		XtFree ( label );
		XmStringFree ( xmStr );

                if ( showDlg ) pggfaw_showWarning ( _gfaForm ); 
		return False;
	   }
	}
	else if ( strncasecmp ( label, "fzl top/bottom", 14 ) == 0 ) {

	   XtVaGetValues ( _currentPanel2->inputField[ _FLIndex ].text,
			   XmNvalue, &text, NULL );

	   if ( ( !value || ( strlen ( value ) == (size_t)0 ) ) &&
	   	( strstr ( text, "FZL" ) ) ) {

		XtFree ( value );
	        XtFree ( text );
		XtFree ( label );
		XmStringFree ( xmStr );

                if ( showDlg ) pggfaw_showWarning ( _gfaForm ); 
		return False;
	   }
	   XtFree ( text );
	}
	XtFree ( value );
	XtFree ( label );
	XmStringFree( xmStr );
    }

    if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "IFR" ) == 0 ) {

       XtVaGetValues ( _currentPanel2->typeText, XmNvalue, &text, NULL );

       if ( strlen ( text ) == (size_t) 0 ) {

	  XtFree ( text );

          if ( showDlg ) pggfaw_showIfrWarning ( _gfaForm ); 

	  return False;
       }
       XtFree ( text );
    }
    else if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "MT_OBSC" ) == 0 ) {
       XtVaGetValues ( _currentPanel2->typeText, XmNvalue, &text, NULL );
       if ( strlen ( text ) == (size_t) 0 ) {
          XtFree ( text );
          if ( showDlg ) pggfaw_showMtobscWarning ( _gfaForm );
          return False;
       }
       XtFree ( text );
    }
    return True;
}

/*=====================================================================*/

static void pggfaw_showWarning ( Widget parent )
/************************************************************************
 * pggfaw_showWarning                                                   *
 *                                                                      *
 * This routine show a warning if top/bottom field is empty. 		*
 *                                                                      *
 * static void pggfaw_showWarning ( parent )		 	        *
 *                                                                      *
 * Input parameters:                                                    *
 * 	parent		Widget		parent widget	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		06/05	Created					*     
 * B. Yin/SAIC		06/05	Added FZL text field in the dialog.	*     
 * B. Yin/SAIC		09/06	Free warningBtn WidgetList.		*     
 ***********************************************************************/
{
    int			ii;
    char		*btnStr[] = { "OK", "Cancel" }, *flText;

    Boolean		enableFZL;
    static Widget	warningW = NULL, flightLevel, fzl, fzlLabel;
    static levels_t	levels;
    Widget		pane, msg, rowColFL, btnForm;
    WidgetList		warningBtn;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( _currentPanel2->inputField[ _FLIndex ].text, 
		   	XmNvalue, &flText, NULL );

    enableFZL = ( flText && strstr( flText, "FZL" ) );

    if ( warningW ) {

       XtVaSetValues ( flightLevel, XmNvalue, flText, NULL );
       XtVaSetValues ( fzl, XmNvalue, "", NULL );
       XtVaSetValues ( fzl, XmNsensitive, enableFZL, NULL );
       XtVaSetValues ( fzlLabel, XmNsensitive, enableFZL, NULL );

       XtUnmanageChild ( warningW );
       XtManageChild ( warningW );
       return;

    }
    
    warningW = XmCreateFormDialog ( parent, "pggfa_warning", NULL, 0);

    XtVaSetValues ( warningW,
		    XmNdefaultPosition, False,
		    XmNx,               200,
		    XmNy,               200,
		    XmNnoResize,        True,
		    XmNdialogStyle,     XmDIALOG_FULL_APPLICATION_MODAL,
		    NULL );

    XtVaSetValues ( XtParent ( warningW ),
		    XmNtitle,           "Error: Missing Flight Levels",
		    NULL );

/*
 *  Create a parent pane widget
 */
    pane = XtVaCreateManagedWidget ( "pggfa_wnpane",
                       xmPanedWindowWidgetClass, warningW,
		       XmNseparatorOn,		 False,
		       XmNmarginHeight,		 20,
		       XmNmarginWidth,		 0,
		       XmNspacing,		 15,
                       XmNsashWidth,             1,
                       XmNsashHeight,            1,
                       NULL );

    msg = XtVaCreateManagedWidget ( "Must supply flight level and/or freezing level information before drawing GFA.",
                       xmLabelGadgetClass, pane,
		       XmNmarginLeft,	   10,
		       XmNmarginRight,     10,
		       NULL );

    rowColFL = XtVaCreateManagedWidget ( "warningRowColFL",
   				xmRowColumnWidgetClass,	pane,
				XmNorientation,		XmHORIZONTAL,
                	  	XmNtopAttachment,	XmATTACH_WIDGET,
				XmNpacking,		XmPACK_COLUMN,
				XmNnumColumns,		2,
                	  	XmNtopWidget,		msg,
                	  	NULL );

/*
 *  Flight level widgets
 */
    XtVaCreateManagedWidget ( "Top/Bottom:",
                	  	xmLabelGadgetClass,	rowColFL,
				XmNmarginLeft,		110,
                	  	NULL );

    flightLevel = XtVaCreateManagedWidget ( "flightLevel",
                		xmTextWidgetClass,      rowColFL,
                		XmNmaxLength,           10,
                		XmNeditable,            TRUE,
                		XmNvalue,               flText,
				XmNwidth,		100,
                    		XmNrecomputeSize,       False,
                		NULL );

/*
 *  Freezing level widgets
 */
    fzlLabel = XtVaCreateManagedWidget ( "FZL Top/Bottom:",
                	  	xmLabelGadgetClass,	rowColFL,
				XmNmarginLeft,		110,
                	  	NULL );

    fzl = XtVaCreateManagedWidget ( "fzl",
                		xmTextWidgetClass,      rowColFL,
                		XmNmaxLength,           10,
                		XmNeditable,            TRUE,
                		XmNvalue,               "",
				XmNwidth,		100,
                    		XmNrecomputeSize,       False,
                		NULL );

    levels.flightLevel   = flightLevel;
    levels.fzlLabel      = fzlLabel;
    levels.fzlLevel      = fzl;

    XtAddCallback ( flightLevel, XmNlosingFocusCallback, 
    		   (XtCallbackProc)pggfaw_fillFLCb, (XtPointer) &levels );

    XtAddCallback ( flightLevel, XmNvalueChangedCallback, 
    		   (XtCallbackProc)pggfaw_changedFLCb, (XtPointer) &levels );

    XtAddCallback ( flightLevel, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc)pggfaw_verifyFLCb, NULL );

    XtAddCallback ( fzl, XmNlosingFocusCallback, 
    		   (XtCallbackProc)pggfaw_fillFzlCb, NULL );

    XtAddCallback ( fzl, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc)pggfaw_verifyFzlCb, NULL );

    XtVaSetValues ( fzl, XmNsensitive, enableFZL, NULL );
    XtVaSetValues ( fzlLabel, XmNsensitive, enableFZL, NULL );

    XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass, pane, NULL );

    btnForm = XtVaCreateManagedWidget ( "warningBtn",
		xmFormWidgetClass,		pane,
                XmNfractionBase,                2 * 50,
		NULL );

    warningBtn = (WidgetList)XtMalloc((size_t)2*sizeof(Widget));

    for ( ii = 0; ii < 2; ii++ ) {
	warningBtn[ ii ] = XtVaCreateManagedWidget ( btnStr[ ii ], 
		xmPushButtonWidgetClass,	btnForm, 
             	XmNleftAttachment,              XmATTACH_POSITION,
                XmNleftPosition,                ((ii * 50) + 10 ),
                XmNrightAttachment,             XmATTACH_POSITION,
                XmNrightPosition,		(((ii + 1) * 50) - 10),
		NULL );

	XtAddCallback ( warningBtn[ ii ], XmNactivateCallback,
		(XtCallbackProc)pggfaw_warningBtnCb, (XtPointer) &levels );

    }
    
   XtFree( (XtPointer) warningBtn );

   XtManageChild ( warningW );

   XtVaSetValues ( XtParent( warningW ),
	    XmNminWidth,	560,
	    XmNmaxWidth,	560,
	    XmNminHeight,	200,
	    XmNmaxHeight,	200,
	    NULL );

   XtFree ( flText );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_warningBtnCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_warningBtnCb							*
 *									*
 * Callback function for the control buttons in the warning window.	*
 *									*
 * static void pggfaw_warningBtnCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	cbs		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC          06/05   Created				        *
 * L. Hinson/AWC        09/05   Added _okOnWarning switch               *
 *                                to signal when OK clicked             *
 *                              Added _cancelOnWarning switch           *
 *                                to signal when Cancel clicked         *
 * E. Safford/SAIC	11/05	use size_t to avoid compiler warning	*
 * B. Yin/SAIC          04/06   check the fZL field in the dialog.      *
 * E. Safford/SAIC	05/06	relocate setting _okOnWarning to True	*
 ***********************************************************************/
{
    int		ii, ier;
    char	*btnLabel = NULL, *levelStr = NULL, *fzl = NULL;

    levels_t	*levels;
    Boolean	ice;
    XmString	xmStr;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &btnLabel );

    XmStringFree ( xmStr );

    ice = False;

    if ( strcasecmp ( btnLabel, "ok" ) == 0  ) {

	levels = (levels_t*)clnt;

	XtVaGetValues ( levels->flightLevel, XmNvalue, &levelStr, NULL );
	XtVaGetValues ( levels->fzlLevel, XmNvalue, &fzl, NULL );

	cst_lcuc ( levelStr, levelStr, &ier );

	ice = ( strcasecmp( _currentPanel2->haz, "ICE" ) == 0 ) && 
	      ( strstr( levelStr, "FZL" ) );

	if ( ( levelStr && ( strlen ( levelStr ) != ( size_t )0 ) ) &&
	     ( !ice || ( ice && fzl && ( strlen ( fzl ) != (size_t) 0 ) ) ) ) {

/*
 *  fill the top/bottom and the freezing level field
 */
    	   for ( ii = 0; ii < _currentPanel2->nInputField; ii++ ) {

		if ( ice && ( ii == _FZLIndex ) ) {

		   XtVaSetValues ( _currentPanel2->inputField[ ii ].text, 
		   			XmNvalue, fzl, NULL );

		}
		else if ( ii == _FLIndex ) {
	
		   XtVaSetValues ( _currentPanel2->inputField[ ii ].text, 
		   			XmNvalue, levelStr, NULL );

		}
	   }

           XtUnmanageChild ( XtParent( XtParent( XtParent( wid ) ) ) );
           _okOnWarning = True;
	}
    }
    else if (  strcasecmp ( btnLabel, "cancel" ) == 0  ) {

        XtUnmanageChild ( XtParent( XtParent( XtParent( wid ) ) ) );
        _cancelOnWarning = True;
    }
    XtFree ( btnLabel );
    XtFree ( levelStr );
    XtFree ( fzl );
}

/*=====================================================================*/

void pggfaw_setOkOnWarning( Boolean value )
/************************************************************************
 * pggfaw_setOkOnWarning  					       	*
 *									*
 *  Routine used for setting value of _okOnWarning static variable 	*
 *									*
 *  void pggfaw_setOkOnWarning( value )					*
 *									*
 *  Input parameters:							*
 *	value		Boolean		new value for _okOnWarning	*
 *									*
 *  Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 *  L. Hinson/AWC      09/05  Created					*
 ***********************************************************************/
{
    _okOnWarning = value;
}

/*=====================================================================*/

void pggfaw_setCancelOnWarning( Boolean value )
/************************************************************************
 * pggfaw_setCancelOnWarning  					       	*
 *									*
 *  Routine used for setting value of _CancelOnWarning static variable 	*
 *									*
 *  void pggfaw_setCancelOnWarning( value )				*
 *									*
 *  Input parameters:							*
 *	value		Boolean		new value for _CancelOnWarning	*
 *									*
 *  Output parameters:							*
 *			None						*
 **									*
 * Log:									*
 *  L. Hinson/AWC      09/05  Created					*
 ***********************************************************************/
{
    _cancelOnWarning = value;
}

/*=====================================================================*/

Boolean pggfaw_getOkOnWarning(void)
/************************************************************************
 * pggfaw_getOkOnWarning  					       	*
 *									*
 *  Routine used to get the value of _OkOnWarning static variable 	*
 *									*
 *  Boolean pggfaw_getOkOnWarning( )					*
 *									*
 *  Input parameters:							*
 *  Output parameters:							*
 *			None						*
 *  Return:								*
 *			Boolean		value of _OkOnWarning		*
 **									*
 * Log:									*
 *  L. Hinson/AWC      09/05  Created					*
 ***********************************************************************/
{
    return( _okOnWarning );
}

/*=====================================================================*/

Boolean pggfaw_getCancelOnWarning(void)
/************************************************************************
 * pggfaw_getCancelOnWarning  					       	*
 *									*
 *  Routine used to get the value of _CancelOnWarning static variable 	*
 *									*
 *  Boolean pggfaw_getCancelOnWarning( )				*
 *									*
 *  Input parameters:							*
 *  Output parameters:							*
 *			None						*
 *  Return:								*
 *			Boolean		value of _CancelOnWarning	*
 **									*
 * Log:									*
 *  L. Hinson/AWC      09/05  Created					*
 ***********************************************************************/
{
    return( _cancelOnWarning );
}

/*=====================================================================*/

Boolean pggfaw_isClosed ( void )
/************************************************************************
 * pggfaw_isClosed  					       		*
 *									*
 *  This routine checks if a FZLVL is closed or not.		 	*
 *									*
 *  Boolean pggfaw_isClosed( )						*
 *									*
 *  Input parameters:							*
 *  Output parameters:							*
 *			None						*
 *  Return:								*
 *			Boolean		True if closed			*
 **									*
 * Log:									*
 *  B. Yin/SAIC		12/05	Copied from Ed's code.			*
 *  B. Yin/SAIC		04/06	Removed the code to find current panel2.*
 ***********************************************************************/
{
    int         jj;
    char        *tmpStr, *valStr;
    Boolean     value = True;
    XmString    xmStr;
    Widget      wPulldown, wPushButton;
/*--------------------------------------------------------------------*/

    if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "FZLVL" ) != 0 ) {

	return True;

    }

    for ( jj = 0; jj < _currentPanel2->nDesc; jj++ ) {

        XtVaGetValues ( _currentPanel2->descStrc[ jj ]. label,
                        XmNlabelString, &xmStr, NULL );

        XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
        XmStringFree ( xmStr );

/*
 *  Look for the Contour manu and get its current selection
 */
        if ( strncasecmp ( tmpStr, "Contour", 7 ) == 0 ) {
           XtVaGetValues ( _currentPanel2->descStrc[ jj ].menu,
                           XmNsubMenuId, &wPulldown, NULL );
           XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

           XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &valStr );
           XmStringFree ( xmStr );

/*
 *  Open = False
 */
           if ( strcasecmp ( valStr, "Open" ) == 0 ) {
              value = False;
              XtFree ( valStr );
              break;
           }
           XtFree ( valStr );
        }
        XtFree ( tmpStr );
    }
    return ( value );
}

/*=====================================================================*/

static void pggfaw_setFromLinePb ( Boolean inFlag )
/************************************************************************
 * pggfaw_setFromLinePb					       		*
 *									*
 *  This routine checks if a FZLVL is closed or not.		 	*
 *									*
 *  Boolean pggfaw_setFromLinePb ( inFlag )				*
 *									*
 *  Input parameters:							*
 *	inFlag		Boolean		whether to enable fromline btn	*
 *  Output parameters:							*
 *			None						*
 *  Return:								*
 *			None						*
 **									*
 * Log:									*
 *  B. Yin/SAIC		12/05	Created 				*
 ***********************************************************************/
{
    Boolean     localFlag;
/*--------------------------------------------------------------------*/

    localFlag = inFlag;

    if ( !pggfaw_isClosed() ) localFlag = False;

    XtSetSensitive ( _fromLinePb, localFlag );

}

/*=====================================================================*/

void pggfaw_getLineWidth ( VG_DBStruct *el )
/************************************************************************
 * pggfaw_getLineWidth					       		*
 *									*
 *  This routine gets the line width from the settings table acccording *
 *  to the GFA subtype.						 	*
 *									*
 *  void pggfaw_getLineWidth ( el )					*
 *									*
 *  Input parameters:							*
 *	*el	VG_DBStruct	GFA element				*
 *									*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  B. Yin/SAIC		01/06	Created 				*
 *  B. Yin/SAIC		07/06	Use the color in the color picker	*
 ***********************************************************************/
{
    int		ier;
    char	subtype[ 32 ];
/*--------------------------------------------------------------------*/
/*
 *  Get GFA line width from the settings table
 */
    cvg_getFld ( el, TAG_GFA_SUBTYPE, subtype, &ier );
    ces_get( atoi( subtype ), el, &ier );

/*
 *  Use the color in the color picker if GFA GUI is up.
 *  (This routine is also called in smear GUI)
 */
    if ( pggfaw_isUp() ) {

       el->hdr.maj_col	= _attrColor;
       el->hdr.min_col	= _attrColor;	
    }
}

/*=====================================================================*/

static void pggfaw_initIssueType ( void ) 
/************************************************************************
 * pggfaw_initIssueType					       		*
 *									*
 *  This routine sets the issue type to NRML.  				*
 *									*
 *  static void pggfaw_initIssueType ( void )				*
 *									*
 *  Input parameters:							*
 *		None							*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  B. Yin/SAIC		01/06	Created 				*
 ***********************************************************************/
{
    long 	ii;
    char	*issueType;

    Boolean 	found = False;
    XmString	xmStr;
/*--------------------------------------------------------------------*/
    
    ii = 0;

    while ( !found && ii < _nIssopt ) {

        XtVaGetValues ( _issoptStrc.pb[ ii ], XmNlabelString, &xmStr, NULL );
        XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &issueType );

	if ( strcasecmp ( issueType, "nrml" ) == 0 ) {
		
	   found = True;

	}
	
        XmStringFree ( xmStr );
        XtFree ( issueType );

	ii++;
    }

    ii--;

    if ( !found ) ii = 0;

    XtVaSetValues ( _issoptStrc.menu, 
     		    XmNmenuHistory,	_issoptStrc.pb[ ii ], 
    		    NULL);
}

/*=====================================================================*/

static void pggfaw_setApplyBtn ( Boolean enable )
/************************************************************************
 * pggfaw_setApplyBtn					       		*
 *									*
 *  This routine enables/disables the apply button and also sets the 	*
 *  background color of the button.		 			*
 *									*
 *  static void pggfaw_setApplyBtn ( enable )				*
 *									*
 *  Input parameters:							*
 *	enable	Boolean		if True, enable the apply button	*
 *	                     	if False, disable the apply button	*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  B. Yin/SAIC		01/06	Created 				*
 ***********************************************************************/
{
    int 	ier;
    Pixel 	bgColor;
/*---------------------------------------------------------------------*/

    if ( enable ) {

       XtSetSensitive ( _ctlButtons[ 0 ], True );        

       xsncolr ( APPLY_COLOR, &bgColor, &ier );

       XtVaSetValues ( _ctlButtons[ 0 ], XmNbackground, bgColor, NULL );

    }
    else {

       XtSetSensitive ( _ctlButtons[ 0 ], False );        

       XtVaGetValues ( _ctlButtons[ 1 ], XmNbackground, &bgColor, NULL );

       XtVaSetValues ( _ctlButtons[ 0 ], XmNbackground, bgColor, NULL );

    }
}	
		
/*=====================================================================*/

static void pggfaw_initDueTo ( void ) 
/************************************************************************
 * pggfaw_initDueTo					       		*
 *									*
 *  This routine sets the DUE_TO menu to the empty string if there is 	*
 *  one.								*
 *									*
 *  static void pggfaw_initDueTo ( void )				*
 *									*
 *  Input parameters:							*
 *		None							*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  E. Safford/SAIC	01/06	Created 				*
 *  B. Yin/SAIC		02/06	Removed the debug print out		*
 ***********************************************************************/
{
    int 	ii, jj, kk;
    char	*labelString;

    Boolean 	foundBlank = False;
    XmString	xmStr = NULL;
/*--------------------------------------------------------------------*/
/*
 *  Return if it is not a turb.
 */
    if ( strcasecmp( _areaTyp[ _areaTypeIndex ], "TURB" ) != 0 && 
    	 strcasecmp( _areaTyp[ _areaTypeIndex ], "TURB-HI" ) != 0 &&
	 strcasecmp( _areaTyp[ _areaTypeIndex ], "TURB-LO" ) != 0 )  {
	 
	 return;

    }

/*
 *  Find the Panel-2 for the hazard
 */
    for ( ii = 0; ii < _nPanel2; ii++ ) {

 	if ( strcasecmp ( _panel2[ ii ].haz, _areaTyp[ _areaTypeIndex ] ) == 0 ) {
             
/*
 *  Find the 'Due To" option menu
 */
            for( jj = 0; jj < _panel2[ii].nDesc; jj++ ) {

                XtVaGetValues ( _panel2[ ii ].descStrc[ jj ].label,
		                XmNlabelString, &xmStr, NULL );
                XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &labelString );

        	XmStringFree( xmStr );

	  	if ( strcasecmp ( labelString, "DUE_TO:" ) == 0 ) {

		   XtFree( labelString );   

/*
 *  Find the empty string for the 'due to' menu
 */
		   for ( kk = 0; kk < MAXNOPT; kk++ ) {

              		XtVaGetValues ( _panel2[ ii ].descStrc[ jj ].pb[kk],
		        		   XmNlabelString, &xmStr, NULL );

                	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &labelString );

			XmStringFree( xmStr );

			if ( strlen ( labelString ) == (size_t)0 ) {

				XtFree( labelString );
				foundBlank = True;
				break;
			}
			XtFree( labelString );
		   }
		   break;
		}
		XtFree( labelString );   
	    }
	    break;
	}
    }

    if ( foundBlank )  {

       XtVaSetValues ( _panel2[ ii ].descStrc[ jj ].menu, 
      		       XmNmenuHistory,	_panel2[ ii ].descStrc[ jj ].pb[kk], 
    		       NULL );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_popupTypeDlgCb ( Widget wid, XtPointer which, 
							XtPointer cbs )
/************************************************************************
 * pggfaw_popupTypeDlgCb				       		*
 *									*
 *  This routine is the callback for the push button(s) in the second 	*
 *  panel. It pops up/down the IFR type edit dialog window associated 	*
 *  with the button.							*
 *									*
 *  static void pggfaw_popupTypeDlgCb ( wid, which, cbs )		*
 *									*
 *  Input parameters:							*
 *	wid	Widget		the push button				*
 *	which	XtPointer	the dialog window			*
 *	cbs	XtPointer	callback structure (not used)		*
 *									*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  B. Yin/SAIC		03/06	Created 				*
 ***********************************************************************/
{
    Position	xPos, yPos, rootx, rooty;
    Widget 	dlg;
/*---------------------------------------------------------------------*/

    dlg = (Widget)which;

    if ( !XtIsManaged ( dlg ) ) {
	
       /*
        *  Position the dialog window below the push button.
	*/
       XtVaGetValues ( wid, XmNx, &xPos, XmNy, &yPos, NULL );
       XtTranslateCoords ( wid, xPos, yPos, &rootx, &rooty );

       XtManageChild ( dlg );
       XtVaSetValues ( dlg, 
       		       XmNx, rootx, 
		       XmNy, rooty + 60, 
		       NULL );

       _curTypeDlg = dlg;

    }
    else {
	XtUnmanageChild ( dlg );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_closeTypeDlgCb ( Widget wid, XtPointer which, 
							XtPointer cbs )
/************************************************************************
 * pggfaw_closeTypeDlgCb				       		*
 *									*
 *  This routine is the callback for the close button in the IFR type	*
 *  edit dialog window. It pops down the dialog window.		 	*
 *									*
 *  static void pggfaw_closeTypeDlgCb ( wid, which, cbs )		*
 *									*
 *  Input parameters:							*
 *	wid	Widget		the close button			*
 *	which	XtPointer	the dialog window			*
 *	cbs	XtPointer	callback structure (not used)		*
 *									*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  B. Yin/SAIC		03/06	Created 				*
 ***********************************************************************/
{
    Widget dlg;
/*---------------------------------------------------------------------*/

    dlg = (Widget)which;
    XtUnmanageChild ( dlg );
}

/*=====================================================================*/

/* ARGSUSED */
static void pggfaw_setTypeTextCb ( Widget wid, XtPointer clnt, 
						XtPointer cbs )
/************************************************************************
 * pggfaw_setTypeTextCb                                                 *
 *                                                                      *
 *  This routine is the callback for the toggle buttons in the IFR or   *
 *  MT OBSC type edit dialog window and the checkbox in the 2nd panel.  *
 *  It sets the contents for the text field in the 2nd panel.           *
 *                                                                      *
 *  static void pggfaw_setTypeTextCb ( wid, clnt, cbs )                 *
 *                                                                      *
 *  Input parameters:                                                   *
 *      wid     Widget          the toggle button                       *
 *      clnt    XtPointer       client data (not used )                 *
 *      cbs     XtPointer       callback structure (not used)           *
 *                                                                      *
 *  Output parameters:                                                  *
 *              None                                                    *
 *  Return:                                                             *
 *              None                                                    *
 **                                                                     *
 * Log:                                                                 *
 *  B. Yin/SAIC         03/06   Created                                 *
 *  B. Yin/SAIC         04/06   Removed the code to find current panel2.*
 *  E. Safford/SAIC     06/06   use " " not "/" between CIG & VIS       *
 *  E. Safford/SAIC     07/06   use ctb_gfaCombineIFRTypes to format    *
 *                                CIG/VIS string                        *
 *  B. Yin/SAIC         09/06   set the type field in IFR warning window*
 *  B. Yin/SAIC         09/06   check if the text field is created      *
 *  B. Yin/SAIC         06/07   add code for MT OBSC type               *
 *  J. Wu/SAIC          06/08   set typeText only if popups exist       *
 ***********************************************************************/
{
    int         ii, jj, ier;
    char        typeStr[ STD_STRLEN ], *tmpStr;
    char        outTypeStr[ STD_STRLEN ];
                                                                                                                 
    XmString    xmStr;
    Boolean     labelFlag = False, ifr, mtobsc;
/*---------------------------------------------------------------------*/
/*
 *  Enable the 'Apply' button.
 */
    pggfaw_setApplyBtn( True );
    if ( _currentPanel2 == NULL ) return;
    typeStr[ 0 ]= '\0';

/*
 *  Loop over all checkboxes in panel2. If it is checked, put its
 *  label in the text string.
 */
    for ( ii = 0; ii < _currentPanel2->nCheckbox; ii++ ) {
        if (  XmToggleButtonGetState ( _currentPanel2->checkbox[ ii ] ) ) {
           XtVaGetValues ( _currentPanel2->checkboxLabel[ ii ], XmNlabelString,
                           &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
           strcat ( typeStr, tmpStr );
           XtFree ( tmpStr );
           XmStringFree ( xmStr );
        }
    }
    ifr = False;
    mtobsc = False;
    if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "IFR" ) == 0 ) {
        ifr = True;
    }
    else if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "MT_OBSC" ) == 0 ) {
        mtobsc = True;
    }

/*
 *  Loop over all popup dialog windows.
 */
    for ( ii = 0; ii < _currentPanel2->nPopup; ii++ ) {

/*
 *   Loop over all toggle buttons in the dialog window.
 *   If any of the buttons is/are checked, put the label of
 *   the dialog and labels of those buttons in the text string.
 */
        for ( jj = 0; jj < _currentPanel2->popup[ ii ].nBtns; jj++ ) {
            if ( XmToggleButtonGetState ( _currentPanel2->popup[ ii ].btns[ jj ] ) ) {
                if ( !labelFlag ) {
                   XtVaGetValues ( _currentPanel2->popupLabel[ ii ], XmNlabelString,
                                   &xmStr, NULL );
                   XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
                   strcat ( typeStr, tmpStr );
                   XtFree ( tmpStr );
                   XmStringFree ( xmStr );
                   if ( !ifr && typeStr[ strlen(typeStr) - 1 ] == ':' ) {
                        typeStr[ strlen(typeStr) - 1 ] = ' ';
                   }
                   labelFlag = True;
                }
                XtVaGetValues ( _currentPanel2->popup[ ii ].btns[ jj ], XmNlabelString,
                                &xmStr, NULL );
                XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
                strcat ( typeStr, tmpStr );
                if ( !ifr ) {
                        strcat ( typeStr, "/" );
                }
                XtFree ( tmpStr );
                XmStringFree ( xmStr );
            }
        }
    }
    if ( ifr ) {
       ctb_gfaCombineIFRTypes( typeStr, NULL, outTypeStr, &ier );
    }
    else {
        if ( strlen(typeStr) > 1 ) typeStr[ strlen(typeStr) -1 ] = '\0';
        strcpy( outTypeStr, typeStr );
    }

/*
 *  Set the text field content
 */
    if (  _currentPanel2->nPopup > 0 ) {
        XtVaSetValues ( _currentPanel2->typeText, XmNvalue, outTypeStr, NULL );
    }

    if ( ifr && _typeTextW && XtIsManaged ( _typeTextW ) ) {
       XtVaSetValues ( _typeTextW, XmNvalue, outTypeStr, NULL );
    }
    else if ( mtobsc && _mtobscTextW && XtIsManaged( _mtobscTextW ) ) {
       XtVaSetValues ( _mtobscTextW, XmNvalue, outTypeStr, NULL );
    }
}

/*=====================================================================*/

static void pggfaw_setFzl ( Boolean value )
/************************************************************************
 * pggfaw_setFzl 	                                                *
 *                                                                      *
 * This routine enables/disables the freezing level input filed.    	*
 *                                                                      *
 * void pggfaw_setFzl ( value ) 			        	*
 *                                                                      *
 * Input parameters:                                                    *
 *	value		Boolean		if true, enable the input field.*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/06	Created					*
 ***********************************************************************/
{

    if ( !value ) {

       XtVaSetValues ( _currentPanel2->inputField[ _FZLIndex ].text, 
       			XmNvalue, "", NULL );

    }

    XtVaSetValues ( _currentPanel2->inputField[ _FZLIndex ].label, 
    			XmNsensitive, value, NULL );
			
    XtVaSetValues ( _currentPanel2->inputField[ _FZLIndex ].text, 
    			XmNsensitive, value, NULL );

}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_verifyFzlCb ( Widget wid, XtPointer clnt, 
							XtPointer call )
/************************************************************************
 * pggfaw_verifyFzlCb                                                   *
 *                                                                      *
 * This routine checks the validation of top/bottom freezing levels.    *
 *                                                                      *
 * void pggfaw_verifyFzlCb ( wid, clnt, call ) 	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid	Widget	  fzl top/bottom text field widget		*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	call	XtPointer callback structure				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		04/06	Created					*
 ***********************************************************************/
{
    size_t	ii;
    char	*text, newText[ 8 ], *strPtr;

    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL ) {
	return;
    }

/*
 *  Get the current contents of the top/bottom widget 
 */
    XtVaGetValues ( wid, XmNvalue, &text, NULL );

/*
 *  Check the validation
 */
    if ( ( cbs->text->length + (int)strlen ( text ) - 
         ( cbs->endPos - cbs->startPos ) ) > 7 ) {
       
       cbs->doit = False;

    }
    else  {

/*
 *  Construct the resulting text
 */
       newText [ 0 ] = '\0';
       strncat ( newText, text, cbs->startPos );	
       strcat ( newText, cbs->text->ptr );
       strcat ( newText, &text[ cbs->endPos ] );

       if ( strlen ( newText ) > (size_t)0 ) {

/*
 *  No spaces are allowed
 */
          if ( strchr ( newText, ' ' ) ) {

	     cbs->doit = False;

          }
          else if ( isdigit ( newText[ 0 ] ) == 0 ) { /* first chr is not digit */

	     cbs->doit = False;

	  }
	  else if ( strchr ( newText, '/' ) != strrchr ( newText, '/' ) ) { /* more than 1 '/' */

	     cbs->doit = False;

     	  }
          else { 
          
/* 
 *  Check first column
 */
	     strPtr = strtok ( newText, "/" );

             if ( strlen ( strPtr ) > (size_t)3 ) {

		cbs->doit = False;

	     }

	     else {

		for ( ii = 1; ii < strlen ( strPtr ); ii++ ) {
		    
		    if ( isdigit ( strPtr[ ii ] ) == 0 ) cbs->doit = False;
		}
	     }

/*
 *  Check second column
 */
	     if ( ( strPtr = strtok ( NULL, "/" ) ) ) {

		if ( strlen ( strPtr ) > (size_t)3 ) {
		      
	           cbs->doit = False;
		}
		else if ( !( strcasecmp ( strPtr, "s" )   == 0 ||
			     strcasecmp ( strPtr, "sf" )  == 0 ||
			     strcasecmp ( strPtr, "sfc" ) == 0 ) ) {

		   for ( ii = 0; ii < strlen ( strPtr ); ii++ ) {
				    
		         if ( isdigit ( strPtr[ ii ] ) == 0 ) cbs->doit = False;
		   }
		}
	     }
	  }
       }
    }
    XtFree ( text );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_fillFzlCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_fillFzlCb							*
 *									*
 * Callback function is used to fill the freezing levels.		*
 *									*
 * static void pggfaw_fillFzlCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data, not used		*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/06		Created				*
 ***********************************************************************/
{
    char	*ptext = NULL, *pstr, tmpStr[ 8 ];
    char	topStr[ 10 ], bottomStr[ 10 ];
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );

    if ( ptext && strlen ( ptext ) > (size_t)0 ) {

        if ( ( pstr = strtok ( ptext, "/" ) ) ) {

	    cst_padString ( pstr, '0', 1, 3, topStr ); 

	}

        if ( ( pstr = strtok ( NULL, "/" ) ) ) {
  
 	   if ( ( strcasecmp ( pstr, "s" )   == 0 ) ||
	        ( strcasecmp ( pstr, "sf" )  == 0 ) ||		
	        ( strcasecmp ( pstr, "sfc" ) == 0 ) ) {
	      strcpy ( bottomStr, "SFC" );
	   }
	   else {
              cst_padString ( pstr, '0', 1, 3, bottomStr );
	   }

 	}
	else {
	     strcpy ( bottomStr, "SFC" );
	}
    }    

    else {
	strcpy ( topStr,    "" );
        strcpy ( bottomStr, "" );
    }

    sprintf ( tmpStr, "%s/%s", topStr, bottomStr );

    if ( strlen ( tmpStr ) > (size_t)1 ) {
       XtVaSetValues ( wid, XmNvalue, tmpStr, NULL );
    }
    XtFree ( ptext );
}

/*=====================================================================*/

static void pggfaw_changedFLCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_changedCb							*
 *									*
 * Callback function is used to enable/disable FZL top/bottom for ICE.	*
 *									*
 * static void pggfaw_changedFLCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data			*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/06		Created				*
 ***********************************************************************/
{
    int		ier;
    char	*text;

    levels_t	*levels;
    Boolean	fzlOn;
/*---------------------------------------------------------------------*/

    if ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) {

       XtVaGetValues ( wid, XmNvalue, &text, NULL );

       cst_lcuc ( text, text, &ier );

       fzlOn = strstr ( text, "FZL" ) ? True : False;

       if ( !fzlOn ) XtVaSetValues ( _currentPanel2->inputField[ _FZLIndex ].text, 
       					XmNvalue, "", NULL );

       XtVaSetValues ( _currentPanel2->inputField[ _FZLIndex ].label, 
    			  XmNsensitive, fzlOn, NULL );
			
       XtVaSetValues ( _currentPanel2->inputField[ _FZLIndex ].text, 
    			  XmNsensitive, fzlOn, NULL );

       if ( clnt ) {

	  levels = (levels_t*) clnt;

          if ( !fzlOn ) XtVaSetValues ( levels->fzlLevel, XmNvalue, "", NULL );

          XtVaSetValues ( levels->fzlLabel, XmNsensitive, fzlOn, NULL );
			
          XtVaSetValues ( levels->fzlLevel, XmNsensitive, fzlOn, NULL );

       }
       XtFree ( text );
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_addFcstHrColon ( Widget wid, XtPointer clnt, 
				XtPointer call )
/************************************************************************
 * pggfaw_addFcstHrColon                                          	*
 *                                                                      *
 * Callback function for the optional forecast hour text. It checks the	*
 * current content of _fcsthrText and automatically adds a colon (':')	*
 * to it if _fcsthrText contains only 1 digit.                		*
 *                                                                      *
 * static void pggfaw_addFcstHrColon ( wid, clnt, call )       		*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   clnt    		XtPointer 	Widget's event data (not used)	*
 *   call       	XtPointer 	Callback structure          	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           05/06   initial coding                          *
 ***********************************************************************/
{
    char    *ptext = NULL;    
/*---------------------------------------------------------------------*/
    
    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );
        
    if ( ptext ) {
	
	if ( strlen ( _tmpFcsthrStr ) == (size_t)0 &&
	     strlen( ptext ) == (size_t)1 ) {
	
	    strcpy ( _fcsthrStr, ptext );	
	    strcat ( _fcsthrStr, ":" ); 
           
	    XtVaSetValues ( wid,
	    		XmNvalue,		_fcsthrStr, 
			XmNcursorPosition,	2,
			NULL );	        
 	        
	    _tmpFcsthrStr[ 0 ] = '\0';
	
	}
	XtFree ( ptext );
    }
}

/*=====================================================================*/

static int pggfaw_getNextTag ( void )
/************************************************************************
 * pggfaw_getnextTag    						*
 *									*
 * This function returns the next available tag number for the current  *
 * desk.								*
 *									*
 * static int pggfaw_getNextTag( )                                 	*
 *									*
 * Input parameters:							*
 *	None                                              		*
 *									*
 * Return parameters:							*
 *	int	Next available tag number.	 			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 ***********************************************************************/
{
    int		ii, jj, ier, *tags, ntags;
    long	fileSize;
    char        vgfname[ 128 ];

    FILE        *fptr;
/*---------------------------------------------------------------------*/

    cvg_open ( cvg_getworkfile(), 0, &fptr, &ier );
    if ( ier < 0 ) return 1;

    cfl_inqr ( cvg_getworkfile(), NULL, &fileSize, vgfname, &ier );
    if ( ier < 0 ) return 1;

    tags  = NULL;
    ntags = 0;

    pggfaw_getTagsForDesk ( vgfname, fptr, _desks[ _deskStrc.current ], 
    			    _areaTyp[ _areaTypStrc.current ], &tags, &ntags );

    for ( ii = 1; ii < ntags + 1; ii++ ) {

	for ( jj = 1; jj < ntags + 1; jj++ ) {

	    if ( ii == tags [jj - 1] ) break;

	}

	if ( jj == ( ntags + 1 ) ) break;

    }
    
    cvg_clos ( fptr, &ier );
    G_FREE ( tags, int );

    return ii;
}

/*=====================================================================*/

/* ARGSUSED */
static void pggfaw_deskCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pggfaw_deskCb							*
 *									*
 * Callback function for the desk menu.					*
 *									*
 * static void pggfaw_deskCb ( wid, which, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which desk			*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06		Created				*
 ***********************************************************************/
{
   int nn, mm;
/*---------------------------------------------------------------------*/

   for ( nn = 0; nn < _nDesks; nn++ ) {

       for ( mm = 0; mm < _nAreatyp; mm++ ) {

	   if ( nn == which && mm == _areaTypStrc.current ) {

	      XtManageChild (  _tagStrc[ nn ][ mm ].form );
	      _deskStrc.current = nn;
	   }
	   else {
	      XtUnmanageChild ( _tagStrc[ nn ][ mm ].form );
	   }
       }
   }
   pggfaw_setDfltTag( _fcsthr[ _currFcsthr ] );
   pggfaw_setApplyBtn( True );
}

/*=====================================================================*/

/* ARGSUSED */
static void pggfaw_tagCb ( Widget wid, long which, XtPointer cbs )
/************************************************************************
 * pggfaw_tagCb								*
 *									*
 * Callback function for the tag number menu.				*
 *									*
 * static void pggfaw_tagCb ( wid, which, cbs )				*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which tag			*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06		Created				*
 ***********************************************************************/
{
   _tagStrc[ _deskStrc.current ][ _areaTypStrc.current ].current = (int)which;
   pggfaw_setApplyBtn( True );
}

/*=====================================================================*/

static void pggfaw_addTag( int tag )
/************************************************************************
 * pggfaw_addTag							*
 *									*
 * This function puts the input tag into the tag menu if it is not there*
 * .									*
 *									*
 * void pggfaw_addTag ( tag )						*
 *									*
 * Input parameters:							*
 *	tag	int  	tag number 					*
 *									*
 * Output parameters:							*
 *	None                                    			*
 *									*
 **						 			*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 * B. Yin/SAIC		10/06	Set the current tag index.             	*
 ***********************************************************************/
{
    int  ii, jj, curDesk, curHaz, pos, ier;
    char tmpStr[ 32 ], tagStr[ 32 ];
/*---------------------------------------------------------------------*/

    curDesk = _deskStrc.current;
    curHaz  = _areaTypStrc.current;
    _tagStrc[ curDesk ][ curHaz].current = tag;
    

    sprintf( tmpStr, "%d", tag );

/*
 *  Set tag
 */
    for ( ii = 0; ii < _nTags[ curDesk ][ curHaz ]; ii++ ) {

	if ( strchr( _tags[ curDesk ][ curHaz ][ ii ], '*' ) ) {

              cst_rmst ( _tags[ curDesk ][ curHaz ][ ii ], "*", &pos,
                         tagStr, &ier );
	}
	else {
	     strcpy ( tagStr,  _tags[ curDesk ][ curHaz ][ ii ] );
	}

	if ( strcmp ( tmpStr, tagStr ) == 0 ) {

	   if ( !strchr( _tags[ curDesk ][ curHaz ][ ii ], '*' ) ) {

	      strcat ( _tags[ curDesk ][ curHaz ][ ii ], "*" );

              pgutls_setOptMenu ( _tags[ curDesk ][ curHaz ][ ii ], 
    				  _tags[ curDesk ][ curHaz ], 
				  _nTags[ curDesk ][ curHaz ],
				  &_tagStrc[ curDesk ][ curHaz ] );
	   }
	   return;
	}
    }

/*
 *  If the tag value is not in the menu, add the tag into the menu
 */
    for ( ii = 1; ii < _nTags[ curDesk ][ curHaz ]; ii++ ) {

	if ( atoi( _tags[ curDesk ][ curHaz ][ ii ] ) > tag ) {

	   for ( jj = _nTags[ curDesk ][ curHaz ]; jj >= ii; jj-- ) {

		strcpy( _tags[ curDesk ][ curHaz ][ jj  + 1], 
			_tags[ curDesk ][ curHaz ][ jj ] );

	   }

           strcpy ( _tags[ curDesk ][ curHaz ][ ii ], tmpStr );
           strcat ( _tags[ curDesk ][ curHaz ][ ii ], "*" );
	   break;
	}
    }

    if ( ii == _nTags[ curDesk ][ curHaz ] ) {

       strcpy ( _tags[ curDesk ][ curHaz ][ _nTags[ curDesk ][ curHaz ] ], tmpStr );
       strcat ( _tags[ curDesk ][ curHaz ][ _nTags[ curDesk ][ curHaz ] ], "*" );
     
    }

    _nTags[ curDesk ][ curHaz ]++;
    pgutls_setOptMenu ( _tags[ curDesk ][ curHaz ][ ii ], 
    			_tags[ curDesk ][ curHaz ], 
			_nTags[ curDesk ][ curHaz ],
			&_tagStrc[ curDesk ][ curHaz ] );
}

/*=====================================================================*/

static void pggfaw_setTagMenu( char tag[] )
/************************************************************************
 * pggfaw_setTagMenu							*
 *									*
 * This function sets the current desk(region) and the current tag      *
 * number using the input tag string(tag# and desk).                    *
 *									*
 * void pggfaw_setTagMenu ( tag )					*
 *									*
 * Input parameters:							*
 *	tag[]	char	tag number and desk(region)			*
 *									*
 * Output parameters:							*
 *	None                                    			*
 *									*
 **						 			*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 ***********************************************************************/
{
    int   	ii, curDesk, curHaz;
    char  	tagNum[ 8 ], desk[ 8 ], tagSt[ 8 ];
/*---------------------------------------------------------------------*/

    ii = 0;
    while ( ii <= (int)strlen( tag ) && isdigit( tag[ ii ] ) ) ii++;
 
    strncpy( tagNum, tag, ii );
    tagNum[ ii ] = '\0';

    sprintf( tagSt, "%s%s", tagNum, "*" );

    strcpy( desk, &tag[ ii ] );

    for ( ii = 0; ii < _nDesks; ii++ ) {

        if ( strcasecmp ( desk, _desks[ ii ] ) == 0 ) {

	    _deskStrc.current = ii;
  
	}
    }
	
    curDesk = _deskStrc.current;
    curHaz = _areaTypStrc.current;

    pggfaw_deskCb ( NULL, curDesk, NULL );
    pgutls_setOptMenu ( _desks[ curDesk ], _desks, _nDesks, &_deskStrc );      

    for ( ii = 0; ii < _nTags[ curDesk ][ curHaz ]; ii++ ) {

	if ( ( strcasecmp( tagNum, _tags[ curDesk ][ curHaz ][ ii ] ) == 0 ) ||
	     ( strcasecmp( tagSt, _tags[ curDesk ][ curHaz ][ ii ] ) == 0 ) ) {

             pgutls_setOptMenu ( _tags[ curDesk ][ curHaz ][ ii ], 
	     			 _tags[ curDesk ][ curHaz ], 
    			         _nTags[ curDesk ][ curHaz ], 
				 &_tagStrc[ curDesk ][ curHaz ] );

	     _tagStrc[ curDesk ][ curHaz ].current = ii;
	   
	}
    }
}

/*=====================================================================*/

static void pggfaw_setAllTags ( void )
/************************************************************************
 * pggfaw_setAllTags							*
 *									*
 * This function goes through all GFA elements in the work file and     *
 * puts all tags in use into the tag menu. It also sets the default tag.*
 *									*
 * void pggfaw_setAllTags ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	None                                    			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 * B. Yin/SAIC		09/06	Use the last touched tag as default.   	*
 ***********************************************************************/
{
    int 	ii, jj, kk, ier, ntags, *tags, lastTagIndx;
    long	fileSize;
    char        vgfname[ 128 ], lastTagStr[ 8 ];
   
    FILE        *fptr;
/*---------------------------------------------------------------------*/
/* 
 *  Open the work file.
 */
    cvg_open ( cvg_getworkfile(), 0, &fptr, &ier );
    if ( ier < 0 ) return;

    cfl_inqr ( cvg_getworkfile(), NULL, &fileSize, vgfname, &ier );
    if ( ier < 0 ) return;

    for ( ii = 0; ii < _nDesks; ii++ ) {

	for ( jj = 0; jj < _nAreatyp; jj++ ) {

/*
 *  Save last touched tag.
 */
	    strcpy( lastTagStr, _tags[ ii ][ jj ][ _tagStrc[ ii ][ jj ].current ] );

/*
 *  Remove the * if there is one.
 */
	    if ( lastTagStr[ strlen( lastTagStr ) - 1 ] == '*' ) {

		lastTagStr[ strlen( lastTagStr ) - 1 ] = '\0';

	    }

	    ntags = 0;
	    tags  = NULL;

            pggfaw_getTagsForDesk ( vgfname, fptr, _desks[ ii ], 
				    _areaTyp[ jj ], &tags, &ntags );

	    for ( kk = 0; kk < ntags; kk++ ) {

	        sprintf( _tags[ ii ][ jj ][ kk + 1 ], 
		         "%d", tags[ kk ] );

	    }

	    _nTags[ ii ][ jj ] = ntags + 1;

/* 
 *  The last touched GFA element may have been deleted,
 *  so we need to check if the last touched tag is still in use.
 *  If the last touched GFA is deleted, set TAG menu to NEW.
 */
	     lastTagIndx = 0;
	     for ( kk = 0; kk < _nTags[ ii ][ jj ]; kk++ ) {

		 if ( strcmp( _tags[ ii ][ jj ][ kk ], lastTagStr ) == 0 ) {

			lastTagIndx = kk;
			break;

		 }
	     }

            pgutls_setOptMenu ( _tags[ ii ][ jj ][ lastTagIndx ],
                                _tags[ ii ][ jj ],
                                _nTags[ ii ][ jj ],
                                &_tagStrc[ ii ][ jj ] );

            _tagStrc[ ii ][ jj ].current = lastTagIndx;

            G_FREE ( tags, int );
	}
    }
    cvg_clos ( fptr, &ier );
    pggfaw_setDfltTag( _fcsthr[ _currFcsthr ] );
}

/*=====================================================================*/

static void pggfaw_setDfltTag( char fcsthr[] )
/************************************************************************
 * pggfaw_setDfltTag							*
 *									*
 * This function sets the default tag for the input forecast hour. If   *
 * the forecast hour is 0, the tag will be set as New. In any other     *
 * cases, the tag will be set as the last in the tag array. It also	*
 * appends an asterisk for the tags in use for the forecast hour.	*
 *									*
 * void pggfaw_setDfltTag ( fcsthr )					*
 *									*
 * Input parameters:							*
 *	fcsthr[]	char	forecast hour.				*
 *									*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 * B. Yin/SAIC		09/06	Use the last touched tag as default.   	*
 ***********************************************************************/
{
    int 	ii, ier, curHaz, curDesk, pos;
    long	fileSize;
    char        vgfname[ 128 ], tag[ 32 ];
   
    FILE        *fptr;
/*---------------------------------------------------------------------*/

    curDesk = _deskStrc.current;
    curHaz = _areaTypStrc.current;

/* 
 *  Open the work file.
 */
    cvg_open ( cvg_getworkfile(), 0, &fptr, &ier );
    if ( ier < 0 ) return;

    cfl_inqr ( cvg_getworkfile(), NULL, &fileSize, vgfname, &ier );
    if ( ier < 0 ) return;

    for ( ii = 1; ii < _nTags[ curDesk ][ curHaz ]; ii++ ) {

	if ( strchr( _tags[ curDesk ][ curHaz ][ ii ], '*' ) ) {

           cst_rmst ( _tags[ curDesk ][ curHaz ][ ii ], "*", &pos,
                      _tags[ curDesk ][ curHaz ][ ii ], &ier );

	}

	sprintf( tag, "%s%s", _tags[ curDesk ][ curHaz ][ ii ], _desks[ curDesk ] );

        if ( pggfaw_tagInUse( vgfname, fptr, tag, fcsthr ) ) {

	   strcat ( _tags[ curDesk ][ curHaz ][ ii ], "*" );

	}
    }

    if ( strcmp( fcsthr, "0" ) == 0 ) {

       pgutls_setOptMenu ( _tags[ curDesk ][ curHaz ][ 0 ], 
       			   _tags[ curDesk ][ curHaz ], 
    			   _nTags[ curDesk ][ curHaz ], 
			   &_tagStrc[ curDesk ][ curHaz ] );   

       _tagStrc[ curDesk ][ curHaz ].current = 0;

    }
    else {

       pgutls_setOptMenu ( _tags[ curDesk ][ curHaz ][ _tagStrc[ curDesk ][ curHaz ].current ],
       			   _tags[ curDesk ][ curHaz ], 
    			   _nTags[ curDesk ][ curHaz ], 
			   &_tagStrc[ curDesk ][ curHaz ] );
    }
    cvg_clos ( fptr, &ier );
}

/*=====================================================================*/

static Boolean pggfaw_tagInUse( char vgfname[], FILE *fptr, char tag[], 
				char fcsthr[] )
/************************************************************************
 * pggfaw_tagInUse							*
 *									*
 * This function takes a tag string as input and checks if the tag is in*
 * use for the same forecast hour.                                      *
 *									*
 * void pggfaw_tagInUse ( vgfname, fptr, tag, fcsthr )			*
 *									*
 * Input parameters:							*
 *	vgfname[]	char	vg file name.				*
 *	*fptr		FILE	FILE pointer to the vg file.		*
 *	tags[]		char	tag string.				*
 *	fcsthr[]	char	forecast hour.				*
 *									*
 * Return parameters:							*
 *	True:	The input tag is in use for the forecast hour.		*
 *	False:	The input tag is NOT in use for the forecast hour.	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 ***********************************************************************/
{
    int 	ier, curHaz, nextEl;
    long	curPos;
    char        tagStr[ 32 ], hrStr[ 32 ], hazStr[ 32 ];

    VG_DBStruct	el;
/*---------------------------------------------------------------------*/
/*
 *  Loop over all elements in the work file. If the tag number is
 *  used by another element for the same desk, do nothing.
 */
    nextEl = 0;
    curPos = 0;
    curHaz = _areaTypStrc.current;

    while ( nextEl < MAX_EDITABLE_ELEMS )  {

       cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

       if ( ier < 0 ) break;

       if ( el.hdr.recsz > 0 )  {
                                                 
          curPos += el.hdr.recsz;
                                                                        
          if ( (int)el.hdr.vg_type == GFA_ELM &&
               !el.hdr.delete )  {
                                                                       
    	     cvg_getFld ( &el, TAG_GFA_FCSTHR, hrStr, &ier );
    	     cvg_getFld ( &el, TAG_GFA_AREATYPE, hazStr, &ier );
             cvg_getFld ( &el, TAG_GFA_TAG, tagStr, &ier );

	     if ( ( strcasecmp ( hazStr, _areaTyp[ curHaz ] ) == 0 ) &&
                  ( strcasecmp ( tagStr, tag ) == 0 ) ) {
		
                cvg_freeElPtr ( &el );

		if ( !fcsthr ) return True;
		else if ( strcasecmp( hrStr, fcsthr ) == 0 ) return True;
	     }
	  }
       }
       cvg_freeElPtr ( &el );
       nextEl++;
    }
    return False;
}

/*=====================================================================*/

static void pggfaw_getTagsForDesk ( char vgfname[], FILE *fptr, char desk[], 
				    char haz[], int **tags, int *ntags )
/************************************************************************
 * pggfaw_getTagsForDesk						*
 *									*
 * This function gets all tags in use for a desk(region) and a hazard	*
 * in the input vg file. Variable tags should be freed by the calling 	*
 * routine.								*
 *									*
 * void pggfaw_getTagsForDesk ( vgfname, fptr, desk, tags, ntags )	*
 *									*
 * Input parameters:							*
 *	vgfname[]	char	vg file name.				*
 *	*fptr		FILE	FILE pointer to the vg file.		*
 *	desk[]		char	desk(region) name.			*
 *	haz[]		char	hazard type.				*
 *									*
 * Output parameters:							*
 *	**tags		int	tags in use.				*
 *	*ntags		int	number of tags in use.			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06	Created.                         	*
 ***********************************************************************/
{
    int		ii, jj, tmp, ier, nextEl, tagNum;
    long	curPos;
    char        tagStr[ 32 ], hazStr[ 32 ];

    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    nextEl = 0;
    curPos = 0;
    *ntags = 0;
    *tags  = NULL;

/*
 *  Loop through all elements in the vg file and get all GFA tags in use.
 */
    while ( nextEl < MAX_EDITABLE_ELEMS )  {

       cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

       if ( ier < 0 ) break;

       if ( el.hdr.recsz > 0 )  {
                                                 
          curPos += el.hdr.recsz;
                                                                        
          if ( (int)el.hdr.vg_type == GFA_ELM &&
               !el.hdr.delete )  {
                                                                       
             cvg_getFld ( &el, TAG_GFA_AREATYPE, hazStr, &ier );

	     if ( ier < 0 || strcasecmp( hazStr, haz ) !=0 ) {
		
                cvg_freeElPtr ( &el );
		continue;

	     }

             cvg_getFld ( &el, TAG_GFA_TAG, tagStr, &ier );
	     if ( ier < 0 || strlen ( tagStr ) <= 0 ) {
		
                cvg_freeElPtr ( &el );
		continue;

	     }

	     for ( ii = 0; ii < (int)strlen ( tagStr ); ii++ ) {

		 if ( ! isdigit ( tagStr[ ii ] ) ) {

		    if ( strcasecmp ( &tagStr[ ii ], desk ) == 0 )  {

		       tagStr[ ii ] = '\0';

		       tagNum = atoi( tagStr );

		       for ( jj = 0; jj < *ntags; jj++ ) {
			   
			   if ( (*tags)[ jj ] == tagNum ) break;

		       }

		       if ( jj >= *ntags ) {

            	          (*ntags)++;
		          G_REALLOC ( *tags, int, *ntags, "pggfaw_getTagForDesk tags" );
		          (*tags)[ *ntags - 1 ] = tagNum;

		       }

		       break;

		    }
		 }
	     }
          }
       }
       cvg_freeElPtr ( &el );
       nextEl++;
    }

/*
 * Sort the tag array. 
 */
    for ( ii = 0; ii < *ntags - 1; ii++ ) {

	for ( jj = 0; jj < *ntags - 1; jj++ ) {

	    if ( (*tags)[ jj ] > (*tags)[ jj + 1 ] ) {

		tmp = (*tags)[ jj ];
		(*tags)[ jj ] = (*tags)[ jj + 1 ];
		(*tags)[ jj + 1 ] = tmp;

	    }
	}
    }
}

/*=====================================================================*/

int pggfaw_getHazardType ( char hazard[] )
/************************************************************************
 * pggfaw_getHazardType							*
 *									*
 * This function returns the integer hazard type of the input hazard 	*
 * name.								*
 *									*
 * int pggfaw_getHazardType ( hazard )					*
 *									*
 * Input parameters:							*
 *	hazard[]	char	hazard name.				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			int	hazard type				*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		07/06	Created.                         	*
 * J. Wu/SAIC           06/08   Added GFA_HAZARD_TS                     *
 ***********************************************************************/
{

    if ( strcasecmp ( hazard, "IFR" ) == 0 )           return GFA_HAZARD_IFR;
    else if ( strcasecmp ( hazard, "MT_OBSC" ) == 0 )  return GFA_HAZARD_MT_OBSC;
    else if ( strcasecmp ( hazard, "ICE" ) == 0 )      return GFA_HAZARD_ICE;
    else if ( strcasecmp ( hazard, "TURB" ) == 0 )     return GFA_HAZARD_TURB;
    else if ( strcasecmp ( hazard, "TURB-HI" ) == 0 )  return GFA_HAZARD_TURB_HI;
    else if ( strcasecmp ( hazard, "TURB-LO" ) == 0 )  return GFA_HAZARD_TURB_LO;
    else if ( strcasecmp ( hazard, "SFC_WND" ) == 0 )  return GFA_HAZARD_SFC_WND;
    else if ( strcasecmp ( hazard, "SIGWX" ) == 0 )    return GFA_HAZARD_SIGWX;
    else if ( strcasecmp ( hazard, "CIG_CLD" ) == 0 )  return GFA_HAZARD_CIG_CLD;
    else if ( strcasecmp ( hazard, "TCU_CLD" ) == 0 )  return GFA_HAZARD_TCU_CLD;
    else if ( strcasecmp ( hazard, "MTW" ) == 0 )      return GFA_HAZARD_MTW;
    else if ( strcasecmp ( hazard, "CLD" ) == 0 )      return GFA_HAZARD_CLD;
    else if ( strcasecmp ( hazard, "M_FZLVL" ) == 0 )  return GFA_HAZARD_M_FZLVL;
    else if ( strcasecmp ( hazard, "LLWS" ) == 0 )     return GFA_HAZARD_LLWS;
    else if ( strcasecmp ( hazard, "FZLVL" ) == 0 )    return GFA_HAZARD_FZLVL;
    else if ( strcasecmp ( hazard, "FZLVL_SFC" ) == 0 )return GFA_HAZARD_FZLVL_SFC;
    else if ( strcasecmp ( hazard, "TS" ) == 0 )       return GFA_HAZARD_TS;

    return -1;

}

/*=====================================================================*/

static int pggfaw_getCategoryType ( void )
/************************************************************************
 * pggfaw_getCategoryType   						*
 *									*
 * This function checks the forecast hours in the GFA GUI and returns   *
 * an integer category type corresponding to the forecast hour. 	*
 *									*
 * static int pggfaw_getCategoryType ( )				*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 * Return parameters:							*
 *			int	category type				*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		07/06	Created.                         	*
 ***********************************************************************/
{
    int cat;
    char hours[ 32 ], *text, *ptr;
/*---------------------------------------------------------------------*/
/*
 *  Get the forecast hour from the GFA GUI.
 */
    cat = -1;
    if ( strcasecmp ( _fcsthr [ _currFcsthr ], "other" ) == 0 ) {

       XtVaGetValues ( _fcsthrText, XmNvalue, &text, NULL )	 ;
       strcpy ( hours, text );
       XtFree ( text );

    }
    else {
       strcpy ( hours, _fcsthr [ _currFcsthr ] );
    }

/*
 *  Find the category type.
 */
    ptr = strchr ( hours, '-' );

    if ( ptr == NULL ) {

	cat = GFA_SNAPSHOT;

    }
    else if ( strcasecmp ( hours, "6-6" ) == 0 ) {

	cat = GFA_USER_SMEAR;

    }
    else {

	ptr[ 0 ] = '\0';

	cat = ( atoi( hours ) < 6 ) ? GFA_USER_SMEAR : GFA_USER_OUTLOOK;

    }

    return cat;

}

/*=====================================================================*/

void pggfaw_makeSubtype ( int hazType, int catType, int *subtype, int *iret )
/************************************************************************
 * pggfaw_makeSubtype							*
 *									*
 * This function calculates the subtype from the hazard type and the	*
 * category type.                                                     	*
 *									*
 * void pggfaw_makeSubtype ( hazType, catType, sutype, iret)   		*
 *									*
 * Input parameters:							*
 *	hazType         int	hazard type    				*
 *	catTyep		int	category type              		*
 *									*
 * Output parameters:							*
 *	*subtype	int	subtype					*
 *	*iret		int	return code. 				*
 *					0:	normal			*
 *					-1:	no haztype or cattype	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		07/06	Created.                         	*
 ***********************************************************************/
{

    if ( hazType < 1 || catType < 0 ) {
	
       *iret = -1;

    }
    else {

       *subtype = hazType * 10 + catType;
       *iret = 0;

    }
}

/*=====================================================================*/

static int pggfaw_getCurrentSubtype ( void )
/************************************************************************
 * pggfaw_getCurrentSubtype						*
 *									*
 * This function calculates the subtype from the hazard type and the	*
 * category type.                                                     	*
 *									*
 * void pggfaw_getCurrentSubtype ( )			   		*
 *									*
 * Input parameters:							*
 *	None                        		          		*
 *									*
 * Output parameters:							*
 *	None                        		          		*
 *									*
 * Output parameters:							*
 *	       int	 current subtype				*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		07/06	Created.                         	*
 ***********************************************************************/
{
    int 	ii, subtype, ier;
    char 	haz[ 32 ], *tmpStr, *level;

    XmString 	xmStr;
    Widget	wPulldown, wPushButton;
/*---------------------------------------------------------------------*/

    strcpy( haz, _areaTyp[ _areaTypeIndex ] );

/*
 *  Check if the hazard is a surface level FZLVL
 */
    if ( strcasecmp( haz, "FZLVL" ) == 0 ) {

       for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

	   XtVaGetValues ( _currentPanel2->descStrc[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	   XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );
	   XmStringFree( xmStr );

	   if ( strcasecmp( tmpStr, "Level:" ) == 0 ) {

	      XtVaGetValues ( _currentPanel2->descStrc[ ii ].menu, 
			      XmNsubMenuId, &wPulldown, NULL );
              XtVaGetValues ( wPulldown, XmNmenuHistory, 
	      		      &wPushButton, NULL );

	      XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
	      XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &level );

	      if ( strcasecmp( level, "SFC" ) == 0 ) {
	      
		 strcat ( haz, "_SFC" );

	         XmStringFree ( xmStr );
	         XtFree ( level );
	         XtFree ( tmpStr );
		 break;

	      }
	      else {

	         XmStringFree ( xmStr );
	         XtFree ( level );
	      }
	   }
	   XtFree ( tmpStr );
       }
    }

    pggfaw_makeSubtype ( pggfaw_getHazardType( haz ),
    			 pggfaw_getCategoryType(), &subtype, &ier );

    if ( ier < 0 ) subtype = -1;

    return subtype;
}

/*=====================================================================*/

static void pggfaw_resetForSubtype( int *iret )
/************************************************************************
 * pggfaw_resetForSubtype						*
 *									*
 * This function resets the color picker for the current subtype.   	*
 *									*
 * void pggfaw_resetForSubtype ( iret)   				*
 *									*
 * Input parameters:							*
 *	None                        		           		*
 *									*
 * Output parameters:							*
 *	*iret		int	return code. 				*
 *					0:	normal			*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		07/06	Created.                         	*
 * B. Yin/SAIC		07/06   Free memory.				*
 ***********************************************************************/
{
    int 	subtype, ier; 
    VG_DBStruct	el;
/*---------------------------------------------------------------------*/

    *iret = 0;

    el.hdr.vg_class = CLASS_MET;
    el.hdr.vg_type  = GFA_ELM;
    el.elem.gfa.info.nblocks  = 0;
    el.elem.gfa.info.npts     = 0;

    subtype = pggfaw_getCurrentSubtype();

    ces_get( subtype, &el, &ier );

    _attrColor = el.hdr.maj_col;

    XtVaSetValues ( _colorPb,
		XmNbackground,		NxmColrP_getColorPixel ( _attrColor ),
		XmNtopShadowColor,	NxmColrP_getColorPixel ( _attrColor ),
		XmNbottomShadowColor,	NxmColrP_getColorPixel ( _attrColor ),
		NULL );

    cvg_freeElPtr ( &el );

}

/*=====================================================================*/

static void pggfaw_showIfrWarning ( Widget parent )
/************************************************************************
 * pggfaw_showIfrWarning                                                *
 *                                                                      *
 * This routine pops up a warning window if IFR type is empty. 		*
 *                                                                      *
 * static void pggfaw_showIfrWarning ( parent )		 	        *
 *                                                                      *
 * Input parameters:                                                    *
 * 	parent		Widget		parent widget	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		09/06	Created					*     
 * B. Yin/SAIC		09/06	Free warningBtn WidgetList.		*     
 * B. Yin/SAIC          06/07   Change _curTypeDlg to IFR type dialog.  *
 ***********************************************************************/
{
    int			nn;
    long		ii;
    char		*btnStr[] = { "OK", "Cancel" };

    Arg    		args[ 10 ];
    Widget		pane, msg, rowColType, vis, btnForm, form1;
    WidgetList		warningBtn;
    static Widget	cig;
/*---------------------------------------------------------------------*/
/*
 *  If the warning window has been created, set the type field to 
 *  blank and the cig check box to false, and manage the window.
 */ 
    if ( _ifrWarningW ) {

        XtVaSetValues ( _typeTextW, XmNvalue, "", NULL );
        XmToggleButtonSetState ( cig, FALSE, FALSE );

        XtManageChild ( _ifrWarningW );

	return;

    }
    
/*
 *  Create the warning dialog
 */
    _ifrWarningW = XmCreateFormDialog ( parent, "pggfa_ifrWarning", NULL, 0);

    XtVaSetValues ( _ifrWarningW,
		    XmNdefaultPosition, False,
		    XmNx,               200,
		    XmNy,               200,
		    XmNnoResize,        True,
		    XmNdialogStyle,     XmDIALOG_FULL_APPLICATION_MODAL,
		    NULL );

    XtVaSetValues ( XtParent ( _ifrWarningW ),
		    XmNtitle,           "Error: Incomplete IFR",
		    NULL );

/*
 *  Create a parent pane widget, a form, a warning message and a
 *  rowcol widget.
 */
    pane = XtVaCreateManagedWidget ( "pggfa_wnpane",
                       xmPanedWindowWidgetClass, _ifrWarningW,
		       XmNseparatorOn,		 False,
		       XmNmarginHeight,		 15,
		       XmNmarginWidth,		 0,
		       XmNspacing,		 5,
                       XmNsashWidth,             1,
                       XmNsashHeight,            1,
                       NULL );

    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, pane, NULL );

    msg = XtVaCreateManagedWidget ( "Please select CIG/VIS types:",
                       xmLabelWidgetClass, form1,
		       XmNmarginRight,     100,
		       XmNx,	    	   10,
		       NULL );

    rowColType = XtVaCreateManagedWidget ( "warningRowColType",
   				xmRowColumnWidgetClass,	form1,
				XmNorientation,		XmHORIZONTAL,
                	  	XmNtopAttachment,	XmATTACH_WIDGET,
				XmNpacking,		XmPACK_COLUMN,
				XmNnumColumns,		2,
                	  	XmNtopWidget,		msg,
                	  	NULL );

/*
 *   CIG check box
 */
    XtVaCreateManagedWidget ( "CIG BLW 010:",
                	  	xmLabelWidgetClass,	rowColType,
				XmNmarginLeft,		20,
                	  	NULL );

    cig = XtVaCreateManagedWidget ( " ", xmToggleButtonWidgetClass, 
    				    rowColType,
				    XmNhighlightThickness,	0,
                    		    XmNrecomputeSize,       False,
				    NULL );

    XtAddCallback ( cig, XmNvalueChangedCallback,
			(XtCallbackProc)pggfaw_cigChkBoxCb, 
			(XtPointer) NULL );

/*
 *  VIS label and 'add/remove types' push button
 */
    XtVaCreateManagedWidget ( "VIS BLW 3SM:",
                	  	xmLabelWidgetClass,	rowColType,
				XmNmarginLeft,		20,
                	  	NULL );

    vis = XtVaCreateManagedWidget ( "Add/Remove Types", xmPushButtonWidgetClass, 
    				    rowColType,
                    		    XmNrecomputeSize,       False,
				    NULL );

    if ( XtIsManaged ( _curTypeDlg ) ) XtUnmanageChild ( _curTypeDlg );

    XtAddCallback ( vis, XmNactivateCallback,
			(XtCallbackProc)pggfaw_popupTypeDlgCb, 
                        (XtPointer) _currentPanel2->popup[0].popupDlg );

/* 
 * Text field
 */
    nn = 0;
    XtSetArg( args[nn], XmNrows,			   1 ); nn++;
    XtSetArg( args[nn], XmNcolumns, 			  28 ); nn++;
    XtSetArg( args[nn], XmNeditable, 		       False );	nn++;
    XtSetArg( args[nn], XmNvisualPolicy, 	  XmCONSTANT ); nn++;
    XtSetArg( args[nn], XmNscrollingPolicy,	  XmAUTOMATIC); nn++; 
    XtSetArg( args[nn], XmNscrollBarDisplayPolicy, XmAS_NEEDED); nn++;
    XtSetArg( args[nn], XmNx,		  		  20 );	nn++;
    XtSetArg( args[nn], XmNy,		  		 100 );	nn++;
    XtSetArg( args[nn], XmNcursorPositionVisible,      False ); nn++;

    _typeTextW = XmCreateScrolledText( form1, "fromText", args, nn );

    XtManageChild( _typeTextW );

    XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass, pane, NULL );

/*
 *  Control buttons
 */
    btnForm = XtVaCreateManagedWidget ( "warningBtn",
		xmFormWidgetClass,		pane,
                XmNfractionBase,                2 * 50,
		NULL );

    warningBtn = (WidgetList)XtMalloc((size_t)2*sizeof(Widget));

    for ( ii = 0; ii < 2; ii++ ) {

	warningBtn[ ii ] = XtVaCreateManagedWidget ( btnStr[ ii ], 
		xmPushButtonWidgetClass,	btnForm, 
             	XmNleftAttachment,              XmATTACH_POSITION,
                XmNleftPosition,                ((ii * 50) + 10 ),
                XmNrightAttachment,             XmATTACH_POSITION,
                XmNrightPosition,		(((ii + 1) * 50) - 10),
		NULL );

	XtAddCallback ( warningBtn[ ii ], XmNactivateCallback,
		(XtCallbackProc)pggfaw_ifrWarningBtnCb, (XtPointer) ii );

    }
    
    XtFree( (XtPointer) warningBtn );

    XtManageChild ( _ifrWarningW );

    XtVaSetValues ( XtParent( _ifrWarningW ),
	    XmNminWidth,	310,
	    XmNmaxWidth,	310,
	    XmNminHeight,	220,
	    XmNmaxHeight,	220,
	    NULL );
}

/*=====================================================================*/

/* ARGSUSED */
static void pggfaw_cigChkBoxCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_cigChkBoxCb							*
 *									*
 * Callback function to set the CIG type in the GFA right panel.	*
 *									*
 * static void pggfaw_cigChkBoxCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data, not used		*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		09/06		Created				*
 ***********************************************************************/
{
    XmToggleButtonSetState ( _currentPanel2->checkbox[ 0 ],
	 		     XmToggleButtonGetState ( wid ),
			     True );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_ifrWarningBtnCb ( Widget wid, XtPointer which,
							XtPointer cbs )
/************************************************************************
 * pggfaw_ifrWarningBtnCb						*
 *									*
 * Callback function to set the CIG type in the GFA right panel.	*
 *									*
 * static void pggfaw_ifrWarningBtnCb ( wid, clnt, call )		*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		XtPointer	client data			*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		09/06		Created				*
 ***********************************************************************/
{
    char *text;
    long whichBtn;
/*---------------------------------------------------------------------*/

    whichBtn = (long)which;

    switch ( whichBtn ) {

	case 0:			/* OK */

           XtVaGetValues ( _typeTextW, XmNvalue, &text, NULL );

           if ( strlen ( text ) != (size_t) 0 ) {

	      _okOnWarning = True;
	      XtUnmanageChild ( _ifrWarningW );
	      XtUnmanageChild ( _curTypeDlg );

	   }

	   XtFree ( text );

	   break;

	case 1:			/* Cancel */

	   _cancelOnWarning = True;
	   XtUnmanageChild ( _ifrWarningW );
	   XtUnmanageChild ( _curTypeDlg );

	   break;
    }
}

/*=====================================================================*/

Boolean pggfaw_getFzlRangesFromFile ( char vgfile[], char ranges[] )
/************************************************************************
 * pggfaw_getFzlRangesFromFile						*
 *									*
 * This routine reads the freezing level ranges from the input vg file.	*
 * If there in no ranges info in the input file, it returns false and  	*
 * sets the ranges string to blank.					*
 *									*
 * Boolean pggfaw_getFzlRangesFromFile ( vgfile,  ranges )		*
 *									*
 * Input parameters:							*
 *   vgfile		char[]		input vg file name		*
 *									*
 * Output parameters:							*
 *   ranges		char[]		freezing level range string	*
 *									*
 * Return parameters:							*
 *			Boolean		True: got the ranges		*
 *					False: no ranges in the file	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		01/07		Created				*
 ***********************************************************************/
{
    int         ier, nextEl, curPos;
    long        fileSize;
    char        vgfname[ 128 ], haz[ 128 ];
    
    FILE        *fptr;
    VG_DBStruct el;
    Boolean	ret = False;
/*---------------------------------------------------------------------*/

    ranges[ 0 ] = '\0';

    cvg_open ( vgfile, 0, &fptr, &ier );
    if ( ier < 0 ) return ret;

    cfl_inqr ( vgfile, NULL, &fileSize, vgfname, &ier );
    if ( ier < 0 ) return ret;

    curPos  = 0;
    nextEl  = 0;
    ier     = 0;

    while ( nextEl < MAX_EDITABLE_ELEMS )  {

        cvg_rdrecnoc ( vgfname, fptr, curPos, &el, &ier );

        if ( ier != 0 ) break;

        if ( el.hdr.recsz > 0 )  {

           curPos += el.hdr.recsz;

           if ( (int)el.hdr.vg_type == GFA_ELM &&
                    !el.hdr.delete )  {

              cvg_getFld ( &el, TAG_GFA_AREATYPE, haz, &ier );

	      if ( strcasecmp( haz, "FZLVL" ) == 0 ) {

		 ret = True;

	      }

              cvg_getFld( &el, TAG_GFA_FZLRANGE, ranges, &ier );
              cvg_freeElPtr ( &el );

              if ( ( ier == 0 ) && strlen( ranges ) != 0 ) {

                 break;

              }
           }

           if ( (int)el.hdr.vg_type == GFA_ELM ) {

	      cvg_freeElPtr ( &el );

	   }
        }
        nextEl++;
    }                                       /* read element loop  */
    cvg_clos ( fptr, &ier );
    return ret;
}

/*=====================================================================*/

static void pggfaw_showMtobscWarning ( Widget parent )
/************************************************************************
 * pggfaw_showMtobscWarning                                             *
 *                                                                      *
 * This routine pops up a warning window if MT OBSC type is empty.      *
 *                                                                      *
 * static void pggfaw_showMtobscWarning ( parent )                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget          parent widget                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          06/07   Created                                 *
 * L. Hinson/AWC        05/09   Take out word "OCNL"                    *
 ***********************************************************************/
{
    int                 nn;
    long                ii;
    char                *btnStr[] = { "OK", "Cancel" };
    Arg                 args[ 10 ];
    Widget              pane, msg, rowColType, mtobsc, btnForm, form1;
    WidgetList          warningBtn;
/*---------------------------------------------------------------------*/
/*
 *  If the warning window has been created, set the type field to
 *  blank, and manage the window.
 */
    if ( _mtobscWarningW ) {
        XtVaSetValues ( _mtobscWarningW, XmNvalue, "", NULL );
        XtManageChild ( _mtobscWarningW );
        return;
    }

/*
 *  Create the warning dialog
 */
    _mtobscWarningW = XmCreateFormDialog ( parent, "pggfa_mtobscWarning", NULL, 0);
    XtVaSetValues ( _mtobscWarningW,
                    XmNdefaultPosition, False,
                    XmNx,               200,
                    XmNy,               200,
                    XmNnoResize,        True,
                    XmNdialogStyle,     XmDIALOG_FULL_APPLICATION_MODAL,
                    NULL );
    XtVaSetValues ( XtParent ( _mtobscWarningW ),
                    XmNtitle,           "Error: Incomplete MT_OBSC",
                    NULL );

/*
 *  Create a parent pane widget, a form, a warning message and a
 *  rowcol widget.
 */
    pane = XtVaCreateManagedWidget ( "pggfa_wnpane",
                       xmPanedWindowWidgetClass, _mtobscWarningW,
                       XmNseparatorOn,           False,
                       XmNmarginHeight,          15,
                       XmNmarginWidth,           0,
                       XmNspacing,               5,
                       XmNsashWidth,             1,
                       XmNsashHeight,            1,
                       XmNwidth,                 400,
                       NULL );
    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, pane, NULL );
    msg = XtVaCreateManagedWidget ( "Please select MT_OBSC types:",
                       xmLabelWidgetClass, form1,
                       XmNmarginRight,     200,
                       XmNheight,          35,
                       XmNx,               10,
                       NULL );
    rowColType = XtVaCreateManagedWidget ( "warningRowColType",
                                xmRowColumnWidgetClass, form1,
                                XmNorientation,         XmHORIZONTAL,
                                XmNtopAttachment,       XmATTACH_WIDGET,
                                XmNpacking,             XmPACK_COLUMN,
                                XmNnumColumns,          1,
                                XmNtopWidget,           msg,
                                NULL );

/*
 *  "Add/Remove types" push button
 */
    XtVaCreateManagedWidget ( "MTNS OBSC BY:",
                                xmLabelWidgetClass,     rowColType,
                                XmNmarginLeft,          20,
                                NULL );
    mtobsc = XtVaCreateManagedWidget ( "Add/Remove Types", xmPushButtonWidgetClass,
                                    rowColType,
                                    XmNmarginLeft,              20,
                                    XmNrecomputeSize,       False,
                                    NULL );
    if ( XtIsManaged ( _curTypeDlg ) ) XtUnmanageChild ( _curTypeDlg );
    XtAddCallback ( mtobsc, XmNactivateCallback,
                        (XtCallbackProc)pggfaw_popupTypeDlgCb,
                        (XtPointer) _currentPanel2->popup[0].popupDlg );

/*
 * Text field
 */
    nn = 0;
    XtSetArg( args[nn], XmNrows,                           1 ); nn++;
    XtSetArg( args[nn], XmNcolumns,                       38 ); nn++;
    XtSetArg( args[nn], XmNeditable,                   False ); nn++;
    XtSetArg( args[nn], XmNvisualPolicy,          XmCONSTANT ); nn++;
    XtSetArg( args[nn], XmNscrollingPolicy,       XmAUTOMATIC); nn++;
    XtSetArg( args[nn], XmNscrollBarDisplayPolicy, XmAS_NEEDED); nn++;
    XtSetArg( args[nn], XmNx,                             20 ); nn++;
    XtSetArg( args[nn], XmNy,                             75 ); nn++;
    XtSetArg( args[nn], XmNcursorPositionVisible,      False ); nn++;
    _mtobscTextW = XmCreateScrolledText( form1, "fromText", args, nn );
    XtManageChild( _mtobscTextW );
    XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass, pane, NULL );

/*
 *  Control buttons
 */
    btnForm = XtVaCreateManagedWidget ( "warningBtn",
                xmFormWidgetClass,              pane,
                XmNfractionBase,                2 * 50,
                NULL );
    warningBtn = (WidgetList)XtMalloc((size_t)2*sizeof(Widget));
    for ( ii = 0; ii < 2; ii++ ) {
        warningBtn[ ii ] = XtVaCreateManagedWidget ( btnStr[ ii ],
                xmPushButtonWidgetClass,        btnForm,
                XmNleftAttachment,              XmATTACH_POSITION,
                XmNleftPosition,                ((ii * 50) + 10 ),
                XmNrightAttachment,             XmATTACH_POSITION,
                XmNrightPosition,               (((ii + 1) * 50) - 10),
                NULL );
        XtAddCallback ( warningBtn[ ii ], XmNactivateCallback,
                (XtCallbackProc)pggfaw_mtobscWarningBtnCb, (XtPointer) ii );
    }
    XtFree( (XtPointer) warningBtn );
    XtManageChild ( _mtobscWarningW );
    XtVaSetValues ( XtParent( _mtobscWarningW ),
            XmNminWidth,        400,
            XmNmaxWidth,        400,
            XmNminHeight,       190,
            XmNmaxHeight,       190,
            NULL );
}
/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_mtobscWarningBtnCb ( Widget wid, XtPointer which,
                                                        XtPointer cbs )
/************************************************************************
 * pggfaw_mtobscWarningBtnCb                                            *
 *                                                                      *
 * Callback function for control buttons in MT OBSC warning dialog.     *
 *                                                                      *
 * static void pggfaw_mtobscWarningBtnCb ( wid, clnt, call )            *
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   which              XtPointer       client data                     *
 *   cbs                XtPointer       callback struct, not used       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          06/07           Created                         *
 ***********************************************************************/
{
    char *text;
    long whichBtn;
/*---------------------------------------------------------------------*/
    whichBtn = (long)which;
    switch ( whichBtn ) {
        case 0:                 /* OK */
           XtVaGetValues ( _mtobscTextW, XmNvalue, &text, NULL );
           if ( strlen ( text ) != (size_t) 0 ) {
              _okOnWarning = True;
              XtUnmanageChild ( _mtobscWarningW );
              XtUnmanageChild ( _curTypeDlg );
           }
           XtFree ( text );
           break;
        case 1:                 /* Cancel */
           _cancelOnWarning = True;
           XtUnmanageChild ( _mtobscWarningW );
           XtUnmanageChild ( _curTypeDlg );
           break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfaw_colorZeroCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfaw_colorZeroCb                                                   *
 *                                                                      *
 * This is the callback function for the color picker button. If the    *
 * color of the GFA is zero, the color is set to black(32) and then     *  
 * popup the color picker.                                              *
 *                                                                      *
 * void pggfaw_colorZeroCb ( wid, clnt, cbs )				*
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget         widget ID                                *
 *      clnt	XtPointer      client data	                        *
 *      cbs     XtPointer      callback structure                       *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           07/07   Created		                *
 ***********************************************************************/
{
    if ( _attrColor == 0 ) _attrColor = BLACK;
    NxmClrW_popup( wid, (XtPointer)&_attrColor, cbs );
}
