#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "hints.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "proto_xw.h"


#define SFC		"SFC"
#define APPLY_COLOR	"yellow"

#define MAX_AREATYPSTR ( 25 )
#define MAX_FCSTHRSTR  (  9 )
#define MAX_TAGSTR     (  9 )
#define MAX_ISSOPTSTR  (  5 )
#define MAX_SEQNUM     (300 )
#define MAX_DSCPSTR    ( 12 )
#define MAX_REGION     (  6 )
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
 *  structure for widgets in panel 3
 *  the array size MAXTBLNAME is required by setOptMenu(...) 
 */
typedef struct {
	Widget			*region;
	Widget			*area;
	Widget			*states;
	Widget			*cycle;
	Widget			*conds_begin;
	Widget			*conds_end;
} panel3_t;

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
static  char                    _fcsthrAndUTC[MAXNOPT][MAXTBLNAME];
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
static  Widget	_fcsthrText;
static  Widget	_curTypeDlg = NULL;		/* Current (IFR) type add/remove dialog */
static  Widget	_ifrWarningW = NULL;		/* Incomplete IFR warning window */
static  Widget  _typeTextW = NULL;              /* Text field in the IFR warning window */
static  Widget  _mtobscWarningW = NULL;         /* Incomplete IFR warning window */
static  Widget  _mtobscTextW = NULL;            /* Text field in the MTOBSC warning window */

static	panel2_t	*_currentPanel2;
static	WidgetList	_ctlButtons;

static	Widget	_areaText;
static	Widget	_stateText;
static	Widget	_beginText;
static	Widget	_endText;
static  Widget	_frameFBBA;
static  Widget	_statesBtn;

static	int	_areaTypeIndex;
static	int	_currFcsthr;

static	char	_topStr[10]	= "";
static	char	_bottomStr[10]	= "";
static  char    _fcsthrStr[10]  = "0";
static  char    _tmpFcsthrStr[10]  = "";

static	int	_attrColor	= 4;

static Boolean	_waitFlag	= False;
static Boolean	_txtActive	= False;

static Boolean	_addMode	= False;	/* add or edit mode flag */
static Boolean	_textDrawn	= True;		/* attr box drawn or not */

static Boolean	_editSmear	= False;	/* Are we editing a smear? */

static panel2_t	*_panel2        = NULL;
static int	_nPanel2 	= 0;

static int	_FLIndex	= 0;		/* array index of FL input in panel2 */
static int	_FZLIndex	= 0;		/* array index of FZL input in panel2 */
static int	_FZLRange	= 0;		/* array index of FZL range input in panel2 */

static XtCallbackProc 	_editCbFunc;

static Boolean _okOnWarning 	= False;
static Boolean _cancelOnWarning = False;
static Boolean _autoPlacement   = False;		/* text box auto placement flag */
static Boolean _show3LinesWarning = True;

/*
 *  private functions -- callback
 */
static void pggfawp_areatypPbCb		( Widget, long, XtPointer );
static void pggfawp_ctlBtnCb		( Widget, long, XtPointer );
static void pggfawp_deskCb		( Widget, long, XtPointer );
static void pggfawp_tagCb		( Widget, long, XtPointer );
static void pggfawp_fcsthrPbCb		( Widget, long, XtPointer );
static void pggfawp_txtMoveCb		( Widget, XtPointer, XtPointer );
static void pggfawp_fillFLCb		( Widget, XtPointer, XtPointer );
static void pggfawp_fillSingleFLCb      ( Widget, XtPointer, XtPointer );
static void pggfawp_optPbCb		( Widget, XtPointer, XtPointer );
static void pggfawp_vrfyFcstHrCb	( Widget, XtPointer, XtPointer );
static void pggfawp_fcsthrTxtCb		( Widget, XtPointer, XtPointer );
static void pggfawp_addFcstHrColon	( Widget, XtPointer, XtPointer );

static void pggfawp_txtDragEh		( Widget, XtPointer, XEvent*, Boolean* );
static void pggfawp_txtPressEh		( Widget, XtPointer, XEvent*, Boolean* );
static void pggfawp_verifyFLCb		( Widget, XtPointer, XtPointer );
static void pggfawp_verifySingleFLCb    ( Widget, XtPointer, XtPointer );
static void pggfawp_verifyAreaCb	( Widget, XtPointer, XtPointer );
static void pggfawp_warningBtnCb	( Widget, XtPointer, XtPointer );
static void pggfawp_warningBtnSingleFLCb( Widget, XtPointer, XtPointer );
static void pggfawp_verifyFzlCb		( Widget, XtPointer, XtPointer );
static void pggfawp_fillFzlCb		( Widget, XtPointer, XtPointer );
static void pggfawp_changedFLCb		( Widget, XtPointer, XtPointer );
static void pggfawp_ifrWarningBtnCb	( Widget, XtPointer, XtPointer );
static void pggfawp_panel3TextCb	( Widget, XtPointer, XtPointer );
static void pggfawp_mtobscWarningBtnCb	( Widget, XtPointer, XtPointer );
static void pggfawp_popupTypeDlgCb	( Widget, XtPointer, XtPointer );
static void pggfawp_closeTypeDlgCb	( Widget, XtPointer, XtPointer );
static void pggfawp_setTypeTextCb	( Widget, XtPointer, XtPointer );
static void pggfawp_colorZeroCb		( Widget, XtPointer, XtPointer );
static void pggfawp_statesListCb 	( Widget, XtPointer, XtPointer );
static void pggfawp_cigOptPbCb          ( Widget, XtPointer, XtPointer );

/*
 * private functions -- action
 */
static void pggfawp_fillStrArray ( int max_strarr, int max_string, char origstr[],
				  char strarr[][MAXTBLNAME] );
static void pggfawp_getGuiInfo ( int *iret );
static void pggfawp_setText ( void );
static void pggfawp_setAddMode ( Boolean inAddMode );
static void pggfawp_cleanup ( void );
static void pggfawp_createPanel1 ( void );
static void pggfawp_createPanel2 ( void );
static void pggfawp_createPanel3 ( void );
static void pggfawp_getPanel2Attr ( VG_DBStruct *el );
static void pggfawp_getPanel3Attr ( VG_DBStruct *el );
static void pggfawp_setPanel2Attr ( VG_DBStruct *el ); 
static void pggfawp_setPanel3Attr ( VG_DBStruct *el ); 
static void pggfawp_showWarning ( Widget parent );
static void pggfawp_showWarningSingleFL ( Widget parent );

static void pggfawp_createOptionMenu ( Widget parent, int nbuttons,
				XtPointer pstart_item, char label_str[], 
				XtCallbackProc callback, Widget *form, 
				Widget *label, Widget *menu, Widget pbs[],
                                int posX, int posY,
				char *btnstrs[], unsigned char orient );
                                
static void pggfawp_initIssueType ( void ) ;
static void pggfawp_setApplyBtn   ( Boolean enable );
static void pggfawp_initDueTo 	 ( void ) ;

static void pggfawp_setFzl( Boolean value );
static int pggfawp_getNextTag ( void );
static void pggfawp_addTag( int tag );
static void pggfawp_setTagMenu( char tag[] );
static void pggfawp_getTagMenu( char tag[], char *tagNum, char *desk );
static void pggfawp_setDfltTag( char fcsthr[] );
static Boolean pggfawp_tagInUse( char vgfname[], FILE *fptr, char tag[], char fcsthr[] );
static void pggfawp_getTagsForDesk ( char vgfname[], FILE *fptr, char desk[], 
				    char haz[], int **tags, int *ntags );
static void pggfawp_setAllTags ( void );
static int pggfawp_getCategoryType ( void );
static int pggfawp_getCurrentSubtype ( void );
static void pggfawp_resetForSubtype( int *iret );
static void pggfawp_showIfrWarning ( Widget parent );
static void pggfawp_showOrigIfrWarning ( Widget parent );
static void pggfawp_cigChkBoxCb( Widget wid, XtPointer clnt, XtPointer call );
static void pggfawp_managePanel3 ( Boolean state );
static void pggfawp_showMtobscWarning ( Widget parent );
static Boolean pggfawp_isFBBA ( void );
static Boolean pggfawp_CvHasType( void );
static Boolean pggfawp_isIFR ( void );
static void pggfawp_createCvPanel( int nLabel );
static Boolean pggfawp_is20KtSfcWind ( void );
static Boolean pggfawp_addNCDesc ( char *haz, char *desc );
static void pggfawp_chkNCDesc4MSel ( char *haz, int *which );

/************************************************************************
 * nmap_pggfawp.c							*
 *									*
 * This module defines everything for GFA (Airmet) creation/editing.	*
 *									*
 * CONTENTS:								*
 *	pggfawp_create()	creates the popup window		*
 *	pggfawp_popup()		manages the popup window		*
 *	pggfawp_popdown()	unmanages the popup window		*
 *									*
 *	pggfawp_getAttr()	gets the current attributes		*
 *	pggfawp_isTxtActive	checks if in move text mode		*
 *	pggfawp_isUp()		queries whether the window is up	*
 *	pggfawp_saveNew()	saves a new GFA element			*
 *	pggfawp_setAttr()	sets the attibutes			*
 *	pggfawp_setFrom()	sets the from line			*
 *      pggfawp_reorderGFA()	re-order the points in a CW direction	*
 *									*
 *	pggfawp_areatypPbCb()	callback for area menu buttons		*
 *	pggfawp_ctlBtnCb()	callback for control buttons		*
 *	pggfawp_fcsthrPbCb()	callback for forecast hour menu buttons	*
 *	pggfawp_fcsthrTxtCb()	callback for forecast hour text field	*
 *      pggfawp_txtMoveCb()     callback for "Move Text" button		*
 *	pggfawp_optPbCb 	callback for option menu		*
 *	pggfawp_fillFLCb	callback for 'top/botton' text field	*
 *									*
 *      pggfawp_txtDragEh()     event handler for dragging attr. box	*
 *	pggfawp_txtPressEh()	event handler for placing attr. box	*
 *	pggfawp_vrfyFcstHrCb()	verify the forecast hour text input	*
 *	pggfawp_addFcstHrColon() add a colon to forecast hour text input*
 *									*
 *	pggfawp_fillStrArray()	fills an array of strings		*
 *	pggfawp_getGuiInfo()	reads GFA infomation table		*
 *	pggfawp_setText		sets attr. box ghosting/event handling	*
 *	pggfawp_setAddMode	sets GFA window mode to Add or Edit	*
 *	pggfawp_setType		sets GFA area type to the new type	*
 *	pggfawp_setHour		sets GFA forecast hour to the new hour	*
 *	pggfawp_cleanup		cleans up if quit before placing box	*
 *	pggfawp_getGuiInfo 	read GUI infomation from a table	*
 *	pggfawp_createPanel1 	create the top panel			*
 *	pggfawp_createPanel2 	create the second panel			*
 *	pggfawp_createPanel2 	create the third panel			*
 *	pggfawp_getPanel2Attr 	get attribute in the hazard panel	*
 *	pggfawp_getPanel3Attr 	get attribute in the F/BB/A panel	*
 *	pggfawp_setPanel2Attr 	set attribute in the hazard panel	*
 *	pggfawp_setPanel3Attr 	set attribute in the F/BB/A panel	*
 *									*
 *      pggfawp_managePanel3    manages/unmanages the F/BB/A frame	*
 *      pggfawp_isFBBA		returns true if current element is FBBA *
 *      pggfawp_resetFcstHrOptMenu() Rebuilds the list of Valid Times   *
 ***********************************************************************/

/*=====================================================================*/

void pggfawp_create ( Widget parent )
/************************************************************************
 * pggfawp_create							*
 *									*
 * This function creates a GFA (Airmet) creating/editing window.	*
 *									*
 * void pggfawp_create ( parent )					*
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
 * B. Yin/SAIC		02/08	read autoplacement flag from prefs.tbl  *
 ***********************************************************************/
{
    int		ier;
    long	ii, nn;
    char	*ctlstrs[] = { "Apply", "Cancel" }, autoPlaceStr[ 32 ];
    XmString	xmstr;
    
/*---------------------------------------------------------------------*/
/*
 *  Load GUI information from "gfa_temp.tbl".
 */
    pggfawp_getGuiInfo ( &ier );

    if ( ier != 0 ) return;
    
/*
 *  Read text auto placement flag from "prefs.tbl".
 */
    ctb_pfstr ( "ENABLE_AUTOPLACE", autoPlaceStr, &ier );

    if ( ier == 0 && strlen( autoPlaceStr ) > 0
         && strcasecmp( autoPlaceStr, "TRUE" ) == 0 ) {
                                                                                  
         _autoPlacement = True;
                                                                            
    }

/*
 *  Create main form dialog window.
 */
    _gfaForm = XmCreateFormDialog ( parent, "pggfawp_popup", NULL, 0 );

    xmstr = XmStringCreateLocalized("GFA F/BB/A Create/Edit");

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
    
    pggfawp_createPanel1 ( );
    pggfawp_createPanel2 ( );
    pggfawp_createPanel3 ( );

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
		(XtCallbackProc)pggfawp_ctlBtnCb, (XtPointer) ii );
    }
    
/*
 *  Initialize _gfaElm
 */
    _gfaElm.elem.gfa.info.nblocks = 0;

}

/*=====================================================================*/

void pggfawp_popup ( VG_DBStruct *el, XtCallbackProc callback )
/************************************************************************
 * pggfawp_popup							*
 *									*
 * This function manages the GFA popup.					*
 *									*
 * void pggfawp_popup ( el, callback )					*
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
 * B. Yin/SAIC		02/08	disable mvTxt btn if autoplace is true	*
 * J. Wu/SAIC		03/08	allow editing of smears			*
 * B. Yin/SAIC		04/08	disable states btn 			*
 * L. Hinson/AWC        09/09   Drop From Line Push Button Logic        *
 * X. Guo/CWS		01/10   Handle multi-selected GFA elements	*
 ***********************************************************************/
{
    int		subtype, ier;
    VG_DBStruct	newel;
    XmString	xmstr;
    Boolean	inAddMode = False;
    char	curLayerName[FILE_NAMESZ], value[32];
    int		iopr;
/*---------------------------------------------------------------------*/
 
    pggfawp_popdown ();

    pggfawp_setAllTags();

    _editSmear = False;
   
   iopr = pgpalw_getCurOperId();
   if ( iopr != FUNC_MULTISEL ) {
        _nDesks --;
	if ( _deskStrc.current >= _nDesks ) {
	    _deskStrc.current = 0;
	}
	_nIssopt --;
   }
   if ( el == NULL ) {	/* Add new elements */

	inAddMode = True;
        pggfawp_setAddMode ( inAddMode );
		
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
	pggfawp_getAttr ( &newel );

	pggfawp_setAttr ( &newel );        
	
/*
 *  Disable 'Move Text' buttons
 */
        XtSetSensitive ( _txtMovePb, False );        

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
            pggfawp_setType ( curLayerName );    
        }
        
/*
 *  Retrieve attribute settings.
 */
        subtype = pggfawp_getCurrentSubtype();
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

	pggfawp_initIssueType();
        pggfawp_initDueTo();

/*
 *  Free block memory.
 */
        cvg_freeElPtr ( &newel );
	
    }
    else {	/* Edit elements */
	if (callback != NULL) _editCbFunc = callback;
    
	pggfawp_setAddMode ( inAddMode );
	
/*
 *  Check if we are editing a smear 
 */
	cvg_getFld ( el, TAG_GFA_SUBTYPE, value, &ier );
	subtype = atoi( value ) - atoi( value )/10 * 10;
	if ( subtype == GFA_SYSTEM_SMEAR || subtype == GFA_SYSTEM_OUTLOOK ||
             subtype == GFA_USER_SMEAR   || subtype == GFA_USER_OUTLOOK ) {
             _editSmear = True;                 
	}
 
 	pggfawp_setAttr ( el );

        if ( iopr == FUNC_MULTISEL ) {
	     XtVaSetValues ( _fromText, XmNvalue, "", NULL );
	     XtSetSensitive ( _txtMovePb, False );
	}
	else {
	    pggfawp_setFrom ( el->elem.gfa.info.npts, el->elem.gfa.latlon, 
    		         &(el->elem.gfa.latlon[el->elem.gfa.info.npts]) );
	
/*
 *  Enable 'Move Text' button
 *  If autoPlacement flag is true, disable 'Move Text' button
 */
            XtSetSensitive ( _txtMovePb, !_autoPlacement );
	}
/*
 *  Show 'Apply' and 'Cancel' buttons for edit mode 
 */
        if ( !XtIsManaged ( _cntlForm ) ) {
	   XtManageChild ( _cntlForm );
        }
  
    }

/*
 *  Disable 'Apply'  and 'States' button
 */
    pggfawp_setApplyBtn( False );
    pggfawp_setStatesBtn( False );
    
/*
 *  Manage GFA form.
 */
    pggfawp_createPanel2 ( );
    XtManageChild ( _gfaForm );
            
/*
 *  Set window title.
 */
    if ( inAddMode ) { 
        xmstr = XmStringCreateLocalized( "GFA F/BB/A Create" );	
    }
    else {
        if ( _editSmear ) {	
	    xmstr = XmStringCreateLocalized( "GFA Smear Edit" );	
	}
	else {	
	    xmstr = XmStringCreateLocalized( "GFA F/BB/A Edit" );	
        }
    }
    
    XtVaSetValues ( _gfaForm, XmNdialogTitle, xmstr, NULL );
    XmStringFree(xmstr);
   if ( iopr != FUNC_MULTISEL ) {
        _nDesks ++;
	_nIssopt ++;
   }
}

/*=====================================================================*/

void pggfawp_popdown ( void )
/************************************************************************
 * pggfawp_popdown							*
 *									*
 * This function unmanages the GFA popup.				*
 *									*
 * void pggfawp_popdown ()						*
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
 * J. Wu/SAIC		03/08	reset _editSmear to False		*
 * B. Yin/SAIC		03/08	do not close if states btn is activated	*
  ***********************************************************************/
{
/*---------------------------------------------------------------------*/

   if ( XtIsSensitive ( _statesBtn ) ) return;

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
    pggfawp_cleanup ();

/*
 *  Reset the edit status to normal.
 */
    _editSmear = False;
}

/*=====================================================================*/

Boolean pggfawp_isUp ( void )
/************************************************************************
 * pggfawp_isUp								*
 *									*
 * Specify whether the GFA dialog is managed or not.			*
 *									*
 * Boolean pggfawp_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pggfawp_isUp		Boolean		Is/is not managed	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		02/04	initial coding				*
 ***********************************************************************/
{
    return ( XtIsManaged (_gfaForm) );
}

/*=====================================================================*/

static void pggfawp_fillStrArray ( int max_strarr, int max_string,
               char origstr[], char strarr[][MAXTBLNAME] )
/************************************************************************
 * pggfawp_fillStrArray							*
 *									*
 * This function copies a string of multiple names delimited by a ';'	*
 * to an array of strings.						*
 *									*
 * static void pggfawp_fillStrArray (max_strarr, max_string, origstr,	*
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
static void pggfawp_areatypPbCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggfawp_areatypPbCb							*
 *									*
 * Callback function for area type menu buttons.			*
 *									*
 * static void pggfawp_areatypPbCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
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
    pggfawp_setDfltTag( _fcsthr[ _currFcsthr ] );
    
/*
 *  Create the panel for the hazard
 */
    pggfawp_createPanel2();

/*
 *  Enable 'Apply' button
 */
    pggfawp_setApplyBtn( True );

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

        if( pggfawp_isAddMode() ) {
            pggfawp_initDueTo();
        }
    }
    pggfawp_resetForSubtype( &ier );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_fcsthrPbCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggfawp_fcsthrPbCb							*
 *									*
 * Callback function for forecast hour menu buttons.			*
 *									*
 * static void pggfawp_fcsthrPbCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
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
 * E. Safford/SAIC	04/07	activate/deactivate panel3  		*
 ***********************************************************************/
{
    int		ier;
    char	*tmpStr, *ptr = NULL;
    XmString	xmStr;
    Boolean	usePanel3 = False;
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

    if( pggfawp_isAddMode() ) {
        pggfawp_setDfltTag ( tmpStr );
    }

/*
 *  If the time contains a '-' then it is an F/BB/A element, not
 *  a snapshot.  Bring up the F/BB/A panel (panel 3).
 */
    ptr = strchr( tmpStr, '-' );
    if( ptr != NULL ) {
	usePanel3 = True;
    } 
    pggfawp_managePanel3( usePanel3 );


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
    pggfawp_setApplyBtn( True );

    pggfawp_resetForSubtype( &ier );

}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_txtMoveCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_txtMoveCb							*
 *									*
 * Callback function for "Move Text" button.				*
 *									*
 * static void pggfawp_txtMoveCb ( wid, clnt, call )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	call		XtPointer	callback struct			*
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
    pggfawp_setText ();
                
/*
 *  Free block memory.
 */
    cvg_freeElPtr ( &el );
}

/*=====================================================================*/

Boolean pggfawp_isTxtActive ( void )
/************************************************************************
 * pggfawp_isTxtActive							*
 *									*
 * Check whether in the "Move Text" mode.				*
 *									*
 * Boolean pggfawp_isTxtActive ()					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *				NONE					*
 * Return parameters:							*
 *	pggfawp_isTxtActive	Boolean		Is/is not active	*
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
static void pggfawp_ctlBtnCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggfawp_ctlBtnCb							*
 *									*
 * Callback function for the control buttons.				*
 *									*
 * static void pggfawp_ctlBtnCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	which		int		which button			*
 *	call		XtPointer	callback struct			*
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
 * E. Safford/SAIC	06/07	check error code from pgvgf_saveNewElm  *
 ***********************************************************************/
{
    int 	ier, np, location;
    float	llx, lly, urx, ury;
    char	tagStr[ 32 ];
/*---------------------------------------------------------------------*/

    switch ( which ) {

      case 0:		/* Apply */

        if ( !pggfawp_okToDraw ( True ) ) break;

        pggfawp_setApplyBtn( False );

	if ( _editCbFunc ) {
	   _editCbFunc ( NULL, (XtPointer)which, NULL );
	}
	else {

    	    np = _gfaElm.elem.gfa.info.npts;

    	    location = pgactv_getElmLoc ();
            pgutls_prepNew ( location, &_gfaElm, &llx, &lly, &urx, &ury, &ier );    

    	    pggfawp_getAttr ( &_gfaElm );
    	    pggfawp_checkHours ( &_gfaElm );

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
	pggfawp_addTag( atoi(tagStr) );

	pggfawp_setAllTags();

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

void pggfawp_getAttr ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_getAttr							*
 *									*
 * This routine returns the current values of the GFA element.		*
 *									*
 * void pggfawp_getAttr ( el )						*
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
 * E. Safford/SAIC	04/07	use cvg_freeElPtr() to avoid mem leak &	*
 *				  call pggfawp_getPanel3Attr()		*
 * B. Yin/SAIC		07/08	Save C&V as either IFR or MVFR 		*
 * B. Yin/SAIC		07/08	Set 20KT SFC WND as SFC_WND20 		*
 * J. Wu/SAIC		07/08	preserve the override issue flag & the 	*
 *                 		number of points in _gfaElm		*
 * L. Hinson/AWC        06/08   Set the tag TAG_GFA_CYCLE               *
 * J. Wu/SAIC		07/08	Minor bugs fix on variable declarations	*
 * X. Guo/CWS		01/10   Handle multi-selected GFA elements.	*
 ***********************************************************************/
{
    int		ii, ier, curDesk, curHaz, curTag, pos, iret;
    char        value[32], *tmpStr, tag[32], tag1[32];

    XmString	xmStr;
    Widget	wPulldown, wPushButton;
    char day[8], cycle[8];
    char fcsValue[32],tagValue[32],typeText[ STD_STRLEN ],typeText1[ STD_STRLEN ];
    char topValue[32],topValue1[32],bottValue[32],bottValue1[32];
    char issoptValue[32];
    char tmp1Value[32],tmp1Value1[32];
    char tmp2Value[32],tmp2Value1[32];
    char tmp3Value[32],tmp3Value1[32];

    int iopr;
/*---------------------------------------------------------------------*/
/*
 *  Initialize the output element.
 */    
    iopr = pgpalw_getCurOperId();
    if ( iopr == FUNC_MULTISEL ) {
	cvg_getFld ( el, TAG_GFA_FCSTHR, fcsValue, &ier );
	if ( ier != 0 ) {
	    fcsValue[0] = '\0';
	}
	cvg_getFld ( el, TAG_GFA_TAG, tagValue, &ier );
        if ( ier != 0 ) {
            tagValue[0] = '\0';
        }
	cvg_getFld ( el, TAG_GFA_STATUS, issoptValue, &ier );
        if ( ier != 0 ) {
            issoptValue[0] = '\0';
        }
        cvg_getFld ( el, "<Type>", typeText, &ier );
        if ( ier != 0 ) {
            typeText[0] = '\0';
        }
        cvg_getFld ( el, TAG_GFA_TOP, topValue, &ier );
        if ( ier != 0 ) {
            topValue[0] = '\0';
        }
        cvg_getFld ( el, TAG_GFA_BOTTOM, bottValue, &ier );
        if ( ier != 0 ) {
            bottValue[0] = '\0';
        }
	if ( strcasecmp (_areaTyp[_areaTypeIndex], "TURB") == 0 ||
	     strcasecmp (_areaTyp[_areaTypeIndex], "TURB-HI") == 0 ||
	     strcasecmp (_areaTyp[_areaTypeIndex], "TURB-LO") == 0 ) {
            cvg_getFld ( el, "<DUE TO>", tmp1Value, &ier );
            if ( ier != 0 ) {
                tmp1Value[0] = '\0';
            }
	}
	else if ( strcasecmp (_areaTyp[_areaTypeIndex], "CLD") == 0 ) { 
            cvg_getFld ( el, "<Coverage>", tmp1Value, &ier );
            if ( ier != 0 ) {
                tmp1Value[0] = '\0';
            }
        }
	else if ( strcasecmp (_areaTyp[_areaTypeIndex], "SFC_WND") == 0 ) {
	    cvg_getFld ( el, "<Speed>", tmp1Value, &ier );
            if ( ier != 0 ) {
            	tmp1Value[0] = '\0';
            }
	}
	else if ( strcasecmp (_areaTyp[_areaTypeIndex], "FZLVL") == 0 ) {
            cvg_getFld ( el, "<Contour>", tmp1Value, &ier );
            if ( ier != 0 ) {
                tmp1Value[0] = '\0';
            }
            cvg_getFld ( el, "<Level>", tmp2Value, &ier );
            if ( ier != 0 ) {
                tmp2Value[0] = '\0';
            }
            cvg_getFld ( el, TAG_GFA_FZLRANGE, tmp3Value, &ier );
            if ( ier != 0 ) {
                tmp3Value[0] = '\0';
            }
	}
	else if ( strcasecmp (_areaTyp[_areaTypeIndex], "TS") == 0 ) {
            cvg_getFld ( el, "<Category>", tmp1Value, &ier );
            if ( ier != 0 ) {
                tmp1Value[0] = '\0';
            }
            cvg_getFld ( el, "<Frequency>", tmp2Value, &ier );
            if ( ier != 0 ) {
                tmp2Value[0] = '\0';
            }
	}
    }
    cvg_freeElPtr( el );

    el->hdr.vg_class	= CLASS_MET;
    el->hdr.vg_type	= GFA_ELM;
    el->hdr.smooth	= 0;
    el->hdr.closed      = pggfawp_isClosed() ? 1 : 0;
    el->hdr.filled	= 0;	/* 2 = solid, 3 = dashed lines, 4 = lines */
    el->hdr.maj_col	= _attrColor;
    el->hdr.min_col	= _attrColor;	

        
    if ( strcasecmp( _areaTyp[_areaTypeIndex], "C&V" ) == 0 ) {

        if ( pggfawp_isIFR() ) {

           cvg_setFld ( el, TAG_GFA_AREATYPE, "IFR", &ier );

        }
        else {

           cvg_setFld ( el, TAG_GFA_AREATYPE, "MVFR", &ier );

        }

    }
    else if ( pggfawp_is20KtSfcWind() ) {

           cvg_setFld ( el, TAG_GFA_AREATYPE, "SFC_WND20", &ier );

    }
    else {

       cvg_setFld ( el, TAG_GFA_AREATYPE, _areaTyp[_areaTypeIndex],
                    &ier );

    }

/*
 *  Get forecast hour
 */
    XtVaGetValues ( _fcsthrStrc.menu, 
   		    XmNsubMenuId, &wPulldown, NULL );
    XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

    XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr ); 

    if ( strcasecmp ( tmpStr, "other" ) == 0 ) {
        if ( iopr == FUNC_MULTISEL ) {
	    cvg_setFld ( el, TAG_GFA_FCSTHR, fcsValue, &ier );
        }
        else {
            cvg_setFld ( el, TAG_GFA_FCSTHR, _fcsthrStr, &ier );
        }
    }
    else {
    cvg_setFld ( el, TAG_GFA_FCSTHR, 
	                 _fcsthr[_currFcsthr], &ier );
    }
			 
    XtFree ( tmpStr );
    XmStringFree( xmStr );

/*
 *  Get tag
 */   
    curDesk = _deskStrc.current;
    curHaz  = _areaTypStrc.current;
    curTag  = _tagStrc[ curDesk ][ curHaz ].current;
    if ( iopr == FUNC_MULTISEL ) {
	if ( strcasecmp ( _tags[ curDesk ][ curHaz ][ curTag ],
                      "new" ) == 0 ) {
	    if ( curDesk <  _nDesks - 1 ) {
		pggfawp_getTagMenu (tagValue,tag,value);
		sprintf ( tagValue, "%s%s",tag,_desks[ curDesk ]);
	    }
	}
	else {
	     if ( strchr( _tags[ curDesk ][ curHaz ][ curTag ], '*' ) ) {

               cst_rmst( _tags[ curDesk ][ curHaz ][ curTag ],
                         "*", &pos, tag, &ier );
            }
            else {
               strcpy( tag, _tags[ curDesk ][ curHaz ][ curTag ] );
            }

	    if ( curDesk <  _nDesks - 1 ) {
		sprintf ( tagValue, "%s%s",tag,_desks[ curDesk ]);
	    }
	    else {
		pggfawp_getTagMenu (tagValue,tag1,value);
		sprintf ( tagValue, "%s%s",tag1, value);
	    }
	}
	cvg_setFld ( el, TAG_GFA_TAG, tagValue, &ier );
    }
    else {
        if ( strcasecmp ( _tags[ curDesk ][ curHaz ][ curTag ],
    	    	      "new" ) == 0 ) {
       	    sprintf( value, "%d%s", pggfawp_getNextTag(), _desks[ curDesk ] );
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
    }
    /* Set the Cycle as a tag in VGF File*/
    pgcycle_getCycle ( day, cycle, &iret);
    cvg_setFld ( el, TAG_GFA_CYCLE,
                         cycle, &ier );

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
		if ( iopr == FUNC_MULTISEL ) {
		    if ( strcmp ( tmpStr, " ")  == 0 ) {
			cvg_setFld ( el, TAG_GFA_STATUS, issoptValue, &ier );
		    }
		    else {
			cvg_setFld ( el, TAG_GFA_STATUS, tmpStr, &ier );
		    }
	        }
		else {
                    cvg_setFld ( el, TAG_GFA_STATUS, tmpStr, &ier );
		}
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
 *  Get OVERRIDE issue time flag. 
 */
    cvg_getFld ( &_gfaElm, TAG_GFA_OVERRIDE_TM, value, &ier );		
    if ( ier == 0 ) {
	cvg_setFld ( el, TAG_GFA_OVERRIDE_TM, value, &ier );		
    }

/*
 *  Get the hazard panel information
 */
    pggfawp_getPanel2Attr ( el );
    if ( iopr == FUNC_MULTISEL ) {
        cvg_getFld ( el, "<Type>", typeText1, &ier );
	if ( (strcasecmp (_areaTyp[_areaTypeIndex], "CLD_TOPS") != 0) &&
	     (strcasecmp (_areaTyp[_areaTypeIndex], "TS") != 0)) {
	    if ( typeText1[0] == '\0' && typeText[0] != '\0') {
	        cvg_setFld ( el, "<Type>", typeText, &ier );
	    }
	}
        cvg_getFld ( el, TAG_GFA_TOP, topValue1, &ier );
        if ( topValue1[0] == '\0'&& topValue[0] != '\0' ) {
	    cvg_setFld ( el, TAG_GFA_TOP, topValue, &ier );
	}
        cvg_getFld ( el, TAG_GFA_BOTTOM, bottValue1, &ier );
        if ( bottValue1[0] == '\0' && bottValue[0] != '\0' ) {
	    cvg_setFld ( el, TAG_GFA_BOTTOM, bottValue, &ier );
	}
        if ( strcasecmp (_areaTyp[_areaTypeIndex], "TURB") == 0 ||
             strcasecmp (_areaTyp[_areaTypeIndex], "TURB-HI") == 0 ||
             strcasecmp (_areaTyp[_areaTypeIndex], "TURB-LO") == 0 ) {
 	    cvg_getFld ( el, "<DUE TO>", tmp1Value1, &ier );
	    if (strcasecmp (tmp1Value1, "N/C" ) == 0 ) {
	        cvg_setFld ( el, "<DUE TO>", tmp1Value, &ier );
	    }
	} 
        else if ( strcasecmp (_areaTyp[_areaTypeIndex], "CLD") == 0 ) {
            cvg_getFld ( el, "<Coverage>", tmp1Value1, &ier );
            if ( strcasecmp ( tmp1Value1, "N/C" ) == 0 ) {
                cvg_setFld ( el, "<Coverage>", tmp1Value, &ier );
            }
  	}
        else if ( strcasecmp (_areaTyp[_areaTypeIndex], "SFC_WND") == 0 ) {
            cvg_getFld ( el, "<Speed>", tmp1Value1, &ier );
            if (strcasecmp ( tmp1Value1, "N/C" ) == 0 ) {
                cvg_setFld ( el, "<Speed>", tmp1Value, &ier );
            }
	}
	else if ( strcasecmp (_areaTyp[_areaTypeIndex], "FZLVL") == 0 ) {
            cvg_getFld ( el, "<Contour>", tmp1Value1, &ier );
            if (strcasecmp ( tmp1Value1, "N/C" )  == 0 ) {
                cvg_setFld ( el, "<Contour>", tmp1Value, &ier );
            }
	    cvg_getFld ( el, "<Contour>", tmp1Value1, &ier );
            if ( strcasecmp ( tmp1Value1 , "Open" ) == 0 ) {
		el->hdr.closed = 0;
	    }
	    else {
		el->hdr.closed = 1;
	    }
            cvg_getFld ( el, "<Level>", tmp2Value1, &ier );
            if (strcasecmp (tmp2Value1, "N/C" ) == 0 ) {
                cvg_setFld ( el, "<Level>", tmp2Value, &ier );
            }
            cvg_getFld ( el, TAG_GFA_FZLRANGE, tmp3Value1, &ier );
            if (strcasecmp (tmp3Value1, "N/C" ) == 0 ) {
                cvg_setFld ( el, TAG_GFA_FZLRANGE, tmp3Value, &ier );
            }
	}
	else if ( strcasecmp (_areaTyp[_areaTypeIndex], "TS") == 0 ) {
            cvg_getFld ( el, "<Category>", tmp1Value1, &ier );
            if (strcasecmp (tmp1Value1, "N/C" ) == 0 ) {
                cvg_setFld ( el, "<Category>", tmp1Value, &ier );
            }
            cvg_getFld ( el, "<Frequency>", tmp2Value1, &ier );
            if (strcasecmp (tmp2Value1, "N/C" ) == 0 ) {
                cvg_setFld ( el, "<Frequency>", tmp2Value, &ier );
            }
	}
    }

/*
 *  Get the F/BB/A information (if any)
 */
    pggfawp_getPanel3Attr ( el );
}
    
/*=====================================================================*/

void pggfawp_setAttr ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_setAttr							*
 *									*
 * This function sets the attributes as in the given el.		*
 *									*
 * void pggfawp_setAttr ( el )						*
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
 * E. Safford/SAIC	04/07	call pggfawp_setPanel3Attr()		*
 * E. Safford/SAIC	04/07	conditionally manage/unmanage panel3	*
 * J. Wu/SAIC		12/07	remove verification for GFA' smears	*
 * J. Wu/SAIC		03/08	move fcst hour verification to creating	*
 * B. Yin/SAIC		07/08	handle 20 KT surface wind		*
 * J. Wu/SAIC		07/08	set override issue flag	& do not change	*
 *				the number of points in _gfaElm		*
 * L. Hinson/AWC        09/22   Use _fcsthrAndUTC list for pulldown     *
 *                              instead of _fcsthr                      *
 ***********************************************************************/
{
    int			ier, ii, areatyp, issoptIndex, category;
    char		temp_str[10];
    char                value[32];

    Boolean             fcsthrInMenu = False, managePanel3 = False;
    int iopr,curDesk,curHaz;
/*---------------------------------------------------------------------*/
/*
 *  Set the area type menu options 
 */
    cvg_getFld ( el, TAG_GFA_AREATYPE, value, &ier );
    areatyp = 0;

    if ( strcasecmp( value, "SFC_WND20") == 0 ) strcpy( value, "SFC_WND" );

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
    iopr = pgpalw_getCurOperId();
    if ( iopr != FUNC_MULTISEL ) { 
        cvg_getFld ( el, TAG_GFA_STATUS, value, &ier );
    }
    else {
	strcpy ( value, " " );
    }
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
    if ( iopr != FUNC_MULTISEL ) {
        cvg_getFld ( el, TAG_GFA_FCSTHR, value, &ier );
    }
    else {
        strcpy ( value, "Other" );
    }
    if ( strlen (value) > (size_t) 0 ) {
      for ( ii = 0; ii < _nFcsthr; ii++ ) {
        if (strcmp ( value, _fcsthr[ii] ) == 0 ) { 
          fcsthrInMenu = True;         
          pgutls_setOptMenu ( _fcsthrAndUTC[ii], _fcsthrAndUTC, 
            _nFcsthr, &_fcsthrStrc );
          _currFcsthr = _fcsthrStrc.current;
          break;
        }
      }
    }
            
    if ( !fcsthrInMenu ) {

       pgutls_setOptMenu ( "Other",
    			_fcsthrAndUTC, _nFcsthr, &_fcsthrStrc );

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

       pggfawp_setDfltTag( _fcsthr[ _currFcsthr ] );

       pgutls_setOptMenu ( _desks[ _deskStrc.current ], _desks, _nDesks, &_deskStrc );

    }
    else {

/* 
 *  set menu from the loading element. 
 */
	if ( iopr == FUNC_MULTISEL ) {
	    strcpy ( value," " );
	}
	else
	{
	    cvg_getFld ( el, TAG_GFA_TAG, value, &ier );
	}
	pggfawp_setTagMenu( value );
	if ( iopr == FUNC_MULTISEL ) {
    	    curDesk = _deskStrc.current;
    	    curHaz = _areaTypStrc.current;
 	    
	    pgutls_setOptMenu ( _tags[ curDesk ][ curHaz ][ 0 ],
                           _tags[ curDesk ][ curHaz ],
                           _nTags[ curDesk ][ curHaz ],
                           &_tagStrc[ curDesk ][ curHaz ] );

       	    _tagStrc[ curDesk ][ curHaz ].current = 0;
	}
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
 *  Set override issue time flag. 
 */
    cvg_getFld ( el, TAG_GFA_OVERRIDE_TM, value, &ier );		
    if ( ier == 0 ) {
        cvg_setFld ( &_gfaElm, TAG_GFA_OVERRIDE_TM, value, &ier );
    }

/*
 *  Set the hazard panel information
 */
    pggfawp_setPanel2Attr ( el );

/*
 *  Set the F/BB/A panel information
 */
    pggfawp_setPanel3Attr ( el );
    
    managePanel3 = False;
    category = pggfawp_getCategoryType();
    if( category == GFA_FBBA_AIRMET || category == GFA_FBBA_OUTLOOK ) {
        managePanel3 = True;             
    }
    pggfawp_managePanel3 ( managePanel3 );
        
}

void pggfawp_resetFcstHrOptMenu( char *cycle) 
/***************************************************************************
  pggfawp_resetFcstHrOptMenu
  
  This function updates the _fcsthrAndUTC array, whenever the cycle time
  is changed.  It is called by nmap_pgcycle::pgcycle_updateCycleTags()
  
  void pggfawp_resetFcstHrOptMenu ( cycle )
  
  Input parameters:
    *cycle   char   2-digit Cycle Time String
    
  Output parameters:
             NONE.
             
  Log:
  L. Hinson/AWC    09/09  Initial Coding
***************************************************************************/
{
  int ier;
  char str[ STD_STRLEN * 2];
  ctb_gfagfhrAndUTC ( ";", cycle, &_nFcsthr, str, &ier );
  pggfawp_fillStrArray ( MAXNOPT, MAX_FCSTHRSTR, str, _fcsthrAndUTC );       
  pgutls_setOptMenu( _fcsthrAndUTC[0], _fcsthrAndUTC, _nFcsthr, &_fcsthrStrc );
}

/*=====================================================================*/

void pggfawp_setFrom ( int np, float *lat, float *lon )
/************************************************************************
 * pggfawp_setFrom							*
 *									*
 * This function sets the from line based on the new coordinates.	*
 *									*
 * void pggfawp_setFrom ( np, lat, lon )				*
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
    if ( pggfawp_isClosed () ) {
       if ( np > 0 ) {
	  clo_from ( GFA_ELM, SIGTYP_AREA, np, 4, lat, lon, 100, 
 			fmline, &ier);
       }
    }
    XtVaSetValues ( _fromText, XmNvalue, fmline, NULL );
}

/*=====================================================================*/

void pggfawp_reorderGFA ( int areaType, int npts, 
			 float *lat, float *lon, int *iret )
/************************************************************************
 * pggfawp_reorderGFA							*
 *									*
 * This function reorders the points of a GFA element in a CW direction *
 * unless the subtype is GFA_HAZARD_FZLVL_SFC or GFA_HAZARD_FZLVL.	* 
 * For all FZLVLs there is no change to point order.              	*
 *									*
 * If an error is encountered there will be no effort made to reorder   *
 * the points.								*
 *									*
 * void pggfawp_reorderGFA ( areaType, inPts, *inLat, *inLon, *outLats, 	*
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

void pggfawp_saveNew ( int np, float *lats, float *lons )
/************************************************************************
 * pggfawp_saveNew							*
 *									*
 * This function saves a new GFA element.				*
 *									*
 * void pggfawp_saveNew ( np, lats, lons )				*
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
 * B. Yin/SAIC		02/08	disable mvTxt btn if autoplace is true	*
 * B. Yin/SAIC		04/08	don't show thw 3 line warning before  	*
 *				finishing the GFA text box		*
 * L. Hinson/AWC        09/09   Drop FromLine PushButton Logic          *
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
    if ( ( np < 3 && pggfawp_isClosed() ) ||
         ( np < 2 && !pggfawp_isClosed() ) ) {
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
    
    pggfawp_getAttr ( &_gfaElm );

    _gfaElm.elem.gfa.info.npts = np;

/*
 *   Reorder the lat/lon points into a clockwise orientation.
 */
    strcpy( haz, _areaTyp[ _areaTypeIndex ] );
    areaType = pggfawp_getHazardType( haz );

    pggfawp_reorderGFA ( areaType, np, lats, lons, &ier );
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
    pggfawp_checkHours ( &_gfaElm );
  
/*
 *  Note that np can change inside the pgsmear_snapEl, which is called
 *  by pggfawp_checkHours.  Ok, I agree that this side effect is dreadful.  
 *  This note will mark this landmine for clean up when we restructure 
 *  the code.
 */ 
    np = _gfaElm.elem.gfa.info.npts;

/*
 *  Set the from line.
 */
    pggfawp_setFrom ( np, &_gfaElm.elem.gfa.latlon[0],
                         &_gfaElm.elem.gfa.latlon[np] );

/*
 *  Set the arrow endpoint
 */
    cvg_todev( &_gfaElm, &np, xp, yp, &ier );
    if ( pggfawp_isClosed() ) {
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
    pggfawp_addTag ( atoi( tag ) );
                                                                                               
/*
 *  If the auto placement flag for GFA text box is NOT set, do NOT
 *  show the warning box at this step. The warning box will pop up 
 *  after the GFA text box is done.
 */
    if ( !_autoPlacement ) {
                                                                                               
        _show3LinesWarning = False;
                                                                                               
    }

/*
 *  Save and plot the GFA frame. The attribute text box
 *  will be added later.
 */
    pgvgf_saveNewElm ( NULL, sys_M, &_gfaElm, np, &_gfaElm.elem.gfa.latlon[ 0 ], 
    		       &_gfaElm.elem.gfa.latlon[ np ], TRUE, &loc, &ier );
    
    pgvgf_dsp ( loc, &ier );
    geplot ( &ier );

/*
 *  Reset the warning box falg so it pops up after finishing the text box.
 */
    _show3LinesWarning = True;
    
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
        pggfawp_setText ();
    } else {
        pgundo_newStep ();
        pgundo_storeThisLoc ( loc, UNDO_ADD, &ier );
        pgundo_endStep ();

/*
 *  Enable 'Move Text' buttons
 *  If autoPlacement flag is true, disable 'Move Text' button
 */
        XtVaSetValues ( _txtMovePb, XmNsensitive, !_autoPlacement, NULL );
    }

    cvg_freeElPtr( &_gfaElm );
}

/*=====================================================================*/

static void pggfawp_setText ( void )
/************************************************************************
 * pggfawp_setText							*
 *									*
 * Sets the ghosting and event handling for the GFA attribute text box.	*
 *									*
 * static  void pggfawp_setText ()					*
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
 * B. Yin/SAIC		02/08	disable mvTxt btn if autoplace is true	*
 * L. Hinson/AWC        09/09   Drop From Line Push Button Logic        *
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

    mcanvw_setDynamicFunc ( (XtEventHandler)&pggfawp_txtPressEh, 
    			    (XtEventHandler)&pggfawp_txtDragEh, 
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
 *  Enable 'Move Text' buttons
 *  If autoPlacement flag is true, disable 'Move Text' button
 */
    XtVaSetValues ( _txtMovePb, XmNsensitive, !_autoPlacement, NULL );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_txtPressEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pggfawp_txtPressEh							*
 *									*
 * Press event handler for placing the GFA attribute text box.		*
 *									*
 * static void pggfawp_txtPressEh (wid, clnt, event, ctdr )		*
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
 * E. Safford/SAIC	04/07	rm call to pggfawp_getAttr()		*
 * J. Wu/SAIC		04/08	Free _gfaElm only if text is inactive	*
 * J. Wu/SGT		06/14	Update the type selection for C&V 	*
 ***********************************************************************/
{
    int		ntxt = 1, np, ier, xoff, yoff, ii, location, num;
    char	value[32], *value1;
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
 *  Update the type selection for C&V.
 */
    if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "C&V" ) == 0 ) {
        value1 = XmTextGetString( _currentPanel2->typeText );
        cvg_setFld (&_gfaElm , "<Type>", value1, &ier );
    }

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
            pggfawp_setText ();
        }
        else {
            pghdlb_deselectAll ();
	    pgnew_setArmDynamic ();
            cvg_freeElPtr ( &_gfaElm );
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
            cvg_freeElPtr ( &_gfaElm );
	}	
    }     
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_txtDragEh ( Widget wid, XtPointer clnt, 
					XEvent *event, Boolean *ctdr )
/************************************************************************
 * pggfawp_txtDragEh							*
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

static void pggfawp_setAddMode ( Boolean inAddMode )
/************************************************************************
 * pggfawp_setAddMode							*
 *									*
 * This function sets the GFA window to the new given mode.		*
 *									*
 * static void pggfawp_setAddMode ( inAddMode )				*
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

Boolean pggfawp_isAddMode ( void )
/************************************************************************
 * pggfawp_isAddMode							*
 *									*
 * This function queries if the GFA window is in the "Add" mode.	*
 *									*
 * Boolean pggfawp_isAddMode ( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *    None								*
 *									*
 * Return parameters:							*
 *    pggfawp_isAddMode() Boolean	Is/Is not in "Add" mode		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		08/04	initial coding				*
 ***********************************************************************/
{
    return ( _addMode );    
}

/*=====================================================================*/

void pggfawp_setType ( const char *newType )
/************************************************************************
 * pggfawp_setType							*
 *									*
 * This function sets the current area type in the menu to the new type	*
 *									*
 * void pggfawp_setType ( newType )					*
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

void pggfawp_setHour ( const char *newHour )
/************************************************************************
 * pggfawp_setHour							*
 *									*
 * This function sets the current forecast hour to the new hour.	*
 *									*
 * void pggfawp_setHour ( newHour )					*
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
 * E. Safford/SAIC	05/07	set panel 3 according to newHour	*
 * E. Safford/SAIC	06/07	avoid source/dest warning with timecmp	*
 * B. Yin/SAIC		12/07	call pggfawp_resetForSubtype to set clr	*
 * J. Wu/SAIC		03/08	do not use panel 3 for editing smears	*
 * L. Hinson/AWC        09/09   Use _fcstHrAndUTC list instead of       *
 *                              _fcsthr in call to pgutils_setOptMenu   *
 ***********************************************************************/
{
    int 	ii, len, ier;
    char	timecmp[10], *ptr = NULL;
    Boolean	usePanel3 = False;
/*---------------------------------------------------------------------*/
  
    timecmp[0] = '\0';

    strcpy ( timecmp, newHour );
    len = strlen ( timecmp );
    if ( timecmp[len-1] == '+' ) {
	timecmp[ len-1 ] = '\0';
    }

/*
 *  If the time contains a '-' and we are not editing a smear, then it is 
 *  an F/BB/A element, not a snapshot.  Bring up the F/BB/A panel (panel 3).
 */
    ptr = strchr( newHour, '-' );
    if( ptr != NULL ) {
        usePanel3 = True;
        if ( _addMode && _editSmear ) {
            usePanel3 = False;	
	}
    } 
    
    for ( ii = 0; ii < _nFcsthr; ii++ ) {
        
/*
 *  If a match found, set the forecast hour to the new hour. 
 */
        if ( strcasecmp ( timecmp,  _fcsthr[ii] ) == 0 ) {
	    pgutls_setOptMenu ( _fcsthrAndUTC[ii],
    			        _fcsthrAndUTC,
				_nFcsthr,
				&_fcsthrStrc );
            _currFcsthr = _fcsthrStrc.current;

	    if ( XtIsManaged(_fcsthrText) ) XtUnmanageChild ( _fcsthrText );

            pggfawp_managePanel3( usePanel3 );
	    pggfawp_setDfltTag( _fcsthr[ _currFcsthr ] );
	    pggfawp_resetForSubtype( &ier );

            break;
        }
    }    	
}

/*=====================================================================*/

static void pggfawp_cleanup ( void )
/************************************************************************
 * pggfawp_cleanup							*
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
 * E. Safford/SAIC	04/07	fix memory leaks			*
 * B. Yin/SAIC		02/08	disable mvTxt btn if autoplace is true	*
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
 *  If autoPlacement flag is true, disable 'Move Text' button
 */
    if ( _txtActive )  {
        _txtActive = False;
        XtSetSensitive ( _txtMovePb, !_autoPlacement );        
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

                if ( pggfawp_isClosed() ) {
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

static void pggfawp_createPanel1 ( void )
/************************************************************************
 * pggfawp_createPanel1                                                  *
 *                                                                      *
 * This routine creates the top panel of the GFA dialog box.            *
 *                                                                      *
 * void pggfawp_createPanel1 ( )                                         *
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
 * B. Yin/SAIC          07/07   move NxmClrW_popup to pggfaw_colorZeroCb*
 * J. Wu/SAIC		03/08	add two callbacks for fcst hr		*
 * L. Hinson/AWC        09/09   Remove _fromLinePb Widget creation and  *
 *                              associated callbacks.  Reposition Move  *
 *                              Text Button.                            *
 * L. Hinson/AWC        09/09   Adjust Tag, Desk, Issue Type Pull-downs;*
 *                              Adjust Move Text and Color Pushbutton to*
 *                              accompany larger width with FcstHr      *                  
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

    pggfawp_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_areaTypStrc.current, "Hazard:",
			(XtCallbackProc)pggfawp_areatypPbCb, &_areaTypStrc.form, 
			&_areaTypStrc.label, &_areaTypStrc.menu, 
			_areaTypStrc.pb, 0, -5, NULL, XmVERTICAL );
    
/*
 *  Forcast Hour pulldown menu
 */     
    _fcsthrStrc.current = 0;
    pggfawp_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_fcsthrStrc.current, "Fcst Hr:",
			(XtCallbackProc)pggfawp_fcsthrPbCb, &_fcsthrStrc.form, 
			&_fcsthrStrc.label, &_fcsthrStrc.menu, 
			_fcsthrStrc.pb, 120, -5, NULL, XmVERTICAL );
/*
 *  Create tag pulldown menu
 */
    _tagStrc = (struct optMenuStrc(*)[ MAXNOPT ] )
       		malloc( _nDesks * sizeof( struct optMenuStrc [ MAXNOPT ] ));


    for ( nn = 0; nn < _nDesks; nn++ ) {

        for ( mm = 0; mm < _nAreatyp; mm++ ) {

        _tagStrc[ nn ][ mm ].current = 0;

	    pggfawp_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_tagStrc[ nn ][ mm ].current, "Tag:",
			(XtCallbackProc)pggfawp_tagCb, &_tagStrc[ nn ][ mm ].form, 
			&_tagStrc[ nn ][ mm ].label, &_tagStrc[ nn ][ mm ].menu, 
			_tagStrc[ nn ][ mm ].pb, 0, -5, NULL, XmVERTICAL );

	    pgutls_setOptMenu ( _tags[ nn ][ mm ][ 0 ], 
       			_tags[ nn ][ mm ], 
    			_nTags[ nn ][ mm ], 
			&_tagStrc[ nn ][ mm ] );

	    XtVaSetValues ( _tagStrc[ nn ][ mm ].form, 
    			XmNx,		  215,
    			XmNwidth,	  75,
                	NULL );
	}
    }
 
/*
 *  Create desk pulldown menu
 */
    _deskStrc.current = 0;
    pggfawp_createOptionMenu ( form1, MAXNOPT, 
			(XtPointer)&_deskStrc.current, "Desk:",
			(XtCallbackProc)pggfawp_deskCb, &_deskStrc.form, 
			&_deskStrc.label, &_deskStrc.menu, 
			_deskStrc.pb, 290, -5, NULL, XmVERTICAL );

/* Issue Buttons pulldown menu */
    _issoptStrc.current = 0;
    pggfawp_createOptionMenu( form1, MAXNOPT,
                          (XtPointer)&_issoptStrc.current, "Issue Type:",
                          (XtCallbackProc)pggfawp_optPbCb, &_issoptStrc.form,
                          &_issoptStrc.label, &_issoptStrc.menu,
                          _issoptStrc.pb, 350, -5, NULL, XmVERTICAL);
		
    _fcsthrText = XtVaCreateManagedWidget ( "fsct_text",
    		    xmTextWidgetClass,		form1,
		    XmNcolumns,			9,
		    XmNmaxLength,		9,
		    XmNeditable,		TRUE,
		    XmNvalue,			"",
		    XmNtopAttachment,		XmATTACH_FORM,
		    XmNtopOffset,		oneRow + 5,
		    XmNleftAttachment,		XmATTACH_FORM,
		    XmNleftOffset,		130,
		    NULL );
    
    XtAddCallback ( _fcsthrText, XmNlosingFocusCallback, 
      		    (XtCallbackProc)pggfawp_fcsthrTxtCb, NULL );

    XtAddCallback ( _fcsthrText, XmNmodifyVerifyCallback, 
                    (XtCallbackProc)pggfawp_vrfyFcstHrCb, NULL );
    
    XtAddCallback ( _fcsthrText, XmNvalueChangedCallback, 
      		    (XtCallbackProc)pggfawp_addFcstHrColon, NULL );	 

/*
 *  Text move button
 */
                
    _txtMovePb = XtVaCreateManagedWidget ( "Move Text",
		xmPushButtonWidgetClass,	form1,
		XmNx,				443,  
		XmNy,				21,
		XmNheight,			32,
		XmNwidth,			120,  
		NULL ); 

    XtAddCallback ( _txtMovePb, XmNactivateCallback,
		    (XtCallbackProc)pggfawp_txtMoveCb, (XtPointer) NULL );
    
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
		XmNx,				536,
		XmNwidth,			25,
		XmNheight,			20,
		NULL );  

    XtAddCallback ( _colorPb, XmNactivateCallback,
                    (XtCallbackProc)pggfawp_colorZeroCb, NULL );

    XtAddCallback ( _colorPb, XmNactivateCallback, 
		    (XtCallbackProc)pggfawp_optPbCb, NULL );

}

/*=====================================================================*/

static void pggfawp_createOptionMenu ( Widget parent, int nbuttons,
				XtPointer pstart_item, char label_str[], 
				XtCallbackProc callback, Widget *form, 
				Widget *label, Widget *menu, Widget pbs[],
                                int posX, int posY,
				char *btnstrs[], unsigned char orient ) 
/************************************************************************
 * pggfawp_createOptionMenu						* 
 *									*
 * This function creates an option menu.				*
 *									*
 * static void pggfawp_createOptionMenu (parent, nbuttons, pstart_item,	*
 *				 label_str, callback, form, label,	*
 *				 menu, pbs, posX, posY,btnstrs)	        *
 *									*
 * Input parameters:							*
 *	parent		Widget	the parent widget			*
 *	nbuttons	int	number of menu buttons			*
 *	pstart_item	XtPointer  pointer to starting menu item	*
 *	label_str[]	char	label string for label			*
 *	callback	XtCallbackPro	push button callback function	*
 *	*form		Widget	the underlying form			*
 *	*label		Widget	the side label				*
 *	*menu		Widget	the option menu				*
 *	pbs[]		Widget	the menu push buttons			*
 *      posX            int     the position in the X                   *
 *      posY            int     the position in the Y                   *
 *	*btnstrs[]	char	the menu push button labels		*
 *	orient		XmNorientation  label/menu orientation 		*
 *					XmHORIZONTAL || XmVERTICAL	*
 *									*
 **									*
 * Log:									*
 *   L. Hinson/AWC      9/05    Initial Coding Hybrid from              *
 *                              pgutls_createOptionMenu &               *
 *                              pgtca_createOptMenu                     *
 *   E. Safford/SAIC	04/07	add orientation param			*
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
                   XmNorientation,      orient,
                   XmNx,                posX,
                   XmNy,                posY,
		   NULL);

    XmStringFree (xmstr);

    XtManageChild (*menu);

}

/*=====================================================================*/

static void pggfawp_createPanel2 ( void )
/************************************************************************
 * pggfawp_createPanel2                                                  *
 *                                                                      *
 * This routine creates the middle panel of the GFA dialog box.         *
 *                                                                      *
 * void pggfawp_createPanel2 ( )                                         *
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
 * B. Yin/SAIC          06/08   Create C&V right panel seperately       *
 * B. Yin/SAIC          06/08   Change layout to avoid overlop	        *
 * L. Hinson/AWC        09/09   Drop From Line Push Button logic        *
 * L. Hinson/AWC        11/09   Add ability to parse "top" and "bottom" *
 *                              input fields.  Call event handlers      *
 *                              pggfawp_fillSingleFLCb, and             *
 *                              pggfawp_verifySingleFLCb on these fields*
 * X. Guo/CWS		01/10   Add N/C descriptor for multi-selected 	*
 *				GFA elements				*
 ***********************************************************************/
{
    int 	ii, jj, ier, inputIndex, pdIndex, checkboxIndex; 
    int         nLabel, nChoices, oneRow = 36, rows, cols, nn, popupIndex;
    char	typeStr[ 32 ], desc[ MAXTBLNAME ], choice[ STD_STRLEN ];
    char	*tmpStr, desc1[ MAXTBLNAME ];

    Arg    	args[ 10 ];
    Widget	typePane, typeRowCol;
    Widget	addTypeBtn;
    Widget	wPulldown, wPushButton;
    XmString	xmStr;
    int		iopr;
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
        
    }
    iopr = pgpalw_getCurOperId();

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
		if ( _panel2[ ii ].nDesc       != 0 ) {
		    pggfawp_chkNCDesc4MSel (_areaTyp[ _areaTypStrc.current ], &ii);		
		}
    	       _currentPanel2 = &_panel2[ ii ];

               if (_addMode) {

                 for (jj = 0;jj < _panel2 [ ii ].nInputField; jj++) {

                   XtVaSetValues( _panel2[ ii ].inputField[ jj ].text,XmNvalue,"",NULL);

		   if ( ( strcasecmp ( _panel2[ ii ].haz, "ICE" ) == 0 ) && 
		   	( jj == _FZLIndex ) ) {

			pggfawp_setFzl ( OFF );

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
	
      if ( strcasecmp( _areaTyp[ _areaTypStrc.current ], "C&V" ) == 0 ) {

        pggfawp_createCvPanel( nLabel );

      }
      else {
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
				XmNwidth,		208,
                    		XmNrecomputeSize,       False,
                		NULL );
	
    		XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text,
				 XmNmodifyVerifyCallback, 
		    		(XtCallbackProc)pggfawp_optPbCb, (XtPointer) NULL );

		if ( strcasecmp ( desc, "top/bottom:" ) == 0 ) {
    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNlosingFocusCallback, 
		    		   (XtCallbackProc)pggfawp_fillFLCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
		   		   XmNvalueChangedCallback, 
    		   		   (XtCallbackProc)pggfawp_changedFLCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNmodifyVerifyCallback, 
		    		   (XtCallbackProc)pggfawp_verifyFLCb, NULL );

		   if ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) {

			_FLIndex = inputIndex;

		   }

		}
                
                else if ( strcasecmp ( desc, "top:") == 0 ||
                          strcasecmp ( desc, "bottom:") == 0 ) {
                   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNlosingFocusCallback, 
		    		   (XtCallbackProc)pggfawp_fillSingleFLCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNmodifyVerifyCallback, 
		    		   (XtCallbackProc)pggfawp_verifySingleFLCb, NULL );
                }

		else if ( strcasecmp ( desc, "fzl top/bottom:" ) == 0 ) {

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNlosingFocusCallback, 
		    		   (XtCallbackProc)pggfawp_fillFzlCb, NULL );

    		   XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text, 
				   XmNmodifyVerifyCallback, 
		    		   (XtCallbackProc)pggfawp_verifyFzlCb, NULL );

		   if ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) {

			_FZLIndex = inputIndex;

		   }

		   pggfawp_setFzl( OFF );

		}

		else if ( strcasecmp ( desc, "fzl range:" ) == 0 ) {

		    _FZLRange = inputIndex;

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
				(XtCallbackProc)pggfawp_setTypeTextCb, 
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
				XmNtopOffset,		ii * oneRow + 5,
				XmNleftAttachment,	XmATTACH_FORM,
		       		NULL );

/*
 *  Create the push button that brings up the popup dialog.
 */
                addTypeBtn = XtVaCreateManagedWidget( "Add/Remove Types",
                                xmPushButtonWidgetClass, _panel2[ _nPanel2 ].form,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           (rows == 0) ? 0 : rows * oneRow,
                                XmNleftAttachment,      XmATTACH_WIDGET,
                                XmNleftWidget,          _panel2[ _nPanel2 ].popupLabel[ popupIndex ],
                                XmNmarginWidth,         5,
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
                                (XtCallbackProc)pggfawp_popupTypeDlgCb,
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
				    (XtCallbackProc) pggfawp_setTypeTextCb, 
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
				(XtCallbackProc)pggfawp_closeTypeDlgCb, 
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
			desc, (XtCallbackProc)pggfawp_optPbCb, 
			&_panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
			&_panel2[ _nPanel2 ].descStrc[ pdIndex ].label, 
			&_panel2[ _nPanel2 ].descStrc[ pdIndex ].menu, 
		         _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb, NULL );


             	XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		rows * oneRow,
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

		if ( iopr == FUNC_MULTISEL) {
		    if ( pggfawp_addNCDesc ( _areaTyp[ _areaTypStrc.current ], desc1 )) {
		    	strcpy (choice, "N/C" );
                    	xmStr = XmStringCreateLocalized ( choice );

                    	XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ],
                              	XmNlabelString,   xmStr,
                              	XmNrecomputeSize,       True,
                              	NULL );

                    	XmStringFree ( xmStr );


                    	XtManageChild ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ] );
		    }
		    jj ++;
		}
            	for (; jj < MAXNOPT; jj++) {
		    if ( XtIsManaged ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ] ) ) {
			XtUnmanageChild ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ] );
		    }
                }
	    	_panel2[ _nPanel2 ].nDesc++;
	   }
       }	/* loop to create user input or pulldown menu */
      }
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

static void pggfawp_createPanel3 ( void )
/************************************************************************
 * pggfawp_createPanel3                                                 *
 *                                                                      *
 * This routine creates the bottom panel of the GFA F/BB/A dialog box.  *
 *                                                                      *
 * void pggfawp_createPanel3 ( )                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                                                                      *
 *      		None                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	04/07   Created                                 *
 * E. Safford/SAIC	04/07   add panel3TextCb() to each text widget  *
 * E. Safford/SAIC	07/07	rm cycle field				*
 * J. Wu/SAIC		12/07	allow '.' & '-' in _beginText/_endText	*
 * J. Wu/SAIC		12/07	allow '.' & '-' in _areaText		*
 * B. Yin/SAIC		04/08	change states label to a button		*
 * J. Wu/SAIC		07/08	widen the state list text field		*
 ***********************************************************************/
{
    Widget	form3;
    Widget	areaLabel;
    Widget	beginLabel, endLabel;
    Arg    	args[ 20 ];
    int		nn;
/*---------------------------------------------------------------------*/
/*
 *  Build the panel 3 form.     
 */
    _frameFBBA = XtVaCreateWidget( "frame",
    			xmFrameWidgetClass, 	_gfaForm,
			XmNtopAttachment,	XmATTACH_WIDGET,
  			XmNtopWidget,		_editPane, 
			XmNleftAttachment,	XmATTACH_FORM,
			XmNrightAttachment,	XmATTACH_FORM,
			XmNshadowType,		XmSHADOW_IN,
			XmNborderWidth,		2,
			NULL );

    form3 = XtVaCreateWidget ("form3",
                        xmFormWidgetClass, 	_frameFBBA, 
			XmNtopAttachment,	XmATTACH_FORM,
			NULL );

/*
 *  Build the Area text field.
 */ 
    areaLabel = XtVaCreateManagedWidget ( "Area:",
                       	xmLabelWidgetClass, 	form3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		10,
    			XmNleftAttachment,	XmATTACH_FORM,
			XmNleftOffset,		10,
		        NULL );

    _areaText = XtVaCreateManagedWidget ( "area_text",
                	xmTextWidgetClass,      form3,
  			XmNleftAttachment,	XmATTACH_WIDGET,
                        XmNleftWidget,		areaLabel,
			XmNleftOffset,		15,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		5,
                	XmNeditable,            TRUE,
			XmNwidth,		90,
                	XmNrecomputeSize,       False,
                	XmNvalue,               "",
                	NULL );
    
    XtAddCallback ( _areaText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyUpperCaseCb, NULL );

    XtAddCallback ( _areaText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyLmtPunctCb, NULL );

    XtAddCallback ( _areaText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyNoDigitsCb, NULL );
  
    XtAddCallback ( _areaText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pggfawp_verifyAreaCb, NULL );

    XtAddCallback ( _areaText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pggfawp_panel3TextCb, NULL );

/*
 *  Build the State List button and text field.
 */ 
    _statesBtn = XtVaCreateManagedWidget( "States:",
                xmPushButtonWidgetClass,        form3,
		XmNtopAttachment,	XmATTACH_FORM,
		XmNtopOffset,		68,
    		XmNleftAttachment,	XmATTACH_FORM,
		XmNleftOffset,		3, 
                NULL ); 
                
    XtAddCallback ( _statesBtn, XmNactivateCallback,
			(XtCallbackProc)pggfawp_statesListCb, NULL );

    nn = 0;
    XtSetArg( args[nn], XmNrows,			   1 ); nn++;
    XtSetArg( args[nn], XmNcolumns, 			  40 ); nn++;
    XtSetArg( args[nn], XmNeditable, 		        True );	nn++;
    XtSetArg( args[nn], XmNvisualPolicy, 	  XmCONSTANT ); nn++;
    XtSetArg( args[nn], XmNscrollingPolicy,	  XmAUTOMATIC); nn++; 
    XtSetArg( args[nn], XmNscrollBarDisplayPolicy, XmAS_NEEDED); nn++;
    XtSetArg( args[nn], XmNleftAttachment,   XmATTACH_WIDGET ); nn++;
    XtSetArg( args[nn], XmNleftWidget,	  	  _statesBtn ); nn++;
    XtSetArg( args[nn], XmNleftOffset,	  	  	   3 ); nn++;
    XtSetArg( args[nn], XmNtopAttachment,      XmATTACH_FORM ); nn++;
    XtSetArg( args[nn], XmNtopOffset,                     68 ); nn++;
    XtSetArg( args[nn], XmNcursorPositionVisible,       True ); nn++;

    _stateText = XmCreateScrolledText( form3, "_stateText", args, nn );
    XtManageChild( _stateText );

    XtAddCallback ( _stateText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyUpperCaseCb, NULL );

    XtAddCallback ( _stateText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyNoPunctCb, NULL );

    XtAddCallback ( _stateText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyNoDigitsCb, NULL );

    XtAddCallback ( _stateText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pggfawp_panel3TextCb, NULL );
 

/*
 *  Build the conditions beginning text field.
 */ 
    beginLabel = XtVaCreateManagedWidget ( "Beginning:",
                       	xmLabelWidgetClass, 	form3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		10,
  			XmNleftAttachment,	XmATTACH_WIDGET,
                        XmNleftWidget,		_areaText,
			XmNleftOffset,		30,
		        NULL );

    nn = 0;
    XtSetArg( args[nn], XmNrows,			   1 ); nn++;
    XtSetArg( args[nn], XmNcolumns, 			  20 ); nn++;
    XtSetArg( args[nn], XmNeditable, 		        True );	nn++;
    XtSetArg( args[nn], XmNvisualPolicy, 	  XmCONSTANT ); nn++;
    XtSetArg( args[nn], XmNscrollingPolicy,	  XmAUTOMATIC); nn++; 
    XtSetArg( args[nn], XmNscrollBarDisplayPolicy, XmAS_NEEDED); nn++;
    XtSetArg( args[nn], XmNleftAttachment,   XmATTACH_WIDGET ); nn++;
    XtSetArg( args[nn], XmNleftWidget,	  	  beginLabel ); nn++;
    XtSetArg( args[nn], XmNleftOffset,	  	  	   3 ); nn++;
    XtSetArg( args[nn], XmNtopAttachment,      XmATTACH_FORM ); nn++;
    XtSetArg( args[nn], XmNtopOffset,                      5 ); nn++;
    XtSetArg( args[nn], XmNcursorPositionVisible,       True ); nn++;

    _beginText = XmCreateScrolledText( form3, "begin_text", args, nn );
    XtManageChild( _beginText );

    XtAddCallback ( _beginText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyUpperCaseCb, NULL );

    XtAddCallback ( _beginText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyLmtPunctCb, NULL );

    XtAddCallback ( _beginText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pggfawp_panel3TextCb, NULL );
 

/*
 *  Build the conditions ending text field.
 */ 
    endLabel = XtVaCreateManagedWidget ( "Ending:",
                       	xmLabelWidgetClass, 	form3,
			XmNtopAttachment,	XmATTACH_FORM,
			XmNtopOffset,		10,
  			XmNleftAttachment,	XmATTACH_WIDGET,
                        XmNleftWidget,		_beginText,
			XmNleftOffset,		15,
		        NULL );

    nn = 0;
    XtSetArg( args[nn], XmNrows,			   1 ); nn++;
    XtSetArg( args[nn], XmNcolumns, 			  20 ); nn++;
    XtSetArg( args[nn], XmNeditable, 		        True );	nn++;
    XtSetArg( args[nn], XmNvisualPolicy, 	  XmCONSTANT ); nn++;
    XtSetArg( args[nn], XmNscrollingPolicy,	  XmAUTOMATIC); nn++; 
    XtSetArg( args[nn], XmNscrollBarDisplayPolicy, XmAS_NEEDED); nn++;
    XtSetArg( args[nn], XmNleftAttachment,   XmATTACH_WIDGET ); nn++;
    XtSetArg( args[nn], XmNleftWidget,	  	    endLabel ); nn++;
    XtSetArg( args[nn], XmNleftOffset,	  	  	   3 ); nn++;
    XtSetArg( args[nn], XmNtopAttachment,      XmATTACH_FORM ); nn++;
    XtSetArg( args[nn], XmNtopOffset,                      5 ); nn++;
    XtSetArg( args[nn], XmNcursorPositionVisible,       True ); nn++;

    _endText = XmCreateScrolledText( form3, "end_text", args, nn );
    XtManageChild( _endText );

    XtAddCallback ( _endText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyUpperCaseCb, NULL );

    XtAddCallback ( _endText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pgutls_vrfyLmtPunctCb, NULL );

    XtAddCallback ( _endText, XmNmodifyVerifyCallback, 
		        (XtCallbackProc)pggfawp_panel3TextCb, NULL );
 

    XtManageChild ( form3 );
    XtManageChild ( _frameFBBA );
}

/*=====================================================================*/

static void pggfawp_getGuiInfo ( int *iret )
/************************************************************************
 * pggfawp_getGuiInfo							*
 *									*
 * This rountine reads in the GFA GUI infomation from a table.		*
 *									*
 * static void pggfawp_getGuiInfo ( iret )				*
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
 * L. Hinson/AWC        09/09   Added _fcsthrAndUTC array to store      *
 *                              forecast hours and valid times.  Add    *
 *                              calls to pgcycle_getCycle/              *
 *                              ctb_gfagfhrAndUTC to set the            *
 *                              _fcsthrAndUTC array                     *
 * X. Guo/CWS		01/10   Add a blank string for Desk and issOpt  *
 *				to support multi-selected GFA elements	*
 ***********************************************************************/
{
    int		ntags, ii, jj, ier;
    char	str[ STD_STRLEN * 2 ];
    char        day[3], cycle[3];
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
       pggfawp_fillStrArray ( MAXNOPT, MAX_AREATYPSTR, str, _areaTyp );		     

       ctb_gfagdesk ( ";", &_nDesks, str, &ier );
       pggfawp_fillStrArray ( MAXNOPT, MAX_TAGSTR, str, _desks);
/*
 * Add one blank item in desks
 */
       strcpy ( _desks[_nDesks]," ");
       _nDesks ++;

       ctb_gfagtag ( ";", &ntags, str, &ier );

       _tags = (char(*)[ MAXNOPT ][ MAXNOPT ][ MAXTBLNAME ])
       		malloc( _nDesks * sizeof( char[ MAXNOPT ][ MAXNOPT ][ MAXTBLNAME ] ));

       _nTags = (int(*)[ MAXNOPT ] )
       		malloc( _nDesks * sizeof( int[ MAXNOPT ] ));

       for ( ii = 0; ii < _nDesks; ii++ ) {

           for ( jj = 0; jj < _nAreatyp; jj++ ) {

               pggfawp_fillStrArray ( MAXNOPT, MAX_TAGSTR, str, _tags[ ii ][jj] );
	       _nTags[ ii ][ jj ] = ntags;

	   }
       }
       
       ctb_gfagiss ( ";", &_nIssopt, str, &ier );
       pggfawp_fillStrArray ( MAXNOPT, MAX_ISSOPTSTR, str, _issopt );
/*
 * Add one blank item in issopt
 */
       strcpy ( _issopt[_nIssopt]," ");
       _nIssopt ++;
   
       ctb_gfagfhr ( ";", &_nFcsthr, str, &ier );
       pggfawp_fillStrArray ( MAXNOPT, MAX_FCSTHRSTR, str, _fcsthr );       
       
       pgcycle_getCycle ( day, cycle, &ier );
       ctb_gfagfhrAndUTC ( ";", cycle, &_nFcsthr, str, &ier );
       pggfawp_fillStrArray ( MAXNOPT, MAX_FCSTHRSTR, str, _fcsthrAndUTC );       
 
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_fillFLCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfawp_fillFLCb							*
 *									*
 * Callback function is used to fill the flight levels.			*
 *									*
 * static void pggfawp_fillFLCb ( wid, clnt, cbs )			*
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
 * J. Wu/SAIC		02/08		Set limit for top & bottom	*
 * J. Wu/SAIC		03/08		Keep bottom if it is "FZL"	*
 ***********************************************************************/
{
    int		ier;
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


    /*
     *  Check top/bottom against GFA_TOP_LIMIT/GFA_BOTTOM_LIMIT in "prefs.tbl"
     */
    ctb_pfstr ( "GFA_TOP_LIMIT", tmpStr, &ier );    

    if ( ier == 0 ) {
        
	if ( atoi( _topStr ) >  atoi( tmpStr ) ) {
            sprintf ( tmpStr, "%d", atoi( tmpStr ) );
            cst_padString ( tmpStr, '0', 1, 3, _topStr );
        }
    }

    ctb_pfstr ( "GFA_BOTTOM_LIMIT", tmpStr, &ier );    

    if ( ier == 0 ) {
        
	if ( strcmp( _bottomStr, "FZL" ) != 0 ) {
            if ( atoi( _bottomStr ) <  atoi( tmpStr )) {
                strcpy  ( _bottomStr, "SFC" );
            }
	}
    }
        
    
    /*
     *  Fill & set the Top/Bottom string
     */
    tmpStr[ 0 ] = '\0';
    sprintf ( tmpStr, "%s/%s", _topStr, _bottomStr );

    if ( strlen ( tmpStr ) > (size_t)1 ) {

       XtVaSetValues ( wid, XmNvalue, tmpStr, NULL );

    }

    if ( ptext ) XtFree ( ptext );

    if ( strcasecmp( _currentPanel2->haz, "ICE" ) == 0 ) {

	pggfawp_setFzl( strcasecmp( _bottomStr, "FZL" ) == 0 );

	if ( clnt ) {

	   levels = ( levels_t*) clnt;

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

static void pggfawp_fillSingleFLCb (Widget wid, XtPointer clnt, XtPointer cbs)
/************************************************************************
 * pggfawp_fillSingleFLCb						*
 *									*
 * Callback function is used to fill a single flight level      	*
 *									*
 * static void pggfawp_fillSingleFLCb ( wid, clnt, cbs )		*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data			*
 *   cbs		XtPointer	callback struct			*
 *									*
 **									*
 * Log:									*
 * L. Hinson/AWC    11/09	        Created from pggfawp_fillFLCb	*
 ***********************************************************************/
{
  int ier;
  char *ptext = NULL, tmpStr[ 8 ];
  
  XtVaGetValues ( wid, XmNvalue, &ptext, NULL );
  if ( ptext && strlen ( ptext ) > (size_t)0 ) {
    cst_padString ( ptext, '0', 1, 3, _topStr );
  }
  /*
     *  Check top/bottom against GFA_TOP_LIMIT/GFA_BOTTOM_LIMIT in "prefs.tbl"
  */
  ctb_pfstr ( "GFA_TOP_LIMIT", tmpStr, &ier );    

  if ( ier == 0 ) {      
    if ( atoi( _topStr ) >  atoi( tmpStr ) ) {
            sprintf ( tmpStr, "%d", atoi( tmpStr ) );
            cst_padString ( tmpStr, '0', 1, 3, _topStr );
    }
  }
  if (strlen ( _topStr) > (size_t)1 ) {
    XtVaSetValues ( wid, XmNvalue, _topStr, NULL );
  }
  if ( ptext ) XtFree ( ptext );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_optPbCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_optPbCb                                                      *
 *                                                                      *
 * Callback function for option menu push buttons.                      *
 *                                                                      *
 * void pggfawp_optPbCb ( wid, clnt, call )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      clnt	XtPointer	client data	                        *
 *      call    XtPointer       not used                                *
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
    pggfawp_resetForSubtype( &ier );
    pggfawp_setApplyBtn( True );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_cigOptPbCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_cigOptPbCb                                                   *
 *                                                                      *
 * Callback function for the CIG option menu push buttons.              *
 *                                                                      *
 * void pggfawp_cigOptPbCb ( wid, clnt, call )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget          widget ID                               *
 *      clnt    XtPointer       client data                             *
 *      call    XtPointer       not used                                *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           06/08   Created                                *
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/
    pggfawp_resetForSubtype( &ier );
    pggfawp_setTypeTextCb( NULL, NULL, NULL);
}

/*=====================================================================*/

static void pggfawp_getPanel2Attr ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_getPanel2Attr						*
 *									*
 * This routine gets the GFA attributes in panel2 and saves to 		*
 * an element.								*
 *									*
 * void pggfawp_getPanelsAttr ( el )					*
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
 * J. Wu/SAIC		06/08	Get types from checkboxes 		*
 * B. Yin/SAIC		06/08	Set blank value if no coverage for C&V	*
 * L. Hinson/AWC        11/09   Add support for setting TAG_GFA_BOTTOM  *
 *                              if "bottom" is input field name         *
 * X. Guo/CWS		01/10   Handle multi-selected GFA elements	* 
 * S. Jacobs/NCEP	12/10	Save old value if using multi-select	*
 ***********************************************************************/
{
    int		ii, ier, len;
    char	tag[ MAXTBLNAME ], *value, *tmpStr;
    char	value1[ MAXTBLNAME ], *value2, *col1, *col2;
    char        tmpStr2[ STD_STRLEN ], typeTextStr[ STD_STRLEN ];
    char	prevalue [ STD_STRLEN ];

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
	cvg_getFld(el, tag, prevalue, &ier);
        if ( ( strcasecmp( tag, "<COVERAGE>" ) == 0 ||
               strcasecmp(tag,"<DUE TO>" )==0 ||
               strcasecmp(tag,"<Type>" )==0 ||
               strcasecmp(tag,"<Speed>" )==0 ||
               strcasecmp(tag,"<Contour>" )==0 ||
               strcasecmp(tag,"<Level>" )==0 ||
               strcasecmp(tag,"<Category>" )==0 ||
               strcasecmp(tag,"<Frequency>" )==0 ) &&
             (strcasecmp( value, "N/A" ) == 0 ||
              strcasecmp( value, "NONE" ) == 0  ||
	      strcasecmp( value, "N/C" ) == 0 )) {
	    if ( strcasecmp( value, "N/C" ) == 0 ) {
		strcpy ( value, prevalue );
	    }
	    else {
                value[ 0 ] = '\0';
	    }

        }

	cvg_setFld ( el, tag, value, &ier );
	
	XtFree ( value );
	XmStringFree( xmStr );
    }

/* 
 * Check that Severity has been set for Turbulence/Icing 
 */
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
 *  Load the Freezing Range info into the element.  We want to load this as
 *  gfa_fzlRange, which is the same tag that gdfrzl uses.  gdfrzl could be
 *  changed to use a different tag, but that would introduce file 
 *  compatibility problems.
 */
	else if( ( strcasecmp( _currentPanel2->haz, "FZLVL" ) == 0 ) &&
		 ( ii == _FZLRange ) ) {

            if( strlen( value ) > 0 ) {
                cvg_setFld( el, TAG_GFA_FZLRANGE, value, &ier );
	    }

	    XtFree( col1 );
	    XtFree( value );
	    XmStringFree( xmStr );
	    continue;
	}

/*
 *  Check if the input filed is in "xxx/xxx" format
 */
	col2 = strstr ( col1, "/" );

	if ( !col2 ) {

           if ( strcasecmp( col1, "top" ) == 0 ) {

              strcpy ( tag, TAG_GFA_TOP );

           } else if (strcasecmp( col1, "bottom" ) == 0 ) {
           
              strcpy ( tag, TAG_GFA_BOTTOM );
              
           } else {

              sprintf ( tag, "<%s>", col1 );

           }

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
 * save the type info for IFR and MT_OBSC
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

static void pggfawp_getPanel3Attr ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_getPanel3Attr						*
 *									*
 * This routine gets the GFA attributes in panel3 and saves to 		*
 * an element.								*
 *									*
 * void pggfawp_getPanels3Attr ( el )					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Input/Output parameters:						*
 *	*el		VG_DBStruct	current element			*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07	initial coding				*
 * E. Safford/SAIC	06/07	pggfawp_isPanel3Up --> pggfawp_isFBBA	*
 ***********************************************************************/
{
    int		ier;
    char	*tmpStr = NULL;
 
    Boolean	isFBBA = True;
/*---------------------------------------------------------------------*/

    isFBBA = pggfawp_isFBBA( );   

/*
 *  If the current element isn't an FBBA (probably a snapshot) set
 *  the FBBA specific fields to empty.
 */
    if( !isFBBA ) {
        cvg_setFld(el, TAG_GFA_AREAS,      "", &ier);
        cvg_setFld(el, TAG_GFA_REGION,     "", &ier);
        cvg_setFld(el, TAG_GFA_STATESLIST, "", &ier);
        cvg_setFld(el, TAG_GFA_CONDSBEGIN, "", &ier);
        cvg_setFld(el, TAG_GFA_CONDSEND,   "", &ier);
    }
    else {				/* this is an FBBA element */

/*
 *  Get the Area(s).
 */
        XtVaGetValues ( _areaText, XmNvalue, &tmpStr, NULL );
        if( tmpStr ) {
            cvg_setFld(el, TAG_GFA_AREAS, tmpStr, &ier);
            G_FREE ( tmpStr, char );
        }

/*
 *  Get the States list.
 */
        XtVaGetValues ( _stateText, XmNvalue, &tmpStr, NULL ); 
        if( tmpStr ) {
            cvg_setFld(el, TAG_GFA_STATESLIST, tmpStr, &ier);
            G_FREE ( tmpStr, char );
        }

/*
 *  Get the conditions beginning.
 */
        XtVaGetValues ( _beginText, XmNvalue, &tmpStr, NULL ); 
        if( tmpStr ) {
            cvg_setFld(el, TAG_GFA_CONDSBEGIN, tmpStr, &ier);
            G_FREE ( tmpStr, char );
        }

/*
 *  Get the conditions ending.
 */
        XtVaGetValues ( _endText, XmNvalue, &tmpStr, NULL ); 
        if( tmpStr ) {
            cvg_setFld(el, TAG_GFA_CONDSEND, tmpStr, &ier);
            G_FREE ( tmpStr, char );
        }
    }
}

/*=====================================================================*/

static void pggfawp_setPanel2Attr ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_setPanel2Attr						*
 *									*
 * This routine sets the GFA attributes in panel2 from a GFA element. 	*
 *									*
 * void pggfawp_setPanelsAttr ( el )					*
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
 * J. Wu/SAIC           06/08   Set checkboxes if they exist    	*
 * B. Yin/SAIC          07/08   Handle the no coverage case for C&V     *
 * X. Guo/CWS		01/10   Handle multi-selected GFA elements	*
 ***********************************************************************/
{
    int		ii, jj, len, ier;
    char	tag[ MAXTBLNAME ], *tmpStr, typeText[ STD_STRLEN ];
    char	value[ MAXTBLNAME ], col1[ MAXTBLNAME ], *col2;
    char	labelStr[ STD_STRLEN ];

    XmString    xmStr;
    int		iopr;
/*---------------------------------------------------------------------*/
/*
 *  Find the current hazard panel
 */
    pggfawp_createPanel2 ( );
    if ( !_currentPanel2 ) return;
    iopr = pgpalw_getCurOperId();
 
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
	if ( (iopr == FUNC_MULTISEL) && 
	     (strcasecmp(tag,"<DUE TO>" )==0 ||
	      strcasecmp(tag,"<Coverage>" )==0 ||
	      strcasecmp(tag,"<Type>" )==0 ||
	      strcasecmp(tag,"<Speed>" )==0 ||
	      strcasecmp(tag,"<Contour>" )==0 ||
	      strcasecmp(tag,"<Level>" )==0 || 
	      strcasecmp(tag,"<Category>" )==0 ||
	      strcasecmp(tag,"<Frequency>" )==0 )) {
	    strcpy ( value, "N/C" );
	}
	else {
	    cvg_getFld ( el, tag, value, &ier );
	}

	for ( jj = 0; jj < MAXNOPT; jj++ ) {
	    
	    XtVaGetValues ( _currentPanel2->descStrc[ ii ].pb[ jj ],
			    XmNlabelString, &xmStr, NULL );
	    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &tmpStr );

            if ( strcmp ( tmpStr, value ) == 0 ||
                 ( strlen( value ) == 0 &&
                   ( strcasecmp( tmpStr, "N/C" ) == 0 ||
		     strcasecmp( tmpStr, "N/A" ) == 0 ||
                     strcasecmp( tmpStr, "NONE" ) == 0 ) ) ) {

	        if ( iopr != FUNC_MULTISEL && strcasecmp( tmpStr, "N/C" ) == 0 ) {
                    XtVaSetValues( _currentPanel2->descStrc[ ii ].menu,
                              XmNmenuHistory,
                              _currentPanel2->descStrc[ ii ].pb[ 0 ],
                              NULL );
	        }
		else { 
	       	    XtVaSetValues( _currentPanel2->descStrc[ ii ].menu,
		              XmNmenuHistory, 
			      _currentPanel2->descStrc[ ii ].pb[ jj ],
			      NULL );
		}
	        break;
	    }

	    XtFree ( tmpStr );
	    XmStringFree( xmStr );
	}
    } 

    for ( ii = 0; ii < _currentPanel2->nInputField; ii++ ) {

	if ( ( strcasecmp ( _currentPanel2->haz, "ICE" ) == 0 ) &&
	     ( ii == _FZLIndex ) ) {

/*
 *  Build the Freezing level top/bottom values.
 */
	   cvg_getFld ( el, TAG_GFA_FZL_TOP, value, &ier );
	   cvg_getFld ( el, TAG_GFA_FZL_BOTTOM, col1, &ier );

	   if ( ( strlen( value ) != 0 ) && ( strlen( col1 ) != 0 ) ) {
	      
	      sprintf ( value, "%s/%s", value, col1 );

              XtVaSetValues( _currentPanel2->inputField[ ii ].text, 
	   		     XmNvalue, value, NULL );

	      pggfawp_setFzl( ON );

	   }
	   else {
	      pggfawp_setFzl( OFF );
	   }
	   continue;
	}
/*
 *  Fill in the Freezing range.
 */
	else if( ( strcasecmp( _currentPanel2->haz, "FZLVL" ) == 0 ) &&
	         ( ii = _FZLRange ) ) {
	    if ( iopr == FUNC_MULTISEL) {
		ier = -1;
	    }
	    else {
	        cvg_getFld ( el, TAG_GFA_FZLRANGE, value, &ier );
	    }

	    if( ier >= 0 && strlen( value ) > 0 ) {
                XtVaSetValues( _currentPanel2->inputField[ ii ].text, 
	   		     XmNvalue, value, NULL );
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

           if ( strcasecmp( col1, "top" ) == 0 ) {

              strcpy ( tag, TAG_GFA_TOP );

           }
           else {

              sprintf ( tag, "<%s>", col1 );

           }
	   if ( iopr == FUNC_MULTISEL ) {
                value[0] = '\0';
           }
           else {
		cvg_getFld ( el, tag, value, &ier );
	   }

           if ( strlen ( value ) > (size_t)0 ) {
              XtVaSetValues( _currentPanel2->inputField[ ii ].text,
                             XmNvalue, value, NULL );
           }
           else {
              XtVaSetValues( _currentPanel2->inputField[ ii ].text,
                             XmNvalue, "", NULL );
           }

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
           if ( iopr == FUNC_MULTISEL ) {
                value[0] = '\0';
           }
           else {
	       cvg_getFld ( el, tag, &value[ len + 1 ], &ier );
	   }

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
	    if ( iopr == FUNC_MULTISEL ) {
		typeText[ 0 ] = '\0';
	    }
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

static void pggfawp_setPanel3Attr ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_setPanel3Attr						*
 *									*
 * This routine sets the GFA attributes in panel3 from a GFA element. 	*
 *									*
 * void pggfawp_setPanelsAttr ( el )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	current element			*
 * Output parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07	initial coding            		*
 * B. Yin/SAIC		09/07	remove code setting the desk value	*
 * J. Wu/SAIC		01/08	clean up _areaText before reset it	*
 ***********************************************************************/
{
    char	tmpStr[ 128 ];
    int		ier;

/*---------------------------------------------------------------------*/
/*
 *  Set the _areaText.
 */
    tmpStr[0] = '\0';
    cvg_getFld( el, TAG_GFA_AREAS, tmpStr, &ier );
    XtVaSetValues ( _areaText, XmNvalue, "", NULL );
    XtVaSetValues ( _areaText, XmNvalue, &tmpStr, NULL );

/*
 *  Set the States list.
 */
    tmpStr[0] = '\0';
    cvg_getFld(el, TAG_GFA_STATESLIST, tmpStr, &ier);

    XtVaSetValues ( _stateText, XmNvalue, &tmpStr, NULL );

/*
 *  Set the Conditions Beginning
 */
    tmpStr[0] = '\0';
    cvg_getFld(el, TAG_GFA_CONDSBEGIN, tmpStr, &ier);

    XtVaSetValues ( _beginText, XmNvalue, &tmpStr, NULL );

/*
 *  Set the Conditions Ending
 */
    tmpStr[0] = '\0';
    cvg_getFld(el, TAG_GFA_CONDSEND, tmpStr, &ier);

    XtVaSetValues ( _endText, XmNvalue, &tmpStr, NULL );

}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_vrfyFcstHrCb ( Widget text_w, XtPointer clnt,
                                 XtPointer call )
/************************************************************************
 * pggfawp_vrfyFcstHrCb                                                 *
 *                                                                      *
 * Verify the contents of the forecast hour text widget as the user is  *
 * entering data.  							*
 * 									*
 * The input could be in format of H:MM or H:MM-H:MM, in which		*
 *     (1) H could only be 0, 1, 2, 3, 4, 5, 6, 9, or 12.		*
 *     (2) MM is optional & could only be 00, 15, 30, or 45;		* 
 *     (3) no MM if H is >= 6; 						*
 *     (4) the hour after '-' should be >= the hour before '-'; 	*
 *     (5) if the hour before '-' is < 6, the hour after '-' should <=6	*
 * 									*
 * So each position could only accept a few certain characters. Any 	*
 * other characters will force the "doit" flag of the cbs structure be 	*
 * set to "FALSE", which signals the text widget to NOT display the data*
 * and SOUND the alarm bell (a single beep).  Note that the bell may be	*
 * switched off by setting the text widget's XmNaudibleWarning value, 	*
 * which is "TRUE" by default.                                      	*
 *                                                                      *
 * Note: This function should be linked to the text widget via the      *
 *       XmNmodifyVerifyCallback event, which fires before the          *
 *       XmNvalueChangedCallback and XmNlosingFocusCallback events.     *
 *                                                                      *
 * static void pggfawp_vrfyFcstHrCb ( text_w, clnt, call )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      text_w		Widget    text widget                           *
 *      clnt		XtPointer Widget's event data (not used here)	*
 *      call		XtPointer callback structure                 	*
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
 * J. Wu/SAIC           03/08   redo algorithm for both specials & fbbas*
 ***********************************************************************/
{
    int         ii;
    char        *text, newText[ 12 ], cval;
    Boolean	doit, firstLsThan6;
    
    XmTextVerifyCallbackStruct *cbs =
                                (XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL ) {
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
    strncat ( newText, text, cbs->startPos );	
    strcat ( newText, cbs->text->ptr );
    strcat ( newText, &text[ cbs->endPos ] );

/*  
 *  Verify the resulting text:
 */     	
    strcpy ( _tmpFcsthrStr, text );
    firstLsThan6 = True;	    
    for ( ii = 0; ii < (int)strlen( newText ); ii++ ) {
        
        doit = False;
	cval = newText[ ii ];

/*  
 *      First char accepts 0,1,2,3,4,5,6, or 9; 
 */     	
	if ( ii == 0 ) {
	    if ( cval == '0' || cval == '1' || cval == '2' ||
	         cval == '3' || cval == '4' || cval == '5' ||
		 cval == '6' || cval == '9' ) {
	        
		doit = True; 
	        
                if ( cval == '6' || cval == '9' ) {
	            firstLsThan6 = False;	    
	        }
	    }
	}

/*  
 *     Second char accepts ':' or '-' if 1st char is < 6; 
 *                 accepts only '-' if 1st char is is 6 or 9; 
 *                 accepts '2' if 1st char '1'; 
 */     	
	else if ( ii == 1 ) {
	    if  ( ( firstLsThan6 && ( cval == ':' || cval == '-' ) ) ||
	          ( !firstLsThan6 && cval == '-' ) ||
		  ( cval == '2' && newText[ 0 ] == '1' ) )  {
	        doit = True; 	    	    
	    }
	}

/*  
 *     Third char accepts 0, 1, 3, 4 if 2nd char is ':'; 
 *                accepts any digit as 1st char does if 2nd char is '-', 
 *                        but must be (1) >= 1st char, (2) <= 6 if 1st char <=6; 
 *                accepts '1' if 2nd char is '-' & 1st char >= 6; 
 *                accepts '-' if 2nd char is '2'; 
 */     	
	else if ( ii == 2 ) {
	    if ( newText[ 1 ] == ':' ) { 
	        if ( cval == '0' || cval == '1' ||
		     cval == '3' || cval == '4' ) {
		   doit = True; 
		}
	    }
	    else if ( newText[ 1 ] == '-' ) {
	        if ( ( cval == '0' || cval == '1' || cval == '2' ||
	               cval == '3' || cval == '4' || cval == '5' ||
		       cval == '6' || cval == '9' ) ) {
		    
		    if ( cval >=  newText[ 0 ] ) {		    
		        doit = True; 	    		
		    }
		    else {
		        if ( cval == '1' && !firstLsThan6 ) {
			    doit = True;
			}
		    }
		    
		    if ( firstLsThan6 && cval == '9' ) {
		        doit = False;
		    }
		}
	    }
	    else { /* 2 */
	        if ( cval == '-' ) {
		    doit = True; 	            
	        }
	    }
	}

/*  
 *     4th char accepts 0 if 2nd char is ':' & 3rd is 0 or 3; 
 *              accepts 5 if 2nd char is ':' & 3rd is 1 or 4;
 *              accepts ':' if 2nd char is '-';
 *              accepts '2' if 2nd char is '-' & 3rd is 1 & 1st ch is not 1;
 *              accepts '1' if 3rd is '-';
 */     	
	else if ( ii == 3 ) {
	    if ( newText[ 1 ] == ':' ) {
	        if  ( ( cval == '0' && ( newText[ 2 ] == '0' || 
		                         newText[ 2 ] == '3' ) ) || 
		      ( cval == '5' && ( newText[ 2 ] == '1' || 
			                 newText[ 2 ] == '4' ) ) ) {
	        
		    doit = True;
		}
	    }
	    else if ( newText[ 1 ] == '-' ) {
	        if ( cval == ':' || 
		    ( newText[ 2 ] == '1' && cval == '2' && newText[ 0 ] != '1' ) ) {
		    doit = True;
	        }
	    }
	    else {
	        if ( cval == '1' && newText[ 2 ] == '-' ) {
		    doit = True; 	            
	        }	    
	    }
	}	  

/*  
 *     5th char accepts 2 if 4th char is 1 (12-12); 
 *              accepts 0, 1, 3, 4 if 4th char is ':';
 *              accepts '-' if 4th char is 0 or 5;
 */     	
        else if ( ii == 4 ) {
            if ( newText[ 3 ] == '1' ) {
		if ( cval == '2' ) {
		    doit = True; 	            	     
	        }
	    }
	    else if ( newText[ 3 ] == ':' ) {
	        if ( cval == '0' || cval == '1' || cval == '3' || cval == '4' ) {
	            doit = True; 	            	     		     	    
	        }
	    }
	    else {  /* 0 or 5 */
	        if ( cval == '-' ) {
		    doit = True;
		}
	    }
	}    

/*  
 *     6th char accepts 0 if 5th char is 0 or 3; 
 *              accepts 5 if 5th char is 1 or 5;
 *              accepts any digit as 1st char does if 5th char is '-', 
 *                        but must be (1) >= 1st char, (2) <= 6 if 1st char <=6; 
 */     	
        else if ( ii == 5 ) {
            if ( newText[ 4 ] == '-' ) {
	        if ( ( cval == '0' || cval == '1' || cval == '2' || 
		       cval == '3' || cval == '4' || cval == '5' || 
		       cval == '6' || cval == '9' ) 
		       && cval >= newText[ 0 ] ) {
		       
		    doit = True;	    
		    
		    if ( firstLsThan6 && cval == '9' ) {
		        doit = False; 	    		
		    }
	        }
	    }
	    else if ( ( newText[ 4 ] == '0' || newText[ 4 ] == '3' ) &&
	              cval == '0' ) {	        
		doit = True;	    
	    }
	    else if ( ( newText[ 4 ] == '1' || newText[ 4 ] == '4' ) &&
	              cval == '5' ) {	        
		doit = True;	    
	    }
	}

/*  
 *     7th char accepts ':' if 5th char is '-'; 
 */
 	else if ( ii == 6 ) {
	    if ( newText[ 4 ] == '-' ) {
	        if ( cval == ':' ) {	        
		    doit = True;
	        }
	    }
	}

/*  
 *     8th char accepts 0, 1, 3, 4 but should be >= 2nd char if 1st char == 5th char;
 */
 	else if ( ii == 7 ) {
	    if ( cval == '0' || cval == '1' || cval == '3' || cval == '4' ) {
                
		if ( newText[ 0 ] == newText[ 5 ] ) {		
		    if ( cval >= newText[ 2 ] ) {
		        doit = True;
	            }
		}
		else {
		    doit = True;
		}
	    }
	}

/*  
 *     9th char accepts 0 if 7th char is 0 or 3;
 *              accepts 5 if 7th char is 1 or 4;
 */
	else if ( ii == 8 ) {
	    if ( cval == '0' && ( newText[ 7 ] == '0' || newText[ 7 ] == '3' ) ) {
	        doit = True;
	    }
	    else if ( cval == '5' && ( newText[ 7 ] == '1' || newText[ 7 ] == '4' ) ) {
                doit = True;
	    }
	}
	
    	
/*
 *  Reject it if invalid
*/     
        if ( !doit ) {    
	    cbs->doit = False;
            break;
	}			
    
    }	

    XtFree( text );
    pggfawp_setApplyBtn( True );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_fcsthrTxtCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfawp_fcsthrTxtCb                                                  *
 *                                                                      *
 * Callback function for the optional forecast hour text. This text     *
 * is available only when the forecast hour menu selection is the       *
 * last option "Other".                                                 *
 *                                                                      *
 * Note: this is a losingFocuse callback function. Some "smart" auto	*
 *       completion occurs if the user start drawing without finishing	*
 *       the input to the forecast hour, including:                     *
 *                                                                      *
 *   (1) Trailing '-' is removed.	                            	*
 *   (2) H:1 and H:4 are completed as H:15 and H:45.               	*
 *   (3) H:0 and H:3 are completed as H:00 and H:30.               	*
 *   (4) for snapshots, if H is not a choice in the pulldown menu and 	*
 *       the user only give "H" or "H:", it will be completed as H:00.  *
 *   (5) 6-1, 9-1, and 12-1 is completed as 6-12, 9-12, and 12-12.      *
 *   (6) If the field is left blank, the GFA ends with hour "0"     	*
 *                                                                      *
 * static void pggfawp_fcsthrTxtCb ( wid, clnt, cbs )                  	*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   clnt             	XtPointer       not used                        *
 *   cbs		XtPointer       callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           04/04   initial coding                          *
 * J. Wu/SAIC           04/04   remove the ending '-' if it exists      *
 * J. Wu/SAIC           08/04   link with layer control window          *
 * J. Wu/SAIC           05/06   add auto-complete for special inputs	*
 * M. Li/SAIC		03/07	check for invalid _fcsthrStr		*
 * J. Wu/SAIC           12/07   Remove auto-complete for smears       	*
 * J. Wu/SAIC           03/08   redesign auto-completion		*
 ***********************************************************************/
{
    int		slen, ii;
    char	*ptext = NULL, tmpstr[2];
    Boolean	fcsthrInMenu;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );
        
    if ( !ptext || strlen( ptext ) == 0 ) {
        strcpy ( _fcsthrStr, "0" );            
        if ( ptext )  {
	     XtFree ( ptext );
        }
    }
    else {

	strcpy ( _fcsthrStr, ptext );

	slen = (int)strlen( ptext );
	
        /*
          *  First remove the trailing '-' or ':'.
          */
	if ( slen > 1 && ( ptext[ slen - 1 ] == '-' ||
	                   ptext[ slen - 1 ] == ':' )  ) {
	    _fcsthrStr[ slen - 1 ] = '\0';
	    slen--;
	}
		
        
	/*
          *  Handling smears or FBBAs.
          */
	if ( strchr ( _fcsthrStr, '-' ) ) {
            if ( slen == 3 ) {
	        if ( ptext[ 1 ] == '-' && ptext[ 2 ] == '1' ) {
	            strcat ( _fcsthrStr, "2" );
	        }
	    }
            if ( slen == 4 ) {
	        if ( ptext[ 2 ] == '-' && ptext[ 3 ] == '1' ) {
	            strcat ( _fcsthrStr, "2" );
	        }
	    }
	    else if ( slen == 5 ) {
	        if ( ptext[4] == '0' || ptext[4] == '3' ) {
	            strcat ( _fcsthrStr, "0" );	    	    
	        }
	        else if ( ptext[4] == '1' || ptext[4] == '4' ) {
	            strcat ( _fcsthrStr, "5" );	    
	        }	        
	    }
	    else if ( slen == 8 ) {
	        if ( ptext[7] == '0' || ptext[7] == '3' ) {
	            strcat ( _fcsthrStr, "0" );	    	    
	        }
	        else if ( ptext[7] == '1' || ptext[7] == '4' ) {
	            strcat ( _fcsthrStr, "5" );	    
	        }	        
	    }
	}

        /*
          *  Now handling snapshots.
          */
	else  {
            
	    if ( slen == 1 && ( ptext[ 0 ] == '1' || ptext[ 0 ] == '2' ||
	                        ptext[ 0 ] == '4' || ptext[ 0 ] == '5' ) ) {
	        strcat ( _fcsthrStr, ":00" );	    	
	    }
	    else if ( slen == 2 && ptext[ 1 ] == ':' ) {
	        
                fcsthrInMenu = False;
	        strncpy( tmpstr, _fcsthrStr, 1 );
		for ( ii = 0; ii < _nFcsthr; ii++ ) {
                    if ( strcmp ( tmpstr, _fcsthr[ii] ) == 0 ) {
                        fcsthrInMenu = True;
                        break;
                    }
                }
		
		if ( !fcsthrInMenu ) {
		    strcat ( _fcsthrStr, "00" );
		}
		else {
		    _fcsthrStr[ 1 ] = '\0';
		}	    
	    }
	    else if ( slen == 3 ) {
	        if ( ptext[2] == '0' || ptext[2] == '3' ) {
	            strcat ( _fcsthrStr, "0" );	    	    
	        }
	        else {
	            strcat ( _fcsthrStr, "5" );	    
	        }
	    }
	}

	slen = strlen( _fcsthrStr );	
        if ( slen <= 0 )  {
	    strcpy( _fcsthrStr, "0" );
	    slen = 1;
        }
        
	XtVaSetValues ( wid,
	    	    	XmNvalue,		_fcsthrStr, 
		    	XmNcursorPosition,	slen,
		    	NULL );
	
        XtFree ( ptext );
    }
	
/*
 *   Match the new hour to filter time if the filtering is active.
 */    
    if ( pgfilterw_isUp () ) {
            pgfilterw_turnOnTime ( _fcsthrStr );
    }
    
}

/*=====================================================================*/

void pggfawp_checkHours ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_checkHours                                                    *
 *                                                                      *
 * This routine checks the GFA forecast hours,  sets the GFA category   *
 * (smear/outlook/snapshot), and snaps GFA if it's a smear or outlook.	*
 *                                                                      *
 * void pggfawp_checkHours ( el ) 			                *
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
 * E. Safford/SAIC	04/07	use GFA_FBBA_AIRMET/OUTLOOK		*
 * J. Wu/SAIC		01/08	Removed hard-coded checks for AIRM/OTLK	*
 * J. Wu/SAIC		02/08	Fixed a bug in checking OTLK		*
 * J. Wu/SAIC		03/08	keep the subtype for editing smears	*
 * B. Yin/SAIC		07/08	Set hazard type for 20 KT SFC WND as  	*
 *				SFC_WND20				*
 ***********************************************************************/
{
    int		ier, subtype, minu;
    char	hours[ 32 ], subtypeStr[ 32 ], level[ 32 ], haz[ 32 ];
    char	secondHr[ 32 ], *cptr, windSpeed[ 32 ];
    Boolean	isAIRM = False, isOTLK = False;
/*---------------------------------------------------------------------*/

    level[ 0 ] = '\0';
    cvg_getFld ( el, "Level", level, &ier );

    strcpy ( haz, _areaTyp[ _areaTypeIndex ] );

    if ( ( strcasecmp( haz, "fzlvl" ) == 0 ) && ( ier == 0 ) &&
    	 ( strcasecmp( level, "sfc" ) == 0 ) ) {

	strcat ( haz, "_SFC" );

    }

    cvg_getFld ( el, "Speed", windSpeed, &ier );

    if ( ( strcasecmp( haz, "sfc_wnd" ) == 0 ) && ( ier == 0 ) &&
         ( strcasecmp( windSpeed, "20kt" ) == 0 ) ) {

        strcat ( haz, "20" );

    }

    hours[ 0 ] = '\0';
    cvg_getFld ( el, TAG_GFA_FCSTHR, hours, &ier );
    
    /*
     *  If "hours" contains a "-" and the hour after "-" > 6,  it is an OUTLOOK.
     *  If "hours" contains a "-" and the hour after "-" <= 6,  it is an AIRMET.
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
       
       pggfawp_makeSubtype ( pggfawp_getHazardType( haz ),
       			    GFA_FBBA_AIRMET, &subtype, &ier );
       
       if ( !_addMode && _editSmear ) {            
	    pggfawp_makeSubtype ( pggfawp_getHazardType( haz ),
       			    GFA_USER_SMEAR, &subtype, &ier );
       }
       
       if ( ier < 0 ) subtype = 99;

       sprintf( subtypeStr, "%d", subtype );
       cvg_setFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

       pgsmear_snapEl ( FALSE, el, &ier );

    }
    else if ( isOTLK ) {

       pggfawp_makeSubtype ( pggfawp_getHazardType( haz ),
       			    GFA_FBBA_OUTLOOK, &subtype, &ier );

       if ( !_addMode && _editSmear ) {               
	   pggfawp_makeSubtype ( pggfawp_getHazardType( haz ),
       			    GFA_USER_OUTLOOK, &subtype, &ier );	       
       }
       
       if ( ier < 0 ) subtype = 99;

       sprintf( subtypeStr, "%d", subtype );
       cvg_setFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

       pgsmear_snapEl ( FALSE, el, &ier );

    }
    else {

       pggfawp_makeSubtype ( pggfawp_getHazardType( haz ),
       			    GFA_SNAPSHOT, &subtype, &ier );

       if ( ier < 0 ) subtype = 99;

       sprintf( subtypeStr, "%d", subtype );
       cvg_setFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

    }

/*
 *  Get GFA line width
 */
    pggfawp_getLineWidth ( el );
}

/*=====================================================================*/

/* ARGSUSED */
static void pggfawp_verifyFLCb ( Widget wid, XtPointer clnt, 
						XtPointer call ) 
/************************************************************************
 * pggfawp_verifyFLCb                                                   *
 *                                                                      *
 * This routine checks the validation of top/bottom flight levels.      *
 *                                                                      *
 * void pggfawp_verifyFLCb ( wid, clnt, call ) 	        		*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid		Widget	  top/bottom text field widget		*
 *	clnt		XtPointer Widget's event data (not used here)  	*
 *	call		XtPointer callback structure			*
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

    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL ) {
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
			     strcasecmp ( strPtr, "sfc" ) == 0 ||
			     ( ( strcasecmp ( strPtr, "f" )   == 0  ||
			         strcasecmp ( strPtr, "fz" )   == 0 ||
			         strcasecmp ( strPtr, "fzl" ) == 0 ) && ice ) ) ) {

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

static void pggfawp_verifySingleFLCb ( Widget wid, XtPointer clnt, 
						XtPointer call ) 
/************************************************************************
 * pggfawp_verifySingleFLCb                                             *
 *                                                                      *
 * This routine checks the validation of singular flight level.         *
 *                                                                      *
 * void pggfawp_verifySingleFLCb ( wid, clnt, call )         		*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid		Widget	  top/bottom text field widget		*
 *	clnt		XtPointer Widget's event data (not used here)  	*
 *	call		XtPointer callback structure			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Hinson/AWC	11/09	Created	from pggfawp_verifyFLCb		*
 ***********************************************************************/
 {
    size_t	ii;
    char	*text, newText[ 8 ];

    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL ) {
	return;
    }
    XtVaGetValues ( wid, XmNvalue, &text, NULL );
    if ( ( cbs->text->length + (int)strlen ( text ) - 
         ( cbs->endPos - cbs->startPos ) ) > 3 ) {
       
       cbs->doit = False;

    }
    else {
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

          else { 
            for ( ii = 1; ii < strlen ( newText ); ii++ ) {
		    
		    if ( isdigit ( newText[ ii ] ) == 0 ) cbs->doit = False;
	    }
          }
       }
   }
   XtFree (text);
 }
    
/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_verifyAreaCb ( Widget wid, XtPointer clnt, 
						XtPointer call ) 
/************************************************************************
 * pggfawp_verifyAreaCb                                                 *
 *                                                                      *
 * This routine validates input to the  FA Area text field.             *
 *                                                                      *
 * void pggfawp_verifyAreaCb ( wid, clnt, call ) 	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *	wid	Widget	  top/bottom text field widget			*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	call	XtPointer callback structure				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	04/07	initial coding				*
 ***********************************************************************/
{
    char	*text = NULL;
    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL || !cbs->doit ) {
	return;
    }

/*
 *  Check for maximum field length.  Max is 7 characters 
 *  (two 3-letter area abreviations + a space between them).
 */
    XtVaGetValues ( wid, XmNvalue, &text, NULL );
    if( strlen( text ) + cbs->text->length > 7 ) {
        cbs->doit = False;
    }
    
    G_FREE ( text, char );
}

/*=====================================================================*/

Boolean pggfawp_okToDraw( Boolean showDlg )
/************************************************************************
 * pggfawp_okToDraw                                                      *
 *                                                                      *
 * This routine checks if top/bottom field is empty. 			*
 *                                                                      *
 * Boolean pggfawp_okToDraw ( )				 	        *
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
 * B. Yin/SAIC          06/08   change IFR to C&V                       *
 * B. Yin/SAIC          07/08   Make top/bottom of MT OBSC optional     *
 * L. Hinson/AWC        11/09   Add code to Activate Warning Dialog     *
 *                              Box When Top or Bottom Input Fields are *
 *                              specified                               *
 * L. Hinson/AWC        11/09   Re-enable Warning Dialog on IFR Layer   *
 * X. Guo/CWS		01/10	return True if the current oper is	*
 *				FUNC_MULTISEL				*
 ***********************************************************************/
{
    int		ii;
    char	*label, *value = NULL, *text;

    XmString    xmStr;
    int		iopr;
/*---------------------------------------------------------------------*/

    if ( !_currentPanel2 ) return True;
    iopr = pgpalw_getCurOperId();
    if ( iopr == FUNC_MULTISEL ) return True;

/*
 *  Check if there is a top/bottom field
 */
    for ( ii = 0; ii < _currentPanel2->nInputField; ii++ ) {

	XtVaGetValues ( _currentPanel2->inputField[ ii ].label, 
			XmNlabelString, &xmStr, NULL );
	XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &label );

	XtVaGetValues ( _currentPanel2->inputField[ ii ].text, 
			   XmNvalue, &value, NULL );

	if ( strncasecmp ( label, "top/bottom", 10 ) == 0 &&
	     strcasecmp ( _areaTyp[ _areaTypeIndex ], "MT_OBSC" ) != 0 ) {
	
	   if ( !value || ( strlen ( value ) == (size_t)0 ) ) {

		XtFree ( value );
		XtFree ( label );
		XmStringFree ( xmStr );

                if ( showDlg ) pggfawp_showWarning ( _gfaForm ); 
		return False;
	   }
	}
        else if ( (strncasecmp(label,"top/bottom",10) != 0 &&
                  (strncasecmp ( label, "top", 3) == 0 || 
                  strncasecmp (label, "bottom", 6) == 0))
                  &&  strcasecmp ( _areaTyp[ _areaTypeIndex ], "C&V" ) != 0 ) {
           if ( !value || ( strlen ( value ) == (size_t)0 ) ) {
                XtFree ( value );
                XtFree ( label );
                XmStringFree ( xmStr );
                if ( showDlg ) pggfawp_showWarningSingleFL ( _gfaForm );
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

                if ( showDlg ) pggfawp_showWarning ( _gfaForm ); 
		return False;

	   }

	   XtFree ( text );
	}
	
	XtFree ( value );
	XtFree ( label );
	XmStringFree( xmStr );
    }

    if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "C&V" ) == 0 ) {

       if ( !pggfawp_CvHasType() ) {

          if ( showDlg ) pggfawp_showIfrWarning ( _gfaForm );

          return False;
       }
    } else if (strcasecmp ( _areaTyp[ _areaTypeIndex ], "IFR" ) == 0 ) {
       XtVaGetValues ( _currentPanel2->typeText, XmNvalue, &text, NULL );

       if ( strlen ( text ) == (size_t) 0 ) {

	  XtFree ( text );

          if ( showDlg ) pggfawp_showOrigIfrWarning ( _gfaForm ); 

	  return False;
       }

       XtFree ( text );
    }
    else if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "MT_OBSC" ) == 0 ) {
       XtVaGetValues ( _currentPanel2->typeText, XmNvalue, &text, NULL );
       if ( strlen ( text ) == (size_t) 0 ) {
          XtFree ( text );
          if ( showDlg ) pggfawp_showMtobscWarning ( _gfaForm );
          return False;
       }
       XtFree ( text );
    }
    return True;
}

/*=====================================================================*/

static void pggfawp_showWarning ( Widget parent )
/************************************************************************
 * pggfawp_showWarning                                                   *
 *                                                                      *
 * This routine show a warning if top/bottom field is empty. 		*
 *                                                                      *
 * static void pggfawp_showWarning ( parent )		 	        *
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
    		   (XtCallbackProc)pggfawp_fillFLCb, (XtPointer) &levels );

    XtAddCallback ( flightLevel, XmNvalueChangedCallback, 
    		   (XtCallbackProc)pggfawp_changedFLCb, (XtPointer) &levels );

    XtAddCallback ( flightLevel, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc)pggfawp_verifyFLCb, NULL );

    XtAddCallback ( fzl, XmNlosingFocusCallback, 
    		   (XtCallbackProc)pggfawp_fillFzlCb, NULL );

    XtAddCallback ( fzl, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc)pggfawp_verifyFzlCb, NULL );

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
		(XtCallbackProc)pggfawp_warningBtnCb, (XtPointer) &levels );

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

static void pggfawp_showWarningSingleFL ( Widget parent )
/************************************************************************
 * pggfawp_showWarningSingleFL                                          *
 *                                                                      *
 * This routine show a warning if single flight field is empty. 	*
 *                                                                      *
 * static void pggfawp_showWarningSingleFL ( parent )         	        *
 *                                                                      *
 * Input parameters:                                                    *
 * 	parent		Widget		parent widget	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Hinson/AWC       11/09   Created from pggfawp_showWarning         *
 ***********************************************************************/
 {
    int			ii;
    char		*btnStr[] = { "OK", "Cancel" }, *flText;

    static Widget	warningWSingle = NULL, flightLevel;
    static levels_t	levels;
    Widget		pane, msg, rowColFL, btnForm, flightLevelLbl;
    WidgetList		warningBtn;
    
    
    XtVaGetValues ( _currentPanel2->inputField[ 0 ].text,
                      XmNvalue, &flText, NULL );
    if (warningWSingle) {
      XtVaSetValues ( flightLevel, XmNvalue, flText, NULL );
      XtUnmanageChild ( warningWSingle );
      XtManageChild ( warningWSingle );
      return;
    }
    
    warningWSingle = XmCreateFormDialog ( parent, "pggfa_warningsingleFL", NULL, 0);
    XtVaSetValues ( warningWSingle,
		    XmNdefaultPosition, False,
		    XmNx,               200,
		    XmNy,               200,
		    XmNnoResize,        True,
		    XmNdialogStyle,     XmDIALOG_FULL_APPLICATION_MODAL,
		    NULL );
    XtVaSetValues ( XtParent ( warningWSingle ),
		    XmNtitle,           "Error: Missing Flight Level",
		    NULL );
    /*
     *  Create a parent pane widget
    */
    pane = XtVaCreateManagedWidget ( "pggfa_wnpane2",
                       xmPanedWindowWidgetClass, warningWSingle,
		       XmNseparatorOn,		 False,
		       XmNmarginHeight,		 20,
		       XmNmarginWidth,		 0,
		       XmNspacing,		 15,
                       XmNsashWidth,             1,
                       XmNsashHeight,            1,
                       NULL );
    msg = XtVaCreateManagedWidget ( "Must supply flight level before drawing GFA. ",
                       xmLabelGadgetClass, pane,
		       XmNmarginLeft,	   10,
		       XmNmarginRight,     10,
		       NULL );
    rowColFL = XtVaCreateManagedWidget ( "warningRowColFL2",
   				xmRowColumnWidgetClass,	pane,
				XmNorientation,		XmHORIZONTAL,
                	  	XmNtopAttachment,	XmATTACH_WIDGET,
				XmNpacking,		XmPACK_COLUMN,
				XmNnumColumns,		2,
                	  	XmNtopWidget,		msg,
                	  	NULL );
    flightLevelLbl = XtVaCreateManagedWidget ( "Flight Level:",
                                xmLabelGadgetClass,	rowColFL,
				XmNmarginLeft,		50,
                	  	NULL );
    flightLevel = XtVaCreateManagedWidget ( "flightLevel2",
                		xmTextWidgetClass,      rowColFL,
                		XmNmaxLength,           10,
                		XmNeditable,            TRUE,
                	        XmNvalue,               flText,
				XmNwidth,		100,
                    		XmNrecomputeSize,       TRUE,
                		NULL );
    levels.flightLevel = flightLevel;
    XtAddCallback ( flightLevel, XmNlosingFocusCallback, 
    		   (XtCallbackProc)pggfawp_fillSingleFLCb, (XtPointer) &levels );
    XtAddCallback ( flightLevel, XmNmodifyVerifyCallback, 
    		   (XtCallbackProc)pggfawp_verifySingleFLCb, NULL );
    XtVaCreateManagedWidget ( "separator2", xmSeparatorWidgetClass, pane, NULL );
    btnForm = XtVaCreateManagedWidget ( "warningBtn2",
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
		(XtCallbackProc)pggfawp_warningBtnSingleFLCb, (XtPointer) &levels );

    }
    XtFree( (XtPointer) warningBtn );
    XtManageChild ( warningWSingle );
    XtVaSetValues ( XtParent( warningWSingle ),
	    XmNminWidth,	360,
	    XmNmaxWidth,	360,
	    XmNminHeight,	200,
	    XmNmaxHeight,	200,
	    NULL );

    XtFree ( flText );
    
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_warningBtnCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_warningBtnCb							*
 *									*
 * Callback function for the control buttons in the warning window.	*
 *									*
 * static void pggfawp_warningBtnCb ( wid, clnt, call )			*
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	call		XtPointer	callback struct			*
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

static void pggfawp_warningBtnSingleFLCb ( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_warningBtnSingleFLCb                                         *
 *									*
 * Callback function for the control buttons in the warning window.	*
 *									*
 * static void pggfawp_warningBtnSingleFLCb ( wid, clnt, call )	        *
 *									*
 * Input parameters:							*
 *	wid		Widget		widget ID			*
 *	clnt		XtPointer	client data			*
 *	call		XtPointer	callback struct			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * L. Hinson/AWC        11/09    Created from pggfawp_warningBtnCb      *
 ***********************************************************************/
 {
    int		ier;
    char	*btnLabel = NULL, *levelStr = NULL;

    levels_t	*levels;
    XmString	xmStr;
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNlabelString, &xmStr, NULL );
    XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &btnLabel );

    XmStringFree ( xmStr );
    if ( strcasecmp ( btnLabel, "ok" ) == 0 ) {
        levels = (levels_t*)clnt;
	XtVaGetValues ( levels->flightLevel, XmNvalue, &levelStr, NULL );
        cst_lcuc ( levelStr, levelStr, &ier );
        if (  levelStr && ( strlen ( levelStr ) != ( size_t )0 ) ) {
          XtVaSetValues ( _currentPanel2->inputField[ 0 ].text, XmNvalue,
                          levelStr, NULL );
        }
        XtUnmanageChild ( XtParent ( XtParent ( XtParent( wid ) ) ) );
        _okOnWarning = True;
    }
    else if ( strcasecmp (btnLabel, "cancel" ) == 0  ) {
        XtUnmanageChild ( XtParent( XtParent( XtParent( wid ) ) ) );
        _cancelOnWarning = True;
    }
    XtFree ( btnLabel );
    XtFree ( levelStr );
}
        
/*=====================================================================*/

void pggfawp_setOkOnWarning( Boolean value )
/************************************************************************
 * pggfawp_setOkOnWarning  					       	*
 *									*
 *  Routine used for setting value of _okOnWarning static variable 	*
 *									*
 *  void pggfawp_setOkOnWarning( value )				*
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

void pggfawp_setCancelOnWarning( Boolean value )
/************************************************************************
 * pggfawp_setCancelOnWarning  					       	*
 *									*
 *  Routine used for setting value of _CancelOnWarning static variable 	*
 *									*
 *  void pggfawp_setCancelOnWarning( value )				*
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

Boolean pggfawp_getOkOnWarning(void)
/************************************************************************
 * pggfawp_getOkOnWarning  					       	*
 *									*
 *  Routine used to get the value of _OkOnWarning static variable 	*
 *									*
 *  Boolean pggfawp_getOkOnWarning( )					*
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

Boolean pggfawp_getCancelOnWarning(void)
/************************************************************************
 * pggfawp_getCancelOnWarning  					       	*
 *									*
 *  Routine used to get the value of _CancelOnWarning static variable 	*
 *									*
 *  Boolean pggfawp_getCancelOnWarning( )				*
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

Boolean pggfawp_isClosed ( void )
/************************************************************************
 * pggfawp_isClosed  					       		*
 *									*
 *  This routine checks if a FZLVL is closed or not.		 	*
 *									*
 *  Boolean pggfawp_isClosed( )						*
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

void pggfawp_getLineWidth ( VG_DBStruct *el )
/************************************************************************
 * pggfawp_getLineWidth					       		*
 *									*
 *  This routine gets the line width from the settings table acccording *
 *  to the GFA subtype.						 	*
 *									*
 *  void pggfawp_getLineWidth ( el )					*
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
    if ( pggfawp_isUp() ) {

       el->hdr.maj_col	= _attrColor;
       el->hdr.min_col	= _attrColor;	

    }
}

/*=====================================================================*/

static void pggfawp_initIssueType ( void ) 
/************************************************************************
 * pggfawp_initIssueType					       	*
 *									*
 *  This routine sets the issue type to NRML.  				*
 *									*
 *  static void pggfawp_initIssueType ( void )				*
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

static void pggfawp_setApplyBtn ( Boolean enable )
/************************************************************************
 * pggfawp_setApplyBtn					       		*
 *									*
 *  This routine enables/disables the apply button and also sets the 	*
 *  background color of the button.		 			*
 *									*
 *  static void pggfawp_setApplyBtn ( enable )				*
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
 *  J. Wu/SGT		06/14	Enable "Apply" only if there are 	*
 *                              element(s) selected			*
 ***********************************************************************/
{
    int 	ier, nsel;
    Pixel 	bgColor;
/*---------------------------------------------------------------------*/
    /*
     *  Only enable when there is at least one element selected!
     */
    nsel = pghdlb_elemSelected();
    
    if ( enable && nsel > 0 ) {

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

static void pggfawp_initDueTo ( void ) 
/************************************************************************
 * pggfawp_initDueTo					       		*
 *									*
 *  This routine sets the DUE_TO menu to the empty string if there is 	*
 *  one.								*
 *									*
 *  static void pggfawp_initDueTo ( void )				*
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
static void pggfawp_popupTypeDlgCb ( Widget wid, XtPointer which, 
				    XtPointer call )
/************************************************************************
 * pggfawp_popupTypeDlgCb				       		*
 *									*
 *  This routine is the callback for the push button(s) in the second 	*
 *  panel. It pops up/down the IFR type edit dialog window associated 	*
 *  with the button.							*
 *									*
 *  static void pggfawp_popupTypeDlgCb ( wid, which, call )		*
 *									*
 *  Input parameters:							*
 *	wid	Widget		the push button				*
 *	which	XtPointer	the dialog window			*
 *	call	XtPointer	callback structure (not used)		*
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
static void pggfawp_closeTypeDlgCb ( Widget wid, XtPointer which, 
				    XtPointer call )
/************************************************************************
 * pggfawp_closeTypeDlgCb				       		*
 *									*
 *  This routine is the callback for the close button in the IFR type	*
 *  edit dialog window. It pops down the dialog window.		 	*
 *									*
 *  static void pggfawp_closeTypeDlgCb ( wid, which, call )		*
 *									*
 *  Input parameters:							*
 *	wid	Widget		the close button			*
 *	which	XtPointer	the dialog window			*
 *	call	XtPointer	callback structure (not used)		*
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

    dlg = (Widget) which;
    XtUnmanageChild ( dlg );
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_setTypeTextCb ( Widget wid, XtPointer clnt, 
				   XtPointer call )
/************************************************************************
 * pggfawp_setTypeTextCb					       	*
 *									*
 *  This routine is the callback for the toggle buttons in the IFR or   *
 *  MT OBSC type edit dialog window and the checkbox in the 2nd panel.  *
 *  It sets the contents for the text field in the 2nd panel.           *
 *									*
 *  static void pggfawp_setTypeTextCb ( wid, clnt, call )		*
 *									*
 *  Input parameters:							*
 *	wid	Widget		the toggle button			*
 *	clnt	XtPointer	client data (not used )			*
 *	call	XtPointer	callback structure (not used)		*
 *									*
 *  Output parameters:							*
 *		None							*
 *  Return:								*
 *		None							*
 **									*
 * Log:									*
 *  B. Yin/SAIC		03/06	Created 				*
 *  B. Yin/SAIC		04/06	Removed the code to find current panel2.*
 *  E. Safford/SAIC	06/06	use " " not "/" between CIG & VIS	*
 *  E. Safford/SAIC	07/06   use ctb_gfaCombineIFRTypes to format    *
 *				  CIG/VIS string			*
 *  B. Yin/SAIC		09/06	set the type field in IFR warning window*
 *  B. Yin/SAIC		09/06	check if the text field is created      *
 *  B. Yin/SAIC         06/07   add code for MT OBSC type               *
 *  J. Wu/SAIC          06/08   set typeText only if popups exist	*
 *  B. Yin/SAIC         06/08   change IFR to C&V                 	*
 ***********************************************************************/
{
    int         ii, jj, ier;
    char        typeStr[ STD_STRLEN ], *labelStr, *tmpStr, *optionValue;
    char        outTypeStr[ STD_STRLEN ], cigStr[ 64 ], visStr[ 64 ];
    char        *ptr;

    XmString    xmStr;
    Widget      wPulldown, wPushButton;
    Boolean     labelFlag = False, ifr, mtobsc, noVis = False;
/*---------------------------------------------------------------------*/
/*
 *  Enable the 'Apply' button.
 */
    pggfawp_setApplyBtn( True );

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

    if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "C&V" ) == 0 ) {
        ifr = True;
    }
    else if ( strcasecmp ( _areaTyp[ _areaTypeIndex ], "MT_OBSC" ) == 0 ) {
        mtobsc = True;
    }
/*
 *  Loop over CIG and VIS option menus in C&V GUI
 *  Write the value to text string
 */
    if ( ifr ) {

       cigStr[ 0 ] = '\0';
       visStr[ 0 ] = '\0';

       for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

           XtVaGetValues ( _currentPanel2->descStrc[ ii ].label,
                           XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &labelStr );

           XmStringFree( xmStr );

           XtVaGetValues ( _currentPanel2->descStrc[ ii ].menu,
                        XmNsubMenuId, &wPulldown, NULL );
           XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

           XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &optionValue );


           if ( strcasecmp( labelStr, "CIG:" ) == 0 ) {

              if ( strlen( optionValue ) > 0 &&
                   strcasecmp( optionValue, "N/A" )  != 0 &&
                   strcasecmp( optionValue, "NONE" ) != 0 ) {

                 sprintf( cigStr, "CIG %s/", optionValue );

              }

           }
           else if ( strcasecmp( labelStr, "VIS:" ) == 0 ) {

              if ( strlen( optionValue ) > 0 &&
                   strcasecmp( optionValue, "N/A" )  != 0 &&
                   strcasecmp( optionValue, "NONE" ) != 0 ) {

                 sprintf( visStr, "VIS %s", optionValue );

              }

              noVis = ( strlen( optionValue ) > 0 &&
                        ( strcasecmp( optionValue, "N/A" )  == 0 ||
			  strcasecmp( optionValue, "NONE" )  == 0 ) );

           }

           XtFree ( optionValue );
           XtFree ( labelStr );
           XmStringFree( xmStr );
       }

       sprintf( typeStr, "%s%s ", cigStr, visStr );

       ptr = typeStr;
       while ( ptr < typeStr + strlen( typeStr ) ) {
                if ( *ptr == '_' ) *ptr = ' ';
                ptr++;
       }

    }
                                                                                               
/*
 *  Loop over all popup dialog windows.
 */
    if ( !noVis ) {
	
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
    }

    if ( ifr ) {
       ctb_gfapCombineIFRTypes( typeStr, NULL, outTypeStr, &ier );
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
    
    if ( mtobsc && _mtobscTextW && XtIsManaged( _mtobscTextW ) ) {
       XtVaSetValues ( _mtobscTextW, XmNvalue, outTypeStr, NULL );
    }

    if ( pggfawp_CvHasType() ) {

        if ( ( _ifrWarningW != NULL ) && XtIsManaged ( _ifrWarningW ) ) {
              _okOnWarning = True;
           XtUnmanageChild ( _ifrWarningW );
        }

    }
}

/*=====================================================================*/

static void pggfawp_setFzl ( Boolean value )
/************************************************************************
 * pggfawp_setFzl 	                                                *
 *                                                                      *
 * This routine enables/disables the freezing level input filed.    	*
 *                                                                      *
 * void pggfawp_setFzl ( value ) 			        	*
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
static void pggfawp_verifyFzlCb( Widget wid, XtPointer clnt, 
				XtPointer call )
/************************************************************************
 * pggfawp_verifyFzlCb                                                   *
 *                                                                      *
 * This routine checks the validation of top/bottom freezing levels.    *
 *                                                                      *
 * void pggfawp_verifyFzlCb ( wid, clnt, call ) 	        	*
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
static void pggfawp_fillFzlCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfawp_fillFzlCb							*
 *									*
 * Callback function is used to fill the freezing levels.		*
 *									*
 * static void pggfawp_fillFzlCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		int		client data, not used		*
 *   cbs		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		04/06		Created				*
 * J. Wu/SAIC		04/08		Check top/bot limit in prefs.tbl*
 ***********************************************************************/
{
    int		ier;
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
    
    /*
     *  Check top/bottom against GFA_TOP_LIMIT/GFA_BOTTOM_LIMIT in "prefs.tbl"
     */
    ctb_pfstr ( "GFA_TOP_LIMIT", tmpStr, &ier );    

    if ( ier == 0 ) {
        
	if ( (strlen( topStr ) > 0) && (atoi( topStr ) > atoi( tmpStr )) ) {
            sprintf ( tmpStr, "%d", atoi( tmpStr ) );
            cst_padString ( tmpStr, '0', 1, 3, topStr );
        }
    }

    ctb_pfstr ( "GFA_BOTTOM_LIMIT", tmpStr, &ier );    

    if ( ier == 0 ) {
        if ( (strlen( bottomStr ) > 0) && (atoi( bottomStr ) < atoi( tmpStr )) ) {
            strcpy  ( bottomStr, "SFC" );
	}
    }
        
    
    /*
     *  Fill & set the Top/Bottom string
     */
    tmpStr[ 0 ] = '\0';
    sprintf ( tmpStr, "%s/%s", topStr, bottomStr );

    if ( strlen ( tmpStr ) > (size_t)1 ) {

       XtVaSetValues ( wid, XmNvalue, tmpStr, NULL );

    }

    XtFree ( ptext );

}

/*=====================================================================*/

static void pggfawp_changedFLCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfawp_changedCb							*
 *									*
 * Callback function is used to enable/disable FZL top/bottom for ICE.	*
 *									*
 * static void pggfawp_changedFLCb ( wid, clnt, cbs )			*
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

	  levels = (levels_t*)clnt;

          if ( !fzlOn ) XtVaSetValues ( levels->fzlLevel, XmNvalue, "", NULL );

          XtVaSetValues ( levels->fzlLabel, XmNsensitive, fzlOn, NULL );

          XtVaSetValues ( levels->fzlLevel, XmNsensitive, fzlOn, NULL );

       }

       XtFree ( text );

    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_addFcstHrColon ( Widget wid, XtPointer clnt, 
						XtPointer call )
/************************************************************************
 * pggfawp_addFcstHrColon                                          	*
 *                                                                      *
 * Callback function for the optional forecast hour text. It checks the	*
 * current content of _fcsthrText and automatically adds a colon (':')	*
 * to it if _fcsthrText contains only 1 digit.                		*
 *                                                                      *
 * static void pggfawp_addFcstHrColon ( wid, which, call )       	*
 *                                                                      *
 * Input parameters:                                                    *
 *   wid                Widget          Widget ID                       *
 *   clnt     		XtPointer 	Widget's event data (not used)	*
 *   call       	XtPointer 	Callback structure          	*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           05/06   initial coding                          *
 * J. Wu/SAIC           03/08   add a colon onif if the hour < 6        *
 ***********************************************************************/
{
    char    *ptext = NULL;    
/*---------------------------------------------------------------------*/

    XtVaGetValues ( wid, XmNvalue, &ptext, NULL );

    if ( ptext ) {

	if ( strlen ( _tmpFcsthrStr ) == (size_t)0 &&
	     strlen( ptext ) == (size_t)1 &&
	     atoi( ptext ) < 6 ) {
	
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

static int pggfawp_getNextTag ( void )
/************************************************************************
 * pggfawp_getnextTag    						*
 *									*
 * This function returns the next available tag number for the current  *
 * desk.								*
 *									*
 * static int pggfawp_getNextTag( )                                 	*
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

    pggfawp_getTagsForDesk ( vgfname, fptr, _desks[ _deskStrc.current ], 
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
static void pggfawp_deskCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggfawp_deskCb							*
 *									*
 * Callback function for the desk menu.					*
 *									*
 * static void pggfawp_deskCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which desk			*
 *   call		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06		Created				*
 ***********************************************************************/
{
   long nn, mm;
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

   pggfawp_setDfltTag( _fcsthr[ _currFcsthr ] );

   pggfawp_setApplyBtn( True );

}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_tagCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pggfawp_tagCb								*
 *									*
 * Callback function for the tag number menu.				*
 *									*
 * static void pggfawp_tagCb ( wid, which, call )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		long		which tag			*
 *   call		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		06/06		Created				*
 ***********************************************************************/
{
   _tagStrc[ _deskStrc.current ][ _areaTypStrc.current ].current = (int)which;
   pggfawp_setApplyBtn( True );
}

/*=====================================================================*/

static void pggfawp_addTag( int tag )
/************************************************************************
 * pggfawp_addTag							*
 *									*
 * This function puts the input tag into the tag menu if it is not there*
 * .									*
 *									*
 * void pggfawp_addTag ( tag )						*
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

static void pggfawp_setTagMenu( char tag[] )
/************************************************************************
 * pggfawp_setTagMenu							*
 *									*
 * This function sets the current desk(region) and the current tag      *
 * number using the input tag string(tag# and desk).                    *
 *									*
 * void pggfawp_setTagMenu ( tag )					*
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

    pggfawp_deskCb ( NULL, curDesk, NULL );
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

static void pggfawp_getTagMenu( char tag[], char *tagNum, char *desk )
/************************************************************************
 * pggfawp_getTagMenu                                                   *
 *                                                                      *
 * This function gets the current desk(region) and the current tag      *
 * number using the input tag string(tag# and desk).                    *
 *                                                                      *
 * void pggfawp_getTagMenu ( tag )                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      tag[]     char    tag number and desk(region)                   *
 *                                                                      *
 * Output parameters:                                                   *
 *      *tagNum   char    tag number                                    *
 *      *desk     char    desk(region)                                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           01/10   Created.                                *
 ***********************************************************************/
{
    int         ii;
/*---------------------------------------------------------------------*/

    ii = 0;
    while ( ii <= (int)strlen( tag ) && isdigit( tag[ ii ] ) ) ii++;

    strncpy( tagNum, tag, ii );
    tagNum[ ii ] = '\0';

    strcpy( desk, &tag[ ii ] );

}
/*=====================================================================*/

static void pggfawp_setAllTags ( void )
/************************************************************************
 * pggfawp_setAllTags							*
 *									*
 * This function goes through all GFA elements in the work file and     *
 * puts all tags in use into the tag menu. It also sets the default tag.*
 *									*
 * void pggfawp_setAllTags ( )						*
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

            pggfawp_getTagsForDesk ( vgfname, fptr, _desks[ ii ], 
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
    pggfawp_setDfltTag( _fcsthr[ _currFcsthr ] );
}

/*=====================================================================*/

static void pggfawp_setDfltTag( char fcsthr[] )
/************************************************************************
 * pggfawp_setDfltTag							*
 *									*
 * This function sets the default tag for the input forecast hour. If   *
 * the forecast hour is 0, the tag will be set as New. In any other     *
 * cases, the tag will be set as the last in the tag array. It also	*
 * appends an asterisk for the tags in use for the forecast hour.	*
 *									*
 * void pggfawp_setDfltTag ( fcsthr )					*
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

        if ( pggfawp_tagInUse( vgfname, fptr, tag, fcsthr ) ) {

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

static Boolean pggfawp_tagInUse( char vgfname[], FILE *fptr, char tag[], 
				char fcsthr[] )
/************************************************************************
 * pggfawp_tagInUse							*
 *									*
 * This function takes a tag string as input and checks if the tag is in*
 * use for the same forecast hour.                                      *
 *									*
 * void pggfawp_tagInUse ( vgfname, fptr, tag, fcsthr )			*
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

static void pggfawp_getTagsForDesk ( char vgfname[], FILE *fptr, char desk[], 
				    char haz[], int **tags, int *ntags )
/************************************************************************
 * pggfawp_getTagsForDesk						*
 *									*
 * This function gets all tags in use for a desk(region) and a hazard	*
 * in the input vg file. Variable tags should be freed by the calling 	*
 * routine.								*
 *									*
 * void pggfawp_getTagsForDesk ( vgfname, fptr, desk, tags, ntags )	*
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
 * B. Yin/SAIC		08/08	Handle IFR/MVFR and 20kt SFC_WND       	*
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

	     if ( strcasecmp( hazStr, "IFR" ) == 0 ||
	          strcasecmp( hazStr, "MVFR" ) == 0 ) {

			strcpy( hazStr, "C&V" );

	     }

	     if ( strcasecmp( hazStr, "SFC_WND20" ) == 0 ) {

			strcpy( hazStr, "SFC_WND" );

	     }


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
		          G_REALLOC ( *tags, int, *ntags, "pggfawp_getTagForDesk tags" );
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

int pggfawp_getHazardType ( char hazard[] )
/************************************************************************
 * pggfawp_getHazardType						*
 *									*
 * This function returns the integer hazard type of the input hazard 	*
 * name.								*
 *									*
 * int pggfawp_getHazardType ( hazard )					*
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
 * J. Wu/SAIC		06/08	Added GFA_HAZARD_TS              	*
 * B. Yin/SAIC		06/08	Added MVFR and C&V              	*
 * B. Yin/SAIC		07/08	Added 20KT surface wind              	*
 * L. Hinson/AWC        04/09   Add GFA SIGMET Hazards                  *
 * L. Hinson/AWC        11/09   Add GFA_HAZARD_CLD_TOPS                 *
 ***********************************************************************/
{

    if ( strcasecmp ( hazard, "IFR" ) == 0 )           return GFA_HAZARD_IFR;
    else if ( strcasecmp ( hazard, "MVFR" ) == 0 )     return GFA_HAZARD_MVFR;
    else if ( strcasecmp ( hazard, "MT_OBSC" ) == 0 )  return GFA_HAZARD_MT_OBSC;
    else if ( strcasecmp ( hazard, "ICE" ) == 0 )      return GFA_HAZARD_ICE;
    else if ( strcasecmp ( hazard, "TURB" ) == 0 )     return GFA_HAZARD_TURB;
    else if ( strcasecmp ( hazard, "TURB-HI" ) == 0 )  return GFA_HAZARD_TURB_HI;
    else if ( strcasecmp ( hazard, "TURB-LO" ) == 0 )  return GFA_HAZARD_TURB_LO;
    else if ( strcasecmp ( hazard, "SFC_WND" ) == 0 )  return GFA_HAZARD_SFC_WND;
    else if ( strcasecmp ( hazard, "SFC_WND20" ) == 0 )return GFA_HAZARD_SFC_WND20;
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
    else if ( strcasecmp ( hazard, "ICE_SIGMET" ) == 0)return GFA_SIGMET_ICE;
    else if ( strcasecmp ( hazard, "TURB_SIGMET" ) ==0)return GFA_SIGMET_TURB;
    else if ( strcasecmp ( hazard, "DS_SIGMET" ) == 0) return GFA_SIGMET_DS;
    else if ( strcasecmp ( hazard, "SS_SIGMET" ) == 0) return GFA_SIGMET_SS;
    else if ( strcasecmp ( hazard, "VASH_SIGMET" ) ==0)return GFA_SIGMET_VASH;
    else if ( strcasecmp ( hazard, "CLD_TOPS" ) == 0 ) return GFA_HAZARD_CLD_TOPS;
    else if ( strcasecmp ( hazard, "C&V" ) == 0 ) {

        if ( pggfawp_isIFR() )  return GFA_HAZARD_IFR;
        else                    return GFA_HAZARD_MVFR;

    }

    return -1;

}

/*=====================================================================*/

static int pggfawp_getCategoryType ( void )
/************************************************************************
 * pggfawp_getCategoryType   						*
 *									*
 * This function checks the forecast hours in the GFA GUI and returns   *
 * an integer category type corresponding to the forecast hour. 	*
 *									*
 * static int pggfawp_getCategoryType ( )				*
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
 * E. Safford/SAIC	04/07	use GFA_FBBA_AIRMET and OUTLOOK		*
 * J. Wu/SAIC		03/08	ditinguish between smears & F/BB/As	*
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

	cat = GFA_FBBA_AIRMET;

        if ( !_addMode && _editSmear ) {	        
            cat =  GFA_USER_SMEAR; 
        }
    }
    else {

	ptr[ 0 ] = '\0';

	cat = ( atoi( hours ) < 6 ) ? GFA_FBBA_AIRMET : GFA_FBBA_OUTLOOK;
        
	if ( !_addMode && _editSmear ) {            	        
	    cat = ( atoi( hours ) < 6 ) ? GFA_USER_SMEAR : GFA_USER_OUTLOOK; 
        }

    }
    return cat;
}

/*=====================================================================*/

void pggfawp_makeSubtype ( int hazType, int catType, int *subtype, int *iret )
/************************************************************************
 * pggfawp_makeSubtype							*
 *									*
 * This function calculates the subtype from the hazard type and the	*
 * category type.                                                     	*
 *									*
 * void pggfawp_makeSubtype ( hazType, catType, sutype, iret)   	*
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

void pggfawp_getHazCat ( int subtype, int *hazType, int *catType, int *iret )
/************************************************************************
 * pggfawp_getHazCat							*
 *									*
 * This function takes a subtype and determines the hazard type and 	*
 * category type.                                                     	*
 *									*
 * void pggfawp_getHazCat ( subtype, hazType, catType, iret)   		*
 *									*
 * Input parameters:							*
 *	subType         int	subtype        				*
 *									*
 * Output parameters:							*
 *	*hazType	int	hazard type				*
 *	*catType	int	category type				*
 *	*iret		int	return code. 				*
 *					0:	normal			*
 *					-1:	invalid subtype		* 
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07	initial coding	               		*
 ***********************************************************************/
{
    *iret = 0;

    if( subtype < 10 ) {
        *iret = -1;
    }
    else {
        *catType = subtype % 10;
        *hazType = subtype - *catType;
    }
}

/*=====================================================================*/

static int pggfawp_getCurrentSubtype ( void )
/************************************************************************
 * pggfawp_getCurrentSubtype						*
 *									*
 * This function calculates the subtype from the hazard type and the	*
 * category type.                                                     	*
 *									*
 * void pggfawp_getCurrentSubtype ( )			   		*
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
 * B. Yin/SAIC		07/08	Set hazard type for 20 KT SFC WND as  	*
 *				SFC_WND20				*
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
    else if ( pggfawp_is20KtSfcWind() ) {

	strcat( haz, "20" );

    }
    
    pggfawp_makeSubtype ( pggfawp_getHazardType( haz ),
    			 pggfawp_getCategoryType(), &subtype, &ier );
    
    if ( ier < 0 ) subtype = -1;

    return subtype;

}

/*=====================================================================*/

static void pggfawp_resetForSubtype( int *iret )
/************************************************************************
 * pggfawp_resetForSubtype						*
 *									*
 * This function resets the color picker for the current subtype.   	*
 *									*
 * void pggfawp_resetForSubtype ( iret)   				*
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
    
    subtype = pggfawp_getCurrentSubtype();

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
static void pggfawp_showOrigIfrWarning ( Widget parent )
/************************************************************************
 * pggfawp_showOrigIfrWarning                                           *
 *                                                                      *
 * This routine pops up a warning window if IFR type is empty. 		*
 * --Note: C&V calls pggfawp_showIfrWarning if C&V type is empty.--     *
 *                                                                      *
 * static void pggfawp_showIfrWarning ( parent )		 	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	parent		Widget		parent widget	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * L. Hinson            11/09   Original IFR Warning Dialog from 5.11.3 *
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
			(XtCallbackProc)pggfawp_cigChkBoxCb, 
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
			(XtCallbackProc)pggfawp_popupTypeDlgCb, 
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
		(XtCallbackProc)pggfawp_ifrWarningBtnCb, (XtPointer) ii );

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
static void pggfawp_cigChkBoxCb( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_cigChkBoxCb							*
 *									*
 * Callback function to set the CIG type in the GFA right panel.	*
 *									*
 * static void pggfawp_cigChkBoxCb ( wid, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data, not used		*
 *   call		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		09/06		Created				*
 * L. Hinson/AWC        11/09           Re-Implemented as original      *
 *                                      pggfawp_showIfrWarning named    *
 *                                      as pggfawp_showOrigIfrWarning   *
 *                                      in this module requires this    *
 ***********************************************************************/
{
    XmToggleButtonSetState ( _currentPanel2->checkbox[ 0 ],
			     XmToggleButtonGetState ( wid ),
			     True );
}

/*=====================================================================*/

static void pggfawp_showIfrWarning ( Widget parent )
/************************************************************************
 * pggfawp_showIfrWarning                                               *
 *                                                                      *
 * This routine pops up a warning window if IFR type is empty. 		*
 *                                                                      *
 * static void pggfawp_showIfrWarning ( parent )		 	*
 *                                                                      *
 * Input parameters:                                                    *
 * 	parent		Widget		parent widget	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC		09/06	Created					*     
 * B. Yin/SAIC		09/06	Free warningBtn WidgetList.		*     
 * B. Yin/SAIC          06/07   Change _curTypeDlg to IFR type dialog.  *
 * B. Yin/SAIC          06/08   Change IFR to C&V.			*
 ***********************************************************************/
{
    long		ii;
    char		*btnStr[] = { "OK", "Cancel" };

    Widget		pane, msg, btnForm, form1;
    WidgetList		warningBtn;
/*---------------------------------------------------------------------*/
/*
 *  If the warning window has been created, set the type field to 
 *  blank and the cig check box to false, and manage the window.
 */ 
    if ( _ifrWarningW ) {

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
                    XmNdialogStyle,     XmDIALOG_MODELESS,
                    NULL );

    XtVaSetValues ( XtParent ( _ifrWarningW ),
                    XmNtitle,           "Warning: Incomplete C&V",
                    NULL );

/*
 *  Create a parent pane widget, a form, a warning message and a
 *  rowcol widget.
 */
    pane = XtVaCreateManagedWidget ( "pggfa_wnpane",
                       xmPanedWindowWidgetClass, _ifrWarningW,
                       XmNseparatorOn,           False,
                       XmNmarginHeight,          15,
                       XmNmarginWidth,           0,
                       XmNspacing,               5,
                       XmNsashWidth,             1,
                       XmNsashHeight,            1,
                       NULL );

    form1 = XtVaCreateManagedWidget ("form1",
                        xmFormWidgetClass, pane, NULL );

    msg = XtVaCreateManagedWidget ( "Please select CIG/VIS and/or add weather types.",
                       xmLabelWidgetClass, form1,
                       XmNmarginRight,     100,
                       XmNx,               10,
                       NULL );

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
                XmNleftPosition,                ((ii * 40) + 10 ),
                XmNrightAttachment,             XmATTACH_POSITION,
                XmNrightPosition,               (((ii + 1) * 40) - 10),
                NULL );

        XtAddCallback ( warningBtn[ ii ], XmNactivateCallback,
                (XtCallbackProc)pggfawp_ifrWarningBtnCb, (XtPointer) ii );

    }

    XtFree( (XtPointer) warningBtn );

    XtManageChild ( _ifrWarningW );

    XtVaSetValues ( XtParent( _ifrWarningW ),
            XmNminWidth,        360,
            XmNmaxWidth,        360,
            XmNminHeight,       80,
            XmNmaxHeight,       80,
            NULL );

}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_ifrWarningBtnCb( Widget wid, XtPointer which, XtPointer call )
/************************************************************************
 * pggfawp_ifrWarningBtnCb						*
 *									*
 * Callback function to set the CIG type in the GFA right panel.	*
 *									*
 * static void pggfawp_ifrWarningBtnCb ( wid, which, call )		*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   which		XtPointer	client data			*
 *   call		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * B. Yin/SAIC		09/06		Created				*
 * B. Yin/SAIC		07/08		Call pggfawp_CvHasType to check	*
 *					if C&V has types.		*
 ***********************************************************************/
{
    long whichBtn;
/*---------------------------------------------------------------------*/

    whichBtn = (long)which;

    switch ( whichBtn ) {

	case 0:			/* OK */

           if ( pggfawp_CvHasType() ) {

              _okOnWarning = True;
              XtUnmanageChild ( _ifrWarningW );
              XtUnmanageChild ( _curTypeDlg );

              }
              else  {

                 XtManageChild ( _curTypeDlg );
           }

           break;

	case 1:			/* Cancel */

	   _cancelOnWarning = True;
	   XtUnmanageChild ( _ifrWarningW );
	   XtUnmanageChild ( _curTypeDlg );

	   break;
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_panel3TextCb( Widget wid, XtPointer clnt, XtPointer call )
/************************************************************************
 * pggfawp_panel3TextCb							*
 *									*
 * Callback function to set the Apply button on if changes are made to  *
 * any of the panel3 text widgets.					*
 *									*
 * static void pggfawp_panel3TextCb ( wid, clnt, call )			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID			*
 *   clnt		XtPointer	client data			*
 *   call		XtPointer	callback struct, not used	*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07		initial coding			*
 ***********************************************************************/
{
   pggfawp_setApplyBtn( True );
}

/*=====================================================================*/

Boolean pggfawp_getFzlRangesFromFile ( char vgfile[], char ranges[] )
/************************************************************************
 * pggfawp_getFzlRangesFromFile						*
 *									*
 * This routine reads the freezing level ranges from the input vg file.	*
 * If there in no ranges info in the input file, it returns false and  	*
 * sets the ranges string to blank.					*
 *									*
 * Boolean pggfawp_getFzlRangesFromFile ( vgfile,  ranges )		*
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

static void pggfawp_managePanel3 ( Boolean state )
/************************************************************************
 * pggfawp_managePanel3         					*
 *									*
 * This routine manages/unmanages panel 3 as per the state param.      	*
 *									*
 * static void pggfawp_managePanel3 ( state )         			*
 *									*
 * Input parameters:							*
 *   	state		Boolean		true/false = managed/unmanaged	*
 *									*
 * Output parameters:							*
 *	None								*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07		initial coding			*
 * E. Safford/SAIC	06/07	pggfawp_isPanel3Up --> pggfawp_isFBBA	*
 * J. Wu/SAIC		03/08	do not manage if we are editing smears	*
 ***********************************************************************/
{
    if( pggfawp_isFBBA() != state ) {
        if( state ) {
	    XtManageChild ( _frameFBBA );
	}
	else {
	    XtUnmanageChild ( _frameFBBA );
	}	
    }
    
    if ( _editSmear && XtIsManaged( _frameFBBA ) )  {
	XtUnmanageChild ( _frameFBBA );    
    }
}

/*=====================================================================*/

static Boolean pggfawp_isFBBA ( void )
/************************************************************************
 * pggfawp_isFBBA	         					*
 *									*
 * This routine returns True if the current element (being drawn or    	*
 * edited) is determined to be a FROM/BOUNDED BY/ALG element.  Any GFA  *
 * element with a '-' in the forecast hour is an F/BB/A element.  If 	*
 * the _frameFBBA widget is currently managed, then this function       *
 * true.								*
 *									*
 * static Boolean pggfawp_isFBBA ( void )         			*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *	None								*
 *									*
 * Return:								*
 *			Boolean		True if FBBA element.		*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07		initial coding			*
 * E. Safford/SAIC	06/07		rename function 		*
 ***********************************************************************/
{
    return( XtIsManaged (_frameFBBA) );
}

/*=====================================================================*/

static void pggfawp_showMtobscWarning ( Widget parent )
/************************************************************************
 * pggfawp_showMtobscWarning                                             *
 *                                                                      *
 * This routine pops up a warning window if MT OBSC type is empty.      *
 *                                                                      *
 * static void pggfawp_showMtobscWarning ( parent )                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent          Widget          parent widget                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          06/07   Created                                 *
 * L. Hinson/AWC        09/05   Take out word "OCNL"                    *
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
                        (XtCallbackProc)pggfawp_popupTypeDlgCb,
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
                (XtCallbackProc)pggfawp_mtobscWarningBtnCb, (XtPointer) ii );
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
static void pggfawp_mtobscWarningBtnCb ( Widget wid, XtPointer which,
                                                        XtPointer cbs )
/************************************************************************
 * pggfawp_mtobscWarningBtnCb                                           *
 *                                                                      *
 * Callback function for control buttons in MT OBSC warning dialog.     *
 *                                                                      *
 * static void pggfawp_mtobscWarningBtnCb ( wid, clnt, call )           *
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
static void pggfawp_colorZeroCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pggfawp_colorZeroCb                                                  *
 *                                                                      *
 * This is the callback function for the color picker button. If the    *
 * color of the GFA is zero, the color is set to black(32) and then 	*
 * popup the color picker.                                              *
 *                                                                      *
 * void pggfawp_colorZeroCb ( wid, which, cbs )                         *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget         widget ID                                *
 *      clnt    XtPointer      client data                              *
 *      cbs     XtPointer      callback structure                       *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           07/07   Created                                *
 ***********************************************************************/
{
    if ( _attrColor == 0 ) _attrColor = BLACK;
    NxmClrW_popup( wid, (XtPointer)&_attrColor, cbs );
}

/*=====================================================================*/

void pggfawp_checkGfa3Lines( VG_DBStruct *el )
/************************************************************************
 * pggfawp_checkGfa3Lines                                               *
 *                                                                      *
 * This routine checks if there are more than 3 from lines in an FBBA.	*
 * A warning box will pop up if there are more than 3 from lines. 	*
 *                                                                      *
 * void pggfawp_checkGfa3Lines ( *el )		                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el     VG_DBStruct      input element	                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           03/08   Created                                *
 * B. Yin/SAIC		 04/08	 Do not show the warning when moving the*
 *				 text box. Change the warning message.	*
 ***********************************************************************/
{
    int         ier, subtype;
    char        subtypeStr[ 32 ], prefix[ 32 ], haz[ 32 ];
    char        contourStr[ 32 ];
                                                                               
    Boolean     cbf = False;
/*---------------------------------------------------------------------*/
                                                                               
    /*
     * Do not show the warning when moving text box.
     */
    if ( !_show3LinesWarning || _txtActive ) return;

    subtypeStr[ 0 ]= '\0';
    cvg_getFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );
                                                                              
    subtype = atoi( subtypeStr ) % 10;
                                                                               
    /*
     *  Return if not an FBBA.
     */
    if ( subtype != GFA_FBBA_AIRMET &&
         subtype != GFA_FBBA_OUTLOOK ) return;
                                                                              
    /*
     *  Set the prefix string required by the 3-lines check routine.
     */
    haz[ 0 ] = '\0';
    cvg_getFld ( el, TAG_GFA_AREATYPE, haz, &ier );
                                                                               
    prefix[ 0 ] = '\0';

    if ( strcasecmp( haz, "FZLVL" ) == 0 ) {
                                                                               
        cvg_getFld( el, "Contour", contourStr, &ier );
                                                                              
        if ( strcasecmp( contourStr, "close" ) == 0 ) {
                                                                              
           strcpy ( prefix, "CLOSED FZLVL" );
                                                                              
        }
        else {
                                                                             
           strcpy ( prefix, "OPEN FZLVL" );
                                                                               
        }
    }
    else {
                                                                                
        strcpy ( prefix, ( subtype == GFA_FBBA_AIRMET )?"FROM":"BOUNDED BY" );
                                                                                
    }
                                                                           
    /*
     *  Check if there are more than 3 FROM lines.
     */
    cbf = cgr_canBeFormatted ( el->elem.gfa.info.npts, el->elem.gfa.latlon,
                               &(el->elem.gfa.latlon[ el->elem.gfa.info.npts]),
                               prefix );
                                                                                
    if ( !cbf ) {
                                                                                
       NxmWarn_show ( _gfaForm, "This FBBA will generate more than 3 FROM lines when formatted!" );
                                                                             
    }
}

/*=====================================================================*/

void pggfawp_statesListWarning( VG_DBStruct *el )
/************************************************************************
 * pggfawp_statesListWarning                                            *
 *                                                                      *
 * This routine pops up a warning window if a FBBA changes its location.*
 *                                                                      *
 * void pggfawp_statesListWarning ( *el )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el     VG_DBStruct      input element	                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           03/08   Created                                *
 ***********************************************************************/
{
    int         ier, subtype, operation;
    char        subtypeStr[ 32 ];
/*---------------------------------------------------------------------*/

    subtypeStr[ 0 ]= '\0';
    cvg_getFld ( el, TAG_GFA_SUBTYPE, subtypeStr, &ier );

    subtype = atoi( subtypeStr ) % 10;

    /*
     *  Return if not an FBBA.
     */
    if ( subtype != GFA_FBBA_AIRMET &&
         subtype != GFA_FBBA_OUTLOOK ) return;

    operation   = pgpalw_getCurOperId();

    if ( operation == FUNC_MODIFY       ||
         operation == FUNC_CONNECT      ||
         operation == FUNC_DELPOINT     ||
         operation == FUNC_PARTDELETE   ||
         operation == FUNC_MOVE         ||
         operation == FUNC_COPY ) {

       NxmWarn_show ( XtParent(XtParent(_gfaForm)), 
       	"FBBA states list may have changed. Please verify the states list" );

    }

}

/*=====================================================================*/

void pggfawp_setStatesBtn( Boolean enable ) 
/************************************************************************
 * pggfawp_setStatesBtn  	                                        *
 *                                                                      *
 * This routine sets the states button status and changes its color.	*
 *                                                                      *
 * void pggfawp_setStatesBtn ( enable )			                *
 *                                                                      *
 * Input parameters:                                                    *
 *      enable     Boolean      Flag to enable.disable states button    *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           03/08   Created                                *
 ***********************************************************************/
{

    int		ier;
    char	statesFlag[ 32 ];
    Pixel 	bgColor;
/*---------------------------------------------------------------------*/

    if ( enable ) {

       ctb_pfstr ( "GFA_ENABLE_STATES_BTN", statesFlag, &ier );

       if ( ier == 0 && strcasecmp( statesFlag, "TRUE" ) == 0 ) {

          XtSetSensitive ( _statesBtn, True );        

       }

       xsncolr ( APPLY_COLOR, &bgColor, &ier );

       XtVaSetValues ( _statesBtn, XmNbackground, bgColor, NULL );

    }
    else {

       XtSetSensitive ( _statesBtn, False );        

       XtVaGetValues ( _ctlButtons[ 1 ], XmNbackground, &bgColor, NULL );

       XtVaSetValues ( _statesBtn, XmNbackground, bgColor, NULL );

    }

}

/*=====================================================================*/
/* ARGSUSED */
static void pggfawp_statesListCb ( Widget wid, XtPointer clnt, XtPointer cbs )
/*************************************************************************
 * pggfawp_statesListCb                                                 *
 *                                                                      *
 * This is the callback function for the states list button. This 	*
 * callback disables the button and change its color to black.	 	*
 *                                                                      *
 * void pggfawp_stateListCb ( wid, clnt, cbs )                          *
 *                                                                      *
 * Input parameters:                                                    *
 *      wid     Widget         widget ID                                *
 *      clnt    XtPointer      client data                              *
 *      cbs     XtPointer      callback structure                       *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           04/08   Created                                *
 ***********************************************************************/
{

   pggfawp_setStatesBtn( False );

}

/*=====================================================================*/

static void pggfawp_createCvPanel( int nLabel )
/************************************************************************
 * pggfawp_createCvPanel                                                *
 *                                                                      *
 * This routine creates the right panel of the C&V hazard.  	 	*
 *                                                                      *
 * static void pggfawp_createCvPanel ( int )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      nLabel     int      number of labels in C&V right panel         *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           06/08   Created                                *
 ***********************************************************************/
{
    int         ii, jj, ier, inputIndex, pdIndex, checkboxIndex;
    int         nChoices, oneRow = 36, rows, nn, popupIndex;
    char        typeStr[ 32 ], desc[ MAXTBLNAME ], choice[ STD_STRLEN ];
    char        desc1[ MAXTBLNAME ];

    Arg         args[ 10 ];
    Widget      typePane, typeRowCol;
    Widget      addTypeBtn;
    XmString    xmStr;
/*---------------------------------------------------------------------*/

/* 
 *  Loop through all labels(option menu, input field, check box, etc.)
 *  Create widgets.
 */
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
	   
           if ( strcasecmp( desc, "Top:" ) == 0 ) {

               _panel2[ _nPanel2 ].inputField [ inputIndex ].label =
                        XtVaCreateManagedWidget ( desc,
                                xmLabelWidgetClass,     _panel2[ _nPanel2 ].form,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           8,
                                XmNleftAttachment,      XmATTACH_FORM,
                                XmNleftOffset,          300,
                                NULL );

               _panel2[ _nPanel2 ].inputField [ inputIndex ].text =
                        XtVaCreateManagedWidget ( "bottom_text",
                                xmTextWidgetClass,      _panel2[ _nPanel2 ].form,
                                XmNmaxLength,           nChoices,
                                XmNeditable,            TRUE,
                                XmNvalue,               "",
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           2,
                                XmNleftAttachment,      XmATTACH_FORM,
                                XmNleftOffset,          350,
                                XmNwidth,               65,
                                XmNrecomputeSize,       False,
                                NULL );
           }
           else {
		
                _panel2[ _nPanel2 ].inputField [ inputIndex ].label =
                        XtVaCreateManagedWidget ( desc,
                                xmLabelWidgetClass,     _panel2[ _nPanel2 ].form,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow + 5,
                                XmNleftAttachment,      XmATTACH_FORM,
                                XmNleftOffset,          0,
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
                                XmNwidth,               225,
                                XmNrecomputeSize,       False,
                                NULL );
				
           }
	   
           XtAddCallback ( _panel2[ _nPanel2 ].inputField [ inputIndex ].text,
                                 XmNmodifyVerifyCallback,
                                (XtCallbackProc)pggfawp_optPbCb, (XtPointer) NULL );

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
                                xmLabelGadgetClass,     _panel2[ _nPanel2 ].form,
                                XmNmarginLeft,          0,
                                XmNmarginRight,         10,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow,
                                XmNleftAttachment,      XmATTACH_FORM,
                                NULL );

/*
 *   Create the toggle button
 */
             _panel2[ _nPanel2 ].checkbox [ checkboxIndex ] =
                        XtVaCreateManagedWidget( " ",
                                xmToggleButtonWidgetClass, _panel2[ _nPanel2 ].form,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           rows * oneRow - 3,
                                XmNleftAttachment,      XmATTACH_WIDGET,
                                XmNleftWidget,          _panel2[ _nPanel2 ].checkboxLabel[ checkboxIndex ],
                                XmNmarginWidth,         5,
                                XmNhighlightThickness,  0,
                                NULL);

             XtAddCallback ( _panel2[ _nPanel2 ].checkbox [ checkboxIndex ],
                                XmNvalueChangedCallback,
                                (XtCallbackProc)pggfawp_setTypeTextCb,
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
                        XtVaCreateManagedWidget ( " ",
                                xmLabelGadgetClass,     _panel2[ _nPanel2 ].form,
                                XmNmarginLeft,          0,
                                XmNmarginRight,         10,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           oneRow + 4,
                                XmNleftAttachment,      XmATTACH_FORM,
                                XmNleftOffset,          160,
                                NULL );

/*
 *  Create the push button that brings up the popup dialog.
 */
             addTypeBtn = XtVaCreateManagedWidget( "Add/Remove Types",
                                xmPushButtonWidgetClass, _panel2[ _nPanel2 ].form,
                                XmNtopAttachment,       XmATTACH_FORM,
                                XmNtopOffset,           oneRow + 4,
                                XmNleftAttachment,      XmATTACH_FORM,
                                XmNleftOffset,          162,
                                XmNwidth,               252,
                                XmNmarginWidth,         5,
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
                                (XtCallbackProc)pggfawp_popupTypeDlgCb,
                                (XtPointer) (_panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg) );

/*
 *  Create pane to hold all widgets in the dialog.
 */
             typePane = XtVaCreateManagedWidget ( "gfaw_popup",
                                xmPanedWindowWidgetClass,       _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg,
                                XmNmarginHeight,                1,
                                XmNmarginWidth,                 1,
                                XmNspacing,                     2,
                                XmNsashWidth,                   1,
                                XmNsashHeight,                  1,
                                XmNseparatorOn,                 False,
                                XmNorientation,                 XmVERTICAL,
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
                                    (XtCallbackProc) pggfawp_setTypeTextCb,
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
                                (XtCallbackProc)pggfawp_closeTypeDlgCb,
                                (XtPointer) _panel2[ _nPanel2 ].popup[ popupIndex ].popupDlg );
             _panel2[ _nPanel2 ].nPopup++;
/*
 * Add the type text field
 */
             nn = 0;
             XtSetArg( args[nn], XmNrows,                    1 );          nn++;
             XtSetArg( args[nn], XmNcolumns,                 44 );         nn++;
             XtSetArg( args[nn], XmNeditable,                False );      nn++;
             XtSetArg( args[nn], XmNvisualPolicy,            XmCONSTANT ); nn++;
             XtSetArg( args[nn], XmNscrollingPolicy,         XmAUTOMATIC); nn++;
             XtSetArg( args[nn], XmNscrollBarDisplayPolicy,  XmAS_NEEDED); nn++;
             XtSetArg( args[nn], XmNcursorPositionVisible,   False );      nn++;
             XtSetArg( args[nn], XmNx,                       3 );          nn++;
             XtSetArg( args[nn], XmNy,                       2 * oneRow ); nn++;

             _panel2[ _nPanel2 ].typeText =
                        XmCreateScrolledText( _panel2[ _nPanel2 ].form,
                                              "typeText", args, nn );
             XtManageChild( _panel2[ _nPanel2 ].typeText );
             rows += 2;

        }
        else {                                       /* pull down menu */

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

             if ( strcasecmp( desc, "Coverage:" ) == 0 ) {
		
                pgutls_createOptionMenu ( _panel2[ _nPanel2 ].form, MAXNOPT,
                        (XtPointer)&_panel2[ _nPanel2 ].descStrc[ pdIndex ].current,
                        desc, (XtCallbackProc)pggfawp_optPbCb,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].label,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                         _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb, NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNtopOffset,           0,
                        XmNrecomputeSize,       False,
                        NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNleftOffset,          68,
                        NULL );

             }
             else if ( strcasecmp( desc, "cig:" ) == 0 ) {
		
                pgutls_createOptionMenu ( _panel2[ _nPanel2 ].form, MAXNOPT,
                        (XtPointer)&_panel2[ _nPanel2 ].descStrc[ pdIndex ].current,
                        desc, (XtCallbackProc)pggfawp_cigOptPbCb,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].label,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                         _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb, NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNtopOffset,           0,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNleftOffset,          155,
                        XmNrecomputeSize,       False,
                        NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNleftOffset,          30,
                        NULL );

             }
             else if ( strcasecmp( desc, "vis:" ) == 0 ) {

                pgutls_createOptionMenu ( _panel2[ _nPanel2 ].form, MAXNOPT,
                        (XtPointer)&_panel2[ _nPanel2 ].descStrc[ pdIndex ].current,
                        desc, (XtCallbackProc)pggfawp_cigOptPbCb,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].label,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                         _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb, NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNtopOffset,           oneRow,
                        XmNrecomputeSize,       False,
                        NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNleftOffset,          30,
                        NULL );

             }
             else {

                pgutls_createOptionMenu ( _panel2[ _nPanel2 ].form, MAXNOPT,
                        (XtPointer)&_panel2[ _nPanel2 ].descStrc[ pdIndex ].current,
                        desc, (XtCallbackProc)pggfawp_optPbCb,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].label,
                        &_panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                         _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb, NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].form,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNtopOffset,           rows * oneRow,
                        XmNrecomputeSize,       False,
                        NULL );

                XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].menu,
                        XmNleftAttachment,      XmATTACH_FORM,
                        XmNleftOffset,          110,
                        NULL );
             }

             XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].label,
                        XmNtopAttachment,       XmATTACH_FORM,
                        XmNtopOffset,           8,
                        NULL );

             for ( jj = 0; jj < nChoices; jj++ ) {

                 ctb_gfagdc( _areaTyp[ _areaTypStrc.current ],
                                &ii, &jj, choice, &ier);


                 xmStr = XmStringCreateLocalized ( choice );

                 XtVaSetValues ( _panel2[ _nPanel2 ].descStrc[ pdIndex ].pb[ jj ],
                              XmNlabelString,   xmStr,
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
    }
}

/*=====================================================================*/

static Boolean pggfawp_CvHasType( void )
/************************************************************************
 * pggfawp_CVHasType	                                                *
 *                                                                      *
 * This routine checks if a C&V hazard has weather types.  	 	*
 *                                                                      *
 * static Boolean pggfawp_CvHasType ( void )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:	                                                *
 *                      True	if C&V has types or hazard is not C&V   *
 *                      false	if C&V has not weather types   		*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           06/08   Created                                *
 ***********************************************************************/
{
    int         ii, jj;
    char        *labelStr, *optionValue;
    Boolean     wxType, isVis, isCig;

    XmString    xmStr;
    Widget      wPulldown, wPushButton;
/*---------------------------------------------------------------------*/

    if( ( strcasecmp( _currentPanel2->haz, "C&V" ) == 0 ) ) {

       /* 
        *  Check if CIG and/or VIS are selected for C&V 
        */

       isVis = False;
       isCig = False;

       for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

           XtVaGetValues ( _currentPanel2->descStrc[ ii ].label,
                           XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &labelStr );

           XmStringFree( xmStr );
           XtVaGetValues ( _currentPanel2->descStrc[ ii ].menu,
                        XmNsubMenuId, &wPulldown, NULL );
           XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

           XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &optionValue );


           if ( strcasecmp( labelStr, "CIG:" ) == 0 ) {

              if ( strlen( optionValue ) > 0 &&
                   strcasecmp( optionValue, "N/A" )  != 0 &&
                   strcasecmp( optionValue, "NONE" ) != 0 ) {

                 isCig = True;

              }

           }

           else if ( strcasecmp( labelStr, "VIS:" ) == 0 ) {

              if ( strlen( optionValue ) > 0 &&
                   strcasecmp( optionValue, "N/A" )  != 0 &&
                   strcasecmp( optionValue, "NONE" ) != 0 ) {

                 isVis = True;

              }

           }

           XtFree ( optionValue );
           XtFree ( labelStr );
           XmStringFree( xmStr );

       }

       /* 
        * Check if VIS has weather types.
	*/
       wxType = False;

       if ( isVis ) {
	 
           for ( jj = 0; jj < _currentPanel2->popup[ 0 ].nBtns; jj++ ) {

               if ( XmToggleButtonGetState ( _currentPanel2->popup[ 0 ].btns[ jj ] ) ) {

                   wxType = True;
                   break;

               }
           }
       }

       /*
        * Return false if C&V does not have VIS or CIG.
	* Or VIS does not have weather types.
	*/
       if ( ( !isCig && !isVis ) ||
            ( isVis && !wxType ) ) {

            return False;

       }
       else {

            return True;

       }

    }

    return True;

}

/*=====================================================================*/

static Boolean pggfawp_isIFR ( void )
/************************************************************************
 * pggfawp_isIFR	                                                *
 *                                                                      *
 * This routine checks if a C&V hazard is an IFR or an MVFR.  	 	*
 * If CIG BLW 010 or VIS BLW 3SM, the the C&V is an IFR. Otherwise, it 	*
 * is an MVFR.
 *                                                                      *
 * static Boolean pggfawp_isIFR ( void )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:	                                                *
 *                      True	if C&V is an IFR 		 	*
 *                      False	if C&V is not an IFR	   		*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           06/08   Created                                *
 * B. Yin/SAIC           08/08   Return false if no current second panel*
 ***********************************************************************/
{
    int         ii;
    char        cigType[ 64 ], visType[ 64 ], *labelStr, *optionValue;
    Boolean     rev;

    XmString    xmStr;
    Widget      wPulldown, wPushButton;
/*---------------------------------------------------------------------*/

    rev = False;

    if ( !_currentPanel2 ) return rev;

    if( ( strcasecmp( _currentPanel2->haz, "C&V" ) == 0 ) ) {

       /* 
        *   Get CIG and VIS selections.
        */
       cigType[ 0 ] = '\0';
       visType[ 0 ] = '\0';

       for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

           XtVaGetValues ( _currentPanel2->descStrc[ ii ].label,
                           XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &labelStr );

           XmStringFree( xmStr );

           XtVaGetValues ( _currentPanel2->descStrc[ ii ].menu,
                        XmNsubMenuId, &wPulldown, NULL );
           XtVaGetValues ( wPulldown, XmNmenuHistory, &wPushButton, NULL );

           XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &optionValue );


           if ( strcasecmp( labelStr, "CIG:" ) == 0 ) {

              if ( optionValue && ( strlen( optionValue ) > 0 ) ) {

                 strcpy( cigType, optionValue );

              }

           }
           else if ( strcasecmp( labelStr, "VIS:" ) == 0 ) {

              if ( optionValue && ( strlen( optionValue ) > 0 )) {

                 strcpy( visType, optionValue );

              }

           }

           XtFree ( optionValue );
           XtFree ( labelStr );
           XmStringFree( xmStr );
       }

       /*
        *  If CIG BLW 010 or VIS BLW 3SM, return true.
	*/
       if ( strcasecmp( cigType, "BLW_010" ) == 0 ||
            strcasecmp( visType, "BLW_3SM" ) == 0 ) {

                rev = True;

       }
    }

    return rev;

}

/*=====================================================================*/

static Boolean pggfawp_is20KtSfcWind ( void )
/************************************************************************
 * pggfawp_is20KtSfcWind                                                *
 *                                                                      *
 * This routine checks if a C&V hazard is an IFR or an MVFR.  	 	*
 * If CIG BLW 010 or VIS BLW 3SM, the the C&V is an IFR. Otherwise, it 	*
 * is an MVFR.
 *                                                                      *
 * static Boolean pggfawp_is20KtSfcWind ( void )                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:	                                                *
 *                      True	if the speed of SFC_WND is 20KT  	*
 *                      False	if the speed of SFC_WND is not 20KT	*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC           07/08   Created                                *
 * B. Yin/SAIC           08/08   Return false if no current second panel*
 ***********************************************************************/
{
    int		ii;
    char	*labelStr, *windSpeed;

    Boolean 	rv;
    XmString    xmStr;
    Widget      wPulldown, wPushButton;
/*---------------------------------------------------------------------*/

    rv = False;

    if ( !_currentPanel2 ) return rv;

    if( ( strcasecmp( _currentPanel2->haz, "SFC_WND" ) == 0 ) ) {

       for ( ii = 0; ii < _currentPanel2->nDesc; ii++ ) {

           XtVaGetValues ( _currentPanel2->descStrc[ ii ].label,
                        XmNlabelString, &xmStr, NULL );
           XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &labelStr );
           XmStringFree( xmStr );

           if ( strcasecmp( labelStr, "Speed:" ) == 0 ) {

              XtVaGetValues ( _currentPanel2->descStrc[ ii ].menu,
                              XmNsubMenuId, &wPulldown, NULL );
              XtVaGetValues ( wPulldown, XmNmenuHistory,
                              &wPushButton, NULL );

              XtVaGetValues ( wPushButton, XmNlabelString, &xmStr, NULL );
              XmStringGetLtoR ( xmStr, XmFONTLIST_DEFAULT_TAG, &windSpeed );

              if ( strcasecmp( windSpeed, "20KT" ) == 0 ) {

                 rv = True;

	      }

              XmStringFree ( xmStr );
              XtFree ( labelStr );
              XtFree ( windSpeed );

              if ( rv ) break;

	   }
	   else {

              XtFree ( labelStr );

	   }
       }
    }

    return rv;
}

/*=====================================================================*/

static Boolean pggfawp_addNCDesc ( char *haz, char *desc )
/************************************************************************
 * pggfawp_addNCDesc                                                 	*
 *                                                                      *
 * This routine checks if a hazard editor needs a Not change desciptor  *
 * for multi-selected GFA element				  	* 
 *                                                                      *
 * static Boolean pggfawp_addNCDesc ( haz, desc )	                *
 *                                                                      *
 * Input parameters:							*
 *	*haz		char	hazard string                           *
 *	*desc		char	descriptor string			*
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:                                                        *
 *                      True    add a blank descriptor	        	*
 *                      False   don't need to add		     	*
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           02/10   Created                                 *
 ***********************************************************************/
{
    Boolean     rv;
/*---------------------------------------------------------------------*/

    rv = False;

    if ( strcasecmp ( haz, "FZLVL" ) == 0 && 
	( strcasecmp ( desc, "Contour" ) == 0 || strcasecmp ( desc, "Level" ) == 0 )) {
	rv = True;
    }
    else if ( strcasecmp ( haz, "CLD" ) == 0 && strcasecmp ( desc, "Coverage" ) == 0 ) {
	rv = True;
    }
    else if ( strcasecmp ( haz, "ICE" ) == 0 && strcasecmp ( desc, "Type" ) == 0 ) {
        rv = True;
    }
    else if ( strcasecmp ( haz, "TS" ) == 0 &&  
	      (strcasecmp ( desc, "Category" ) == 0 || strcasecmp ( desc, "Frequency" ) == 0 ) ) {
        rv = True;
    }
    else if ( strcasecmp ( haz, "SFC_WND" ) == 0 && strcasecmp ( desc, "Speed" ) == 0 ) {
        rv = True;
    }
    else if ( (strcasecmp (haz, "TURB") == 0 ||
             strcasecmp (haz, "TURB-HI") == 0 ||
             strcasecmp (haz, "TURB-LO") == 0) && 
	     strcasecmp ( desc, "DUE_TO" ) == 0 ) {
	rv = True;
    }
 
    return rv;                                             
}

/*=====================================================================*/

static void pggfawp_chkNCDesc4MSel ( char *haz, int *which )
/************************************************************************
 * pggfawp_chkNCDesc4MSel                                       	*
 *                                                                      *
 * This routine checks a NotChange desciptor for GFA editor		* 
 *                                                                      *
 * static void pggfawp_chkBlankDesc4Msel ( haz, desc )                  *
 *                                                                      *
 * Input parameters:							*
 *	*haz		char	hazard string                           *
 *	*which		int	which descriptor                        *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:                                                        *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS           02/10   Created                                 *
 ***********************************************************************/
{
    int 	ii, ier, pdIndex=0; 
    int         nLabel, nChoices;
    char	typeStr[ 32 ], choice[ STD_STRLEN ];
    char 	desc1[ MAXTBLNAME ];

    XmString	xmStr;
    int		iopr;
/*---------------------------------------------------------------------*/

    ctb_gfagndesc ( haz, &nLabel, &ier );

    for ( ii = 0; ii < nLabel; ii++ ) {
                                                                                              
        ctb_gfagdesc ( haz, &ii, typeStr, desc1, &nChoices, &ier );
        if ( strcasecmp ( typeStr, "PULLDOWN" ) == 0 ) {
            if ( pggfawp_addNCDesc ( haz, desc1 ) ) {
	        iopr = pgpalw_getCurOperId();
	        if ( iopr == FUNC_MULTISEL) {
		    if ( !XtIsManaged ( _panel2[ *which ].descStrc[ pdIndex ].pb[ nChoices ] )) { 
		        strcpy (choice, "N/C");
                        xmStr = XmStringCreateLocalized ( choice );

                        XtVaSetValues ( _panel2[ *which ].descStrc[ pdIndex ].pb[ nChoices ],
                              		XmNlabelString,   xmStr,
                              		XmNrecomputeSize,       True,
                              		NULL );

                        XmStringFree ( xmStr );
		        XtManageChild ( _panel2[ *which ].descStrc[ pdIndex ].pb[ nChoices ] );
		    }
	        }
	        else {
		    if ( XtIsManaged ( _panel2[ *which ].descStrc[ pdIndex ].pb[ nChoices ] )) { 
		        XtUnmanageChild ( _panel2[ *which ].descStrc[ pdIndex ].pb[ nChoices ] );
		    }
	        }
	    }
	    pdIndex ++ ;
	}
    }
}
