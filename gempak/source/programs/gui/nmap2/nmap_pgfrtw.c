#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"

#define MAX_STRNGTH_FRT	 9
#define MAX_SIZE_FRT     50
#define MIN_SIZE_FRT     40
#define MAX_SMTH_LVLS	 3
#define IMP_CHOICE       -99


static Widget	_front_colrW[2];
static Widget	_fronts_dlgW;
static Widget   _frt_strngth_sldW;
static Widget	_frt_strngth_txtW;
static Widget	_frt_size_sldW;
static Widget	_frt_size_txtW;
static Widget	_frt_smth_optW;
static Widget	_frt_smth_pbW[MAX_SMTH_LVLS];

static Widget   _labelReminder_formW;
static Widget   _labelReminder_textW;

static Widget	_label_formW;
static Widget	_label_menuW, _label_menuW2;
static Widget	_label_toggW, _label_toggW2;
static Widget	_label_submenuW;
static Widget	_label_optW;
static Widget	_label_pbW[GRP_MXELE];

static Widget	_group_typeW;
static WidgetList _group_buttonW;

static Widget	_ctlForm;
static WidgetList _ctlBtns;

static int	_frtMajColr;
static int	_frtMinColr;

static int  	_frtType;
static int	_numGrp   = 0;
static int      _groupTyp = IMP_CHOICE;
static int      _curGrpIdx= IMP_CHOICE;
static int      _objId    = IMP_CHOICE; /* A local copy of cur. objId */

static char	_frtSmooth;
static char	_labelFlag, _labelColorFlag;
static char	_labelName[10];


/*
 *  private callback functions
 */
void pgfrtw_grpTypCb    ( Widget, long, XtPointer );
void pgfrtw_labelPbCb   ( Widget, long, XtPointer ); 
void pgfrtw_labelToggCb ( Widget, XtPointer, XtPointer );
void pgfrtw_majcolorCb  ( Widget, XtPointer, XtPointer );
void pgfrtw_mincolorCb  ( Widget, XtPointer, XtPointer );
void pgfrtw_smthPbCb    ( Widget, XtPointer, XtPointer );
void pgfrtw_sizeCb 	( Widget, XtPointer, XmScaleCallbackStruct* );
void pgfrtw_strengthCb  ( Widget, XtPointer, XmScaleCallbackStruct* );
void pgfrtw_sizeTxtCb   ( Widget, XtPointer, XmTextVerifyCallbackStruct* );
void pgfrtw_strengthTxtCb(Widget, XtPointer, XmTextVerifyCallbackStruct* );

/*
 *  private functions
 */
void pgfrtw_setLabValue ( int which );
void pgfrtw_setTable ( int strength, int size, int smooth,
				int majcolr, int mincolr );
void pgfrtw_initType ( VG_DBStruct *el );


/************************************************************************
 * nmap_pgfrtw.c							*
 *									*
 * This module creates and displays the VG fronts setting box. It also	*
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *  pgfrtw_create()	creates front attribute window			*
 *  pgfrtw_popup()	popup front attribute window			*
 *  pgfrtw_popdown()	pop down the front attribute window		*
 *  pgfrtw_setAttr()	set the front attributes			*
 *  pgfrtw_setmajColor()set the front major color			*
 *  pgfrtw_setminColor()set the front minor color			*
 *  pgfrtw_setLabFlag() set the label flag				*
 *  pgfrtw_setLabItems()set label item strings                          *
 *									*
 *  pgfrtw_getAttr()	get the front attributes			*
 *  pgfrtw_isUp()	querry whether the window is up			*
 *  pgfrtw_parseFcode()	decode fcode value for a front			*
 *  pgfrtw_getLabValue  get the label value for text editor		*
 *  pgfrtw_getFrtColor  get the current front color                     *
 *  pgfrtw_getLabFlag() get the label flag				*
 *  pgfrtw_getLabColorFlag()  get the label color flag                  *
 *  pgfrtw_getGrptyp()	get the current group type			*
 *									*
 *  pgfrtw_strengthCb()	Callback for front strength scale		*
 *  pgfrtw_strengthTxtCb()	Callback for front strength text widget	*
 *  pgfrtw_sizeCb()	Callback for front size scale			*
 *  pgfrtw_sizeTxtCb()	Callback for front size text widget		*
 *  pgfrtw_smthPbCb()	Callback for front smoothing level		*
 *  pgfrtw_majcolorCb()	Callback for front major color palette		*
 *  pgfrtw_mincolorCb()	Callback for front minor color palette		*
 *  pgfrtw_labelToggCb  callback for label toggle button		*
 *  pgfrtw_labelPbCb()	callback for label type 			*
 *  pgfrtw_grpTypCb()   callback for group type option menu             *
 *									*
 *  pgfrtw_setLabValue  places a selected label name into a text tring	*
 *  pgfrtw_setTable()	places element into set table			*
 *  pgfrtw_initType()   initialize the group type on attrib. window     *
 *  pgfrtw_updtGrpMenu()update the group menu				*
 ***********************************************************************/

/*=====================================================================*/

void pgfrtw_create ( Widget parent )
/************************************************************************
 * pgfrtw_create							*
 *									*
 * This function creates a Fronts attribute selection box.		*
 *									*
 * void pgfrtw_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	07/97						*
 * C. Lin/EAI		10/97	rename from NxmFrontsCr, cleanup	*
 * E. Safford/GSC	11/97	added control buttons for edit		*
 * A. Hardy/GSC		02/98	fixed scale bars, they go left to right	*
 * S. Law/GSC		04/98	added smooth level option menu		*
 * F. J. Yen/NCEP	05/98	cleaned up; reorganized; removed stroke	*
 * W. Li/EAI		06/98	Changed text size (1--10)-->(0.1--5.0)	*
 * W. Li/EAI		07/98	Added colors for front			*
 * W. Li/EAI		03/99   Added two more label types		*
 * W. Li/EAI		05/99	added group type in symbol editor	*
 * H. Zeng/EAI          02/00   did minor changes to the appearance     *
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * H. Zeng/EAI          06/00   added label color toggle                *
 * H. Zeng/EAI          07/00   added label reminder text widget        *
 * E. Safford/GSC	12/00	fix comp warnings, rename grp defines	*
 * H. Zeng/EAI          04/02   removed pggrpw_createTypeBox()          *
 * J. Wu/SAIC           05/02   verify input to front strength & size	*
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 ***********************************************************************/
{
    Widget	size_form, strngth_form, pane, bb, colrlbl;
    Widget	smth_form, smth_menubar, smth_label, pulldown;
    Widget	label_form, label_menubar;
    Widget	type_menu, group_form, labelReminder_label;
    XmString	xmstr;
    Arg	   	args[10];
    int		grptyp, iret;
    long	ii, nn;
    char	cc[10], grpnam[64], *btnstr[] = {"Apply", "Cancel"};
    char        *names = NULL;

/*---------------------------------------------------------------------*/

    /*
     * Create the VG fronts selection box.
     */
    _fronts_dlgW = XmCreateFormDialog ( parent, "fronts_edit",
				       NULL, 0 );
    xmstr = XmStringCreateLocalized("Front Attributes");

    XtVaSetValues(_fronts_dlgW,
		 XmNnoResize,			TRUE,
		 XmNautoUnmanage,		FALSE,
    		 XmNdialogTitle, 		xmstr,
		 NULL);

    XmStringFree(xmstr);

    pane  = XtVaCreateManagedWidget ("pane",
		xmPanedWindowWidgetClass,	_fronts_dlgW,
                XmNmarginHeight,                0,
                XmNmarginWidth,                 0,
                XmNspacing,                     3,
		XmNsashWidth,			1,
		XmNsashHeight,			1,
		NULL); 
    
    /*
     * create color widget
     */

    bb = XtVaCreateWidget( "pg_front_bb",
		xmBulletinBoardWidgetClass,	pane,
		NULL);
    colrlbl = XtVaCreateManagedWidget ("Color:",
		xmLabelGadgetClass,        	bb,
		XmNx,                      	10,
		XmNy,                      	15,
                NULL);
    XtManageChild (colrlbl);

    _front_colrW[0] = XtVaCreateManagedWidget(" ",
	 	xmPushButtonWidgetClass,	bb,
	 	XmNwidth,			25,
	 	XmNheight,			20,
	 	XmNx,				130,
	 	XmNy,				15,
	 	NULL);

    XtAddCallback(_front_colrW[0], XmNactivateCallback, 
    		(XtCallbackProc)pgfrtw_majcolorCb, NULL);

    _front_colrW[1] = XtVaCreateManagedWidget(" ",
	 	xmPushButtonWidgetClass,	bb,
	 	XmNwidth,			25,
	 	XmNheight,			20,
	 	XmNx,				155,
	 	XmNy,				15,
	 	NULL);

    XtAddCallback(_front_colrW[1], XmNactivateCallback, 
    		(XtCallbackProc)pgfrtw_mincolorCb, NULL);


    XtManageChild(bb);

    /*
     * Create strength input
     */

    strngth_form = (Widget)XtVaCreateManagedWidget ("_frt_strngth_formW",
	 	xmFormWidgetClass,		pane,
	 	NULL);

    _frt_strngth_txtW   = (Widget) XtVaCreateManagedWidget ("frt_strngth",
	 	xmTextFieldWidgetClass, 	strngth_form,
	 	XmNcolumns,	   	  	4,
		XmNvalue,	    	   	"1",
	 	XmNcursorPositionVisible,   	True,
	 	XmNrightAttachment,	    	XmATTACH_FORM,
	 	NULL);

    XtAddCallback(_frt_strngth_txtW, XmNmodifyVerifyCallback, 
                 (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);    
    XtAddCallback(_frt_strngth_txtW, XmNvalueChangedCallback, 
	  	(XtCallbackProc)pgfrtw_strengthTxtCb, NULL);

    _frt_strngth_sldW=(Widget)XmCreateScale(strngth_form, 
					    "strngth", NULL, 0);
    XtManageChild( _frt_strngth_sldW);
    xmstr = XmStringCreateLocalized("Strength");
    XtVaSetValues(_frt_strngth_sldW,
		XmNorientation,			XmHORIZONTAL,
		XmNmaximum,			MAX_STRNGTH_FRT,
		XmNminimum,			1,
		XmNvalue,			1,
		XmNprocessingDirection,		XmMAX_ON_RIGHT,
		XmNscaleMultiple,		1,
		XmNshowValue,			False,
		XmNtitleString,			xmstr,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_WIDGET,
		XmNrightWidget,			_frt_strngth_txtW,
		NULL);
    XmStringFree(xmstr);

    XtAddCallback(_frt_strngth_sldW, XmNvalueChangedCallback, 
		(XtCallbackProc)pgfrtw_strengthCb, NULL);
    XtAddCallback(_frt_strngth_sldW, XmNdragCallback,
		(XtCallbackProc)pgfrtw_strengthCb, NULL);
 

    /*
     * Create size input
     */

    size_form = (Widget)XtVaCreateManagedWidget ("_frt_size_formW",
	 	xmFormWidgetClass,		pane,
	 	NULL);

    _frt_size_txtW = (Widget) XtVaCreateManagedWidget ("frt_size",
	 	xmTextFieldWidgetClass,		size_form,
	 	XmNcolumns,	            	4,
	 	XmNvalue,		    	"1",
	 	XmNcursorPositionVisible,   	True,
	 	XmNrightAttachment,	    	XmATTACH_FORM,
	 	NULL);

    XtAddCallback(_frt_size_txtW, XmNmodifyVerifyCallback, 
                  (XtCallbackProc)pgutls_vrfyPosFltCb, NULL);    
    XtAddCallback(_frt_size_txtW, XmNvalueChangedCallback, 
		(XtCallbackProc)pgfrtw_sizeTxtCb, NULL);

    _frt_size_sldW = (Widget)XmCreateScale(size_form, "size", NULL, 0);
    XtManageChild( _frt_size_sldW);
    xmstr = XmStringCreateLocalized("Pip size");
    XtVaSetValues(_frt_size_sldW,
		XmNorientation,			XmHORIZONTAL,
		XmNmaximum,			MAX_SIZE_FRT * 10,
		XmNminimum,			MIN_SIZE_FRT,
		XmNvalue,	        	50,
		XmNprocessingDirection,		XmMAX_ON_RIGHT,
		XmNscaleMultiple,		10,
		XmNshowValue,			False,
		XmNtitleString,			xmstr,
		XmNtopAttachment,		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_WIDGET,
		XmNrightWidget,			_frt_size_txtW,
		NULL);
    XmStringFree(xmstr);

    XtAddCallback(_frt_size_sldW, XmNvalueChangedCallback, 
		(XtCallbackProc)pgfrtw_sizeCb, NULL);
    XtAddCallback(_frt_size_sldW, XmNdragCallback, 
		(XtCallbackProc)pgfrtw_sizeCb, NULL);

    /*
     * Create smooth input
     */

    smth_form = (Widget)XtVaCreateManagedWidget ("_frt_smth_formW",
		xmFormWidgetClass,		pane,
	 	NULL);
 
    smth_label  = XtVaCreateManagedWidget ("Smoothing Level:",
		xmLabelGadgetClass,		smth_form,
		XmNleftAttachment,		XmATTACH_FORM,
		NULL); 

    smth_menubar  = XmCreatePulldownMenu (smth_form, "Smooth", NULL, 0);
    _frt_smth_optW = XmCreateOptionMenu (smth_form, "smth", NULL, 0);

    for (ii=0; ii < MAX_SMTH_LVLS; ii++) {
			    sprintf (cc, "%ld", ii);
        xmstr = XmStringCreateLocalized (cc);
        _frt_smth_pbW[ii] = XtVaCreateManagedWidget (cc,
		 xmPushButtonWidgetClass,	smth_menubar,
		 XmNlabelString,		xmstr,
		 NULL);
        XmStringFree (xmstr);
        XtAddCallback(_frt_smth_pbW[ii], XmNactivateCallback,
		 (XtCallbackProc)pgfrtw_smthPbCb, (XtPointer) ii);
    }

    xmstr = XmStringCreateLocalized ("");
    XtVaSetValues (_frt_smth_optW, 
		XmNlabelString,			xmstr,	
		XmNsubMenuId,			smth_menubar,
		XmNmenuHistory,			_frt_smth_pbW[0], 
		XmNrightAttachment,		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,			smth_label,
		NULL);
    XmStringFree (xmstr);

    XtManageChild (_frt_smth_optW);


    /*
     * Create label reminder
     */
    _labelReminder_formW = 
             (Widget)XtVaCreateWidget ("_label_reminder_formW",
		     xmFormWidgetClass,		pane,
	 	     NULL);

    labelReminder_label  = 
             (Widget)XtVaCreateManagedWidget ("Label:  ",
		     xmLabelGadgetClass,     _labelReminder_formW,
                     XmNtopAttachment,       XmATTACH_FORM,
                     XmNtopOffset,           10,
		     XmNleftAttachment,	     XmATTACH_FORM,
		     NULL);

    nn = 0;
    XtSetArg(args[nn], XmNrows,                         1); nn++;
    XtSetArg(args[nn], XmNcolumns,                     13); nn++;
    XtSetArg(args[nn], XmNcursorPositionVisible,    False); nn++;
    XtSetArg(args[nn], XmNresizeHeight,              True); nn++;
    XtSetArg(args[nn], XmNeditable,                 False); nn++;
    XtSetArg(args[nn], XmNeditMode,     XmMULTI_LINE_EDIT); nn++; 

    _labelReminder_textW =  
             (Widget)XmCreateScrolledText(_labelReminder_formW, 
                                       "Label", args, nn);

    XtVaSetValues( XtParent(_labelReminder_textW),
		XmNleftAttachment,	 XmATTACH_WIDGET,
		XmNleftWidget,           labelReminder_label,
		NULL);

    XtManageChild(_labelReminder_textW);

    /*
     * create label widget input
     */
    _label_formW = XtVaCreateManagedWidget ("form", 
		xmFormWidgetClass, 	   	pane, 
		NULL);

    type_menu  = XtVaCreateManagedWidget ("type_menu",
	 xmRowColumnWidgetClass,	_label_formW,
	 XmNorientation,		XmVERTICAL,
	 XmNpacking,			XmPACK_TIGHT,
	 XmNnumColumns,			1,
	 NULL);

    label_form = (Widget)XtVaCreateManagedWidget("_label_formW",
	 xmFormWidgetClass,		type_menu,
	 NULL);

    _label_menuW = (Widget)XtVaCreateManagedWidget ("_lin_label_menu",
	 xmRowColumnWidgetClass,	label_form,
	 XmNtopAttachment,	        XmATTACH_FORM,
	 XmNpacking,			XmPACK_TIGHT,
	 XmNorientation, 		XmHORIZONTAL,
	 NULL);

    _label_toggW = XtVaCreateManagedWidget(" ",
	 xmToggleButtonGadgetClass,	_label_menuW,
         XmNtraversalOn,                FALSE,
	 NULL);

    XtAddCallback(_label_toggW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgfrtw_labelToggCb, (XtPointer)0 );

    XtVaCreateManagedWidget ("Label:",
	 xmLabelGadgetClass,		_label_menuW,
	 NULL); 

    _label_submenuW = XtVaCreateManagedWidget("_lin_lab_submenu",
	 xmRowColumnWidgetClass,	_label_menuW,
	 NULL);

    label_menubar = XmCreatePulldownMenu (_label_submenuW, 
					  "Label", NULL, 0);

    _label_optW = XmCreateOptionMenu (_label_submenuW, 
					  "label", NULL, 0);

    for (ii=0; ii < GRP_MXELE; ii++) {
	sprintf (cc, "xxxx" );
        xmstr = XmStringCreateLocalized (cc);
        _label_pbW[ii] = XtVaCreateManagedWidget(cc,
	     xmPushButtonWidgetClass,	    label_menubar,
	     NULL);
	XtVaSetValues(_label_pbW[ii],
	    XmNlabelString, 		xmstr,
	    XmNalignment,		XmALIGNMENT_CENTER,
	    NULL);
        XmStringFree (xmstr);
        XtAddCallback(_label_pbW[ii], XmNactivateCallback,
		      (XtCallbackProc)pgfrtw_labelPbCb, (XtPointer) ii);
    }

    XtVaSetValues (_label_optW, 
	XmNsubMenuId,			label_menubar,
	XmNmenuHistory,			_label_pbW[0], 
	NULL);

    XtManageChild (_label_optW);

    /*
     * create use FRONT color toggle
     */
    _label_menuW2 = (Widget)XtVaCreateManagedWidget ("_label_color_menu",
	 xmRowColumnWidgetClass,	label_form,
	 XmNtopAttachment,	        XmATTACH_WIDGET,
         XmNtopWidget,                  _label_menuW,
	 XmNpacking,			XmPACK_TIGHT,
	 XmNorientation, 		XmHORIZONTAL,
	 NULL);

    _label_toggW2 = XtVaCreateManagedWidget(" ",
	 xmToggleButtonGadgetClass,	_label_menuW2,
         XmNtraversalOn,                FALSE,
	 NULL);

    XtAddCallback(_label_toggW2, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgfrtw_labelToggCb, (XtPointer)1 );

    XtVaCreateManagedWidget ("use FRONT color",
	        xmLabelGadgetClass,		_label_menuW2,
	        NULL); 


    XtManageChild (_label_formW);

    /*
     * Create group type buttons
     */
    group_form = (Widget)XtVaCreateManagedWidget("_group_formW",
	 xmFormWidgetClass,		type_menu,
	 NULL);

    /*
     * Build group type option menu.
     */
    ces_gtggrps(&_numGrp, &names, &iret);   
    if ( names != NULL )  free (names);

    pulldown = XmCreatePulldownMenu(group_form, "menuW", NULL, 0);
    _group_typeW = XmCreateOptionMenu(group_form, "option_menu", NULL, 0 ); 
    _group_buttonW = (WidgetList)XtMalloc(_numGrp * sizeof(Widget));

    for( ii = 0; ii < _numGrp; ii++ ) { 
        grptyp = ces_gtgmsid ( ii );
        ces_gtgnam (grptyp, grpnam, &iret);
        _group_buttonW[ii] = XtVaCreateManagedWidget(grpnam,
             xmPushButtonWidgetClass,		pulldown,
             NULL);

	XtAddCallback (_group_buttonW[ii], XmNactivateCallback, 
		       (XtCallbackProc)pgfrtw_grpTypCb, (XtPointer)ii);

    }

    xmstr = XmStringCreateLocalized("Group Type");
    XtVaSetValues(_group_typeW,
	XmNsubMenuId, 		pulldown,
	XmNlabelString, 	xmstr, 
	NULL );
    XmStringFree(xmstr);

    XtManageChild(_group_typeW);


    /*
     * Create control buttons
     */

    _ctlForm  = (Widget)XtVaCreateManagedWidget ("_frt_ctl_formW",
		xmFormWidgetClass,	 	pane,
	 	NULL);

    _ctlBtns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));
    NxmCtlBtn_create (_ctlForm, 1, "ctlBtns", XtNumber(btnstr), btnstr,
		      NULL, _ctlBtns);


    /* 
     * Initialize member variables 
     */
    _frtSmooth	= 0;
    _labelFlag  = 0; 
    _labelColorFlag = 0;

    return;

}

/*=====================================================================*/

void pgfrtw_popup ( int frt_typ, int show_strngth, int show_size, int show_ctl,
		    XtCallbackProc callback, char *label_ptr, Boolean init_grp )
/************************************************************************
 * pgfrtw_popup								*
 *									*
 * This function shows a VG fronts drawing attribute box.		*
 *									*
 * void pgfrtw_popup (frt_typ, show_strngth, show_size,			*
 * 			show_ctl, callback, label_ptr, init_grp )	*
 *									*
 * Input parameters:							*
 *    frt_typ	        int	front type ID or the fcode value from   *
 *				  the vgf file				*
 *    show_strngth	int	flag whether to show strength		*
 *    show_size		int	flag whether to show size		*
 *    show_ctl 		int	flag whether to show control buttons 	*
 *    callback		XtCallbackProc Callback for control buttons	*
 *    label_ptr         char*   pointer to the possible label string    *
 *    init_grp          Boolean re-initialize the group menu            *
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * E. Wehner/EAi	 8/97	retrieve settings from CES		*
 * C. Lin/EAI	        10/97	rename from NxmFrontsSh, cleanup	*
 * E. Safford/GSC	11/97	add show_ctl flag & callback function   *
 * F. J. Yen/NCEP	 4/98	updated with new ces function names	*
 * F. J. Yen/NCEP	 5/98	remove show_strk flag			*
 * S. Law/GSC		05/98	added scale updates			*
 * W. Li/EAI		06/98	Changed text size (1--10)-->(0.1--5.0)	*
 * E. Safford/GSC	06/98	added tableset to strength		*
 * W. Li/EAI		07/98	added colors for front			*
 * C. Lin/EAI		09/98	add smoothing level			*
 * W. Li/EAI		02/99	add label and label type selection	*
 * W. Li/EAI		05/99	Initialized label value using obj_type	*
 * W. Li/EAI		05/99	added group type in symbol editor	*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * H. Zeng/EAI          06/00   added label color toggle initialization *
 * H. Zeng/EAI          07/00   added a new para. label_ptr             *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * H. Zeng/EAI          02/01   added call to pggrpw_initType()         *
 * H. Zeng/EAI          03/01   modified to check group type id         *
 * E. Safford/GSC	03/01	add init_grp param, mod edit setup      *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * E. Safford/SAIC	02/02	rm pgfrt_setTable			*
 * M. Li/SAIC		04/02 	Added pglabel_setLabelPending		*
 * M. Li/SAIC		05/02 	Check for the active Grouping		*
 * J. Wu/SAIC		05/02	wipe "size" text B4 assigning a new one	*
 * E. Safford/SAIC	06/03	wipe width text before assignment	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		level, ier, ier1, fstr, fcode, group_type; 
    long	ii;
    float  	fpipsz_f;
    char	grp[4], txtstr[5], logstr[10];
/*---------------------------------------------------------------------*/

    if ( XtIsManaged ( _fronts_dlgW ) )
	XtUnmanageChild ( _fronts_dlgW );

    XtSetSensitive(_frt_strngth_sldW, show_strngth);
    XtSetSensitive(_frt_strngth_txtW, show_strngth);
    XtSetSensitive(_frt_size_sldW, show_size);
    XtSetSensitive(_frt_size_txtW, show_size);

    pglabel_setLabelPending (False);

    if (show_ctl) {
        XtManageChild (_ctlForm);
	XtUnmanageChild (_label_formW);
        if (callback) {
	    for (ii=0; ii < 2; ii++) {
                XtRemoveAllCallbacks (_ctlBtns[ii], XmNactivateCallback);
                XtAddCallback (_ctlBtns[ii], XmNactivateCallback,
                              (XtCallbackProc)callback, (XtPointer)ii);
	    }
        }
    }
    else {
        XtUnmanageChild (_ctlForm);
  	XtManageChild (_label_formW);
    }

    pgfrtw_parseFcode (frt_typ, &_frtType, &fstr, &fcode);  

    el.hdr.vg_type = FRONT_ELM;
    el.hdr.vg_class = CLASS_FRONTS;
    ces_get(_frtType, &el, &ier);

    if (ier  != 0) {
        sprintf(logstr, "%i ", frt_typ);
        strcpy(grp, "CES");
        level = 2;
        er_lmsg ( &level, grp, &ier, logstr, &ier1, strlen(grp), strlen(logstr) );
   	NxmErr_update();
        return;
    }

    sprintf (txtstr, "%i", el.elem.frt.info.fwidth);
    XmTextFieldSetString (_frt_strngth_txtW, "");
    XmTextFieldSetString (_frt_strngth_txtW, txtstr);
    XmScaleSetValue (_frt_strngth_sldW, el.elem.frt.info.fwidth);

    fpipsz_f = (float)(el.elem.frt.info.fpipsz)/100.0F;
    sprintf (txtstr, "%3.1f", fpipsz_f);
    XmTextFieldSetString (_frt_size_txtW, "");
    XmTextFieldSetString (_frt_size_txtW, txtstr);

    XmScaleSetValue (_frt_size_sldW, el.elem.frt.info.fpipsz);

    /*
     * set front color
     */

    _frtMajColr = el.hdr.maj_col;
    _frtMinColr = el.hdr.min_col;


    XtVaSetValues(_front_colrW[0],
        XmNbackground,		NxmColrP_getColorPixel(_frtMajColr),
        XmNtopShadowColor,	NxmColrP_getColorPixel(_frtMajColr),
        XmNbottomShadowColor,	NxmColrP_getColorPixel(_frtMajColr),
        NULL);

    if (frt_typ == 0) {
	XtManageChild(_front_colrW[1]);
	XtVaSetValues(_front_colrW[1],
	    XmNbackground,		NxmColrP_getColorPixel(_frtMinColr),
	    XmNtopShadowColor,		NxmColrP_getColorPixel(_frtMinColr),
	    XmNbottomShadowColor,	NxmColrP_getColorPixel(_frtMinColr),
	    NULL);
    }
    else 
	XtUnmanageChild(_front_colrW[1]);

    /*
     * set front smooth level
     */

    _frtSmooth = el.hdr.smooth;
    XtVaSetValues (_frt_smth_optW,
                XmNmenuHistory, _frt_smth_pbW[(int)_frtSmooth],
                NULL);


    /*
     * If there is a label string associated with the front,
     * show it on the attribute window.
     */
    if( label_ptr != NULL ) {
       XtManageChild(_labelReminder_formW);
       XmTextSetString (_labelReminder_textW, label_ptr);
    }
    else {
       if( XtIsManaged(_labelReminder_formW) ) {
          XtUnmanageChild(_labelReminder_formW);
       }        
    }

    XtManageChild ( _fronts_dlgW );

    /*
     * set group type and label items.
     */
    if( init_grp ) {
       pgfrtw_initType( &el );
       group_type = _groupTyp;
       pgfrtw_setLabItems(group_type);
    }

    XmToggleButtonGadgetSetState (_label_toggW2, _labelColorFlag, TRUE);
    XmToggleButtonGadgetSetState (_label_toggW, _labelFlag, TRUE);
    XtSetSensitive(_label_submenuW, _labelFlag);
    XtSetSensitive(_label_menuW2, _labelFlag);
    if ( pgpalw_isGrpActv() ) {
	XtSetSensitive(_group_typeW, False);
    }
    else {
        XtSetSensitive(_group_typeW, _labelFlag);
    }

}

/*=====================================================================*/

void pgfrtw_popdown ( void )
/************************************************************************
 * pgfrtw_popdown							*
 *									*
 * This function unmanages the fronts dialog box			*
 *									*
 * void pgfrtw_popdown ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *				NONE					*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * C. Lin/EAI	        10/97	rename from NxmFrontsUnmanage, cleanup	*
 * J. Wu/SAIC		12/03   pop down color pallette			*
 ***********************************************************************/
{
    if( XtIsManaged(_fronts_dlgW) ) {
        NxmClrW_popdown();    	
    	XtUnmanageChild(_fronts_dlgW);
    }
}

/*=====================================================================*/

void pgfrtw_setAttr ( int fcode, int pipsz, char csmth, 
					int maj_color, int min_color )
/************************************************************************
 * pgfrtw_setAttr							*
 *									*
 * This function sets the values in the fronts dialog box		*
 *									*
 * void pgfrtw_setAttr( fcode, pipsz, csmth, maj_color, min_color )	*
 *									*
 * Input parameters:							*
 *	fcode		int	fcode of front				*
 *	pipsz		int	pip size				*
 *	csmth		char	smoothing level				*
 *	maj_color	int	major color of front			*
 *	min_color	int	miner color of front			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	11/97	initial coding				*
 * S. Law/GSC		04/98	Added smoothing level			*
 * E. Safford/GSC	04/98	initialize _frtSmooth			*
 * F. J. Yen/NCEP	 5/98	removed pipst from arguement list	*
 * S. Law/GSC		05/98	added scale updates			*
 * E. Safford/GSC	06/98	added setTable for initial pipsz	*
 * W. Li/EAI		06/98	Changed text size (1--10)-->(0.1--5.0)	*
 * W. Li/EAI		07/98	Added colors for front			*
 * E. Safford/SAIC	06/02	wipe str/wdth strings before assignment	*
 ***********************************************************************/
{
    char		txtstr[5];
    int			str, f_type, f_code, ismth;
    float       	pipsz_f;
/*---------------------------------------------------------------------*/

    /*
     *  Front strength is stored in the 10s digit of the fcode
     *    obtain it by subtracting the 100s digit and dividing by 10
     */
    pgfrtw_parseFcode (fcode, &f_type, &str, &f_code);

    sprintf(txtstr, "%i", str);
    XmTextFieldSetString(_frt_strngth_txtW, "");
    XmTextFieldSetString(_frt_strngth_txtW, txtstr);

    XmScaleSetValue(_frt_strngth_sldW, str );
    pipsz_f = (float)pipsz/100.0F;
    sprintf(txtstr, "%3.1f", pipsz_f);

    XmTextFieldSetString(_frt_size_txtW, "");
    XmTextFieldSetString(_frt_size_txtW, txtstr);
    XmScaleSetValue(_frt_size_sldW, pipsz );

    /*
     * set color
     */
    _frtMajColr = maj_color; 
    _frtMinColr = min_color;

    XtVaSetValues(_front_colrW[0],
	XmNbackground,		NxmColrP_getColorPixel(_frtMajColr),
	XmNtopShadowColor,	NxmColrP_getColorPixel(_frtMajColr),
	XmNbottomShadowColor,	NxmColrP_getColorPixel(_frtMajColr),
	NULL);

    if (_frtType == 0) {
	XtManageChild(_front_colrW[1]);
    	XtVaSetValues(_front_colrW[1],
	    XmNbackground,		NxmColrP_getColorPixel(_frtMinColr),
	    XmNtopShadowColor,		NxmColrP_getColorPixel(_frtMinColr),
	    XmNbottomShadowColor,	NxmColrP_getColorPixel(_frtMinColr),
	    NULL);
    }
    else
	XtUnmanageChild(_front_colrW[1]);

    _frtSmooth = csmth;
    ismth = csmth;

    pgfrtw_setTable (str, pipsz, csmth, maj_color, min_color);

    XtVaSetValues (_frt_smth_optW, 
		XmNmenuHistory,	_frt_smth_pbW[ismth], 
		NULL);


}

/*=====================================================================*/

void pgfrtw_getAttr ( char *fsmth, int *fmajcolor, int *fmincolor )
/************************************************************************
 * pgfrtw_getAttr							*
 *									*
 * This function gets the values in the fronts dialog box		*
 *									*
 * void pgfrtw_getAttr (fsmth, fmajcolor, fmincolor)			*
 *									*
 * Input parameters:							*
 *	*fsmth		char	fcode of front				*
 *	*fmajcolor	int	major color of front			*
 *	*fmincolor	int	miner color of front			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	initial coding                        	*
 * W. Li/EAI		07/98	Added colors for front			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    *fsmth  = _frtSmooth;
    *fmajcolor = _frtMajColr;
    *fmincolor = _frtMinColr;
}

/*=====================================================================*/

Boolean pgfrtw_isUp ( void )
/************************************************************************
 * pgfrtw_isUp								*
 *									*
 * This function returns a boolean value specifying whether the fronts	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgfrtw_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgfrtw_isUp		Boolean	    True -- managed (up), False -- down	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * C. Lin/EAI	        10/97	rename from NxmFrontsManaged, cleanup	*
 ***********************************************************************/
{

    return (XtIsManaged(_fronts_dlgW) );

}

/*=====================================================================*/

void pgfrtw_parseFcode ( int fcode, int *f_type, int *strength, 
							int *front_code )
/************************************************************************
 * pgfrtw_parseFcode							*
 *									*
 * The fcode contains three values.  This function decodes those values *
 * into  the gem_type, the front strength, and the full front code.     *
 *									*
 * void pgfrtw_parseFcode(fcode, f_type, strength, front_code)		*
 *									*
 * Input parameters:							*
 *	fcode		int	fcode (from vgf record entry)		*
 *									*
 * Output parameters:							*
 *	*f_type		int	type of front (ie warm etc) * 100       *
 *	*strength	int	strength code for front	0-9		*
 *	*front_code	int	a value for the front combining the     *
 *				  gem type and the specific type of     *
 *				  front (ie cold + diffuse)		*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	12/97	initial coding                      	*
 * F. J. Yen/NCEP	 5/98	Corrected prologue			*
 ***********************************************************************/
{
    /*
     *  fcode is 3 digits.  f_type is hundreds digit * 100, stength is the 
     *  value of the fcode 10s digit, front_code is hundreds digit and ones
     *  digit.
     */
 
    *f_type     = (fcode/100) * 100;
    *strength   = (fcode - *f_type)/10;
    *front_code = fcode - (*strength * 10);

}

/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_strengthCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgfrtw_strengthCb							*
 *									*
 * This function is the callback function for front strength scale.	*
 *									*
 * void pgfrtw_strengthCb ( w, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *      w              Widget                widget ID                  *
 *      clnt    XtPointer       	     client data                *
 *      *cbs	       XmScaleCallbackStruct callback struct            *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * C. Lin/EAI	        10/97	rename from NxmFrontsStrngthCb, cleanup	*
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 ***********************************************************************/
{
    int		frtstrength;	/* front strength */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    frtstrength = (int) cbs->value;
    sprintf(txtstr, "%i", frtstrength);
    XmTextFieldSetString(_frt_strngth_txtW, txtstr);
    if (cbs->reason == XmCR_VALUE_CHANGED)
	pgfrtw_setTable (frtstrength, -1, -1, -1, -1);
}

/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_sizeCb ( Widget w, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgfrtw_sizeCb							*
 *									*
 * This function is the callback for front size	scale			*
 *									*
 * void pgfrtw_sizeCb ( w, clnt, cbs )				*
 *									*
 * Input parameters:							*
 *      w               Widget                 widget ID                *
 *      clnt     XtPointer                    client data              *
 *      *cbs	        XmScaleCallbackStruct  callback struct          *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * C. Lin/EAI	        10/97	rename from NxmFrontsSizeCb, cleanup	*
 * F. J. Yen/NCEP	 5/98	used XmTextFieldSetString		*
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 * W. Li/EAI		06/98	Changed text size (1--10)-->(0.1--5.0)	*
 * W. Li/EAI		07/98	Added colors for front			*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
    int		frtsiz;	/* front size */
    float  	frtsiz_f; 
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    frtsiz = (int) cbs->value;
    frtsiz_f = (float)frtsiz/100.0F;
    sprintf (txtstr, "%3.1f", frtsiz_f);
    XmTextFieldSetString (_frt_size_txtW, "");
    XmTextFieldSetString (_frt_size_txtW, txtstr);

    if (cbs->reason == XmCR_VALUE_CHANGED)
	pgfrtw_setTable (-1, frtsiz, -1, -1, -1);
}

/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_strengthTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgfrtw_strengthTxtCb							*
 *									*
 * This function is the callback for front strength text widgets.	*
 *									*
 * void pgfrtw_strengthTxtCb ( w, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *      w               Widget               widget ID                  *
 *      clnt     XtPointer                  client data                *
 *      *cbs	      XmTextVerifyCallbackStruct  callback struct       *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * E. Wehner/EAi	 8/97	Change settings in CES			*
 * C. Lin/EAI	        10/97	rename from NxmFrontsTxtWidCb, cleanup	*
 * F. J. Yen/NCEP	 4/98	Updated wtih new ces function names	*
 * F. J. Yen/NCEP	 5/98	Freed up s; used XmTextFieldGetString	*
 * S. Law/GSC		05/98	added event check and call to _setTable	*
 * C. Lin/EAI	        06/98   use XtFree (S.Danz/AWC)			*
 * W. Li/EAI		10/98	Fixed value change call back problem	*
 ***********************************************************************/
{
    int		slval, frtstrength;	/* front strngth */
    char	*ss;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if (!cbs->event) 
	return;

    /*
     * if the value on corresponding slider is different, set the sliders
     * value accordingly.
     */
    XmScaleGetValue(_frt_strngth_sldW, &slval);
    ss = XmTextFieldGetString(_frt_strngth_txtW);
    frtstrength = atoi(ss);
    XtFree (ss);
    if ( (frtstrength >= 1) && (frtstrength <= MAX_STRNGTH_FRT)) {

        if ((int) frtstrength != slval) {
            XmScaleSetValue(_frt_strngth_sldW, (int) (frtstrength));
        }
	pgfrtw_setTable (frtstrength, -1, -1, -1, -1);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_sizeTxtCb ( Widget w, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgfrtw_sizeTxtCb							*
 *									*
 * This function is the callback function of front size txt widget.	*
 *									*
 * void pgfrtw_sizeTxtCb ( w, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *      w               Widget               widget ID                  *
 *      clnt     XtPointer                  client data                *
 *      *cbs       XmTextVerifyCallbackStruct  callback struct          *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * C. Lin/EAI	        10/97	rename from NxmFrontsTxtSizCb, cleanup	*
 * F. J. Yen/NCEP	 4/98	updated with new ces function names	*
 * F. J. Yen/NCEP	 5/98	Freed s; used XmTextFieldGetString	*
 * S. Law/GSC		05/98	added event check and call to _setTable	*
 * C. Lin/EAI	        06/98	use XtFree (S.Danz/AWC)			*
 * W. Li/EAI		06/98	Changed text size (1--10)-->(0.1--5.0)	*
 * W. Li/EAI		10/98	Fixed value change call back problem	*
 ***********************************************************************/
{
    int		frtsiz;	/* front size */
    float       frtsiz_f;  
    int		slval;
    char	*ss;
/*---------------------------------------------------------------------*/

    /*
     * Confirm there is an event.  If not, this text has already
     * been set up.
     */
    if (!cbs->event) 
	return;

    /* 
     * if the value on corresponding slider is different,
     * set the sliders value accordingly.
     */
    XmScaleGetValue(_frt_size_sldW, &slval);
    ss = XmTextFieldGetString(_frt_size_txtW);
    frtsiz_f = (float)atof(ss);
    XtFree (ss);

    if ( (frtsiz_f >= (float)MIN_SIZE_FRT/100.0F) && 
	 (frtsiz_f <= (float)MAX_SIZE_FRT/10.0F )) {
        frtsiz=(int)(frtsiz_f*100.0F);
        if ( frtsiz  != slval) {
            XmScaleSetValue(_frt_size_sldW, frtsiz);
        }

	pgfrtw_setTable (-1, frtsiz, -1, -1, -1);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_smthPbCb ( Widget ww, XtPointer clnt, XtPointer cbs )
/************************************************************************
 * pgfrtw_smthPbCb							*
 *									*
 * This function is the callback function of front smooth level widget.	*
 *									*
 * void pgfrtw_smthPbCb ( ww, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	ww	Widget		widget ID		*
 *	clnt	XtPointer	client data		*
 *	cbs	XtPointer	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	Created					*
 * F. J. Yen/NCEP	04/98	Updated with new ces function names	*
 * S. Law/GSC		05/98	Added call to pgfrtw_setTable		*
 ***********************************************************************/
{
    int	new_lvl = (long)clnt;
/*---------------------------------------------------------------------*/

    pgfrtw_setTable (-1, -1, new_lvl, -1, -1);

    _frtSmooth = new_lvl;

}

/*=====================================================================*/

void pgfrtw_setTable ( int strength, int size, int smooth, 
						int majcolr, int mincolr )
/************************************************************************
 * pgfrtw_setTable							*
 *									*
 * Places an element into the set table					*
 *									*
 * void pgfrtw_setTable (strength, size, smooth, majcolr, mincolr)	*
 *									*
 * Input parameters:							*
 *	strength	int	front strength				*
 *	size		int	front size				*
 *	smooth		int	front smooth level			*
 *	majcolr		int	front majcolor				*
 *	mincolr		int	front mincolor				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	moved from various function		*
 * E. Safford/GSC	04/99	fix irix6 compiler warnings		*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * E. Safford/SAIC	12/01	added pgutls_initHdr()			*
 * E. Safford/SAIC	02/02	set group type in el for ces_set	*
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		loglev, ier, ier1;
    char	logstr[10], grp[4];
/*---------------------------------------------------------------------*/

    loglev = 2;
    strcpy(grp, "CES");

    pgutls_initHdr( &(el.hdr) );

    el.hdr.vg_type = FRONT_ELM;
    el.hdr.vg_class = CLASS_FRONTS;
    ces_get(_frtType, &el, &ier);
    if (ier  != 0) {
	sprintf(logstr, "%d ", _frtType);

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1, strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }

    if (strength > 0) 
	el.elem.frt.info.fwidth = strength;
    if (size > 0)
	el.elem.frt.info.fpipsz = size;
    if (smooth >= 0)
	el.hdr.smooth = (char) smooth;

    if (majcolr>=0) {
	el.hdr.maj_col = majcolr;
	if (_frtType != 0)
	    el.hdr.min_col = majcolr;
    }

    if( mincolr >= 0 ) {
	el.hdr.min_col = mincolr;
    }

    if ( _groupTyp >= 0 ) {
	el.hdr.grptyp = _groupTyp;
    }

    ces_set(_frtType, &el, &ier);

    if (ier  != 0) {
	sprintf(logstr, "%d ", _frtType);
        er_lmsg ( &loglev, grp, &ier, logstr, &ier1, strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }
}

/*=====================================================================*/

void pgfrtw_majcolorCb ( Widget wid, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgfrtw_majcolorCb							*
 *									*
 * Callback for color button widget. 					*
 *									*
 * void pgfrtw_majcolorCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *   wid		Widget		Widget ID		*
 *   clnt		XtPointer      	color			*
 *   cbs		XtPointer	callback struct		*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		07/98						*
 * W. Li/EAI            10/98   Removed OK button from color palette    *
 ***********************************************************************/
{
    clnt = (XtPointer) &_frtMajColr;
    NxmClrW_popup (wid, clnt, cbs);
}

/*=====================================================================*/

void pgfrtw_mincolorCb ( Widget wid, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgfrtw_mincolorCb							*
 *									*
 * Callback for color button widget. 					*
 *									*
 * void pgfrtw_mincolorCb (wid, clnt, cbs)			*
 *									*
 * Input parameters:							*
 *   wid		Widget			Widget ID		*
 *   clnt		XtPointer       	color			*
 *   cbs		XtPointer		callback struct		*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		07/98						*
 * W. Li/EAI            10/98   Removed OK button from color palette    *
 ***********************************************************************/
{
    clnt = (XtPointer) &_frtMinColr;
    NxmClrW_popup (wid, clnt, cbs);
}

/*=====================================================================*/

void pgfrtw_setmajColor ( void ) 
/************************************************************************
 * pgfrtw_setmajColor							*
 *									*
 * Stores the new major color set by the NxmClrW functions		*
 *									*
 * void pgfrtw_setmajColor ()						*
 *									*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI	07/98							*
 ***********************************************************************/
{
    pgfrtw_setTable (-1, -1, -1, _frtMajColr, -1);

}

/*=====================================================================*/

void pgfrtw_setminColor ( void ) 
/************************************************************************
 * pgfrtw_setminColor							*
 *									*
 * Stores the new minor color set by the NxmClrW functions		*
 *									*
 * void pgfrtw_setminColor ()						*
 *									*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI	07/98							*
 ***********************************************************************/
{
	pgfrtw_setTable (-1, -1, -1, -1, _frtMinColr);

}
/*=====================================================================*/

void pgfrtw_setLabFlag ( Boolean lab_flag )
/************************************************************************
 * pgfrtw_setLabFlag							*
 *									*
 * set the label flag 							*
 *									*
 * void pgfrtw_setLabFlag (lab_flag)					*
 *									*
 * Input parameters:							*
 *	lab_flag	Boolean	lab_flag				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	02/99							*
 * W. Li/EAI	05/99	Removed label initial value setting by lab_flag	*
 * W. Li/EAI	05/99	added group type in symbol editor		*
 * M. Li/SAIC	05/02   checked for active Grouping			*
 ***********************************************************************/
{
    _labelFlag = lab_flag;

    XtSetSensitive(_label_submenuW, _labelFlag);
    if ( pgpalw_isGrpActv() ) {
        XtSetSensitive(_group_typeW, False);
    }
    else {
        XtSetSensitive(_group_typeW, _labelFlag);
    }
}

/*=====================================================================*/

void pgfrtw_setLabItems ( int group_type )
/************************************************************************
 * pgfrtw_setLabItems							*
 *									*
 * set the label item strings based on the group type and choose the    *
 * default label. 							*
 *									*
 * void pgfrtw_setLabItems (group_type)					*
 *									*
 * Input parameters:							*
 *	group_type	int	group type				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI  	04/00   initial coding                          *
 * H. Zeng/EAI  	04/00   added _groupTyp&_objId variable         *
 * E. Safford/GSC	12/00	renamed grp defines			*
 * H. Zeng/EAI          03/01   modified to use ces_gtglbls()           *
 * S. Jacobs/NCEP	 3/01	Added OBJ_TRPTFNT			*
 * E. Safford/SAIC	04/02	handle empty label string		*
 * H. Zeng/EAI          04/02   modified to use cur_grptyp              *
 ***********************************************************************/
{
int   		ii, which_label, obj_id, nlbl = 0, iret;
char  		*ptr, lbls[256];
XmString  	xmstr;
static int      cur_grptyp = IMP_CHOICE;
/*---------------------------------------------------------------------*/

    obj_id = pgpalw_getCurObjId(); 

    if(group_type != cur_grptyp || obj_id != _objId) {
        cur_grptyp = group_type;
        _objId = obj_id;

        /*
         * Set label item strings based on selected group type
         */
        ces_gtglbls(group_type, &nlbl, lbls, &iret);
        if ( iret < 0 ) {
 	    strcpy (lbls, "Other;");
	    nlbl = 1;
        }

        ii = 0;
        ptr = strtok(lbls, ";");
        while ( ptr != (char *)NULL && ii < nlbl ) {
            xmstr = XmStringCreateLocalized (ptr);
	    XtVaSetValues(_label_pbW[ii],
	      XmNlabelString, 		xmstr,
	      XmNalignment,		XmALIGNMENT_CENTER,
	      NULL);
    	    XtManageChild(_label_pbW[ii]);
            XmStringFree (xmstr);

	    ptr = strtok(NULL, ";" );
            ii++;

        }

        for (ii = nlbl; ii < GRP_MXELE; ii++) {
    	    XtUnmanageChild(_label_pbW[ii]);
        }

 
        /*
         * Set default label item
         */
        if(group_type != 3) {
       
           /*
            * When the group type is not FRONT
            */
           which_label = 0;
        }
        else {
            switch (obj_id){
	        case OBJ_OCCLFNT:
	        case OBJ_WKOCCLFNT:
	        case OBJ_DIFOCCLFNT:
                   which_label = 0;
	           break;
	        case OBJ_COLDFNT:
	        case OBJ_WKCOLDFNT:
	        case OBJ_DIFCOLDFNT:
                   which_label = 2;
	           break;
	        case OBJ_WARMFNT:
	        case OBJ_WKWARMFNT:
	        case OBJ_DIFWARMFNT:
                   which_label = 4;
	           break;
	        case OBJ_STATFNT:
	        case OBJ_WKSTATFNT:
	        case OBJ_DIFSTATFNT:
                   which_label = 6;
	           break;
	        case OBJ_DRYFNT:
	        case OBJ_TROFFNT:
	        case OBJ_TRPTFNT:
	        case OBJ_SQUALL:
                   which_label = 8;
	           break;
	        default:
                   which_label = 0;
	           break;

            }

        } /* the end of else */        


        pgfrtw_setLabValue (which_label);


    } /* the end of if */

}

/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_labelToggCb ( Widget w, XtPointer clnt, XtPointer cbs ) 
/************************************************************************
 * pgfrtw_labelToggCb                                                   *
 *                                                                      *
 * Callback for label toggle button widget.                             *
 *                                                                      *
 * void pgfrtw_labelToggCb( w, clnt, cbs)                  *
 *                                                                      *
 * Input parameters:                                                    *
 *   w          Widget          Widget ID                           *
 *   clnt    	XtPointer       not used                            *
 *   cbs    	XtPointer	callback struct                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	02/99							*
 * W. Li/EAI	05/99	Removed label initial value setting by lab_flag	*
 * W. Li/EAI	05/99	added group type in symbol editor		*
 * H. Zeng/EAI  06/00   added label color toggle callback               *
 * M. Li/SAIC	05/02	Check for the active Grouping			*
 ***********************************************************************/
{
     int      which_toggle;
     Boolean  btnval;
/*---------------------------------------------------------------------*/

    /* check to see if the button is up or down and take appropriate
     * actions.
     */
    XtVaGetValues(w, XmNset, &btnval, NULL);
    which_toggle = (long)clnt;

    if(which_toggle == 0) {
       _labelFlag=(btnval) ? 1 : 0;

       XtSetSensitive(_label_submenuW, _labelFlag);
       XtSetSensitive(_label_menuW2, _labelFlag);
       if ( pgpalw_isGrpActv() ) {
           XtSetSensitive(_group_typeW, False);
    	}
    	else {
            XtSetSensitive(_group_typeW, _labelFlag);
    	}
    }
    else {
       _labelColorFlag=(btnval) ? 1 : 0;

    }

}
/*=====================================================================*/
/* ARGSUSED */
void pgfrtw_labelPbCb ( Widget w, long clnt, XtPointer cbs ) 
/************************************************************************
 * pgfrtw_labelPbCb							*
 *									*
 * This function is the callback function of line label widget.		*
 *									*
 * void pgfrtw_labelPbCb ( w, clnt, cbs )			*
 *									*
 * Input parameters:							*
 *	w		Widget			widget ID		*
 *	clnt	long			client data		*
 *	cbs	XtPointer	callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	        02/99        initial coding			*
 * H. Zeng/EAI          04/00        modified for new grptyp.tbl        *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    pgfrtw_setLabValue ((int)clnt);
}

/*=====================================================================*/

/* ARGSUSED */
void pgfrtw_grpTypCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgfrtw_grpTypCb							*
 *									*
 * Callback function for group type option menu.			*
 *									*
 * void pgfrtw_grpTypCb (w, which, call )				*
 *									*
 * Input parameters:							*
 *	w	Widget		option button widget ID			*
 *	which	long		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          04/02   initial coding                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _curGrpIdx = (int)which;
    _groupTyp = ces_gtgmsid (_curGrpIdx);     
    pgfrtw_setLabItems(_groupTyp);   

}

/*=====================================================================*/

void pgfrtw_setLabValue ( int which )
/************************************************************************
 * pgfrtw_setLabValue							*
 *									*
 * put the selected label name into the text string 			*
 *									*
 * void pgfrtw_setLabValue (which)					*
 *									*
 * Input parameters:							*
 *	which		int	which label				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 * W. Li/EAI	02/99	added six label types				*
 * W. Li/EAI	03/99	changed value setting to XtGetVlaue		*
 * W. Li/EAI	05/99	added label initial value setting by obj_type	*
 * H. Zeng/EAI  04/00   added call to XtFree()                          *
 ***********************************************************************/
{
    XmString	xmstr;
    char	*text;
/*---------------------------------------------------------------------*/

    XtVaSetValues (_label_optW, 
	XmNmenuHistory,			_label_pbW[which], 
	NULL);

    XtVaGetValues(_label_pbW[which],
	XmNlabelString, 		&xmstr,
	NULL);    

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text);
    XmStringFree(xmstr);

    if ( strcmp(text, "Other")!=0 ) {
        sprintf(_labelName, "%s", text);
    }
    else {
	sprintf(_labelName, "%s", "");
    }
    XtFree(text);
}

/*=====================================================================*/

void pgfrtw_getLabValue ( char *label )
/************************************************************************
 * pgfrtw_getLabValue							*
 *									*
 * This function gets the label text value 				*
 *									*
 * void pgfrtw_getLabValue ( label)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*label		char		* label value			*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 ***********************************************************************/
{
    strcpy(label, _labelName);
}

/*=====================================================================*/

int pgfrtw_getFrtColor ( void )
/************************************************************************
 * pgfrtw_getFrtColor							*
 *									*
 * The function returns an integer value specifying the index of current*
 * front color.	                                                        *
 *									*
 * int  pgfrtw_getFrtColor()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgfrtw_getFrtColor	int	color index  	                        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	   06/00    initial coding				*
 ***********************************************************************/
{
   return( _frtMajColr );
}

/*=====================================================================*/

Boolean pgfrtw_getLabFlag ( void )
/************************************************************************
 * pgfrtw_getLabFlag							*
 *									*
 * This function returns a boolean value specifying whether the lines	*
 * labelFlag is TRUE or FALSE. 						*
 *									*
 * Boolean pgfrtw_getLabFlag( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgfrtw_getLabFlag	Boolean	True -- use flag, False -- do not use  	*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	02/99							*
 ***********************************************************************/
{
   return( _labelFlag );
}

/*=====================================================================*/

Boolean pgfrtw_getLabColorFlag ( void )
/************************************************************************
 * pgfrtw_getLabColorFlag						*
 *									*
 * This function returns a boolean value specifying whether the label   *
 * color flag is TRUE or FALSE. 					*
 *									*
 * Boolean pgfrtw_getLabColorFlag( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgfrtw_getLabColorFlag	Boolean	  True -- use flag              *
 *                                        False -- do not use  	        *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	      06/00     initial coding				*
 ***********************************************************************/
{
   return( _labelColorFlag );
}

/*=====================================================================*/

char pgfrtw_getGrptyp ( void )
/************************************************************************
 * pgfrtw_getGrptyp							*
 *									*
 * This function returns the current grptyp from the attribute window.	*
 *									*
 * char pgfrtw_getGrptyp ( )  	 					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return:								*
 * pgfrtw_getGrptyp  	char	the current value of the grptyp		*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	04/01						*
 ***********************************************************************/
{
char	grptyp;
/*---------------------------------------------------------------------*/

    grptyp = ( pgfrtw_getLabFlag() ) ? _groupTyp : 0;
    return ( grptyp );

}

/*=====================================================================*/

void pgfrtw_initType ( VG_DBStruct *el )
/************************************************************************
 * pgfrtw_initType							*
 *									*
 * This function initiates group type menu option based on grptyp field *
 * of el.                                                               *
 *									*
 * void pgfrtw_initType ( el )				                *
 *									*
 * Input parameters:							*
 *	el	VG_DBStruct*	       pointer to vg element		*
 *									*
 * Output parameters:						 	*
 *		NONE						        *
 * Return parameters:							*
 *		NONE						        *
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		05/99						*
 * E. Safford/GSC	06/99	set OBJ_CNTR to group type label	*
 * E. Safford/GSC	06/99	set all symbols to group type label	*
 * D.W.Plummer/NCEP	01/01	added SPLN6/20/21 to get OUTLOOK	*
 * M. Li/GSC            01/01   added CLASS_PRODUCTS                    *
 * D.W.Plummer/NCEP     01/01   replaced hardwired grptyp specification *
 * M. Li/GSC		01/01	Other -> OUTLOOK for outlook		*
 * H. Zeng/EAI          02/01   changed para. for new setting table     *
 * H. Zeng/EAI          03/01   rewrote for new group type table        *
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * H. Zeng/EAI          04/02   rewrote from pggrpw_initType()          *
 * M. Li/SAIC		05/02   Check for the active Grouping		*
 * H. Zeng/XTRIA	03/03   checked layer default group type        *
 ***********************************************************************/
{
int	 cur_layer, def_grp, ier, type_id = 0;
Boolean  layer_flag;
/*---------------------------------------------------------------------*/

    /*
     * Check if there is a default group for the current layer.
     */
    cur_layer  = pglayer_getCurLayer();
    def_grp    = pglayer_getDefGrp(cur_layer);
    layer_flag = ces_getflag(_frtType, el, &ier);

    /*
     * Initialize the group type based on a preference order.
     */ 
    if ( pgpalw_isGrpActv() ) {

        /*
     	 * If currently GROUP is active, use active group type. 
     	 */
        _groupTyp  = pggrpw_getGrpType ();
	_curGrpIdx = ces_gtgavid (_groupTyp);
    }
    else if ( pgpalw_isLayerActv() &&
              def_grp != NON_GRPID && 
              ier == 0             && 
              layer_flag              ) { 

             _groupTyp  = def_grp;
             _curGrpIdx = ces_gtgavid (_groupTyp);
    }
    else {
    	type_id   = (int)el->hdr.grptyp;

    	if(type_id != NON_GRPID) {
           _groupTyp = type_id;
           _curGrpIdx = ces_gtgavid (_groupTyp);
    	}
    	else {
           _curGrpIdx = 0;
           _groupTyp  = ces_gtgmsid (_curGrpIdx);
    	}
    }


    XtVaSetValues (_group_typeW,
		XmNmenuHistory,		_group_buttonW[_curGrpIdx], 
		NULL);

}

/*=====================================================================*/
void pgfrtw_updtGrpMenu ( int index )
/************************************************************************
 * pgfrtw_updtGrpMenu                                                   *
 *                                                                      *
 * This function updates the group type menu based on the input group   *
 * type index.                                                          *
 *                                                                      *
 * void pgfrtw_updtGrpMenu ( index )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      index   int             group type index                        *
 *                                                                      *
 * Output parameters:                                                   *
 *              NONE                                                    *
 * Return parameters:                                                   *
 *              NONE                                                    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           05/02                                           *
 ***********************************************************************/
{

    XtVaSetValues (_group_typeW,
                XmNmenuHistory,         _group_buttonW[index],
                NULL);
    pgfrtw_grpTypCb(NULL, index, NULL);
}

/*=====================================================================*/
