#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "proto_xw.h"

#define MIN_SIZE_SCALE	 0.1F
#define MAX_SIZE_SCALE	10.0F

#define MIN_WIDTH_SCALE	 1
#define MAX_WIDTH_SCALE	10

#define MIN_KPOS_SCALE	25
#define MAX_KPOS_SCALE	75

#define MAX_SMTH_LVLS	 3
#define IMP_CHOICE       -99

#define FILLED_FLD	 0
#define CLOSED_FLD	 1

#define  ICON_DIR	"$NAWIPS/icons/fillPatt"
#define  ICON_SYM_DIR	"$NAWIPS/icons/nmap"

#define  LABEL_TOGGLE    (0)
#define  USE_LINE_COLOR  (1)

static Widget	_lines_dlgW;
static Widget   _lin_fillW;
static Widget   _lin_closW;
static Widget	_lin_size_sldW;
static Widget	_lin_size_txtW;
static Widget   _lin_width_sldW;
static Widget	_lin_width_txtW;
static Widget	_kink_pos_sldW;
static Widget	_kink_pos_txtW;
static Widget   _lin_patt_menuW;
static Widget	_lin_smth_optW;
static Widget	_lin_smth_pbW[MAX_SMTH_LVLS];

static Widget   _labelReminder_formW;
static Widget   _labelReminder_textW;
static Widget   _labelReminder_typTextW;

static Widget	_label_formW;
static Widget	_label_optW;
static Widget	_label_pbW[GRP_MXELE];
static Widget	_label_toggW, _label_toggW2;
static Widget	_label_menuW, _label_menuW2;
static Widget	_label_submenuW;

static Widget	_label_choiceW;
static Widget	_label_radioW[2];
static Widget	_symb_optW;
static Widget	_symb_menuW;
static Widget	_symb_submenuW;
static Widget	_symb_pbW[GRP_MXELE];

static Widget	  _lineColrW;
static WidgetList _fillPattWid;

static Widget	  _group_typeW;
static WidgetList _group_buttonW;

static Widget	  _ctlBb;
static WidgetList _ctlBtns;

static Boolean	_labelFlag = FALSE, _labelColorFlag = FALSE;

static char     _lineAttrStr[GRP_MXINFO];

static int	_one		= 1;
static int	_attrFill	= 0;	
static int	_attrClos	= 0;
static int	_attrColr	= 0;	
static int	_attrWdth	= 1;
static float	_attrSize	= 1.0F;
static int	_attrKpos	= 50; /* Kink position 25~75 */
static int	_attrSmth	= 0;
static int	_attrPatt	= 1;

static int      _numGrp         = 0;
static int      _groupTyp       = IMP_CHOICE; 
static int      _curGrpIdx      = IMP_CHOICE;
                               
static int      _objId          = IMP_CHOICE; 
                                /* A local copy of cur. objId */

static char	_vgType;
static int	_subTyp;

static char	_labelName[10];
static int	_symbID[GRP_MXELE]; /* Available symbols IDs*/
static char	_symbName[GRP_MXELE][10];
static int	_currSymbID = 0;    /* set the first symbol as default */
static char	*_currSymbName;   

/*
 *  private callback functions
 */
void pgline_closeCb 	( Widget, XtPointer, XtPointer );
void pgline_colorCb 	( Widget, XtPointer, XtPointer );
void pgline_fillCb 	( Widget, XtPointer, XtPointer );
void pgline_fillPattCb 	( Widget, XtPointer, XtPointer );
void pgline_grpTypCb    ( Widget, XtPointer, XtPointer );
void pgline_kposCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgline_kposTxtCb 	( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
void pgline_radioBoxCb 	( Widget, XtPointer, XtPointer );
void pgline_labelPbCb 	( Widget, XtPointer, XtPointer );
void pgline_symbPbCb 	( Widget, XtPointer, XtPointer );
void pgline_labelToggCb ( Widget, XtPointer, XtPointer );
void pgline_sizeCb 	( Widget, XtPointer, XmScaleCallbackStruct *cbs );
void pgline_sizTxtCb 	( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );
void pgline_smthPbCb 	( Widget, XtPointer, XtPointer );
void pgline_widthCb 	( Widget, XtPointer, XtPointer );
void pgline_widTxtCb 	( Widget, XtPointer, XmTextVerifyCallbackStruct *cbs );

/*
 *  private functions
 */
void pgline_setLabValue ( int which );
static void pgline_setLabelAttr ( int *iret );
void pgline_initType ( VG_DBStruct *el );
int pgline_getSymbCls ( int symb_id );   

/************************************************************************
 * nmap_pgline.c							*
 *									*
 * This module creates and displays the VG lines setting box. It also	*
 * contains the callbacks for the box.					*
 *									*
 * CONTENTS:								*
 *  pgline_create()     create line attribute editing window            *
 *  pgline_popup()      pop up line attribute editing window            *
 *  pgline_popdown()    pop down line attribute editing window          *
 *  pgline_setAttr()    set the line attributes                     	*
 *  pgline_setLabFlag() set the label flag				*
 *  pgline_setLabItems()set label items based on the group type         *
 *									*
 *  pgline_isUp()       querry whether the window is up         	*
 *  pgline_getAttr()    querry the line attributes  		      	*
 *  pgline_getColor()   querry the line color        			*
 *  pgline_getLabFlag() querry the saved label flag			*
 *  pgline_getLabColorFlag() querry the label color flag                *
 *  pgline_getLabValue  querry the saved label name text value		*
 *  pgline_getAttrStr   gets a copy of _lineAttrStr                     *
 *  pgline_getGrptyp    returns the current grptyp 			*
 *  pgline_setAttrStr   sets _lineAttrStr                               *
 *									*
 *  pgline_fillCb()     callback for fill button        		*
 *  pgline_closeCb()    callback for close button        		*
 *  pgline_widthCb()    callback for width scale        		*
 *  pgline_widTxtCb()   callback for width text widget			*
 *  pgline_sizeCb()     callback for size scale        			*
 *  pgline_sizTxtCb()   callback for size text widget			*
 *  pgline_kposCb()     callback for kink position scale    		*
 *  pgline_kposTxtCb()  callback for kink position text widget		*
 *  pgline_colorCb()    callback for color button        		*
 *  pgline_smthPbCb()	Callback for front smoothing level		*
 *  pgline_radioBoxCb() Callback for radio box widget			*
 *  pgline_labelPbCb()  Callback for label-line-in-text widget		*
 *  pgline_symbPbCb()   Callback for label-line-in-symbol widget	*
 *  pgline_labelToggCb  Callback for line label toggle button widget	*
 *  pgline_fillPattCb()	callback for fill pattern			*
 *  pgline_grpTypCb()   callback for group type option menu             *
 *									*
 *  pgline_saveAttr()	saves current element attribs into set table	*
 *  pgline_setLabValue	put the selected label name into text editor	*
 *  pgline_initType ()  initialize the group type                       *
 *  pgline_updtGrpMenu() Update the group type menu			* 
 *  pgline_getLabType()  gets the selected label type			*
 *  pgline_getSymbInfo() gets the current symbol's class and ID		*
 *  pgline_getSymbCls()  retrieves a symbol's class based on its ID	*
 ***********************************************************************/

/*=====================================================================*/

void pgline_create ( Widget parent )
/************************************************************************
 * pgline_create							*
 *									*
 * This function creates a Lines attribute selection box.		*
 *									*
 * void pgline_create ( parent )					*
 *									*
 * Input parameters:							*
 *	parent		Widget		Parent widget			*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	07/97						*
 * E. Wehner/EAi	07/97		Added scrollers for settings	*
 * C. Lin/EAI	       	10/97		Add color widget, some clean up	*
 * C. Lin/EAI	       	10/97	Rename from NxmLinesCr,  clean up	*
 * E. Safford/GSC      	10/97	Modified to use NxmClrW (color popup)   *
 * E. Safford/GSC      	11/97	Added Apply/Cancel buttons for editing  *
 * A. Hardy/GSC        	02/98    Fixed scale bars, they go left to right*
 * W. Li/EAI	       	04/98	Added fill pattern			*
 * W. Li/EAI	       	04/98	Added frame for popup window		*
 * W. Li/EAI	       	04/98	Removed stroke from CLASS_LINES		*
 * F. J. Yen/NCEP      	05/98	Renamed _gemType with _subTyp		*
 * W. Li/EAI		02/99	Added label type selections 		*
 * W. Li/EAI		02/99	added 3, 4, 5, 6 label types		*
 * W. Li/EAI		03/99	cleaned up				*
 * S. Law/GSC		03/99	cleaned up attribute initializations	*
 * W. Li/EAI		05/99	added group type in line editor		*
 * E. Safford/GSC	06/99	add outlook label menu items		*
 * S. Schotz/NCEP	07/99	Added 0 label value			*
 * S. Law/GSC		07/99	added defines for minimum scale values	*
 * S. Law/GSC		11/99	changed label names			*
 * H. Zeng/EAI          02/00   Did minor changes to appearance         *
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * H. Zeng/EAI          06/00   added label color toggle                *
 * H. Zeng/EAI          07/00   added label reminder text widget        *
 * E. Safford/GSC	12/00	rename grp defines, fix comp warnings   *
 * M. Li/GSC		01/01	set default for _label_menuW2 on	*
 * E. Safford/GSC	01/01	add group type display			*
 * D.W.Plummer/NCEP	 5/01	chg def of "Use Line Color" toggle OFF	*
 * E. Safford/SAIC	08/01   use toggle defines			*
 * J. Wu/SAIC		10/01   add kink position slider		*
 * H. Zeng/EAI          04/02   removed pggrpw_createTypeBox()          *
 * J. Wu/SAIC		05/02   verify line width/size/kink position	*
 * J. Wu/SAIC		07/02   add capability to label	line with symbol*
 * m.gamazaychikov/SAIC 01/03	added new names patt03 through patt06	*
 *				to pattern selections			*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    Widget	bb, rc1, rc2, size_form, width_form, smth_form, label_form;
    Widget	smth_menubar, smth_label, label_menubar, pulldown;
    Widget	line_patt_menu_bar, pane;
    Widget	type_menu, group_form, labelReminder_label;
    Widget	kpos_form;
    Widget	radio_opt, pulldown_opt;
    XmString	xmstr;
    int		jj, grptyp, iret;
    long	ii, ignore;
    Arg         args[10];
    Cardinal	nn;
    Pixel	fg, bg;
    Pixmap	patt_pxm, symb_pxm;
    char	*symb_xbm = "blank.xbm";
    char	filename[256], warning[200];
    char	*btnstr[] = {"Apply", "Cancel"};
    char	*lblstr[2] = {"Text", "Symbol"};
    char        *names = NULL, cc[10], grpnam[64];
    char	*patt_xbm[]  = {"patt00.xbm", "patt01.xbm", "patt02.xbm", 
                                "patt03.xbm", "patt04.xbm", "patt05.xbm", 
                                "patt06.xbm"}; 
/*---------------------------------------------------------------------*/
    
/*
 *  Create the VG lines selection box.
 */
    _lines_dlgW = XmCreateFormDialog ( parent, "lines_edit",
				      NULL, 0 );
    XtVaSetValues(_lines_dlgW,
		  XmNnoResize,		TRUE,
		  XmNautoUnmanage,	FALSE,
		  NULL);

    xmstr = XmStringCreateLocalized("Line Attributes");
    XtVaSetValues( _lines_dlgW, XmNdialogTitle, xmstr, NULL);
    XmStringFree(xmstr);

    pane  = XtVaCreateManagedWidget ("pane",
				     xmPanedWindowWidgetClass,	_lines_dlgW,
				     XmNsashWidth,		1,
				     XmNsashHeight,		1,
				     NULL);

/*
 *  Create fill, close, and color input
 */

    bb = XtVaCreateWidget( "pg_lines_bb",
			  xmBulletinBoardWidgetClass,	pane,
			  NULL);

    rc1 = XtVaCreateWidget("Attribs",
			   xmRowColumnWidgetClass, 	bb,
			   XmNorientation, 		XmHORIZONTAL,
			   XmNradioAlwaysOne,		FALSE,
			   NULL);

    _lin_fillW	= XtVaCreateManagedWidget("Filled",
					  xmToggleButtonGadgetClass,	rc1,
					  NULL);

    XtAddCallback(_lin_fillW, XmNvalueChangedCallback, 
    			(XtCallbackProc)pgline_fillCb, NULL);

    _lin_closW	= XtVaCreateManagedWidget("Closed",
					  xmToggleButtonGadgetClass,	rc1,
					  NULL);

    XtAddCallback(_lin_closW, XmNvalueChangedCallback, 
    			(XtCallbackProc)pgline_closeCb, NULL);
    XtManageChild (rc1);

/*
 *  Create color widget
 */

    _lineColrW	= XtVaCreateManagedWidget(" ",
					  xmPushButtonWidgetClass,	bb,
					  XmNwidth,			25,
					  XmNheight,			20,
					  XmNx,				180,
					  XmNy,				15,
					  NULL);

    XtAddCallback(_lineColrW, XmNactivateCallback, 
    			(XtCallbackProc)pgline_colorCb, NULL);
    XtManageChild(bb);

/*
 *  Create width and size input
 */

    rc2 = XtVaCreateWidget("Attribs",
			   xmRowColumnWidgetClass, 	pane,
			   XmNorientation, 		XmVERTICAL,
			   XmNradioAlwaysOne,		FALSE,
			   NULL);

    width_form = XtVaCreateManagedWidget("_lin_width_formW",
					 xmFormWidgetClass,
					 rc2, NULL);

    _lin_width_txtW = 
	XtVaCreateManagedWidget("lin_width",
				xmTextFieldWidgetClass,		width_form,
				XmNcolumns,	                4,
				XmNvalue,			"2",
				XmNcursorPositionVisible,	True,
				XmNrightAttachment,		XmATTACH_FORM,
				NULL);

    XtAddCallback(_lin_width_txtW, XmNmodifyVerifyCallback, 
                 (XtCallbackProc)pgutls_vrfyPosIntCb, NULL);    
    XtAddCallback(_lin_width_txtW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_widTxtCb, NULL);

    _lin_width_sldW=(Widget)XmCreateScale(width_form, "width", NULL, 0);
    XtManageChild( _lin_width_sldW);
    xmstr = XmStringCreateLocalized("Width");
    XtVaSetValues( _lin_width_sldW,
		  XmNorientation,		XmHORIZONTAL,
		  XmNminimum,			MIN_WIDTH_SCALE,
		  XmNmaximum,			MAX_WIDTH_SCALE,
		  XmNprocessingDirection,	XmMAX_ON_RIGHT,
		  XmNvalue,			_attrWdth,
		  XmNscaleMultiple,		1,
		  XmNshowValue,			False,
		  XmNtitleString,		xmstr,
		  XmNtopAttachment,		XmATTACH_FORM,
		  XmNleftAttachment,		XmATTACH_FORM,
		  XmNrightAttachment,		XmATTACH_WIDGET,
		  XmNrightWidget,		_lin_width_txtW,
		  NULL);
    XmStringFree(xmstr);
    XtAddCallback(_lin_width_sldW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_widthCb, NULL);
    XtAddCallback(_lin_width_sldW, XmNdragCallback, 
		  (XtCallbackProc)pgline_widthCb, NULL);

    size_form = 
	XtVaCreateManagedWidget("_lin_size_formW",
				xmFormWidgetClass,	rc2,
				XmNtopAttachment,	XmATTACH_WIDGET,
				XmNtopWidget,		width_form,
				XmNleftAttachment,	XmATTACH_FORM,
				XmNrightAttachment,	XmATTACH_FORM,
				NULL);

    _lin_size_txtW = 
	XtVaCreateManagedWidget("lin_size",
				xmTextFieldWidgetClass,		size_form,
				XmNcolumns,			4,
				XmNvalue,			"1",
				XmNcursorPositionVisible,	True,
				XmNrightAttachment,		XmATTACH_FORM,
				NULL);

    XtAddCallback(_lin_size_txtW, XmNmodifyVerifyCallback, 
                 (XtCallbackProc)pgutls_vrfyPosFltCb, NULL);    
    XtAddCallback(_lin_size_txtW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_sizTxtCb, NULL);

    _lin_size_sldW = (Widget)XmCreateScale(size_form, "size", NULL, 0);
    XtManageChild( _lin_size_sldW);
    xmstr = XmStringCreateLocalized("Pattern size");
    XtVaSetValues( _lin_size_sldW,
		  XmNorientation,             	XmHORIZONTAL,
		  XmNminimum,                 	(int) (MIN_SIZE_SCALE * 10.0F),
		  XmNmaximum,                 	(int) (MAX_SIZE_SCALE * 10.0F),
		  XmNprocessingDirection,     	XmMAX_ON_RIGHT,
		  XmNvalue,                   	(int) (_attrSize * 10.0F),
		  XmNshowValue,               	False,
		  XmNtitleString,             	xmstr,
		  XmNtopAttachment,           	XmATTACH_FORM,
		  XmNleftAttachment,          	XmATTACH_FORM,
		  XmNrightAttachment,         	XmATTACH_WIDGET,
		  XmNrightWidget,             	_lin_size_txtW,
		  NULL);
    XmStringFree(xmstr);

    XtAddCallback(_lin_size_sldW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_sizeCb, NULL);
    XtAddCallback(_lin_size_sldW, XmNdragCallback, 
		  (XtCallbackProc)pgline_sizeCb, NULL);
    kpos_form = 
	XtVaCreateManagedWidget("_kink_pos_formW",
				xmFormWidgetClass,	rc2,
				XmNtopAttachment,	XmATTACH_WIDGET,
				XmNtopWidget,		size_form,
				XmNleftAttachment,	XmATTACH_FORM,
				XmNrightAttachment,	XmATTACH_FORM,
				NULL);

    _kink_pos_txtW = 
	XtVaCreateManagedWidget("kink_pos",
				xmTextFieldWidgetClass,		kpos_form,
				XmNcolumns,			4,
				XmNvalue,			"0.50",
				XmNcursorPositionVisible,	True,
				XmNrightAttachment,		XmATTACH_FORM,
				NULL);

    XtAddCallback(_kink_pos_txtW, XmNmodifyVerifyCallback, 
                 (XtCallbackProc)pgutls_vrfyPosFltCb, NULL);    
    XtAddCallback(_kink_pos_txtW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_kposTxtCb, NULL);

    _kink_pos_sldW = (Widget)XmCreateScale(kpos_form, "kpos", NULL, 0);
    XtManageChild( _kink_pos_sldW );
    xmstr = XmStringCreateLocalized("Kink Position");
    XtVaSetValues( _kink_pos_sldW,
		  XmNorientation,             	XmHORIZONTAL,
		  XmNminimum,                 	MIN_KPOS_SCALE,
		  XmNmaximum,                 	MAX_KPOS_SCALE,
		  XmNprocessingDirection,     	XmMAX_ON_RIGHT,
		  XmNvalue,                   	_attrKpos,
		  XmNscaleMultiple,		5,
		  XmNshowValue,               	False,
		  XmNtitleString,             	xmstr,
		  XmNtopAttachment,           	XmATTACH_FORM,
		  XmNleftAttachment,          	XmATTACH_FORM,
		  XmNrightAttachment,         	XmATTACH_WIDGET,
		  XmNrightWidget,             	_kink_pos_txtW,
		  NULL);
    XmStringFree(xmstr);

    XtAddCallback(_kink_pos_sldW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_kposCb, NULL);
    XtAddCallback(_kink_pos_sldW, XmNdragCallback, 
		  (XtCallbackProc)pgline_kposCb, NULL);    
    
    XtManageChild(rc2);

/*
 *  Create smooth widget input
 */

    smth_form = 
	XtVaCreateManagedWidget("_lin_smth_formW",
				xmFormWidgetClass,	pane,
				NULL);
    smth_label  = XtVaCreateManagedWidget ("Smoothing Level:",
					   xmLabelGadgetClass,	smth_form,
					   XmNleftAttachment,	XmATTACH_FORM,
					   NULL); 

    smth_menubar  = XmCreatePulldownMenu (smth_form, "Smooth", NULL, 0);
    _lin_smth_optW = XmCreateOptionMenu (smth_form, "smth", NULL, 0);

    for (ii=0; ii < MAX_SMTH_LVLS; ii++) {
	sprintf (cc, "%ld", ii);
        xmstr = XmStringCreateLocalized (cc);
        _lin_smth_pbW[ii] = 
	    XtVaCreateManagedWidget(cc,
				    xmPushButtonWidgetClass,	smth_menubar,
				    XmNlabelString,		xmstr,
				    NULL);
        XmStringFree (xmstr);
        XtAddCallback(_lin_smth_pbW[ii], XmNactivateCallback,
		      (XtCallbackProc)pgline_smthPbCb, (XtPointer) ii);
    }

    xmstr = XmStringCreateLocalized ("\0");
    XtVaSetValues (_lin_smth_optW, 
		   XmNlabelString,	xmstr,	
		   XmNsubMenuId,	smth_menubar,
		   XmNmenuHistory,	_lin_smth_pbW[0], 
		   XmNrightAttachment,	XmATTACH_FORM,
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftWidget,	smth_label,
		   NULL);
    XmStringFree (xmstr);

    XtManageChild (_lin_smth_optW);


/*
 *  Create label reminder
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
    
    XtVaCreateManagedWidget ("Type:  ",
		     xmLabelGadgetClass,     _labelReminder_formW,
                     XmNtopAttachment,       XmATTACH_WIDGET,
		     XmNtopWidget,	     labelReminder_label,
                     XmNtopOffset,           30,
		     XmNleftAttachment,	     XmATTACH_FORM,
		     NULL);
  
    _labelReminder_typTextW = 
    	     (Widget)XtVaCreateManagedWidget ("group type",
		     xmTextFieldWidgetClass, _labelReminder_formW,
	             XmNtopAttachment,       XmATTACH_WIDGET,
		     XmNtopWidget,	     labelReminder_label,
                     XmNtopOffset,           30,
		     XmNleftAttachment,	     XmATTACH_WIDGET,
		     XmNleftWidget,          labelReminder_label,
		     XmNcolumns,             13,
		     XmNeditable,	     False,
		     XmNcursorPositionVisible,	False,
		     NULL);

    XtManageChild(_labelReminder_textW);


/*
 *  Create label widget input
 */
    _label_formW = 
	XtVaCreateManagedWidget("_label_formW",
				xmFormWidgetClass,	pane,
				NULL);

    type_menu  = XtVaCreateManagedWidget ("type_menu",
	 xmRowColumnWidgetClass,	_label_formW,
	 XmNorientation,		XmVERTICAL,
	 XmNpacking,			XmPACK_TIGHT,
	 XmNnumColumns,			1,
	 NULL);

    label_form = XtVaCreateManagedWidget("_label_formW",
	 xmFormWidgetClass,		type_menu,
	 NULL);

    _label_menuW = XtVaCreateManagedWidget ("_label_menu",
	 xmRowColumnWidgetClass,	label_form, 
         XmNtopAttachment,              XmATTACH_FORM,
	 XmNorientation, 		XmHORIZONTAL,
	 NULL);

    _label_toggW = XtVaCreateManagedWidget("  ",
	 xmToggleButtonGadgetClass,	_label_menuW,
         XmNtraversalOn,                FALSE,
	 NULL);

    XtAddCallback(_label_toggW, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_labelToggCb, (XtPointer)LABEL_TOGGLE );

    XtVaCreateManagedWidget ("Label:",
	 xmLabelGadgetClass,		_label_menuW,
	 NULL); 

/* 
 *  Create a horizonal rowcolumn to hold two vertical rowcolumns:
 *  left:   hold two radio buttons for "Text" & "Symbol" choices.
 *  right:  hold two Optionmenus correponding with the radio buttons.
 */        
    _label_choiceW = XtVaCreateManagedWidget ("label_chioce",
	 xmRowColumnWidgetClass,	label_form, 
         XmNtopAttachment,              XmATTACH_WIDGET,
         XmNtopWidget,                  _label_toggW,
	 XmNorientation, 		XmHORIZONTAL,
	 XmNpacking,			XmPACK_TIGHT,
	 XmNnumColumns,			2,
	 XmNisHomogeneous,		FALSE,
	 NULL);

    radio_opt = XtVaCreateManagedWidget("radio_opt",
                xmRowColumnWidgetClass, _label_choiceW,
                XmNpacking,             XmPACK_TIGHT,
                XmNorientation,         XmVERTICAL,
		XmNradioBehavior,       True,
                XmNtraversalOn,         False,
                XmNmarginHeight,	5, 
                XmNmarginWidth,		18, 
                XmNspacing,         	18,
                NULL);

    for ( ii = 0; ii < 2; ii++ ){
	_label_radioW[ii] = XtVaCreateManagedWidget ( lblstr[ii],
                xmToggleButtonGadgetClass,	radio_opt,
                NULL);
        XtAddCallback(_label_radioW[ii], XmNarmCallback,
		      (XtCallbackProc)pgline_radioBoxCb, (XtPointer) ii);
    }
       
    pulldown_opt = XtVaCreateManagedWidget("pulldown_opt",
                xmRowColumnWidgetClass, _label_choiceW,
                XmNpacking,             XmPACK_TIGHT,
                XmNorientation,         XmVERTICAL,
                XmNtraversalOn,         False,
                XmNmarginHeight,	0, 
                XmNmarginWidth,		0, 
                XmNspacing,         	1,
                NULL);
    
    _label_submenuW = XtVaCreateManagedWidget("_lin_lab_submenu",
	 xmRowColumnWidgetClass,	pulldown_opt,
	 NULL);

    label_menubar = XmCreatePulldownMenu (_label_submenuW, 
					  "Label1", NULL, 0);

    _label_optW = XmCreateOptionMenu (_label_submenuW, 
					  "label1", NULL, 0);

    for (ii=0; ii < GRP_MXELE; ii++) {
	sprintf (cc, "xxxxxxxxx");
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
		      (XtCallbackProc)pgline_labelPbCb, (XtPointer) ii);
    }

    XtVaSetValues (_label_optW, 
	XmNsubMenuId,			label_menubar,
	XmNmenuHistory,			_label_pbW[0], 
	NULL);

    XtManageChild (_label_optW);

    
    _symb_submenuW = XtVaCreateManagedWidget("linlab_submenu",
	 xmRowColumnWidgetClass,	pulldown_opt,
	 NULL);

    _symb_menuW = XmCreatePulldownMenu (_symb_submenuW, 
					  "Label2", NULL, 0);
    _symb_optW = XmCreateOptionMenu (_symb_submenuW, 
					  "label2", NULL, 0);
    XtVaGetValues (pane,
	XmNforeground, 			&fg,
	XmNbackground, 			&bg,
	NULL);

    
    for ( ii = 1; ii < GRP_MXELE; ii++) {
        _symbID[ ii ] = -1; /* Initialize symb. IDs */
    }
    
    cfl_inqr ( symb_xbm, ICON_SYM_DIR, &ignore, filename, &iret );
    symb_pxm = XmGetPixmap(XtScreen(parent), filename, fg, bg );
    if ( symb_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
	sprintf( warning, "cannot load pixmap file %s", filename );
	NxmWarn_show(parent, warning);
	_symb_pbW[ii] = XtVaCreateManagedWidget("blank", 
		 xmPushButtonWidgetClass,	_symb_menuW,
		 NULL);
        for ( ii = 0; ii < GRP_MXELE; ii++) {
	    _symb_pbW[ii] = XtVaCreateManagedWidget("blank", 
		 xmPushButtonWidgetClass,	_symb_menuW,
		 NULL);	
	    XtAddCallback(_symb_pbW[ii], XmNactivateCallback,
		      (XtCallbackProc)pgline_symbPbCb, (XtPointer) ii);
 	}    
    }
    else {
        for ( ii = 0; ii < GRP_MXELE; ii++) {
	    _symb_pbW[ii] = XtVaCreateManagedWidget( symb_xbm,
		 xmPushButtonWidgetClass,	_symb_menuW,
		 XmNlabelType,			XmPIXMAP,
		 XmNlabelPixmap,		symb_pxm,
		 NULL);
	    XtAddCallback(_symb_pbW[ii], XmNactivateCallback,
		      (XtCallbackProc)pgline_symbPbCb, (XtPointer) ii);
	}        
    }

    XtVaSetValues (_symb_optW, 
	XmNsubMenuId,			_symb_menuW,
        XmNmenuHistory,			_symb_pbW[0],
	NULL);

    XtManageChild (_symb_optW);
    
    
/*
 *  Create use LINE color toggle
 */
    _label_menuW2 = XtVaCreateManagedWidget ("_label_color_menu",
	 xmRowColumnWidgetClass,	label_form, 
         XmNtopAttachment,              XmATTACH_WIDGET,
         XmNtopWidget,                  _label_choiceW,
	 XmNorientation, 		XmHORIZONTAL,
	 NULL);

    _label_toggW2 = XtVaCreateManagedWidget("  ",
	 xmToggleButtonGadgetClass,	_label_menuW2,
         XmNtraversalOn,                FALSE,
	 NULL);

    XtAddCallback(_label_toggW2, XmNvalueChangedCallback, 
		  (XtCallbackProc)pgline_labelToggCb, 
		  (XtPointer)USE_LINE_COLOR ); 

    XtVaCreateManagedWidget ("use LINE color",
	 xmLabelGadgetClass,		_label_menuW2,
	 NULL); 

    XmToggleButtonGadgetSetState (_label_toggW2, FALSE, TRUE);

    XtManageChild (_label_formW);

/*
 *  Create group type buttons
 */
    group_form = (Widget)XtVaCreateManagedWidget("_group_formW",
	 xmFormWidgetClass,		type_menu,
	 NULL);

/*
 *  Create group type option menu
 */
    ces_gtggrps(&_numGrp, &names, &iret);   
    if ( names != NULL )  free (names);

    pulldown = XmCreatePulldownMenu(group_form, "menuW", NULL, 0);
    _group_typeW = XmCreateOptionMenu(group_form, "option_menu", NULL, 0 ); 
    _group_buttonW = (WidgetList)XtMalloc((size_t)_numGrp * sizeof(Widget));

    for( ii = 0; ii < _numGrp; ii++ ) { 
        grptyp = ces_gtgmsid ( ii );
        ces_gtgnam (grptyp, grpnam, &iret);
        _group_buttonW[ii] = XtVaCreateManagedWidget(grpnam,
             xmPushButtonWidgetClass,		pulldown,
             NULL);

	XtAddCallback (_group_buttonW[ii], XmNactivateCallback, 
		       (XtCallbackProc)pgline_grpTypCb, (XtPointer)ii);

    }

    xmstr = XmStringCreateLocalized("Group Type");
    XtVaSetValues(_group_typeW,
	XmNsubMenuId, 		pulldown,
	XmNlabelString, 	xmstr, 
	NULL );
    XmStringFree(xmstr);

    XtManageChild(_group_typeW);


/*
 *  Create fill pattern input
 */

   _lin_patt_menuW = XtVaCreateManagedWidget ("_lin_patt_menu",
  	xmRowColumnWidgetClass,		pane, 
	NULL);

    line_patt_menu_bar  = XmCreatePulldownMenu (_lin_patt_menuW, 
		          "Patt", NULL, 0); 
    _lin_patt_menuW = XmCreateOptionMenu (_lin_patt_menuW,
		          "PattMenu", NULL, 0);

    xmstr = XmStringCreateLocalized ("Fill_pattern:"); 
    XtVaSetValues (_lin_patt_menuW,
        XmNlabelString,                 xmstr,
        NULL);
    XmStringFree (xmstr);

    jj = XtNumber(patt_xbm);

    _fillPattWid  = (WidgetList)XtMalloc((size_t)jj * sizeof(Widget));

    for (ii=0; ii<jj; ii++) {
	cfl_inqr(patt_xbm[ii], ICON_DIR, &ignore, filename, &iret );
	patt_pxm = XmGetPixmap(XtScreen(parent), filename, fg, bg );
	if ( patt_pxm == (Pixmap)XmUNSPECIFIED_PIXMAP ) {
	    sprintf( warning, "cannot load pixmap file %s", filename );
	    NxmWarn_show(parent, warning);
	    _fillPattWid[ii] = XtVaCreateManagedWidget(patt_xbm[ii], 
		 xmPushButtonWidgetClass,	line_patt_menu_bar,
		 NULL);
	}
	else {
	    _fillPattWid[ii] = XtVaCreateManagedWidget(patt_xbm[ii],
		 xmPushButtonWidgetClass,	line_patt_menu_bar,
		 XmNlabelType,			XmPIXMAP,
		 XmNlabelPixmap,		patt_pxm,
		 NULL);
	}

        XtAddCallback(_fillPattWid[ii], XmNactivateCallback,
		      (XtCallbackProc)pgline_fillPattCb, (XtPointer) ii);
    }

    XtVaSetValues (_lin_patt_menuW, 
	XmNsubMenuId,			line_patt_menu_bar,
        XmNmenuHistory,			_fillPattWid[1],
	NULL);

    XtSetSensitive(_lin_patt_menuW, False);

    XtManageChild(_lin_patt_menuW);

    _ctlBb = (Widget)XtVaCreateManagedWidget("_lin_ctl_bbW",
	xmBulletinBoardWidgetClass,	pane,
        XmNheight,                      50,
        XmNwidth,                       200,
	NULL);

    _ctlBtns = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));

    NxmCtlBtn_create (_ctlBb, 1, "ctlBtns", XtNumber(btnstr), btnstr,
							NULL, _ctlBtns);

    return;
}

/*=====================================================================*/

void pgline_popup ( int subtyp, int eltyp, int show_width, int show_size, 
		    int show_kpos, int show_ctl, XtCallbackProc callback,
		    char *label_ptr, int grptyp, Boolean init_grp )
/************************************************************************
 * pgline_popup								*
 *									*
 * This function shows a VG lines drawing attribute box.		*
 *									*
 * pgline_popup ( subtyp, eltyp, show_width, show_size, show_kpos,	*
 *	 	  show_ctl, callback, label_ptr, grptyp, init_grp )    	*
 *									*
 * Input parameters:							*
 *	subtyp	        int	GEMPAK subtype code			*
 *	eltyp		int	elemnet type 				*
 *	show_width	int	show width flag 			*
 *	show_size	int	show size flag 				*
 *	show_kpos	int	show kink position flag 		*
 *	show_ctl 	int	show control buttons flag		*
 *	callback	XtCallbackProc callback for Apply/Cancel btns 	*
 *      label_ptr       char*   pointer to the possible label string    *
 *	grptyp		int	group type (0 = none)			*
 *	init_grp	Boolean	re-initialize the group menu		*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * E. Wehner/EAi	7/97		Added scrollers for settings	*
 * E. Wehner/EAi	8/97		Get settings from CES		*
 * C. Lin/EAI	       10/97		Add line color			*
 * C. Lin/EAI	       10/97	Rename from NxmLinesSh, cleanup		*
 * E. Safford/GSC      11/97	add show_ctl flag & callback		*
 * F. J. Yen/NCEP	4/98	Updated with new ces function names	*
 * W. Li/EAI		05/98	Removed cmd_class , and stroke from 	*
 * 				CLASS_LINES				*
 * F. J. Yen/NCEP	05/98	Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	added scale updates			*
 * C. Lin/EAI		09/98	added smooth level			*
 * W. Li/EAI		02/99	added label type selections		*
 * S. Law/GSC		03/99	changed to use _setAttr better		*
 * S. Law/GSC		04/99	fixed XmToggleButtonGadgetSetState call	*
 * W. Li/EAI		05/99	added group type in line editor		*
 * E. Safford/GSC	06/99	add outlook label menu items		*
 * H. Zeng/EAI          10/99   fixed a problem on AIX4                 *
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * H. Zeng/EAI          06/00   added label color toggle initialization *
 * H. Zeng/EAI          07/00   added a new para. label_ptr             *
 * S. Law/GSC		08/00	moved setting width/size sensitivity	*
 * E. Safford/GSC	08/00	changed order of operation (fix aix bug)*
 * E. Safford/GSC	01/01	add group type display on edit		*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * H. Zeng/EAI          02/01   added call to pggrpw_initType()         *
 * H. Zeng/EAI          03/01   modified to use ces_gtgnam()            *
 * H. Zeng/EAI          03/01   modified to check group type id         *
 * E. Safford/GSC	03/01	add init_grp param, mod edit setup      *
 * E. Safford/SAIC	08/01	clean up				*
 * H. Zeng/EAI          09/01   revised for new GROUP functionality     *
 * J. Wu/SAIC		11/01   adjust kink line attributes		*
 * J. Wu/SAIC		11/01   disable	LABEL capability for kink lines	*
 * M. Li/SAIC		04/02	Added pglabel_setLabelPending		*
 * M. Li/SAIC		05/02	Check for pgpalw_isGrpAct		*
 * J. Wu/SAIC		08/02   revise for label line with symbols	*
 * T. Piper/SAIC	10/05	declared ii long			*
 ***********************************************************************/
{
    VG_DBStruct	*element;
    int		ier, group_type, iret;
    long	ii;
    char	grpname[20];
/*---------------------------------------------------------------------*/

    if (XtIsManaged (_lines_dlgW)) {
	XtUnmanageChild (_lines_dlgW);
    }


/*
 *  Initialize the label pending
 */
    pglabel_setLabelPending (False);


    if (show_ctl) {
	XtManageChild (_ctlBb);
	XtUnmanageChild (_label_formW);
        if (callback) {
	    for (ii=0; ii< 2; ii++) {
                XtRemoveAllCallbacks (_ctlBtns[ii], XmNactivateCallback);
  	        XtAddCallback (_ctlBtns[ii], XmNactivateCallback, 
			       callback, (XtPointer)ii);
	    }
	}
    }
    else {
	XtUnmanageChild (_ctlBb);
        XtManageChild (_label_formW);
    }

/*
 *  Get the settings for this element type
 */
    _subTyp  = subtyp;
    _vgType  = (char)eltyp;
    G_CALLOC(element, VG_DBStruct, _one, "pgline_popup:  VG_DBStruct");

    element->hdr.vg_type = _vgType;
    element->hdr.vg_class = CLASS_LINES;

    ces_get(_subTyp, element, &ier);

/*
 *  EDIT ONLY:  If there is a label string associated with the line,
 *  show it and the group type on the attribute window.  But if the 
 *  object is a kink line, do not show the label.
 */
    if ( show_ctl && grptyp > 0 ) {
        if ( _subTyp == 24 || _subTyp == 25 ) {	
	    XtUnmanageChild(_labelReminder_formW);
	}
	else {
	    XtManageChild(_labelReminder_formW);	
	}
	
	if ( label_ptr != NULL ) {
            XmTextSetString (_labelReminder_textW, label_ptr);
        }
	else {
            XmTextSetString (_labelReminder_textW, " ");
	}

        ces_gtgnam(grptyp, grpname, &iret);
        if( iret == 0 ) {
           XmTextSetString (_labelReminder_typTextW, grpname);
        }
        else {
           XmTextSetString (_labelReminder_typTextW, "UNKNOWN");
        }
      
    }
    else {
        if( XtIsManaged(_labelReminder_formW) ) {
            XtUnmanageChild(_labelReminder_formW);
       }
    }

/*
 *  Disable LABEL capability for kink lines
 */
    if ( _subTyp == 24 || _subTyp == 25 ) { 
        XtUnmanageChild (_label_formW); 
    }

    XtManageChild ( _lines_dlgW );

    XtSetSensitive(_lin_width_sldW, show_width);
    XtSetSensitive(_lin_width_txtW, show_width);
    XtSetSensitive(_lin_size_sldW, show_size);
    XtSetSensitive(_lin_size_txtW, show_size);
    XtSetSensitive(_kink_pos_sldW, show_kpos);
    XtSetSensitive(_kink_pos_txtW, show_kpos);

/*
 *  Set label color
 */
    XmToggleButtonGadgetSetState (_label_toggW2, (int)_labelColorFlag, TRUE);

/*
 *  Set group type. 
 */
    if ( init_grp ) {
       pgline_initType( element );
       group_type = _groupTyp;
    }
   
    XtSetSensitive(_label_choiceW, (int)_labelFlag);
    XtSetSensitive(_label_menuW2, (int)_labelFlag);
    if (pgpalw_isGrpActv()) {
	XtSetSensitive(_group_typeW, False);
    }
    else {
        XtSetSensitive(_group_typeW, (int)_labelFlag);
    }
    XmToggleButtonGadgetSetState (_label_toggW, (int)_labelFlag, TRUE);


/*
 *  Disable fill/close/smooth buttons for kink lines
 */
    if ( _subTyp == 24 || _subTyp == 25 ) { 
        XtSetSensitive(_lin_fillW, FALSE);    
        XtSetSensitive(_lin_closW, FALSE);    
        XtSetSensitive(_lin_smth_optW, FALSE);    
    }
    else {
        XtSetSensitive(_lin_fillW, TRUE);    
        XtSetSensitive(_lin_closW, TRUE);    
        XtSetSensitive(_lin_smth_optW, TRUE);    
    }

    if (element->hdr.vg_type == LINE_ELM) {
	pgline_setAttr (element->hdr.filled, 
			element->hdr.closed,
			element->hdr.maj_col, 
			element->elem.lin.info.width,
			(float)0, 50,  
			element->hdr.smooth );
    }
    else {
	if ( _subTyp == 24 || _subTyp == 25 ) { /* kink lines */
	pgline_setAttr (0, 
			0,
			element->hdr.maj_col, 
			element->elem.spl.info.splwid,
			element->elem.spl.info.splsiz,
			_attrKpos,
			0 );
        }
	else {
	    pgline_setAttr (element->hdr.filled, 
			element->hdr.closed,
			element->hdr.maj_col, 
			element->elem.spl.info.splwid,
			element->elem.spl.info.splsiz,
			50,
			element->hdr.smooth );
        }
    }
    G_FREE(element, VG_DBStruct);

/*
 *  Set label items at the end because line attrib. info. may changes 
 *  according to the label choice.
 */
    if ( init_grp ) {
        pgline_setLabItems(group_type);
    }

}

/*=====================================================================*/

void pgline_popdown ( void )
/************************************************************************
 * pgline_popdown							*
 *									*
 * This function unmanages the lines dialog box				*
 *									*
 * void pgline_popdown ( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *				NONE					*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 * E. Safford/GSC	10/97	Added NxmClrW_popdown call		*
 * S. Law/GSC		03/99	Added call to pgline_setTable		*
 * E. Safford/SAIC	12/01	move pgline_setTable inside if stmnt    *
 * E. Safford/SAIC	02/02	rm pgline_setTable                   	*
 * E. Safford/SAIC	03/02	add pgline_saveAttr()         		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    pgline_saveAttr();

    if ( XtIsManaged(_lines_dlgW) ) {
        NxmClrW_popdown();
    	XtUnmanageChild(_lines_dlgW);
    }
}

/*=====================================================================*/

void pgline_setAttr ( char fill, char close, int color, int width, 
					float size, int kpos, char smooth )
/************************************************************************
 * pgline_setAttr							*
 *									*
 * This function sets the values in the lines dialog box		*
 *									*
 * void pgline_setAttr (fill, close, color, width, size, kpos, smooth)	*
 *									*
 * Input parameters:                                                    *
 *	fill		char		fill flag			*
 *	close		char		close flag			*
 *	color		int		color value			*
 *	width		int		width value			*
 *	size		float		pattern size value		*
 *	kpos		int		kink position value		*
 *	smooth		char		smoothing level			*
 *                                      string                          *
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	11/97	initial coding				*
 * S. Law/GSC		04/98	Added smoothing level			*
 * W. Li/EAI		04/98	Added fill pattern			*
 * E. Safford/GSC	04/98	Set _attrSmth				*
 * F. J. Yen/NCEP	04/98	Updated with new ces function names	*
 * W. Li/EAI		04/98	Removed stroke from CLASS_LINES		*
 * F. J. Yen/NCEP	05/98	Renamed _gemType with _subTyp		*
 * W. Li/EAI		10/98	Fixed value setting problem		*
 * S. Law/GSC		03/99	Clean up				*
 * S. Law/GSC		04/99	fixed XmToggleButtonGadgetSetState call	*
 * S. Law/GSC		07/99	added defines for minimum scale values	*
 * M. Li/GSC		01/01	added check for line color setting	*
 * M. Li/GSC		02/01	added a check for the line/label color	*
 * H. Zeng/EAI          03/01   removed call to pggrpw_getGrpTypColr()  *
 * E. Safford/SAIC	08/01	clean up				*
 * J. Wu/SAIC		10/01	add kink position 			*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
    char	str[5];
    int		state;
/*---------------------------------------------------------------------*/
/* 
 * fill
 */
    _attrFill = (int)fill;

    if (_attrFill > 0) {
	_attrPatt = (_attrFill > 1) ? _attrFill - 2 : _attrFill;
    }

    XtVaSetValues (_lin_patt_menuW, 
        XmNmenuHistory, 	_fillPattWid[_attrPatt],
	NULL);

    state = ( fill == 0) ? FALSE : TRUE;  /* needed for linux2 */
    XmToggleButtonGadgetSetState (_lin_fillW, state, TRUE);

/*
 * close
 */
    _attrClos = (int)close;

    state = ((int) close == 0) ? FALSE : TRUE; /* needed for linux2 */
    XmToggleButtonGadgetSetState (_lin_closW, state, TRUE);

/*
 * color
 */
    _attrColr = color;

    XtVaSetValues(_lineColrW,
	XmNbackground,		NxmColrP_getColorPixel(_attrColr),
	XmNtopShadowColor,	NxmColrP_getColorPixel(_attrColr),
	XmNbottomShadowColor,	NxmColrP_getColorPixel(_attrColr),
	NULL);

/* 
 * width
 */
    if (XtIsSensitive (_lin_width_sldW)) { 
        if (MIN_WIDTH_SCALE <= width && width <= MAX_WIDTH_SCALE) {
	    sprintf (str, "%i", width);
            XmTextFieldSetString(_lin_width_txtW, str);
            XmScaleSetValue(_lin_width_sldW, width);

	    _attrWdth = width;
	}
    } 

/*
 * size
 */
    if (XtIsSensitive (_lin_size_sldW)) {
        if (MIN_SIZE_SCALE <= size && size <= MAX_SIZE_SCALE) {
	    sprintf (str, "%.1f", size);
            XmTextFieldSetString(_lin_size_txtW, "");
            XmTextFieldSetString(_lin_size_txtW, str);
            XmScaleSetValue(_lin_size_sldW, (int) (size * 10.0F));

	    _attrSize = size;
	}
    }

/*
 * kink position
 */
    if (XtIsSensitive (_kink_pos_sldW)) {
        if (MIN_KPOS_SCALE <= kpos && kpos <= MAX_KPOS_SCALE) {
	    sprintf (str, "%.2f", (float)kpos/100.0F);
            XmTextFieldSetString(_kink_pos_txtW, "");
            XmTextFieldSetString(_kink_pos_txtW, str);
            XmScaleSetValue(_kink_pos_sldW, kpos );

	    _attrKpos = kpos;
	}
    }

/*
 * smooth
 */
    _attrSmth = (int)smooth;   

    XtVaSetValues (_lin_smth_optW, 
		   XmNmenuHistory, _lin_smth_pbW[_attrSmth],
		   NULL);
}
 
/*=====================================================================*/

void pgline_setLabValue ( int which )
/************************************************************************
 * pgline_setLabValue							*
 *									*
 * put the selected label name into the text string 			*
 *									*
 * void pgline_setLabValue (which)					*
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
 * W. Li/EAI	        02/99   initial coding				*
 * W. Li/EAI	        02/99	added 3, 4, 5, 6 label types		*
 * W. Li/EAI	        03/99	changed value setting to XtVaGetValues	*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * M. Li/GSC		01/00	line color = label color		*
 * M. Li/GSC		01/01	set line color according to grptyp.tbl	*
 * H. Zeng/EAI          03/01   set line attrib. based on label choice  *
 * E. Safford/SAIC	08/01	use pgline_setLabelAttr()		*
 ***********************************************************************/
{
    int        	ier;
    char	*text;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    XtVaSetValues (_label_optW, 
	XmNmenuHistory,			_label_pbW[which], 
	NULL);

    XtVaGetValues(_label_pbW[which],
	    XmNlabelString, 		&xmstr,
	    NULL);    

    XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text);
    XmStringFree(xmstr);

    if (strcmp(text, "Other")!=0 ) {
        sprintf(_labelName, "%s", text);
    }
    else {
	sprintf(_labelName, "%s", "\0");
    }

    XtFree(text);

    pgline_setLabelAttr (&ier);

}

/*=====================================================================*/

Boolean pgline_isUp ( void )
/************************************************************************
 * pgline_isUp								*
 *									*
 * This function returns a boolean value specifying whether the lines	*
 * dialog is managed or not.						*
 *									*
 * Boolean pgline_isUp ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgline_isUp	Boolean		True -- up, 	False -- down 		*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 7/97						*
 ***********************************************************************/
{
    return (XtIsManaged(_lines_dlgW) );
}

/*=====================================================================*/

void pgline_getAttr ( signed char *fill, char *close, int *color, int *width,
			float *size, int *kpos, char *smooth, char *grptyp )
/************************************************************************
 * pgline_getAttr							*
 *									*
 * This function gets the line attributes.				*
 *									*
 * void pgline_getAttr (fill, close, color, width, size, kpos		*
 *                      smooth, grptyp)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*fill		signed char	fill flag			*
 *	*close		char		close flag			*
 *	*color		int		major color			*
 *	*width		int		width				*
 *	*size		float		size				*
 *	*kpos		int		kink position			*
 *	*smooth		char		smooth flag			*
 *	*grptyp		char		group type 			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	07/97						*
 * S. Law/GSC		04/98	Added smooth flag			*
 * W. Li/EAI		04/98	Added fill pattern			*
 * W. Li/EAI		06/98	Fixed pattern display problem		*
 * S. Law/GSC		03/99	Added color, width, and size parameters	*
 * E. Safford/GSC	03/01	added grptyp param			*
 * J. Wu/SAIC		10/01	add kpos param				*
 ***********************************************************************/
{
    *fill	= (char)_attrFill;
    *close	= (char)_attrClos;
    *color	= _attrColr;
    *width	= _attrWdth;
    *size	= _attrSize;
    *kpos	= _attrKpos;
    *smooth	= (char)_attrSmth;
    *grptyp     = (char)(( pgline_getLabFlag() ) ? _groupTyp : 0);
}

/*=====================================================================*/

void pgline_getColor ( int *colr )
/************************************************************************
 * pgline_getColor							*
 *									*
 * This function gets line color. 					*
 *									*
 * pgline_getColor( colr )						*
 *									*
 * Output parameters:							*
 * 	*colr	int	line color					*
 * 									*
 * Return value:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI	 	10/97						*
 ***********************************************************************/
{
    *colr = _attrColr; 
}

/*=====================================================================*/

Boolean pgline_getLabFlag ( void )
/************************************************************************
 * pgline_getLabFlag							*
 *									*
 * This function returns a boolean value specifying whether the lines	*
 * labelFlag is TRUE or FALSE. 						*
 *									*
 * Boolean pgline_getLabFlag( )						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgline_getLabFlag	Boolean	True -- use label,			*
 *				False -- do not use label 		*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI	02/99							*
 ***********************************************************************/
{
    return( _labelFlag );
}

/*=====================================================================*/

Boolean pgline_getLabColorFlag ( void )
/************************************************************************
 * pgline_getLabColorFlag						*
 *									*
 * This function returns a boolean value specifying whether the lines	*
 * label color flag is TRUE or FALSE. 					*
 *									*
 * Boolean pgline_getLabColorFlag( )					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * pgline_getLabColorFlag    Boolean  True -- use label,		*
 *				      False -- do not use label 	*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI	  06/00     initial coding				*
 ***********************************************************************/
{
    return( _labelColorFlag );
}

/*=====================================================================*/

void pgline_getLabValue ( char *label )
/************************************************************************
 * pgline_getLabValue							*
 *									*
 * This function gets the label text value 				*
 *									*
 * void pgline_getLabValue ( label)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*label		char		label value			*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 ***********************************************************************/
{
    strcpy(label, _labelName);
}


/*=====================================================================*/

char pgline_getGrptyp ( void )
/************************************************************************
 * pgline_getGrptyp							*
 *									*
 * This function returns the current grptyp from the attribute window.	*
 *									*
 * char pgline_getGrptyp ( )  	 					*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 * Return:								*
 * pgline_getGrptyp 	char	the current value of the grptyp		*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	04/01						*
 ***********************************************************************/
{
    char grptyp;
/*---------------------------------------------------------------------*/

    grptyp = (char)(( pgline_getLabFlag() ) ? _groupTyp : 0);
    return ( grptyp );
}

/*=====================================================================*/

void pgline_getAttrStr ( char *info )
/************************************************************************
 * pgline_getAttrStr							*
 *									*
 * This function gets a copy of _lineAttrStr string.			*
 *									*
 * void pgline_getAttrStr (info)					*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*info		char		copy of _lineAttrStr		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		03/01						*
 ***********************************************************************/
{
    sprintf(info, "%s", _lineAttrStr);
}

/*=====================================================================*/

void pgline_setAttrStr ( char *info )
/************************************************************************
 * pgline_setAttrStr							*
 *									*
 * This function sets _lineAttrStr string.			        *
 *									*
 * void pgline_setAttrStr (info)					*
 *									*
 * Input parameters:							*
 *	*info           char            pointer to new content.         *
 * Output parameters:							*
 *	                NULL                                            *
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		03/01						*
 ***********************************************************************/
{
    sprintf(_lineAttrStr, "%s", info);
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_fillCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_fillCb                                                   	*
 *                                                                      *
 * Callback for fill button widget.                                     *
 *                                                                      *
 * void pgline_fillCb( wdgt, clnt, call)				*
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt		Widget		Widget ID                       *
 *   clnt		XtPointer	not used                        *
 *   call		XtPointer	callback struct			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         7/97       Created                             *
 * C. Lin/EAI           10/97   Rename from NxmLinesFillCb, cleanup     *
 * W. Li/EAI		04/98	Modified for fill pattern		*
 ***********************************************************************/
{
    Boolean btnval;
/*---------------------------------------------------------------------*/

/*
 * check to see if the button is up or down and take appropriate actions.
 */
    XtVaGetValues(_lin_fillW, XmNset, &btnval, NULL);

    _attrFill = (btnval) ? _attrPatt + 2 : 0;

    XtSetSensitive(_lin_patt_menuW, (int) _attrFill);
    XtManageChild(_lin_patt_menuW); 
   
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_closeCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_closeCb                                                   	*
 *                                                                      *
 * Callback for close button widget.                                    *
 *                                                                      *
 * void pgline_closeCb( wdgt, clnt, call)      	                	*
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt		Widget		Widget ID                       *
 *   clnt		XtPointer	not used                        *
 *   call		XtPointer	callback struct     	        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         7/97       Created                             *
 * C. Lin/EAI           10/97   Rename from NxmLinesCloseCb, cleanup    *
 ***********************************************************************/
{
    Boolean btnval;
/*---------------------------------------------------------------------*/

/*
 * check to see if the button is up or down and take appropriate actions.
 */
    XtVaGetValues(_lin_closW, XmNset, &btnval, NULL);

    _attrClos = (btnval) ? 1 : 0;

}

/*=====================================================================*/
/* ARGSUSED */
void pgline_widthCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgline_widthCb                                                       *
 *                                                                      *
 * Callback for width scale widget.                                     *
 *                                                                      *
 * void pgline_widthCb( wdgt, clnt, call )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt		Widget          	Widget ID               *
 *   clnt		XtPointer       	not used                *
 *   call		XtPointer		callback struct         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         7/97       Created                             *
 * C. Lin/EAI           10/97   Rename from NxmLinesWidthCb, cleanup    *
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 * S. Law/GSC		03/99	removed call to pgline_setTable		*
 ***********************************************************************/
{
    int		linwid;		/* line width */
    char	txtstr[5];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    linwid = cbs->value;
    XmScaleSetValue(wdgt, linwid);
    sprintf(txtstr, "%i", linwid);
    XmTextFieldSetString(_lin_width_txtW, txtstr);

    if (cbs->reason == XmCR_VALUE_CHANGED) {
	_attrWdth = linwid;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_widTxtCb ( Widget wdgt, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgline_widTxtCb                                                      *
 *                                                                      *
 * Callback for line width text widget.                                 *
 *                                                                      *
 * void pgline_widTxtCb( wdgt, clnt, cbs )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt		Widget          	  Widget ID             *
 *   clnt		XtPointer       	  not used              *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         7/97       Created                             *
 * C. Lin/EAI           10/97   Rename from NxmLinesTxtWidCb, cleanup   *
 * F. J. Yen/NCEP	 4/98	Updated with new ces function names	*
 * F. J. Yen/NCEP	 5/98	Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	added event check and call to _setTable	*
 * W. Li/EAI		10/98	Fixed value change call back problem	*
 * S. Law/GSC		03/99	removed call to pgline_setTable		*
 * S. Law/GSC		07/99	added defines for minimum scale values	*
 ***********************************************************************/
{
    char	*ss;
    int		slval, linwid;
/*---------------------------------------------------------------------*/

/*
 * Confirm there is an event.  If not, this text has already been set up.
 */
    if (!cbs->event) 
	return;

/*
 * if the value on corresponding slider is different, set the sliders
 * value accordingly.
 */
    XmScaleGetValue(_lin_width_sldW, &slval);
    ss = XmTextFieldGetString(_lin_width_txtW);
    linwid = atoi(ss);
    XtFree(ss);

    if (MIN_WIDTH_SCALE <= linwid && linwid <= MAX_WIDTH_SCALE) {
        if (linwid != slval) {
            XmScaleSetValue(_lin_width_sldW, linwid);

	    _attrWdth = linwid;
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_sizeCb ( Widget wdgt, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgline_sizeCb                                                      	*
 *                                                                      *
 * Callback for pattern size scale widget.                              *
 *                                                                      *
 * void pgline_sizeCb( wdgt, clnt, cbs )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt              Widget           Widget ID                          *
 *   clnt	    XtPointer        not used                           *
 *   *cbs     XmScaleCallbackStruct  callback struct 		        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         7/97       Created                             *
 * C. Lin/EAI           10/97   Rename from NxmLinesSizeCb, cleanup   	*
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 * S. Law/GSC		03/99	removed call to pgline_setTable		*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
    float	linsiz;	/* line size */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    linsiz = (float)cbs->value / 10.0F;
    sprintf(txtstr, "%.1f", linsiz);
    XmTextFieldSetString(_lin_size_txtW, "");
    XmTextFieldSetString(_lin_size_txtW, txtstr);

    if (cbs->reason == XmCR_VALUE_CHANGED && _vgType == SPLN_ELM) {
	_attrSize = linsiz;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_sizTxtCb ( Widget wdgt, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgline_sizTxtCb                                                      *
 *                                                                      *
 * Callback for  pattern size text widget.                              *
 *                                                                      *
 * void pgline_sizTxtCb( wdgt, clnt, cbs )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt              Widget          	  Widget ID                     *
 *   clnt	    XtPointer       	  not used                      *
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         7/97       Created                             *
 * C. Lin/EAI           10/97   Rename from NxmLinesTxtSizCb, cleanup   *
 * F. J. Yen/NCEP	 4/98	Updated with new ces function names	*
 * F. J. Yen/NCEP	 5/98	Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	added event check and call to _setTable	*
 * W. Li/EAI		10/98	Fixed value change call back problem	*
 * S. Law/GSC		03/99	removed call to pgline_setTable		*
 * S. Law/GSC		07/99	added defines for minimum scale values	*
 ***********************************************************************/
{
    char        *ss;
    int         slval;
    float	linsiz;
/*---------------------------------------------------------------------*/

/*
 * Confirm there is an event.  If not, this text has already been set up.
 */
    if (!cbs->event) 
	return;

/*
 * if the value on corresponding slider is different, set the sliders
 * value accordingly.
 */
    XmScaleGetValue(_lin_size_sldW, &slval);
    ss = XmTextFieldGetString(_lin_size_txtW);
    linsiz = (float)(atof(ss));
    XtFree(ss);

    if (MIN_SIZE_SCALE <= linsiz && linsiz <= MAX_SIZE_SCALE) {
        if ( (int) (linsiz * 10.0F) != slval) {
            XmScaleSetValue(_lin_size_sldW, (int) (linsiz * 10.0F));

	    _attrSize = linsiz;
        }
    }
}

/*=====================================================================*/

void pgline_colorCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_colorCb							*
 *									*
 * Callback for color button widget.  Replaces original call to		*
 * NxmClrW_popup to insert a call to NxmClrW_setOkF, before passing	*
 * the information on to NxmClrW_popup.					*
 *									*
 * void pgline_colorCb (wdgt, clnt, call)				*
 *									*
 * Input parameters:							*
 *   wdgt		Widget			Widget ID		*
 *   clnt		XtPointer       	color			*
 *   call		XtPointer		callback struct		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		03/98	Initial coding				*
 * W. Li/EAI		10/98	Removed NxmClrW_setOkF			*
 ***********************************************************************/
{
    NxmClrW_popup (wdgt, (XtPointer)&_attrColr, call);
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_smthPbCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_smthPbCb							*
 *									*
 * This function is the callback function of line smooth level widget.	*
 *									*
 * void pgline_smthPbCb ( wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt		Widget			widget ID		*
 *	clnt		XtPointer		client data		*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	Created					*
 * F. J. Yen/NCEP       04/98   Updated with new ces function names     *
 * F. J. Yen/NCEP	05/98	Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	Added call to pgline_setTable		*
 * S. Law/GSC		03/99	Removed call to pgline_setTable		*
 ***********************************************************************/
{
    _attrSmth = (long)clnt;
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_labelPbCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_labelPbCb							*
 *									*
 * This function is the callback function of line label widget.		*
 *									*
 * void pgline_labelPbCb ( wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt		Widget			widget ID		*
 *	clnt		XtPointer		client data		*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 * S. Law/GSC		04/99	fixed XmToggleButtonGadgetSetState call	*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 ***********************************************************************/
{
    int which = (long)clnt;
/*---------------------------------------------------------------------*/

    pgline_setLabValue(which);
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_labelToggCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_labelToggCb                                                   *
 *                                                                      *
 * Callback for label toggle button widget.                             *
 *                                                                      *
 * void pgline_labelToggCb( wdgt, clnt, call)                  		*
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt		Widget          Widget ID			*
 *   clnt		XtPointer       0 = label toggle,		* 
 *					1 = "use label color" toggle	*
 *   call		XtPointer	callback struct                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		02/99						*
 * W. Li/EAI		05/99	added group type in line editor		*
 * E. Safford/GSC	06/99	mod for new label menu items		*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * H. Zeng/EAI          06/00   added label color toggle callback       *
 * M. Li/GSC		01/01	set line color button			*
 * H. Zeng/EAI          03/01   removed call to pggrpw_getGrpTypColr()  *
 * E. Safford/SAIC	08/01	clean up				*
 * M. Li/SAIC           05/02   Check for pgpalw_isGrpAct               *
 * J. Wu/SAIC           07/02   update for labeling with symbols        *
 ***********************************************************************/
{
    int     	ier, which_toggle = (long)clnt;
    Boolean 	btnval;
/*---------------------------------------------------------------------*/

/* 
 * Callback can be from either the label toggle or the 
 * "use label color" toggle.
 */
    XtVaGetValues(wdgt, XmNset, &btnval, NULL);

    if (which_toggle == LABEL_TOGGLE) {

        _labelFlag = btnval;
	XtSetSensitive(_label_choiceW, (int)_labelFlag);
	XtSetSensitive(_label_menuW2, (int)_labelFlag);
	if (pgpalw_isGrpActv()) {
            XtSetSensitive(_group_typeW, False);
    	}
        else {
            XtSetSensitive(_group_typeW, (int)_labelFlag);
        }

/*
 *  Update the line attributes for the group type & label 
 *  string if the label toggle has been switched on. 
 */
        if (_labelFlag) {
            pgline_setLabelAttr(&ier);
        }

    }
    else {
       _labelColorFlag = btnval;
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgline_fillPattCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_fillPattCb							*
 *									*
 * Callback for fill pattern widget.					*
 *									*
 * void pgline_fillPattCb ( wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt	Widget			Widget ID			*
 *	clnt	XtPointer		new pattern			*
 *	call	XtPointer		callback struct			*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		04/98	Initial coding				*
 * S. Law/GSC		03/99	changed how fill and patt attribs work	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
 
    _attrPatt = (long)clnt;

    if (_attrFill > 0) {
	_attrFill = _attrPatt + 2;
    }
}

/*=====================================================================*/

void pgline_saveAttr ( void )
/************************************************************************
 * pgline_saveAttr							*
 *									*
 * Saves the current element settings from the attribute window to the  *
 * set table								*
 *									*
 * void pgline_saveAttr ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	moved from various functions		*
 * W. Li/EAI		05/98	fixed value setting problem		*
 * S. Law/GSC		03/99	added fill, closed, color & use _attr	*
 *				variables instead of parameters		*
 * M. Li/GSC		02/00	removed arguments from this function	*
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * J. Wu/SAIC		10/01	add _attrKpos to splstr			*
 * E. Safford/SAIC	12/01	added pgutls_initHdr()			*
 * E. Safford/SAIC	02/02	rename from pgline_setTable 		*
 ***********************************************************************/
{
    VG_DBStruct	*element;
    int		loglev, ier, ier1;
    char	logstr[10], grp[4];
/*---------------------------------------------------------------------*/

    loglev = 2;
    strcpy(grp, "CES");

    G_CALLOC(element, VG_DBStruct, _one, "pgline_saveAttr:  VG_DBStruct");
    pgutls_initHdr ( &element->hdr );

    element->hdr.vg_type = _vgType;
    element->hdr.vg_class = CLASS_LINES;
    ces_get(_subTyp, element, &ier);

    if (ier  != 0) {
	sprintf(logstr, "%d ", _subTyp);
        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
	G_FREE(element, VG_DBStruct);
        return;
    }

    element->hdr.filled  = (char)_attrFill;
    element->hdr.closed  = (char)_attrClos;
    element->hdr.maj_col = _attrColr;
    element->hdr.smooth = (char)_attrSmth;

    if (_vgType == LINE_ELM) {
	element->elem.lin.info.width  = _attrWdth;
    }
    else {
	element->elem.spl.info.splwid = _attrWdth;
	element->elem.spl.info.splsiz = _attrSize;
	if ( _subTyp == 24 || _subTyp == 25 ) { /* kink lines */
	    element->elem.spl.info.splstr = _attrKpos;
        }	
    }

    if ( _groupTyp >= 0 ) {
        element->hdr.grptyp = (char)_groupTyp;
    }

    ces_set(_subTyp, element, &ier);
    if (ier  != 0) {
	sprintf(logstr, "%d ", _subTyp);

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
    }
    G_FREE(element, VG_DBStruct);    
}

/*=====================================================================*/

void pgline_setLabFlag ( Boolean lab_flag )
/************************************************************************
 * pgline_setLabFlag							*
 *									*
 * set the label flag 							*
 *									*
 * void pgline_setLabFlag (lab_flag)					*
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
 * W. Li/EAI		02/99						*
 * W. Li/EAI		05/99	added group type in line editor		*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * M. Li/SAIC           05/02   Check for pgpalw_isGrpAct               *
 ***********************************************************************/
{
    _labelFlag = lab_flag;

    XtSetSensitive(_label_submenuW, (int)_labelFlag);
    if (pgpalw_isGrpActv()) {
        XtSetSensitive(_group_typeW, False);
    }
    else {
        XtSetSensitive(_group_typeW, (int)_labelFlag);
    }

}

/*=====================================================================*/

void pgline_setLabItems ( int group_type )
/************************************************************************
 * pgline_setLabItems							*
 *									*
 * set the label item strings based on the group type and choose the    *
 * default label. 							*
 *									*
 * void pgline_setLabItems (group_type)					*
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
 * E. Safford/GSC	12/00	renamed the grp defines			*
 * H. Zeng/EAI          03/01   modified to use ces_gtglbls()           *
 * E. Safford/SAIC	04/02	handle empty label string		*
 * H. Zeng/EAI          04/02   modified to use cur_grptyp              *
 * J. Wu/SAIC           07/02   load symbols              		*
 ***********************************************************************/
{
    int		ii, which_label, obj_id, nlbl = 0, iret;
    int		ntxt, nsymb, tmp_id, tmp_cls; 
    char	*ptr, lbls[256], filename[256], xbm_name[32];
    long	ignore;
    XmString	xmstr;
    static int	cur_grptyp = IMP_CHOICE;
    Pixel	fg, bg;
    Pixmap	symb_pxm;
    Boolean	symbAvailable = False;
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
	
	XtVaGetValues ( XtParent(_symb_submenuW),
	    XmNforeground, 			&fg,
	    XmNbackground, 			&bg,
	    NULL);
        
/*
 *  If a label item matches one of the pgen symbol icon names, 
 *  (including classes SYMBOLS, MARKER, and COMSYM), treat
 *  it as an symbol item; otherwise, as a text item.
 */	
	ii = ntxt = nsymb = 0;        
	ptr = strtok(lbls, ";");
	while ( ptr != (char *)NULL && ii < nlbl ) {
	    sprintf ( xbm_name, "%s.xbm", ptr );
	    cfl_inqr ( xbm_name, ICON_SYM_DIR, &ignore, filename, &iret );
	    
	    tmp_cls = 0;
	    if ( iret == 0 ) {
	        tmp_id = pgpalw_getObjID ( ptr ); 
                tmp_cls = pgline_getSymbCls( tmp_id );
	    }
	    
	    if ( tmp_cls > 0 ) { /* Symbols */	        		    
		symbAvailable = True;
		_symbID[nsymb] = tmp_id;
		strcpy ( _symbName[nsymb], ptr );
		    
		symb_pxm = XmGetPixmap(XtScreen(XtParent(_symb_submenuW)), 
		                       filename, fg, bg );	        
	        XtVaSetValues(_symb_pbW[nsymb],
		     XmNlabelType,	XmPIXMAP,
		     XmNlabelPixmap,	symb_pxm,
	             NULL);
    	        XtManageChild(_symb_pbW[nsymb]);

		nsymb++;		
	    }
	    else {  /* Text */
	     	xmstr = XmStringCreateLocalized (ptr);	        
		XtVaSetValues(_label_pbW[ntxt],
	            XmNlabelString, 	xmstr,
	            XmNalignment,	XmALIGNMENT_CENTER,
	            NULL);
    	        XtManageChild(_label_pbW[ntxt]);
                XmStringFree (xmstr);	
	        ntxt++;
	    }
	    
	    ptr = strtok(NULL, ";" );
            ii++;        
	}


/*
 *  Unmanage those previous label items. If no symbols
 *  presented, use the blank as the first symbol item.  
 */
	for ( ii = ntxt; ii < GRP_MXELE; ii++) {
    	    XtUnmanageChild(_label_pbW[ii]);
        }

	for ( ii = nsymb; ii < GRP_MXELE; ii++) {
    	    _symbID[nsymb] = -1;
	    strcpy ( _symbName[nsymb], "\0" );
	    XtUnmanageChild(_symb_pbW[ii]);
        }
	
	if ( nsymb == 0 ) {
	    cfl_inqr ( "blank.xbm", ICON_SYM_DIR, &ignore, filename, &iret );
	    symb_pxm = XmGetPixmap(XtScreen(XtParent(_symb_submenuW)), 
		                   filename, fg, bg );	        
	    XtVaSetValues(_symb_pbW[nsymb],
		XmNlabelType,		XmPIXMAP,
		XmNlabelPixmap,		symb_pxm,
	        NULL);
    	    XtManageChild(_symb_pbW[nsymb]);		    
	}
	
	
/*
 *  Set the default choice for label type.
 */
	XtSetSensitive(_label_choiceW, (int)_labelFlag);
        XtSetSensitive(_label_radioW[1], (int)symbAvailable);
        XtSetSensitive(_symb_submenuW,   (int)symbAvailable);
	
	if ( ntxt == 1 && symbAvailable ) {
	    XmToggleButtonSetState(_label_radioW[1], True, True);
	}
	else {
	    XmToggleButtonSetState(_label_radioW[0], True, True);
	}
	
		
/*
 *  Set default label item
 */
        _currSymbID = _symbID[0];
        _currSymbName = _symbName[0];
        XtVaSetValues (_symb_optW, 
	    XmNmenuHistory,		_symb_pbW[0], 
	    NULL);
        
	which_label = 0;
	switch (obj_id){
            default:
                 if(group_type == 8) {

/*
 * When the group type is LABEL
 */
                    which_label = 2;
                 }
	         break;

        } /* the end of switch */
	
	pgline_setLabValue (which_label);        
    } 

}

/*=====================================================================*/

static void pgline_setLabelAttr ( int *iret )
/************************************************************************
 * pgline_setLabelAttr 							*
 *									*
 * Set the attribute window settings based on set group type and label. *
 *									*
 * static void pgline_setLabelAttr ( iret ) 				*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*iret		int	return code.  				*
 *				      0 = normal 			*
 *				     -1 = toggle button not on		*
 *				     -2 = no table entry for grp/label  *
 **									*
 * Log:									*
 * E. Safford/SAIC	08/01	initial coding           		*
 * J. Wu/SAIC		10/01	add param for pgline_SetAttr() 		*
 * J. Wu/SAIC		08/02	update for labeling with symbols	*
 ***********************************************************************/
{
    int		grptyp, color, width, smooth, fill, close, ier;
    char	*ptr, *ptr1, info[GRP_MXINFO];
    float	size;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

/*
 *  If the label toggle button is not on, quit w/ error code
 */
    if ( !XmToggleButtonGetState(_label_toggW) ) {
	*iret = -1;
	return;
    }


/*
 *  Set attribute info. according to the label item choice.
 */
    grptyp = _groupTyp;
    _lineAttrStr[0] = '\0';

    if ( pgline_getLabType() == 1 ) { /* TEXT */
        ces_gtginfo(grptyp, _labelName, _lineAttrStr, &ier);     
    }
    else { /* SYMBOLS */
        ces_gtginfo(grptyp, _currSymbName, _lineAttrStr, &ier);         
    }

    if ( (ier < 0) || (_lineAttrStr[0] == '\0') ) {
	*iret = -2;
	return;
    }


    strcpy (info, _lineAttrStr);
    ptr = strtok(info, "|");
     
    ptr1 = strtok(ptr, "/");
    sscanf(ptr1, "%d", &color);
    ptr1 = strtok(NULL, "/");
    sscanf(ptr1, "%d", &width);
    ptr1 = strtok(NULL, "/");
    sscanf(ptr1, "%f", &size);
    ptr1 = strtok(NULL, "/");
    sscanf(ptr1, "%d", &smooth);
    ptr1 = strtok(NULL, "/");
    sscanf(ptr1, "%d", &fill);
    ptr1 = strtok(NULL, "/");
    sscanf(ptr1, "%d", &close);

    if ( _subTyp == 24 || _subTyp == 25 ) {
        pgline_setAttr((char)fill, (char)close, color, width, 
                             size, _attrKpos, (char)smooth);
    }
    else {
        pgline_setAttr((char)fill, (char)close, color, width, 
                             size, 1, (char)smooth);
    }

}

/*=====================================================================*/
/* ARGSUSED */
void pgline_kposCb ( Widget wdgt, XtPointer clnt, 
					XmScaleCallbackStruct *cbs )
/************************************************************************
 * pgline_kposCb                                                      	*
 *                                                                      *
 * Callback for kink position scale widget.                             *
 *                                                                      *
 * void pgline_kposCb( wdgt, clnt, cbs )                     		*
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt		Widget           Widget ID                      *
 *   clnt		XtPointer        not used                       *
 *   *cbs     XmScaleCallbackStruct  callback struct 		        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/01	copy from pgline_widthCb()		*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
    int		kinkpos;	/* kink position */
    char	txtstr[5];
/*---------------------------------------------------------------------*/

    kinkpos = (cbs->value)/5*5;
    XmScaleSetValue(wdgt, kinkpos);
    sprintf(txtstr, "%.2f", (float)kinkpos/100.0F);
    XmTextFieldSetString(_kink_pos_txtW, "");
    XmTextFieldSetString(_kink_pos_txtW, txtstr);

    if (cbs->reason == XmCR_VALUE_CHANGED && _vgType == SPLN_ELM) {
	_attrKpos = kinkpos/5*5;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgline_kposTxtCb ( Widget wdgt, XtPointer clnt, 
					XmTextVerifyCallbackStruct *cbs )
/************************************************************************
 * pgline_kposTxtCb                                                     *
 *                                                                      *
 * Callback for kink position text widget.                              *
 *                                                                      *
 * void pgline_kposTxtCb( wdgt, clnt, cbs )		                *
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt              	Widget		Widget ID			* 
 *   clnt		XtPointer	not used			*
 *   *cbs     XmTextVerifyCallbackStruct  callback struct               *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		10/01	copy from pgline_widTxtCb()		*
 * J. Wu/SAIC		11/01	adjust min/max limit			*
 ***********************************************************************/
{
char    *ss;
int     slval;
int	kinkpos, kint, kflt;
double	dblkint;
/*---------------------------------------------------------------------*/

/*
 *  Confirm there is an event.  If not, this text has already been set up.
 */
    if (!cbs->event) 
	return;

/*
 *  If the value on corresponding slider is different, set the sliders
 *  value accordingly.
 */
    XmScaleGetValue(_kink_pos_sldW, &slval);
    ss = XmTextFieldGetString(_kink_pos_txtW);
    dblkint = floor(atof(ss)*10.0);    
    kint = (int)dblkint * 10;
    kflt = ( (atof(ss)*100. - (double)kint) < 5.0 ) ? 0 : 5;
    kinkpos = kint + kflt;    
    XtFree(ss);    

    if ( kinkpos < MIN_KPOS_SCALE ) kinkpos = MIN_KPOS_SCALE;
    if ( kinkpos > MAX_KPOS_SCALE ) kinkpos = MAX_KPOS_SCALE;
    
    if ( kinkpos != slval) {
        XmScaleSetValue(_kink_pos_sldW, kinkpos);
	_attrKpos = kinkpos;
    }
}

/*=====================================================================*/

/* ARGSUSED */
void pgline_grpTypCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_grpTypCb							*
 *									*
 * Callback function for group type option menu.			*
 *									*
 * void pgline_grpTypCb (wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		option button widget ID			*
 *	clnt	XtPointer	which button				*
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
    int which = (long)clnt;
/*---------------------------------------------------------------------*/

    _curGrpIdx = which;
    _groupTyp = ces_gtgmsid (_curGrpIdx);     
    pgline_setLabItems(_groupTyp);   

}

/*=====================================================================*/

void pgline_initType ( VG_DBStruct *el )
/************************************************************************
 * pgline_initType							*
 *									*
 * This function initiates group type menu option based on grptyp field *
 * of el.                                                               *
 *									*
 * void pgline_initType ( el )				                *
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
 * M. Li/SAIC		05/02	use active group type if GROUP is active*
 * H. Zeng/XTRIA	03/03   checked layer default group type	*
 ***********************************************************************/
{
    int		cur_layer, def_grp, ier, type_id = 0;
    Boolean	layer_flag;
/*---------------------------------------------------------------------*/

/*
 *  Check if there is a default group for the current layer.
 */
    cur_layer  = pglayer_getCurLayer();
    def_grp    = pglayer_getDefGrp(cur_layer);
    layer_flag = ces_getflag(_subTyp, el, &ier);

/*
 *  Initialize the group type based on a preference order.
 */ 
    if ( pgpalw_isGrpActv() ) {

/*
 *  If currently GROUP is active, use active group type.
 */
        _groupTyp  = pggrpw_getGrpType ();
        _curGrpIdx = ces_gtgavid ((char)_groupTyp);
    }
    else if ( pgpalw_isLayerActv() &&
              def_grp != NON_GRPID && 
              ier == 0             && 
              layer_flag              ) { 

             _groupTyp  = def_grp;
             _curGrpIdx = ces_gtgavid ((char)_groupTyp);
    }
    else {

        type_id   = (int)el->hdr.grptyp;

        if(type_id != NON_GRPID) {
           _groupTyp = type_id;
           _curGrpIdx = ces_gtgavid ((char)_groupTyp);
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

void pgline_updtGrpMenu ( int index )   
/************************************************************************
 * pgline_updtGrpMenu                                                   *
 *                                                                      *
 * This function updates the group type menu based on the input group	*
 * type index.								*
 *                                                                      *
 * void pgline_updtGrpMenu ( index )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *      index	int          	group type index			*
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
			XmNmenuHistory, _group_buttonW[index],
			NULL);
    pgline_grpTypCb (NULL, (XtPointer)&index, NULL);
}

/*=====================================================================*/

int pgline_getLabType ( void )   
/************************************************************************
 * pgline_getLabType                                                    *
 *                                                                      *
 * This function gets the selected label type (text or symbol)		*
 *                                                                      *
 * int pgline_getLabType ( void )                                    	*
 *                                                                      *
 * Input parameters:                                                    *
 *      NONE                                                    	*
 *                                              			*
 * Output parameters:                                                   *
 *      NONE                                                    	*
 *                                              			*
 * Return parameters:                                                   *
 *      pgline_getLabType	int          	label type 		*
 *                                              0 - Not set           	*
 *                                              1 - Text           	*
 *                                              2 - Symbol        	*
 **                                                               	*
 * Log:                                                                 *
 * J. Wu/SAIC           07/02	initial coding				*
 ***********************************************************************/
{
    int ii, lab_type;
/*---------------------------------------------------------------------*/
    
    lab_type = 0;
    for ( ii = 0; ii < 2; ii++ ) {
	if ( XmToggleButtonGetState(_label_radioW[ii]) ) {
	    lab_type = ii + 1;
	    break;
	}
    }
    
    return lab_type;
}
/*=====================================================================*/

/* ARGSUSED */
void pgline_radioBoxCb ( Widget wdgt, XtPointer clnt, XtPointer call)
/************************************************************************
 * pgline_radioBoxCb							*
 *									*
 * This function is the callback function of radio box widget.		*
 *									*
 * void pgline_radioBoxCb ( wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt		Widget			widget ID		*
 *	clnt		XtPointer		client data		*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          08/02   initial coding             		*
 ***********************************************************************/
{
    int ier, which = (long)clnt;
/*---------------------------------------------------------------------*/

    XmToggleButtonSetState ( _label_radioW[which], TRUE, TRUE );       
    pgline_setLabelAttr (&ier);

}

/*=====================================================================*/

/* ARGSUSED */
void pgline_symbPbCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgline_symbPbCb							*
 *									*
 * This function is the callback function of line label widget.		*
 *									*
 * void pgline_symbPbCb ( wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt		Widget			widget ID		*
 *	clnt		XtPointer		client data		*
 *	call		XtPointer		callback struct		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          07/02   initial coding             		*
 ***********************************************************************/
{
    int ier, symb_idx = (long)clnt;
/*---------------------------------------------------------------------*/

    _currSymbID = _symbID [symb_idx];
    _currSymbName = _symbName [symb_idx];
    
    XtVaSetValues (_symb_optW, 
	XmNmenuHistory,			_symb_pbW[symb_idx], 
	NULL);
    
    pgline_setLabelAttr (&ier);

}

/*=====================================================================*/

void pgline_getSymbInfo ( int *symb_cls, int *sym_id )   
/************************************************************************
 * pgline_getSymbInfo                                          		*
 *                                                                      *
 * This function gets the currently selected symbol's class and ID.	*
 *                                                                      *
 * int pgline_getSymbInfo ( symb_cls, sym_id ) 		              	*
 *                                                                      *
 * Input parameters:                                                    *
 *      NONE                                                    	*
 *                                              			*
 * Output parameters:                                                   *
 *      *symb_cls		int	symbol class                  	*
 *      *symb_id		int	symbol ID          		*
 *                                              			*
 * Return parameters:                                                   *
 *      none          					 		*
 *                                              		       	*
 **                                                               	*
 * Log:                                                                 *
 * J. Wu/SAIC           07/02	initial coding				*
 ***********************************************************************/
{
    *sym_id = _currSymbID;
    *symb_cls = pgline_getSymbCls ( *sym_id );
}

/*=====================================================================*/

int pgline_getSymbCls ( int symb_id )   
/************************************************************************
 * pgline_getSymbCls                                          		*
 *                                                                      *
 * This function gets a symbol's class associated with its ID.		*
 *                                                                      *
 * int pgline_getSymbCls ( symb_id )         				*
 *                                                                      *
 * Input parameters:                                                    *
 *      symb_id			int	symbol ID          		*
 *                                              			*
 * Output parameters:                                                   *
 *      none          					 		*
 *                                              			*
 * Return parameters:                                                   *
 *      pgline_getSymbCls()	int	symbol class                  	*
 *                                              		       	*
 **                                                               	*
 * Log:                                                                 *
 * J. Wu/SAIC           08/02	initial coding				*
 * m.gamazaychikov/SAIC 04/03	increased special symbols number to 49  *
 * m.gamazaychikov/SAIC 04/03	increased combo symbols number to 28	*
 * A. Hardy/NCEP	12/03	increased special symbols 49 -> 50	*
 * S. Jacobs/NCEP	 5/09	increased special symbols 50 -> 56	*
 ***********************************************************************/
{
    int symb_cls;
/*---------------------------------------------------------------------*/
    
    symb_cls = 0;
    
    if ( symb_id >= OBJ_CSYMB01 && symb_id <= OBJ_CSYMB28 ) {
        symb_cls = CLASS_COMSYM;	
    }
    else if ( symb_id >= OBJ_MARK1 && symb_id <= OBJ_MARK22) {
        symb_cls = CLASS_MARKER;	    
    }
    else if ( ( symb_id >= OBJ_CLOUD01 && symb_id <= OBJ_CLOUD29 )   ||
              ( symb_id >= OBJ_PSTWX03 && symb_id <= OBJ_PSTWX09 )   ||
              ( symb_id >= OBJ_PTEND00 && symb_id <= OBJ_PTEND08 )   ||
              ( symb_id >= OBJ_SKY00 && symb_id <= OBJ_SKY10 )       ||
              ( symb_id >= OBJ_ICE00 && symb_id <= OBJ_ICE10 )       ||
              ( symb_id >= OBJ_SPSYM00 && symb_id <= OBJ_SPSYM56 )   ||
              ( symb_id >= OBJ_TURB00 && symb_id <= OBJ_TURB08 )     ||
              ( symb_id >= OBJ_WXSYM000 && symb_id <= OBJ_WXSYM099 ) ||     
              ( symb_id >= OBJ_WXSYM201 && symb_id <= OBJ_WXSYM203 ) ||     
              ( symb_id == OBJ_WXSYM107 )  ) {    
        symb_cls = CLASS_SYMBOLS;	            
    }
    
    return symb_cls;        
}
