#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "Nxm.h"
#include "drwids.h"
#include "vgstruct.h"

#define MIN_SIZ_SCALE		0.1F
#define MAX_SIZ_SCALE		10
#define MIN_HDZ_SCALE		0.1F
#define MAX_HDZ_SCALE		10
#define MIN_DIR_SCALE		0
#define MAX_DIR_SCALE		360
#define MIN_WIDTH_SCALE		1
#define MAX_WIDTH_SCALE		10
#define MIN_SPD_SCALE		0
#define MAX_SPD_SCALE		400
#define MAX_ARRW_SPD_SCALE	200
#define FORM_OFFSET_SIZE	8

#define DIR			0
#define SPD			1
#define SIZE			2
#define WIDTH			3
#define A_SIZ			4
#define COLOR			5

#define VALUE			0
#define INCREMENT		1
#define NUM_BTS			6

/* 
 * Declare local widgets for the vector editor
 */
static Widget	_wnd_dlgW;
static Widget	_clearRcW;
static Widget	_formsRcW;
static Widget	_ctlFormW;
static Widget	_clorButW;

static WidgetList  _clearBtnW;
static WidgetList  _ctlBtnsW;
static WidgetList  _wnd_sldW;
static WidgetList  _wnd_txtW;
static WidgetList  _actionBtnW;
static WidgetList  _attrbEdit;

static float	_wndDir[4] = {0.0F, 0.0F, 0.0F, 0.0F};
static float	_wndSpd[4] = {100.0F, 10.0F, 0.0F, 0.0F};
static float    _dirInc[4] = {0.0F, 0.0F, 0.0F, 0.0F};
static float    _spdInc[4] = {0.0F, 0.0F, 0.0F, 0.0F};
static float	_wndSiz    = 0.0F;
static int	_wndWidth  = 0;
static float	_wndHdz    = 0.0F;

static int	_subTyp   = -1;
static int	_vgType   = BARB_ELM;
static int	_wndColor = 25;
static int	_wndType  = 112;

static int	_action  = VALUE;

static Boolean   _editFlags[NUM_BTS];
/*
 * callback functions
 */
void pgwndw_clearCb (   Widget, XtPointer, XtPointer );
void pgwndw_actionCb (  Widget, XtPointer, XtPointer );
void pgwndw_colorCb (   Widget, XtPointer, XtPointer );
void pgwndw_dirCb (     Widget, XtPointer, XtPointer );
void pgwndw_dirTxtCb (  Widget, XtPointer, XtPointer );
void pgwndw_hdzCb (     Widget, XtPointer, XtPointer );
void pgwndw_hdzTxtCb (  Widget, XtPointer, XtPointer );
void pgwndw_sizeCb (    Widget, XtPointer, XtPointer );
void pgwndw_sizTxtCb (  Widget, XtPointer, XtPointer );
void pgwndw_spdCb (     Widget, XtPointer, XtPointer );
void pgwndw_spdTxtCb (  Widget, XtPointer, XtPointer );
void pgwndw_widthCb (   Widget, XtPointer, XtPointer );
void pgwndw_widthTxtCb( Widget, XtPointer, XtPointer );
void pgwndw_attrbEditCb(Widget, long, XtPointer );

/*
 *  private functions
 */
void pgwndw_draw (     void );
void pgwndw_getwtype ( int vg_type, int *wtype );
void pgwndw_setTable (  float size, float hdsize, int width, int wndtype );
void pgwndw_initAttrEdit ( void );


/************************************************************************
 * nmap_pgwndw.c							*
 *									*
 * This module creates and displays the vector attribute editing window.*
 *									*
 * CONTENTS:								*
 *  pgwndw_create()	creates vector attribute editing window		*
 *  pgwndw_popup()	popup vector attribute editing window		*
 *  pgwndw_popdown()	pop down vector attribute editing window	*
 *  pgwndw_setAttr()	set the vector widget attributes		*
 *  pgwndw_setColr()	set the vector widget color			*
 *  pgwndw_saveColor()	save the new vector color			*
 *									*
 *  pgwndw_isUp()	check whether attribute editing window is up	*
 *  pgwndw_getData()	get vector related data				*
 *  pgwndw_getEditFlag()get multiple selection attribute edit flags	*
 *									*
 *  pgwndw_dirCb()	callback for direction scale widget		*
 *  pgwndw_clearCb()	callback for clear button widget		*
 *  pgwndw_dirTxtCb()	callback for direction text widget		*
 *  pgwndw_spdCb()	callback for speed scale widget			*
 *  pgwndw_spdTxtCb()	callback for speed text widget			*
 *  pgwndw_sizeCb()	callback for size scale widget			*
 *  pgwndw_sizTxtCb()	callback for size text widget			*
 *  pgwndw_widthCb()	callback for barb width scale widget		*
 *  pgwndw_widthTxtCb()	callback for barb thickness text widget		*
 *  pgwndw_hdzCb()	callback for arrow head size scale widget	*
 *  pgwndw_hdzTxtCb()	callback for arrow head size text widget	*
 *  pgwndw_colorCb()	callback for color button			*
 *  pgwndw_attrbEditCb()  callback for attribute editing		* 
 *									*
 *  pgwndw_draw()	setup the mouse event handler for drawing	*
 *  pgwndw_setTable()	places element into set table			*
 *  pgwndw_getwtype()   internal function  to get widget type 		*
 *  pgwndw_initAttrEdit() set toggle buttons of edit window 		* 
 ***********************************************************************/

/*=====================================================================*/

void pgwndw_create ( Widget parent )
/************************************************************************
 * pgwndw_create     							*
 *                                                                      *
 * Create a Wind editor Dialog Box.					*
 *                                                                      *
 * void pgwndw_create  ( parent )                           		*
 *                                                                      *
 * Input parameters:                                                    *
 *      parent       Widget          parent widget ID     		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E.Wehner/EAI		03/97	Created                         	*
 * E. Wehner/EAi	08/97	Don't return widget id			*
 * C. Lin/EAI		10/97	rename from NxmWindCr, cleanup		*
 * E. Safford/GSC	12/97	added apply/cancel buttons for editing  *
 * E. Safford/GSC	12/97	standardize labels, change init dir to 0*
 * A. Hardy/GSC		02/98	fixed scale bar, goes left to right	*
 * S. Law/GSC		03/98	Added color form, cleanup		*
 * S. Law/GSC		04/98	Modified so that dir_sld uses 5 deg step*
 * F. J. Yen/NCEP       04/98   Updated with new ces function names     *
 * W. Li/EAI		05/98	Cleanup					*
 * I. Durham/GSC	05/98	Changed underscore decl. to an include	*
 * W. Li/EAI		09/98	Added clear type for vect		*
 * W. Li/EAI		10/98	combined widgets to widget list		*
 * E. Safford/GSC	12/98	remove XtSetSensitive slider calls	*
 * E. Safford/GSC	01/01	move window location to resource file	*
 * T. Piper/SAIC	11/01	freed scl_txt_forms			*
 * J. Wu/SAIC		05/02	verify dir/speed/size/width/head size	*
 * T. Piper/SAIC        12/02   radio box -> check box                  *
 * T. Piper/SAIC	10/05	declared ii & nn long			*
 * M. Li/SACI		09/06	added 'Value'/'Incremenr' ratio buttons	*
 * M. Li/SACI		09/07	added multi-selection buttons		*
 ***********************************************************************/
{
    Widget	pane, clear_form, radio_box, action_form;
    WidgetList  scl_txt_forms;
    XmString	xmstr;

    char	*btnstr[] = {"Apply", "Cancel"};
    char	*clea_type[] = {"On", "Off"}; 
    char	*action_type[] = {"Value", "Increment"}; 
    char	*form_type[] = {"dir_form", "spd_form", "size_form", 
				"width_form", "hdz_form"};
    char	*titl_strs[] = {"Direction","Speed (m/s)", "Size","Width", 
			 	"Arrowhead Size"};
    char	*wnd_sld_type[] = {"Direction", "Wind_speed", "Size", 
				   "Width", "Arrowhead_size"};
    char	*wnd_txt_type[] = {"dirtxt", "spdtxt", "siztxt", 
				   "widthtxt", "hdztxt"};
    long	ii, nn;
    int		int_scl_value[] = {1, 10, 1,  1, 1 }; 
    int		scale_multipl[] = {2, 5,  10, 1, 10};
    int		text_decimalP[] = {0, 0,  1,  0, 0 };
    int		scl_max[]= {MAX_DIR_SCALE, MAX_SPD_SCALE, MAX_SIZ_SCALE,
			    MAX_WIDTH_SCALE, MAX_HDZ_SCALE};
    
    int		scl_min[]= {MIN_DIR_SCALE, MIN_SPD_SCALE, 1, 
			    MIN_WIDTH_SCALE, 1};
    int		scl_mdf[]= {2, 10, 100, 10, 100};
    XtCallbackProc (scl_cb[]) = {pgwndw_dirCb, pgwndw_spdCb, 
			         pgwndw_sizeCb,pgwndw_widthCb,
			         pgwndw_hdzCb};
    XtCallbackProc (txt_cb[]) = {pgwndw_dirTxtCb, pgwndw_spdTxtCb, 
			         pgwndw_sizTxtCb, pgwndw_widthTxtCb, 
			         pgwndw_hdzTxtCb} ;
    XtCallbackProc (vrfy_cb[]) = {pgutls_vrfyPosIntCb, pgutls_vrfyPosIntCb, 
			         pgutls_vrfyPosFltCb, pgutls_vrfyPosIntCb, 
			         pgutls_vrfyPosFltCb} ;
/*---------------------------------------------------------------------*/
/* create the object editor dialog box */
    
    _wnd_dlgW = XmCreateFormDialog(parent, "wind_edit", NULL, 0);

    XtVaSetValues(_wnd_dlgW,
  		XmNdefaultPosition,             False,
		XmNnoResize,                    True,
		XmNautoUnmanage,                FALSE,
		NULL);
/* 
 * create a pane window for all objects input
 */
    pane  = XtVaCreateManagedWidget ("pane",
                xmPanedWindowWidgetClass,       _wnd_dlgW,
                XmNsashWidth,                   1,
                XmNsashHeight,                  1,
                NULL); 
/* 
 * create a form for object clear and color botton input
 */
    clear_form = (Widget)XtVaCreateManagedWidget ("clear_form",
		xmFormWidgetClass,		pane,
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL);
/* 
 * create a label for object clear
 */
    XtVaCreateManagedWidget ("Clear:",
		xmLabelGadgetClass,		clear_form,
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNtopOffset,			FORM_OFFSET_SIZE,
		XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  20,
		NULL);
/* 
 * create row column widget for object clear input
 */
    _clearRcW   =  XtVaCreateWidget ("_clearRowColW",
		xmRowColumnWidgetClass,		clear_form, 
                XmNorientation,			XmHORIZONTAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			2,
		XmNradioBehavior,       	False,
		XmNtraversalOn,         	False,
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNleftOffset,			80,	
		NULL);
 
    nn = XtNumber(clea_type);
    _clearBtnW = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){
	_clearBtnW[ii] = XtVaCreateManagedWidget (clea_type[ii],
		xmToggleButtonGadgetClass,	_clearRcW,
		NULL);
	XtAddCallback(_clearBtnW[ii], XmNvalueChangedCallback,
		      (XtCallbackProc)pgwndw_clearCb, (XtPointer)ii );
    }
    XtManageChild(_clearRcW);

/* 
 * create object color input
 */
    _clorButW   =  XtVaCreateManagedWidget (" ",
		xmPushButtonWidgetClass,	clear_form, 
  		XmNresize,			FALSE,
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNtopOffset,			FORM_OFFSET_SIZE,
		XmNwidth,			25,	
                XmNheight,                      20,
		NULL);
    XtAddCallback(_clorButW, XmNactivateCallback, 
		  (XtCallbackProc)pgwndw_colorCb, (XtPointer)(&_wndColor));

/* 
 * create row column widget for object dir, speed, size, 
 * width, arrow size input
 */
    _formsRcW = XtVaCreateWidget ("_formsRowColW",
		xmRowColumnWidgetClass,		pane, 
                XmNorientation,			XmVERTICAL,
		XmNpacking,			XmPACK_TIGHT,
		XmNnumColumns,			1,
		XmNtopAttachment, 		XmATTACH_WIDGET,
		XmNtopWidget,			clear_form,
		XmNtopOffset,			FORM_OFFSET_SIZE,
		XmNleftAttachment,		XmATTACH_FORM,
		XmNrightAttachment,		XmATTACH_FORM,
		NULL);

/* 
 * create a form for action radio box 
 */
    action_form = (Widget)XtVaCreateManagedWidget ("action_form",
                xmFormWidgetClass,              _formsRcW,
		XmNtopWidget,                   clear_form,
                XmNtopOffset,                   FORM_OFFSET_SIZE,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNrightAttachment,             XmATTACH_FORM,
                NULL);

/* 
 * create radio box for action. 
 */
    radio_box  =  XtVaCreateManagedWidget ("_actionRowColW",
                xmRowColumnWidgetClass,         action_form,
                XmNorientation,                 XmHORIZONTAL,
                XmNpacking,                     XmPACK_TIGHT,
                XmNnumColumns,                  2,
                XmNradioBehavior,               True,
                XmNtraversalOn,                 False,
                XmNtopAttachment,               XmATTACH_FORM,
                XmNleftAttachment,              XmATTACH_FORM,
                XmNleftOffset,                  25,
                NULL);

    nn = XtNumber(action_type);
    _actionBtnW = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){
        _actionBtnW[ii] = XtVaCreateManagedWidget (action_type[ii],
                xmToggleButtonGadgetClass,      radio_box,
                NULL);

        XtAddCallback(_actionBtnW[ii], XmNvalueChangedCallback,
                      (XtCallbackProc)pgwndw_actionCb, (XtPointer)ii );
    }

    XtSetSensitive( _actionBtnW[1], False );

/* 
 * create form widgets for object dir, speed, size, 
 * width, arrow size input
 */
    nn = XtNumber(form_type);
    scl_txt_forms  = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){
	scl_txt_forms[ii] = XtVaCreateManagedWidget (form_type[ii],
		xmFormWidgetClass,	_formsRcW,
		NULL);
    }
    XtManageChild(_formsRcW);

/*
 * Create toggle buttons on each editing form for editing selection
 */
    nn = NUM_BTS;
    _attrbEdit = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){

	if ( ii == COLOR ) {
	    _attrbEdit[ii] = XtVaCreateManagedWidget (" ",
                xmToggleButtonGadgetClass,      clear_form,
		XmNtopAttachment,               XmATTACH_FORM,
                XmNtopOffset,                   FORM_OFFSET_SIZE/2,
                XmNleftAttachment,              XmATTACH_WIDGET,
                XmNleftWidget,                	_clearBtnW[1],
                NULL);
	} else {
            _attrbEdit[ii] = XtVaCreateManagedWidget (" ",
                xmToggleButtonGadgetClass,      scl_txt_forms[ii],
                XmNtraversalOn,                 False,
                XmNtopAttachment,               XmATTACH_FORM,
		XmNleftAttachment,             XmATTACH_FORM,
                NULL);
	}

	 if ( ii == DIR ) {
	     XmToggleButtonSetState (_attrbEdit[ii], True, True );
	     _editFlags[ii] = True;
	 } else {
	     _editFlags[ii] = False;
	 }
	
         XtAddCallback(_attrbEdit[ii], XmNvalueChangedCallback,
                       (XtCallbackProc)pgwndw_attrbEditCb, (XtPointer)(ii));
    }

/* 
 * create scale widgets for object dir, speed, size, 
 * width, arrow size input
 */
    nn = XtNumber(wnd_sld_type);
    _wnd_sldW  = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){
	_wnd_sldW[ii] = (Widget)XmCreateScale (scl_txt_forms[ii], 
			 wnd_sld_type[ii], NULL, 0); 
    }

/* 
 * create Text widgets for object dir, speed, size, 
 * width, arrow size input
 */
    nn = XtNumber(wnd_txt_type);
    _wnd_txtW  = (WidgetList) XtMalloc( nn * sizeof(Widget));

    for (ii=0; ii<nn; ii++){
	_wnd_txtW[ii] = (Widget) XtVaCreateManagedWidget(wnd_txt_type[ii],
		xmTextFieldWidgetClass,         scl_txt_forms[ii], 
		XmNcolumns,                     4,
		XmNcursorPositionVisible,       True,
		XmNrightAttachment,             XmATTACH_FORM,
		XmNrightOffset,                 FORM_OFFSET_SIZE,
 		NULL); 
    }

/* 
 * value setting and call back for scale widget of object dir, speed, size, 
 * width, arrow size
 */
    nn = XtNumber(wnd_sld_type);
    for (ii=0; ii<nn; ++ii){
	xmstr = XmStringCreateLocalized(titl_strs[ii]);
	XtVaSetValues( _wnd_sldW[ii],
		XmNorientation, 		XmHORIZONTAL,
		XmNscaleWidth,                  150,
		XmNmaximum, 			scl_max[ii]*scl_mdf[ii]/10,
		XmNminimum, 			scl_min[ii],
 		XmNprocessingDirection, 	XmMAX_ON_RIGHT,
		XmNvalue, 			int_scl_value[ii],
		XmNscaleMultiple, 		scale_multipl[ii],
		XmNdecimalPoints, 		text_decimalP[ii],
		XmNshowValue, 			False,
		XmNtitleString, 		xmstr, 
		XmNtopAttachment, 		XmATTACH_FORM,
		XmNleftAttachment,		XmATTACH_WIDGET,
		XmNleftWidget,		        _attrbEdit[ii],
		XmNrightAttachment,		XmATTACH_WIDGET,
		XmNrightWidget,			_wnd_txtW[ii],
		NULL);
	XmStringFree(xmstr);
	if ( ii > 1 ) 
	    XtAddCallback(_wnd_txtW[ii], XmNmodifyVerifyCallback, (XtCallbackProc)vrfy_cb[ii], NULL);
	XtAddCallback(_wnd_txtW[ii], XmNvalueChangedCallback, (XtCallbackProc)txt_cb[ii], NULL);
	XtAddCallback(_wnd_sldW[ii], XmNvalueChangedCallback, (XtCallbackProc)scl_cb[ii],  NULL);
	XtAddCallback(_wnd_sldW[ii], XmNdragCallback, (XtCallbackProc)scl_cb[ii],  NULL);
	XtManageChild( _wnd_sldW[ii]);
    }

/* 
 * create control input
 */
    _ctlFormW  = (Widget)XtVaCreateManagedWidget("_wnd_ctl_formW",
                xmFormWidgetClass,		pane,
  		XmNtopAttachment, 		XmATTACH_WIDGET,
		XmNtopWidget,			scl_txt_forms[4],
		XmNtopOffset,			FORM_OFFSET_SIZE,
                XmNleftAttachment,		XmATTACH_FORM,
                XmNrightAttachment,		XmATTACH_FORM,
                XmNbottomAttachment,		XmATTACH_FORM,
		XmNbottomOffset,		10,
                NULL);
     XtFree((XtPointer)scl_txt_forms); 
    _ctlBtnsW = (WidgetList)XtMalloc(XtNumber(btnstr) * sizeof(Widget));
    NxmCtlBtn_create (_ctlFormW, 1, "ctlBtns", XtNumber(btnstr), btnstr,
                      NULL, _ctlBtnsW);
    return;
}

/*=====================================================================*/

void pgwndw_popup ( int wind_type, int show_ctl, XtCallbackProc callback )
/************************************************************************
 * pgwndw_popup     							*
 *                                                                      *
 * Show the Wind editor Dialog Box.					*
 *                                                                      *
 * void pgwndw_popup(wind_type, show_ctl, callback)			*
 *                                                                      *
 * Input parameters:                                                    *
 *	wind_type	int	OBJ_WINDARRW				*
 *				     OBJ_WINDBARB			*
 *				     OBJ_WINDDARR			*
 *				     OBJ_WINDHASH			*
 *      show_ctl	int	control flag for accept/cancel		*
 *	callback	XtCallbackProc	callback for accept/cancel	*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E.Wehner/EAI	03/97	Created                         		*
 * C.Lin/EAI		10/97	Rename from NxmWindSh, cleanup  	*
 * C.Lin/EAI		11/97	Add retrieving settings  		*
 * E. Safford/GSC	12/97	Add params for attribute editing	*
 * E. Safford/GSC	12/97	modify labels and value width		*
 * S. Law/GSC		03/98	Added set attributes, cleanup		*
 * S. Law/GSC		04/98	Updated _wndSpd to array		*
 * W. Li/EAI		04/98	Add darr and hash in CLASS_WINS		*
 * F. J. Yen/NCEP	05/98   Renamed _gemType with _subTyp		*
 * W. Li/EAI		05/98	Add title for edit window and cleanup	*
 * W. Li/EAI		09/98	Added clear type for vect		*
 * W. Li/EAI		09/98	Updated _wndDir to array		*
 * W. Li/EAI		02/99	set the keyboard focus to wind speed	*
 * H. Zeng/EAI		02/00   made changes to the titles              *
 * T. Piper/SAIC	10/05	declared ii long			*
 * M. Li/SAIC	 	09/06	Add INCREMENT action			*
 * M. Li/SAIC	 	09/07	Add pgwndw_initAttrEdit			* 
 ***********************************************************************/
{
    int			ier;
    long		ii;
    XmString		xmstr, title_str;
    VG_DBStruct		el;
    Boolean 		show_speed, show_hsize;
    Widget		drawingw;
/*---------------------------------------------------------------------*/

    if  (XtIsManaged( _wnd_dlgW)) {
        XtUnmanageChild(_wnd_dlgW);
    }

    _subTyp   = -1;

/* 
 * setup attributes 
 */

    switch (wind_type) {

      case OBJ_WINDBARB:

	el.hdr.vg_type = _vgType = BARB_ELM;
	el.hdr.vg_class = CLASS_WINDS;

	ces_get(_subTyp, &el, &ier);
	pgwndw_setAttr( _wndDir[0], _wndSpd[0], 
		        el.elem.wnd.info.size, 
		        el.elem.wnd.info.width, 
		        el.elem.wnd.info.hdsiz,
			el.elem.wnd.info.wndtyp);
	title_str = XmStringCreateLocalized("Wind Barb Attributes");
	xmstr = XmStringCreateLocalized("Wind Speed (kts)");
	show_speed = TRUE;
	show_hsize = FALSE;

/*
 * set key board focus to wind speed text field 
 */

	drawingw = (Widget)mcanvw_getDrawingW();
	XtSetKeyboardFocus(drawingw, _wnd_txtW[SPD]);
	XtManageChild(_wnd_txtW[SPD]);
	break;

      case OBJ_WINDARRW:

	el.hdr.vg_type = _vgType = ARROW_ELM;
	el.hdr.vg_class = CLASS_WINDS;
	ces_get(_subTyp, &el, &ier);
	pgwndw_setAttr( _wndDir[1], _wndSpd[1], 
		       el.elem.wnd.info.size, 
		       el.elem.wnd.info.width, 
		       el.elem.wnd.info.hdsiz,
		       el.elem.wnd.info.wndtyp);
	title_str = XmStringCreateLocalized("Wind Arrow Attributes");
	xmstr = XmStringCreateLocalized("Wind Speed (m/s)");
	show_speed = TRUE;
	show_hsize = TRUE;

/*
 * set key board focus to arrow's text field 
 */

	drawingw = (Widget)mcanvw_getDrawingW();
	XtSetKeyboardFocus(drawingw, _wnd_txtW[SPD]);
	XtManageChild(_wnd_txtW[SPD]);
	break; 
  
      case OBJ_WINDDARR:

	el.hdr.vg_type = _vgType = DARR_ELM;
	el.hdr.vg_class = CLASS_WINDS;
	ces_get(_subTyp, &el, &ier);
	pgwndw_setAttr( _wndDir[2], _wndSpd[2], 
		       el.elem.wnd.info.size, 
		       el.elem.wnd.info.width, 
		       el.elem.wnd.info.hdsiz,
		       el.elem.wnd.info.wndtyp);
	title_str = XmStringCreateLocalized("Directional Arrow Attributes");
	xmstr = XmStringCreateLocalized("    ");
	show_speed = FALSE;
	show_hsize = TRUE;
	break;

      case OBJ_WINDHASH:

	el.hdr.vg_type = _vgType = HASH_ELM;
	el.hdr.vg_class = CLASS_WINDS;
	ces_get(_subTyp, &el, &ier);
	pgwndw_setAttr( _wndDir[3], _wndSpd[3], 
		       el.elem.wnd.info.size, 
		       el.elem.wnd.info.width, 
		       el.elem.wnd.info.hdsiz,
		       el.elem.wnd.info.wndtyp);
	title_str = XmStringCreateLocalized("Hash Attributes");
	xmstr = XmStringCreateLocalized("    ");
	show_speed = FALSE;
	show_hsize = FALSE;

	break;

      default:

	break;
    }

/* 
 * setup object edit window title 
 */

    XtVaSetValues(_wnd_dlgW,
		XmNdialogTitle, 		title_str, 
		NULL); 
    XmStringFree(title_str);

/* 
 * setup object speed title 
 */

    XtVaSetValues( _wnd_sldW[SPD],
		XmNtitleString, 		xmstr,
		NULL);
    XmStringFree(xmstr);

/* 
 * setup speed and arrow head size sensitivity 
 */ 

    XtSetSensitive(_wnd_sldW[SPD], show_speed);
    XtSetSensitive(_wnd_txtW[SPD], show_speed);
    XtSetSensitive(_wnd_sldW[A_SIZ], show_hsize);
    XtSetSensitive(_wnd_txtW[A_SIZ], show_hsize);

/*  
 * setup color  
 */

    _wndColor = el.hdr.maj_col;
    XtVaSetValues (_clorButW, 
		XmNbackground,		NxmColrP_getColorPixel(_wndColor),
		XmNtopShadowColor,	NxmColrP_getColorPixel(_wndColor),
		XmNbottomShadowColor,	NxmColrP_getColorPixel(_wndColor),
		NULL);
  
    if (show_ctl) {
	if (!XtIsManaged (_ctlFormW)) 
	    XtManageChild (_ctlFormW);
        if (callback) {
	    for (ii=0; ii<2; ii++) {
		XtRemoveAllCallbacks (_ctlBtnsW[ii], XmNactivateCallback);
                XtAddCallback (_ctlBtnsW[ii], XmNactivateCallback,
                                        callback, (XtPointer)ii);
	    }
        }

	XtSetSensitive( _actionBtnW[1], True);

    }
    else {
	if (XtIsManaged (_ctlFormW)) 
	    XtUnmanageChild (_ctlFormW);

	if (XmToggleButtonGadgetGetState(_actionBtnW[1])) {
	    XmToggleButtonGadgetSetState(_actionBtnW[1], FALSE, FALSE);
	    _action = VALUE;
	}
	XtSetSensitive( _actionBtnW[1], False );
    }
  
    if (!(show_ctl)) {
        pgwndw_draw();
    }
    XtManageChild(_wnd_dlgW);

/*
 *  Set attributes in multi-selection
 */
    pgwndw_initAttrEdit();

}

/*=====================================================================*/

void pgwndw_popdown ( void )
/************************************************************************
 * pgwndw_popdown							*
 * 									*
 * This function unmanages the vector dialog box			*
 * 									*
 * void pgwndw_popdown ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *				NONE					*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97                                           *
 * C. Lin/EAI	10/97 	rename from NxmWindUnmanage, add check  	*
 * W. Li/EAI	02/99	resets the keyboard focus to main-drawing window*
 * J. Wu/SAIC	09/01	add parentheses around assignment when used as	*
 *			truth value					*
 * J. Wu/SAIC		12/03   pop down color pallette			*
 ***********************************************************************/
{
    Widget	drawingw;
/*---------------------------------------------------------------------*/

    if ( XtIsManaged(_wnd_dlgW) ) {
        NxmClrW_popdown();    	
    	XtUnmanageChild(_wnd_dlgW);
    }

    if ((drawingw = (Widget)mcanvw_getDrawingW())){
        XtSetKeyboardFocus(drawingw, NULL);
    }
}

/*=====================================================================*/

void pgwndw_setAttr ( float dir, float speed, float brbsiz, 
				int brbwidth, float hdsiz, int wndtype )
/************************************************************************
 * pgwndw_setAttr							*
 *									*
 * This function sets the initial values of the vector edit box		*
 *									*
 * void pgwndw_setAttr( dir, speed, brbsiz, brbwidth, hdsiz, wndtype)	*
 *									*
 * Input parameters:							*
 *	dir		float	direction				*
 *	speed		float	speed					*
 *	brbsiz		float	size					*
 *	brbwidth	int	width					*
 *	hdsiz		float	arrow head size				*
 *	wndtype		int	clear type				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	12/97	initial coding				*
 * E. Safford/GSC	12/97	modify to handle -value degrees		*
 * S. Law/GSC		03/98	added MIN values, cleanup		*
 * S. Law/GSC		04/98	added new slider values	_wndSpd as array*
 * W. Li/EAI		04/98	added darr and hash in CLASS_WINDS	*
 * W. Li/EAI		05/98	added call to pgwndw_setTable		*
 * W. Li/EAI		09/98	Added clear type for vect		*
 * W. Li/EAI		09/98	Updated _wndDir to array		*
 * W. Li/EAI		02/99	set the keyboard focus to wind speed	*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 * M. Li/SAIC		09/06	add INCREMENT action			*
 ***********************************************************************/
{
    char	str[5];
    int		maxspd, newspd, newdir, wtype;
    int		max_scale[4] = {MAX_SPD_SCALE, MAX_ARRW_SPD_SCALE, 
				MAX_ARRW_SPD_SCALE, MAX_SPD_SCALE};  
/*---------------------------------------------------------------------*/

      _wndType = wndtype;
 
    if (_wndType == 112) {
	  XmToggleButtonGadgetSetState( _clearBtnW[0], FALSE, FALSE);
	  XmToggleButtonGadgetSetState( _clearBtnW[1], TRUE, FALSE);
    }
    else if (_wndType == 114) {
	  XmToggleButtonGadgetSetState( _clearBtnW[0], TRUE, FALSE);
	  XmToggleButtonGadgetSetState( _clearBtnW[1], FALSE, FALSE);
    }

    pgwndw_getwtype (_vgType, &wtype);    
    maxspd = max_scale[wtype];

    if (wtype  == 3) {
        XtSetSensitive(_clearBtnW[0], FALSE);
        XtSetSensitive(_clearBtnW[1], FALSE);
    }
    else{
        XtSetSensitive(_clearBtnW[0], TRUE);
        XtSetSensitive(_clearBtnW[1], TRUE);
    }

    if (((float)MIN_SPD_SCALE <= speed)&&(speed <= (float) maxspd)) {
        _wndSpd[wtype] = speed;

	if ( _action != INCREMENT ) {
            if (wtype == 1)   
                XmScaleSetValue(_wnd_sldW[SPD], 2 * (int) (_wndSpd[wtype]));
            else
                XmScaleSetValue(_wnd_sldW[SPD], (int) (_wndSpd[wtype]));

            newspd = (int)_wndSpd[wtype];
            if (_vgType == BARB_ELM) {
                newspd = ((newspd % 5) < 3) ? newspd - (newspd%5) :
                newspd + (5 - newspd%5);
            }
            sprintf (str, "%i", newspd);
	}
	else {
	    XmScaleSetValue(_wnd_sldW[SPD], (int) (_spdInc[wtype]) + MAX_SPD_SCALE);
	    sprintf (str, "%+d", (int)_spdInc[wtype]);
	} 

	XmTextFieldSetString (_wnd_txtW[SPD], "");
        XmTextFieldSetString (_wnd_txtW[SPD], str);

        XmTextSetInsertionPosition(_wnd_txtW[SPD],
            XmTextGetLastPosition (_wnd_txtW[SPD]));

    }

    if (((float) (-1 * MAX_DIR_SCALE) <= dir) &&
		     (dir <= (float) MAX_DIR_SCALE)) {
	if (dir < 0.0F)
	    dir += 360.0F; 

	newdir = (int) ((dir + 3.0F) / 5.0F) * 5;
	
   	if (newdir == 0 || newdir == 360) 
       	    newdir = (G_DIFF(_wndSpd[wtype], 0.0F) ) ? 0 : 360;
      
	_wndDir[wtype] = (float)newdir;

	if ( _action != INCREMENT) {
            XmScaleSetValue(_wnd_sldW[DIR], (newdir / 5));
            sprintf (str, "%i", newdir);
	}
	else {
	    newdir = (int) ((_dirInc[wtype] + 3.0F) / 5.0F) * 5;
            XmScaleSetValue(_wnd_sldW[DIR], ( (newdir + MAX_DIR_SCALE) / 5) );
            sprintf (str, "%+d", (int)_dirInc[wtype]);
   	}
    	XmTextFieldSetString (_wnd_txtW[DIR], "");
        XmTextFieldSetString (_wnd_txtW[DIR], str);
    }


    if ((MIN_SIZ_SCALE <= brbsiz) && (brbsiz <= (float) MAX_SIZ_SCALE)) {
        _wndSiz = brbsiz;
        XmScaleSetValue(_wnd_sldW[SIZE], (int)(_wndSiz*10.0F));
	sprintf (str, "%.1f", _wndSiz);
	XmTextFieldSetString (_wnd_txtW[SIZE], "");
	XmTextFieldSetString (_wnd_txtW[SIZE], str);
    }

    if ((MIN_WIDTH_SCALE <= brbwidth) && (brbwidth <= MAX_WIDTH_SCALE)) {
        _wndWidth = brbwidth;
        XmScaleSetValue(_wnd_sldW[WIDTH], _wndWidth);
	sprintf (str, "%i", _wndWidth);
	XmTextFieldSetString (_wnd_txtW[WIDTH], str);
    }

    if ((MIN_HDZ_SCALE <= hdsiz) && (hdsiz <= (float)MAX_HDZ_SCALE))  {
        _wndHdz = hdsiz;
        XmScaleSetValue(_wnd_sldW[A_SIZ], (int) (_wndHdz*10.0F));
	sprintf (str, "%.1f", _wndHdz);
	XmTextFieldSetString (_wnd_txtW[A_SIZ], "");
	XmTextFieldSetString (_wnd_txtW[A_SIZ], str);
    }

    pgwndw_setTable(_wndSiz, _wndHdz, _wndWidth, _wndType);

}

/*=====================================================================*/

void pgwndw_setColr ( int colr )
/************************************************************************
 * pgwndw_setColr							*
 *									*
 * This function sets the color value of the vector edit box		*
 *									*
 * void pgwndw_setColr(colr)						*
 *									*
 * Input parameters:							*
 *	colr		int	object color				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 **									*
 * Log:									*
 * S. Law/GSC		03/98	initial coding				*
 ***********************************************************************/
{
    XtVaSetValues (_clorButW, 
		XmNbackground,		NxmColrP_getColorPixel (colr),
		XmNtopShadowColor,	NxmColrP_getColorPixel (colr),
		XmNbottomShadowColor,	NxmColrP_getColorPixel (colr),
		NULL);   
    _wndColor = colr;
    pgwndw_saveColor ();
}

/*=====================================================================*/

Boolean pgwndw_isUp ( void )
/************************************************************************
 * pgwndw_isUp								*
 *                                                                      *
 * This function returns a boolean value specifying whether the vector	*
 * dialog is managed or not.                                            *
 *                                                                      *
 * Boolean pgwndw_isUp ( )                                              *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * pgwndw_isUp	Boolean		True -- up, 	False -- down		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         8/97                                           *
 * C. Lin/EAi           10/97 	rename from NxmWindManaged 		*
 ***********************************************************************/
{
    return (XtIsManaged(_wnd_dlgW) );
}

/*=====================================================================*/

void pgwndw_getData ( float *dir, float *speed, int *color, int *increment )
/************************************************************************
 * pgwndw_getData							*
 *									*
 * This function gets vector related data.				*
 *									*
 * void pgwndw_getData (dir, speed, color )				*
 *									*
 * Input parameters:							*
 *			NONE						*
 * Output parameters:							*
 *	*dir	float	 direction					*
 *	*speed	float	 speed						*
 *	*color	int	 color						*
 *	*increment int	 increment flag					*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **                                                                     *
 * Log:									*
 * C. Lin/EAi		10/97	Created					*
 * S. Law/GSC		03/98	Removed attributes, rename from getAttr	*
 * S. Law/GSC		04/98	Modified to use _wndSpd as array	*
 *  W. Li/EAI		09/98	Updated _wndDir to array		*
 *  W. Li/EAI		10/98	Added color				*
 * M. Li/SAIC		09/06	Added INCREMENT action			*
 ***********************************************************************/
{
    int	wtype;
/*---------------------------------------------------------------------*/
    pgwndw_getwtype (_vgType, &wtype);    

    if ( _action == INCREMENT ) {
	*increment = 1;
	*dir = _dirInc[wtype];
	*speed = _spdInc[wtype];
    }
    else {
	*increment = 0;
        *dir    =  _wndDir[wtype];
        *speed  =  _wndSpd[wtype];
    }
    *color  =  _wndColor;
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_spdTxtCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_spdTxtCb                                                      *
 *                                                                      *
 * Callback for speed text.                                        	*
 *                                                                      *
 * void pgwndw_spdTxtCb( wdgt, clnt, call)                     		*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi		Created                                 *
 * E. Safford/GSC	09/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI		10/97	Rename from NxmWindTxtSpdCb, cleanup	*
 * S. Law/GSC		03/98	Added MIN values, cleanup		*
 * S. Law/GSC		04/98	Modified to use _wndSpd as array	*
 * S. Law/GSC		05/98	Added event check			*
 * W. Li/EAI		09/98	Updated _wndDir to array		*
 * W. Li/EAI		10/98	Cleaned up and added call to gettype()	*
 * M. Li/SAIC		09/06	Added INCREMENT	action			*
 ***********************************************************************/
{
    char 	*ss;
    float	wndspd[4];
    int  	slval, maxspd, minspd, wtype, spdoff;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
/*---------------------------------------------------------------------*/
/*
 * Confirm there is an event.  If not, this text has already
 * been set up.
 */
    if (!cbs->event) 
	return;

    ss = XmTextFieldGetString(_wnd_txtW[SPD]);
    pgwndw_getwtype (_vgType, &wtype);    
    wndspd[wtype] = (float) (atof(ss));
    XtFree(ss);

    if ( _action == INCREMENT ) {
	_spdInc[wtype] = wndspd[wtype];
	spdoff = MAX_SPD_SCALE;
    }
    else {
	_wndSpd[wtype] =  wndspd[wtype];
	spdoff = 0;
    }

/*
 * if the value on corresponding slider is different, set the 
 * sliders value accordingly.
 */
    XmScaleGetValue(_wnd_sldW[SPD], &slval);
    maxspd = (_vgType == BARB_ELM) ? MAX_SPD_SCALE : MAX_ARRW_SPD_SCALE;
    minspd = (_action == INCREMENT) ? -maxspd : MIN_SPD_SCALE;

    if(((float)minspd <= wndspd[wtype])&&(wndspd[wtype] <= (float)maxspd)){
        if ((int) wndspd[wtype] != slval) { 
	    if (wtype == 1)
	        XmScaleSetValue(_wnd_sldW[SPD], (int) (2 * wndspd[wtype]) + spdoff);
	    else
	        XmScaleSetValue(_wnd_sldW[SPD], (int) (wndspd[wtype]) + spdoff);
	}
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_dirTxtCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_dirTxtCb                                                      *
 *                                                                      *
 * Callback for direction text.                                    	*
 *                                                                      *
 * void pgwndw_dirTxtCb( wdgt, clnt, call)               	        *
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi		Created                                 *
 * E. Safford/GSC	09/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI		10/97	Rename from NxmWindTxtDirCb, cleanup	*
 * S. Law/GSC		03/98	Added MIN values, cleanup		*
 * S. Law/GSC		04/98	Modified to use new slider values	*
 * S. Law/GSC		05/98	Added event check			*
 * W. Li/EAI		10/98	Cleaned up and added call to gettype()	*
 * M. Li/SAIC		09/06	Added INCREMENT action			*
 ***********************************************************************/
{
    char *ss;
    float  wnddir[4];
    int  slval, newval, wtype, min_dir;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
/*---------------------------------------------------------------------*/
/*
 * Confirm there is an event.  If not, this text has already
 * been set up.
 */
    if (!cbs->event) 
	return;
    pgwndw_getwtype (_vgType, &wtype);    
    ss = XmTextFieldGetString(_wnd_txtW[DIR]);
    wnddir[wtype] = (float)(atof(ss));
    XtFree(ss);

/* if the value on corresponding slider is different, set the sliders
 * value accordingly.
 */
    XmScaleGetValue(_wnd_sldW[DIR], &slval);

    min_dir = (_action == VALUE) ? MIN_DIR_SCALE : (-MAX_DIR_SCALE);

    if (((float)min_dir <= wnddir[wtype]) && (wnddir[wtype] <= (float)MAX_DIR_SCALE)){
	if ( _action == VALUE ) {
	    _wndDir[wtype] = wnddir[wtype];
            newval = (int)_wndDir[wtype];
	}
	else {
	    _dirInc[wtype] = wnddir[wtype];
            newval = (int)(_dirInc[wtype] + MAX_DIR_SCALE);
	}
	newval /= 5;
        if (newval != slval) {
            XmScaleSetValue(_wnd_sldW[DIR], newval);
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_sizTxtCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_sizTxtCb                                                      *
 *                                                                      *
 * Callback for size text.                                         	*
 *                                                                      *
 * void pgwndw_sizTxtCb( wdgt, clnt, call)	                        *
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi		Created                                 *
 * E. Safford/GSC	09/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindTxtSizCb, cleanup	*
 * S. Law/GSC		03/98	Added MIN values, cleanup		*
 * F. J. Yen/NCEP       04/98   Updated with new ces function names     *
 * F. J. Yen/NCEP       05/98   Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	Added event check and call to _setTable	*
 ***********************************************************************/
{
    char *ss;
    int  slval;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
/*---------------------------------------------------------------------*/
/*
 * Confirm there is an event.  If not, this text has already
 * been set up.
 */
    if (!cbs->event) 
	return;

    ss = XmTextFieldGetString(_wnd_txtW[SIZE]);
    _wndSiz = (float)(atof(ss));
    XtFree(ss);

/*
 * if the value on corresponding slider is different, set the sliders
 * value accordingly.
 */
    XmScaleGetValue(_wnd_sldW[SIZE], &slval);

    if ((MIN_SIZ_SCALE <= _wndSiz) && (_wndSiz <= (float) MAX_SIZ_SCALE)) {
	pgwndw_setTable (_wndSiz, -1.0F, -1, -1);

        if ( (int) (_wndSiz*10.0F) != slval) {
            XmScaleSetValue(_wnd_sldW[SIZE], (int)(_wndSiz*10.0F));
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_hdzTxtCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_hdzTxtCb                                                      *
 *                                                                      *
 * Callback for arrow head size text widget.                       	*
 *                                                                      *
 * void pgwndw_hdzTxtCb( wdgt, clnt, call)                           	*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi		Created                                 *
 * E. Safford/GSC	09/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindTxtHdzCb, cleanup	*
 * S. Law/GSC		03/98	Added MIN values, cleanup		*
 * F. J. Yen/NCEP       05/98   Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	Added event check and call to _setTable	*
 ***********************************************************************/
{
    char *ss;
    int  slval;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
/*---------------------------------------------------------------------*/
/*
 * Confirm there is an event.  If not, this text has already
 * been set up.
 */
    if (!cbs->event) 
	return;

    ss = XmTextFieldGetString(_wnd_txtW[A_SIZ]);
    _wndHdz = (float)(atof(ss));
    XtFree(ss);

/* if the value on corresponding slider is different, set the sliders
 * value accordingly.
 */
    XmScaleGetValue(_wnd_sldW[A_SIZ], &slval);

    if ((MIN_HDZ_SCALE <= _wndHdz) && (_wndHdz <= (float)MAX_HDZ_SCALE)) {
	pgwndw_setTable (-1.0F, _wndHdz, -1, -1);

        if ( (int) (_wndHdz*10.0F) != slval) {
            XmScaleSetValue(_wnd_sldW[A_SIZ], (int) (_wndHdz*10.0F));
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_widthTxtCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_widthTxtCb							*
 *									*
 * Callback for width text widget.					*
 *									*
 * void pgwndw_widthTxtCb( wdgt, clnt, call )	        		*
 *									*
 * Input parameters:							*
 *	wdgt	Widget		Widget ID				*
 *	clnt	XtPointer	not used				*
 *	call	XtPointer	callback data				*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi		Created					*
 * E. Safford/GSC	09/97	Replaced grP->cmd with cmd_ routines	*
 * E. Wehner/EAi	09/97	Remove graphics info record		*
 * C. Lin/EAI		10/97	Rename from NxmWindTxtWidthCb, cleanup	*
 * S. Law/GSC		03/98	Added MIN values, cleanup		*
 * F. J. Yen/NCEP       04/98   Updated with new ces function names     *
 * F. J. Yen/NCEP       05/98   Renamed _gemType with _subTyp		*
 * S. Law/GSC		05/98	Added event check and call to _setTable	*
 ***********************************************************************/
{
    char *ss;
    int  slval;
    XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
/*---------------------------------------------------------------------*/
/*
 * Confirm there is an event.  If not, this text has already
 * been set up.
 */
    if (!cbs->event) 
	return;

    ss = XmTextFieldGetString(_wnd_txtW[WIDTH]);
    _wndWidth = atoi(ss);
    XtFree(ss);

/*
 * if the value on corresponding slider is different, set the sliders
 * value accordingly.
 */
    XmScaleGetValue(_wnd_sldW[WIDTH], &slval);
    if ( (MIN_WIDTH_SCALE <= _wndWidth) && (_wndWidth <= MAX_WIDTH_SCALE)) {
	pgwndw_setTable (-1.0F, -1.0F, _wndWidth, -1);

        if (_wndWidth != slval) {
            XmScaleSetValue(_wnd_sldW[WIDTH], _wndWidth);
        }
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_spdCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_spdCb                                                      	*
 *                                                                      *
 * Callback for speed scale widget.                                	*
 *                                                                      *
 * void pgwndw_spdCb( wdgt, clnt, call)                     		*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget	    	Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi                Created                                 *
 * E. Safford/GSC	 9/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindSpdCb, cleanup	*
 * W. Li/EAI		10/98	Cleaned up and added call to gettype()	*
 * M. Li/SAIC		09/06	Added INCREMENT action			*
 ***********************************************************************/
{
    char txtstr[5];
    int  rounded_val, wtype, minspd;
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    pgwndw_getwtype (_vgType, &wtype);   
    minspd = (_action == INCREMENT) ? -MAX_SPD_SCALE : MIN_SPD_SCALE;
    if (pgpalw_getCurObjId() ==  OBJ_WINDBARB) {
     
/* 
 * wind speed is only to nearest 5 mph increment.
 */
         rounded_val = ((cbs->value % 5) < 3) ? cbs->value - (cbs->value%5) :
                                      cbs->value + (5 - cbs->value%5);
         if ((rounded_val >= minspd) && (rounded_val <= MAX_SPD_SCALE))
             XmScaleSetValue(wdgt, rounded_val);
    }
    else {
         rounded_val = (int)(0.5F * (float)cbs->value);
         if ((rounded_val >= minspd) && (rounded_val <= MAX_ARRW_SPD_SCALE))
             XmScaleSetValue(wdgt, 2 * rounded_val);
    }


    if ( _action == INCREMENT ) {
        _spdInc[wtype] = (float) (rounded_val - MAX_ARRW_SPD_SCALE);
	sprintf(txtstr, "%+d", (int)_spdInc[wtype]);
    }
    else {
        sprintf(txtstr, "%i", rounded_val);
        _wndSpd[wtype] = (float) rounded_val;
    }
    XmTextFieldSetString(_wnd_txtW[SPD], "");
    XmTextFieldSetString(_wnd_txtW[SPD], txtstr);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_clearCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwndw_clearCb                                                      	*
 *                                                                      *
 * Callback for clear button widget.                            	*
 *                                                                      *
 * void pgwndw_clearCb( wdgt, clnt, call )    	             		*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer    	not used				*
 *   call	XtPointer	callback struct				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI	09/98							*
 * T. Piper/SAIC        12/02   radio box -> check box                  *
 ***********************************************************************/
{  
    int which = (long)clnt;
/*---------------------------------------------------------------------*/

    if (which == 0){
        _wndType = 114;
	XmToggleButtonGadgetSetState( _clearBtnW[1], FALSE, FALSE);
	XmToggleButtonGadgetSetState( _clearBtnW[0], TRUE, FALSE);
    }
    else if (which == 1){
        _wndType = 112;
	XmToggleButtonGadgetSetState( _clearBtnW[0], FALSE, FALSE);
	XmToggleButtonGadgetSetState( _clearBtnW[1], TRUE, FALSE);
    }

    pgwndw_setTable(-1.0F, -1.0F, -1, _wndType);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_actionCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * pgwndw_actionCb                                                      *
 *                                                                      *
 * Callback for action radio box widget.                                *
 *                                                                      *
 * void pgwndw_clearCb( wdgt, clnt, call )                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *   wdgt       Widget          Widget ID                               *
 *   clnt	XtPointer       not used                                *
 *   call       XtPointer       callback struct                         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC   09/06                                                   *
 * M. Li/SAIC	10/06	Initiate wind speed slider			*
 ***********************************************************************/
{
    int which = (long)clnt, ii, wtype;
    int scl_max[]= {MAX_DIR_SCALE, MAX_SPD_SCALE};
    int scl_min[]= {MIN_DIR_SCALE, MIN_SPD_SCALE};
    int scl_mdf[]= {2, 10}; 
    char str[5], str1[5];

/*---------------------------------------------------------------------*/

    pgwndw_getwtype (_vgType, &wtype);

    if (which == 1) {
	_action = INCREMENT;

     	for ( ii = 0; ii < 2; ii++ ) {
	    XtVaSetValues( _wnd_sldW[ii],
                	XmNmaximum,                     2*scl_max[ii]*scl_mdf[ii]/10,
                	XmNminimum,                     scl_min[ii],
                	XmNvalue,                       scl_max[ii]*scl_mdf[ii]/10,
                	NULL);
     	}

	sprintf (str, "%+d", (int)_spdInc[wtype]);
	sprintf (str1, "%+d", (int)_dirInc[wtype]);
        XmScaleSetValue(_wnd_sldW[SPD], (int) (_spdInc[wtype]) + MAX_SPD_SCALE);
	XmScaleSetValue(_wnd_sldW[DIR], (int)(_dirInc[wtype] + MAX_DIR_SCALE)/5);

    }
    else {
	_action = VALUE;

       	for ( ii = 0; ii < 2; ii++ ) {
                XtVaSetValues( _wnd_sldW[ii],
                        XmNmaximum,                     scl_max[ii]*scl_mdf[ii]/10,
                        XmNminimum,                     scl_min[ii],
                        NULL);
        }
	XmScaleSetValue(_wnd_sldW[SPD], 2 * (int) (_wndSpd[wtype]));
	XmScaleSetValue(_wnd_sldW[DIR], (int)(_wndDir[wtype])/5);
	sprintf (str, "%i", (int)(_wndSpd[wtype]));
        sprintf (str1, "%i",(int)(_wndDir[wtype]));

    }

    XmTextFieldSetString (_wnd_txtW[SPD], "");
    XmTextFieldSetString (_wnd_txtW[SPD], str);
    XmTextFieldSetString (_wnd_txtW[DIR], "");
    XmTextFieldSetString (_wnd_txtW[DIR], str1);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_dirCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_dirCb                                                      	*
 *                                                                      *
 * Callback for direction scale widget.                            	*
 *                                                                      *
 * void pgwndw_dirCb( wdgt, clnt, call)     	             		*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data 				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi                Created                                 *
 * E. Safford/GSC	 9/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindDirCb, cleanup	*
 * E. Safford/GSC	12/97	modified to step in units of 5          *
 * S. Law/GSC		04/98	modified to accept new slider values	*
 * W. Li/EAI		10/98	Cleaned up and added call to gettype()	*
 * M. Li/SAIC		09/06	Added INCREMENT action			*
 ***********************************************************************/
{
    int  wtype;
    char txtstr[5];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/
    pgwndw_getwtype (_vgType, &wtype);    

    if ( _action == VALUE ) {
	_wndDir[wtype] = (float) cbs->value * 5.0F;
	sprintf(txtstr, "%i", (int)_wndDir[wtype]);
    }
    else {      
        _dirInc[wtype] = (float) (cbs->value * 5.0F - MAX_DIR_SCALE);
	sprintf(txtstr, "%+d", (int)_dirInc[wtype]);
    }
    XmTextFieldSetString(_wnd_txtW[DIR], "");
    XmTextFieldSetString(_wnd_txtW[DIR], txtstr);
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_sizeCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_sizeCb                                                        *
 *                                                                      *
 * Callback for size scale widget.                            		*
 *                                                                      *
 * void pgwndw_sizeCb ( wdgt, clnt, call )     	                	*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi                Created                                 *
 * E. Safford/GSC	 9/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindSizCb, cleanup	*
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
    char txtstr[5];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    _wndSiz = (float)(cbs->value) /10.0F;
    sprintf(txtstr, "%.1f", _wndSiz);
    XmTextFieldSetString(_wnd_txtW[SIZE], "");
    XmTextFieldSetString(_wnd_txtW[SIZE], txtstr);

    if ((MIN_SIZ_SCALE <= _wndSiz) && (_wndSiz <= (float) MAX_SIZ_SCALE)) {
	if (cbs->reason == XmCR_VALUE_CHANGED)
	    pgwndw_setTable (_wndSiz, -1.0F, -1, -1);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_hdzCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_hdzCb                                                         *
 *                                                                      *
 * Callback for speed text.                                        	*
 *                                                                      *
 * void pgwndw_hdzCb( wdgt, clnt, call)                	        	*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi                Created                                 *
 * E. Safford/GSC	 9/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindHdzCb, cleanup	*
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 * J. Wu/SAIC		05/02	wipe text fields B4 assigning new vals  *
 ***********************************************************************/
{
    char txtstr[5];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    _wndHdz = (float)(cbs->value) /10.0F;
    sprintf(txtstr, "%.1f", _wndHdz);
    XmTextFieldSetString(_wnd_txtW[A_SIZ], "");
    XmTextFieldSetString(_wnd_txtW[A_SIZ], txtstr);

    if ((MIN_HDZ_SCALE <= _wndHdz) && (_wndHdz <= (float)MAX_HDZ_SCALE)) {
	if (cbs->reason == XmCR_VALUE_CHANGED)
	    pgwndw_setTable (-1.0F, _wndHdz, -1, -1);
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgwndw_widthCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_widthCb                                                      	*
 *                                                                      *
 * Callback for width scale.                                  		*
 *                                                                      *
 * void pgwndw_widthCb( wdgt, clnt, call)                     		*
 *									*
 * Input parameters:                                                    *
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer	not used				*
 *   call	XtPointer	callback data				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi                Created                                 *
 * E. Safford/GSC	 9/97	Replaced grP->cmd with cmd_ routines    *
 * E. Wehner/EAi	 9/97	Remove graphics info record		*
 * C. Lin/EAI	        10/97	Rename from NxmWindWidthCb, cleanup	*
 * S. Law/GSC		05/98	added drag check and call to _setTable	*
 ***********************************************************************/
{
    char txtstr[5];
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
/*---------------------------------------------------------------------*/

    _wndWidth = cbs->value;
    sprintf(txtstr, "%i", _wndWidth);
    XmTextFieldSetString(_wnd_txtW[WIDTH], txtstr);

    if ( (MIN_WIDTH_SCALE <= _wndWidth) && (_wndWidth <= MAX_WIDTH_SCALE)) {
	if (cbs->reason == XmCR_VALUE_CHANGED)
	    pgwndw_setTable (-1.0F, -1.0F, _wndWidth, -1);
    }
}

/*=====================================================================*/

void pgwndw_draw ( void )
/************************************************************************
 * pgwndw_draw     							*
 *                                                                      *
 * This function setup the mouse event to draw vector objects.		*
 *                                                                      *
 * void pgwndw_draw  ( )                           			*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *			NONE						*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E.Wehner/EAI     3/97        Created                         	*
 * C. Lin/EAI	   10/97	rename from NxmWindPlace, cleanup	*
 * G. Krueger/EAI   6/98	Uniform status hints			*
 ***********************************************************************/
{
/* 
 * arm the functions for placing wind barbs
 */
    pgnew_setArmDynamic();
    mcanvw_setDynActFlag(True); 
}

/*=====================================================================*/

void pgwndw_colorCb ( Widget wdgt, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgwndw_colorCb							*
 *									*
 * Callback for color button widget.  Calls to NxmClrW_setOkF, before	*
 * passing the information on to NxmClrW_popup.				*
 *									*
 * void pgwndw_colorCb (wdgt, clnt, call)				*
 *									*
 * Input parameters:							*
 *   wdgt	Widget		Widget ID				*
 *   clnt	XtPointer      	color					*
 *   call	XtPointer	callback data				*
 *									*
 **									*
 * Log:									*
 * S. Law	03/10/98	Initial coding				*
 * W. Li/EAI	10/98	Removed "OK" button in color editing window	*
 ***********************************************************************/
{
    NxmClrW_popup (wdgt, clnt, call);
}

/*=====================================================================*/

void pgwndw_saveColor ( void )
/************************************************************************
 * pgwndw_saveColor							*
 *									*
 * Saves the new color set by the NxmClrW functions			*
 *									*
 * void pgwndw_saveColor ()						*
 *									*
 **									*
 * Log:									*
 * S. Law	03/10/98	Initial coding				*
 * F. J. Yen/NCEP       04/98   Updated with new ces function names     *
 * F. J. Yen/NCEP       05/98   Renamed _gemType with _subTyp		*
 * E. Safford/SAIC	12/01	add pgutls_initHdr()			*
 ***********************************************************************/
{
    VG_DBStruct 	el;
    int 		ier;
/*---------------------------------------------------------------------*/
   
    pgutls_initHdr ( &(el.hdr) );

    _subTyp   = -1;

    el.hdr.vg_type = _vgType;
    el.hdr.vg_class = CLASS_WINDS;
    ces_get(_subTyp, &el, &ier);

    el.hdr.maj_col = _wndColor;
    el.hdr.min_col = _wndColor;
    ces_set(_subTyp, &el, &ier);
}

/*=====================================================================*/

void pgwndw_setTable ( float size, float hdsize, int width, int wndtype )
/************************************************************************
 * pgwndw_setTable							*
 *									*
 * Places an element into the set table					*
 *									*
 * void pgwndw_setTable (size, hdsize, width, wndtype )			*
 *									*
 * Input parameters:							*
 *	size		float	 size					*
 *	hdsize		float	 head size				*
 *	width		int	 width					*
 *	wndtype		int	 clear type				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/98	moved from various functions		*
 * W. Li/EAI		05/98	cleanup					*
 * W. Li/EAI		09/98	Added clear type for vect		*
 * E. Safford/GSC	04/99	fix format problem on sprintf calls	*
 * J. Wu/SAIC		09/01	eliminate extra'\0' in sprintf format	*
 * E. Safford/SAIC	12/01	add pgutls_initHdr()			*
 ***********************************************************************/
{
    VG_DBStruct	el;
    int		loglev, ier, ier1;
    char	logstr[10], grp[4];
/*---------------------------------------------------------------------*/
    _wndType = wndtype;
    _subTyp   = -1;
    loglev = 2;
    strcpy(grp, "CES");

    pgutls_initHdr ( &(el.hdr) );

    el.hdr.vg_type = _vgType;
    el.hdr.vg_class = CLASS_WINDS;

    ces_get(_subTyp, &el, &ier);
    if (ier  != 0) {
	sprintf(logstr, "%d ", _subTyp);

        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }

    if (size >= MIN_SIZ_SCALE)
	el.elem.wnd.info.size = size;
    if (hdsize >= MIN_HDZ_SCALE)
	el.elem.wnd.info.hdsiz = hdsize;
    if (width >= MIN_WIDTH_SCALE)
	el.elem.wnd.info.width = width;
    if (wndtype >= 0)
	el.elem.wnd.info.wndtyp = wndtype;

    ces_set(_subTyp, &el, &ier);

    if (ier  != 0) {
	sprintf(logstr, "%d ", _subTyp);
        er_lmsg ( &loglev, grp, &ier, logstr, &ier1,
                        strlen(grp), strlen(logstr) );
	NxmErr_update();
        return;
    }
}

/*=====================================================================*/

void pgwndw_getwtype ( int vg_type, int *wtype )
/************************************************************************
 * pgwndw_getwtype							*
 *									*
 * This function is a internal function which gets vector wtype.	*
 *									*
 * void pgwndw_getwtype (vg_type, wtype)				*
 *									*
 * Input parameters:							*
 *	vg_type	int	 vg type					*
 * Output parameters:							*
 *	*wtype	int	 widget type					*
 *									*
 * Return parameters:							*
 *			NONE						*
 *									*
 **                                                                     *
 * Log:									*
 *  W. Li/EAI		10/98						*
 ***********************************************************************/
{
    if (vg_type == BARB_ELM )
        *wtype = 0;
    else if (vg_type == ARROW_ELM ) 
        *wtype = 1;
    else if (vg_type == DARR_ELM ) 
        *wtype = 2;
    else if (vg_type == HASH_ELM ) 
        *wtype = 3;
}
/*=====================================================================*/
/* ARGSUSED */
void pgwndw_attrbEditCb ( Widget w, long which, XtPointer call )
/************************************************************************
 * pgwndw_attrbEditCb                                                   *
 *                                                                      *
 * This function is the call back for the attribute toggle button       *
 *                                                                      *
 * void  pgwndw_attrbEditCb( w, which, call)                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *      w               Widget          Parent widget                   *
 *      which   	long            which button			*
 *      call    	XtPointer	not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC   09/07                                                   *
 ***********************************************************************/
{
int	btnid;
/*---------------------------------------------------------------------*/

     btnid = (long) which;

/* 
 *  check if the button is up or down and take appropriate actions
 */
     if (_editFlags[btnid]) {
         _editFlags[btnid] = False;
     }
     else {
        _editFlags[btnid] = True;
     }

     if ( btnid == COLOR ) {
	XtSetSensitive(_clorButW, _editFlags[btnid]);
     } else {
	XtSetSensitive( _wnd_sldW[btnid], _editFlags[btnid]);
        XtSetSensitive( _wnd_txtW[btnid], _editFlags[btnid]);
     }
}

/*=====================================================================*/

void pgwndw_initAttrEdit ( void )
/************************************************************************
 * pgwndw_initAttrEdit                                                  *
 *                                                                      *
 * This function set toggle buttons of edit window                      *
 *                                                                      *
 * void pgwndw_initAttrEdit ()                                      	*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      none                                                            *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           09/07                                           *       
 ***********************************************************************/
{
    int		ii, oper;
/*---------------------------------------------------------------------*/

    oper = pgpalw_getCurOperId();
    if ( oper == FUNC_MULTISEL ) {
	for ( ii = 0; ii < NUM_BTS; ii++ ) {

	    if ( !XtIsManaged (_attrbEdit[ii] ) )	
		XtManageChild ( _attrbEdit[ii] );   		

 	    if ( (ii == SPD && (_vgType == DARR_ELM || _vgType == HASH_ELM)) ||
		 (ii == A_SIZ && (_vgType == BARB_ELM || _vgType == HASH_ELM) ) ) {
		XmToggleButtonGadgetSetState(_attrbEdit[ii], FALSE, FALSE);
		XtSetSensitive(_attrbEdit[ii], False);
		_editFlags[ii] = False;
                continue;
	    }

	    if ( ii == COLOR ) {
	    	XtVaSetValues( _attrbEdit[ii],
	    		XmNleftAttachment,              XmATTACH_WIDGET,
                	XmNleftWidget,                  _clearBtnW[1],
			NULL );
	    	XtVaSetValues( _clorButW,
                	XmNleftAttachment,              XmATTACH_WIDGET,
                	XmNleftWidget,                  _attrbEdit[COLOR],
			XmNleftOffset,                  0,
                	NULL );
		XtSetSensitive(_clorButW, _editFlags[ii]);
	    } else {
	        XtSetSensitive(_wnd_sldW[ii], _editFlags[ii]);
	        XtSetSensitive(_wnd_txtW[ii], _editFlags[ii]);
	    }

	    if ( !XtIsSensitive(_attrbEdit[ii]) )  XtSetSensitive(_attrbEdit[ii], True);
	}
    } else {

	XtVaSetValues( _clorButW,
                XmNleftAttachment,              XmATTACH_WIDGET,
                XmNleftWidget,                  _clearBtnW[1],
		XmNleftOffset, 			FORM_OFFSET_SIZE,
                NULL );

	for ( ii = 0; ii < NUM_BTS; ii++ ) {

	    if ( XtIsManaged (_attrbEdit[ii] ) )
                XtUnmanageChild ( _attrbEdit[ii] );

	    if ( (ii == SPD && (_vgType == DARR_ELM || _vgType == HASH_ELM)) ||
                 (ii == A_SIZ && (_vgType == BARB_ELM || _vgType == HASH_ELM) ) ) {
                continue;
            }

            if ( ii != COLOR && !XtIsSensitive(_wnd_sldW[ii]) ) { 
		XtSetSensitive(_wnd_sldW[ii], True);
		XtSetSensitive(_wnd_txtW[ii], True);
	    }

	    if ( ii == COLOR && !XtIsSensitive(_clorButW) )
		XtSetSensitive(_clorButW, TRUE);
        }
    }
}

/*=====================================================================*/

void pgwndw_getEditFlag ( Boolean edit_flags[NUM_BTS] )
/************************************************************************
 * pgwndw_getEditFlag                                                   *
 *                                                                      *
 * This function get multiple selection attribute edit flags            *
 *                                                                      *
 * void pgwndw_getEditFlag ( edit_flags )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      edit_flags[5]  Boolean multiple selection attribute edit flags 	*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC           09/07 						* 
 ***********************************************************************/
{
int     ii;
/*---------------------------------------------------------------------*/
    for ( ii = 0; ii < NUM_BTS; ii++){
        edit_flags[ii] = _editFlags[ii]; 
    }
}
